//! This mutator attempts to remove various kinds of items from a wasm module.
//!
//! Removing an item from a wasm module is a somewhat tricky process because
//! there are many locations where indices are referenced. That means that when
//! this mutator is used it will need to renumber all indices in the wasm module
//! for references after the item that's removed. Notably this means that this
//! mutator largely translates between `wasmparser` structures and
//! `wasm_encoder` structures.

use crate::mutators::{Item, Mutator};
use crate::{Error, ReencodeResult};
use crate::{ModuleInfo, Result, WasmMutate};
use rand::Rng;
use std::collections::HashSet;
use wasm_encoder::reencode::Reencode;
use wasm_encoder::*;

/// Mutator that removes a random item in a wasm module (function, global,
/// table, etc).
#[derive(Copy, Clone)]
pub struct RemoveItemMutator(pub Item);

impl Mutator for RemoveItemMutator {
    fn can_mutate(&self, config: &WasmMutate) -> bool {
        self.0.can_mutate(config)
    }

    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<wasm_encoder::Module>> + 'a>> {
        let idx = self.0.choose_removal_index(config);
        log::trace!("attempting to remove {:?} index {}", self.0, idx);

        let result = RemoveItem {
            info: config.info(),
            item: self.0,
            idx,
            referenced_functions: HashSet::new(),
            function_reference_action: Funcref::Save,
            used_index_that_was_removed: false,
        }
        .remove();
        match result {
            Ok(result) => {
                log::debug!("removed {:?} index {}", self.0, idx);
                Ok(Box::new(std::iter::once(Ok(result))))
            }
            Err(e) => {
                log::trace!("failed to remove {:?} index {}: {:?}", self.0, idx, e);
                Err(e)
            }
        }
    }
}

impl Item {
    fn can_mutate(&self, config: &WasmMutate) -> bool {
        // This heuristic is a bit of a lie in that just because an item is
        // present doesn't mean that it's actually candidate for removal. The
        // alternative would be to build up some sort of precise liveness set of
        // all items which is a bit of a pain to do, so instead this mutator is
        // generally always applicable and then might just frequently return a
        // "no mutations applicable" error later
        let info = config.info();
        match self {
            Item::Function => info.num_functions() > 0,
            Item::Table => info.num_tables() > 0,
            Item::Memory => info.num_memories() > 0,
            Item::Global => info.num_globals() > 0,
            Item::Tag => info.num_tags() > 0,
            Item::Type => info.num_types() > 0,

            // Note that data/elements can lead to traps and side-effectful
            // initialization of imported tables/memories, so these are only
            // considered for removal if we're not preserving semantics.
            Item::Data => !config.preserve_semantics && info.num_data() > 0,
            Item::Element => !config.preserve_semantics && info.num_elements() > 0,
        }
    }

    fn choose_removal_index(&self, config: &mut WasmMutate) -> u32 {
        let info = config.info();
        let max = match self {
            Item::Function => info.num_functions(),
            Item::Table => info.num_tables(),
            Item::Memory => info.num_memories(),
            Item::Global => info.num_globals(),
            Item::Tag => info.num_tags(),
            Item::Type => info.num_types(),
            Item::Data => info.num_data(),
            Item::Element => info.num_elements(),
        };
        config.rng().gen_range(0..max)
    }
}

struct RemoveItem<'a> {
    info: &'a ModuleInfo<'a>,
    item: Item,
    idx: u32,
    function_reference_action: Funcref,
    referenced_functions: HashSet<u32>,
    used_index_that_was_removed: bool,
}

enum Funcref {
    /// References to functions are saved in `referenced_functions`.
    Save,
    /// References to functions are ignored for validity.
    Skip,
    /// References to functions are required to be in `.referenced_functions`
    /// and if they're not then an error happens.
    RequireReferenced,
}

impl RemoveItem<'_> {
    fn remove(&mut self) -> Result<Module> {
        // This is the main workhorse loop of the module translation. This will
        // iterate over the original wasm sections, raw, and create the new
        // module section-by-section. Sections are rewritten on-the-fly.
        let mut module = Module::new();
        self.parse_core_module(
            &mut module,
            wasmparser::Parser::new(0),
            self.info.input_wasm,
        )?;

        // If an index was used that was actually being removed then flag this
        // module as no longer applicable to mutate because the item wasn't
        // actually a candidate for removal.
        if self.used_index_that_was_removed {
            return Err(Error::no_mutations_applicable());
        }

        Ok(module)
    }

    /// This is a helper function to filter out the items of the `section`
    /// provided.
    ///
    /// The `section` given, which has items of type `section_item`, will be
    /// iterated over and translated with the `encode` callback. The `encode`
    /// callback is only called for items we're actually preserving in this
    /// module.
    fn filter_out<'a, T>(
        &mut self,
        section: impl IntoIterator<Item = wasmparser::Result<T>>,
        section_item: Item,
        mut encode: impl FnMut(&mut Self, T) -> ReencodeResult<()>,
    ) -> ReencodeResult<()> {
        // Calculate the initial index by taking into account any imported items
        // in this namespace (note that the filtering of imports doesn't use
        // this function).
        let mut index = match section_item {
            Item::Function => self.info.num_imported_functions(),
            Item::Table => self.info.num_imported_tables(),
            Item::Tag => self.info.num_imported_tags(),
            Item::Memory => self.info.num_imported_memories(),
            Item::Global => self.info.num_imported_globals(),
            Item::Data | Item::Element | Item::Type => 0,
        };
        for item in section {
            let item = item?;
            if index != self.idx || section_item != self.item {
                encode(self, item)?;
            }
            index += 1;
        }
        Ok(())
    }

    /// This is "the point" of this type. This function remaps an `idx`
    /// provided, in the `item` index space, to a new index.
    ///
    /// This `RemoveItem` structure will only remove at most one item which
    /// means that the index given is in one of four cases:
    ///
    /// * If the `item` doesn't match the index space of the item we're
    ///   removing, then `idx` is guaranteed to not need modification.
    /// * Otherwise if `idx` is less than the index being removed, it's entirely
    ///   unmodified since we're only modifying later items.
    /// * Otherwise if `idx` matches the index that's being removed then this
    ///   means that the item was actually uses. In this situation we simply say
    ///   that the mutation is not applicable. This will bail out this entire
    ///   attempt to remove the `idx`th item and loops like `wasm-shrink` will
    ///   try something else.
    /// * Finally our index is larger than the one being removed which means we
    ///   now decrement it by one to account for the removed item.
    fn remap(&mut self, item: Item, idx: u32) -> u32 {
        // If we're before the code section then all function references, no
        // matter where they are, are considered "referencing functions" so we
        // save the indices of that which is referenced.
        if item == Item::Function {
            if let Funcref::Save = self.function_reference_action {
                self.referenced_functions.insert(idx);
            }
        }

        if item != self.item || idx < self.idx {
            // Different kind of item or a later item was removed, index doesn't change
            idx
        } else if idx == self.idx {
            // If we're removing a referenced item then that means that this
            // mutation fails. Flag this for later failure and otherwise pass
            // through the index as-is.
            self.used_index_that_was_removed = true;
            idx
        } else {
            // Otherwise this item comes after the item being removed, so
            // this item's index has decreased by one.
            idx - 1
        }
    }
}

impl Reencode for RemoveItem<'_> {
    type Error = Error;

    fn function_index(&mut self, idx: u32) -> u32 {
        self.remap(Item::Function, idx)
    }

    fn table_index(&mut self, idx: u32) -> u32 {
        self.remap(Item::Table, idx)
    }

    fn memory_index(&mut self, idx: u32) -> u32 {
        self.remap(Item::Memory, idx)
    }

    fn tag_index(&mut self, idx: u32) -> u32 {
        self.remap(Item::Tag, idx)
    }

    fn global_index(&mut self, idx: u32) -> u32 {
        self.remap(Item::Global, idx)
    }

    fn type_index(&mut self, idx: u32) -> u32 {
        self.remap(Item::Type, idx)
    }

    fn data_index(&mut self, idx: u32) -> u32 {
        self.remap(Item::Data, idx)
    }

    fn element_index(&mut self, idx: u32) -> u32 {
        self.remap(Item::Element, idx)
    }

    fn parse_type_section(
        &mut self,
        types: &mut wasm_encoder::TypeSection,
        section: wasmparser::TypeSectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.filter_out(section, Item::Type, |me, rec_group| {
            me.parse_recursive_type_group(types.ty(), rec_group)?;
            Ok(())
        })
    }

    fn parse_import_section(
        &mut self,
        imports: &mut wasm_encoder::ImportSection,
        section: wasmparser::ImportSectionReader<'_>,
    ) -> ReencodeResult<()> {
        // The import section is a little special because it defines
        // items in multiple index spaces. This means that the
        // `filter_out` helper can't be used and we have to process
        // everything manually here.
        let mut function = 0;
        let mut global = 0;
        let mut table = 0;
        let mut memory = 0;
        let mut tag = 0;
        for item in section {
            let item = item?;
            let retain;
            match &item.ty {
                wasmparser::TypeRef::Func(_) => {
                    retain = self.item != Item::Function || self.idx != function;
                    function += 1;
                }
                wasmparser::TypeRef::Table(_) => {
                    retain = self.item != Item::Table || self.idx != table;
                    table += 1;
                }
                wasmparser::TypeRef::Memory(_) => {
                    retain = self.item != Item::Memory || self.idx != memory;
                    memory += 1;
                }
                wasmparser::TypeRef::Global(_) => {
                    retain = self.item != Item::Global || self.idx != global;
                    global += 1;
                }
                wasmparser::TypeRef::Tag(_) => {
                    retain = self.item != Item::Tag || self.idx != tag;
                    tag += 1;
                }
            }
            if retain {
                self.parse_import(imports, item)?;
            }
        }
        Ok(())
    }

    fn parse_function_section(
        &mut self,
        funcs: &mut wasm_encoder::FunctionSection,
        section: wasmparser::FunctionSectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.filter_out(section, Item::Function, |me, func| {
            funcs.function(me.type_index(func));
            Ok(())
        })
    }

    fn parse_table_section(
        &mut self,
        tables: &mut wasm_encoder::TableSection,
        section: wasmparser::TableSectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.filter_out(section, Item::Table, |me, table| {
            me.parse_table(tables, table)?;
            Ok(())
        })
    }

    fn parse_memory_section(
        &mut self,
        memories: &mut wasm_encoder::MemorySection,
        section: wasmparser::MemorySectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.filter_out(section, Item::Memory, |me, memory| {
            memories.memory(me.memory_type(memory));
            Ok(())
        })
    }

    fn parse_global_section(
        &mut self,
        globals: &mut wasm_encoder::GlobalSection,
        section: wasmparser::GlobalSectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.filter_out(section, Item::Global, |me, global| {
            me.parse_global(globals, global)?;
            Ok(())
        })
    }

    fn parse_element_section(
        &mut self,
        elements: &mut wasm_encoder::ElementSection,
        section: wasmparser::ElementSectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.filter_out(section, Item::Element, |me, elem| {
            me.parse_element(elements, elem)?;
            Ok(())
        })
    }

    fn parse_data_section(
        &mut self,
        ret: &mut wasm_encoder::DataSection,
        section: wasmparser::DataSectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.filter_out(section, Item::Data, |me, data| {
            me.parse_data(ret, data)?;
            Ok(())
        })
    }

    fn parse_tag_section(
        &mut self,
        tags: &mut wasm_encoder::TagSection,
        section: wasmparser::TagSectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.filter_out(section, Item::Tag, |me, tag| {
            tags.tag(me.tag_type(tag));
            Ok(())
        })
    }

    fn data_count(&mut self, count: u32) -> u32 {
        // Note that the data count section is decremented here if
        // we're removing a data item, otherwise it's preserved
        // as-is.
        if self.item == Item::Data {
            count - 1
        } else {
            count
        }
    }

    fn start_section(&mut self, func: u32) -> u32 {
        self.function_reference_action = Funcref::Skip;
        let func = self.remap(Item::Function, func);
        self.function_reference_action = Funcref::Save;
        func
    }

    fn parse_code_section(
        &mut self,
        code: &mut wasm_encoder::CodeSection,
        section: wasmparser::CodeSectionReader<'_>,
    ) -> ReencodeResult<()> {
        self.function_reference_action = Funcref::RequireReferenced;
        self.filter_out(section, Item::Function, |me, body| {
            me.parse_function_body(code, body)
        })
    }

    fn instruction<'a>(&mut self, op: wasmparser::Operator<'a>) -> ReencodeResult<Instruction<'a>> {
        // The reason for this is that in the code section instructions
        // such as `ref.func 0` are only valid if function 0 is
        // otherwise referenced somewhere in the module via things like
        // globals, exports, element segments, etc. This means that
        // removal of a global funcref *could* make `ref.func 0`
        // invalid where it was valid before.  To prevent creating an
        // invalid module this block guards against this by recognizing
        // when we're in the code section and on seeing a `ref.func`
        // instruction it'll return an error if the index isn't
        // otherwise referenced (probably because we removed the one
        // item that referenced it).
        if let wasmparser::Operator::RefFunc { function_index } = op {
            if let Funcref::RequireReferenced = self.function_reference_action {
                if !self.referenced_functions.contains(&function_index) {
                    self.used_index_that_was_removed = true;
                }
            }
        }

        wasm_encoder::reencode::utils::instruction(self, op)
    }
}

#[cfg(test)]
mod tests {
    use super::{Item, RemoveItemMutator};

    #[test]
    fn remove_type() {
        crate::mutators::match_mutation(
            r#"(module (type (func)))"#,
            RemoveItemMutator(Item::Type),
            r#"(module)"#,
        );
    }

    #[test]
    fn remove_function() {
        crate::mutators::match_mutation(
            r#"(module (func))"#,
            RemoveItemMutator(Item::Function),
            r#"(module (type (func)))"#,
        );
        crate::mutators::match_mutation(
            r#"(module (import "" "" (func)))"#,
            RemoveItemMutator(Item::Function),
            r#"(module (type (func)))"#,
        );
    }

    #[test]
    fn remove_table() {
        crate::mutators::match_mutation(
            r#"(module (table 1 funcref))"#,
            RemoveItemMutator(Item::Table),
            r#"(module)"#,
        );
        crate::mutators::match_mutation(
            r#"(module (import "" "" (table 1 funcref)))"#,
            RemoveItemMutator(Item::Table),
            r#"(module)"#,
        );
    }

    #[test]
    fn remove_memory() {
        crate::mutators::match_mutation(
            r#"(module (memory 1))"#,
            RemoveItemMutator(Item::Memory),
            r#"(module)"#,
        );
        crate::mutators::match_mutation(
            r#"(module (import "" "" (memory 1)))"#,
            RemoveItemMutator(Item::Memory),
            r#"(module)"#,
        );
    }

    #[test]
    fn remove_global() {
        crate::mutators::match_mutation(
            r#"(module (global i32 (i32.const 1)))"#,
            RemoveItemMutator(Item::Global),
            r#"(module)"#,
        );
        crate::mutators::match_mutation(
            r#"(module (import "" "" (global i32)))"#,
            RemoveItemMutator(Item::Global),
            r#"(module)"#,
        );
    }

    #[test]
    fn remove_data() {
        crate::mutators::match_mutation(
            r#"(module (data "xxxx"))"#,
            RemoveItemMutator(Item::Data),
            r#"(module)"#,
        );
    }

    #[test]
    fn remove_elem() {
        crate::mutators::match_mutation(
            r#"(module (elem funcref))"#,
            RemoveItemMutator(Item::Element),
            r#"(module)"#,
        );
    }

    #[test]
    fn renumber_functions() {
        crate::mutators::match_mutation(
            r#"(module
                    (func)
                    (func)
                    (func (export "renumber")
                        call 0
                        call 3)
                    (func)
            )"#,
            RemoveItemMutator(Item::Function),
            r#"(module
                    (func)
                    (func (export "renumber") call 0 call 2)
                    (func)
            )"#,
        );
    }

    #[test]
    fn renumber_table() {
        crate::mutators::match_mutation(
            r#"(module
                    (func (export "")
                        i32.const 0
                        i32.const 0
                        i32.const 0
                        table.copy 0 2
                    )
                    (table 1 funcref)
                    (table 1 funcref)
                    (table 1 funcref)
            )"#,
            RemoveItemMutator(Item::Table),
            r#"(module
                    (func (export "")
                        i32.const 0
                        i32.const 0
                        i32.const 0
                        table.copy 0 1
                    )
                    (table 1 funcref)
                    (table 1 funcref)
            )"#,
        );
    }

    #[test]
    fn renumber_memory() {
        crate::mutators::match_mutation(
            r#"(module
                    (func (export "")
                        i32.const 0
                        i32.const 0
                        i32.const 0
                        memory.copy 0 2
                    )
                    (memory 1)
                    (memory 1)
                    (memory 1)
            )"#,
            RemoveItemMutator(Item::Memory),
            r#"(module
                    (func (export "")
                        i32.const 0
                        i32.const 0
                        i32.const 0
                        memory.copy 0 1
                    )
                    (memory 1)
                    (memory 1)
            )"#,
        );
    }

    #[test]
    fn renumber_data() {
        crate::mutators::match_mutation(
            r#"(module
                    (func (export "")
                        data.drop 1
                    )
                    (data "a")
                    (data "b")
            )"#,
            RemoveItemMutator(Item::Data),
            r#"(module
                    (func (export "")
                        data.drop 0
                    )
                    (data "b")
            )"#,
        );
    }

    #[test]
    fn renumber_elem() {
        crate::mutators::match_mutation(
            r#"(module
                    (func (export "")
                        elem.drop 1
                    )
                    (func)
                    (elem func 0)
                    (elem func 1)
            )"#,
            RemoveItemMutator(Item::Element),
            r#"(module
                    (func (export "")
                        elem.drop 0
                    )
                    (func)
                    (elem func 1)
            )"#,
        );
    }

    #[test]
    fn renumber_type() {
        crate::mutators::match_mutation(
            r#"(module
                    (type (func))
                    (type (func (param i32)))
                    (func (type 1))
            )"#,
            RemoveItemMutator(Item::Type),
            r#"(module
                    (type (func (param i32)))
                    (func (type 0))
            )"#,
        );
    }

    #[test]
    fn renumber_global() {
        crate::mutators::match_mutation(
            r#"(module
                    (global i32 (i32.const 0))
                    (global i32 (i32.const 1))
                    (func (export "") (result i32) global.get 1)
            )"#,
            RemoveItemMutator(Item::Global),
            r#"(module
                    (global i32 (i32.const 1))
                    (func (export "") (result i32) global.get 0)
            )"#,
        );
    }

    #[test]
    fn remove_function_with_memarg64() {
        crate::mutators::match_mutation(
            r#"(module
                    (memory i64 0)
                    (func)
                    (func (export "")
                        i64.const 0
                        i64.load offset=1125899906842624
                        drop)
            )"#,
            RemoveItemMutator(Item::Function),
            r#"(module
                    (memory i64 0)
                    (func (export "")
                        i64.const 0
                        i64.load offset=1125899906842624
                        drop)
            )"#,
        );
    }

    #[test]
    fn remove_empty_element() {
        crate::mutators::match_mutation(
            r#"(module
                    (func (result funcref)
                        ref.func 0)
                    ;; cannot be removed otherwise the `ref.func 0` would be
                    ;; inapplicable
                    (elem declare func 0)
                    (elem declare func)
            )"#,
            RemoveItemMutator(Item::Element),
            r#"(module
                    (func (result funcref)
                        ref.func 0)
                    (elem declare func 0)
            )"#,
        );
    }
}
