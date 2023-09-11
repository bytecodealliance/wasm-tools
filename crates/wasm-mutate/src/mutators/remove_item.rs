//! This mutator attempts to remove various kinds of items from a wasm module.
//!
//! Removing an item from a wasm module is a somewhat tricky process because
//! there are many locations where indices are referenced. That means that when
//! this mutator is used it will need to renumber all indices in the wasm module
//! for references after the item that's removed. Notably this means that this
//! mutator largely translates between `wasmparser` structures and
//! `wasm_encoder` structures.

use crate::mutators::translate::ConstExprKind;
use crate::mutators::{translate, Item, Mutator, Translator};
use crate::Error;
use crate::{ModuleInfo, Result, WasmMutate};
use rand::Rng;
use std::collections::HashSet;
use wasm_encoder::*;
use wasmparser::{
    BinaryReader, CodeSectionReader, DataSectionReader, ElementSectionReader, ExportSectionReader,
    ExternalKind, FromReader, FunctionSectionReader, GlobalSectionReader, ImportSectionReader,
    MemorySectionReader, Operator, TableInit, TableSectionReader, TagSectionReader,
    TypeSectionReader,
};

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
            item: self.0,
            idx,
            referenced_functions: HashSet::new(),
            function_reference_action: Funcref::Save,
        }
        .remove(config.info());
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

struct RemoveItem {
    item: Item,
    idx: u32,
    function_reference_action: Funcref,
    referenced_functions: HashSet<u32>,
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

impl RemoveItem {
    fn remove(&mut self, info: &ModuleInfo) -> Result<Module> {
        // This is the main workhorse loop of the module translation. This will
        // iterate over the original wasm sections, raw, and create the new
        // module section-by-section. Sections are rewritten on-the-fly.
        let mut module = Module::new();
        for section in info.raw_sections.iter() {
            crate::module::match_section_id! {
                match section.id;

                Custom => {
                    module.section(section);
                },

                Type => {
                    self.filter_out(
                        &mut module,
                        0,
                        TypeSectionReader::new(section.data, 0)?.into_iter_err_on_gc_types(),
                        Item::Type,
                        |me, ty, section| {
                            me.translate_func_type(ty,section)?;
                            Ok(())
                        },
                    )?;
                },

                Import => {
                    // The import section is a little special because it defines
                    // items in multiple index spaces. This means that the
                    // `filter_out` helper can't be used and we have to process
                    // everything manually here.
                    let mut result = ImportSection::new();
                    let mut function = 0;
                    let mut global = 0;
                    let mut table = 0;
                    let mut memory = 0;
                    let mut tag = 0;
                    for item in ImportSectionReader::new(section.data, 0)? {
                        let item = item?;
                        match &item.ty {
                            wasmparser::TypeRef::Func(ty) => {
                                if self.item != Item::Function || self.idx != function {
                                    let ty = self.remap(Item::Type, *ty)?;
                                    result.import(item.module, item.name, EntityType::Function(ty));
                                }
                                function += 1;
                            }
                            wasmparser::TypeRef::Table(ty) => {
                                if self.item != Item::Table || self.idx != table {
                                    let ty = self.translate_table_type(ty)?;
                                    result.import(item.module, item.name, ty);
                                }
                                table += 1;
                            }
                            wasmparser::TypeRef::Memory(ty) => {
                                if self.item != Item::Memory || self.idx != memory {
                                    let ty = self.translate_memory_type(ty)?;
                                    result.import(item.module, item.name, ty);
                                }
                                memory += 1;
                            }
                            wasmparser::TypeRef::Global(ty) => {
                                if self.item != Item::Global || self.idx != global {
                                    let ty = self.translate_global_type(ty)?;
                                    result.import(item.module, item.name, ty);
                                }
                                global += 1;
                            }
                            wasmparser::TypeRef::Tag(ty) => {
                                if self.item != Item::Tag || self.idx != tag {
                                    let ty = self.translate_tag_type(ty)?;
                                    result.import(item.module, item.name, ty);
                                }
                                tag += 1;
                            }
                        }
                    }
                    module.section(&result);
                },

                Function => {
                    self.filter_out(
                        &mut module,
                        info.num_imported_functions(),
                        FunctionSectionReader::new(section.data, 0)?,
                        Item::Function,
                        |me, idx, section: &mut FunctionSection| {
                            let idx = me.remap(Item::Type, idx)?;
                            section.function(idx);
                            Ok(())
                        },
                    )?;
                },

                Table => {
                    self.filter_out(
                        &mut module,
                        info.num_imported_tables(),
                        TableSectionReader::new(section.data, 0)?,
                        Item::Table,
                        |me, table, section: &mut TableSection| {
                            let ty = me.translate_table_type(&table.ty)?;
                            match &table.init {
                                TableInit::RefNull => {
                                    section.table(ty);
                                }
                                TableInit::Expr(expr) => {
                                    let init = me.translate_const_expr(
                                        expr,
                                        &table.ty.element_type.into(),
                                        ConstExprKind::TableInit,
                                    )?;
                                    section.table_with_init(ty, &init);
                                }
                            }
                            Ok(())
                        },
                    )?;
                },

                Memory => {
                    self.filter_out(
                        &mut module,
                        info.num_imported_memories(),
                        MemorySectionReader::new(section.data, 0)?,
                        Item::Memory,
                        |me, ty, section: &mut MemorySection| {
                            let ty = me.translate_memory_type(&ty)?;
                            section.memory(ty);
                            Ok(())
                        },
                    )?;
                },

                Global => {
                    self.filter_out(
                        &mut module,
                        info.num_imported_globals(),
                        GlobalSectionReader::new(section.data, 0)?,
                        Item::Global,
                        |me, ty, section| me.translate_global(ty, section),
                    )?;
                },

                Export => {
                    let mut result = ExportSection::new();
                    for item in ExportSectionReader::new(section.data, 0)? {
                        let item = item?;
                        let (kind, index) = match &item.kind {
                            ExternalKind::Func => {
                                (ExportKind::Func, self.remap(Item::Function, item.index)?)
                            }
                            ExternalKind::Table => {
                                (ExportKind::Table, self.remap(Item::Table, item.index)?)
                            }
                            ExternalKind::Memory => {
                                (ExportKind::Memory, self.remap(Item::Memory, item.index)?)
                            }
                            ExternalKind::Tag => (ExportKind::Tag, self.remap(Item::Tag, item.index)?),
                            ExternalKind::Global => {
                                (ExportKind::Global, self.remap(Item::Global, item.index)?)
                            }
                        };
                        result.export(item.name, kind, index);
                    }
                    module.section(&result);
                },

                Start => {
                    let function_index = BinaryReader::new(section.data).read_var_u32()?;
                    self.function_reference_action = Funcref::Skip;
                    let function_index = self.remap(Item::Function, function_index)?;
                    self.function_reference_action = Funcref::Save;
                    module.section(&StartSection { function_index });
                },

                Element => {
                    self.filter_out(
                        &mut module,
                        0,
                        ElementSectionReader::new(section.data, 0)?,
                        Item::Element,
                        |me, ty, section| me.translate_element(ty, section),
                    )?;
                },

                Code => {
                    // In the code section we require that all functions
                    // referenced in `ref.func` are referenced elsewhere in the
                    // module, so indicate so in our internal state here.
                    self.function_reference_action = Funcref::RequireReferenced;
                    self.filter_out(
                        &mut module,
                        info.num_imported_functions(),
                        CodeSectionReader::new(section.data, 0)?,
                        Item::Function,
                        |me, body, section| me.translate_code(body, section),
                    )?;
                },

                Data => {
                    self.filter_out(
                        &mut module,
                        0,
                        DataSectionReader::new(section.data, 0)?,
                        Item::Data,
                        |me, ty, section| me.translate_data(ty, section),
                    )?;
                },

                DataCount => {
                    let count = BinaryReader::new(section.data).read_var_u32()?;
                    // Note that the data count section is decremented here if
                    // we're removing a data item, otherwise it's preserved
                    // as-is.
                    let count = if self.item == Item::Data {
                        count - 1
                    } else {
                        count
                    };
                    module.section(&DataCountSection { count });
                },

                Tag => {
                    self.filter_out(
                        &mut module,
                        info.num_imported_tags(),
                        TagSectionReader::new(section.data, 0)?,
                        Item::Tag,
                        |me, ty, section: &mut TagSection| {
                            let ty = me.translate_tag_type(&ty)?;
                            section.tag(ty);
                            Ok(())
                        },
                    )?;
                },

                _ => panic!("unknown id: {}", section.id),
            };
        }
        Ok(module)
    }

    /// This is a helper function to filter out the items of the `section`
    /// provided.
    ///
    /// The `section` given, which has items of type `section_item`, will be
    /// iterated over and translated with the `encode` callback. The `encode`
    /// callback is only called for items we're actually preserving in this
    /// module. The section is finally added to `module` at the end of
    /// translation.
    ///
    /// The `offset` provided is the initial offset in the index space, for
    /// example the global section starts at the offset equal to the number of
    /// imported globals because local globals are numbered afterwards.
    fn filter_out<'a, S, T>(
        &mut self,
        module: &mut Module,
        offset: u32,
        section: impl IntoIterator<Item = wasmparser::Result<S>>,
        section_item: Item,
        encode: impl Fn(&mut Self, S, &mut T) -> Result<()>,
    ) -> Result<()>
    where
        S: FromReader<'a>,
        T: Default + Section,
    {
        let mut result = T::default();
        let mut index = offset;
        for item in section {
            let item = item?;
            if index != self.idx || section_item != self.item {
                encode(self, item, &mut result)?;
            }
            index += 1;
        }
        module.section(&result);
        Ok(())
    }
}

impl Translator for RemoveItem {
    fn as_obj(&mut self) -> &mut dyn Translator {
        self
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
    fn remap(&mut self, item: Item, idx: u32) -> Result<u32> {
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
            Ok(idx)
        } else if idx == self.idx {
            // If we're removing a referenced item then that means that this
            // mutation fails.
            Err(Error::no_mutations_applicable())
        } else {
            // Otherwise this item comes after the item being removed, so
            // this item's index has decreased by one.
            Ok(idx - 1)
        }
    }

    fn translate_op(&mut self, op: &Operator<'_>) -> Result<Instruction<'static>> {
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
        if let Operator::RefFunc { function_index } = op {
            if let Funcref::RequireReferenced = self.function_reference_action {
                if !self.referenced_functions.contains(function_index) {
                    return Err(Error::no_mutations_applicable());
                }
            }
        }

        translate::op(self, op)
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
