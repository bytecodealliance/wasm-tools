//! This mutator attempts to remove various kinds of items from a wasm module.
//!
//! Removing an item from a wasm module is a somewhat tricky process because
//! there are many locations where indices are referenced. That means that when
//! this mutator is used it will need to renumber all indices in the wasm module
//! for references after the item that's removed. Notably this means that this
//! mutator largely translates between `wasmparser` structures and
//! `wasm_encoder` structures.

use super::Mutator;
use crate::Error;
use crate::{ModuleInfo, Result, WasmMutate};
use rand::Rng;
use std::collections::HashSet;
use wasm_encoder::*;
use wasmparser::*;

/// Mutator that removes a random item in a wasm module (function, global,
/// table, etc).
#[derive(Copy, Clone)]
pub struct RemoveItemMutator(pub Item);

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub enum Item {
    Function,
    Table,
    Memory,
    Tag,
    Global,
    Type,
    Data,
    Element,
}

impl Mutator for RemoveItemMutator {
    fn can_mutate(&self, config: &WasmMutate) -> bool {
        self.0.can_mutate(config)
    }

    fn mutate<'a>(
        self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<wasm_encoder::Module>> + 'a>>
    where
        Self: Copy,
    {
        let idx = self.0.choose_removal_index(config);
        log::trace!("attempting to remove {:?} index {}", self.0, idx);

        let result = RemoveItem {
            item: self.0,
            idx,
            referenced_functions: HashSet::new(),
            function_reference_action: Funcref::Save,
        }
        .remove(config.info())?;
        log::debug!("removed {:?} index {}", self.0, idx);
        Ok(Box::new(std::iter::once(Ok(result))))
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
        config.rng().gen_range(0, max)
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
        const CUSTOM: u8 = SectionId::Custom as u8;
        const TYPE: u8 = SectionId::Type as u8;
        const IMPORT: u8 = SectionId::Import as u8;
        const FUNCTION: u8 = SectionId::Function as u8;
        const TABLE: u8 = SectionId::Table as u8;
        const MEMORY: u8 = SectionId::Memory as u8;
        const GLOBAL: u8 = SectionId::Global as u8;
        const EXPORT: u8 = SectionId::Export as u8;
        const START: u8 = SectionId::Start as u8;
        const ELEMENT: u8 = SectionId::Element as u8;
        const CODE: u8 = SectionId::Code as u8;
        const DATA: u8 = SectionId::Data as u8;
        const DATACOUNT: u8 = SectionId::DataCount as u8;
        const TAG: u8 = SectionId::Tag as u8;
        const MODULE: u8 = SectionId::Module as u8;
        const INSTANCE: u8 = SectionId::Instance as u8;
        const ALIAS: u8 = SectionId::Alias as u8;

        // This is the main workhorse loop of the module translation. This will
        // iterate over the original wasm sections, raw, and create the new
        // module section-by-section. Sections are rewritten on-the-fly.
        let mut module = Module::new();
        for section in info.raw_sections.iter() {
            match section.id {
                CUSTOM => {
                    module.section(section);
                }

                TYPE => {
                    self.filter_out(
                        &mut module,
                        0,
                        TypeSectionReader::new(section.data, 0)?,
                        Item::Type,
                        |me, ty, section| me.translate_type_def(ty, section),
                    )?;
                }

                // The import section is a little special because it defines
                // items in multiple index spaces. This means that the
                // `filter_out` helper can't be used and we have to process
                // everything manually here.
                IMPORT => {
                    let mut result = ImportSection::default();
                    let mut function = 0;
                    let mut global = 0;
                    let mut table = 0;
                    let mut memory = 0;
                    let mut tag = 0;
                    for item in ImportSectionReader::new(section.data, 0)? {
                        let item = item?;
                        match &item.ty {
                            ImportSectionEntryType::Function(ty) => {
                                if self.item != Item::Function || self.idx != function {
                                    let ty = self.remap(Item::Type, *ty)?;
                                    result.import(
                                        item.module,
                                        item.field,
                                        EntityType::Function(ty),
                                    );
                                }
                                function += 1;
                            }
                            ImportSectionEntryType::Table(ty) => {
                                if self.item != Item::Table || self.idx != table {
                                    let ty = self.translate_table_type(ty)?;
                                    result.import(item.module, item.field, ty);
                                }
                                table += 1;
                            }
                            ImportSectionEntryType::Memory(ty) => {
                                if self.item != Item::Memory || self.idx != memory {
                                    let ty = self.translate_memory_type(ty)?;
                                    result.import(item.module, item.field, ty);
                                }
                                memory += 1;
                            }
                            ImportSectionEntryType::Global(ty) => {
                                if self.item != Item::Global || self.idx != global {
                                    let ty = self.translate_global_type(ty)?;
                                    result.import(item.module, item.field, ty);
                                }
                                global += 1;
                            }
                            ImportSectionEntryType::Tag(ty) => {
                                if self.item != Item::Tag || self.idx != tag {
                                    let ty = self.translate_tag_type(ty)?;
                                    result.import(item.module, item.field, ty);
                                }
                                tag += 1;
                            }
                            ImportSectionEntryType::Instance(_)
                            | ImportSectionEntryType::Module(_) => {
                                return Err(Error::no_mutations_applicable())
                            }
                        }
                    }
                    module.section(&result);
                }

                FUNCTION => {
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
                }

                TABLE => {
                    self.filter_out(
                        &mut module,
                        info.num_imported_tables(),
                        TableSectionReader::new(section.data, 0)?,
                        Item::Table,
                        |me, ty, section: &mut TableSection| {
                            let ty = me.translate_table_type(&ty)?;
                            section.table(ty);
                            Ok(())
                        },
                    )?;
                }

                MEMORY => {
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
                }

                GLOBAL => {
                    self.filter_out(
                        &mut module,
                        info.num_imported_globals(),
                        GlobalSectionReader::new(section.data, 0)?,
                        Item::Global,
                        |me, ty, section| me.translate_global(ty, section),
                    )?;
                }

                EXPORT => {
                    use wasm_encoder::Export;

                    let mut result = ExportSection::default();
                    for item in ExportSectionReader::new(section.data, 0)? {
                        let item = item?;
                        let e = match &item.kind {
                            ExternalKind::Function => {
                                Export::Function(self.remap(Item::Function, item.index)?)
                            }
                            ExternalKind::Table => {
                                Export::Table(self.remap(Item::Table, item.index)?)
                            }
                            ExternalKind::Memory => {
                                Export::Memory(self.remap(Item::Memory, item.index)?)
                            }
                            ExternalKind::Tag => Export::Tag(self.remap(Item::Tag, item.index)?),
                            ExternalKind::Global => {
                                Export::Global(self.remap(Item::Global, item.index)?)
                            }
                            ExternalKind::Type | ExternalKind::Instance | ExternalKind::Module => {
                                return Err(Error::no_mutations_applicable())
                            }
                        };
                        result.export(item.field, e);
                    }
                    module.section(&result);
                }

                START => {
                    let function_index = BinaryReader::new(section.data).read_var_u32()?;
                    self.function_reference_action = Funcref::Skip;
                    let function_index = self.remap(Item::Function, function_index)?;
                    self.function_reference_action = Funcref::Save;
                    module.section(&StartSection { function_index });
                }

                ELEMENT => {
                    self.filter_out(
                        &mut module,
                        0,
                        ElementSectionReader::new(section.data, 0)?,
                        Item::Element,
                        |me, ty, section| me.translate_element(ty, section),
                    )?;
                }

                CODE => {
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
                }

                DATA => {
                    self.filter_out(
                        &mut module,
                        0,
                        DataSectionReader::new(section.data, 0)?,
                        Item::Data,
                        |me, ty, section| me.translate_data(ty, section),
                    )?;
                }

                DATACOUNT => {
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
                }

                TAG => {
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
                }

                // Module linking is not supported at this time.
                MODULE | INSTANCE | ALIAS => return Err(Error::no_mutations_applicable()),

                id => panic!("unknown id: {}", id),
            }
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
    fn filter_out<S, T>(
        &mut self,
        module: &mut Module,
        offset: u32,
        mut section: S,
        section_item: Item,
        encode: impl Fn(&mut Self, S::Item, &mut T) -> Result<()>,
    ) -> Result<()>
    where
        S: SectionReader,
        T: Default + Section,
    {
        let mut result = T::default();
        let mut index = offset;
        while !section.eof() {
            let item = section.read()?;
            if index != self.idx || section_item != self.item {
                encode(self, item, &mut result)?;
            }
            index += 1;
        }
        module.section(&result);
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
    fn remap(&mut self, item: Item, idx: u32) -> Result<u32> {
        if item != self.item {
            // Different kind of item, no change
            Ok(idx)
        } else if idx < self.idx {
            // A later item was removed, so this index doesn't change
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

    fn translate_type_def(&mut self, ty: TypeDef, s: &mut TypeSection) -> Result<()> {
        match ty {
            TypeDef::Func(f) => {
                s.function(
                    f.params
                        .iter()
                        .map(|t| self.translate_type(t))
                        .collect::<Result<Vec<_>>>()?,
                    f.returns
                        .iter()
                        .map(|t| self.translate_type(t))
                        .collect::<Result<Vec<_>>>()?,
                );
                Ok(())
            }

            // Module linking is not supported at this time.
            TypeDef::Instance(_) | TypeDef::Module(_) => Err(Error::no_mutations_applicable()),
        }
    }

    fn translate_table_type(
        &mut self,
        ty: &wasmparser::TableType,
    ) -> Result<wasm_encoder::TableType> {
        Ok(wasm_encoder::TableType {
            element_type: self.translate_type(&ty.element_type)?,
            minimum: ty.initial,
            maximum: ty.maximum,
        })
    }

    fn translate_memory_type(
        &mut self,
        ty: &wasmparser::MemoryType,
    ) -> Result<wasm_encoder::MemoryType> {
        Ok(wasm_encoder::MemoryType {
            memory64: ty.memory64,
            minimum: ty.initial,
            maximum: ty.maximum,
        })
    }

    fn translate_global_type(
        &mut self,
        ty: &wasmparser::GlobalType,
    ) -> Result<wasm_encoder::GlobalType> {
        Ok(wasm_encoder::GlobalType {
            val_type: self.translate_type(&ty.content_type)?,
            mutable: ty.mutable,
        })
    }

    fn translate_tag_type(&mut self, ty: &wasmparser::TagType) -> Result<wasm_encoder::TagType> {
        Ok(wasm_encoder::TagType {
            kind: TagKind::Exception,
            func_type_idx: self.remap(Item::Type, ty.type_index)?,
        })
    }

    fn translate_type(&mut self, ty: &Type) -> Result<ValType> {
        match ty {
            Type::I32 => Ok(ValType::I32),
            Type::I64 => Ok(ValType::I64),
            Type::F32 => Ok(ValType::F32),
            Type::F64 => Ok(ValType::F64),
            Type::V128 => Ok(ValType::V128),
            Type::FuncRef => Ok(ValType::FuncRef),
            Type::ExternRef => Ok(ValType::ExternRef),

            // not supported in wasm-encoder
            Type::ExnRef => Err(Error::no_mutations_applicable()),

            // Shouldn't ever show up as these are used in different contexts
            // within wasmparser.
            Type::Func | Type::EmptyBlockType => Err(Error::no_mutations_applicable()),
        }
    }

    fn translate_global(&mut self, global: Global, s: &mut GlobalSection) -> Result<()> {
        let ty = self.translate_global_type(&global.ty)?;
        let insn = self.translate_init_expr(&global.init_expr)?;
        s.global(ty, &insn);
        Ok(())
    }

    fn translate_init_expr(&mut self, e: &InitExpr<'_>) -> Result<Instruction<'static>> {
        let mut e = e.get_operators_reader();
        let op = e.read()?;
        let op = self.translate_op(&op)?;
        match e.read()? {
            Operator::End if e.eof() => {}
            _ => return Err(Error::no_mutations_applicable()),
        }
        Ok(op)
    }

    fn translate_element(
        &mut self,
        element: wasmparser::Element<'_>,
        s: &mut ElementSection,
    ) -> Result<()> {
        let offset;
        let mode = match &element.kind {
            ElementKind::Active {
                table_index,
                init_expr,
            } => {
                offset = self.translate_init_expr(init_expr)?;
                ElementMode::Active {
                    table: Some(self.remap(Item::Table, *table_index)?),
                    offset: &offset,
                }
            }
            ElementKind::Passive => ElementMode::Passive,
            ElementKind::Declared => ElementMode::Declared,
        };
        let element_type = self.translate_type(&element.ty)?;
        let mut functions = Vec::new();
        let mut exprs = Vec::new();
        let mut reader = element.items.get_items_reader()?;
        for _ in 0..reader.get_count() {
            match reader.read()? {
                ElementItem::Func(idx) => {
                    functions.push(self.remap(Item::Function, idx)?);
                }
                ElementItem::Expr(expr) => match self.translate_init_expr(&expr)? {
                    Instruction::RefFunc(n) => {
                        exprs.push(wasm_encoder::Element::Func(n));
                    }
                    Instruction::RefNull(_) => {
                        exprs.push(wasm_encoder::Element::Null);
                    }
                    _ => return Err(Error::no_mutations_applicable()),
                },
            }
        }
        s.segment(ElementSegment {
            mode,
            element_type,
            elements: if reader.uses_exprs() {
                Elements::Expressions(&exprs)
            } else {
                Elements::Functions(&functions)
            },
        });
        Ok(())
    }

    fn translate_data(&mut self, data: wasmparser::Data<'_>, s: &mut DataSection) -> Result<()> {
        let offset;
        let mode = match &data.kind {
            DataKind::Active {
                memory_index,
                init_expr,
            } => {
                offset = self.translate_init_expr(init_expr)?;
                DataSegmentMode::Active {
                    memory_index: self.remap(Item::Memory, *memory_index)?,
                    offset: &offset,
                }
            }
            DataKind::Passive => DataSegmentMode::Passive,
        };
        s.segment(DataSegment {
            mode,
            data: data.data.iter().copied(),
        });
        Ok(())
    }

    fn translate_code(&mut self, body: FunctionBody<'_>, s: &mut CodeSection) -> Result<()> {
        let locals = body
            .get_locals_reader()?
            .into_iter()
            .map(|local| {
                let (cnt, ty) = local?;
                Ok((cnt, self.translate_type(&ty)?))
            })
            .collect::<Result<Vec<_>>>()?;
        let mut func = Function::new(locals);

        for op in body.get_operators_reader()? {
            let op = op?;
            func.instruction(&self.translate_op(&op)?);
        }
        s.function(&func);
        Ok(())
    }

    /// This is a pretty gnarly function that translates from `wasmparser`
    /// operators to `wasm_encoder` operators. It's quite large because there's
    /// quite a few wasm instructions. The theory though is that at least each
    /// individual case is pretty self-contained.
    fn translate_op(&mut self, op: &Operator<'_>) -> Result<Instruction<'static>> {
        use wasm_encoder::Instruction as I;
        use wasmparser::Operator as O;
        Ok(match op {
            O::Unreachable => I::Unreachable,
            O::Nop => I::Nop,

            O::Block { ty } => I::Block(self.translate_block_type(ty)?),
            O::Loop { ty } => I::Loop(self.translate_block_type(ty)?),
            O::If { ty } => I::If(self.translate_block_type(ty)?),
            O::Else => I::Else,

            O::Try { ty } => I::Try(self.translate_block_type(ty)?),
            O::Catch { index } => I::Catch(self.remap(Item::Tag, *index)?),
            O::Throw { index } => I::Throw(self.remap(Item::Tag, *index)?),
            O::Rethrow { relative_depth } => I::Rethrow(*relative_depth),
            O::End => I::End,
            O::Br { relative_depth } => I::Br(*relative_depth),
            O::BrIf { relative_depth } => I::BrIf(*relative_depth),
            O::BrTable { table } => I::BrTable(
                table
                    .targets()
                    .collect::<Result<Vec<_>, wasmparser::BinaryReaderError>>()?
                    .into(),
                table.default(),
            ),

            O::Return => I::Return,
            O::Call { function_index } => I::Call(self.remap(Item::Function, *function_index)?),
            O::CallIndirect { index, table_index } => I::CallIndirect {
                ty: self.remap(Item::Type, *index)?,
                table: self.remap(Item::Table, *table_index)?,
            },
            O::Delegate { relative_depth } => I::Delegate(*relative_depth),
            O::CatchAll => I::CatchAll,
            O::Drop => I::Drop,
            O::Select => I::Select,
            O::TypedSelect { ty } => I::TypedSelect(self.translate_type(ty)?),

            O::LocalGet { local_index } => I::LocalGet(*local_index),
            O::LocalSet { local_index } => I::LocalSet(*local_index),
            O::LocalTee { local_index } => I::LocalTee(*local_index),

            O::GlobalGet { global_index } => I::GlobalGet(self.remap(Item::Global, *global_index)?),
            O::GlobalSet { global_index } => I::GlobalSet(self.remap(Item::Global, *global_index)?),

            O::I32Load { memarg } => I::I32Load(self.memarg(memarg)?),
            O::I64Load { memarg } => I::I64Load(self.memarg(memarg)?),
            O::F32Load { memarg } => I::F32Load(self.memarg(memarg)?),
            O::F64Load { memarg } => I::F64Load(self.memarg(memarg)?),
            O::I32Load8S { memarg } => I::I32Load8_S(self.memarg(memarg)?),
            O::I32Load8U { memarg } => I::I32Load8_U(self.memarg(memarg)?),
            O::I32Load16S { memarg } => I::I32Load16_S(self.memarg(memarg)?),
            O::I32Load16U { memarg } => I::I32Load16_U(self.memarg(memarg)?),
            O::I64Load8S { memarg } => I::I64Load8_S(self.memarg(memarg)?),
            O::I64Load8U { memarg } => I::I64Load8_U(self.memarg(memarg)?),
            O::I64Load16S { memarg } => I::I64Load16_S(self.memarg(memarg)?),
            O::I64Load16U { memarg } => I::I64Load16_U(self.memarg(memarg)?),
            O::I64Load32S { memarg } => I::I64Load32_S(self.memarg(memarg)?),
            O::I64Load32U { memarg } => I::I64Load32_U(self.memarg(memarg)?),
            O::I32Store { memarg } => I::I32Store(self.memarg(memarg)?),
            O::I64Store { memarg } => I::I64Store(self.memarg(memarg)?),
            O::F32Store { memarg } => I::F32Store(self.memarg(memarg)?),
            O::F64Store { memarg } => I::F64Store(self.memarg(memarg)?),
            O::I32Store8 { memarg } => I::I32Store8(self.memarg(memarg)?),
            O::I32Store16 { memarg } => I::I32Store16(self.memarg(memarg)?),
            O::I64Store8 { memarg } => I::I64Store8(self.memarg(memarg)?),
            O::I64Store16 { memarg } => I::I64Store16(self.memarg(memarg)?),
            O::I64Store32 { memarg } => I::I64Store32(self.memarg(memarg)?),

            O::MemorySize { mem, .. } => I::MemorySize(self.remap(Item::Memory, *mem)?),
            O::MemoryGrow { mem, .. } => I::MemoryGrow(self.remap(Item::Memory, *mem)?),

            O::I32Const { value } => I::I32Const(*value),
            O::I64Const { value } => I::I64Const(*value),
            O::F32Const { value } => I::F32Const(f32::from_bits(value.bits())),
            O::F64Const { value } => I::F64Const(f64::from_bits(value.bits())),

            O::RefNull { ty } => I::RefNull(self.translate_type(ty)?),
            O::RefIsNull => I::RefIsNull,
            O::RefFunc { function_index } => {
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
                let idx = *function_index;
                match self.function_reference_action {
                    Funcref::Save => {
                        self.referenced_functions.insert(idx);
                    }
                    Funcref::Skip => {}
                    Funcref::RequireReferenced => {
                        if !self.referenced_functions.contains(&idx) {
                            return Err(Error::no_mutations_applicable());
                        }
                    }
                }

                I::RefFunc(self.remap(Item::Function, idx)?)
            }

            O::I32Eqz => I::I32Eqz,
            O::I32Eq => I::I32Eq,
            O::I32Ne => I::I32Ne,
            O::I32LtS => I::I32LtS,
            O::I32LtU => I::I32LtU,
            O::I32GtS => I::I32GtS,
            O::I32GtU => I::I32GtU,
            O::I32LeS => I::I32LeS,
            O::I32LeU => I::I32LeU,
            O::I32GeS => I::I32GeS,
            O::I32GeU => I::I32GeU,
            O::I64Eqz => I::I64Eqz,
            O::I64Eq => I::I64Eq,
            O::I64Ne => I::I64Ne,
            O::I64LtS => I::I64LtS,
            O::I64LtU => I::I64LtU,
            O::I64GtS => I::I64GtS,
            O::I64GtU => I::I64GtU,
            O::I64LeS => I::I64LeS,
            O::I64LeU => I::I64LeU,
            O::I64GeS => I::I64GeS,
            O::I64GeU => I::I64GeU,
            O::F32Eq => I::F32Eq,
            O::F32Ne => I::F32Ne,
            O::F32Lt => I::F32Lt,
            O::F32Gt => I::F32Gt,
            O::F32Le => I::F32Le,
            O::F32Ge => I::F32Ge,
            O::F64Eq => I::F64Eq,
            O::F64Ne => I::F64Ne,
            O::F64Lt => I::F64Lt,
            O::F64Gt => I::F64Gt,
            O::F64Le => I::F64Le,
            O::F64Ge => I::F64Ge,
            O::I32Clz => I::I32Clz,
            O::I32Ctz => I::I32Ctz,
            O::I32Popcnt => I::I32Popcnt,
            O::I32Add => I::I32Add,
            O::I32Sub => I::I32Sub,
            O::I32Mul => I::I32Mul,
            O::I32DivS => I::I32DivS,
            O::I32DivU => I::I32DivU,
            O::I32RemS => I::I32RemS,
            O::I32RemU => I::I32RemU,
            O::I32And => I::I32And,
            O::I32Or => I::I32Or,
            O::I32Xor => I::I32Xor,
            O::I32Shl => I::I32Shl,
            O::I32ShrS => I::I32ShrS,
            O::I32ShrU => I::I32ShrU,
            O::I32Rotl => I::I32Rotl,
            O::I32Rotr => I::I32Rotr,
            O::I64Clz => I::I64Clz,
            O::I64Ctz => I::I64Ctz,
            O::I64Popcnt => I::I64Popcnt,
            O::I64Add => I::I64Add,
            O::I64Sub => I::I64Sub,
            O::I64Mul => I::I64Mul,
            O::I64DivS => I::I64DivS,
            O::I64DivU => I::I64DivU,
            O::I64RemS => I::I64RemS,
            O::I64RemU => I::I64RemU,
            O::I64And => I::I64And,
            O::I64Or => I::I64Or,
            O::I64Xor => I::I64Xor,
            O::I64Shl => I::I64Shl,
            O::I64ShrS => I::I64ShrS,
            O::I64ShrU => I::I64ShrU,
            O::I64Rotl => I::I64Rotl,
            O::I64Rotr => I::I64Rotr,
            O::F32Abs => I::F32Abs,
            O::F32Neg => I::F32Neg,
            O::F32Ceil => I::F32Ceil,
            O::F32Floor => I::F32Floor,
            O::F32Trunc => I::F32Trunc,
            O::F32Nearest => I::F32Nearest,
            O::F32Sqrt => I::F32Sqrt,
            O::F32Add => I::F32Add,
            O::F32Sub => I::F32Sub,
            O::F32Mul => I::F32Mul,
            O::F32Div => I::F32Div,
            O::F32Min => I::F32Min,
            O::F32Max => I::F32Max,
            O::F32Copysign => I::F32Copysign,
            O::F64Abs => I::F64Abs,
            O::F64Neg => I::F64Neg,
            O::F64Ceil => I::F64Ceil,
            O::F64Floor => I::F64Floor,
            O::F64Trunc => I::F64Trunc,
            O::F64Nearest => I::F64Nearest,
            O::F64Sqrt => I::F64Sqrt,
            O::F64Add => I::F64Add,
            O::F64Sub => I::F64Sub,
            O::F64Mul => I::F64Mul,
            O::F64Div => I::F64Div,
            O::F64Min => I::F64Min,
            O::F64Max => I::F64Max,
            O::F64Copysign => I::F64Copysign,
            O::I32WrapI64 => I::I32WrapI64,
            O::I32TruncF32S => I::I32TruncF32S,
            O::I32TruncF32U => I::I32TruncF32U,
            O::I32TruncF64S => I::I32TruncF64S,
            O::I32TruncF64U => I::I32TruncF64U,
            O::I64ExtendI32S => I::I64ExtendI32S,
            O::I64ExtendI32U => I::I64ExtendI32U,
            O::I64TruncF32S => I::I64TruncF32S,
            O::I64TruncF32U => I::I64TruncF32U,
            O::I64TruncF64S => I::I64TruncF64S,
            O::I64TruncF64U => I::I64TruncF64U,
            O::F32ConvertI32S => I::F32ConvertI32S,
            O::F32ConvertI32U => I::F32ConvertI32U,
            O::F32ConvertI64S => I::F32ConvertI64S,
            O::F32ConvertI64U => I::F32ConvertI64U,
            O::F32DemoteF64 => I::F32DemoteF64,
            O::F64ConvertI32S => I::F64ConvertI32S,
            O::F64ConvertI32U => I::F64ConvertI32U,
            O::F64ConvertI64S => I::F64ConvertI64S,
            O::F64ConvertI64U => I::F64ConvertI64U,
            O::F64PromoteF32 => I::F64PromoteF32,
            O::I32ReinterpretF32 => I::I32ReinterpretF32,
            O::I64ReinterpretF64 => I::I64ReinterpretF64,
            O::F32ReinterpretI32 => I::F32ReinterpretI32,
            O::F64ReinterpretI64 => I::F64ReinterpretI64,
            O::I32Extend8S => I::I32Extend8S,
            O::I32Extend16S => I::I32Extend16S,
            O::I64Extend8S => I::I64Extend8S,
            O::I64Extend16S => I::I64Extend16S,
            O::I64Extend32S => I::I64Extend32S,

            O::I32TruncSatF32S => I::I32TruncSatF32S,
            O::I32TruncSatF32U => I::I32TruncSatF32U,
            O::I32TruncSatF64S => I::I32TruncSatF64S,
            O::I32TruncSatF64U => I::I32TruncSatF64U,
            O::I64TruncSatF32S => I::I64TruncSatF32S,
            O::I64TruncSatF32U => I::I64TruncSatF32U,
            O::I64TruncSatF64S => I::I64TruncSatF64S,
            O::I64TruncSatF64U => I::I64TruncSatF64U,

            O::MemoryInit { segment, mem } => I::MemoryInit {
                data: self.remap(Item::Data, *segment)?,
                mem: self.remap(Item::Memory, *mem)?,
            },
            O::DataDrop { segment } => I::DataDrop(self.remap(Item::Data, *segment)?),
            O::MemoryCopy { src, dst } => I::MemoryCopy {
                src: self.remap(Item::Memory, *src)?,
                dst: self.remap(Item::Memory, *dst)?,
            },
            O::MemoryFill { mem, .. } => I::MemoryFill(self.remap(Item::Memory, *mem)?),

            O::TableInit { segment, table } => I::TableInit {
                segment: self.remap(Item::Element, *segment)?,
                table: self.remap(Item::Table, *table)?,
            },
            O::ElemDrop { segment } => I::ElemDrop {
                segment: self.remap(Item::Element, *segment)?,
            },
            O::TableCopy {
                dst_table,
                src_table,
            } => I::TableCopy {
                dst: self.remap(Item::Table, *dst_table)?,
                src: self.remap(Item::Table, *src_table)?,
            },
            O::TableFill { table } => I::TableFill {
                table: self.remap(Item::Table, *table)?,
            },
            O::TableGet { table } => I::TableGet {
                table: self.remap(Item::Table, *table)?,
            },
            O::TableSet { table } => I::TableSet {
                table: self.remap(Item::Table, *table)?,
            },
            O::TableGrow { table } => I::TableGrow {
                table: self.remap(Item::Table, *table)?,
            },
            O::TableSize { table } => I::TableSize {
                table: self.remap(Item::Table, *table)?,
            },

            O::V128Load { memarg } => I::V128Load {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load8x8S { memarg } => I::V128Load8x8S {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load8x8U { memarg } => I::V128Load8x8U {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load16x4S { memarg } => I::V128Load16x4S {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load16x4U { memarg } => I::V128Load16x4U {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load32x2S { memarg } => I::V128Load32x2S {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load32x2U { memarg } => I::V128Load32x2U {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load8Splat { memarg } => I::V128Load8Splat {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load16Splat { memarg } => I::V128Load16Splat {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load32Splat { memarg } => I::V128Load32Splat {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load64Splat { memarg } => I::V128Load64Splat {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load32Zero { memarg } => I::V128Load32Zero {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load64Zero { memarg } => I::V128Load64Zero {
                memarg: self.memarg(memarg)?,
            },
            O::V128Store { memarg } => I::V128Store {
                memarg: self.memarg(memarg)?,
            },
            O::V128Load8Lane { memarg, lane } => I::V128Load8Lane {
                memarg: self.memarg(memarg)?,
                lane: *lane,
            },
            O::V128Load16Lane { memarg, lane } => I::V128Load16Lane {
                memarg: self.memarg(memarg)?,
                lane: *lane,
            },
            O::V128Load32Lane { memarg, lane } => I::V128Load32Lane {
                memarg: self.memarg(memarg)?,
                lane: *lane,
            },
            O::V128Load64Lane { memarg, lane } => I::V128Load64Lane {
                memarg: self.memarg(memarg)?,
                lane: *lane,
            },
            O::V128Store8Lane { memarg, lane } => I::V128Store8Lane {
                memarg: self.memarg(memarg)?,
                lane: *lane,
            },
            O::V128Store16Lane { memarg, lane } => I::V128Store16Lane {
                memarg: self.memarg(memarg)?,
                lane: *lane,
            },
            O::V128Store32Lane { memarg, lane } => I::V128Store32Lane {
                memarg: self.memarg(memarg)?,
                lane: *lane,
            },
            O::V128Store64Lane { memarg, lane } => I::V128Store64Lane {
                memarg: self.memarg(memarg)?,
                lane: *lane,
            },

            O::V128Const { value } => I::V128Const(value.i128()),
            O::I8x16Shuffle { lanes } => I::I8x16Shuffle { lanes: *lanes },
            O::I8x16ExtractLaneS { lane } => I::I8x16ExtractLaneS { lane: *lane },
            O::I8x16ExtractLaneU { lane } => I::I8x16ExtractLaneU { lane: *lane },
            O::I8x16ReplaceLane { lane } => I::I8x16ReplaceLane { lane: *lane },
            O::I16x8ExtractLaneS { lane } => I::I16x8ExtractLaneS { lane: *lane },
            O::I16x8ExtractLaneU { lane } => I::I16x8ExtractLaneU { lane: *lane },
            O::I16x8ReplaceLane { lane } => I::I16x8ReplaceLane { lane: *lane },
            O::I32x4ExtractLane { lane } => I::I32x4ExtractLane { lane: *lane },
            O::I32x4ReplaceLane { lane } => I::I32x4ReplaceLane { lane: *lane },
            O::I64x2ExtractLane { lane } => I::I64x2ExtractLane { lane: *lane },
            O::I64x2ReplaceLane { lane } => I::I64x2ReplaceLane { lane: *lane },
            O::F32x4ExtractLane { lane } => I::F32x4ExtractLane { lane: *lane },
            O::F32x4ReplaceLane { lane } => I::F32x4ReplaceLane { lane: *lane },
            O::F64x2ExtractLane { lane } => I::F64x2ExtractLane { lane: *lane },
            O::F64x2ReplaceLane { lane } => I::F64x2ReplaceLane { lane: *lane },

            O::I8x16Swizzle => I::I8x16Swizzle,
            O::I8x16Splat => I::I8x16Splat,
            O::I16x8Splat => I::I16x8Splat,
            O::I32x4Splat => I::I32x4Splat,
            O::I64x2Splat => I::I64x2Splat,
            O::F32x4Splat => I::F32x4Splat,
            O::F64x2Splat => I::F64x2Splat,
            O::I8x16Eq => I::I8x16Eq,
            O::I8x16Ne => I::I8x16Ne,
            O::I8x16LtS => I::I8x16LtS,
            O::I8x16LtU => I::I8x16LtU,
            O::I8x16GtS => I::I8x16GtS,
            O::I8x16GtU => I::I8x16GtU,
            O::I8x16LeS => I::I8x16LeS,
            O::I8x16LeU => I::I8x16LeU,
            O::I8x16GeS => I::I8x16GeS,
            O::I8x16GeU => I::I8x16GeU,
            O::I16x8Eq => I::I16x8Eq,
            O::I16x8Ne => I::I16x8Ne,
            O::I16x8LtS => I::I16x8LtS,
            O::I16x8LtU => I::I16x8LtU,
            O::I16x8GtS => I::I16x8GtS,
            O::I16x8GtU => I::I16x8GtU,
            O::I16x8LeS => I::I16x8LeS,
            O::I16x8LeU => I::I16x8LeU,
            O::I16x8GeS => I::I16x8GeS,
            O::I16x8GeU => I::I16x8GeU,
            O::I32x4Eq => I::I32x4Eq,
            O::I32x4Ne => I::I32x4Ne,
            O::I32x4LtS => I::I32x4LtS,
            O::I32x4LtU => I::I32x4LtU,
            O::I32x4GtS => I::I32x4GtS,
            O::I32x4GtU => I::I32x4GtU,
            O::I32x4LeS => I::I32x4LeS,
            O::I32x4LeU => I::I32x4LeU,
            O::I32x4GeS => I::I32x4GeS,
            O::I32x4GeU => I::I32x4GeU,
            O::I64x2Eq => I::I64x2Eq,
            O::I64x2Ne => I::I64x2Ne,
            O::I64x2LtS => I::I64x2LtS,
            O::I64x2GtS => I::I64x2GtS,
            O::I64x2LeS => I::I64x2LeS,
            O::I64x2GeS => I::I64x2GeS,
            O::F32x4Eq => I::F32x4Eq,
            O::F32x4Ne => I::F32x4Ne,
            O::F32x4Lt => I::F32x4Lt,
            O::F32x4Gt => I::F32x4Gt,
            O::F32x4Le => I::F32x4Le,
            O::F32x4Ge => I::F32x4Ge,
            O::F64x2Eq => I::F64x2Eq,
            O::F64x2Ne => I::F64x2Ne,
            O::F64x2Lt => I::F64x2Lt,
            O::F64x2Gt => I::F64x2Gt,
            O::F64x2Le => I::F64x2Le,
            O::F64x2Ge => I::F64x2Ge,
            O::V128Not => I::V128Not,
            O::V128And => I::V128And,
            O::V128AndNot => I::V128AndNot,
            O::V128Or => I::V128Or,
            O::V128Xor => I::V128Xor,
            O::V128Bitselect => I::V128Bitselect,
            O::V128AnyTrue => I::V128AnyTrue,
            O::I8x16Abs => I::I8x16Abs,
            O::I8x16Neg => I::I8x16Neg,
            O::I8x16Popcnt => I::I8x16Popcnt,
            O::I8x16AllTrue => I::I8x16AllTrue,
            O::I8x16Bitmask => I::I8x16Bitmask,
            O::I8x16NarrowI16x8S => I::I8x16NarrowI16x8S,
            O::I8x16NarrowI16x8U => I::I8x16NarrowI16x8U,
            O::I8x16Shl => I::I8x16Shl,
            O::I8x16ShrS => I::I8x16ShrS,
            O::I8x16ShrU => I::I8x16ShrU,
            O::I8x16Add => I::I8x16Add,
            O::I8x16AddSatS => I::I8x16AddSatS,
            O::I8x16AddSatU => I::I8x16AddSatU,
            O::I8x16Sub => I::I8x16Sub,
            O::I8x16SubSatS => I::I8x16SubSatS,
            O::I8x16SubSatU => I::I8x16SubSatU,
            O::I8x16MinS => I::I8x16MinS,
            O::I8x16MinU => I::I8x16MinU,
            O::I8x16MaxS => I::I8x16MaxS,
            O::I8x16MaxU => I::I8x16MaxU,
            O::I8x16RoundingAverageU => I::I8x16RoundingAverageU,
            O::I16x8ExtAddPairwiseI8x16S => I::I16x8ExtAddPairwiseI8x16S,
            O::I16x8ExtAddPairwiseI8x16U => I::I16x8ExtAddPairwiseI8x16U,
            O::I16x8Abs => I::I16x8Abs,
            O::I16x8Neg => I::I16x8Neg,
            O::I16x8Q15MulrSatS => I::I16x8Q15MulrSatS,
            O::I16x8AllTrue => I::I16x8AllTrue,
            O::I16x8Bitmask => I::I16x8Bitmask,
            O::I16x8NarrowI32x4S => I::I16x8NarrowI32x4S,
            O::I16x8NarrowI32x4U => I::I16x8NarrowI32x4U,
            O::I16x8ExtendLowI8x16S => I::I16x8ExtendLowI8x16S,
            O::I16x8ExtendHighI8x16S => I::I16x8ExtendHighI8x16S,
            O::I16x8ExtendLowI8x16U => I::I16x8ExtendLowI8x16U,
            O::I16x8ExtendHighI8x16U => I::I16x8ExtendHighI8x16U,
            O::I16x8Shl => I::I16x8Shl,
            O::I16x8ShrS => I::I16x8ShrS,
            O::I16x8ShrU => I::I16x8ShrU,
            O::I16x8Add => I::I16x8Add,
            O::I16x8AddSatS => I::I16x8AddSatS,
            O::I16x8AddSatU => I::I16x8AddSatU,
            O::I16x8Sub => I::I16x8Sub,
            O::I16x8SubSatS => I::I16x8SubSatS,
            O::I16x8SubSatU => I::I16x8SubSatU,
            O::I16x8Mul => I::I16x8Mul,
            O::I16x8MinS => I::I16x8MinS,
            O::I16x8MinU => I::I16x8MinU,
            O::I16x8MaxS => I::I16x8MaxS,
            O::I16x8MaxU => I::I16x8MaxU,
            O::I16x8RoundingAverageU => I::I16x8RoundingAverageU,
            O::I16x8ExtMulLowI8x16S => I::I16x8ExtMulLowI8x16S,
            O::I16x8ExtMulHighI8x16S => I::I16x8ExtMulHighI8x16S,
            O::I16x8ExtMulLowI8x16U => I::I16x8ExtMulLowI8x16U,
            O::I16x8ExtMulHighI8x16U => I::I16x8ExtMulHighI8x16U,
            O::I32x4ExtAddPairwiseI16x8S => I::I32x4ExtAddPairwiseI16x8S,
            O::I32x4ExtAddPairwiseI16x8U => I::I32x4ExtAddPairwiseI16x8U,
            O::I32x4Abs => I::I32x4Abs,
            O::I32x4Neg => I::I32x4Neg,
            O::I32x4AllTrue => I::I32x4AllTrue,
            O::I32x4Bitmask => I::I32x4Bitmask,
            O::I32x4ExtendLowI16x8S => I::I32x4ExtendLowI16x8S,
            O::I32x4ExtendHighI16x8S => I::I32x4ExtendHighI16x8S,
            O::I32x4ExtendLowI16x8U => I::I32x4ExtendLowI16x8U,
            O::I32x4ExtendHighI16x8U => I::I32x4ExtendHighI16x8U,
            O::I32x4Shl => I::I32x4Shl,
            O::I32x4ShrS => I::I32x4ShrS,
            O::I32x4ShrU => I::I32x4ShrU,
            O::I32x4Add => I::I32x4Add,
            O::I32x4Sub => I::I32x4Sub,
            O::I32x4Mul => I::I32x4Mul,
            O::I32x4MinS => I::I32x4MinS,
            O::I32x4MinU => I::I32x4MinU,
            O::I32x4MaxS => I::I32x4MaxS,
            O::I32x4MaxU => I::I32x4MaxU,
            O::I32x4DotI16x8S => I::I32x4DotI16x8S,
            O::I32x4ExtMulLowI16x8S => I::I32x4ExtMulLowI16x8S,
            O::I32x4ExtMulHighI16x8S => I::I32x4ExtMulHighI16x8S,
            O::I32x4ExtMulLowI16x8U => I::I32x4ExtMulLowI16x8U,
            O::I32x4ExtMulHighI16x8U => I::I32x4ExtMulHighI16x8U,
            O::I64x2Abs => I::I64x2Abs,
            O::I64x2Neg => I::I64x2Neg,
            O::I64x2AllTrue => I::I64x2AllTrue,
            O::I64x2Bitmask => I::I64x2Bitmask,
            O::I64x2ExtendLowI32x4S => I::I64x2ExtendLowI32x4S,
            O::I64x2ExtendHighI32x4S => I::I64x2ExtendHighI32x4S,
            O::I64x2ExtendLowI32x4U => I::I64x2ExtendLowI32x4U,
            O::I64x2ExtendHighI32x4U => I::I64x2ExtendHighI32x4U,
            O::I64x2Shl => I::I64x2Shl,
            O::I64x2ShrS => I::I64x2ShrS,
            O::I64x2ShrU => I::I64x2ShrU,
            O::I64x2Add => I::I64x2Add,
            O::I64x2Sub => I::I64x2Sub,
            O::I64x2Mul => I::I64x2Mul,
            O::I64x2ExtMulLowI32x4S => I::I64x2ExtMulLowI32x4S,
            O::I64x2ExtMulHighI32x4S => I::I64x2ExtMulHighI32x4S,
            O::I64x2ExtMulLowI32x4U => I::I64x2ExtMulLowI32x4U,
            O::I64x2ExtMulHighI32x4U => I::I64x2ExtMulHighI32x4U,
            O::F32x4Ceil => I::F32x4Ceil,
            O::F32x4Floor => I::F32x4Floor,
            O::F32x4Trunc => I::F32x4Trunc,
            O::F32x4Nearest => I::F32x4Nearest,
            O::F32x4Abs => I::F32x4Abs,
            O::F32x4Neg => I::F32x4Neg,
            O::F32x4Sqrt => I::F32x4Sqrt,
            O::F32x4Add => I::F32x4Add,
            O::F32x4Sub => I::F32x4Sub,
            O::F32x4Mul => I::F32x4Mul,
            O::F32x4Div => I::F32x4Div,
            O::F32x4Min => I::F32x4Min,
            O::F32x4Max => I::F32x4Max,
            O::F32x4PMin => I::F32x4PMin,
            O::F32x4PMax => I::F32x4PMax,
            O::F64x2Ceil => I::F64x2Ceil,
            O::F64x2Floor => I::F64x2Floor,
            O::F64x2Trunc => I::F64x2Trunc,
            O::F64x2Nearest => I::F64x2Nearest,
            O::F64x2Abs => I::F64x2Abs,
            O::F64x2Neg => I::F64x2Neg,
            O::F64x2Sqrt => I::F64x2Sqrt,
            O::F64x2Add => I::F64x2Add,
            O::F64x2Sub => I::F64x2Sub,
            O::F64x2Mul => I::F64x2Mul,
            O::F64x2Div => I::F64x2Div,
            O::F64x2Min => I::F64x2Min,
            O::F64x2Max => I::F64x2Max,
            O::F64x2PMin => I::F64x2PMin,
            O::F64x2PMax => I::F64x2PMax,
            O::I32x4TruncSatF32x4S => I::I32x4TruncSatF32x4S,
            O::I32x4TruncSatF32x4U => I::I32x4TruncSatF32x4U,
            O::F32x4ConvertI32x4S => I::F32x4ConvertI32x4S,
            O::F32x4ConvertI32x4U => I::F32x4ConvertI32x4U,
            O::I32x4TruncSatF64x2SZero => I::I32x4TruncSatF64x2SZero,
            O::I32x4TruncSatF64x2UZero => I::I32x4TruncSatF64x2UZero,
            O::F64x2ConvertLowI32x4S => I::F64x2ConvertLowI32x4S,
            O::F64x2ConvertLowI32x4U => I::F64x2ConvertLowI32x4U,
            O::F32x4DemoteF64x2Zero => I::F32x4DemoteF64x2Zero,
            O::F64x2PromoteLowF32x4 => I::F64x2PromoteLowF32x4,
            O::I8x16SwizzleRelaxed => I::I8x16SwizzleRelaxed,
            O::I32x4TruncSatF32x4SRelaxed => I::I32x4TruncSatF32x4SRelaxed,
            O::I32x4TruncSatF32x4URelaxed => I::I32x4TruncSatF32x4URelaxed,
            O::I32x4TruncSatF64x2SZeroRelaxed => I::I32x4TruncSatF64x2SZeroRelaxed,
            O::I32x4TruncSatF64x2UZeroRelaxed => I::I32x4TruncSatF64x2UZeroRelaxed,
            O::F32x4FmaRelaxed => I::F32x4FmaRelaxed,
            O::F32x4FmsRelaxed => I::F32x4FmsRelaxed,
            O::F64x2FmaRelaxed => I::F64x2FmaRelaxed,
            O::F64x2FmsRelaxed => I::F64x2FmsRelaxed,
            O::I8x16LaneSelect => I::I8x16LaneSelect,
            O::I16x8LaneSelect => I::I16x8LaneSelect,
            O::I32x4LaneSelect => I::I32x4LaneSelect,
            O::I64x2LaneSelect => I::I64x2LaneSelect,
            O::F32x4MinRelaxed => I::F32x4MinRelaxed,
            O::F32x4MaxRelaxed => I::F32x4MaxRelaxed,
            O::F64x2MinRelaxed => I::F64x2MinRelaxed,
            O::F64x2MaxRelaxed => I::F64x2MaxRelaxed,

            // Note that these cases are not supported in `wasm_encoder` yet,
            // and in general `wasmparser` often parses more things than
            // `wasm_encoder` supports. If these are seen we simply say that
            // this mutation isn't applicable because `wasm-encoder` can't
            // create the new function anyway.
            O::MemoryAtomicNotify { .. }
            | O::MemoryAtomicWait32 { .. }
            | O::MemoryAtomicWait64 { .. }
            | O::I32AtomicLoad { .. }
            | O::I64AtomicLoad { .. }
            | O::I32AtomicLoad8U { .. }
            | O::I32AtomicLoad16U { .. }
            | O::I64AtomicLoad8U { .. }
            | O::I64AtomicLoad16U { .. }
            | O::I64AtomicLoad32U { .. }
            | O::I32AtomicStore { .. }
            | O::I64AtomicStore { .. }
            | O::I32AtomicStore8 { .. }
            | O::I32AtomicStore16 { .. }
            | O::I64AtomicStore8 { .. }
            | O::I64AtomicStore16 { .. }
            | O::I64AtomicStore32 { .. }
            | O::I32AtomicRmwAdd { .. }
            | O::I64AtomicRmwAdd { .. }
            | O::I32AtomicRmw8AddU { .. }
            | O::I32AtomicRmw16AddU { .. }
            | O::I64AtomicRmw8AddU { .. }
            | O::I64AtomicRmw16AddU { .. }
            | O::I64AtomicRmw32AddU { .. }
            | O::I32AtomicRmwSub { .. }
            | O::I64AtomicRmwSub { .. }
            | O::I32AtomicRmw8SubU { .. }
            | O::I32AtomicRmw16SubU { .. }
            | O::I64AtomicRmw8SubU { .. }
            | O::I64AtomicRmw16SubU { .. }
            | O::I64AtomicRmw32SubU { .. }
            | O::I32AtomicRmwAnd { .. }
            | O::I64AtomicRmwAnd { .. }
            | O::I32AtomicRmw8AndU { .. }
            | O::I32AtomicRmw16AndU { .. }
            | O::I64AtomicRmw8AndU { .. }
            | O::I64AtomicRmw16AndU { .. }
            | O::I64AtomicRmw32AndU { .. }
            | O::I32AtomicRmwOr { .. }
            | O::I64AtomicRmwOr { .. }
            | O::I32AtomicRmw8OrU { .. }
            | O::I32AtomicRmw16OrU { .. }
            | O::I64AtomicRmw8OrU { .. }
            | O::I64AtomicRmw16OrU { .. }
            | O::I64AtomicRmw32OrU { .. }
            | O::I32AtomicRmwXor { .. }
            | O::I64AtomicRmwXor { .. }
            | O::I32AtomicRmw8XorU { .. }
            | O::I32AtomicRmw16XorU { .. }
            | O::I64AtomicRmw8XorU { .. }
            | O::I64AtomicRmw16XorU { .. }
            | O::I64AtomicRmw32XorU { .. }
            | O::I32AtomicRmwXchg { .. }
            | O::I64AtomicRmwXchg { .. }
            | O::I32AtomicRmw8XchgU { .. }
            | O::I32AtomicRmw16XchgU { .. }
            | O::I64AtomicRmw8XchgU { .. }
            | O::I64AtomicRmw16XchgU { .. }
            | O::I64AtomicRmw32XchgU { .. }
            | O::I32AtomicRmwCmpxchg { .. }
            | O::I64AtomicRmwCmpxchg { .. }
            | O::I32AtomicRmw8CmpxchgU { .. }
            | O::I32AtomicRmw16CmpxchgU { .. }
            | O::I64AtomicRmw8CmpxchgU { .. }
            | O::I64AtomicRmw16CmpxchgU { .. }
            | O::I64AtomicRmw32CmpxchgU { .. }
            | O::ReturnCall { .. }
            | O::ReturnCallIndirect { .. }
            | O::AtomicFence { .. } => return Err(Error::no_mutations_applicable()),
        })
    }

    fn translate_block_type(&mut self, ty: &TypeOrFuncType) -> Result<BlockType> {
        match ty {
            TypeOrFuncType::Type(Type::EmptyBlockType) => Ok(BlockType::Empty),
            TypeOrFuncType::Type(ty) => Ok(BlockType::Result(self.translate_type(ty)?)),
            TypeOrFuncType::FuncType(f) => Ok(BlockType::FunctionType(self.remap(Item::Type, *f)?)),
        }
    }

    fn memarg(&mut self, memarg: &MemoryImmediate) -> Result<MemArg> {
        Ok(MemArg {
            offset: memarg.offset,
            align: memarg.align.into(),
            memory_index: self.remap(Item::Memory, memarg.memory)?,
        })
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
            r#"(module (elem))"#,
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
}
