/* Copyright 2020 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use anyhow::{bail, Result};
use std::collections::HashMap;
use wasmparser::{DataKind, ElementItem, ElementKind};
use wasmparser::{Export, FunctionBody, Parser};
use wasmparser::{ExternalKind, Import, ImportSectionEntryType};
use wasmparser::{GlobalType, MemoryType, TableType, Type, TypeDef, TypeOrFuncType};
use wasmparser::{
    NameSectionReader, ResizableLimits, SectionReader, SectionWithLimitedItems, TypeSectionReader,
};
use wast as ast;

pub fn read(bytes: &[u8]) -> Result<wast::Module> {
    AstReader::new().read_all(bytes)
}

#[derive(Default)]
pub struct AstReader<'a> {
    fields: Vec<ast::ModuleField<'a>>,
    func_types: Vec<ast::TypeUse<'a, ast::FunctionType<'a>>>,
    func_to_field_map: Vec<usize>,
    type_index_to_id: Vec<Option<ast::Id<'static>>>,
    type_id_to_index: HashMap<ast::Id<'static>, u32>,
    func_index_to_id: Vec<Option<ast::Id<'static>>>,
    memory_index_to_id: Vec<Option<ast::Id<'static>>>,
    table_index_to_id: Vec<Option<ast::Id<'static>>>,
    global_index_to_id: Vec<Option<ast::Id<'static>>>,
    module_name: Option<ast::NameAnnotation<'a>>,
    id_gen: ast::IdGenerator,
}

impl<'a> AstReader<'a> {
    pub fn new() -> AstReader<'a> {
        AstReader::default()
    }

    pub fn read_all(mut self, bytes: &'a [u8]) -> Result<ast::Module<'a>> {
        use wasmparser::Payload::*;
        for payload in Parser::new(0).parse_all(bytes) {
            match payload? {
                Version { num, .. } => {
                    if num != 1 {
                        bail!("bad wasm file version");
                    }
                }
                TypeSection(s) => self.type_section(&s)?,
                ImportSection(s) => self.import_section(&s)?,

                FunctionSection(s) => self.function_section(&s)?,
                TableSection(s) => self.table_section(&s)?,
                MemorySection(s) => self.memory_section(&s)?,
                GlobalSection(s) => self.global_section(&s)?,
                ExportSection(s) => self.export_section(&s)?,
                StartSection { func, .. } => self
                    .fields
                    .push(ast::ModuleField::Start(self.func_index(func))),
                ElementSection(s) => self.element_section(&s)?,
                DataCountSection { .. } => (),
                CodeSectionStart { .. } => (),
                CodeSectionEntry(body) => self.func_body(body)?,
                DataSection(s) => self.data_section(&s)?,
                End => (),

                CustomSection {
                    name: "name",
                    data_offset,
                    data,
                } => {
                    let s = NameSectionReader::new(data, data_offset)?;
                    self.name_section(s)?;
                }
                CustomSection { .. } | UnknownSection { .. } => (),

                AliasSection(_)
                | InstanceSection(_)
                | ModuleSection(_)
                | ModuleCodeSectionStart { .. }
                | ModuleCodeSectionEntry { .. } => {
                    bail!("Unsupported section type");
                }
            }
        }
        let module = ast::Module {
            span: no_offset(),
            id: None,
            name: self.module_name,
            kind: ast::ModuleKind::Text(self.fields),
        };
        Ok(module)
    }

    fn type_index<'i>(&self, index: u32) -> ast::Index<'static> {
        self.type_index_to_id[index as usize].map_or_else(
            || ast::Index::Num(index, no_offset()),
            |id| ast::Index::Id(id),
        )
    }

    fn func_index<'i>(&self, index: u32) -> ast::Index<'i> {
        self.func_index_to_id[index as usize].map_or_else(
            || ast::Index::Num(index, no_offset()),
            |id| ast::Index::Id(id),
        )
    }

    fn table_index<'i>(&self, index: u32) -> ast::Index<'i> {
        self.table_index_to_id[index as usize].map_or_else(
            || ast::Index::Num(index, no_offset()),
            |id| ast::Index::Id(id),
        )
    }

    fn memory_index<'i>(&self, index: u32) -> ast::Index<'i> {
        self.memory_index_to_id[index as usize].map_or_else(
            || ast::Index::Num(index, no_offset()),
            |id| ast::Index::Id(id),
        )
    }

    fn global_index<'i>(&self, index: u32) -> ast::Index<'i> {
        self.global_index_to_id[index as usize].map_or_else(
            || ast::Index::Num(index, no_offset()),
            |id| ast::Index::Id(id),
        )
    }

    fn section<T>(
        &mut self,
        section: &T,
        mut convert_item: impl FnMut(&mut Self, T::Item) -> Result<()>,
    ) -> Result<()>
    where
        T: SectionReader + Clone + SectionWithLimitedItems,
    {
        let mut section = section.clone();
        for _ in 0..section.get_count() {
            let item = section.read()?;
            convert_item(self, item)?;
        }
        section.ensure_end()?;
        Ok(())
    }

    fn next_id(&mut self) -> Option<ast::Id<'static>> {
        Some(self.id_gen.next(no_offset()))
    }

    pub fn type_section(&mut self, section: &TypeSectionReader<'_>) -> Result<()> {
        self.section(section, |me, item| {
            let typedef = me.type_def(item)?;
            if let Some(id) = typedef.id {
                me.type_id_to_index
                    .insert(id, me.type_index_to_id.len() as u32);
            }
            me.type_index_to_id.push(typedef.id.clone());
            me.fields.push(ast::ModuleField::Type(typedef));
            Ok(())
        })
    }

    fn type_def<'i>(&mut self, def: TypeDef) -> Result<ast::Type<'i>> {
        let def = match def {
            TypeDef::Func(t) => {
                let params = t
                    .params
                    .into_iter()
                    .map(|p| Ok((None, None, to_val_type(*p)?)))
                    .collect::<Result<Vec<_>>>()?
                    .into_boxed_slice();
                let results = t
                    .returns
                    .into_iter()
                    .map(|p| to_val_type(*p))
                    .collect::<Result<Vec<_>>>()?
                    .into_boxed_slice();
                ast::TypeDef::Func(ast::FunctionType { params, results })
            }
            x => bail!("Unsupported type: {:?}", x),
        };
        let id = self.next_id();
        Ok(ast::Type {
            span: no_offset(),
            id,
            def,
        })
    }

    pub fn import_section(&mut self, section: &wasmparser::ImportSectionReader<'a>) -> Result<()> {
        self.section(section, |me, item| {
            let import = me.import(item)?;
            if let ast::Import {
                item:
                    ast::ItemSig {
                        kind: ast::ItemKind::Func(_),
                        ..
                    },
                ..
            } = import
            {
                me.func_to_field_map.push(me.fields.len());
            }
            me.fields.push(ast::ModuleField::Import(import));
            Ok(())
        })
    }

    fn import<'i>(&mut self, entry: Import<'i>) -> Result<ast::Import<'i>> {
        let id = self.next_id();
        let kind = match entry.ty {
            ImportSectionEntryType::Function(type_index) => {
                self.func_index_to_id.push(id);
                ast::ItemKind::Func(ast::TypeUse::new_with_index(self.type_index(type_index)))
            }
            ImportSectionEntryType::Table(ty) => {
                self.table_index_to_id.push(id);
                ast::ItemKind::Table(to_table_type(ty)?)
            }
            ImportSectionEntryType::Memory(ty) => {
                self.memory_index_to_id.push(id);
                ast::ItemKind::Memory(to_memory_type(ty)?)
            }
            ImportSectionEntryType::Global(ty) => {
                self.global_index_to_id.push(id);
                ast::ItemKind::Global(to_global_type(ty)?)
            }
            x => {
                bail!("Unsupported import type: {:?}", x);
            }
        };

        Ok(ast::Import {
            span: no_offset(),
            module: entry.module,
            field: entry.field,
            item: ast::ItemSig {
                span: no_offset(),
                id,
                name: None,
                kind,
            },
        })
    }

    pub fn function_section<'i>(
        &mut self,
        section: &wasmparser::FunctionSectionReader<'i>,
    ) -> Result<()> {
        self.section(section, |me, i| {
            let id = me.next_id();
            me.func_index_to_id.push(id);
            me.func_types
                .push(ast::TypeUse::new_with_index(me.type_index(i)));
            Ok(())
        })
    }

    pub fn table_section(&mut self, section: &wasmparser::TableSectionReader<'a>) -> Result<()> {
        self.section(section, |me, ty| {
            let id = me.next_id();
            me.table_index_to_id.push(id);
            me.fields.push(ast::ModuleField::Table(ast::Table {
                span: no_offset(),
                id,
                exports: ast::InlineExport { names: vec![] },
                kind: ast::TableKind::Normal(to_table_type(ty)?),
            }));
            Ok(())
        })
    }

    pub fn memory_section<'i>(
        &mut self,
        section: &wasmparser::MemorySectionReader<'i>,
    ) -> Result<()> {
        self.section(section, |me, ty| {
            let id = me.next_id();
            me.memory_index_to_id.push(id);
            me.fields.push(ast::ModuleField::Memory(ast::Memory {
                span: no_offset(),
                id,
                exports: ast::InlineExport { names: vec![] },
                kind: ast::MemoryKind::Normal(to_memory_type(ty)?),
            }));
            Ok(())
        })
    }

    pub fn global_section(&mut self, section: &wasmparser::GlobalSectionReader<'a>) -> Result<()> {
        self.section(section, |me, g| {
            let id = me.next_id();
            me.global_index_to_id.push(id);
            me.fields.push(ast::ModuleField::Global(ast::Global {
                span: no_offset(),
                id,
                exports: ast::InlineExport { names: vec![] },
                ty: to_global_type(g.ty)?,
                kind: {
                    let init_expr = me.read_expr(g.init_expr.get_operators_reader())?;
                    ast::GlobalKind::Inline(init_expr)
                },
            }));
            Ok(())
        })
    }

    pub fn export_section(&mut self, section: &wasmparser::ExportSectionReader<'a>) -> Result<()> {
        self.section(section, |me, e| {
            me.fields.push(ast::ModuleField::Export(me.export(e)?));
            Ok(())
        })
    }

    fn export<'i>(&self, export: Export<'i>) -> Result<ast::Export<'i>> {
        let index = export.index;
        let kind = match export.kind {
            ExternalKind::Function => ast::ExportKind::Func(self.func_index(index)),
            ExternalKind::Table => ast::ExportKind::Table(self.table_index(index)),
            ExternalKind::Memory => ast::ExportKind::Memory(self.memory_index(index)),
            ExternalKind::Global => ast::ExportKind::Global(self.global_index(index)),
            x => {
                bail!("Unsupported export kind: {:?}", x);
            }
        };
        Ok(ast::Export {
            span: no_offset(),
            name: export.field,
            kind,
        })
    }

    pub fn element_section(
        &mut self,
        section: &wasmparser::ElementSectionReader<'a>,
    ) -> Result<()> {
        self.section(section, |me, e| {
            let kind = match e.kind {
                ElementKind::Active {
                    table_index,
                    init_expr,
                } => ast::ElemKind::Active {
                    table: me.table_index(table_index),
                    offset: me.read_expr(init_expr.get_operators_reader())?,
                },
                ElementKind::Passive => ast::ElemKind::Passive,
                ElementKind::Declared => ast::ElemKind::Declared,
            };
            let mut items = e.items.get_items_reader()?;
            let mut exprs = Vec::new();
            for _ in 0..items.get_count() {
                match items.read()? {
                    ElementItem::Null(_ty) => {
                        exprs.push(None);
                    }
                    ElementItem::Func(f) => {
                        exprs.push(Some(me.func_index(f)));
                    }
                }
            }

            me.fields.push(ast::ModuleField::Elem(ast::Elem {
                span: no_offset(),
                id: None,
                kind,
                payload: ast::ElemPayload::Exprs {
                    ty: to_ref_type(e.ty)?,
                    exprs,
                },
            }));
            Ok(())
        })
    }

    fn func_body(&mut self, body: FunctionBody<'a>) -> Result<()> {
        let ty = self.func_types.remove(0);
        let id = self.func_index_to_id[self.func_to_field_map.len()];
        let f = ast::Func {
            span: no_offset(),
            id,
            name: None,
            exports: ast::InlineExport { names: vec![] },
            kind: self.code_section_entry(body)?,
            ty,
        };
        self.func_to_field_map.push(self.fields.len());
        self.fields.push(ast::ModuleField::Func(f));
        Ok(())
    }

    pub fn code_section_entry<'i>(&mut self, body: FunctionBody<'i>) -> Result<ast::FuncKind<'i>> {
        let mut locals = Vec::new();
        for l in body.get_locals_reader()? {
            let (count, ty) = l?;
            let ty = to_val_type(ty)?;
            for _ in 0..count {
                locals.push(ast::Local {
                    id: None,
                    name: None,
                    ty,
                })
            }
        }
        let expression = self.read_expr(body.get_operators_reader()?)?;
        Ok(ast::FuncKind::Inline { locals, expression })
    }

    pub fn data_section(&mut self, section: &wasmparser::DataSectionReader<'a>) -> Result<()> {
        self.section(section, |me, d| {
            let kind = match d.kind {
                DataKind::Passive => ast::DataKind::Passive,
                DataKind::Active {
                    memory_index,
                    init_expr,
                } => ast::DataKind::Active {
                    memory: me.memory_index(memory_index),
                    offset: me.read_expr(init_expr.get_operators_reader())?,
                },
            };
            me.fields.push(ast::ModuleField::Data(ast::Data {
                span: no_offset(),
                id: None,
                kind,
                data: vec![d.data],
            }));
            Ok(())
        })
    }

    #[allow(non_snake_case)]
    fn read_expr<'i>(
        &self,
        op_reader: wasmparser::OperatorsReader<'i>,
    ) -> Result<ast::Expression<'i>> {
        use wasmparser::Operator::*;
        let mut instrs = Vec::new();
        for op in op_reader {
            let i = match op? {
                Block { ty } => ast::Instruction::Block(self.block_type(ty)?),
                If { ty } => ast::Instruction::If(self.block_type(ty)?),
                Else => ast::Instruction::Else(None),
                Loop { ty } => ast::Instruction::Loop(self.block_type(ty)?),
                End => ast::Instruction::End(None),

                Unreachable => ast::Instruction::Unreachable,
                Nop => ast::Instruction::Nop,
                Br { relative_depth } => {
                    ast::Instruction::Br(ast::Index::Num(relative_depth, no_offset()))
                }
                BrIf { relative_depth } => {
                    ast::Instruction::BrIf(ast::Index::Num(relative_depth, no_offset()))
                }
                BrTable { table } => ast::Instruction::BrTable(read_br_table(table)?),
                Return => ast::Instruction::Return,
                Call { function_index } => ast::Instruction::Call(self.func_index(function_index)),
                CallIndirect { index, table_index } => {
                    ast::Instruction::CallIndirect(ast::CallIndirect {
                        table: self.table_index(table_index),
                        ty: ast::TypeUse::new_with_index(self.type_index(index)),
                    })
                }

                // tail-call proposal
                ReturnCall { function_index } => {
                    ast::Instruction::ReturnCall(self.func_index(function_index))
                }
                ReturnCallIndirect { index, table_index } => {
                    ast::Instruction::ReturnCallIndirect(ast::CallIndirect {
                        table: self.table_index(table_index),
                        ty: ast::TypeUse::new_with_index(self.type_index(index)),
                    })
                }

                // function-references proposal
                // CallRef => ast::Instruction::CallRef,
                // ReturnCallRef => ast::Instruction::ReturnCallRef,
                //FuncBind => ast::Instruction::FuncBind(FuncBindType<'a>),
                //Let => ast::Instruction::Let(LetType<'a>),
                Drop => ast::Instruction::Drop,
                Select => ast::Instruction::Select(ast::SelectTypes { tys: None }),
                LocalGet { local_index } => {
                    ast::Instruction::LocalGet(ast::Index::Num(local_index, no_offset()))
                }
                LocalSet { local_index } => {
                    ast::Instruction::LocalSet(ast::Index::Num(local_index, no_offset()))
                }
                LocalTee { local_index } => {
                    ast::Instruction::LocalTee(ast::Index::Num(local_index, no_offset()))
                }
                GlobalGet { global_index } => {
                    ast::Instruction::GlobalGet(self.global_index(global_index))
                }
                GlobalSet { global_index } => {
                    ast::Instruction::GlobalSet(self.global_index(global_index))
                }

                // TableGet { table } => ast::Instruction::TableGet(TableArg<'a>),
                // TableSet { table } => ast::Instruction::TableSet(TableArg<'a>),
                I32Load { memarg } => ast::Instruction::I32Load(self.memarg(memarg)),
                I64Load { memarg } => ast::Instruction::I64Load(self.memarg(memarg)),
                F32Load { memarg } => ast::Instruction::F32Load(self.memarg(memarg)),
                F64Load { memarg } => ast::Instruction::F64Load(self.memarg(memarg)),
                I32Load8S { memarg } => ast::Instruction::I32Load8s(self.memarg(memarg)),
                I32Load8U { memarg } => ast::Instruction::I32Load8u(self.memarg(memarg)),
                I32Load16S { memarg } => ast::Instruction::I32Load16s(self.memarg(memarg)),
                I32Load16U { memarg } => ast::Instruction::I32Load16u(self.memarg(memarg)),
                I64Load8S { memarg } => ast::Instruction::I64Load8s(self.memarg(memarg)),
                I64Load8U { memarg } => ast::Instruction::I64Load8u(self.memarg(memarg)),
                I64Load16S { memarg } => ast::Instruction::I64Load16s(self.memarg(memarg)),
                I64Load16U { memarg } => ast::Instruction::I64Load16u(self.memarg(memarg)),
                I64Load32S { memarg } => ast::Instruction::I64Load32s(self.memarg(memarg)),
                I64Load32U { memarg } => ast::Instruction::I64Load32u(self.memarg(memarg)),
                I32Store { memarg } => ast::Instruction::I32Store(self.memarg(memarg)),
                I64Store { memarg } => ast::Instruction::I64Store(self.memarg(memarg)),
                F32Store { memarg } => ast::Instruction::F32Store(self.memarg(memarg)),
                F64Store { memarg } => ast::Instruction::F64Store(self.memarg(memarg)),
                I32Store8 { memarg } => ast::Instruction::I32Store8(self.memarg(memarg)),
                I32Store16 { memarg } => ast::Instruction::I32Store16(self.memarg(memarg)),
                I64Store8 { memarg } => ast::Instruction::I64Store8(self.memarg(memarg)),
                I64Store16 { memarg } => ast::Instruction::I64Store16(self.memarg(memarg)),
                I64Store32 { memarg } => ast::Instruction::I64Store32(self.memarg(memarg)),

                // Lots of bulk memory proposal here as well
                MemorySize { mem, .. } => ast::Instruction::MemorySize(ast::MemoryArg {
                    mem: self.memory_index(mem),
                }),
                MemoryGrow { mem, .. } => ast::Instruction::MemoryGrow(ast::MemoryArg {
                    mem: self.memory_index(mem),
                }),
                // MemoryInit => ast::Instruction::MemoryInit(MemoryInit<'a>),
                // MemoryCopy => ast::Instruction::MemoryCopy(MemoryCopy<'a>),
                // MemoryFill => ast::Instruction::MemoryFill(MemoryArg<'a>),
                // DataDrop => ast::Instruction::DataDrop(ast::Index<'a>),
                // ElemDrop => ast::Instruction::ElemDrop(ast::Index<'a>),
                // TableInit => ast::Instruction::TableInit(TableInit<'a>),
                // TableCopy => ast::Instruction::TableCopy(TableCopy<'a>),
                // TableFill => ast::Instruction::TableFill(TableArg<'a>),
                // TableSize => ast::Instruction::TableSize(TableArg<'a>),
                // TableGrow => ast::Instruction::TableGrow(TableArg<'a>),
                RefNull { ty } => ast::Instruction::RefNull(to_heap_type(ty)?),
                RefIsNull => ast::Instruction::RefIsNull,
                //RefExtern => ast::Instruction::RefExtern(u32),
                RefFunc { function_index } => {
                    ast::Instruction::RefFunc(self.func_index(function_index))
                }

                // // function-references proposal
                // RefAsNonNull => ast::Instruction::RefAsNonNull,
                // BrOnNull => ast::Instruction::BrOnNull(ast::Index<'a>),

                // // gc proposal: eqref
                // RefEq => ast::Instruction::RefEq,

                // // gc proposal (moz specific, will be removed)
                // StructNew => ast::Instruction::StructNew(ast::Index<'a>),

                // // gc proposal: struct
                // StructNewWithRtt => ast::Instruction::StructNewWithRtt(ast::Index<'a>),
                // StructNewDefaultWithRtt => ast::Instruction::StructNewDefaultWithRtt(ast::Index<'a>),
                // StructGet => ast::Instruction::StructGet(StructAccess<'a>),
                // StructGetS => ast::Instruction::StructGetS(StructAccess<'a>),
                // StructGetU => ast::Instruction::StructGetU(StructAccess<'a>),
                // StructSet => ast::Instruction::StructSet(StructAccess<'a>),

                // // gc proposal (moz specific, will be removed)
                // StructNarrow => ast::Instruction::StructNarrow(StructNarrow<'a>),

                // // gc proposal: array
                // ArrayNewWithRtt => ast::Instruction::ArrayNewWithRtt(ast::Index<'a>),
                // ArrayNewDefaultWithRtt => ast::Instruction::ArrayNewDefaultWithRtt(ast::Index<'a>),
                // ArrayGet => ast::Instruction::ArrayGet(ast::Index<'a>),
                // ArrayGetS => ast::Instruction::ArrayGetS(ast::Index<'a>),
                // ArrayGetU => ast::Instruction::ArrayGetU(ast::Index<'a>),
                // ArraySet => ast::Instruction::ArraySet(ast::Index<'a>),
                // ArrayLen => ast::Instruction::ArrayLen(ast::Index<'a>),

                // // gc proposal, i31
                // I31New => ast::Instruction::I31New,
                // I31GetS => ast::Instruction::I31GetS,
                // I31GetU => ast::Instruction::I31GetU,

                // // gc proposal, rtt/casting
                // RTTCanon => ast::Instruction::RTTCanon(HeapType<'a>),
                // RTTSub => ast::Instruction::RTTSub(RTTSub<'a>),
                // RefTest => ast::Instruction::RefTest(RefTest<'a>),
                // RefCast => ast::Instruction::RefCast(RefTest<'a>),
                // BrOnCast => ast::Instruction::BrOnCast(BrOnCast<'a>),
                I32Const { value } => ast::Instruction::I32Const(value),
                I64Const { value } => ast::Instruction::I64Const(value),
                F32Const { value } => {
                    ast::Instruction::F32Const(ast::Float32 { bits: value.bits() })
                }
                F64Const { value } => {
                    ast::Instruction::F64Const(ast::Float64 { bits: value.bits() })
                }

                I32Clz => ast::Instruction::I32Clz,
                I32Ctz => ast::Instruction::I32Ctz,
                I32Popcnt => ast::Instruction::I32Popcnt,
                I32Add => ast::Instruction::I32Add,
                I32Sub => ast::Instruction::I32Sub,
                I32Mul => ast::Instruction::I32Mul,
                I32DivS => ast::Instruction::I32DivS,
                I32DivU => ast::Instruction::I32DivU,
                I32RemS => ast::Instruction::I32RemS,
                I32RemU => ast::Instruction::I32RemU,
                I32And => ast::Instruction::I32And,
                I32Or => ast::Instruction::I32Or,
                I32Xor => ast::Instruction::I32Xor,
                I32Shl => ast::Instruction::I32Shl,
                I32ShrS => ast::Instruction::I32ShrS,
                I32ShrU => ast::Instruction::I32ShrU,
                I32Rotl => ast::Instruction::I32Rotl,
                I32Rotr => ast::Instruction::I32Rotr,

                I64Clz => ast::Instruction::I64Clz,
                I64Ctz => ast::Instruction::I64Ctz,
                I64Popcnt => ast::Instruction::I64Popcnt,
                I64Add => ast::Instruction::I64Add,
                I64Sub => ast::Instruction::I64Sub,
                I64Mul => ast::Instruction::I64Mul,
                I64DivS => ast::Instruction::I64DivS,
                I64DivU => ast::Instruction::I64DivU,
                I64RemS => ast::Instruction::I64RemS,
                I64RemU => ast::Instruction::I64RemU,
                I64And => ast::Instruction::I64And,
                I64Or => ast::Instruction::I64Or,
                I64Xor => ast::Instruction::I64Xor,
                I64Shl => ast::Instruction::I64Shl,
                I64ShrS => ast::Instruction::I64ShrS,
                I64ShrU => ast::Instruction::I64ShrU,
                I64Rotl => ast::Instruction::I64Rotl,
                I64Rotr => ast::Instruction::I64Rotr,

                F32Abs => ast::Instruction::F32Abs,
                F32Neg => ast::Instruction::F32Neg,
                F32Ceil => ast::Instruction::F32Ceil,
                F32Floor => ast::Instruction::F32Floor,
                F32Trunc => ast::Instruction::F32Trunc,
                F32Nearest => ast::Instruction::F32Nearest,
                F32Sqrt => ast::Instruction::F32Sqrt,
                F32Add => ast::Instruction::F32Add,
                F32Sub => ast::Instruction::F32Sub,
                F32Mul => ast::Instruction::F32Mul,
                F32Div => ast::Instruction::F32Div,
                F32Min => ast::Instruction::F32Min,
                F32Max => ast::Instruction::F32Max,
                F32Copysign => ast::Instruction::F32Copysign,

                F64Abs => ast::Instruction::F64Abs,
                F64Neg => ast::Instruction::F64Neg,
                F64Ceil => ast::Instruction::F64Ceil,
                F64Floor => ast::Instruction::F64Floor,
                F64Trunc => ast::Instruction::F64Trunc,
                F64Nearest => ast::Instruction::F64Nearest,
                F64Sqrt => ast::Instruction::F64Sqrt,
                F64Add => ast::Instruction::F64Add,
                F64Sub => ast::Instruction::F64Sub,
                F64Mul => ast::Instruction::F64Mul,
                F64Div => ast::Instruction::F64Div,
                F64Min => ast::Instruction::F64Min,
                F64Max => ast::Instruction::F64Max,
                F64Copysign => ast::Instruction::F64Copysign,

                I32Eqz => ast::Instruction::I32Eqz,
                I32Eq => ast::Instruction::I32Eq,
                I32Ne => ast::Instruction::I32Ne,
                I32LtS => ast::Instruction::I32LtS,
                I32LtU => ast::Instruction::I32LtU,
                I32GtS => ast::Instruction::I32GtS,
                I32GtU => ast::Instruction::I32GtU,
                I32LeS => ast::Instruction::I32LeS,
                I32LeU => ast::Instruction::I32LeU,
                I32GeS => ast::Instruction::I32GeS,
                I32GeU => ast::Instruction::I32GeU,

                I64Eqz => ast::Instruction::I64Eqz,
                I64Eq => ast::Instruction::I64Eq,
                I64Ne => ast::Instruction::I64Ne,
                I64LtS => ast::Instruction::I64LtS,
                I64LtU => ast::Instruction::I64LtU,
                I64GtS => ast::Instruction::I64GtS,
                I64GtU => ast::Instruction::I64GtU,
                I64LeS => ast::Instruction::I64LeS,
                I64LeU => ast::Instruction::I64LeU,
                I64GeS => ast::Instruction::I64GeS,
                I64GeU => ast::Instruction::I64GeU,

                F32Eq => ast::Instruction::F32Eq,
                F32Ne => ast::Instruction::F32Ne,
                F32Lt => ast::Instruction::F32Lt,
                F32Gt => ast::Instruction::F32Gt,
                F32Le => ast::Instruction::F32Le,
                F32Ge => ast::Instruction::F32Ge,

                F64Eq => ast::Instruction::F64Eq,
                F64Ne => ast::Instruction::F64Ne,
                F64Lt => ast::Instruction::F64Lt,
                F64Gt => ast::Instruction::F64Gt,
                F64Le => ast::Instruction::F64Le,
                F64Ge => ast::Instruction::F64Ge,

                I32WrapI64 => ast::Instruction::I32WrapI64,
                I32TruncF32S => ast::Instruction::I32TruncF32S,
                I32TruncF32U => ast::Instruction::I32TruncF32U,
                I32TruncF64S => ast::Instruction::I32TruncF64S,
                I32TruncF64U => ast::Instruction::I32TruncF64U,
                I64ExtendI32S => ast::Instruction::I64ExtendI32S,
                I64ExtendI32U => ast::Instruction::I64ExtendI32U,
                I64TruncF32S => ast::Instruction::I64TruncF32S,
                I64TruncF32U => ast::Instruction::I64TruncF32U,
                I64TruncF64S => ast::Instruction::I64TruncF64S,
                I64TruncF64U => ast::Instruction::I64TruncF64U,
                F32ConvertI32S => ast::Instruction::F32ConvertI32S,
                F32ConvertI32U => ast::Instruction::F32ConvertI32U,
                F32ConvertI64S => ast::Instruction::F32ConvertI64S,
                F32ConvertI64U => ast::Instruction::F32ConvertI64U,
                F32DemoteF64 => ast::Instruction::F32DemoteF64,
                F64ConvertI32S => ast::Instruction::F64ConvertI32S,
                F64ConvertI32U => ast::Instruction::F64ConvertI32U,
                F64ConvertI64S => ast::Instruction::F64ConvertI64S,
                F64ConvertI64U => ast::Instruction::F64ConvertI64U,
                F64PromoteF32 => ast::Instruction::F64PromoteF32,
                I32ReinterpretF32 => ast::Instruction::I32ReinterpretF32,
                I64ReinterpretF64 => ast::Instruction::I64ReinterpretF64,
                F32ReinterpretI32 => ast::Instruction::F32ReinterpretI32,
                F64ReinterpretI64 => ast::Instruction::F64ReinterpretI64,

                // non-trapping float to int
                I32TruncSatF32S => ast::Instruction::I32TruncSatF32S,
                I32TruncSatF32U => ast::Instruction::I32TruncSatF32U,
                I32TruncSatF64S => ast::Instruction::I32TruncSatF64S,
                I32TruncSatF64U => ast::Instruction::I32TruncSatF64U,
                I64TruncSatF32S => ast::Instruction::I64TruncSatF32S,
                I64TruncSatF32U => ast::Instruction::I64TruncSatF32U,
                I64TruncSatF64S => ast::Instruction::I64TruncSatF64S,
                I64TruncSatF64U => ast::Instruction::I64TruncSatF64U,

                // sign extension proposal
                I32Extend8S => ast::Instruction::I32Extend8S,
                I32Extend16S => ast::Instruction::I32Extend16S,
                I64Extend8S => ast::Instruction::I64Extend8S,
                I64Extend16S => ast::Instruction::I64Extend16S,
                I64Extend32S => ast::Instruction::I64Extend32S,

                // atomics proposal
                MemoryAtomicNotify { memarg } => {
                    ast::Instruction::MemoryAtomicNotify(self.memarg(memarg))
                }
                MemoryAtomicWait32 { memarg } => {
                    ast::Instruction::MemoryAtomicWait32(self.memarg(memarg))
                }
                MemoryAtomicWait64 { memarg } => {
                    ast::Instruction::MemoryAtomicWait64(self.memarg(memarg))
                }
                AtomicFence { flags: _ } => ast::Instruction::AtomicFence,

                I32AtomicLoad { memarg } => ast::Instruction::I32AtomicLoad(self.memarg(memarg)),
                I64AtomicLoad { memarg } => ast::Instruction::I64AtomicLoad(self.memarg(memarg)),
                I32AtomicLoad8U { memarg } => {
                    ast::Instruction::I32AtomicLoad8u(self.memarg(memarg))
                }
                I32AtomicLoad16U { memarg } => {
                    ast::Instruction::I32AtomicLoad16u(self.memarg(memarg))
                }
                I64AtomicLoad8U { memarg } => {
                    ast::Instruction::I64AtomicLoad8u(self.memarg(memarg))
                }
                I64AtomicLoad16U { memarg } => {
                    ast::Instruction::I64AtomicLoad16u(self.memarg(memarg))
                }
                I64AtomicLoad32U { memarg } => {
                    ast::Instruction::I64AtomicLoad32u(self.memarg(memarg))
                }
                I32AtomicStore { memarg } => ast::Instruction::I32AtomicStore(self.memarg(memarg)),
                I64AtomicStore { memarg } => ast::Instruction::I64AtomicStore(self.memarg(memarg)),
                I32AtomicStore8 { memarg } => {
                    ast::Instruction::I32AtomicStore8(self.memarg(memarg))
                }
                I32AtomicStore16 { memarg } => {
                    ast::Instruction::I32AtomicStore16(self.memarg(memarg))
                }
                I64AtomicStore8 { memarg } => {
                    ast::Instruction::I64AtomicStore8(self.memarg(memarg))
                }
                I64AtomicStore16 { memarg } => {
                    ast::Instruction::I64AtomicStore16(self.memarg(memarg))
                }
                I64AtomicStore32 { memarg } => {
                    ast::Instruction::I64AtomicStore32(self.memarg(memarg))
                }

                I32AtomicRmwAdd { memarg } => {
                    ast::Instruction::I32AtomicRmwAdd(self.memarg(memarg))
                }
                I64AtomicRmwAdd { memarg } => {
                    ast::Instruction::I64AtomicRmwAdd(self.memarg(memarg))
                }
                I32AtomicRmw8AddU { memarg } => {
                    ast::Instruction::I32AtomicRmw8AddU(self.memarg(memarg))
                }
                I32AtomicRmw16AddU { memarg } => {
                    ast::Instruction::I32AtomicRmw16AddU(self.memarg(memarg))
                }
                I64AtomicRmw8AddU { memarg } => {
                    ast::Instruction::I64AtomicRmw8AddU(self.memarg(memarg))
                }
                I64AtomicRmw16AddU { memarg } => {
                    ast::Instruction::I64AtomicRmw16AddU(self.memarg(memarg))
                }
                I64AtomicRmw32AddU { memarg } => {
                    ast::Instruction::I64AtomicRmw32AddU(self.memarg(memarg))
                }

                I32AtomicRmwSub { memarg } => {
                    ast::Instruction::I32AtomicRmwSub(self.memarg(memarg))
                }
                I64AtomicRmwSub { memarg } => {
                    ast::Instruction::I64AtomicRmwSub(self.memarg(memarg))
                }
                I32AtomicRmw8SubU { memarg } => {
                    ast::Instruction::I32AtomicRmw8SubU(self.memarg(memarg))
                }
                I32AtomicRmw16SubU { memarg } => {
                    ast::Instruction::I32AtomicRmw16SubU(self.memarg(memarg))
                }
                I64AtomicRmw8SubU { memarg } => {
                    ast::Instruction::I64AtomicRmw8SubU(self.memarg(memarg))
                }
                I64AtomicRmw16SubU { memarg } => {
                    ast::Instruction::I64AtomicRmw16SubU(self.memarg(memarg))
                }
                I64AtomicRmw32SubU { memarg } => {
                    ast::Instruction::I64AtomicRmw32SubU(self.memarg(memarg))
                }

                I32AtomicRmwAnd { memarg } => {
                    ast::Instruction::I32AtomicRmwAnd(self.memarg(memarg))
                }
                I64AtomicRmwAnd { memarg } => {
                    ast::Instruction::I64AtomicRmwAnd(self.memarg(memarg))
                }
                I32AtomicRmw8AndU { memarg } => {
                    ast::Instruction::I32AtomicRmw8AndU(self.memarg(memarg))
                }
                I32AtomicRmw16AndU { memarg } => {
                    ast::Instruction::I32AtomicRmw16AndU(self.memarg(memarg))
                }
                I64AtomicRmw8AndU { memarg } => {
                    ast::Instruction::I64AtomicRmw8AndU(self.memarg(memarg))
                }
                I64AtomicRmw16AndU { memarg } => {
                    ast::Instruction::I64AtomicRmw16AndU(self.memarg(memarg))
                }
                I64AtomicRmw32AndU { memarg } => {
                    ast::Instruction::I64AtomicRmw32AndU(self.memarg(memarg))
                }

                I32AtomicRmwOr { memarg } => ast::Instruction::I32AtomicRmwOr(self.memarg(memarg)),
                I64AtomicRmwOr { memarg } => ast::Instruction::I64AtomicRmwOr(self.memarg(memarg)),
                I32AtomicRmw8OrU { memarg } => {
                    ast::Instruction::I32AtomicRmw8OrU(self.memarg(memarg))
                }
                I32AtomicRmw16OrU { memarg } => {
                    ast::Instruction::I32AtomicRmw16OrU(self.memarg(memarg))
                }
                I64AtomicRmw8OrU { memarg } => {
                    ast::Instruction::I64AtomicRmw8OrU(self.memarg(memarg))
                }
                I64AtomicRmw16OrU { memarg } => {
                    ast::Instruction::I64AtomicRmw16OrU(self.memarg(memarg))
                }
                I64AtomicRmw32OrU { memarg } => {
                    ast::Instruction::I64AtomicRmw32OrU(self.memarg(memarg))
                }

                I32AtomicRmwXor { memarg } => {
                    ast::Instruction::I32AtomicRmwXor(self.memarg(memarg))
                }
                I64AtomicRmwXor { memarg } => {
                    ast::Instruction::I64AtomicRmwXor(self.memarg(memarg))
                }
                I32AtomicRmw8XorU { memarg } => {
                    ast::Instruction::I32AtomicRmw8XorU(self.memarg(memarg))
                }
                I32AtomicRmw16XorU { memarg } => {
                    ast::Instruction::I32AtomicRmw16XorU(self.memarg(memarg))
                }
                I64AtomicRmw8XorU { memarg } => {
                    ast::Instruction::I64AtomicRmw8XorU(self.memarg(memarg))
                }
                I64AtomicRmw16XorU { memarg } => {
                    ast::Instruction::I64AtomicRmw16XorU(self.memarg(memarg))
                }
                I64AtomicRmw32XorU { memarg } => {
                    ast::Instruction::I64AtomicRmw32XorU(self.memarg(memarg))
                }

                I32AtomicRmwXchg { memarg } => {
                    ast::Instruction::I32AtomicRmwXchg(self.memarg(memarg))
                }
                I64AtomicRmwXchg { memarg } => {
                    ast::Instruction::I64AtomicRmwXchg(self.memarg(memarg))
                }
                I32AtomicRmw8XchgU { memarg } => {
                    ast::Instruction::I32AtomicRmw8XchgU(self.memarg(memarg))
                }
                I32AtomicRmw16XchgU { memarg } => {
                    ast::Instruction::I32AtomicRmw16XchgU(self.memarg(memarg))
                }
                I64AtomicRmw8XchgU { memarg } => {
                    ast::Instruction::I64AtomicRmw8XchgU(self.memarg(memarg))
                }
                I64AtomicRmw16XchgU { memarg } => {
                    ast::Instruction::I64AtomicRmw16XchgU(self.memarg(memarg))
                }
                I64AtomicRmw32XchgU { memarg } => {
                    ast::Instruction::I64AtomicRmw32XchgU(self.memarg(memarg))
                }

                I32AtomicRmwCmpxchg { memarg } => {
                    ast::Instruction::I32AtomicRmwCmpxchg(self.memarg(memarg))
                }
                I64AtomicRmwCmpxchg { memarg } => {
                    ast::Instruction::I64AtomicRmwCmpxchg(self.memarg(memarg))
                }
                I32AtomicRmw8CmpxchgU { memarg } => {
                    ast::Instruction::I32AtomicRmw8CmpxchgU(self.memarg(memarg))
                }
                I32AtomicRmw16CmpxchgU { memarg } => {
                    ast::Instruction::I32AtomicRmw16CmpxchgU(self.memarg(memarg))
                }
                I64AtomicRmw8CmpxchgU { memarg } => {
                    ast::Instruction::I64AtomicRmw8CmpxchgU(self.memarg(memarg))
                }
                I64AtomicRmw16CmpxchgU { memarg } => {
                    ast::Instruction::I64AtomicRmw16CmpxchgU(self.memarg(memarg))
                }
                I64AtomicRmw32CmpxchgU { memarg } => {
                    ast::Instruction::I64AtomicRmw32CmpxchgU(self.memarg(memarg))
                }

                // proposal: simd
                V128Load { memarg } => ast::Instruction::V128Load(self.memarg(memarg)),
                I16x8Load8x8S { memarg } => ast::Instruction::I16x8Load8x8S(self.memarg(memarg)),
                I16x8Load8x8U { memarg } => ast::Instruction::I16x8Load8x8U(self.memarg(memarg)),
                I32x4Load16x4S { memarg } => ast::Instruction::I32x4Load16x4S(self.memarg(memarg)),
                I32x4Load16x4U { memarg } => ast::Instruction::I32x4Load16x4U(self.memarg(memarg)),
                I64x2Load32x2S { memarg } => ast::Instruction::I64x2Load32x2S(self.memarg(memarg)),
                I64x2Load32x2U { memarg } => ast::Instruction::I64x2Load32x2U(self.memarg(memarg)),
                V8x16LoadSplat { memarg } => ast::Instruction::V8x16LoadSplat(self.memarg(memarg)),
                V16x8LoadSplat { memarg } => ast::Instruction::V16x8LoadSplat(self.memarg(memarg)),
                V32x4LoadSplat { memarg } => ast::Instruction::V32x4LoadSplat(self.memarg(memarg)),
                V64x2LoadSplat { memarg } => ast::Instruction::V64x2LoadSplat(self.memarg(memarg)),
                V128Store { memarg } => ast::Instruction::V128Store(self.memarg(memarg)),

                V128Const { value } => ast::Instruction::V128Const(ast::V128Const::I8x16({
                    let mut v128 = [0i8; 16];
                    for (i, v) in value.bytes().iter().enumerate() {
                        v128[i] = *v as i8;
                    }
                    v128
                })),
                V8x16Shuffle { lanes } => {
                    ast::Instruction::V8x16Shuffle(ast::V8x16Shuffle { lanes })
                }
                V8x16Swizzle => ast::Instruction::V8x16Swizzle,

                I8x16Splat => ast::Instruction::I8x16Splat,
                I16x8Splat => ast::Instruction::I16x8Splat,
                I32x4Splat => ast::Instruction::I32x4Splat,
                I64x2Splat => ast::Instruction::I64x2Splat,
                F32x4Splat => ast::Instruction::F32x4Splat,
                F64x2Splat => ast::Instruction::F64x2Splat,

                I8x16ExtractLaneS { lane } => ast::Instruction::I8x16ExtractLaneS(lane),
                I8x16ExtractLaneU { lane } => ast::Instruction::I8x16ExtractLaneU(lane),
                I8x16ReplaceLane { lane } => ast::Instruction::I8x16ReplaceLane(lane),
                I16x8ExtractLaneS { lane } => ast::Instruction::I16x8ExtractLaneS(lane),
                I16x8ExtractLaneU { lane } => ast::Instruction::I16x8ExtractLaneU(lane),
                I16x8ReplaceLane { lane } => ast::Instruction::I16x8ReplaceLane(lane),
                I32x4ExtractLane { lane } => ast::Instruction::I32x4ExtractLane(lane),
                I32x4ReplaceLane { lane } => ast::Instruction::I32x4ReplaceLane(lane),
                I64x2ExtractLane { lane } => ast::Instruction::I64x2ExtractLane(lane),
                I64x2ReplaceLane { lane } => ast::Instruction::I64x2ReplaceLane(lane),
                F32x4ExtractLane { lane } => ast::Instruction::F32x4ExtractLane(lane),
                F32x4ReplaceLane { lane } => ast::Instruction::F32x4ReplaceLane(lane),
                F64x2ExtractLane { lane } => ast::Instruction::F64x2ExtractLane(lane),
                F64x2ReplaceLane { lane } => ast::Instruction::F64x2ReplaceLane(lane),

                I8x16Eq => ast::Instruction::I8x16Eq,
                I8x16Ne => ast::Instruction::I8x16Ne,
                I8x16LtS => ast::Instruction::I8x16LtS,
                I8x16LtU => ast::Instruction::I8x16LtU,
                I8x16GtS => ast::Instruction::I8x16GtS,
                I8x16GtU => ast::Instruction::I8x16GtU,
                I8x16LeS => ast::Instruction::I8x16LeS,
                I8x16LeU => ast::Instruction::I8x16LeU,
                I8x16GeS => ast::Instruction::I8x16GeS,
                I8x16GeU => ast::Instruction::I8x16GeU,
                I16x8Eq => ast::Instruction::I16x8Eq,
                I16x8Ne => ast::Instruction::I16x8Ne,
                I16x8LtS => ast::Instruction::I16x8LtS,
                I16x8LtU => ast::Instruction::I16x8LtU,
                I16x8GtS => ast::Instruction::I16x8GtS,
                I16x8GtU => ast::Instruction::I16x8GtU,
                I16x8LeS => ast::Instruction::I16x8LeS,
                I16x8LeU => ast::Instruction::I16x8LeU,
                I16x8GeS => ast::Instruction::I16x8GeS,
                I16x8GeU => ast::Instruction::I16x8GeU,
                I32x4Eq => ast::Instruction::I32x4Eq,
                I32x4Ne => ast::Instruction::I32x4Ne,
                I32x4LtS => ast::Instruction::I32x4LtS,
                I32x4LtU => ast::Instruction::I32x4LtU,
                I32x4GtS => ast::Instruction::I32x4GtS,
                I32x4GtU => ast::Instruction::I32x4GtU,
                I32x4LeS => ast::Instruction::I32x4LeS,
                I32x4LeU => ast::Instruction::I32x4LeU,
                I32x4GeS => ast::Instruction::I32x4GeS,
                I32x4GeU => ast::Instruction::I32x4GeU,

                F32x4Eq => ast::Instruction::F32x4Eq,
                F32x4Ne => ast::Instruction::F32x4Ne,
                F32x4Lt => ast::Instruction::F32x4Lt,
                F32x4Gt => ast::Instruction::F32x4Gt,
                F32x4Le => ast::Instruction::F32x4Le,
                F32x4Ge => ast::Instruction::F32x4Ge,
                F64x2Eq => ast::Instruction::F64x2Eq,
                F64x2Ne => ast::Instruction::F64x2Ne,
                F64x2Lt => ast::Instruction::F64x2Lt,
                F64x2Gt => ast::Instruction::F64x2Gt,
                F64x2Le => ast::Instruction::F64x2Le,
                F64x2Ge => ast::Instruction::F64x2Ge,

                V128Not => ast::Instruction::V128Not,
                V128And => ast::Instruction::V128And,
                V128AndNot => ast::Instruction::V128Andnot,
                V128Or => ast::Instruction::V128Or,
                V128Xor => ast::Instruction::V128Xor,
                V128Bitselect => ast::Instruction::V128Bitselect,

                I8x16Abs => ast::Instruction::I8x16Abs,
                I8x16Neg => ast::Instruction::I8x16Neg,
                I8x16AnyTrue => ast::Instruction::I8x16AnyTrue,
                I8x16AllTrue => ast::Instruction::I8x16AllTrue,
                I8x16Bitmask => ast::Instruction::I8x16Bitmask,
                I8x16NarrowI16x8S => ast::Instruction::I8x16NarrowI16x8S,
                I8x16NarrowI16x8U => ast::Instruction::I8x16NarrowI16x8U,
                I8x16Shl => ast::Instruction::I8x16Shl,
                I8x16ShrS => ast::Instruction::I8x16ShrS,
                I8x16ShrU => ast::Instruction::I8x16ShrU,
                I8x16Add => ast::Instruction::I8x16Add,
                I8x16AddSaturateS => ast::Instruction::I8x16AddSaturateS,
                I8x16AddSaturateU => ast::Instruction::I8x16AddSaturateU,
                I8x16Sub => ast::Instruction::I8x16Sub,
                I8x16SubSaturateS => ast::Instruction::I8x16SubSaturateS,
                I8x16SubSaturateU => ast::Instruction::I8x16SubSaturateU,
                I8x16MinS => ast::Instruction::I8x16MinS,
                I8x16MinU => ast::Instruction::I8x16MinU,
                I8x16MaxS => ast::Instruction::I8x16MaxS,
                I8x16MaxU => ast::Instruction::I8x16MaxU,
                // I8x16AvgrU => ast::Instruction::I8x16AvgrU,
                I16x8Abs => ast::Instruction::I16x8Abs,
                I16x8Neg => ast::Instruction::I16x8Neg,
                I16x8AnyTrue => ast::Instruction::I16x8AnyTrue,
                I16x8AllTrue => ast::Instruction::I16x8AllTrue,
                I16x8Bitmask => ast::Instruction::I16x8Bitmask,
                I16x8NarrowI32x4S => ast::Instruction::I16x8NarrowI32x4S,
                I16x8NarrowI32x4U => ast::Instruction::I16x8NarrowI32x4U,
                I16x8WidenLowI8x16S => ast::Instruction::I16x8WidenLowI8x16S,
                I16x8WidenHighI8x16S => ast::Instruction::I16x8WidenHighI8x16S,
                I16x8WidenLowI8x16U => ast::Instruction::I16x8WidenLowI8x16U,
                I16x8WidenHighI8x16U => ast::Instruction::I16x8WidenHighI8x16u,
                I16x8Shl => ast::Instruction::I16x8Shl,
                I16x8ShrS => ast::Instruction::I16x8ShrS,
                I16x8ShrU => ast::Instruction::I16x8ShrU,
                I16x8Add => ast::Instruction::I16x8Add,
                I16x8AddSaturateS => ast::Instruction::I16x8AddSaturateS,
                I16x8AddSaturateU => ast::Instruction::I16x8AddSaturateU,
                I16x8Sub => ast::Instruction::I16x8Sub,
                I16x8SubSaturateS => ast::Instruction::I16x8SubSaturateS,
                I16x8SubSaturateU => ast::Instruction::I16x8SubSaturateU,
                I16x8Mul => ast::Instruction::I16x8Mul,
                I16x8MinS => ast::Instruction::I16x8MinS,
                I16x8MinU => ast::Instruction::I16x8MinU,
                I16x8MaxS => ast::Instruction::I16x8MaxS,
                I16x8MaxU => ast::Instruction::I16x8MaxU,
                // I16x8AvgrU => ast::Instruction::I16x8AvgrU,
                I32x4Abs => ast::Instruction::I32x4Abs,
                I32x4Neg => ast::Instruction::I32x4Neg,
                I32x4AnyTrue => ast::Instruction::I32x4AnyTrue,
                I32x4AllTrue => ast::Instruction::I32x4AllTrue,
                I32x4Bitmask => ast::Instruction::I32x4Bitmask,
                I32x4WidenLowI16x8S => ast::Instruction::I32x4WidenLowI16x8S,
                I32x4WidenHighI16x8S => ast::Instruction::I32x4WidenHighI16x8S,
                I32x4WidenLowI16x8U => ast::Instruction::I32x4WidenLowI16x8U,
                I32x4WidenHighI16x8U => ast::Instruction::I32x4WidenHighI16x8u,
                I32x4Shl => ast::Instruction::I32x4Shl,
                I32x4ShrS => ast::Instruction::I32x4ShrS,
                I32x4ShrU => ast::Instruction::I32x4ShrU,
                I32x4Add => ast::Instruction::I32x4Add,
                I32x4Sub => ast::Instruction::I32x4Sub,
                // I32x4DotI16x8S => ast::Instruction::I32x4DotI16x8S,
                I32x4Mul => ast::Instruction::I32x4Mul,
                I32x4MinS => ast::Instruction::I32x4MinS,
                I32x4MinU => ast::Instruction::I32x4MinU,
                I32x4MaxS => ast::Instruction::I32x4MaxS,
                I32x4MaxU => ast::Instruction::I32x4MaxU,

                I64x2Neg => ast::Instruction::I64x2Neg,
                I64x2Shl => ast::Instruction::I64x2Shl,
                I64x2ShrS => ast::Instruction::I64x2ShrS,
                I64x2ShrU => ast::Instruction::I64x2ShrU,
                I64x2Add => ast::Instruction::I64x2Add,
                I64x2Sub => ast::Instruction::I64x2Sub,
                I64x2Mul => ast::Instruction::I64x2Mul,

                F32x4Abs => ast::Instruction::F32x4Abs,
                F32x4Neg => ast::Instruction::F32x4Neg,
                F32x4Sqrt => ast::Instruction::F32x4Sqrt,
                F32x4Add => ast::Instruction::F32x4Add,
                F32x4Sub => ast::Instruction::F32x4Sub,
                F32x4Mul => ast::Instruction::F32x4Mul,
                F32x4Div => ast::Instruction::F32x4Div,
                F32x4Min => ast::Instruction::F32x4Min,
                F32x4Max => ast::Instruction::F32x4Max,

                F64x2Abs => ast::Instruction::F64x2Abs,
                F64x2Neg => ast::Instruction::F64x2Neg,
                F64x2Sqrt => ast::Instruction::F64x2Sqrt,
                F64x2Add => ast::Instruction::F64x2Add,
                F64x2Sub => ast::Instruction::F64x2Sub,
                F64x2Mul => ast::Instruction::F64x2Mul,
                F64x2Div => ast::Instruction::F64x2Div,
                F64x2Min => ast::Instruction::F64x2Min,
                F64x2Max => ast::Instruction::F64x2Max,

                I32x4TruncSatF32x4S => ast::Instruction::I32x4TruncSatF32x4S,
                I32x4TruncSatF32x4U => ast::Instruction::I32x4TruncSatF32x4U,
                F32x4ConvertI32x4S => ast::Instruction::F32x4ConvertI32x4S,
                F32x4ConvertI32x4U => ast::Instruction::F32x4ConvertI32x4U,

                x => {
                    bail!("Unsupported op {:?}", x);
                }
            };
            instrs.push(i);
        }
        let _ = instrs.pop();
        Ok(ast::Expression {
            instrs: instrs.into_boxed_slice(),
        })
    }

    pub fn name_section(&mut self, mut section: wasmparser::NameSectionReader<'a>) -> Result<()> {
        use wasmparser::Name::*;
        while !section.eof() {
            match section.read()? {
                Function(name) => {
                    let mut reader = name.get_map()?;
                    for _ in 0..reader.get_count() {
                        let naming = reader.read()?;
                        let field_index = self.func_to_field_map[naming.index as usize];
                        match self.fields[field_index] {
                            ast::ModuleField::Func(ast::Func { ref mut name, .. })
                            | ast::ModuleField::Import(ast::Import {
                                item: ast::ItemSig { ref mut name, .. },
                                ..
                            }) => {
                                *name = Some(ast::NameAnnotation { name: naming.name });
                            }
                            _ => bail!("expected function or imported function"),
                        }
                    }
                }
                Module(name) => {
                    self.module_name = Some(ast::NameAnnotation {
                        name: name.get_name()?,
                    });
                }
                Local(name) => {
                    let mut reader = name.get_function_local_reader()?;
                    for _ in 0..reader.get_count() {
                        let func_names = reader.read()?;

                        let mut reader = func_names.get_map()?;
                        let mut names = HashMap::new();
                        for _ in 0..reader.get_count() {
                            let naming = reader.read()?;
                            names.insert(naming.index, ast::NameAnnotation { name: naming.name });
                        }

                        let field_index = self.func_to_field_map[func_names.func_index as usize];
                        let ty_index = match self.fields[field_index] {
                            ast::ModuleField::Func(ast::Func { ref mut ty, .. })
                            | ast::ModuleField::Import(ast::Import {
                                item:
                                    ast::ItemSig {
                                        kind: ast::ItemKind::Func(ref mut ty),
                                        ..
                                    },
                                ..
                            }) => ty.index.unwrap(),
                            _ => {
                                bail!("expected function or imported function");
                            }
                        };
                        let mut typedef = match self.fields.get_mut(match ty_index {
                            ast::Index::Num(i, _) => i,
                            ast::Index::Id(id) => *self.type_id_to_index.get(&id).unwrap(),
                        }
                            as usize)
                        {
                            Some(ast::ModuleField::Type(ast::Type {
                                def: ast::TypeDef::Func(ref typedef),
                                ..
                            })) => typedef.clone(),
                            _ => {
                                bail!("expected function type");
                            }
                        };

                        let params_len = typedef.params.len();
                        for i in 0..params_len {
                            if let Some(n) = names.get(&(i as u32)) {
                                typedef.params[i].1 = Some(n.clone());
                            }
                        }

                        match self.fields[field_index] {
                            ast::ModuleField::Func(ast::Func { ref mut ty, .. })
                            | ast::ModuleField::Import(ast::Import {
                                item:
                                    ast::ItemSig {
                                        kind: ast::ItemKind::Func(ref mut ty),
                                        ..
                                    },
                                ..
                            }) => {
                                ty.inline = Some(typedef);
                            }
                            _ => (),
                        }
                        match self.fields[field_index] {
                            ast::ModuleField::Func(ast::Func {
                                kind: ast::FuncKind::Inline { ref mut locals, .. },
                                ..
                            }) => {
                                for i in 0..locals.len() {
                                    if let Some(n) = names.get(&((i + params_len) as u32)) {
                                        locals[i].name = Some(n.clone());
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn block_type<'i>(&self, ty: TypeOrFuncType) -> Result<ast::BlockType<'i>> {
        let ty = match ty {
            TypeOrFuncType::Type(Type::EmptyBlockType) => ast::TypeUse {
                index: None,
                inline: None,
            },
            TypeOrFuncType::Type(ty) => ast::TypeUse {
                index: None,
                inline: Some(ast::FunctionType {
                    params: Box::new([]),
                    results: Box::new([to_val_type(ty)?]),
                }),
            },
            TypeOrFuncType::FuncType(i) => ast::TypeUse::new_with_index(self.type_index(i)),
        };
        Ok(ast::BlockType { label: None, ty })
    }

    fn memarg<'i>(&self, arg: wasmparser::MemoryImmediate) -> ast::MemArg<'i> {
        ast::MemArg {
            align: 1 << arg.align,
            offset: arg.offset,
            memory: self.memory_index(arg.memory),
        }
    }
}

fn read_br_table<'a>(table: wasmparser::BrTable<'a>) -> Result<ast::BrTableIndices<'a>> {
    let mut labels = Vec::new();
    let mut default = None;
    for e in table.targets() {
        let (i, d) = e.map_err(|_| anyhow::format_err!("bad index"))?;
        let index = ast::Index::Num(i, no_offset());
        if d {
            default = Some(index);
        } else {
            labels.push(index);
        }
    }
    Ok(ast::BrTableIndices {
        labels,
        default: default.unwrap(),
    })
}

#[inline]
fn no_offset() -> ast::Span {
    ast::Span::from_offset(0)
}

fn to_val_type<'a>(ty: Type) -> Result<ast::ValType<'a>> {
    Ok(match ty {
        Type::I32 => ast::ValType::I32,
        Type::I64 => ast::ValType::I64,
        Type::F32 => ast::ValType::F32,
        Type::F64 => ast::ValType::F64,
        x => {
            bail!("Unsupported valtype: {:?}", x);
        }
    })
}

fn to_ref_type<'a>(ty: Type) -> Result<ast::RefType<'a>> {
    Ok(match ty {
        Type::FuncRef => ast::RefType {
            nullable: true,
            heap: ast::HeapType::Func,
        },
        Type::ExternRef => ast::RefType {
            nullable: true,
            heap: ast::HeapType::Extern,
        },
        x => {
            bail!("Unsupported reftype: {:?}", x);
        }
    })
}

fn to_heap_type<'a>(ty: Type) -> Result<ast::HeapType<'a>> {
    Ok(match ty {
        Type::FuncRef => ast::HeapType::Func,
        Type::ExternRef => ast::HeapType::Extern,
        x => {
            bail!("Unsupported heaptype: {:?}", x);
        }
    })
}

fn to_limits(l: ResizableLimits) -> ast::Limits {
    ast::Limits {
        min: l.initial,
        max: l.maximum,
    }
}

fn to_table_type<'a>(ty: TableType) -> Result<ast::TableType<'a>> {
    Ok(ast::TableType {
        limits: to_limits(ty.limits),
        elem: to_ref_type(ty.element_type)?,
    })
}

fn to_memory_type(ty: MemoryType) -> Result<ast::MemoryType> {
    Ok(match ty {
        MemoryType::M32 { limits, shared } => ast::MemoryType::B32 {
            limits: to_limits(limits),
            shared,
        },
        x => {
            bail!("Unsupported memory type: {:?}", x);
        }
    })
}

fn to_global_type<'a>(ty: GlobalType) -> Result<ast::GlobalType<'a>> {
    Ok(ast::GlobalType {
        ty: to_val_type(ty.content_type)?,
        mutable: ty.mutable,
    })
}
