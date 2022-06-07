use crate::{Error, Result};
use wasm_encoder::*;
use wasmparser::{
    DataKind, ElementItem, ElementKind, FunctionBody, Global, InitExpr, MemoryImmediate, Operator,
    Type,
};

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

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub enum InitExprKind {
    Global,
    ElementOffset,
    ElementFunction,
    DataOffset,
}

pub trait Translator {
    fn as_obj(&mut self) -> &mut dyn Translator;

    fn translate_type_def(&mut self, ty: Type, s: &mut TypeSection) -> Result<()> {
        type_def(self.as_obj(), ty, s)
    }

    fn translate_table_type(
        &mut self,
        ty: &wasmparser::TableType,
    ) -> Result<wasm_encoder::TableType> {
        table_type(self.as_obj(), ty)
    }

    fn translate_memory_type(
        &mut self,
        ty: &wasmparser::MemoryType,
    ) -> Result<wasm_encoder::MemoryType> {
        memory_type(self.as_obj(), ty)
    }

    fn translate_global_type(
        &mut self,
        ty: &wasmparser::GlobalType,
    ) -> Result<wasm_encoder::GlobalType> {
        global_type(self.as_obj(), ty)
    }

    fn translate_tag_type(&mut self, ty: &wasmparser::TagType) -> Result<wasm_encoder::TagType> {
        tag_type(self.as_obj(), ty)
    }

    fn translate_ty(&mut self, t: &wasmparser::ValType) -> Result<ValType> {
        ty(self.as_obj(), t)
    }

    fn translate_global(&mut self, g: Global, s: &mut GlobalSection) -> Result<()> {
        global(self.as_obj(), g, s)
    }

    fn translate_init_expr(
        &mut self,
        e: &InitExpr<'_>,
        _ty: &wasmparser::ValType,
        _ctx: InitExprKind,
    ) -> Result<Instruction<'static>> {
        init_expr(self.as_obj(), e)
    }

    fn translate_element(
        &mut self,
        e: wasmparser::Element<'_>,
        s: &mut ElementSection,
    ) -> Result<()> {
        element(self.as_obj(), e, s)
    }

    fn translate_data(&mut self, d: wasmparser::Data<'_>, s: &mut DataSection) -> Result<()> {
        data(self.as_obj(), d, s)
    }

    fn translate_code(&mut self, body: FunctionBody<'_>, s: &mut CodeSection) -> Result<()> {
        code(self.as_obj(), body, s)
    }

    fn translate_op(&mut self, e: &Operator<'_>) -> Result<Instruction<'static>> {
        op(self.as_obj(), e)
    }

    fn translate_block_type(&mut self, ty: &wasmparser::BlockType) -> Result<BlockType> {
        block_type(self.as_obj(), ty)
    }

    fn translate_memarg(&mut self, arg: &MemoryImmediate) -> Result<MemArg> {
        memarg(self.as_obj(), arg)
    }

    fn remap(&mut self, item: Item, idx: u32) -> Result<u32> {
        drop(item);
        Ok(idx)
    }
}

pub struct DefaultTranslator;

impl Translator for DefaultTranslator {
    fn as_obj(&mut self) -> &mut dyn Translator {
        self
    }
}

pub fn type_def(t: &mut dyn Translator, ty: Type, s: &mut TypeSection) -> Result<()> {
    match ty {
        Type::Func(f) => {
            s.function(
                f.params
                    .iter()
                    .map(|ty| t.translate_ty(ty))
                    .collect::<Result<Vec<_>>>()?,
                f.returns
                    .iter()
                    .map(|ty| t.translate_ty(ty))
                    .collect::<Result<Vec<_>>>()?,
            );
            Ok(())
        }
    }
}

pub fn table_type(
    t: &mut dyn Translator,
    ty: &wasmparser::TableType,
) -> Result<wasm_encoder::TableType> {
    Ok(wasm_encoder::TableType {
        element_type: t.translate_ty(&ty.element_type)?,
        minimum: ty.initial,
        maximum: ty.maximum,
    })
}

pub fn memory_type(
    _t: &mut dyn Translator,
    ty: &wasmparser::MemoryType,
) -> Result<wasm_encoder::MemoryType> {
    Ok(wasm_encoder::MemoryType {
        memory64: ty.memory64,
        minimum: ty.initial,
        maximum: ty.maximum,
        shared: ty.shared,
    })
}

pub fn global_type(
    t: &mut dyn Translator,
    ty: &wasmparser::GlobalType,
) -> Result<wasm_encoder::GlobalType> {
    Ok(wasm_encoder::GlobalType {
        val_type: t.translate_ty(&ty.content_type)?,
        mutable: ty.mutable,
    })
}

pub fn tag_type(t: &mut dyn Translator, ty: &wasmparser::TagType) -> Result<wasm_encoder::TagType> {
    Ok(wasm_encoder::TagType {
        kind: TagKind::Exception,
        func_type_idx: t.remap(Item::Type, ty.func_type_idx)?,
    })
}

pub fn ty(_t: &mut dyn Translator, ty: &wasmparser::ValType) -> Result<ValType> {
    match ty {
        wasmparser::ValType::I32 => Ok(ValType::I32),
        wasmparser::ValType::I64 => Ok(ValType::I64),
        wasmparser::ValType::F32 => Ok(ValType::F32),
        wasmparser::ValType::F64 => Ok(ValType::F64),
        wasmparser::ValType::V128 => Ok(ValType::V128),
        wasmparser::ValType::FuncRef => Ok(ValType::FuncRef),
        wasmparser::ValType::ExternRef => Ok(ValType::ExternRef),
    }
}

pub fn global(t: &mut dyn Translator, global: Global, s: &mut GlobalSection) -> Result<()> {
    let ty = t.translate_global_type(&global.ty)?;
    let insn = t.translate_init_expr(
        &global.init_expr,
        &global.ty.content_type,
        InitExprKind::Global,
    )?;
    s.global(ty, &insn);
    Ok(())
}

pub fn init_expr(t: &mut dyn Translator, e: &InitExpr<'_>) -> Result<Instruction<'static>> {
    let mut e = e.get_operators_reader();
    let op = e.read()?;
    let op = t.translate_op(&op)?;
    match e.read()? {
        Operator::End if e.eof() => {}
        _ => return Err(Error::no_mutations_applicable()),
    }
    Ok(op)
}

pub fn element(
    t: &mut dyn Translator,
    element: wasmparser::Element<'_>,
    s: &mut ElementSection,
) -> Result<()> {
    let offset;
    let mode = match &element.kind {
        ElementKind::Active {
            table_index,
            init_expr,
        } => {
            offset = t.translate_init_expr(
                init_expr,
                &wasmparser::ValType::I32,
                InitExprKind::ElementOffset,
            )?;
            ElementMode::Active {
                table: Some(t.remap(Item::Table, *table_index)?),
                offset: &offset,
            }
        }
        ElementKind::Passive => ElementMode::Passive,
        ElementKind::Declared => ElementMode::Declared,
    };
    let element_type = t.translate_ty(&element.ty)?;
    let mut functions = Vec::new();
    let mut exprs = Vec::new();
    let mut reader = element.items.get_items_reader()?;
    for _ in 0..reader.get_count() {
        match reader.read()? {
            ElementItem::Func(idx) => {
                functions.push(t.remap(Item::Function, idx)?);
            }
            ElementItem::Expr(expr) => {
                match t.translate_init_expr(&expr, &element.ty, InitExprKind::ElementFunction)? {
                    Instruction::RefFunc(n) => {
                        exprs.push(wasm_encoder::Element::Func(n));
                    }
                    Instruction::RefNull(_) => {
                        exprs.push(wasm_encoder::Element::Null);
                    }
                    _ => return Err(Error::no_mutations_applicable()),
                }
            }
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

/// This is a pretty gnarly function that translates from `wasmparser`
/// operators to `wasm_encoder` operators. It's quite large because there's
/// quite a few wasm instructions. The theory though is that at least each
/// individual case is pretty self-contained.
pub fn op(t: &mut dyn Translator, op: &Operator<'_>) -> Result<Instruction<'static>> {
    use wasm_encoder::Instruction as I;
    use wasmparser::Operator as O;
    Ok(match op {
        O::Unreachable => I::Unreachable,
        O::Nop => I::Nop,

        O::Block { ty } => I::Block(t.translate_block_type(ty)?),
        O::Loop { ty } => I::Loop(t.translate_block_type(ty)?),
        O::If { ty } => I::If(t.translate_block_type(ty)?),
        O::Else => I::Else,

        O::Try { ty } => I::Try(t.translate_block_type(ty)?),
        O::Catch { index } => I::Catch(t.remap(Item::Tag, *index)?),
        O::Throw { index } => I::Throw(t.remap(Item::Tag, *index)?),
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
        O::Call { function_index } => I::Call(t.remap(Item::Function, *function_index)?),
        O::CallIndirect {
            index,
            table_index,
            table_byte: _,
        } => I::CallIndirect {
            ty: t.remap(Item::Type, *index)?,
            table: t.remap(Item::Table, *table_index)?,
        },
        O::Delegate { relative_depth } => I::Delegate(*relative_depth),
        O::CatchAll => I::CatchAll,
        O::Drop => I::Drop,
        O::Select => I::Select,
        O::TypedSelect { ty } => I::TypedSelect(t.translate_ty(ty)?),

        O::LocalGet { local_index } => I::LocalGet(*local_index),
        O::LocalSet { local_index } => I::LocalSet(*local_index),
        O::LocalTee { local_index } => I::LocalTee(*local_index),

        O::GlobalGet { global_index } => I::GlobalGet(t.remap(Item::Global, *global_index)?),
        O::GlobalSet { global_index } => I::GlobalSet(t.remap(Item::Global, *global_index)?),

        O::I32Load { memarg } => I::I32Load(t.translate_memarg(memarg)?),
        O::I64Load { memarg } => I::I64Load(t.translate_memarg(memarg)?),
        O::F32Load { memarg } => I::F32Load(t.translate_memarg(memarg)?),
        O::F64Load { memarg } => I::F64Load(t.translate_memarg(memarg)?),
        O::I32Load8S { memarg } => I::I32Load8_S(t.translate_memarg(memarg)?),
        O::I32Load8U { memarg } => I::I32Load8_U(t.translate_memarg(memarg)?),
        O::I32Load16S { memarg } => I::I32Load16_S(t.translate_memarg(memarg)?),
        O::I32Load16U { memarg } => I::I32Load16_U(t.translate_memarg(memarg)?),
        O::I64Load8S { memarg } => I::I64Load8_S(t.translate_memarg(memarg)?),
        O::I64Load8U { memarg } => I::I64Load8_U(t.translate_memarg(memarg)?),
        O::I64Load16S { memarg } => I::I64Load16_S(t.translate_memarg(memarg)?),
        O::I64Load16U { memarg } => I::I64Load16_U(t.translate_memarg(memarg)?),
        O::I64Load32S { memarg } => I::I64Load32_S(t.translate_memarg(memarg)?),
        O::I64Load32U { memarg } => I::I64Load32_U(t.translate_memarg(memarg)?),
        O::I32Store { memarg } => I::I32Store(t.translate_memarg(memarg)?),
        O::I64Store { memarg } => I::I64Store(t.translate_memarg(memarg)?),
        O::F32Store { memarg } => I::F32Store(t.translate_memarg(memarg)?),
        O::F64Store { memarg } => I::F64Store(t.translate_memarg(memarg)?),
        O::I32Store8 { memarg } => I::I32Store8(t.translate_memarg(memarg)?),
        O::I32Store16 { memarg } => I::I32Store16(t.translate_memarg(memarg)?),
        O::I64Store8 { memarg } => I::I64Store8(t.translate_memarg(memarg)?),
        O::I64Store16 { memarg } => I::I64Store16(t.translate_memarg(memarg)?),
        O::I64Store32 { memarg } => I::I64Store32(t.translate_memarg(memarg)?),

        O::MemorySize { mem, .. } => I::MemorySize(t.remap(Item::Memory, *mem)?),
        O::MemoryGrow { mem, .. } => I::MemoryGrow(t.remap(Item::Memory, *mem)?),

        O::I32Const { value } => I::I32Const(*value),
        O::I64Const { value } => I::I64Const(*value),
        O::F32Const { value } => I::F32Const(f32::from_bits(value.bits())),
        O::F64Const { value } => I::F64Const(f64::from_bits(value.bits())),

        O::RefNull { ty } => I::RefNull(t.translate_ty(ty)?),
        O::RefIsNull => I::RefIsNull,
        O::RefFunc { function_index } => I::RefFunc(t.remap(Item::Function, *function_index)?),

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
            data: t.remap(Item::Data, *segment)?,
            mem: t.remap(Item::Memory, *mem)?,
        },
        O::DataDrop { segment } => I::DataDrop(t.remap(Item::Data, *segment)?),
        O::MemoryCopy { src, dst } => I::MemoryCopy {
            src: t.remap(Item::Memory, *src)?,
            dst: t.remap(Item::Memory, *dst)?,
        },
        O::MemoryFill { mem, .. } => I::MemoryFill(t.remap(Item::Memory, *mem)?),

        O::TableInit { segment, table } => I::TableInit {
            segment: t.remap(Item::Element, *segment)?,
            table: t.remap(Item::Table, *table)?,
        },
        O::ElemDrop { segment } => I::ElemDrop {
            segment: t.remap(Item::Element, *segment)?,
        },
        O::TableCopy {
            dst_table,
            src_table,
        } => I::TableCopy {
            dst: t.remap(Item::Table, *dst_table)?,
            src: t.remap(Item::Table, *src_table)?,
        },
        O::TableFill { table } => I::TableFill {
            table: t.remap(Item::Table, *table)?,
        },
        O::TableGet { table } => I::TableGet {
            table: t.remap(Item::Table, *table)?,
        },
        O::TableSet { table } => I::TableSet {
            table: t.remap(Item::Table, *table)?,
        },
        O::TableGrow { table } => I::TableGrow {
            table: t.remap(Item::Table, *table)?,
        },
        O::TableSize { table } => I::TableSize {
            table: t.remap(Item::Table, *table)?,
        },

        O::V128Load { memarg } => I::V128Load {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load8x8S { memarg } => I::V128Load8x8S {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load8x8U { memarg } => I::V128Load8x8U {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load16x4S { memarg } => I::V128Load16x4S {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load16x4U { memarg } => I::V128Load16x4U {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load32x2S { memarg } => I::V128Load32x2S {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load32x2U { memarg } => I::V128Load32x2U {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load8Splat { memarg } => I::V128Load8Splat {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load16Splat { memarg } => I::V128Load16Splat {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load32Splat { memarg } => I::V128Load32Splat {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load64Splat { memarg } => I::V128Load64Splat {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load32Zero { memarg } => I::V128Load32Zero {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load64Zero { memarg } => I::V128Load64Zero {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Store { memarg } => I::V128Store {
            memarg: t.translate_memarg(memarg)?,
        },
        O::V128Load8Lane { memarg, lane } => I::V128Load8Lane {
            memarg: t.translate_memarg(memarg)?,
            lane: *lane,
        },
        O::V128Load16Lane { memarg, lane } => I::V128Load16Lane {
            memarg: t.translate_memarg(memarg)?,
            lane: *lane,
        },
        O::V128Load32Lane { memarg, lane } => I::V128Load32Lane {
            memarg: t.translate_memarg(memarg)?,
            lane: *lane,
        },
        O::V128Load64Lane { memarg, lane } => I::V128Load64Lane {
            memarg: t.translate_memarg(memarg)?,
            lane: *lane,
        },
        O::V128Store8Lane { memarg, lane } => I::V128Store8Lane {
            memarg: t.translate_memarg(memarg)?,
            lane: *lane,
        },
        O::V128Store16Lane { memarg, lane } => I::V128Store16Lane {
            memarg: t.translate_memarg(memarg)?,
            lane: *lane,
        },
        O::V128Store32Lane { memarg, lane } => I::V128Store32Lane {
            memarg: t.translate_memarg(memarg)?,
            lane: *lane,
        },
        O::V128Store64Lane { memarg, lane } => I::V128Store64Lane {
            memarg: t.translate_memarg(memarg)?,
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
        O::I8x16RelaxedSwizzle => I::I8x16RelaxedSwizzle,
        O::I32x4RelaxedTruncSatF32x4S => I::I32x4RelaxedTruncSatF32x4S,
        O::I32x4RelaxedTruncSatF32x4U => I::I32x4RelaxedTruncSatF32x4U,
        O::I32x4RelaxedTruncSatF64x2SZero => I::I32x4RelaxedTruncSatF64x2SZero,
        O::I32x4RelaxedTruncSatF64x2UZero => I::I32x4RelaxedTruncSatF64x2UZero,
        O::F32x4Fma => I::F32x4Fma,
        O::F32x4Fms => I::F32x4Fms,
        O::F64x2Fma => I::F64x2Fma,
        O::F64x2Fms => I::F64x2Fms,
        O::I8x16LaneSelect => I::I8x16LaneSelect,
        O::I16x8LaneSelect => I::I16x8LaneSelect,
        O::I32x4LaneSelect => I::I32x4LaneSelect,
        O::I64x2LaneSelect => I::I64x2LaneSelect,
        O::F32x4RelaxedMin => I::F32x4RelaxedMin,
        O::F32x4RelaxedMax => I::F32x4RelaxedMax,
        O::F64x2RelaxedMin => I::F64x2RelaxedMin,
        O::F64x2RelaxedMax => I::F64x2RelaxedMax,

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

pub fn block_type(t: &mut dyn Translator, ty: &wasmparser::BlockType) -> Result<BlockType> {
    match ty {
        wasmparser::BlockType::Empty => Ok(BlockType::Empty),
        wasmparser::BlockType::Type(ty) => Ok(BlockType::Result(t.translate_ty(ty)?)),
        wasmparser::BlockType::FuncType(f) => Ok(BlockType::FunctionType(t.remap(Item::Type, *f)?)),
    }
}

pub fn memarg(t: &mut dyn Translator, memarg: &MemoryImmediate) -> Result<MemArg> {
    Ok(MemArg {
        offset: memarg.offset,
        align: memarg.align.into(),
        memory_index: t.remap(Item::Memory, memarg.memory)?,
    })
}

pub fn data(t: &mut dyn Translator, data: wasmparser::Data<'_>, s: &mut DataSection) -> Result<()> {
    let offset;
    let mode = match &data.kind {
        DataKind::Active {
            memory_index,
            init_expr,
        } => {
            offset = t.translate_init_expr(
                init_expr,
                &wasmparser::ValType::I32,
                InitExprKind::DataOffset,
            )?;
            DataSegmentMode::Active {
                memory_index: t.remap(Item::Memory, *memory_index)?,
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

pub fn code(t: &mut dyn Translator, body: FunctionBody<'_>, s: &mut CodeSection) -> Result<()> {
    let locals = body
        .get_locals_reader()?
        .into_iter()
        .map(|local| {
            let (cnt, ty) = local?;
            Ok((cnt, t.translate_ty(&ty)?))
        })
        .collect::<Result<Vec<_>>>()?;
    let mut func = Function::new(locals);

    let mut reader = body.get_operators_reader()?;
    reader.allow_memarg64(true);
    for op in reader {
        let op = op?;
        func.instruction(&t.translate_op(&op)?);
    }
    s.function(&func);
    Ok(())
}
