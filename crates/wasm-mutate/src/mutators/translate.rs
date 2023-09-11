use crate::{Error, Result};
use wasm_encoder::*;
use wasmparser::{DataKind, ElementKind, FunctionBody, Global, Operator};

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
pub enum ConstExprKind {
    Global,
    ElementOffset,
    ElementFunction,
    DataOffset,
    TableInit,
}

pub trait Translator {
    fn as_obj(&mut self) -> &mut dyn Translator;

    fn translate_func_type(&mut self, ty: wasmparser::FuncType, s: &mut TypeSection) -> Result<()> {
        func_type(self.as_obj(), ty, s)
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

    fn translate_refty(&mut self, t: &wasmparser::RefType) -> Result<RefType> {
        refty(self.as_obj(), t)
    }

    fn translate_heapty(&mut self, t: &wasmparser::HeapType) -> Result<HeapType> {
        heapty(self.as_obj(), t)
    }

    fn translate_global(&mut self, g: Global, s: &mut GlobalSection) -> Result<()> {
        global(self.as_obj(), g, s)
    }

    fn translate_const_expr(
        &mut self,
        e: &wasmparser::ConstExpr<'_>,
        _ty: &wasmparser::ValType,
        ctx: ConstExprKind,
    ) -> Result<wasm_encoder::ConstExpr> {
        const_expr(self.as_obj(), e, ctx)
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

    fn translate_memarg(&mut self, arg: &wasmparser::MemArg) -> Result<MemArg> {
        memarg(self.as_obj(), arg)
    }

    fn remap(&mut self, item: Item, idx: u32) -> Result<u32> {
        let _ = item;
        Ok(idx)
    }
}

pub struct DefaultTranslator;

impl Translator for DefaultTranslator {
    fn as_obj(&mut self) -> &mut dyn Translator {
        self
    }
}

pub fn func_type(
    t: &mut dyn Translator,
    ty: wasmparser::FuncType,
    s: &mut TypeSection,
) -> Result<()> {
    s.function(
        ty.params()
            .iter()
            .map(|ty| t.translate_ty(ty))
            .collect::<Result<Vec<_>>>()?,
        ty.results()
            .iter()
            .map(|ty| t.translate_ty(ty))
            .collect::<Result<Vec<_>>>()?,
    );
    Ok(())
}

pub fn table_type(
    t: &mut dyn Translator,
    ty: &wasmparser::TableType,
) -> Result<wasm_encoder::TableType> {
    Ok(wasm_encoder::TableType {
        element_type: t.translate_refty(&ty.element_type)?,
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

pub fn ty(t: &mut dyn Translator, ty: &wasmparser::ValType) -> Result<ValType> {
    match ty {
        wasmparser::ValType::I32 => Ok(ValType::I32),
        wasmparser::ValType::I64 => Ok(ValType::I64),
        wasmparser::ValType::F32 => Ok(ValType::F32),
        wasmparser::ValType::F64 => Ok(ValType::F64),
        wasmparser::ValType::V128 => Ok(ValType::V128),
        wasmparser::ValType::Ref(ty) => Ok(ValType::Ref(t.translate_refty(ty)?)),
    }
}

pub fn refty(t: &mut dyn Translator, ty: &wasmparser::RefType) -> Result<RefType> {
    Ok(RefType {
        nullable: ty.is_nullable(),
        heap_type: t.translate_heapty(&ty.heap_type())?,
    })
}

pub fn heapty(t: &mut dyn Translator, ty: &wasmparser::HeapType) -> Result<HeapType> {
    match ty {
        wasmparser::HeapType::Func => Ok(HeapType::Func),
        wasmparser::HeapType::Extern => Ok(HeapType::Extern),
        wasmparser::HeapType::Any => Ok(HeapType::Any),
        wasmparser::HeapType::None => Ok(HeapType::None),
        wasmparser::HeapType::NoExtern => Ok(HeapType::NoExtern),
        wasmparser::HeapType::NoFunc => Ok(HeapType::NoFunc),
        wasmparser::HeapType::Eq => Ok(HeapType::Eq),
        wasmparser::HeapType::Struct => Ok(HeapType::Struct),
        wasmparser::HeapType::Array => Ok(HeapType::Array),
        wasmparser::HeapType::I31 => Ok(HeapType::I31),
        wasmparser::HeapType::Indexed(i) => {
            Ok(HeapType::Indexed(t.remap(Item::Type, (*i).into())?))
        }
    }
}

pub fn global(t: &mut dyn Translator, global: Global, s: &mut GlobalSection) -> Result<()> {
    let ty = t.translate_global_type(&global.ty)?;
    let insn = t.translate_const_expr(
        &global.init_expr,
        &global.ty.content_type,
        ConstExprKind::Global,
    )?;
    s.global(ty, &insn);
    Ok(())
}

pub fn const_expr(
    t: &mut dyn Translator,
    e: &wasmparser::ConstExpr<'_>,
    ctx: ConstExprKind,
) -> Result<wasm_encoder::ConstExpr> {
    let mut e = e.get_operators_reader();
    let mut offset_bytes = Vec::new();
    let op = e.read()?;
    if let ConstExprKind::ElementFunction = ctx {
        match op {
            Operator::RefFunc { .. }
            | Operator::RefNull {
                hty: wasmparser::HeapType::Func,
                ..
            }
            | Operator::GlobalGet { .. } => {}
            _ => return Err(Error::no_mutations_applicable()),
        }
    }
    t.translate_op(&op)?.encode(&mut offset_bytes);
    match e.read()? {
        Operator::End if e.eof() => {}
        _ => return Err(Error::no_mutations_applicable()),
    }
    Ok(wasm_encoder::ConstExpr::raw(offset_bytes))
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
            offset_expr,
        } => {
            offset = t.translate_const_expr(
                offset_expr,
                &wasmparser::ValType::I32,
                ConstExprKind::ElementOffset,
            )?;
            let table_index = table_index.unwrap_or(0);
            let table = t.remap(Item::Table, table_index)?;
            ElementMode::Active {
                table: if table == 0 { None } else { Some(table) },
                offset: &offset,
            }
        }
        ElementKind::Passive => ElementMode::Passive,
        ElementKind::Declared => ElementMode::Declared,
    };
    let functions;
    let exprs;
    let elements = match element.items {
        wasmparser::ElementItems::Functions(reader) => {
            functions = reader
                .into_iter()
                .map(|f| t.remap(Item::Function, f?))
                .collect::<Result<Vec<_>, _>>()?;
            Elements::Functions(&functions)
        }
        wasmparser::ElementItems::Expressions(ty, reader) => {
            exprs = reader
                .into_iter()
                .map(|f| {
                    t.translate_const_expr(
                        &f?,
                        &wasmparser::ValType::Ref(ty),
                        ConstExprKind::ElementFunction,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            Elements::Expressions(t.translate_refty(&ty)?, &exprs)
        }
    };
    s.segment(ElementSegment { mode, elements });
    Ok(())
}

#[allow(unused_variables)]
pub fn op(t: &mut dyn Translator, op: &Operator<'_>) -> Result<Instruction<'static>> {
    use wasm_encoder::Instruction as I;

    macro_rules! translate {
        ($( @$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
            Ok(match op {
                $(
                    wasmparser::Operator::$op $({ $($arg),* })? => {
                        $(
                            $(let $arg = translate!(map $arg $arg);)*
                        )?
                        translate!(build $op $($($arg)*)?)
                    }
                )*
            })
        };

        // This case is used to map, based on the name of the field, from the
        // wasmparser payload type to the wasm-encoder payload type through
        // `Translator` as applicable.
        (map $arg:ident tag_index) => (t.remap(Item::Tag, *$arg)?);
        (map $arg:ident function_index) => (t.remap(Item::Function, *$arg)?);
        (map $arg:ident table) => (t.remap(Item::Table, *$arg)?);
        (map $arg:ident table_index) => (t.remap(Item::Table, *$arg)?);
        (map $arg:ident table) => (t.remap(Item::Table, *$arg)?);
        (map $arg:ident dst_table) => (t.remap(Item::Table, *$arg)?);
        (map $arg:ident src_table) => (t.remap(Item::Table, *$arg)?);
        (map $arg:ident type_index) => (t.remap(Item::Type, *$arg)?);
        (map $arg:ident global_index) => (t.remap(Item::Global, *$arg)?);
        (map $arg:ident mem) => (t.remap(Item::Memory, *$arg)?);
        (map $arg:ident src_mem) => (t.remap(Item::Memory, *$arg)?);
        (map $arg:ident dst_mem) => (t.remap(Item::Memory, *$arg)?);
        (map $arg:ident data_index) => (t.remap(Item::Data, *$arg)?);
        (map $arg:ident elem_index) => (t.remap(Item::Element, *$arg)?);
        (map $arg:ident blockty) => (t.translate_block_type($arg)?);
        (map $arg:ident relative_depth) => (*$arg);
        (map $arg:ident targets) => ((
            $arg
                .targets()
                .collect::<Result<Vec<_>, wasmparser::BinaryReaderError>>()?
                .into(),
            $arg.default(),
        ));
        (map $arg:ident table_byte) => (());
        (map $arg:ident mem_byte) => (());
        (map $arg:ident flags) => (());
        (map $arg:ident ty) => (t.translate_ty($arg)?);
        (map $arg:ident hty) => (t.translate_heapty($arg)?);
        (map $arg:ident memarg) => (t.translate_memarg($arg)?);
        (map $arg:ident local_index) => (*$arg);
        (map $arg:ident value) => ($arg);
        (map $arg:ident lane) => (*$arg);
        (map $arg:ident lanes) => (*$arg);

        // This case takes the arguments of a wasmparser instruction and creates
        // a wasm-encoder instruction. There are a few special cases for where
        // the structure of a wasmparser instruction differs from that of
        // wasm-encoder.
        (build $op:ident) => (I::$op);
        (build BrTable $arg:ident) => (I::BrTable($arg.0, $arg.1));
        (build I32Const $arg:ident) => (I::I32Const(*$arg));
        (build I64Const $arg:ident) => (I::I64Const(*$arg));
        (build F32Const $arg:ident) => (I::F32Const(f32::from_bits($arg.bits())));
        (build F64Const $arg:ident) => (I::F64Const(f64::from_bits($arg.bits())));
        (build V128Const $arg:ident) => (I::V128Const($arg.i128()));
        (build $op:ident $arg:ident) => (I::$op($arg));
        (build CallIndirect $ty:ident $table:ident $_:ident) => (I::CallIndirect {
            ty: $ty,
            table: $table,
        });
        (build ReturnCallIndirect $ty:ident $table:ident) => (I::ReturnCallIndirect {
            ty: $ty,
            table: $table,
        });
        (build MemoryGrow $mem:ident $_:ident) => (I::MemoryGrow($mem));
        (build MemorySize $mem:ident $_:ident) => (I::MemorySize($mem));
        (build $op:ident $($arg:ident)*) => (I::$op { $($arg),* });
    }

    wasmparser::for_each_operator!(translate)
}

pub fn block_type(t: &mut dyn Translator, ty: &wasmparser::BlockType) -> Result<BlockType> {
    match ty {
        wasmparser::BlockType::Empty => Ok(BlockType::Empty),
        wasmparser::BlockType::Type(ty) => Ok(BlockType::Result(t.translate_ty(ty)?)),
        wasmparser::BlockType::FuncType(f) => Ok(BlockType::FunctionType(t.remap(Item::Type, *f)?)),
    }
}

pub fn memarg(t: &mut dyn Translator, memarg: &wasmparser::MemArg) -> Result<MemArg> {
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
            offset_expr,
        } => {
            offset = t.translate_const_expr(
                offset_expr,
                &wasmparser::ValType::I32,
                ConstExprKind::DataOffset,
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
