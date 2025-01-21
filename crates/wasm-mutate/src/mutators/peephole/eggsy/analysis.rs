use crate::{
    module::{PrimitiveTypeInfo, TypeInfo},
    mutators::peephole::{
        eggsy::{lang::RefType, Lang},
        EG,
    },
    Error, ModuleInfo,
};
use egg::{Analysis, EGraph, Id};

/// Analysis implementation for our defined language
/// It will maintain the information regarding to map eterm to wasm and back: the DFG, the symbols
/// and the mapping between the equivalence classes and the stack entry in the DFG of the Wasm basic block]

#[derive(Clone)]
pub struct PeepholeMutationAnalysis {
    /// Module information for globals
    global_types: Vec<PrimitiveTypeInfo>,
    /// Module information for function locals
    locals: Vec<PrimitiveTypeInfo>,
    /// Information from the ModuleInfo
    /// types for functions
    types_map: Vec<TypeInfo>,
    /// function idx to type idx
    function_map: Vec<u32>,
    /// table index to the type of element it has
    table_types: Vec<wasmparser::TableType>,
    memory_types: Vec<wasmparser::MemoryType>,
}

impl PeepholeMutationAnalysis {
    /// Returns a new analysis from the given DFG
    pub fn new(info: &ModuleInfo<'_>, locals: Vec<PrimitiveTypeInfo>) -> Self {
        PeepholeMutationAnalysis {
            locals,
            global_types: info.global_types.clone(),
            types_map: info.types_map.clone(),
            function_map: info.function_map.clone(),
            table_types: info.table_types.clone(),
            memory_types: info.memory_types.clone(),
        }
    }

    /// Gets returning type of node
    pub fn get_returning_tpe(&self, l: &Lang, eg: &EG) -> crate::Result<PrimitiveTypeInfo> {
        match l {
            Lang::I32Add(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Add(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Sub(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Sub(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Mul(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Mul(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32And(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64And(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Or(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Or(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Xor(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Xor(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Shl(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Shl(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32ShrU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64ShrU(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32DivU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64DivU(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32DivS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64DivS(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32ShrS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64ShrS(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32RotR(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64RotR(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32RotL(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64RotL(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32RemS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64RemS(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32RemU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64RemU(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Eqz(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Eqz(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Eq(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Eq(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Ne(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Ne(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32LtS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64LtS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32LtU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64LtU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32GtS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64GtS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32GtU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64GtU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32LeS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64LeS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32LeU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64LeU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32GeS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64GeS(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32GeU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64GeU(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::LocalTee(idx, _) => Ok(self.locals[*idx as usize]),
            Lang::Wrap(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::Call(idx, _) => {
                let type_idx = self.function_map[*idx as usize];
                match &self.types_map[type_idx as usize] {
                    TypeInfo::Func(ty) => {
                        if ty.returns.is_empty() {
                            return Ok(PrimitiveTypeInfo::Empty);
                        }

                        if ty.returns.len() > 1 {
                            return Err(Error::no_mutations_applicable());
                        }

                        Ok(ty.returns[0])
                    }
                }
            }
            Lang::I32Popcnt(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Popcnt(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::Drop(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::I32Load { .. } => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Load { .. } => Ok(PrimitiveTypeInfo::I64),
            Lang::RandI32 => Ok(PrimitiveTypeInfo::I32),
            Lang::RandI64 => Ok(PrimitiveTypeInfo::I64),
            Lang::RandF32 => Ok(PrimitiveTypeInfo::F32),
            Lang::RandF64 => Ok(PrimitiveTypeInfo::F64),
            Lang::Undef => Ok(PrimitiveTypeInfo::Empty),
            Lang::UnfoldI32(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::UnfoldI64(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::V128(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32Extend8S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Extend8S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Extend16S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Extend16S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Extend32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64ExtendI32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64ExtendI32U(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::LocalSet(_, _) => Ok(PrimitiveTypeInfo::Empty),
            Lang::GlobalSet(_, _) => Ok(PrimitiveTypeInfo::Empty),
            Lang::LocalGet(idx) => Ok(self.locals[*idx as usize]),
            Lang::GlobalGet(v) => Ok(self.global_types[*v as usize]),
            Lang::I32Store { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::I64Store { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::F32(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Add(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Add(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Sub(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Sub(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Mul(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Mul(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Div(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Div(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Min(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Min(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Max(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Max(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Copysign(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Copysign(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Eq(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F64Eq(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F32Ne(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F64Ne(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F32Lt(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F64Lt(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F32Gt(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F64Gt(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F32Le(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F64Le(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F32Ge(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::F64Ge(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Clz(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Ctz(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Ctz(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Clz(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::F32Abs(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Abs(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Neg(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Neg(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Sqrt(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Sqrt(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Ceil(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Ceil(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Floor(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Floor(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Trunc(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Trunc(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F32Nearest(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Nearest(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::I32TruncF32S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32TruncF32U(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32TruncF64S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32TruncF64U(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64TruncF32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64TruncF32U(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64TruncF64S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64TruncF64U(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::F32ConvertI32S(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F32ConvertI32U(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F32ConvertI64S(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F32ConvertI64U(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F32DemoteF64(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64ConvertI32S(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F64ConvertI32U(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F64ConvertI64S(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F64ConvertI64U(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::F64PromoteF32(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::I32ReinterpretF32(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64ReinterpretF64(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::F32ReinterpretI32(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64ReinterpretI64(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::I32TruncSatF32S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32TruncSatF32U(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32TruncSatF64S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32TruncSatF64U(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64TruncSatF32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64TruncSatF32U(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64TruncSatF64S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64TruncSatF64U(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::F32Load { .. } => Ok(PrimitiveTypeInfo::F32),
            Lang::F64Load { .. } => Ok(PrimitiveTypeInfo::F64),
            Lang::I32Load8S { .. } => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Load8U { .. } => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Load16S { .. } => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Load16U { .. } => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Load8S { .. } => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Load8U { .. } => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Load16S { .. } => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Load16U { .. } => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Load32S { .. } => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Load32U { .. } => Ok(PrimitiveTypeInfo::I64),
            Lang::F32Store { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::F64Store { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::I32Store8 { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::I32Store16 { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::I64Store8 { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::I64Store16 { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::I64Store32 { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::Nop => Ok(PrimitiveTypeInfo::Empty),
            // This node is not directly written to Wasm
            Lang::Container(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::Select([consequent, alternative, _]) => {
                // Get from operands
                let consequenttpe = eg[*consequent]
                    .data
                    .clone()
                    .expect("Missing operand type")
                    .tpe;
                let alternativetpe = eg[*alternative]
                    .data
                    .clone()
                    .expect("Missing operand type")
                    .tpe;
                debug_assert_eq!(consequenttpe, alternativetpe);
                Ok(consequenttpe)
            }
            Lang::MemoryGrow(mem, _) | Lang::MemorySize(mem) => {
                let ty = self.memory_types[*mem as usize];
                if ty.memory64 {
                    Ok(PrimitiveTypeInfo::I64)
                } else {
                    Ok(PrimitiveTypeInfo::I32)
                }
            }
            Lang::MemoryInit { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::DataDrop(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::MemoryCopy { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::MemoryFill { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::TableGrow(table, _) => {
                let ty = self.table_types[*table as usize];
                if ty.table64 {
                    Ok(PrimitiveTypeInfo::I64)
                } else {
                    Ok(PrimitiveTypeInfo::I32)
                }
            }
            Lang::TableSize(table) => {
                let ty = self.table_types[*table as usize];
                if ty.table64 {
                    Ok(PrimitiveTypeInfo::I64)
                } else {
                    Ok(PrimitiveTypeInfo::I32)
                }
            }
            Lang::TableInit { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::ElemDrop(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::TableCopy { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::TableFill { .. } => Ok(PrimitiveTypeInfo::Empty),
            Lang::TableSet(..) => Ok(PrimitiveTypeInfo::Empty),
            Lang::TableGet(idx, _) => {
                let ty = self.table_types[*idx as usize];
                ty.element_type.try_into()
            }
            Lang::I32UseGlobal(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64UseGlobal(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::F32UseGlobal(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64UseGlobal(_) => Ok(PrimitiveTypeInfo::F64),
            Lang::RefNull(RefType::Func) => Ok(PrimitiveTypeInfo::FuncRef),
            Lang::RefNull(RefType::Extern) => Ok(PrimitiveTypeInfo::ExternRef),
            Lang::RefFunc(_) => Ok(PrimitiveTypeInfo::FuncRef),
            Lang::RefIsNull(_) => Ok(PrimitiveTypeInfo::I32),

            Lang::V128Load(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load8x8S(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load8x8U(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load16x4S(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load16x4U(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load32x2S(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load32x2U(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load8Splat(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load16Splat(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load32Splat(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load64Splat(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load32Zero(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load64Zero(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Store(..) => Ok(PrimitiveTypeInfo::Empty),
            Lang::V128Load8Lane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load16Lane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load32Lane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Load64Lane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Store8Lane(..) => Ok(PrimitiveTypeInfo::Empty),
            Lang::V128Store16Lane(..) => Ok(PrimitiveTypeInfo::Empty),
            Lang::V128Store32Lane(..) => Ok(PrimitiveTypeInfo::Empty),
            Lang::V128Store64Lane(..) => Ok(PrimitiveTypeInfo::Empty),
            Lang::V128Not(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128And(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128AndNot(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Or(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Xor(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128Bitselect(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::V128AnyTrue(..) => Ok(PrimitiveTypeInfo::I32),

            Lang::I8x16ExtractLaneS(..) => Ok(PrimitiveTypeInfo::I32),
            Lang::I8x16ExtractLaneU(..) => Ok(PrimitiveTypeInfo::I32),
            Lang::I8x16ReplaceLane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtractLaneS(..) => Ok(PrimitiveTypeInfo::I32),
            Lang::I16x8ExtractLaneU(..) => Ok(PrimitiveTypeInfo::I32),
            Lang::I16x8ReplaceLane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtractLane(..) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32x4ReplaceLane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ExtractLane(..) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64x2ReplaceLane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4ExtractLane(..) => Ok(PrimitiveTypeInfo::F32),
            Lang::F32x4ReplaceLane(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2ExtractLane(..) => Ok(PrimitiveTypeInfo::F64),
            Lang::F64x2ReplaceLane(..) => Ok(PrimitiveTypeInfo::V128),

            Lang::I8x16Swizzle(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16Shuffle(..) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16Splat(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Splat(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Splat(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2Splat(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Splat(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Splat(_) => Ok(PrimitiveTypeInfo::V128),

            Lang::I8x16Eq(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16Ne(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16LtS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16LtU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16GtS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16GtU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16LeS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16LeU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16GeS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16GeU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Eq(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Ne(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8LtS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8LtU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8GtS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8GtU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8LeS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8LeU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8GeS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8GeU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Eq(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Ne(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4LtS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4LtU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4GtS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4GtU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4LeS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4LeU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4GeS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4GeU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2Eq(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2Ne(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2LtS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2GtS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2LeS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2GeS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Eq(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Ne(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Lt(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Gt(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Le(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Ge(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Eq(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Ne(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Lt(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Gt(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Le(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Ge(_) => Ok(PrimitiveTypeInfo::V128),

            Lang::I8x16Abs(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16Neg(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16Popcnt(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16AllTrue(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I8x16Bitmask(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I8x16NarrowI16x8S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16NarrowI16x8U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16Shl(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16ShrS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16ShrU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16Add(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16AddSatS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16AddSatU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16Sub(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16SubSatS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16SubSatU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16MinS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16MinU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16MaxS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16MaxU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16AvgrU(_) => Ok(PrimitiveTypeInfo::V128),

            Lang::I16x8ExtAddPairwiseI8x16S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtAddPairwiseI8x16U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Abs(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Neg(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Q15MulrSatS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8AllTrue(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I16x8Bitmask(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I16x8NarrowI32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8NarrowI32x4U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtendLowI8x16S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtendHighI8x16S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtendLowI8x16U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtendHighI8x16U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Shl(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ShrS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ShrU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Add(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8AddSatS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8AddSatU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Sub(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8SubSatS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8SubSatU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8Mul(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8MinS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8MinU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8MaxS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8MaxU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8AvgrU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtMulLowI8x16S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtMulHighI8x16S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtMulLowI8x16U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8ExtMulHighI8x16U(_) => Ok(PrimitiveTypeInfo::V128),

            Lang::I32x4ExtAddPairwiseI16x8S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtAddPairwiseI16x8U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Abs(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Neg(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4AllTrue(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32x4Bitmask(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32x4ExtendLowI16x8S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtendHighI16x8S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtendLowI16x8U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtendHighI16x8U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Shl(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ShrS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ShrU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Add(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Sub(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4Mul(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4MinS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4MinU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4MaxS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4MaxU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4DotI16x8S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtMulLowI16x8S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtMulHighI16x8S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtMulLowI16x8U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4ExtMulHighI16x8U(_) => Ok(PrimitiveTypeInfo::V128),

            Lang::I64x2Abs(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2Neg(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2AllTrue(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64x2Bitmask(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64x2ExtendLowI32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ExtendHighI32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ExtendLowI32x4U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ExtendHighI32x4U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2Shl(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ShrS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ShrU(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2Add(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2Sub(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2Mul(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ExtMulLowI32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ExtMulHighI32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ExtMulLowI32x4U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2ExtMulHighI32x4U(_) => Ok(PrimitiveTypeInfo::V128),

            Lang::F32x4Ceil(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Floor(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Trunc(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Nearest(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Abs(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Neg(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Sqrt(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Add(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Sub(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Mul(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Div(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Min(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4Max(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4PMin(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4PMax(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Ceil(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Floor(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Trunc(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Nearest(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Abs(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Neg(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Sqrt(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Add(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Sub(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Mul(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Div(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Min(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2Max(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2PMin(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2PMax(_) => Ok(PrimitiveTypeInfo::V128),

            Lang::I32x4TruncSatF32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4TruncSatF32x4U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4ConvertI32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4ConvertI32x4U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4TruncSatF64x2SZero(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4TruncSatF64x2UZero(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2ConvertLowI32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2ConvertLowI32x4U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4DemoteF64x2Zero(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2PromoteLowF32x4(_) => Ok(PrimitiveTypeInfo::V128),

            Lang::I8x16RelaxedSwizzle(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4RelaxedTruncF32x4S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4RelaxedTruncF32x4U(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4RelaxedTruncF64x2SZero(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4RelaxedTruncF64x2UZero(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4RelaxedMadd(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4RelaxedNmadd(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2RelaxedMadd(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2RelaxedNmadd(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I8x16RelaxedLaneselect(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8RelaxedLaneselect(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4RelaxedLaneselect(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I64x2RelaxedLaneselect(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4RelaxedMin(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F32x4RelaxedMax(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2RelaxedMin(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::F64x2RelaxedMax(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8RelaxedQ15mulrS(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I16x8RelaxedDotI8x16I7x16S(_) => Ok(PrimitiveTypeInfo::V128),
            Lang::I32x4RelaxedDotI8x16I7x16AddS(_) => Ok(PrimitiveTypeInfo::V128),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClassData {
    /// Type 't' of the operator
    /// 't'.op
    pub tpe: PrimitiveTypeInfo,
}

impl PartialEq for ClassData {
    fn eq(&self, other: &Self) -> bool {
        self.tpe == other.tpe
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl Analysis<Lang> for PeepholeMutationAnalysis {
    type Data = Option<ClassData>;

    fn make(egraph: &EGraph<Lang, Self>, l: &Lang) -> Self::Data {
        // We build the nodes collection in the same order the egraph is built
        Some(ClassData {
            // This type information is used only when the rewriting rules are being applied ot the egraph
            // Thats why we need the original expression in the analysis beforehand :)
            // Beyond that the random extracted expression needs to be pass to the `get_returning_tpe` method
            tpe: egraph
                .analysis
                .get_returning_tpe(l, egraph)
                .unwrap_or(PrimitiveTypeInfo::Empty),
        })
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        egg::merge_if_different(to, to.clone().or(from))
    }

    fn modify(_: &mut EGraph<Lang, Self>, _: Id) {}
}
