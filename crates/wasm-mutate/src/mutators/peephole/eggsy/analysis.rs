use crate::{
    module::{PrimitiveTypeInfo, TypeInfo},
    mutators::peephole::{eggsy::Lang, EG},
    Error,
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
}

impl PeepholeMutationAnalysis {
    /// Returns a new analysis from the given DFG
    pub fn new(
        global_types: Vec<PrimitiveTypeInfo>,
        locals: Vec<PrimitiveTypeInfo>,
        types_map: Vec<TypeInfo>,
        function_map: Vec<u32>,
    ) -> Self {
        PeepholeMutationAnalysis {
            global_types,
            locals,
            types_map,
            function_map,
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
            Lang::LocalTee(idx, _) => Ok(self.locals[*idx as usize].clone()),
            Lang::Wrap(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::Call(idx, _) => {
                let typeinfo = self.get_functype_idx(*idx);

                match typeinfo {
                    TypeInfo::Func(ty) => {
                        if ty.returns.is_empty() {
                            return Ok(PrimitiveTypeInfo::Empty);
                        }

                        if ty.returns.len() > 1 {
                            return Err(Error::no_mutations_applicable());
                        }

                        Ok(ty.returns[0].clone())
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
            Lang::Undef => Ok(PrimitiveTypeInfo::Empty),
            Lang::UnfoldI32(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::UnfoldI64(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Extend8S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Extend8S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Extend16S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Extend16S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Extend32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64ExtendI32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64ExtendI32U(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::LocalSet(_, _) => Ok(PrimitiveTypeInfo::Empty),
            Lang::GlobalSet(_, _) => Ok(PrimitiveTypeInfo::Empty),
            Lang::LocalGet(idx) => Ok(self.locals[*idx as usize].clone()),
            Lang::GlobalGet(v) => Ok(self.global_types[*v as usize].clone()),
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
            Lang::MemoryGrow { .. } => Ok(PrimitiveTypeInfo::I32),
            Lang::MemorySize { .. } => Ok(PrimitiveTypeInfo::I32),
            Lang::I32UseGlobal(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64UseGlobal(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::F32UseGlobal(_) => Ok(PrimitiveTypeInfo::F32),
            Lang::F64UseGlobal(_) => Ok(PrimitiveTypeInfo::F64),
        }
    }

    /// Returns the function type based on the index of the function type
    /// `types[functions[idx]]`
    pub fn get_functype_idx(&self, idx: usize) -> &TypeInfo {
        let functpeindex = self.function_map[idx] as usize;
        &self.types_map[functpeindex]
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
                .expect("Missing type"),
        })
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        egg::merge_if_different(to, to.clone().or(from))
    }

    fn modify(_: &mut EGraph<Lang, Self>, _: Id) {}
}
