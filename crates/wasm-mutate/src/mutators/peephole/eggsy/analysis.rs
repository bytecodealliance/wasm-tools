use std::cell::RefCell;

use crate::{
    module::{PrimitiveTypeInfo, TypeInfo},
    mutators::peephole::eggsy::Lang,
};
use egg::{Analysis, EGraph, Id};

/// Analysis implementation for our defined language
/// It will maintain the information regarding to map eterm to wasm and back: the DFG, the symbols
/// and the mapping between the equivalence classes and the stack entry in the DFG of the Wasm basic block
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
    // Egraph nodes
    // This will help to infer returning types from
    // local, globals and function calls
    nodes: RefCell<Vec<Lang>>,
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
            nodes: RefCell::new(Vec::new()),
        }
    }

    /// Gets returning type of node
    pub fn get_returning_tpe(&self, l: &Lang, expr: &[Lang]) -> crate::Result<PrimitiveTypeInfo> {
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
            Lang::LocalTee(operands) => {
                let idxnode = &expr[usize::from(operands[0])];
                match idxnode {
                    Lang::Arg(v) => Ok(self.locals[*v as usize].clone()),
                    _ => unreachable!("Invalid idx node {:?} for local.tee", idxnode),
                }
            }
            Lang::Wrap(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::Call(operands) => {
                let first = operands[0];
                let firstnode = &expr[usize::from(first)];
                let functionindex = match firstnode {
                    Lang::Arg(val) => *val as u32,
                    Lang::Const(val) => *val as u32,
                    _ => unreachable!(
                        "The first argument for Call nodes should be an inmmediate node type (Arg)"
                    ),
                };
                let typeinfo = self.get_functype_idx(functionindex as usize);

                match typeinfo {
                    TypeInfo::Func(ty) => {
                        if ty.returns.is_empty() {
                            return Ok(PrimitiveTypeInfo::Empty);
                        }

                        if ty.returns.len() > 1 {
                            return Err(crate::Error::NoMutationsApplicable);
                        }

                        Ok(ty.returns[0].clone())
                    }
                    _ => unreachable!("Invalid function type {:?}", typeinfo),
                }
            }
            Lang::I32Popcnt(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Popcnt(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::Drop(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::I32Load(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Load(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::RandI32 => Ok(PrimitiveTypeInfo::I32),
            Lang::RandI64 => Ok(PrimitiveTypeInfo::I64),
            Lang::Undef => Ok(PrimitiveTypeInfo::Empty),
            Lang::UnfoldI32(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::UnfoldI64(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::Arg(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::Const(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Extend8S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Extend8S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Extend16S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Extend16S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Extend32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64ExtendI32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64ExtendI32U(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::LocalSet(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::GlobalSet(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::LocalGet(operands) => {
                let idxnode = &expr[usize::from(operands[0])];
                match idxnode {
                    Lang::Arg(v) => Ok(self.locals[*v as usize].clone()),
                    _ => unreachable!("Invalid idx node {:?} for local.get", idxnode),
                }
            }
            Lang::GlobalGet(operands) => {
                let idxnode = &expr[usize::from(operands[0])];
                match idxnode {
                    Lang::Arg(v) => Ok(self.global_types[*v as usize].clone()),
                    _ => unreachable!("Invalid idx node {:?} for global.get", idxnode),
                }
            }
            Lang::I32Store(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::I64Store(_) => Ok(PrimitiveTypeInfo::Empty),
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
        egraph.analysis.nodes.borrow_mut().push(l.clone());
        Some(ClassData {
            // This type information is used only when the rewriting rules are being applied ot the egraph
            // Thats why we need the original expression in the analysis beforehand :)
            // Beyond that the random extracted expression needs to be pass to the `get_returning_tpe` method
            tpe: egraph
                .analysis
                .get_returning_tpe(l, &egraph.analysis.nodes.borrow())
                .expect("Missing type"),
        })
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        egg::merge_if_different(to, to.clone().or(from))
    }

    fn modify(_: &mut EGraph<Lang, Self>, _: Id) {}
}
