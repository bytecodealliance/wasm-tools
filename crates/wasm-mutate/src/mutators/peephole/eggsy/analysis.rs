use std::{collections::HashMap};

use crate::{
    error::EitherType,
    module::PrimitiveTypeInfo,
    mutators::peephole::{
        dfg::{MiniDFG, StackEntry},
        eggsy::Lang,
    },
};
use egg::{Analysis, EGraph, Id};

/// Analysis implementation for our defined language
/// It will maintain the information regarding to map eterm to wasm and back: the DFG, the symbols
/// and the mapping between the equivalence classes and the stack entry in the DFG of the Wasm basic block
#[derive(Clone, Debug, Default)]
pub struct PeepholeMutationAnalysis {
    /// Egraph node ID to Stack entry in the minidfg entries
    /// The Lang eterm is hashed, which means that if the first entrance is the expression to mutated,
    /// the eclass data could be maintained by using the
    /// Hashing of the original eterm
    pub lang_to_stack_entries: HashMap<Lang, (Id, Vec<usize>)>,
    /// DFG data mapping from the input Wasm basic block
    minidfg: MiniDFG,
}

impl PeepholeMutationAnalysis {
    /// Returns a new analysis from the given DFG
    pub fn new(lang_to_stack_entries: HashMap<Lang, (Id, Vec<usize>)>, minidfg: MiniDFG) -> Self {
        PeepholeMutationAnalysis {
            lang_to_stack_entries,
            minidfg,
        }
    }

    /// Returns a stack entry from the DFG given its index
    pub fn get_stack_entry(&self, idx: usize) -> &StackEntry {
        &self.minidfg.entries[idx]
    }
    /// Returns a stack entry from the DFG given the symbol name
    /// If the symbol is not in the DFG, it returns None
    pub fn get_stack_entry_from_symbol(&self, symbol: String) -> Option<&StackEntry> {
        self.lang_to_stack_entries
            .get(&Lang::Symbol(symbol.into()))
            .and_then(|(_, entries)| entries.get(0).map(|&x| &self.minidfg.entries[x]))
    }
    /// Returns a stack entry from the DFG given node repr
    /// If the node is not in the DFG, it returns None
    pub fn get_stack_entry_from_lang(&self, l: &Lang) -> Option<&StackEntry> {
        self.lang_to_stack_entries
            .get(l)
            .and_then(|(_, entries)| entries.get(0).map(|&x| &self.minidfg.entries[x]))
    }

    /// Return the parental relations in the DFG
    pub fn get_roots(&self) -> &Vec<i32> {
        &self.minidfg.parents
    }

    /// Returns returning type of node
    pub fn get_tpe(&self, l: &Lang) -> crate::Result<PrimitiveTypeInfo> {
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
            Lang::Tee(_) => {
                let entry = self.get_stack_entry_from_lang(l).ok_or_else(|| {
                    crate::Error::UnsupportedType(EitherType::EggError(
                        "The current symbol cannot be retrieved".into(),
                    ))
                })?;
                Ok(entry.return_type.clone())
            }
            Lang::Wrap(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::Call(_) => {
                // Replace all by this
                let entry = self.get_stack_entry_from_lang(l).ok_or_else(|| {
                    crate::Error::UnsupportedType(EitherType::EggError(
                        "The current symbol cannot be retrieved".into(),
                    ))
                })?;
                Ok(entry.return_type.clone())
            }
            Lang::I32Popcnt(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Popcnt(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::Drop(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::I32Load(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Load(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::Rand => Ok(PrimitiveTypeInfo::I32),
            Lang::Undef => Ok(PrimitiveTypeInfo::Empty),
            Lang::Unfold(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::I32(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64(_) => Ok(PrimitiveTypeInfo::I64),
            // Get the type from the locals
            Lang::Symbol(s) => {
                let entry = self
                    .get_stack_entry_from_symbol(s.to_string())
                    .ok_or_else(|| {
                        crate::Error::UnsupportedType(EitherType::EggError(
                            "The current symbol cannot be retrieved".into(),
                        ))
                    })?;
                Ok(entry.return_type.clone())
            }
            Lang::Arg(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::Const(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I32Extend8S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Extend8S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I32Extend16S(_) => Ok(PrimitiveTypeInfo::I32),
            Lang::I64Extend16S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64Extend32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64ExtendI32S(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::I64ExtendI32U(_) => Ok(PrimitiveTypeInfo::I64),
            Lang::Set(_) => Ok(PrimitiveTypeInfo::Empty),
            Lang::GlobalSet(_) => Ok(PrimitiveTypeInfo::Empty),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClassData {
    /// Index to the dfg stack entry
    pub eclass_and_stackentries: (Id, Vec<usize>),
    // The eclass can represent several stack entries
    // Every time a new stack entry information is requested
    current_entry: usize,

    /// Type 't' of the operator
    /// 't'.op
    pub tpe: PrimitiveTypeInfo,
}

impl ClassData {
    /// Returns the stack enntry that corresponds to this equivalence class.
    /// The method will check if this equivalence class has a stack entry in the DFG of the analysis.
    /// If true the StackEntry is then returned
    pub fn get_next_stack_entry(&self, analysis: &PeepholeMutationAnalysis) -> StackEntry {
        let idx = self.eclass_and_stackentries.1[self.current_entry];
        analysis.minidfg.entries[idx].clone()
    }
}

impl PartialEq for ClassData {
    fn eq(&self, other: &Self) -> bool {
        self.eclass_and_stackentries.1 == other.eclass_and_stackentries.1
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl Analysis<Lang> for PeepholeMutationAnalysis {
    type Data = Option<ClassData>;

    fn make(egraph: &EGraph<Lang, Self>, l: &Lang) -> Self::Data {
        // This works because always the first expression is the one constructed from the DFG
        // The node id to stack is consistent then with the order in which this method is call
        if egraph.analysis.lang_to_stack_entries.contains_key(l) {
            Some(ClassData {
                eclass_and_stackentries: egraph.analysis.lang_to_stack_entries[l].clone(),
                current_entry: 0,
                tpe: egraph.analysis.get_tpe(l).expect("Missing type"),
            })
        } else {
            None
        }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        egg::merge_if_different(to, to.clone().or(from))
    }

    fn modify(_: &mut EGraph<Lang, Self>, _: Id) {}
}
