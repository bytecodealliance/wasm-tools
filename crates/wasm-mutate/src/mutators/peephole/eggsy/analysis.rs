use std::collections::HashMap;

use crate::{
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
    /// The Lang eterm is hashed, which means that if the first entrance is the expression to mutated, the eclass data could be maintained by using the
    /// Hashing of the original eterm
    lang_to_stack_entries: HashMap<Lang, (Id, Vec<usize>)>,
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
            .and_then(|(_, entries)| entries.get(0).and_then(|&x| Some(&self.minidfg.entries[x])))
    }
    /// Return the parental relations in the DFG
    pub fn get_roots(&self) -> &Vec<i32> {
        &self.minidfg.parents
    }
}

#[derive(Debug, Clone)]
pub struct ClassData {
    /// Index to the dfg stack entry
    pub eclass_and_stackentries: (Id, Vec<usize>),

    // The eclass can represent several stack entries
    // Every time a new stack entry information is requested
    current_entry: usize,
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
        self.eclass_and_stackentries.1 == other.eclass_and_stackentries.1 // && self.operatoridx == other.operatoridx
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl Analysis<Lang> for PeepholeMutationAnalysis {
    type Data = Option<ClassData>;

    fn make(egraph: &EGraph<Lang, Self>, l: &Lang) -> Self::Data {
        // This works beacuase always the first expression is the one constructed from the DFG
        // The node id to stack is consistent then with the order in which this method is call
        //
        if egraph.analysis.lang_to_stack_entries.contains_key(l) {
            println!("eg {:?} {:?}", l, egraph.analysis.lang_to_stack_entries[&l]);
            Some(ClassData {
                eclass_and_stackentries: egraph.analysis.lang_to_stack_entries[&l].clone(),
                current_entry: 0,
            })
        } else {
            None
        }
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        egg::merge_if_different(to, to.clone().or(from))
    }

    fn modify(egraph: &mut EGraph<Lang, Self>, id: Id) {}
}
