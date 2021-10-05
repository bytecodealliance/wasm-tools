use std::{collections::HashMap, ops::Add};

use crate::{
    module::PrimitiveTypeInfo,
    mutators::peephole::{
        dfg::{MiniDFG, StackEntry},
        eggsy::Lang,
    },
};
use egg::{Analysis, CostFunction, EGraph, Id, Language};

/// Analysis implementation for our defined language
/// It will maintain the information regarding to map eterm to wasm and back: the DFG, the symbols
/// and the mapping between the equivalence classes and the stack entry in the DFG of the Wasm basic block
#[derive(Clone, Debug, Default)]
pub struct PeepholeMutationAnalysis {
    /// Egraph node ID to Stack entry in the minidfg entries
    id_to_stack: HashMap<Id, usize>,
    /// DFG data mapping from the input Wasm basic block
    minidfg: MiniDFG,
    /// Symbol map to corresponding stack entries in the minidfg field
    symbolsmap: HashMap<String /* Check that this can be an ID to a symbol */, usize>,
}

impl PeepholeMutationAnalysis {
    /// Returns a new analysis from the given DFG
    pub fn new(
        id_to_stack: HashMap<Id, usize>,
        minidfg: MiniDFG,
        symbolsmap: HashMap<String, usize>,
    ) -> Self {
        PeepholeMutationAnalysis {
            id_to_stack,
            minidfg,
            symbolsmap,
        }
    }

    /// Returns a stack entry from the DFG given its index
    pub fn get_stack_entry(&self, idx: usize) -> &StackEntry {
        &self.minidfg.entries[idx]
    }
    /// Returns a stack entry from the DFG given the symbol name
    /// If the symbol is not in the DFG, it returns None
    pub fn get_stack_entry_from_symbol(&self, symbol: String) -> Option<&StackEntry> {
        self.symbolsmap
            .get(&symbol)
            .and_then(|idx| Some(self.get_stack_entry(*idx)))
    }
    /// Return the parental relations in the DFG
    pub fn get_roots(&self) -> &Vec<i32> {
        &self.minidfg.parents
    }
}

#[derive(Debug, Clone)]
pub struct ClassData {
    /// Id of the equivalence class
    pub node_id: usize,
    /// Index to the dfg stack entry
    stack_entry_id: usize,
    /// Type information for the eclass
    pub tpes: Vec<PrimitiveTypeInfo>,
}

impl ClassData {
    /// Returns the stack enntry that corresponds to this equivalence class.
    /// The method will check if this equivalence class has a stack entry in the DFG of the analysis.
    /// If true the StackEntry is then returned
    pub fn get_stack_entry(&self, analysis: &PeepholeMutationAnalysis) -> StackEntry {
        analysis.get_stack_entry(self.stack_entry_id).clone()
    }
}

impl PartialEq for ClassData {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id // && self.operatoridx == other.operatoridx
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl Analysis<Lang> for PeepholeMutationAnalysis {
    type Data = Option<ClassData>;

    fn make(egraph: &EGraph<Lang, Self>, enode: &Lang) -> Self::Data {
        // Add stack entry from here ?, add the encoder here
        None
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        egg::merge_if_different(to, to.clone().or(from))
    }

    fn modify(egraph: &mut EGraph<Lang, Self>, id: Id) {
        // Map node information to corresponding stack entry
        if egraph.analysis.id_to_stack.contains_key(&id) {
            // all nodes in the equivalence graph should have the same stack_entry_id
            if let None = &egraph[id].data {
                egraph[id].data = Some(ClassData {
                    node_id: usize::from(id),
                    stack_entry_id: egraph.analysis.id_to_stack[&id],
                    tpes: egraph
                        .analysis
                        .get_stack_entry(egraph.analysis.id_to_stack[&id])
                        .tpes
                        .clone(),
                })
            }
        }
    }
}
