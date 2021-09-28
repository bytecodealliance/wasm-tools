use crate::mutators::peephole::eggsy::Lang;
use egg::{Analysis, CostFunction, EGraph, Id, Language};

// Analysis implementation for our defined language
#[derive(Clone, Copy, Debug, Default)]
pub struct PeepholeMutationAnalysis;

impl Analysis<Lang> for PeepholeMutationAnalysis {
    type Data = Option<i32>;

    fn make(egraph: &EGraph<Lang, Self>, enode: &Lang) -> Self::Data {
        None
    }

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        egg::merge_if_different(to, to.or(from))
    }

    fn modify(egraph: &mut EGraph<Lang, Self>, id: Id) {
        if let Some(x) = egraph[id].data {
            let added = egraph.add(Lang::I32Const(x));
            egraph.union(id, added);
        }
    }
}
