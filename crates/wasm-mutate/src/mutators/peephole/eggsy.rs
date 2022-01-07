//! Intermediate representation of Wasm operators to be used with the `egg`
//! engine
//!

pub mod analysis;
pub mod encoder;
pub mod expr_enumerator;
pub mod lang;

use self::{analysis::ClassData, lang::Lang};
use egg::{Analysis, CostFunction, EClass, EGraph, Id, Language, RecExpr};
use std::{cell::RefCell, cmp::Ordering, collections::HashMap};

/// This struct is a wrapper of egg::Extractor
/// The majority of the methods are copied and adapted to our needs
pub struct RandomExtractor<'a, CF: CostFunction<L>, L: Language, N: Analysis<L>> {
    cost_function: CF,
    costs: HashMap<Id, (CF::Cost, usize)>,
    egraph: &'a EGraph<L, N>,
}

fn cmp<T: PartialOrd>(a: &Option<T>, b: &Option<T>) -> Ordering {
    // None is high
    match (a, b) {
        (None, None) => Ordering::Equal,
        (None, Some(_)) => Ordering::Greater,
        (Some(_), None) => Ordering::Less,
        (Some(a), Some(b)) => a.partial_cmp(b).unwrap(),
    }
}

impl<'a, CF, L, N> RandomExtractor<'a, CF, L, N>
where
    CF: CostFunction<L>,
    L: Language,
    N: Analysis<L, Data = Option<ClassData>>, // The analysis should return the index of the node in the e-class
{
    /// Returns a new Random extractor from an egraph and a custom cost function
    pub fn new(egraph: &'a EGraph<L, N>, cost_function: CF) -> Self {
        let costs = HashMap::default();

        let mut extractor = RandomExtractor {
            costs,
            egraph,
            cost_function,
        };
        extractor.costs = extractor.find_costs();
        extractor
    }

    fn find_costs(&mut self) -> HashMap<Id, (CF::Cost, usize)> {
        let mut costs = HashMap::new();

        let mut did_something = true;
        while did_something {
            did_something = false;

            for class in self.egraph.classes() {
                let pass = self.make_pass(&mut costs, class);
                match (costs.get(&class.id), pass) {
                    (None, Some(new)) => {
                        costs.insert(class.id, new);
                        did_something = true;
                    }
                    (Some(old), Some(new)) if new.0 < old.0 => {
                        costs.insert(class.id, new);
                        did_something = true;
                    }
                    _ => (),
                }
            }
        }

        costs
    }

    fn make_pass(
        &mut self,
        costs: &mut HashMap<Id, (CF::Cost, usize)>,
        eclass: &EClass<L, Option<ClassData>>,
    ) -> Option<(CF::Cost, usize)> {
        let (cost, node_idx) = eclass
            .iter()
            .enumerate()
            .map(|(i, n)| (self.node_total_cost(n, costs), i))
            .min_by(|a, b| cmp(&a.0, &b.0))
            .unwrap_or_else(|| panic!("Can't extract, eclass is empty: {:#?}", eclass));
        cost.map(|c| (c, node_idx))
    }

    fn node_total_cost(
        &mut self,
        node: &L,
        costs: &mut HashMap<Id, (CF::Cost, usize)>,
    ) -> Option<CF::Cost> {
        let egraph = self.egraph;
        let has_cost = |&id| costs.contains_key(&egraph.find(id));
        if node.children().iter().all(has_cost) {
            let costs = &costs;
            let cost_f = |id| costs[&egraph.find(id)].0.clone();
            Some(self.cost_function.cost(node, cost_f))
        } else {
            None
        }
    }

    /// Do a pre-order traversal of the e-graph. As we visit each e-class, choose
    /// choose the smallest e-node (according to the cost function).
    pub fn extract_smallest(
        &self,
        eclass: Id,
        recexpr: &RefCell<RecExpr<L>>,
        expression_builder: impl for<'b> Fn(Id, &mut RecExpr<L>, &dyn Fn(Id) -> (&'b L, &'b [Id])) -> Id,
    ) -> crate::Result<Id> // return the random tree, TODO, improve the way the tree is returned
    {
        // A map from a node's id to its actual node data.
        let mut id_to_node = vec![];
        // A map from a parent node id to its child operand node ids.
        let mut operands = vec![];

        // Select a random node in this e-class
        let rootidx = self.costs[&eclass].1;
        let rootnode = &self.egraph[eclass].nodes[rootidx];
        // The operator index is the same in all eclass nodes
        id_to_node.push(self.egraph[eclass].nodes[rootidx].clone());
        operands.push(vec![]);

        let mut worklist: Vec<_> = rootnode
            .children()
            .iter()
            .rev()
            .map(|id| (0, id, 0)) // (root, operant, depth)
            .collect();

        // The RecExpr can be built directly here following the following rules
        // The childrens of a node are before in the array

        while let Some((parentidx, &node, depth)) = worklist.pop() {
            let node_idx = self.costs[&node].1;
            let operand = Id::from(id_to_node.len());
            let operandidx = id_to_node.len();
            let last_node_id = parentidx; // id_to_node.len() - 1;

            id_to_node.push(self.egraph[node].nodes[node_idx].clone());
            operands.push(vec![]);

            operands[last_node_id].push(operand);

            worklist.extend(
                self.egraph[node].nodes[node_idx]
                    .children()
                    .iter()
                    .rev()
                    .map(|id| (operandidx, id, depth + 1)),
            );
        }
        // Build the tree with the right language constructor
        let mut to_write_in = recexpr.borrow_mut();
        let expr = expression_builder(Id::from(0), &mut to_write_in, &|id| {
            (&id_to_node[usize::from(id)], &operands[usize::from(id)])
        });
        Ok(expr)
    }
}
