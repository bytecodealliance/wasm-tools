use std::{cmp::Ordering, collections::HashMap};

use egg::{define_language, Analysis, CostFunction, EClass, EGraph, Id, Language, Symbol};
use rand::Rng;

pub mod analysis;
pub mod lang;

use crate::mutators::peephole::eggsy::lang::Lang;

use super::{cfg::MiniDFG, TupleType};

pub struct NoPopcnt;

impl CostFunction<Lang> for NoPopcnt {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &Lang, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            // Doubts on this...
            Lang::I32Popcnt(_) => usize::MAX,
            _ => 0,
        };
        enode.fold(op_cost, |sum, id| sum.saturating_add(costs(id)))
    }
}

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
        (Some(a), Some(b)) => a.partial_cmp(&b).unwrap(),
    }
}

impl<'a, CF, L, N> RandomExtractor<'a, CF, L, N>
where
    CF: CostFunction<L>,
    L: Language,
    N: Analysis<L, Data = Option<i32>>, // The analysis should return the index of the node in the e-class
{
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
        eclass: &EClass<L, Option<i32>>,
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
            Some(self.cost_function.cost(&node, cost_f))
        } else {
            None
        }
    }

    pub fn get_costs(&self) -> &HashMap<Id, (<CF as CostFunction<L>>::Cost, usize)> {
        &self.costs
    }

    /// Here is where the random magic is done
    pub fn generate_random_tree(
        &self,
        rnd: &mut rand::prelude::SmallRng,
        eclass: Id,
        max_depth: u32,
    ) -> crate::Result<(Vec<&L>, Vec<Vec<Id>>)> // return the random tree, TODO, improve the way the tree is returned
    {
        // A map from a node's id to its actual node data.
        let mut id_to_node = vec![];
        // A map from a parent node id to its child operand node ids.
        let mut operands = vec![];

        let egraph = self.egraph;
        // Select a random node in this e-class
        let rootidx = rnd.gen_range(0, egraph[eclass].nodes.len());
        let rootnode = &egraph[eclass].nodes[rootidx];

        id_to_node.push(&egraph[eclass].nodes[rootidx]);
        operands.push(vec![]);

        let mut worklist: Vec<_> = rootnode
            .children()
            .iter()
            .map(|id| (eclass, 0, id, 0)) // (root, operant, depth)
            .collect();

        while let Some((parent, parentidx, &node, depth)) = worklist.pop() {
            //println!("{:?}", parent);

            let node_idx = if depth >= max_depth {
                // look nearest leaf path, in this case, the best in AST size
                self.costs[&node].1
            } else {
                rnd.gen_range(0, egraph[node].nodes.len())
            };

            //println!("random e-node {:?}", node_idx);
            //println!("options {:?}", egraph[node].nodes);

            let operand = Id::from(id_to_node.len());
            let operandidx = id_to_node.len();
            let last_node_id = parentidx; // id_to_node.len() - 1;
            id_to_node.push(&egraph[node].nodes[node_idx]);
            operands.push(vec![]);

            operands[last_node_id].push(operand);

            //let operand = &egraph[node].nodes[node_idx];

            worklist.extend(
                egraph[node].nodes[node_idx]
                    .children()
                    .iter()
                    .map(|id| (operand, operandidx, id, depth + 1)),
            );
        }
        Ok((id_to_node, operands))
    }
}

/// Turns wasm to eterm and back
pub struct Encoder;

impl Encoder {
    /// Maps wasm to eterm expression
    /// TODO, check the return since it would be better to return a RecExpr
    pub fn wasm2eterm(dfg: &MiniDFG, oidx: usize, operators: &Vec<TupleType>) -> Option<String> {
        let entry = &dfg.entries[dfg.map[&oidx]];

        //todo!();

        match *entry.data {
            crate::mutators::peephole::cfg::StackEntryData::Leaf => {
                // Map this to a variable depends on the type of the operator
                Some("?x".to_string())
            }
            crate::mutators::peephole::cfg::StackEntryData::Node(_) => {
                todo!()
            }
            crate::mutators::peephole::cfg::StackEntryData::Unknown => Some("skip".to_string()), // This is the previous state of the stack
        }
    }

    pub fn eterm2wasm() {
        todo!();
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        mutators::{
            peephole::{cfg::DFGIcator, PeepholeMutator, TupleType},
            Mutator,
        },
        WasmMutate,
    };
    use egg::{rewrite, Id, Pattern, RecExpr, Rewrite, Runner, Searcher};
    use rand::{prelude::SliceRandom, rngs::SmallRng, Rng, SeedableRng};
    use wasmparser::Parser;

    use super::{Encoder, NoPopcnt, RandomExtractor};
    use crate::mutators::peephole::Lang;
    use crate::mutators::peephole::PeepholeMutationAnalysis;

    /// For debugging mostly
    pub fn build_expr(root: Id, id_to_node: Vec<&Lang>, operands: Vec<Vec<Id>>) -> RecExpr<Lang> {
        let mut expr = RecExpr::default();

        // A map from the `Id`s we assigned to each sub-expression when extracting a
        // random expression to the `Id`s assigned to each sub-expression by the
        // `RecExpr`.
        let mut node_to_id: HashMap<Id, Id> = Default::default();

        enum Event {
            Enter,
            Exit,
        }

        let mut to_visit = vec![(Event::Exit, root), (Event::Enter, root)];

        while let Some((event, node)) = to_visit.pop() {
            match event {
                Event::Enter => {
                    let start_children = to_visit.len();

                    for child in operands[usize::from(node)].iter().copied() {
                        to_visit.push((Event::Enter, child));
                        to_visit.push((Event::Exit, child));
                    }

                    // Reverse to make it so that we visit children in order
                    // (e.g. operands are visited in order).
                    to_visit[start_children..].reverse();
                }
                Event::Exit => {
                    let operands = &operands[usize::from(node)];
                    let operand = |i| node_to_id[&operands[i]];
                    let sub_expr_id = match &id_to_node[usize::from(node)] {
                        Lang::I32Add(_) => expr.add(Lang::I32Add([operand(0), operand(1)])),
                        Lang::I32Sub(_) => expr.add(Lang::I32Sub([operand(0), operand(1)])),
                        Lang::I32Mul(_) => expr.add(Lang::I32Mul([operand(0), operand(1)])),
                        Lang::I32And(_) => expr.add(Lang::I32And([operand(0), operand(1)])),
                        Lang::I32Or(_) => expr.add(Lang::I32Or([operand(0), operand(1)])),
                        Lang::I32Xor(_) => expr.add(Lang::I32Xor([operand(0), operand(1)])),
                        Lang::I32Shl(_) => expr.add(Lang::I32Shl([operand(0), operand(1)])),
                        Lang::I32ShrU(_) => expr.add(Lang::I32ShrU([operand(0), operand(1)])),
                        Lang::I32Popcnt(_) => expr.add(Lang::I32Popcnt(operand(0))),
                        Lang::Unfold(_) => expr.add(Lang::Unfold([operand(0)])),
                        c @ Lang::I32Const(_) => expr.add((*c).clone()),
                        s @ Lang::Symbol(_) => expr.add((*s).clone()),
                        s @ Lang::Rand => expr.add((*s).clone()),
                        p @ Lang::Prev => expr.add((*p).clone()),
                        Lang::ILoad(_) => expr.add(Lang::ILoad(operand(0))),
                    };
                    let old_entry = node_to_id.insert(node, sub_expr_id);
                    assert!(old_entry.is_none());
                }
            }
        }

        expr
    }

    #[test]
    fn test_random_generation() {
        let rules: &[Rewrite<Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("unfold-1";  "?x" => "(i32.add rand (i32.sub rand ?x))"),
            rewrite!("unfold-2";  "?x" => "(unfold ?x)"), // Use a custom instruction-mutator for this
            rewrite!("strength-undo";  "(i32.shl ?x 1)" => "(i32.mul ?x ?x)"),
        ];

        let start = "?x".parse().unwrap();
        let runner = Runner::default().with_expr(&start).run(rules);
        let mut egraph = runner.egraph;
        let cf = NoPopcnt;
        let mut rnd = SmallRng::seed_from_u64(121);

        // ?x is the root
        let root = egraph.add_expr(&start);
        let extractor = RandomExtractor::new(&egraph, cf);

        let (id_to_node, operands) = extractor.generate_random_tree(&mut rnd, root, 10).unwrap();

        println!("{:?} {:?}", id_to_node, operands);
        let random_outcome = build_expr(root, id_to_node, operands);

        println!("{}", random_outcome.pretty(35));
    }

    #[test]
    pub fn test_wasm2eterm() {
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                i32.const 42
                drop
                local.get 0
                local.get 0
                i32.const 109
                i32.add
                i32.add
                i32.load
                if 
                    i32.const 54
                else
                    i32.const 87
                end
                i32.const 56
                i32.add
                loop
                    i32.const 1
                    local.get 0
                    i32.add
                    local.set 0
                end
            )
        )
        "#,
        )
        .unwrap();

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        loop {
            let (payload, size) = match parser.parse(&original[consumed..], true).unwrap() {
                wasmparser::Chunk::NeedMoreData(_) => {
                    panic!("This should not happen")
                }
                wasmparser::Chunk::Parsed { consumed, payload } => (payload, consumed),
            };

            consumed += size;

            match payload {
                wasmparser::Payload::CodeSectionEntry(reader) => {
                    let operators = reader
                        .get_operators_reader()
                        .unwrap()
                        .into_iter_with_offsets()
                        .collect::<wasmparser::Result<Vec<TupleType>>>()
                        .unwrap();

                    let dfg = DFGIcator::new().get_fulldfg(&operators).unwrap();

                    let eterm = Encoder::wasm2eterm(&dfg, 6, &operators);

                    println!("{:?}", eterm);
                    todo!();
                }
                wasmparser::Payload::End => {
                    break;
                }
                _ => {
                    // Do nothing
                }
            }
        }
    }
}
