use std::{cmp::Ordering, collections::HashMap, num::Wrapping};

use egg::{define_language, Analysis, CostFunction, EClass, EGraph, Id, Language, Symbol};
use rand::{
    prelude::{SliceRandom, SmallRng},
    Rng,
};
use wasm_encoder::{Function, Instruction};
use wasmparser::Operator;

pub mod analysis;
pub mod lang;

use crate::{error::EitherType, mutators::peephole::eggsy::lang::Lang, ModuleInfo};

use super::{
    dfg::{BBlock, MiniDFG, StackEntry},
    OperatorAndByteOffset,
};

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

    // Do a pre-order traversal of the e-graph. As we visit each e-class, choose
    // one of its e-nodes at random and then do the same with its children,
    // etc. You can imagine this process as a kind of rolling wave function
    // collapse, where we choose a concrete expression out of the potentially
    // infinite number of equivalent expressions each e-class represents.
    //
    // `worklist` constains the operands we still need to process. These are
    // currently a pair of the parent node and its operand e-class.
    pub fn extract_random(
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
            let node_idx = if depth >= max_depth {
                // look nearest leaf path, in this case, the best in AST size
                self.costs[&node].1
            } else {
                rnd.gen_range(0, egraph[node].nodes.len())
            };

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
    fn eterm2wasm(
        info: &ModuleInfo,
        eterm: &Lang,
        newfunc: &mut Function,
        symbolsmap: &HashMap<String, usize>,
        minidfg: &MiniDFG,
        rnd: &mut SmallRng,
        insertion_point: usize,
        operators: &Vec<OperatorAndByteOffset>,
        id_to_node: &Vec<&Lang>,
        operands: &Vec<Vec<Id>>,
        current: usize,
    ) -> crate::Result<()> {
        log::debug!("Writing operator {:?}", eterm);
        match eterm {
            Lang::I32Mul(_) => {
                newfunc.instruction(Instruction::I32Mul);
            }
            Lang::I32Const(v) => {
                newfunc.instruction(Instruction::I32Const(*v));
            }
            Lang::I32Shl(_) => {
                newfunc.instruction(Instruction::I32Shl);
            }
            Lang::I32Add(_) => {
                newfunc.instruction(Instruction::I32Add);
            }
            Lang::I32Sub(_) => {
                newfunc.instruction(Instruction::I32Sub);
            }
            Lang::I32And(_) => {
                newfunc.instruction(Instruction::I32And);
            }
            Lang::I32Or(_) => {
                newfunc.instruction(Instruction::I32Or);
            }
            Lang::I32Xor(_) => {
                newfunc.instruction(Instruction::I32Xor);
            }
            Lang::I32ShrU(_) => {
                newfunc.instruction(Instruction::I32ShrU);
            }
            Lang::I32Popcnt(_) => {
                newfunc.instruction(Instruction::I32Popcnt);
            }
            Lang::ILoad(_) => {
                // Do nothing
                let operators = &operators[insertion_point..=insertion_point + 1/* take to the next operator to save the offset */];
                let range = (operators[0].1, operators[1].1);

                // Copy the mem operation
                let raw_data = &info.get_code_section().data[range.0..range.1];
                newfunc.raw(raw_data.iter().copied());
            }
            Lang::Rand => {
                newfunc.instruction(Instruction::I32Const(rnd.gen()));
            }
            Lang::Unfold(_) => {
                let child = operands[current][0];
                let node = id_to_node[usize::from(child)];
                match node {
                    Lang::I32Const(value) => {
                        let r: i32 = rnd.gen();
                        log::debug!("Unfolding {:?}", value);
                        newfunc.instruction(Instruction::I32Const(r));
                        newfunc.instruction(Instruction::I32Const((Wrapping(r) - Wrapping(*value)).0));
                        newfunc.instruction(Instruction::I32Add);
                    },
                    _ => panic!("The operand for this operator should be a constant, check if the rewriting rule is defined with such conditions")
                }
            }
            Lang::Symbol(s) => {
                // Copy the byte stream to aavoid mapping
                log::debug!("symbolmap {:?}, entries {:?}", symbolsmap, &minidfg.entries);
                let entryidx = symbolsmap[&s.to_string()];
                let entry = &minidfg.entries[entryidx];

                // Write the symbol in the correct place of the functions

                // Entry could not be an indepent symbol
                match &entry.data {
                    super::dfg::StackEntryData::Leaf => {
                        // Write as it is
                        //todo!("entry {:?}", entry);
                    }
                    super::dfg::StackEntryData::Node { operands } => {
                        todo!("todo {:?}", operands);
                    }
                    super::dfg::StackEntryData::Undef => {
                        // do nothing
                        return Ok(());
                    }
                }

                let bytes = &info.get_code_section().data
                    [entry.byte_stream_range.start..entry.byte_stream_range.end];
                log::debug!("Symbol {:?}, raw bytes: {:?}", s, bytes);
                newfunc.raw(bytes.iter().copied());
            }
            _ => {
                return Err(crate::Error::UnsupportedType(EitherType::Operator(
                    format!("Eterm {:?} is not implemented", eterm),
                )))
            }
        }
        Ok(())
    }

    fn etermtree2wasm(
        info: &ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        insertion_point: usize,
        id_to_node: &Vec<&Lang>,
        operands: &Vec<Vec<Id>>,
        current: usize,
        newfunc: &mut Function,
        symbolmap: &HashMap<String, usize>,
        minidfg: &MiniDFG,
        operators: &Vec<OperatorAndByteOffset>,
    ) -> crate::Result<()> {
        let root = id_to_node[current];

        // This is a patch, this logic should be here
        // If root is Unfol...bypass

        if let Lang::Unfold(_) = root {
            log::debug!("Unfolding a constant yeih !")
        } else {
            // Process operands
            log::debug!("Writing operands for {:?}", root);
            for idx in &operands[current] {
                Encoder::etermtree2wasm(
                    info,
                    rnd,
                    insertion_point,
                    id_to_node,
                    operands,
                    usize::from(*idx),
                    newfunc,
                    symbolmap,
                    minidfg,
                    operators,
                )?;
            }
        }

        Encoder::eterm2wasm(
            info,
            root,
            newfunc,
            symbolmap,
            minidfg,
            rnd,
            insertion_point,
            operators,
            id_to_node,
            operands,
            current,
        )?;
        Ok(())
    }

    fn writestackentry(
        info: &ModuleInfo,
        minidfg: &MiniDFG,
        entry: &StackEntry,
        entryidx: usize,
        newfunc: &mut Function,
    ) -> crate::Result<()> {
        // Write the deps in the dfg
        // Process operands
        match &entry.data {
            super::dfg::StackEntryData::Leaf => {
                // Write the current operator
                let bytes = &info.get_code_section().data
                    [entry.byte_stream_range.start..entry.byte_stream_range.end];
                newfunc.raw(bytes.iter().copied());
                log::debug!("Stack entry leaf bytes {:?}", bytes);
            }
            super::dfg::StackEntryData::Node { operands } => {
                for idx in operands {
                    let entry = &minidfg.entries[*idx];
                    Encoder::writestackentry(info, minidfg, entry, *idx, newfunc)?;
                }
                // Write the operator
                let bytes = &info.get_code_section().data
                    [entry.byte_stream_range.start..entry.byte_stream_range.end];
                newfunc.raw(bytes.iter().copied());
            }
            super::dfg::StackEntryData::Undef => {
                // do nothing, this is the previous state of the stack
            }
        }

        Ok(())
    }

    pub fn build_function(
        info: &ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        current: usize,
        insertion_point: usize,
        id_to_node: &Vec<&Lang>,
        operands: &Vec<Vec<Id>>,
        operators: &Vec<OperatorAndByteOffset>,
        basicblock: &BBlock,
        newfunc: &mut Function,
        symbolmap: &HashMap<String, usize>,
        minidfg: &MiniDFG,
    ) -> crate::Result<()> {
        // Copy previous code
        let range = basicblock.range;
        let byterange = (&operators[0].1, &operators[range.start].1);
        let bytes = &info.get_code_section().data[*byterange.0..*byterange.1];
        newfunc.raw(bytes.iter().copied());

        // Write all entries in the minidfg in reverse order
        // The stack neutral will be preserved but the position of the changed operands not that much :(
        // The edges of the stackentries are always backward in the array, so, it consistent to
        // do the writing in reverse
        let len = minidfg.map.len();
        for (entryidx, parentidx) in minidfg.parents.iter().enumerate() {
            if *parentidx == -1 {
                // It is a root, write then
                let entry = &minidfg.entries[entryidx];
                if entry.operator_idx == insertion_point {
                    log::debug!("Encoding mutation at {:?}", insertion_point);
                    Encoder::etermtree2wasm(
                        info,
                        rnd,
                        insertion_point,
                        id_to_node,
                        operands,
                        current,
                        newfunc,
                        symbolmap,
                        minidfg,
                        operators,
                    )?;
                } else {
                    // Copy the stack entry as it is
                    log::debug!("writing no mutated DFG at {:?}", entry.operator_idx);
                    Encoder::writestackentry(info, minidfg, entry, entryidx, newfunc)?;
                }
            }
        }

        // Copy remaining function
        let range = basicblock.range;
        let byterange = (
            &operators[range.end].1, // In the worst case the next instruction will be and end
            &operators[operators.len() - 1].1,
        );
        let bytes = &info.get_code_section().data[*byterange.0..=*byterange.1];

        newfunc.raw(bytes.iter().copied());
        Ok(())
    }

    /// This function maps the operator to a simple eterm
    /// TODO, others, plus type information
    /// It returns the eterm and if the eterm should considered as a symbol
    fn get_simpleterm(operator: &Operator) -> crate::Result<(String, bool)> {
        match operator {
            Operator::I32Add => Ok(("i32.add".into(), false)),
            Operator::I32Const { value } => Ok((format!("{}", value), false)),
            Operator::LocalGet { local_index } => Ok((format!("?l{}", local_index), true)),
            Operator::I32Shl => Ok(("i32.shl".into(), false)),
            Operator::I32Load { .. } => Ok(("i.load".into(), false)),
            _ => Err(crate::Error::UnsupportedType(EitherType::Operator(
                format!("{:?}", operator),
            ))),
        }
    }
    /// Maps wasm to eterm expression
    /// This method receives also a random generator, the idea is to map StackEntry operands to symbols in a random way.
    /// This will allow to map expressions like `i32.const x; i32.const y; i32.shl` to `(i32.shl ?x y)` or the full expression
    /// `(i32.shl ?x ?y)`
    ///
    /// TODO, check the return since it would be better to return a RecExpr
    pub fn wasm2eterm(
        dfg: &MiniDFG,
        oidx: usize,
        operators: &Vec<OperatorAndByteOffset>,
        rnd: &mut SmallRng,
    ) -> crate::Result<(String, HashMap<String, usize>)> {
        let stack_entry_index = dfg.map[&oidx];
        let entry = &dfg.entries[stack_entry_index];
        let (operator, _) = &operators[entry.operator_idx];
        let assymbol = |stack_entry_idx: usize| -> (String, HashMap<String, usize>) {
            let symbolname = format!("?x{}", stack_entry_idx);
            let mut smap = HashMap::new();
            smap.insert(symbolname.clone(), stack_entry_idx);
            (symbolname, smap)
        };

        // continue or break
        // 0 continue, 1 break
        match &entry.data {
            crate::mutators::peephole::dfg::StackEntryData::Leaf => {
                // Map this to a variable depends on the type of the operator
                match rnd.gen_range(0, 2) {
                    0 => {
                        let (operator, _) = &operators[entry.operator_idx];
                        let (term, addtosymbolsmap) = Encoder::get_simpleterm(&operator)?;
                        Ok((
                            term.clone(),
                            if addtosymbolsmap {
                                vec![(term, stack_entry_index)].into_iter().collect()
                            } else {
                                HashMap::new()
                            },
                        ))
                    }
                    1 => Ok(assymbol(oidx)),
                    _ => unreachable!(),
                }
            }
            crate::mutators::peephole::dfg::StackEntryData::Node { operands } => {
                // This is an operator
                let (operator, _) = &operators[entry.operator_idx];
                let (term, _) = Encoder::get_simpleterm(&operator)?;
                let mut operandterms = Vec::new();
                let mut smap: HashMap<String, usize> = HashMap::new();
                for operandi in operands {
                    let stack_entry = &dfg.entries[*operandi];

                    let (eterm, symbols) = match rnd.gen_range(0, 2) {
                        0 => Encoder::wasm2eterm(dfg, stack_entry.operator_idx, operators, rnd)?,
                        1 => assymbol(stack_entry.operator_idx),
                        _ => unreachable!(),
                    };
                    operandterms.push(eterm);
                    smap.extend(symbols.into_iter())
                }
                Ok((format!("({} {})", term, operandterms.join(" ")), smap))
            }
            crate::mutators::peephole::dfg::StackEntryData::Undef => {
                Ok(("skip".to_string(), HashMap::new()))
            } // This is the previous state of the stack
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        mutators::{
            peephole::{dfg::DFGIcator, OperatorAndByteOffset, PeepholeMutator},
            Mutator,
        },
        WasmMutate,
    };
    use egg::{rewrite, AstSize, Id, Pattern, RecExpr, Rewrite, Runner, Searcher};
    use rand::{prelude::SliceRandom, rngs::SmallRng, Rng, SeedableRng};
    use wasmparser::Parser;

    use super::{Encoder, RandomExtractor};
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
        let cf = AstSize;
        let mut rnd = SmallRng::seed_from_u64(121);

        // ?x is the root
        let root = egraph.add_expr(&start);
        let extractor = RandomExtractor::new(&egraph, cf);

        let (id_to_node, operands) = extractor.extract_random(&mut rnd, root, 10).unwrap();

        let random_outcome = build_expr(root, id_to_node, operands);
    }
}
