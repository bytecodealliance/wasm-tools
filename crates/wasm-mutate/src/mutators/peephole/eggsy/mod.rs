use std::{cmp::Ordering, collections::HashMap, num::Wrapping};

use egg::{define_language, Analysis, CostFunction, EClass, EGraph, Id, Language, RecExpr, Symbol};
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

    /// The the cost of the egraph nodes
    pub fn get_costs(&self) -> &HashMap<Id, (<CF as CostFunction<L>>::Cost, usize)> {
        &self.costs
    }

    /// Do a pre-order traversal of the e-graph. As we visit each e-class, choose
    /// one of its e-nodes at random and then do the same with its children,
    /// etc. You can imagine this process as a kind of rolling wave function
    /// collapse, where we choose a concrete expression out of the potentially
    /// infinite number of equivalent expressions each e-class represents.
    ///
    /// `worklist` constains the operands we still need to process. These are
    /// currently a pair of the parent node and its operand e-class.
    pub fn extract_random(
        &self,
        rnd: &mut rand::prelude::SmallRng,
        eclass: Id,
        max_depth: u32,
        encoder: impl Fn(Id, &Vec<&L>, &Vec<Vec<Id>>) -> RecExpr<L>,
    ) -> crate::Result<RecExpr<L>> // return the random tree, TODO, improve the way the tree is returned
    {
        // A map from a node's id to its actual node data.
        let mut id_to_node = vec![];
        // A map from a parent node id to its child operand node ids.
        let mut operands = vec![];

        // Select a random node in this e-class
        let rootidx = rnd.gen_range(0, self.egraph[eclass].nodes.len());
        let rootnode = &self.egraph[eclass].nodes[rootidx];

        id_to_node.push(&self.egraph[eclass].nodes[rootidx]);
        operands.push(vec![]);

        let mut worklist: Vec<_> = rootnode
            .children()
            .iter()
            .rev()
            .map(|id| (eclass, 0, id, 0)) // (root, operant, depth)
            .collect();

        while let Some((parent, parentidx, &node, depth)) = worklist.pop() {
            let node_idx = if depth >= max_depth {
                // look nearest leaf path, in this case, the best in AST size
                self.costs[&node].1
            } else {
                rnd.gen_range(0, self.egraph[node].nodes.len())
            };

            let operand = Id::from(id_to_node.len());
            let operandidx = id_to_node.len();
            let last_node_id = parentidx; // id_to_node.len() - 1;

            id_to_node.push(&self.egraph[node].nodes[node_idx]);
            operands.push(vec![]);

            operands[last_node_id].push(operand);

            //let operand = &egraph[node].nodes[node_idx];

            worklist.extend(
                self.egraph[node].nodes[node_idx]
                    .children()
                    .iter()
                    .rev()
                    .map(|id| (operand, operandidx, id, depth + 1)),
            );
        }
        // Build the tree with the right language constructor
        Ok(encoder(
            Id::from(0), /* The root of the expr is the node at position 0 */
            &id_to_node,
            &operands,
        ))
    }
}

/// Turns wasm to eterm and back
pub struct Encoder;

impl Encoder {
    fn expr2wasm(
        info: &ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        insertion_point: usize,
        expr: &RecExpr<Lang>,
        newfunc: &mut Function,
        symbolmap: &HashMap<String, usize>,
        minidfg: &MiniDFG,
        operators: &Vec<OperatorAndByteOffset>,
    ) -> crate::Result<()> {
        // This is a patch, this logic should be here
        // If root is Unfold...bypass
        let nodes = expr.as_ref();
        fn expr2wasm_aux(
            info: &ModuleInfo,
            rnd: &mut rand::prelude::SmallRng,
            insertion_point: usize,
            nodes: &[Lang],
            current: usize,
            newfunc: &mut Function,
            symbolsmap: &HashMap<String, usize>,
            minidfg: &MiniDFG,
            operators: &Vec<OperatorAndByteOffset>,
        ) -> crate::Result<()> {
            let root = &nodes[current];
            match root {
                Lang::I32Add(operands)
                | Lang::I32Sub(operands)
                | Lang::I32Mul(operands)
                | Lang::I32Shl(operands)
                | Lang::I32And(operands)
                | Lang::I32Xor(operands)
                | Lang::I32Or(operands)
                | Lang::I32ShrU(operands) => {
                    // Process left operand
                    expr2wasm_aux(
                        info,
                        rnd,
                        insertion_point,
                        nodes,
                        usize::from(operands[0]),
                        newfunc,
                        symbolsmap,
                        minidfg,
                        operators,
                    )?;
                    // Process right operand
                    expr2wasm_aux(
                        info,
                        rnd,
                        insertion_point,
                        nodes,
                        usize::from(operands[1]),
                        newfunc,
                        symbolsmap,
                        minidfg,
                        operators,
                    )?;
                    // Write the current operator
                    match root {
                        Lang::I32Add(_) => {
                            newfunc.instruction(Instruction::I32Add);
                        }
                        Lang::I32Sub(_) => {
                            newfunc.instruction(Instruction::I32Sub);
                        }
                        Lang::I32Mul(_) => {
                            newfunc.instruction(Instruction::I32Mul);
                        }
                        Lang::I32Shl(_) => {
                            newfunc.instruction(Instruction::I32Shl);
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
                        _ => unreachable!(),
                    }
                }
                Lang::I32Popcnt(operand) => {
                    expr2wasm_aux(
                        info,
                        rnd,
                        insertion_point,
                        nodes,
                        usize::from(*operand),
                        newfunc,
                        symbolsmap,
                        minidfg,
                        operators,
                    )?;
                    newfunc.instruction(Instruction::I32Popcnt);
                }
                Lang::ILoad(operand) => {
                    // Write memory load operations
                    expr2wasm_aux(
                        info,
                        rnd,
                        insertion_point,
                        nodes,
                        usize::from(*operand),
                        newfunc,
                        symbolsmap,
                        minidfg,
                        operators,
                    )?;

                    let operators = &operators[insertion_point..=insertion_point + 1/* take to the next operator to save the offset */];
                    let range = (operators[0].1, operators[1].1);

                    // Copy the mem operation
                    let raw_data = &info.get_code_section().data[range.0..range.1];
                    newfunc.raw(raw_data.iter().copied());
                }
                Lang::Rand => {
                    newfunc.instruction(Instruction::I32Const(rnd.gen()));
                }
                Lang::Undef => {
                    log::debug!("Undefined value reached, this means that the operand will come from the evaluation of pred basic blocks");
                }
                Lang::Unfold(operand) => {
                    let child = &nodes[usize::from(operand[0])];
                    match child {
                        Lang::I32Const(value) => {
                            let r: i32 = rnd.gen();
                            log::debug!("Unfolding {:?}", value);
                            newfunc.instruction(Instruction::I32Const(r));
                            newfunc.instruction(Instruction::I32Const((Wrapping(r) - Wrapping(*value)).0));
                            newfunc.instruction(Instruction::I32Add);
                        },
                        _ => unreachable!("The operand for this operator should be a constant, check if the rewriting rule is defined with such conditions")
                    }
                }
                Lang::I32Const(val) => {
                    newfunc.instruction(Instruction::I32Const(*val));
                }
                Lang::Symbol(s) => {
                    // Copy the byte stream to aavoid mapping
                    log::debug!("symbolmap {:?}, entries {:?}", symbolsmap, &minidfg.entries);
                    let entryidx = symbolsmap[&s.to_string()];
                    let entry = &minidfg.entries[entryidx];
                    // Entry could not be an indepent symbol
                    match &entry.data {
                        super::dfg::StackEntryData::Leaf => {
                            let bytes = &info.input_wasm[entry.byte_stream_range.start..entry.byte_stream_range.end];
                            log::debug!("Symbol {:?}, raw bytes: {:?}", s, bytes);
                            newfunc.raw(bytes.iter().copied());
                        }
                        _ => {
                            return Err(crate::Error::UnsupportedType(EitherType::TypeDef(format!("A leaf stack entry that cannot be mapped directly to a Symbol is not right"))))
                        }
                    }
                }
                Lang::Drop => {
                    newfunc.instruction(Instruction::Drop);
                }
            }

            Ok(())
        }
        expr2wasm_aux(
            info,
            rnd,
            insertion_point,
            nodes,
            nodes.len() - 1,
            newfunc,
            symbolmap,
            minidfg,
            operators,
        )
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

    /// Reassembles the mutated function and return a `Function` entry
    pub fn build_function(
        info: &ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        insertion_point: usize,
        expr: &RecExpr<Lang>,
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
        for (entryidx, parentidx) in minidfg.parents.iter().enumerate() {
            if *parentidx == -1 {
                // It is a root, write then
                let entry = &minidfg.entries[entryidx];
                if entry.operator_idx == insertion_point {
                    Encoder::expr2wasm(
                        info,
                        rnd,
                        insertion_point,
                        expr,
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

    /// Maps wasm to eterm expression
    /// This method receives also a random generator, the idea is to map StackEntry operands to symbols in a random way.
    pub fn wasm2expr(
        dfg: &MiniDFG,
        oidx: usize,
        operators: &Vec<OperatorAndByteOffset>,
        // The wasm expressions will be added here
        expr: &mut RecExpr<Lang>, // Replace this by RecExpr
    ) -> crate::Result<(Id, HashMap<String, usize>)> {
        let stack_entry_index = dfg.map[&oidx];
        let entry = &dfg.entries[stack_entry_index];

        match &entry.data {
            crate::mutators::peephole::dfg::StackEntryData::Leaf => {
                // map to a symbol or constant
                let (operator, _) = &operators[entry.operator_idx];
                match operator {
                    Operator::I32Const { value } => {
                        let id = expr.add(Lang::I32Const(*value));
                        Ok((
                            id,
                            HashMap::new(), // No symbol
                        ))
                    }
                    Operator::LocalGet { local_index } => {
                        let name = format!("?l{}", local_index);
                        let id = expr.add(Lang::Symbol(name.clone().into()));
                        let mut smap = HashMap::new();
                        smap.insert(name, stack_entry_index);

                        Ok((id, smap))
                    }
                    _ => Err(crate::Error::UnsupportedType(EitherType::Operator(
                        format!("Operator {:?} is not supported as symbol", operator),
                    ))),
                }
            }
            crate::mutators::peephole::dfg::StackEntryData::Node { operands } => {
                // This is an operator
                let (operator, _) = &operators[entry.operator_idx];
                let mut subexpressions = Vec::new();
                let mut smap: HashMap<String, usize> = HashMap::new();

                for operandi in operands {
                    let stack_entry = &dfg.entries[*operandi];
                    let (eterm, symbols) =
                        Encoder::wasm2expr(dfg, stack_entry.operator_idx, operators, expr)?;
                    subexpressions.push(eterm);
                    smap.extend(symbols.into_iter());
                }
                let operatorid = match operator {
                    Operator::I32Shl => {
                        // Check this node has only two child
                        assert_eq!(subexpressions.len(), 2);

                        Ok(expr.add(Lang::I32Shl([subexpressions[0], subexpressions[1]])))
                    }
                    Operator::I32Add => {
                        // Check this node has only two child
                        assert_eq!(subexpressions.len(), 2);

                        Ok(expr.add(Lang::I32Add([subexpressions[0], subexpressions[1]])))
                    }
                    Operator::I32Load { .. } => {
                        assert_eq!(subexpressions.len(), 1);
                        Ok(expr.add(Lang::ILoad(subexpressions[0])))
                    }
                    Operator::Drop => Ok(expr.add(Lang::Drop)),
                    _ => Err(crate::Error::UnsupportedType(EitherType::Operator(
                        format!("The operator {:?} cannot be mapped to egg lang", operator),
                    ))),
                }?;

                Ok((operatorid, smap))
            }
            crate::mutators::peephole::dfg::StackEntryData::Undef => {
                let undefid = expr.add(Lang::Undef);
                Ok((undefid, HashMap::new()))
            } // This is the previous state of the stack
        }
    }
    /// Build RecExpr from tree information
    pub fn build_expr(root: Id, id_to_node: &Vec<&Lang>, operands: &Vec<Vec<Id>>) -> RecExpr<Lang> {
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
                        Lang::ILoad(_) => expr.add(Lang::ILoad(operand(0))),
                        c @ Lang::I32Const(_) => expr.add((*c).clone()),
                        s @ Lang::Symbol(_) => expr.add((*s).clone()),
                        s @ Lang::Rand => expr.add((*s).clone()),
                        u @ Lang::Undef => expr.add((*u).clone()),
                        d @ Lang::Drop => expr.add((*d).clone()),
                    };
                    let old_entry = node_to_id.insert(node, sub_expr_id);
                    assert!(old_entry.is_none());
                }
            }
        }

        expr
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        module::PrimitiveTypeInfo,
        mutators::{
            peephole::{dfg::DFGIcator, OperatorAndByteOffset, PeepholeMutator},
            Mutator,
        },
        WasmMutate,
    };
    use egg::{rewrite, AstSize, Id, Pattern, RecExpr, Rewrite, Runner, Searcher};
    use rand::{prelude::SliceRandom, rngs::SmallRng, Rng, SeedableRng};
    use wasm_encoder::Function;
    use wasmparser::Parser;

    use super::{Encoder, RandomExtractor};
    use crate::mutators::peephole::Lang;
    use crate::mutators::peephole::PeepholeMutationAnalysis;

    #[test]
    fn test_random_generation() {
        let rules: &[Rewrite<Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("unfold-2";  "?x" => "(unfold ?x)"), // Use a custom instruction-mutator for this
                                                          // rewrite!("strength-undo";  "(i32.shl ?x 1)" => "(i32.mul ?x ?x)"),
        ];

        let start = "?x".parse().unwrap();
        let runner = Runner::default().with_expr(&start).run(rules);
        let mut egraph = runner.egraph;
        let cf = AstSize;
        let mut rnd = SmallRng::seed_from_u64(4);

        // ?x is the root
        let root = egraph.add_expr(&start);
        let extractor = RandomExtractor::new(&egraph, cf);
        let encoder = Encoder::build_expr;

        let expr = extractor
            .extract_random(&mut rnd, root, 10, encoder)
            .unwrap();
    }

    #[test]
    fn test_wasm2expr() {
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                i32.const 32
                i32.const 32
                i32.add
                i32.add
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
                        .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()
                        .unwrap();

                    let bb = DFGIcator::new()
                        .get_bb_from_operator(4, &operators)
                        .unwrap();

                    let roots = DFGIcator::new()
                        .get_dfg(&operators, &bb, &vec![PrimitiveTypeInfo::I32])
                        .unwrap();

                    let mut exprroot = RecExpr::<Lang>::default();
                    let (_, _) = Encoder::wasm2expr(&roots, 4, &operators, &mut exprroot).unwrap();
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

    #[test]
    fn test_expr2wasm() {
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                i32.const 32
                i32.const 32
                i32.add
                i32.add
            )
        )
        "#,
        )
        .unwrap();
        let wasmmutate = WasmMutate::default();
        let mut info = wasmmutate.get_module_info(original).unwrap();

        let mut parser = Parser::new(0);
        let mut rnd = SmallRng::seed_from_u64(0);
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
                        .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()
                        .unwrap();

                    let bb = DFGIcator::new()
                        .get_bb_from_operator(4, &operators)
                        .unwrap();

                    let roots = DFGIcator::new()
                        .get_dfg(&operators, &bb, &vec![PrimitiveTypeInfo::I32])
                        .unwrap();

                    let mut exprroot = RecExpr::<Lang>::default();
                    let (root, symbolsmap) =
                        Encoder::wasm2expr(&roots, 4, &operators, &mut exprroot).unwrap();

                    let mut newfunc = Function::new(vec![]);

                    Encoder::expr2wasm(
                        &info,
                        &mut rnd,
                        4,
                        &exprroot,
                        &mut newfunc,
                        &symbolsmap,
                        &roots,
                        &operators,
                    )
                    .unwrap();
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
