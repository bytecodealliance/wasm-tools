//! Helper methods for encoding eterm expressions to Wasm and back

use std::{collections::HashMap, num::Wrapping};

use egg::{Id, RecExpr};
use rand::Rng;
use wasm_encoder::{Function, Instruction};
use wasmparser::Operator;

use crate::mutators::peephole::Lang;
use crate::{
    error::EitherType,
    mutators::peephole::{
        dfg::{BBlock, MiniDFG, StackEntry, StackEntryData},
        OperatorAndByteOffset,
    },
    ModuleInfo,
};
/// Turns wasm to eterm and back
pub struct Encoder;

impl Encoder {
    pub(crate) fn expr2wasm(
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
                        StackEntryData::Leaf => {
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
            StackEntryData::Leaf => {
                // Write the current operator
                let bytes = &info.get_code_section().data
                    [entry.byte_stream_range.start..entry.byte_stream_range.end];
                newfunc.raw(bytes.iter().copied());
                log::debug!("Stack entry leaf bytes {:?}", bytes);
            }
            StackEntryData::Node { operands } => {
                for idx in operands {
                    let entry = &minidfg.entries[*idx];
                    Encoder::writestackentry(info, minidfg, entry, *idx, newfunc)?;
                }
                // Write the operator
                let bytes = &info.get_code_section().data
                    [entry.byte_stream_range.start..entry.byte_stream_range.end];
                newfunc.raw(bytes.iter().copied());
            }
            StackEntryData::Undef => {
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
