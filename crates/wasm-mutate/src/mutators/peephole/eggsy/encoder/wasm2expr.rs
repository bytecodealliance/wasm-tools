use crate::{
    mutators::peephole::{
        dfg::{MiniDFG, StackType},
        eggsy::encoder::TraversalEvent,
        Lang, OperatorAndByteOffset,
    },
};
use egg::{Id, RecExpr};
use std::collections::HashMap;
use wasmparser::Operator;

/// Maps wasm to eterm expression
/// This method receives also a random generator, the idea is to map StackEntry
/// operands to symbols in a random way.
/// This method returns the enode(Lang) mapping the eclass and the stack
/// entries where this enode is the same
pub fn wasm2expr(
    dfg: &MiniDFG,
    oidx: usize,
    operators: &[OperatorAndByteOffset],
    // The wasm expressions will be added here
    expr: &mut RecExpr<Lang>, // Replace this by RecExpr
) -> crate::Result<HashMap<Lang, (Id, Vec<usize>)>> {
    let stack_entry_index = dfg.map[&oidx];
    // If the enode hashing is already in the egraph, the node is not added,
    // this affects our mapping, therefore, we simulate this behavior by hashing
    // the enode ourselves and creating the Id by the size of the hashset
    fn put_enode(
        l: Lang,
        hashset: &mut HashMap<Lang, (Id, Vec<usize>)>,
        entryindex: usize,
        expr: &mut RecExpr<Lang>, // Replace this by RecExpr
    ) -> Id {
        if hashset.contains_key(&l) {
            let id = hashset[&l].0;
            hashset.get_mut(&l).unwrap().1.push(entryindex);
            id
        } else {
            let newid = Id::from(hashset.len());
            expr.add(l.clone());
            hashset.insert(l.clone(), (newid, vec![entryindex]));

            newid
        }
    }

    let mut worklist = vec![
        (stack_entry_index, TraversalEvent::Exit),
        (stack_entry_index, TraversalEvent::Enter),
    ];

    let mut r = HashMap::new();
    let mut ids_stack = Vec::new();
    while let Some((entryidx, event)) = worklist.pop() {
        let entry = &dfg.entries[entryidx];
        let op = &entry.operator;
        match event {
            TraversalEvent::Enter => {
                // Push the children first
                let mut operands = entry.operands.clone();
                operands.reverse();

                // Special case, the first child of the Call node is a helper Arg
                if let StackType::Call {
                    function_index,
                    params_count: _,
                } = op
                {
                    ids_stack.push(put_enode(
                        Lang::Arg(*function_index as u64),
                        &mut r,
                        entry.entry_idx,
                        expr,
                    ));
                }

                for operand in &operands {
                    worklist.push((*operand, TraversalEvent::Exit));
                    worklist.push((*operand, TraversalEvent::Enter))
                }
            }
            TraversalEvent::Exit => {
                match op {
                    StackType::I32(value) => {
                        // Create a new ID and add it to the ids stack
                        let n = Lang::I32(*value);
                        // Place the new id in the id_stack to recover it in the Exit event
                        ids_stack.push(put_enode(n, &mut r, entry.entry_idx, expr));
                    }
                    StackType::I64(value) => {
                        let n = Lang::I64(*value);
                        ids_stack.push(put_enode(n, &mut r, entry.entry_idx, expr));
                    }
                    StackType::LocalGet(idx) => {
                        let name = format!("?l{}", idx);
                        ids_stack.push(put_enode(
                            Lang::Symbol(name.clone().into()),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        ));
                    }
                    StackType::LocalSet(idx) => {
                        let local_id = put_enode(
                            Lang::Symbol(format!("?l{}", idx).into()),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        );
                        ids_stack.push(put_enode(
                            Lang::Set([local_id]),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        ));
                    }
                    StackType::LocalTee(idx) => {
                        let local_id = put_enode(
                            Lang::Symbol(format!("?l{}", idx).into()),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        );
                        let value = ids_stack.pop().expect("Ids stack should not be empty");
                        ids_stack.push(put_enode(
                            Lang::Tee([local_id, value]),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        ));
                    }
                    StackType::GlobalGet(idx) => {
                        let name = format!("?g{}", idx);
                        ids_stack.push(put_enode(
                            Lang::Symbol(name.clone().into()),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        ));
                    }
                    StackType::GlobalSet(global_idx) => {
                        let local_id = put_enode(
                            Lang::Symbol(format!("?g{}", global_idx).into()),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        );
                        ids_stack.push(put_enode(
                            Lang::GlobalSet([local_id]),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        ));
                    }
                    StackType::Drop => {
                        let id = ids_stack.pop().expect("Missing expected drop argument");

                        ids_stack.push(put_enode(Lang::Drop([id]), &mut r, entry.entry_idx, expr));
                    }
                    StackType::Call {
                        function_index: _,
                        params_count,
                    } => {
                        // params_count + helper node
                        let mut children = (0..*params_count + 1)
                            .map(|_| ids_stack.pop().expect("Stack should not be empty"))
                            .collect::<Vec<Id>>();
                        children.reverse();

                        ids_stack.push(put_enode(
                            Lang::Call(children),
                            &mut r,
                            entry.entry_idx,
                            expr,
                        ));
                    }
                    StackType::Undef => {
                        ids_stack.push(put_enode(Lang::Undef, &mut r, entry.entry_idx, expr));
                    }
                    StackType::IndexAtCode(operatoridx, childcount) => {
                        let subexpressions = (0..*childcount)
                            .map(|_| ids_stack.pop().expect("Invalid stack state"))
                            .collect::<Vec<Id>>();
                        let (operator, _) = &operators[*operatoridx];

                        let nodeid = match operator {
                            Operator::I32Add => put_enode(
                                // Notice, children are declared in reverse order
                                Lang::I32Add([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Add => put_enode(
                                // Notice, children are declared in reverse order
                                Lang::I64Add([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Shl => put_enode(
                                Lang::I32Shl([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Shl => put_enode(
                                Lang::I64Shl([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32ShrU => put_enode(
                                Lang::I32ShrU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64ShrU => put_enode(
                                Lang::I64ShrU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32ShrS => put_enode(
                                Lang::I32ShrS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64ShrS => put_enode(
                                Lang::I64ShrS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32DivS => put_enode(
                                Lang::I32DivS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64DivS => put_enode(
                                Lang::I64DivS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32DivU => put_enode(
                                Lang::I32DivU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64DivU => put_enode(
                                Lang::I64DivU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32And => put_enode(
                                Lang::I32And([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64And => put_enode(
                                Lang::I64And([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Or => put_enode(
                                Lang::I32Or([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Or => put_enode(
                                Lang::I64Or([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Xor => put_enode(
                                Lang::I32Xor([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Xor => put_enode(
                                Lang::I64Xor([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Sub => put_enode(
                                Lang::I32Sub([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Sub => put_enode(
                                Lang::I64Sub([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Mul => put_enode(
                                Lang::I32Mul([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Mul => put_enode(
                                Lang::I64Mul([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Eqz => put_enode(
                                Lang::I32Eqz([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Eqz => put_enode(
                                Lang::I64Eqz([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Eq => put_enode(
                                Lang::I32Eq([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Eq => put_enode(
                                Lang::I64Eq([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Ne => put_enode(
                                Lang::I32Ne([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Ne => put_enode(
                                Lang::I64Ne([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32LtS => put_enode(
                                Lang::I32LtS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64LtS => put_enode(
                                Lang::I64LtS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32LtU => put_enode(
                                Lang::I32LtU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64LtU => put_enode(
                                Lang::I64LtU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32GtS => put_enode(
                                Lang::I32GtS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64GtS => put_enode(
                                Lang::I64GtS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32GtU => put_enode(
                                Lang::I32GtU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64GtU => put_enode(
                                Lang::I64GtU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32LeS => put_enode(
                                Lang::I32LeS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64LeS => put_enode(
                                Lang::I64LeS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32LeU => put_enode(
                                Lang::I32LeU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64LeU => put_enode(
                                Lang::I64LeU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32GeU => put_enode(
                                Lang::I32GeU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64GeU => put_enode(
                                Lang::I64GeU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32GeS => put_enode(
                                Lang::I32GeS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64GeS => put_enode(
                                Lang::I64GeS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Rotr => put_enode(
                                Lang::I32RotR([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Rotr => put_enode(
                                Lang::I64RotR([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Rotl => put_enode(
                                Lang::I32RotL([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Rotl => put_enode(
                                Lang::I64RotL([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32RemS => put_enode(
                                Lang::I32RemS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64RemS => put_enode(
                                Lang::I64RemS([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32RemU => put_enode(
                                Lang::I32RemU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64RemU => put_enode(
                                Lang::I64RemU([subexpressions[1], subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32WrapI64 => put_enode(
                                Lang::Wrap([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Extend8S => put_enode(
                                Lang::I32Extend8S([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Extend8S => put_enode(
                                Lang::I64Extend8S([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Extend16S => put_enode(
                                Lang::I32Extend16S([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Extend16S => put_enode(
                                Lang::I64Extend16S([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Extend32S => put_enode(
                                Lang::I64Extend32S([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64ExtendI32S => put_enode(
                                Lang::I64ExtendI32S([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64ExtendI32U => put_enode(
                                Lang::I64ExtendI32U([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Popcnt => put_enode(
                                Lang::I32Popcnt([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I64Popcnt => put_enode(
                                Lang::I64Popcnt([subexpressions[0]]),
                                &mut r,
                                entry.entry_idx,
                                expr,
                            ),
                            Operator::I32Load { memarg } => {
                                let static_offset = Lang::Arg(memarg.offset);
                                let static_offset =
                                    put_enode(static_offset, &mut r, entry.entry_idx, expr);

                                let align = Lang::Arg(memarg.align as u64);
                                let align = put_enode(align, &mut r, entry.entry_idx, expr);

                                let memory = Lang::Arg(memarg.memory as u64);
                                let memory = put_enode(memory, &mut r, entry.entry_idx, expr);

                                put_enode(
                                    Lang::I32Load([
                                        subexpressions[0],
                                        static_offset,
                                        align,
                                        memory,
                                    ]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                )
                            }
                            Operator::I64Load { memarg } => {
                                let static_offset = Lang::Arg(memarg.offset);
                                let static_offset =
                                    put_enode(static_offset, &mut r, entry.entry_idx, expr);

                                let align = Lang::Arg(memarg.align as u64);
                                let align = put_enode(align, &mut r, entry.entry_idx, expr);

                                let memory = Lang::Arg(memarg.memory as u64);
                                let memory = put_enode(memory, &mut r, entry.entry_idx, expr);

                                put_enode(
                                    Lang::I64Load([
                                        subexpressions[0],
                                        static_offset,
                                        align,
                                        memory,
                                    ]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                )
                            }
                            _ => return Err(crate::Error::NoMutationsApplicable),
                        };

                        ids_stack.push(nodeid);
                    }
                }
            }
        }
    }
    Ok(r)
}
