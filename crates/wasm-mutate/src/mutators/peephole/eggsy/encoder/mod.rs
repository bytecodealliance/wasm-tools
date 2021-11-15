//! Helper methods for encoding eterm expressions to Wasm and back

use crate::mutators::peephole::dfg::MiniDFG;
use crate::mutators::peephole::eggsy::encoder::expr2wasm::expr2wasm;
use crate::mutators::peephole::{Lang, EG};
use crate::{
    mutators::peephole::{
        dfg::{BBlock, StackEntry},
        OperatorAndByteOffset,
    },
    ModuleInfo,
};
use egg::RecExpr;

use wasm_encoder::{Function, Instruction, MemArg};

pub mod expr2wasm;
pub mod rebuild;

/// Turns wasm to eterm and back
pub struct Encoder;

/// Traversing node events
enum TraversalEvent {
    Enter,
    Exit,
}

impl Encoder {
    fn writestackentry(
        dfg: &MiniDFG,
        entry: &StackEntry,
        newfunc: &mut Function,
    ) -> crate::Result<()> {
        let mut worklist = vec![
            (entry, TraversalEvent::Exit),
            (entry, TraversalEvent::Enter),
        ];

        while let Some((entry, event)) = worklist.pop() {
            match event {
                TraversalEvent::Enter => {
                    // push operands
                    for idx in entry.operands.iter().rev() {
                        let entry = &dfg.entries[*idx];
                        worklist.push((entry, TraversalEvent::Exit));
                        worklist.push((entry, TraversalEvent::Enter));
                    }
                }
                TraversalEvent::Exit => {
                    match &entry.operator {
                        Lang::I32Add(_) => {
                            newfunc.instruction(&Instruction::I32Add);
                        }
                        Lang::I64Add(_) => {
                            newfunc.instruction(&Instruction::I64Add);
                        }
                        Lang::I32Sub(_) => {
                            newfunc.instruction(&Instruction::I32Sub);
                        }
                        Lang::I64Sub(_) => {
                            newfunc.instruction(&Instruction::I64Sub);
                        }
                        Lang::I32Mul(_) => {
                            newfunc.instruction(&Instruction::I32Mul);
                        }
                        Lang::I64Mul(_) => {
                            newfunc.instruction(&Instruction::I64Mul);
                        }
                        Lang::I32And(_) => {
                            newfunc.instruction(&Instruction::I32And);
                        }
                        Lang::I64And(_) => {
                            newfunc.instruction(&Instruction::I64And);
                        }
                        Lang::I32Or(_) => {
                            newfunc.instruction(&Instruction::I32Or);
                        }
                        Lang::I64Or(_) => {
                            newfunc.instruction(&Instruction::I64Or);
                        }
                        Lang::I32Xor(_) => {
                            newfunc.instruction(&Instruction::I32Xor);
                        }
                        Lang::I64Xor(_) => {
                            newfunc.instruction(&Instruction::I64Xor);
                        }
                        Lang::I32Shl(_) => {
                            newfunc.instruction(&Instruction::I32Shl);
                        }
                        Lang::I64Shl(_) => {
                            newfunc.instruction(&Instruction::I64Shl);
                        }
                        Lang::I32ShrU(_) => {
                            newfunc.instruction(&Instruction::I32ShrU);
                        }
                        Lang::I64ShrU(_) => {
                            newfunc.instruction(&Instruction::I64ShrU);
                        }
                        Lang::I32DivU(_) => {
                            newfunc.instruction(&Instruction::I32DivU);
                        }
                        Lang::I64DivU(_) => {
                            newfunc.instruction(&Instruction::I64DivU);
                        }
                        Lang::I32DivS(_) => {
                            newfunc.instruction(&Instruction::I32DivS);
                        }
                        Lang::I64DivS(_) => {
                            newfunc.instruction(&Instruction::I64DivS);
                        }
                        Lang::I32ShrS(_) => {
                            newfunc.instruction(&Instruction::I32ShrS);
                        }
                        Lang::I64ShrS(_) => {
                            newfunc.instruction(&Instruction::I64ShrS);
                        }
                        Lang::I32RotR(_) => {
                            newfunc.instruction(&Instruction::I32Rotr);
                        }
                        Lang::I64RotR(_) => {
                            newfunc.instruction(&Instruction::I64Rotr);
                        }
                        Lang::I32RotL(_) => {
                            newfunc.instruction(&Instruction::I32Rotl);
                        }
                        Lang::I64RotL(_) => {
                            newfunc.instruction(&Instruction::I64Rotl);
                        }
                        Lang::I32RemS(_) => {
                            newfunc.instruction(&Instruction::I32RemS);
                        }
                        Lang::I64RemS(_) => {
                            newfunc.instruction(&Instruction::I64RemS);
                        }
                        Lang::I32RemU(_) => {
                            newfunc.instruction(&Instruction::I32RemU);
                        }
                        Lang::I64RemU(_) => {
                            newfunc.instruction(&Instruction::I64RemU);
                        }
                        Lang::I32Eqz(_) => {
                            newfunc.instruction(&Instruction::I32Eqz);
                        }
                        Lang::I64Eqz(_) => {
                            newfunc.instruction(&Instruction::I64Eqz);
                        }
                        Lang::I32Eq(_) => {
                            newfunc.instruction(&Instruction::I32Eq);
                        }
                        Lang::I64Eq(_) => {
                            newfunc.instruction(&Instruction::I64Eq);
                        }
                        Lang::I32Ne(_) => {
                            newfunc.instruction(&Instruction::I32Neq);
                        }
                        Lang::I64Ne(_) => {
                            newfunc.instruction(&Instruction::I64Neq);
                        }
                        Lang::I32LtS(_) => {
                            newfunc.instruction(&Instruction::I32LtS);
                        }
                        Lang::I64LtS(_) => {
                            newfunc.instruction(&Instruction::I64LtS);
                        }
                        Lang::I32LtU(_) => {
                            newfunc.instruction(&Instruction::I32LtU);
                        }
                        Lang::I64LtU(_) => {
                            newfunc.instruction(&Instruction::I64LtU);
                        }
                        Lang::I32GtS(_) => {
                            newfunc.instruction(&Instruction::I32GtS);
                        }
                        Lang::I64GtS(_) => {
                            newfunc.instruction(&Instruction::I64GtS);
                        }
                        Lang::I32GtU(_) => {
                            newfunc.instruction(&Instruction::I32GtU);
                        }
                        Lang::I64GtU(_) => {
                            newfunc.instruction(&Instruction::I64GtU);
                        }
                        Lang::I32LeS(_) => {
                            newfunc.instruction(&Instruction::I32LeS);
                        }
                        Lang::I64LeS(_) => {
                            newfunc.instruction(&Instruction::I64LeS);
                        }
                        Lang::I32LeU(_) => {
                            newfunc.instruction(&Instruction::I32LeU);
                        }
                        Lang::I64LeU(_) => {
                            newfunc.instruction(&Instruction::I64LeU);
                        }
                        Lang::I32GeS(_) => {
                            newfunc.instruction(&Instruction::I32GeS);
                        }
                        Lang::I64GeS(_) => {
                            newfunc.instruction(&Instruction::I64GeS);
                        }
                        Lang::I32GeU(_) => {
                            newfunc.instruction(&Instruction::I32GeU);
                        }
                        Lang::I64GeU(_) => {
                            newfunc.instruction(&Instruction::I64GeU);
                        }
                        Lang::I32Popcnt(_) => {
                            newfunc.instruction(&Instruction::I32Popcnt);
                        }
                        Lang::I64Popcnt(_) => {
                            newfunc.instruction(&Instruction::I64Popcnt);
                        }
                        Lang::LocalTee(operands) => {
                            let argid = operands[0];
                            let arg = &dfg.nodes[usize::from(argid)];

                            match arg {
                                Lang::Arg(v) => {
                                    newfunc.instruction(&Instruction::LocalTee(*v as u32));
                                }
                                _ => unreachable!(
                                    "Invalid node type, local index should be an Arg node"
                                ),
                            }
                        }
                        Lang::LocalSet(operands) => {
                            let argid = operands[0];
                            let arg = &dfg.nodes[usize::from(argid)];

                            match arg {
                                Lang::Arg(v) => {
                                    newfunc.instruction(&Instruction::LocalSet(*v as u32));
                                }
                                _ => unreachable!(
                                    "Invalid node type, local index should be an Arg node"
                                ),
                            }
                        }
                        Lang::LocalGet(operands) => {
                            let argid = operands[0];
                            let arg = &dfg.nodes[usize::from(argid)];

                            match arg {
                                Lang::Arg(v) => {
                                    newfunc.instruction(&Instruction::LocalGet(*v as u32));
                                }
                                _ => unreachable!(
                                    "Invalid node type, local index should be an Arg node"
                                ),
                            }
                        }
                        Lang::GlobalSet(operands) => {
                            let argid = operands[0];
                            let arg = &dfg.nodes[usize::from(argid)];

                            match arg {
                                Lang::Arg(v) => {
                                    newfunc.instruction(&Instruction::GlobalSet(*v as u32));
                                }
                                _ => unreachable!(
                                    "Invalid node type, local index should be an Arg node"
                                ),
                            }
                        }
                        Lang::GlobalGet(operands) => {
                            let argid = operands[0];
                            let arg = &dfg.nodes[usize::from(argid)];

                            match arg {
                                Lang::Arg(v) => {
                                    newfunc.instruction(&Instruction::GlobalGet(*v as u32));
                                }
                                _ => unreachable!(
                                    "Invalid node type, globals index should be an Arg node"
                                ),
                            }
                        }
                        Lang::Wrap(_) => {
                            newfunc.instruction(&Instruction::I32WrapI64);
                        }
                        Lang::I32Extend8S(_) => {
                            newfunc.instruction(&Instruction::I32Extend8S);
                        }
                        Lang::I64Extend8S(_) => {
                            newfunc.instruction(&Instruction::I64Extend8S);
                        }
                        Lang::I32Extend16S(_) => {
                            newfunc.instruction(&Instruction::I32Extend16S);
                        }
                        Lang::I64Extend16S(_) => {
                            newfunc.instruction(&Instruction::I64Extend16S);
                        }
                        Lang::I64Extend32S(_) => {
                            newfunc.instruction(&Instruction::I64Extend32S);
                        }
                        Lang::I64ExtendI32S(_) => {
                            newfunc.instruction(&Instruction::I64ExtendI32S);
                        }
                        Lang::I64ExtendI32U(_) => {
                            newfunc.instruction(&Instruction::I64ExtendI32U);
                        }
                        Lang::Call(operands) => {
                            // newfunc.instruction(&Instruction::Call);
                            let argid = &operands[0];
                            let arg = &dfg.nodes[usize::from(*argid)];

                            match arg {
                                Lang::Arg(v) => {
                                    newfunc.instruction(&Instruction::Call(*v as u32));
                                }
                                _ => unreachable!(
                                    "Invalid node type, function index should be an Arg node"
                                ),
                            }
                        }
                        Lang::Drop(_) => {
                            newfunc.instruction(&Instruction::Drop);
                        }
                        Lang::I32Load(operands) => {
                            let offset = &operands[1];
                            let offset = &dfg.nodes[usize::from(*offset)];

                            let offset = match offset {
                                Lang::Arg(v) => *v,
                                Lang::Const(v) => *v as u64,
                                _ => unreachable!(
                                    "Invalid node type, load static offset cannot be inferred {:?}",
                                    offset
                                ),
                            };

                            let align = &operands[2];
                            let align = &dfg.nodes[usize::from(*align)];

                            let align = match align {
                                Lang::Arg(v) => *v as u32,
                                _ => unreachable!(
                                    "Invalid node type, load static offset cannot be inferred {:?}",
                                    align
                                ),
                            };

                            let memory_index = &operands[3];
                            let memory_index = &dfg.nodes[usize::from(*memory_index)];

                            let memory_index = match memory_index {
                                Lang::Arg(v) => *v as u32,
                                _ => unreachable!(
                                    "Invalid node type, load static offset cannot be inferred {:?}",
                                    memory_index
                                ),
                            };
                            newfunc.instruction(&Instruction::I32Load(MemArg {
                                offset,
                                align,
                                memory_index,
                            }));
                        }
                        Lang::I64Load(operands) => {
                            let offset = &operands[1];
                            let offset = &dfg.nodes[usize::from(*offset)];

                            let offset = match offset {
                                Lang::Arg(v) => *v,
                                Lang::Const(v) => *v as u64,
                                _ => unreachable!(
                                    "Invalid node type, load static offset cannot be inferred {:?}",
                                    offset
                                ),
                            };

                            let align = &operands[2];
                            let align = &dfg.nodes[usize::from(*align)];

                            let align = match align {
                                Lang::Arg(v) => *v as u32,
                                _ => unreachable!(
                                    "Invalid node type, load static offset cannot be inferred {:?}",
                                    align
                                ),
                            };

                            let memory_index = &operands[3];
                            let memory_index = &dfg.nodes[usize::from(*memory_index)];

                            let memory_index = match memory_index {
                                Lang::Arg(v) => *v as u32,
                                _ => unreachable!(
                                    "Invalid node type, load static offset cannot be inferred {:?}",
                                    memory_index
                                ),
                            };
                            newfunc.instruction(&Instruction::I64Load(MemArg {
                                offset,
                                align,
                                memory_index,
                            }));
                        }
                        Lang::I32(v) => {
                            newfunc.instruction(&Instruction::I32Const(*v));
                        }
                        Lang::I64(v) => {
                            newfunc.instruction(&Instruction::I64Const(*v));
                        }
                        Lang::RandI32
                        |  Lang::RandI64
                        | Lang::Undef
                        | Lang::UnfoldI32(_)
                        | Lang::UnfoldI64(_)
                        | Lang::Const(_)
                        | Lang::Arg(_) => unreachable!(
                            "Custom and helper nodes cannot be directly encoded to Wasm"
                        ),
                    }
                }
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
        operators: &[OperatorAndByteOffset],
        basicblock: &BBlock, // move to the analysis
        newfunc: &mut Function,
        dfg: &MiniDFG,
        egraph: &EG,
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

        for (entryidx, parentidx) in dfg.parents.iter().enumerate() {
            if *parentidx == -1 {
                // It is a root, write then
                let entry = &dfg.entries[entryidx];
                if entry.operator_idx == insertion_point {
                    expr2wasm(info, rnd, expr, newfunc, egraph)?;
                } else {
                    // Copy the stack entry as it is
                    Encoder::writestackentry(dfg, entry, newfunc)?;
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
}
