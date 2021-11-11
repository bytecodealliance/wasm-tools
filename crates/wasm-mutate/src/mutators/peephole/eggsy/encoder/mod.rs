//! Helper methods for encoding eterm expressions to Wasm and back

use crate::mutators::peephole::dfg::StackType;
use crate::mutators::peephole::eggsy::encoder::expr2wasm::expr2wasm;
use crate::mutators::peephole::{Lang, EG};
use crate::{
    mutators::peephole::{
        dfg::{BBlock, StackEntry},
        OperatorAndByteOffset,
    },
    ModuleInfo,
};
use egg::{Id, RecExpr};

use wasm_encoder::{Function, Instruction};

pub mod expr2wasm;
pub mod rebuild;
pub mod wasm2expr;

/// Turns wasm to eterm and back
pub struct Encoder;

/// Traversing node events
enum TraversalEvent {
    Enter,
    Exit,
}

impl Encoder {
    fn writestackentry(
        info: &ModuleInfo,
        egraph: &EG,
        entry: &StackEntry,
        operators: &[OperatorAndByteOffset],
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
                        let entry = &egraph.analysis.get_stack_entry(*idx);
                        worklist.push((entry, TraversalEvent::Exit));
                        worklist.push((entry, TraversalEvent::Enter));
                    }
                }
                TraversalEvent::Exit => {
                    match entry.operator {
                        StackType::I32(val) => {
                            newfunc.instruction(&Instruction::I32Const(val));
                        }
                        StackType::I64(val) => {
                            newfunc.instruction(&Instruction::I64Const(val));
                        }
                        StackType::LocalGet(idx) => {
                            newfunc.instruction(&Instruction::LocalGet(idx));
                        }
                        StackType::LocalSet(idx) => {
                            newfunc.instruction(&Instruction::LocalSet(idx));
                        }
                        /* StackType::Load {
                            offset,
                            align,
                            memory,
                        } => {
                            // Here it depends on the type
                            match entry.return_type {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32Load(MemArg {
                                        offset,
                                        align: align as u32,
                                        memory_index: memory,
                                    }));
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64Load(MemArg {
                                        offset,
                                        align: align as u32,
                                        memory_index: memory,
                                    }));
                                }
                                _ => unreachable!("Type {:?} is not supported", entry.return_type),
                            }
                        } */
                        StackType::Undef => {
                            // Do nothing
                        }
                        StackType::IndexAtCode(operatoridx, _) => {
                            // Copy as it is
                            let range = (operatoridx, operatoridx + 1);
                            let range = &operators[range.0..=range.1];
                            let range = [range[0].1, range[1].1];
                            let raw_data = &info.get_code_section().data[range[0]..range[1]];
                            newfunc.raw(raw_data.iter().copied());
                        }
                        StackType::Call {
                            function_index,
                            params_count: _,
                        } => {
                            newfunc.instruction(&Instruction::Call(function_index));
                        }
                        StackType::Drop => {
                            newfunc.instruction(&Instruction::Drop);
                        }
                        StackType::LocalTee(idx) => {
                            newfunc.instruction(&Instruction::LocalTee(idx));
                        }
                        StackType::GlobalGet(idx) => {
                            newfunc.instruction(&Instruction::GlobalGet(idx));
                        }
                        StackType::GlobalSet(idx) => {
                            newfunc.instruction(&Instruction::GlobalSet(idx));
                        }
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
        node_to_eclass: &[Id],
        operators: &[OperatorAndByteOffset],
        basicblock: &BBlock, // move to the analysis
        newfunc: &mut Function,
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
        for (entryidx, parentidx) in egraph.analysis.get_roots().iter().enumerate() {
            if *parentidx == -1 {
                // It is a root, write then
                let entry = &egraph.analysis.get_stack_entry(entryidx);
                if entry.operator_idx == insertion_point {
                    expr2wasm(info, rnd, expr, node_to_eclass, newfunc, egraph)?;
                } else {
                    // Copy the stack entry as it is
                    Encoder::writestackentry(info, egraph, entry, operators, newfunc)?;
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
