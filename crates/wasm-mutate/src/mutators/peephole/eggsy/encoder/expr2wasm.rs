//! Helper to encode [Lang] expressions to Wasm.

use crate::{
    module::PrimitiveTypeInfo,
    mutators::peephole::{eggsy::encoder::TraversalEvent, Lang, EG},
    Error, Result, WasmMutate,
};
use egg::{Id, Language, RecExpr};
use rand::Rng;
use std::num::Wrapping;
use wasm_encoder::{Function, Instruction, MemArg};

/// Some custom nodes might need special resource allocation outside the
/// function. Fore xample, if a new global is needed is should be added outside
/// the construction of the function in the `expr2wasm` method.
#[derive(Clone, Debug)]
pub enum ResourceRequest {
    /// Global resource request
    Global {
        /// Global index
        index: usize,
        /// Global type
        tpe: PrimitiveTypeInfo,
        /// If its mutable
        mutable: bool,
    },
    // TODO add other needed resources here, for example, needed locals, needed
    // memory etc. Notice that how this resources are translated to Wasm code,
    // you need to change the code inside the `mutate_with_rules` of the
    // PeepholeMutator trait
}

/// Encodes an expression in the [Lang][Lang] intermediate language to Wasm
/// * `rnd`  Random generator
/// * `expr` [Lang] expression to encode
/// * `newfunc` Function stream in which the expression will be encoded
pub fn expr2wasm(
    config: &mut WasmMutate,
    expr: &RecExpr<Lang>,
    newfunc: &mut Function,
    _egraph: &EG,
) -> Result<Vec<ResourceRequest>> {
    let nodes = expr.as_ref();
    // The last node is the root.
    let root = Id::from(nodes.len() - 1);

    struct Context {
        // Current visited node index in the tree traversing
        current_node: Id,
        // In which state of the traversing of the current node the process is
        traversal_event: TraversalEvent,
    }

    impl Context {
        fn new(current_node: Id, traversal_event: TraversalEvent) -> Self {
            Context {
                current_node,
                traversal_event,
            }
        }
    }

    let mut worklist = vec![
        Context::new(root, TraversalEvent::Exit),
        Context::new(root, TraversalEvent::Enter),
    ];

    let enqueue = |worklist: &mut Vec<_>, child| {
        worklist.push(Context::new(child, TraversalEvent::Exit));
        worklist.push(Context::new(child, TraversalEvent::Enter));
    };

    // Resources out of the function body that the expr translation needs, for
    // example, the creation of new globals
    let mut resources = vec![];
    // Next global idx
    let mut global_idx = config.info().get_global_count() as u32;

    // Enqueue the coding back nodes and infer types
    while let Some(context) = worklist.pop() {
        let rootlang = &nodes[usize::from(context.current_node)];

        match context.traversal_event {
            TraversalEvent::Enter => match rootlang {
                Lang::UnfoldI32(_) | Lang::UnfoldI64(_) => {}
                _ => {
                    for operand in rootlang.children().iter().rev() {
                        enqueue(&mut worklist, *operand);
                    }
                }
            },
            TraversalEvent::Exit => {
                match rootlang {
                    Lang::LocalGet(idx) => {
                        newfunc.instruction(&Instruction::LocalGet(*idx as u32));
                    }
                    Lang::GlobalGet(idx) => {
                        newfunc.instruction(&Instruction::GlobalGet(*idx as u32));
                    }
                    Lang::LocalSet(idx, _val) => {
                        newfunc.instruction(&Instruction::LocalSet(*idx as u32));
                    }
                    Lang::GlobalSet(idx, _val) => {
                        newfunc.instruction(&Instruction::GlobalSet(*idx as u32));
                    }
                    Lang::LocalTee(idx, _) => {
                        newfunc.instruction(&Instruction::LocalTee(*idx as u32));
                    }
                    Lang::Wrap(_) => {
                        newfunc.instruction(&Instruction::I32WrapI64);
                    }
                    Lang::Call(idx, _) => {
                        newfunc.instruction(&Instruction::Call(*idx as u32));
                    }
                    Lang::Drop(_) => {
                        newfunc.instruction(&Instruction::Drop);
                    }
                    Lang::I32Load {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I32Load(memarg));
                    }
                    Lang::I64Load {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Load(memarg));
                    }
                    Lang::F32Load {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::F32Load(memarg));
                    }
                    Lang::F64Load {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::F64Load(memarg));
                    }
                    Lang::I32Load8S {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I32Load8_S(memarg));
                    }
                    Lang::I32Load8U {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I32Load8_U(memarg));
                    }
                    Lang::I32Load16S {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I32Load16_S(memarg));
                    }
                    Lang::I32Load16U {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I32Load16_U(memarg));
                    }
                    Lang::I64Load8S {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Load8_S(memarg));
                    }
                    Lang::I64Load8U {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Load8_U(memarg));
                    }
                    Lang::I64Load16S {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Load16_S(memarg));
                    }
                    Lang::I64Load16U {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Load16_U(memarg));
                    }
                    Lang::I64Load32S {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Load32_S(memarg));
                    }
                    Lang::I64Load32U {
                        align,
                        mem,
                        static_offset,
                        offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Load32_U(memarg));
                    }
                    Lang::RandI32 => {
                        newfunc.instruction(&Instruction::I32Const(config.rng().gen()));
                    }
                    Lang::RandI64 => {
                        newfunc.instruction(&Instruction::I64Const(config.rng().gen()));
                    }
                    Lang::Undef => { /* Do nothig */ }
                    Lang::UnfoldI32(value) => {
                        let child = &nodes[usize::from(*value)];
                        match child {
                            Lang::I32(value) => {
                                // Getting type from eclass.

                                let r: i32 = config.rng().gen();
                                newfunc.instruction(&Instruction::I32Const(r));
                                newfunc.instruction(&Instruction::I32Const(
                                    (Wrapping(*value as i32) - Wrapping(r)).0,
                                ));
                                newfunc.instruction(&Instruction::I32Add);
                            }
                            _ => {
                                return Err(Error::other(format!(
                                    "The current eterm cannot be unfolded {:?}.\n expr {}",
                                    child, expr
                                )));
                            }
                        }
                    }
                    Lang::UnfoldI64(value) => {
                        let child = &nodes[usize::from(*value)];
                        match child {
                            Lang::I64(value) => {
                                // Getting type from eclass.

                                let r: i64 = config.rng().gen();
                                newfunc.instruction(&Instruction::I64Const(r));
                                newfunc.instruction(&Instruction::I64Const(
                                    (Wrapping(*value) - Wrapping(r)).0,
                                ));
                                newfunc.instruction(&Instruction::I64Add);
                            }
                            _ => {
                                return Err(Error::other(format!(
                                    "The current eterm cannot be unfolded {:?}.\n expr {}",
                                    child, expr
                                )))
                            }
                        }
                    }
                    Lang::I32(v) => {
                        newfunc.instruction(&Instruction::I32Const(*v));
                    }
                    Lang::I64(v) => {
                        newfunc.instruction(&Instruction::I64Const(*v));
                    }
                    Lang::F32(v) => {
                        newfunc.instruction(&Instruction::F32Const(f32::from_bits(*v)));
                    }
                    Lang::F64(v) => {
                        newfunc.instruction(&Instruction::F64Const(f64::from_bits(*v)));
                    }
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
                        newfunc.instruction(&Instruction::I32Ne);
                    }
                    Lang::I64Ne(_) => {
                        newfunc.instruction(&Instruction::I64Ne);
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
                    Lang::I32Clz(_) => {
                        newfunc.instruction(&Instruction::I32Clz);
                    }
                    Lang::I32Ctz(_) => {
                        newfunc.instruction(&Instruction::I32Ctz);
                    }
                    Lang::I64Ctz(_) => {
                        newfunc.instruction(&Instruction::I64Ctz);
                    }
                    Lang::I64Clz(_) => {
                        newfunc.instruction(&Instruction::I64Clz);
                    }
                    Lang::F32Abs(_) => {
                        newfunc.instruction(&Instruction::F32Abs);
                    }
                    Lang::F64Abs(_) => {
                        newfunc.instruction(&Instruction::F64Abs);
                    }
                    Lang::F32Neg(_) => {
                        newfunc.instruction(&Instruction::F32Neg);
                    }
                    Lang::F64Neg(_) => {
                        newfunc.instruction(&Instruction::F64Neg);
                    }
                    Lang::F32Sqrt(_) => {
                        newfunc.instruction(&Instruction::F32Sqrt);
                    }
                    Lang::F64Sqrt(_) => {
                        newfunc.instruction(&Instruction::F64Sqrt);
                    }
                    Lang::F32Ceil(_) => {
                        newfunc.instruction(&Instruction::F32Ceil);
                    }
                    Lang::F64Ceil(_) => {
                        newfunc.instruction(&Instruction::F64Ceil);
                    }
                    Lang::F32Floor(_) => {
                        newfunc.instruction(&Instruction::F32Floor);
                    }
                    Lang::F64Floor(_) => {
                        newfunc.instruction(&Instruction::F64Floor);
                    }
                    Lang::F32Trunc(_) => {
                        newfunc.instruction(&Instruction::F32Trunc);
                    }
                    Lang::F64Trunc(_) => {
                        newfunc.instruction(&Instruction::F64Trunc);
                    }
                    Lang::F32Nearest(_) => {
                        newfunc.instruction(&Instruction::F32Nearest);
                    }
                    Lang::F64Nearest(_) => {
                        newfunc.instruction(&Instruction::F64Nearest);
                    }
                    Lang::I32TruncF32S(_) => {
                        newfunc.instruction(&Instruction::I32TruncF32S);
                    }
                    Lang::I32TruncF32U(_) => {
                        newfunc.instruction(&Instruction::I32TruncF32U);
                    }
                    Lang::I32TruncF64S(_) => {
                        newfunc.instruction(&Instruction::I32TruncF64S);
                    }
                    Lang::I32TruncF64U(_) => {
                        newfunc.instruction(&Instruction::I32TruncF64U);
                    }
                    Lang::I64TruncF32S(_) => {
                        newfunc.instruction(&Instruction::I64TruncF32S);
                    }
                    Lang::I64TruncF32U(_) => {
                        newfunc.instruction(&Instruction::I64TruncF32U);
                    }
                    Lang::I64TruncF64S(_) => {
                        newfunc.instruction(&Instruction::I64TruncF64S);
                    }
                    Lang::I64TruncF64U(_) => {
                        newfunc.instruction(&Instruction::I64TruncF64U);
                    }
                    Lang::F32ConvertI32S(_) => {
                        newfunc.instruction(&Instruction::F32ConvertI32S);
                    }
                    Lang::F32ConvertI32U(_) => {
                        newfunc.instruction(&Instruction::F32ConvertI32U);
                    }
                    Lang::F32ConvertI64S(_) => {
                        newfunc.instruction(&Instruction::F32ConvertI64S);
                    }
                    Lang::F32ConvertI64U(_) => {
                        newfunc.instruction(&Instruction::F32ConvertI64U);
                    }
                    Lang::F32DemoteF64(_) => {
                        newfunc.instruction(&Instruction::F32DemoteF64);
                    }
                    Lang::F64ConvertI32S(_) => {
                        newfunc.instruction(&Instruction::F64ConvertI32S);
                    }
                    Lang::F64ConvertI32U(_) => {
                        newfunc.instruction(&Instruction::F64ConvertI32U);
                    }
                    Lang::F64ConvertI64S(_) => {
                        newfunc.instruction(&Instruction::F64ConvertI64S);
                    }
                    Lang::F64ConvertI64U(_) => {
                        newfunc.instruction(&Instruction::F64ConvertI64U);
                    }
                    Lang::F64PromoteF32(_) => {
                        newfunc.instruction(&Instruction::F64PromoteF32);
                    }
                    Lang::I32ReinterpretF32(_) => {
                        newfunc.instruction(&Instruction::I32ReinterpretF32);
                    }
                    Lang::I64ReinterpretF64(_) => {
                        newfunc.instruction(&Instruction::I64ReinterpretF64);
                    }
                    Lang::F32ReinterpretI32(_) => {
                        newfunc.instruction(&Instruction::F32ReinterpretI32);
                    }
                    Lang::F64ReinterpretI64(_) => {
                        newfunc.instruction(&Instruction::F64ReinterpretI64);
                    }
                    Lang::I32TruncSatF32S(_) => {
                        newfunc.instruction(&Instruction::I32TruncSatF32S);
                    }
                    Lang::I32TruncSatF32U(_) => {
                        newfunc.instruction(&Instruction::I32TruncSatF32U);
                    }
                    Lang::I32TruncSatF64S(_) => {
                        newfunc.instruction(&Instruction::I32TruncSatF64S);
                    }
                    Lang::I32TruncSatF64U(_) => {
                        newfunc.instruction(&Instruction::I32TruncSatF64U);
                    }
                    Lang::I64TruncSatF32S(_) => {
                        newfunc.instruction(&Instruction::I64TruncSatF32S);
                    }
                    Lang::I64TruncSatF32U(_) => {
                        newfunc.instruction(&Instruction::I64TruncSatF32U);
                    }
                    Lang::I64TruncSatF64S(_) => {
                        newfunc.instruction(&Instruction::I64TruncSatF64S);
                    }
                    Lang::I64TruncSatF64U(_) => {
                        newfunc.instruction(&Instruction::I64TruncSatF64U);
                    }
                    Lang::I32Popcnt(_) => {
                        newfunc.instruction(&Instruction::I32Popcnt);
                    }
                    Lang::I64Popcnt(_) => {
                        newfunc.instruction(&Instruction::I64Popcnt);
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
                    Lang::F32Add(_) => {
                        newfunc.instruction(&Instruction::F32Add);
                    }
                    Lang::F64Add(_) => {
                        newfunc.instruction(&Instruction::F64Add);
                    }
                    Lang::F32Sub(_) => {
                        newfunc.instruction(&Instruction::F32Sub);
                    }
                    Lang::F64Sub(_) => {
                        newfunc.instruction(&Instruction::F64Sub);
                    }
                    Lang::F32Mul(_) => {
                        newfunc.instruction(&Instruction::F32Mul);
                    }
                    Lang::F64Mul(_) => {
                        newfunc.instruction(&Instruction::F64Mul);
                    }
                    Lang::F32Div(_) => {
                        newfunc.instruction(&Instruction::F32Div);
                    }
                    Lang::F64Div(_) => {
                        newfunc.instruction(&Instruction::F64Div);
                    }
                    Lang::F32Min(_) => {
                        newfunc.instruction(&Instruction::F32Min);
                    }
                    Lang::F64Min(_) => {
                        newfunc.instruction(&Instruction::F64Min);
                    }
                    Lang::F32Max(_) => {
                        newfunc.instruction(&Instruction::F32Max);
                    }
                    Lang::F64Max(_) => {
                        newfunc.instruction(&Instruction::F64Max);
                    }
                    Lang::F32Copysign(_) => {
                        newfunc.instruction(&Instruction::F32Copysign);
                    }
                    Lang::F64Copysign(_) => {
                        newfunc.instruction(&Instruction::F64Copysign);
                    }
                    Lang::F32Eq(_) => {
                        newfunc.instruction(&Instruction::F32Eq);
                    }
                    Lang::F64Eq(_) => {
                        newfunc.instruction(&Instruction::F64Eq);
                    }
                    Lang::F32Ne(_) => {
                        newfunc.instruction(&Instruction::F32Ne);
                    }
                    Lang::F64Ne(_) => {
                        newfunc.instruction(&Instruction::F64Ne);
                    }
                    Lang::F32Lt(_) => {
                        newfunc.instruction(&Instruction::F32Lt);
                    }
                    Lang::F64Lt(_) => {
                        newfunc.instruction(&Instruction::F64Lt);
                    }
                    Lang::F32Gt(_) => {
                        newfunc.instruction(&Instruction::F32Gt);
                    }
                    Lang::F64Gt(_) => {
                        newfunc.instruction(&Instruction::F64Gt);
                    }
                    Lang::F32Le(_) => {
                        newfunc.instruction(&Instruction::F32Le);
                    }
                    Lang::F64Le(_) => {
                        newfunc.instruction(&Instruction::F64Le);
                    }
                    Lang::F32Ge(_) => {
                        newfunc.instruction(&Instruction::F32Ge);
                    }
                    Lang::F64Ge(_) => {
                        newfunc.instruction(&Instruction::F64Ge);
                    }
                    Lang::I32Store {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I32Store(memarg));
                    }
                    Lang::I64Store {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Store(memarg));
                    }
                    Lang::F32Store {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::F32Store(memarg));
                    }
                    Lang::F64Store {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::F64Store(memarg));
                    }
                    Lang::I32Store8 {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I32Store8(memarg));
                    }
                    Lang::I32Store16 {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I32Store16(memarg));
                    }
                    Lang::I64Store8 {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Store8(memarg));
                    }
                    Lang::I64Store16 {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Store16(memarg));
                    }
                    Lang::I64Store32 {
                        align,
                        mem,
                        static_offset,
                        value_and_offset: _,
                    } => {
                        let memarg = MemArg {
                            offset: *static_offset,
                            align: *align as u32,
                            memory_index: *mem,
                        };

                        newfunc.instruction(&Instruction::I64Store32(memarg));
                    }
                    Lang::Nop => {
                        newfunc.instruction(&Instruction::Nop);
                    }
                    Lang::Container(_) => {
                        // Do nothing
                    }
                    Lang::Select(_) => {
                        newfunc.instruction(&Instruction::Select);
                    }
                    Lang::MemoryGrow { mem, .. } => {
                        newfunc.instruction(&Instruction::MemoryGrow(*mem));
                    }
                    Lang::MemorySize { mem, .. } => {
                        newfunc.instruction(&Instruction::MemorySize(*mem));
                    }
                    Lang::I32UseGlobal(_) => {
                        // Request a new global
                        let request = ResourceRequest::Global {
                            index: global_idx as usize,
                            tpe: PrimitiveTypeInfo::I32,
                            mutable: true,
                        };
                        resources.push(request);

                        newfunc.instruction(&Instruction::GlobalSet(global_idx));
                        newfunc.instruction(&Instruction::GlobalGet(global_idx));
                        global_idx += 1;
                    }
                    Lang::I64UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            index: global_idx as usize,
                            tpe: PrimitiveTypeInfo::I64,
                            mutable: true,
                        };
                        resources.push(request);

                        newfunc.instruction(&Instruction::GlobalSet(global_idx));
                        newfunc.instruction(&Instruction::GlobalGet(global_idx));
                        global_idx += 1;
                    }
                    Lang::F32UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            index: global_idx as usize,
                            tpe: PrimitiveTypeInfo::F32,
                            mutable: true,
                        };
                        resources.push(request);

                        newfunc.instruction(&Instruction::GlobalSet(global_idx));
                        newfunc.instruction(&Instruction::GlobalGet(global_idx));
                        global_idx += 1;
                    }
                    Lang::F64UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            index: global_idx as usize,
                            tpe: PrimitiveTypeInfo::F64,
                            mutable: true,
                        };
                        resources.push(request);

                        newfunc.instruction(&Instruction::GlobalSet(global_idx));
                        newfunc.instruction(&Instruction::GlobalGet(global_idx));
                        global_idx += 1;
                    }
                }
            }
        }
    }
    Ok(resources)
}
