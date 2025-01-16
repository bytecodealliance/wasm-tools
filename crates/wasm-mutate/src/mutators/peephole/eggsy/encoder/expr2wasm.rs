//! Helper to encode [Lang] expressions to Wasm.

use crate::{
    module::PrimitiveTypeInfo,
    mutators::peephole::{eggsy::encoder::TraversalEvent, Lang, MemArg, EG},
    Error, Result, WasmMutate,
};
use egg::{Id, Language, RecExpr};
use rand::Rng;
use std::num::Wrapping;
use wasm_encoder::{Function, Instruction};

/// Some custom nodes might need special resource allocation outside the
/// function. Fore xample, if a new global is needed is should be added outside
/// the construction of the function in the `expr2wasm` method.
#[derive(Clone, Debug)]
pub enum ResourceRequest {
    /// Global resource request
    Global {
        /// Global type
        tpe: PrimitiveTypeInfo,
        /// If its mutable
        mutable: bool,
        /// If its shared
        shared: bool,
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
                let mut insn = |i: Instruction| {
                    newfunc.instruction(&i);
                };
                match rootlang {
                    Lang::LocalGet(idx) => insn(Instruction::LocalGet(*idx as u32)),
                    Lang::GlobalGet(idx) => insn(Instruction::GlobalGet(*idx as u32)),
                    Lang::LocalSet(idx, _val) => insn(Instruction::LocalSet(*idx as u32)),
                    Lang::GlobalSet(idx, _val) => insn(Instruction::GlobalSet(*idx as u32)),
                    Lang::LocalTee(idx, _) => insn(Instruction::LocalTee(*idx as u32)),
                    Lang::Wrap(_) => insn(Instruction::I32WrapI64),
                    Lang::Call(idx, _) => insn(Instruction::Call(*idx as u32)),
                    Lang::Drop(_) => insn(Instruction::Drop),
                    Lang::I32Load(memarg, _) => insn(Instruction::I32Load(memarg.into())),
                    Lang::I64Load(memarg, _) => insn(Instruction::I64Load(memarg.into())),
                    Lang::F32Load(memarg, _) => insn(Instruction::F32Load(memarg.into())),
                    Lang::F64Load(memarg, _) => insn(Instruction::F64Load(memarg.into())),
                    Lang::I32Load8U(memarg, _) => insn(Instruction::I32Load8U(memarg.into())),
                    Lang::I32Load8S(memarg, _) => insn(Instruction::I32Load8S(memarg.into())),
                    Lang::I32Load16U(memarg, _) => insn(Instruction::I32Load16U(memarg.into())),
                    Lang::I32Load16S(memarg, _) => insn(Instruction::I32Load16S(memarg.into())),
                    Lang::I64Load8U(memarg, _) => insn(Instruction::I64Load8U(memarg.into())),
                    Lang::I64Load8S(memarg, _) => insn(Instruction::I64Load8S(memarg.into())),
                    Lang::I64Load16U(memarg, _) => insn(Instruction::I64Load16U(memarg.into())),
                    Lang::I64Load16S(memarg, _) => insn(Instruction::I64Load16S(memarg.into())),
                    Lang::I64Load32U(memarg, _) => insn(Instruction::I64Load32U(memarg.into())),
                    Lang::I64Load32S(memarg, _) => insn(Instruction::I64Load32S(memarg.into())),
                    Lang::RandI32 => insn(Instruction::I32Const(config.rng().r#gen())),
                    Lang::RandI64 => insn(Instruction::I64Const(config.rng().r#gen())),
                    Lang::RandF32 => {
                        newfunc.instruction(&Instruction::F32Const(f32::from_bits(
                            config.rng().r#gen(),
                        )));
                    }
                    Lang::RandF64 => {
                        newfunc.instruction(&Instruction::F64Const(f64::from_bits(
                            config.rng().r#gen(),
                        )));
                    }
                    Lang::Undef => { /* Do nothig */ }
                    Lang::UnfoldI32(value) => {
                        let child = &nodes[usize::from(*value)];
                        match child {
                            Lang::I32(value) => {
                                // Getting type from eclass.

                                let r: i32 = config.rng().r#gen();
                                insn(Instruction::I32Const(r));
                                insn(Instruction::I32Const(
                                    (Wrapping(*value as i32) - Wrapping(r)).0,
                                ));
                                insn(Instruction::I32Add);
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

                                let r: i64 = config.rng().r#gen();
                                insn(Instruction::I64Const(r));
                                insn(Instruction::I64Const((Wrapping(*value) - Wrapping(r)).0));
                                insn(Instruction::I64Add);
                            }
                            _ => {
                                return Err(Error::other(format!(
                                    "The current eterm cannot be unfolded {:?}.\n expr {}",
                                    child, expr
                                )))
                            }
                        }
                    }
                    Lang::I32(v) => insn(Instruction::I32Const(*v)),
                    Lang::I64(v) => insn(Instruction::I64Const(*v)),
                    Lang::F32(v) => insn(Instruction::F32Const(v.to_f32())),
                    Lang::F64(v) => insn(Instruction::F64Const(v.to_f64())),
                    Lang::V128(v) => insn(Instruction::V128Const(*v)),
                    Lang::I32Add(_) => insn(Instruction::I32Add),
                    Lang::I64Add(_) => insn(Instruction::I64Add),
                    Lang::I32Sub(_) => insn(Instruction::I32Sub),
                    Lang::I64Sub(_) => insn(Instruction::I64Sub),
                    Lang::I32Mul(_) => insn(Instruction::I32Mul),
                    Lang::I64Mul(_) => insn(Instruction::I64Mul),
                    Lang::I32And(_) => insn(Instruction::I32And),
                    Lang::I64And(_) => insn(Instruction::I64And),
                    Lang::I32Or(_) => insn(Instruction::I32Or),
                    Lang::I64Or(_) => insn(Instruction::I64Or),
                    Lang::I32Xor(_) => insn(Instruction::I32Xor),
                    Lang::I64Xor(_) => insn(Instruction::I64Xor),
                    Lang::I32Shl(_) => insn(Instruction::I32Shl),
                    Lang::I64Shl(_) => insn(Instruction::I64Shl),
                    Lang::I32ShrU(_) => insn(Instruction::I32ShrU),
                    Lang::I64ShrU(_) => insn(Instruction::I64ShrU),
                    Lang::I32DivU(_) => insn(Instruction::I32DivU),
                    Lang::I64DivU(_) => insn(Instruction::I64DivU),
                    Lang::I32DivS(_) => insn(Instruction::I32DivS),
                    Lang::I64DivS(_) => insn(Instruction::I64DivS),
                    Lang::I32ShrS(_) => insn(Instruction::I32ShrS),
                    Lang::I64ShrS(_) => insn(Instruction::I64ShrS),
                    Lang::I32RotR(_) => insn(Instruction::I32Rotr),
                    Lang::I64RotR(_) => insn(Instruction::I64Rotr),
                    Lang::I32RotL(_) => insn(Instruction::I32Rotl),
                    Lang::I64RotL(_) => insn(Instruction::I64Rotl),
                    Lang::I32RemS(_) => insn(Instruction::I32RemS),
                    Lang::I64RemS(_) => insn(Instruction::I64RemS),
                    Lang::I32RemU(_) => insn(Instruction::I32RemU),
                    Lang::I64RemU(_) => insn(Instruction::I64RemU),
                    Lang::I32Eqz(_) => insn(Instruction::I32Eqz),
                    Lang::I64Eqz(_) => insn(Instruction::I64Eqz),
                    Lang::I32Eq(_) => insn(Instruction::I32Eq),
                    Lang::I64Eq(_) => insn(Instruction::I64Eq),
                    Lang::I32Ne(_) => insn(Instruction::I32Ne),
                    Lang::I64Ne(_) => insn(Instruction::I64Ne),
                    Lang::I32LtS(_) => insn(Instruction::I32LtS),
                    Lang::I64LtS(_) => insn(Instruction::I64LtS),
                    Lang::I32LtU(_) => insn(Instruction::I32LtU),
                    Lang::I64LtU(_) => insn(Instruction::I64LtU),
                    Lang::I32GtS(_) => insn(Instruction::I32GtS),
                    Lang::I64GtS(_) => insn(Instruction::I64GtS),
                    Lang::I32GtU(_) => insn(Instruction::I32GtU),
                    Lang::I64GtU(_) => insn(Instruction::I64GtU),
                    Lang::I32LeS(_) => insn(Instruction::I32LeS),
                    Lang::I64LeS(_) => insn(Instruction::I64LeS),
                    Lang::I32LeU(_) => insn(Instruction::I32LeU),
                    Lang::I64LeU(_) => insn(Instruction::I64LeU),
                    Lang::I32GeS(_) => insn(Instruction::I32GeS),
                    Lang::I64GeS(_) => insn(Instruction::I64GeS),
                    Lang::I32GeU(_) => insn(Instruction::I32GeU),
                    Lang::I64GeU(_) => insn(Instruction::I64GeU),
                    Lang::I32Clz(_) => insn(Instruction::I32Clz),
                    Lang::I32Ctz(_) => insn(Instruction::I32Ctz),
                    Lang::I64Ctz(_) => insn(Instruction::I64Ctz),
                    Lang::I64Clz(_) => insn(Instruction::I64Clz),
                    Lang::F32Abs(_) => insn(Instruction::F32Abs),
                    Lang::F64Abs(_) => insn(Instruction::F64Abs),
                    Lang::F32Neg(_) => insn(Instruction::F32Neg),
                    Lang::F64Neg(_) => insn(Instruction::F64Neg),
                    Lang::F32Sqrt(_) => insn(Instruction::F32Sqrt),
                    Lang::F64Sqrt(_) => insn(Instruction::F64Sqrt),
                    Lang::F32Ceil(_) => insn(Instruction::F32Ceil),
                    Lang::F64Ceil(_) => insn(Instruction::F64Ceil),
                    Lang::F32Floor(_) => insn(Instruction::F32Floor),
                    Lang::F64Floor(_) => insn(Instruction::F64Floor),
                    Lang::F32Trunc(_) => insn(Instruction::F32Trunc),
                    Lang::F64Trunc(_) => insn(Instruction::F64Trunc),
                    Lang::F32Nearest(_) => insn(Instruction::F32Nearest),
                    Lang::F64Nearest(_) => insn(Instruction::F64Nearest),
                    Lang::I32TruncF32S(_) => insn(Instruction::I32TruncF32S),
                    Lang::I32TruncF32U(_) => insn(Instruction::I32TruncF32U),
                    Lang::I32TruncF64S(_) => insn(Instruction::I32TruncF64S),
                    Lang::I32TruncF64U(_) => insn(Instruction::I32TruncF64U),
                    Lang::I64TruncF32S(_) => insn(Instruction::I64TruncF32S),
                    Lang::I64TruncF32U(_) => insn(Instruction::I64TruncF32U),
                    Lang::I64TruncF64S(_) => insn(Instruction::I64TruncF64S),
                    Lang::I64TruncF64U(_) => insn(Instruction::I64TruncF64U),
                    Lang::F32ConvertI32S(_) => insn(Instruction::F32ConvertI32S),
                    Lang::F32ConvertI32U(_) => insn(Instruction::F32ConvertI32U),
                    Lang::F32ConvertI64S(_) => insn(Instruction::F32ConvertI64S),
                    Lang::F32ConvertI64U(_) => insn(Instruction::F32ConvertI64U),
                    Lang::F32DemoteF64(_) => insn(Instruction::F32DemoteF64),
                    Lang::F64ConvertI32S(_) => insn(Instruction::F64ConvertI32S),
                    Lang::F64ConvertI32U(_) => insn(Instruction::F64ConvertI32U),
                    Lang::F64ConvertI64S(_) => insn(Instruction::F64ConvertI64S),
                    Lang::F64ConvertI64U(_) => insn(Instruction::F64ConvertI64U),
                    Lang::F64PromoteF32(_) => insn(Instruction::F64PromoteF32),
                    Lang::I32ReinterpretF32(_) => insn(Instruction::I32ReinterpretF32),
                    Lang::I64ReinterpretF64(_) => insn(Instruction::I64ReinterpretF64),
                    Lang::F32ReinterpretI32(_) => insn(Instruction::F32ReinterpretI32),
                    Lang::F64ReinterpretI64(_) => insn(Instruction::F64ReinterpretI64),
                    Lang::I32TruncSatF32S(_) => insn(Instruction::I32TruncSatF32S),
                    Lang::I32TruncSatF32U(_) => insn(Instruction::I32TruncSatF32U),
                    Lang::I32TruncSatF64S(_) => insn(Instruction::I32TruncSatF64S),
                    Lang::I32TruncSatF64U(_) => insn(Instruction::I32TruncSatF64U),
                    Lang::I64TruncSatF32S(_) => insn(Instruction::I64TruncSatF32S),
                    Lang::I64TruncSatF32U(_) => insn(Instruction::I64TruncSatF32U),
                    Lang::I64TruncSatF64S(_) => insn(Instruction::I64TruncSatF64S),
                    Lang::I64TruncSatF64U(_) => insn(Instruction::I64TruncSatF64U),
                    Lang::I32Popcnt(_) => insn(Instruction::I32Popcnt),
                    Lang::I64Popcnt(_) => insn(Instruction::I64Popcnt),
                    Lang::I32Extend8S(_) => insn(Instruction::I32Extend8S),
                    Lang::I64Extend8S(_) => insn(Instruction::I64Extend8S),
                    Lang::I32Extend16S(_) => insn(Instruction::I32Extend16S),
                    Lang::I64Extend16S(_) => insn(Instruction::I64Extend16S),
                    Lang::I64Extend32S(_) => insn(Instruction::I64Extend32S),
                    Lang::I64ExtendI32S(_) => insn(Instruction::I64ExtendI32S),
                    Lang::I64ExtendI32U(_) => insn(Instruction::I64ExtendI32U),
                    Lang::F32Add(_) => insn(Instruction::F32Add),
                    Lang::F64Add(_) => insn(Instruction::F64Add),
                    Lang::F32Sub(_) => insn(Instruction::F32Sub),
                    Lang::F64Sub(_) => insn(Instruction::F64Sub),
                    Lang::F32Mul(_) => insn(Instruction::F32Mul),
                    Lang::F64Mul(_) => insn(Instruction::F64Mul),
                    Lang::F32Div(_) => insn(Instruction::F32Div),
                    Lang::F64Div(_) => insn(Instruction::F64Div),
                    Lang::F32Min(_) => insn(Instruction::F32Min),
                    Lang::F64Min(_) => insn(Instruction::F64Min),
                    Lang::F32Max(_) => insn(Instruction::F32Max),
                    Lang::F64Max(_) => insn(Instruction::F64Max),
                    Lang::F32Copysign(_) => insn(Instruction::F32Copysign),
                    Lang::F64Copysign(_) => insn(Instruction::F64Copysign),
                    Lang::F32Eq(_) => insn(Instruction::F32Eq),
                    Lang::F64Eq(_) => insn(Instruction::F64Eq),
                    Lang::F32Ne(_) => insn(Instruction::F32Ne),
                    Lang::F64Ne(_) => insn(Instruction::F64Ne),
                    Lang::F32Lt(_) => insn(Instruction::F32Lt),
                    Lang::F64Lt(_) => insn(Instruction::F64Lt),
                    Lang::F32Gt(_) => insn(Instruction::F32Gt),
                    Lang::F64Gt(_) => insn(Instruction::F64Gt),
                    Lang::F32Le(_) => insn(Instruction::F32Le),
                    Lang::F64Le(_) => insn(Instruction::F64Le),
                    Lang::F32Ge(_) => insn(Instruction::F32Ge),
                    Lang::F64Ge(_) => insn(Instruction::F64Ge),
                    Lang::I32Store(memarg, _) => insn(Instruction::I32Store(memarg.into())),
                    Lang::I64Store(memarg, _) => insn(Instruction::I64Store(memarg.into())),
                    Lang::F32Store(memarg, _) => insn(Instruction::F32Store(memarg.into())),
                    Lang::F64Store(memarg, _) => insn(Instruction::F64Store(memarg.into())),
                    Lang::I32Store8(memarg, _) => insn(Instruction::I32Store8(memarg.into())),
                    Lang::I32Store16(memarg, _) => insn(Instruction::I32Store16(memarg.into())),
                    Lang::I64Store8(memarg, _) => insn(Instruction::I64Store8(memarg.into())),
                    Lang::I64Store16(memarg, _) => insn(Instruction::I64Store16(memarg.into())),
                    Lang::I64Store32(memarg, _) => insn(Instruction::I64Store32(memarg.into())),
                    Lang::Nop => insn(Instruction::Nop),
                    Lang::Container(_) => {
                        // Do nothing
                    }
                    Lang::Select(_) => insn(Instruction::Select),
                    Lang::MemoryGrow(mem, _) => insn(Instruction::MemoryGrow(*mem)),
                    Lang::MemorySize(mem) => insn(Instruction::MemorySize(*mem)),
                    Lang::MemoryInit(init, _) => {
                        newfunc.instruction(&Instruction::MemoryInit {
                            mem: init.memory,
                            data_index: init.segment,
                        });
                    }
                    Lang::MemoryCopy(cp, _) => {
                        newfunc.instruction(&Instruction::MemoryCopy {
                            src_mem: cp.src,
                            dst_mem: cp.dst,
                        });
                    }
                    Lang::MemoryFill(mem, _) => insn(Instruction::MemoryFill(*mem)),
                    Lang::DataDrop(idx) => insn(Instruction::DataDrop(*idx)),
                    Lang::TableInit(init, _) => {
                        newfunc.instruction(&Instruction::TableInit {
                            table: init.table,
                            elem_index: init.segment,
                        });
                    }
                    Lang::TableCopy(cp, _) => {
                        newfunc.instruction(&Instruction::TableCopy {
                            src_table: cp.src,
                            dst_table: cp.dst,
                        });
                    }
                    Lang::TableFill(table, _) => insn(Instruction::TableFill(*table)),
                    Lang::ElemDrop(idx) => insn(Instruction::ElemDrop(*idx)),
                    Lang::TableGrow(table, _) => insn(Instruction::TableGrow(*table)),
                    Lang::TableSize(table) => insn(Instruction::TableSize(*table)),
                    Lang::TableGet(table, _) => insn(Instruction::TableGet(*table)),
                    Lang::TableSet(table, _) => insn(Instruction::TableSet(*table)),
                    Lang::I32UseGlobal(_) => {
                        // Request a new global
                        let request = ResourceRequest::Global {
                            tpe: PrimitiveTypeInfo::I32,
                            mutable: true,
                            shared: false,
                        };
                        resources.push(request);

                        insn(Instruction::GlobalSet(global_idx));
                        insn(Instruction::GlobalGet(global_idx));
                        global_idx += 1;
                    }
                    Lang::I64UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            tpe: PrimitiveTypeInfo::I64,
                            mutable: true,
                            shared: false,
                        };
                        resources.push(request);

                        insn(Instruction::GlobalSet(global_idx));
                        insn(Instruction::GlobalGet(global_idx));
                        global_idx += 1;
                    }
                    Lang::F32UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            tpe: PrimitiveTypeInfo::F32,
                            mutable: true,
                            shared: false,
                        };
                        resources.push(request);

                        insn(Instruction::GlobalSet(global_idx));
                        insn(Instruction::GlobalGet(global_idx));
                        global_idx += 1;
                    }
                    Lang::F64UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            tpe: PrimitiveTypeInfo::F64,
                            mutable: true,
                            shared: false,
                        };
                        resources.push(request);

                        insn(Instruction::GlobalSet(global_idx));
                        insn(Instruction::GlobalGet(global_idx));
                        global_idx += 1;
                    }
                    Lang::RefNull(valtype) => insn(Instruction::RefNull((*valtype).into())),
                    Lang::RefFunc(idx) => insn(Instruction::RefFunc(*idx)),
                    Lang::RefIsNull(_) => insn(Instruction::RefIsNull),

                    Lang::V128Not(_) => insn(Instruction::V128Not),
                    Lang::V128And(_) => insn(Instruction::V128And),
                    Lang::V128AndNot(_) => insn(Instruction::V128AndNot),
                    Lang::V128Or(_) => insn(Instruction::V128Or),
                    Lang::V128Xor(_) => insn(Instruction::V128Xor),
                    Lang::V128AnyTrue(_) => insn(Instruction::V128AnyTrue),
                    Lang::V128Bitselect(_) => insn(Instruction::V128Bitselect),

                    Lang::V128Load(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load(memarg.into()));
                    }
                    Lang::V128Load8x8S(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load8x8S(memarg.into()));
                    }
                    Lang::V128Load8x8U(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load8x8U(memarg.into()));
                    }
                    Lang::V128Load16x4S(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load16x4S(memarg.into()));
                    }
                    Lang::V128Load16x4U(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load16x4U(memarg.into()));
                    }
                    Lang::V128Load32x2S(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load32x2S(memarg.into()));
                    }
                    Lang::V128Load32x2U(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load32x2U(memarg.into()));
                    }
                    Lang::V128Load8Splat(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load8Splat(memarg.into()));
                    }
                    Lang::V128Load16Splat(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load16Splat(memarg.into()));
                    }
                    Lang::V128Load32Splat(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load32Splat(memarg.into()));
                    }
                    Lang::V128Load64Splat(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load64Splat(memarg.into()));
                    }
                    Lang::V128Load32Zero(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load32Zero(memarg.into()));
                    }
                    Lang::V128Load64Zero(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load64Zero(memarg.into()));
                    }
                    Lang::V128Store(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Store(memarg.into()));
                    }
                    Lang::V128Load8Lane(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load8Lane {
                            memarg: (&memarg.memarg).into(),
                            lane: memarg.lane,
                        });
                    }
                    Lang::V128Load16Lane(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load16Lane {
                            memarg: (&memarg.memarg).into(),
                            lane: memarg.lane,
                        });
                    }
                    Lang::V128Load32Lane(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load32Lane {
                            memarg: (&memarg.memarg).into(),
                            lane: memarg.lane,
                        });
                    }
                    Lang::V128Load64Lane(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Load64Lane {
                            memarg: (&memarg.memarg).into(),
                            lane: memarg.lane,
                        });
                    }
                    Lang::V128Store8Lane(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Store8Lane {
                            memarg: (&memarg.memarg).into(),
                            lane: memarg.lane,
                        });
                    }
                    Lang::V128Store16Lane(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Store16Lane {
                            memarg: (&memarg.memarg).into(),
                            lane: memarg.lane,
                        });
                    }
                    Lang::V128Store32Lane(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Store32Lane {
                            memarg: (&memarg.memarg).into(),
                            lane: memarg.lane,
                        });
                    }
                    Lang::V128Store64Lane(memarg, _) => {
                        newfunc.instruction(&Instruction::V128Store64Lane {
                            memarg: (&memarg.memarg).into(),
                            lane: memarg.lane,
                        });
                    }

                    Lang::I8x16ExtractLaneS(lane, _) => insn(Instruction::I8x16ExtractLaneS(*lane)),
                    Lang::I8x16ExtractLaneU(lane, _) => insn(Instruction::I8x16ExtractLaneU(*lane)),
                    Lang::I8x16ReplaceLane(lane, _) => insn(Instruction::I8x16ReplaceLane(*lane)),
                    Lang::I16x8ExtractLaneS(lane, _) => insn(Instruction::I16x8ExtractLaneS(*lane)),
                    Lang::I16x8ExtractLaneU(lane, _) => insn(Instruction::I16x8ExtractLaneU(*lane)),
                    Lang::I16x8ReplaceLane(lane, _) => insn(Instruction::I16x8ReplaceLane(*lane)),
                    Lang::I32x4ExtractLane(lane, _) => insn(Instruction::I32x4ExtractLane(*lane)),
                    Lang::I32x4ReplaceLane(lane, _) => insn(Instruction::I32x4ReplaceLane(*lane)),
                    Lang::I64x2ExtractLane(lane, _) => insn(Instruction::I64x2ExtractLane(*lane)),
                    Lang::I64x2ReplaceLane(lane, _) => insn(Instruction::I64x2ReplaceLane(*lane)),
                    Lang::F32x4ExtractLane(lane, _) => insn(Instruction::F32x4ExtractLane(*lane)),
                    Lang::F32x4ReplaceLane(lane, _) => insn(Instruction::F32x4ReplaceLane(*lane)),
                    Lang::F64x2ExtractLane(lane, _) => insn(Instruction::F64x2ExtractLane(*lane)),
                    Lang::F64x2ReplaceLane(lane, _) => insn(Instruction::F64x2ReplaceLane(*lane)),

                    Lang::I8x16Swizzle(_) => insn(Instruction::I8x16Swizzle),
                    Lang::I8x16Shuffle(indices, _) => {
                        insn(Instruction::I8x16Shuffle(indices.indices))
                    }
                    Lang::I8x16Splat(_) => insn(Instruction::I8x16Splat),
                    Lang::I16x8Splat(_) => insn(Instruction::I16x8Splat),
                    Lang::I32x4Splat(_) => insn(Instruction::I32x4Splat),
                    Lang::I64x2Splat(_) => insn(Instruction::I64x2Splat),
                    Lang::F32x4Splat(_) => insn(Instruction::F32x4Splat),
                    Lang::F64x2Splat(_) => insn(Instruction::F64x2Splat),

                    Lang::I8x16Eq(_) => insn(Instruction::I8x16Eq),
                    Lang::I8x16Ne(_) => insn(Instruction::I8x16Ne),
                    Lang::I8x16LtS(_) => insn(Instruction::I8x16LtS),
                    Lang::I8x16LtU(_) => insn(Instruction::I8x16LtU),
                    Lang::I8x16GtS(_) => insn(Instruction::I8x16GtS),
                    Lang::I8x16GtU(_) => insn(Instruction::I8x16GtU),
                    Lang::I8x16LeS(_) => insn(Instruction::I8x16LeS),
                    Lang::I8x16LeU(_) => insn(Instruction::I8x16LeU),
                    Lang::I8x16GeS(_) => insn(Instruction::I8x16GeS),
                    Lang::I8x16GeU(_) => insn(Instruction::I8x16GeU),
                    Lang::I16x8Eq(_) => insn(Instruction::I16x8Eq),
                    Lang::I16x8Ne(_) => insn(Instruction::I16x8Ne),
                    Lang::I16x8LtS(_) => insn(Instruction::I16x8LtS),
                    Lang::I16x8LtU(_) => insn(Instruction::I16x8LtU),
                    Lang::I16x8GtS(_) => insn(Instruction::I16x8GtS),
                    Lang::I16x8GtU(_) => insn(Instruction::I16x8GtU),
                    Lang::I16x8LeS(_) => insn(Instruction::I16x8LeS),
                    Lang::I16x8LeU(_) => insn(Instruction::I16x8LeU),
                    Lang::I16x8GeS(_) => insn(Instruction::I16x8GeS),
                    Lang::I16x8GeU(_) => insn(Instruction::I16x8GeU),
                    Lang::I32x4Eq(_) => insn(Instruction::I32x4Eq),
                    Lang::I32x4Ne(_) => insn(Instruction::I32x4Ne),
                    Lang::I32x4LtS(_) => insn(Instruction::I32x4LtS),
                    Lang::I32x4LtU(_) => insn(Instruction::I32x4LtU),
                    Lang::I32x4GtS(_) => insn(Instruction::I32x4GtS),
                    Lang::I32x4GtU(_) => insn(Instruction::I32x4GtU),
                    Lang::I32x4LeS(_) => insn(Instruction::I32x4LeS),
                    Lang::I32x4LeU(_) => insn(Instruction::I32x4LeU),
                    Lang::I32x4GeS(_) => insn(Instruction::I32x4GeS),
                    Lang::I32x4GeU(_) => insn(Instruction::I32x4GeU),
                    Lang::I64x2Eq(_) => insn(Instruction::I64x2Eq),
                    Lang::I64x2Ne(_) => insn(Instruction::I64x2Ne),
                    Lang::I64x2LtS(_) => insn(Instruction::I64x2LtS),
                    Lang::I64x2GtS(_) => insn(Instruction::I64x2GtS),
                    Lang::I64x2LeS(_) => insn(Instruction::I64x2LeS),
                    Lang::I64x2GeS(_) => insn(Instruction::I64x2GeS),
                    Lang::F32x4Eq(_) => insn(Instruction::F32x4Eq),
                    Lang::F32x4Ne(_) => insn(Instruction::F32x4Ne),
                    Lang::F32x4Lt(_) => insn(Instruction::F32x4Lt),
                    Lang::F32x4Gt(_) => insn(Instruction::F32x4Gt),
                    Lang::F32x4Le(_) => insn(Instruction::F32x4Le),
                    Lang::F32x4Ge(_) => insn(Instruction::F32x4Ge),
                    Lang::F64x2Eq(_) => insn(Instruction::F64x2Eq),
                    Lang::F64x2Ne(_) => insn(Instruction::F64x2Ne),
                    Lang::F64x2Lt(_) => insn(Instruction::F64x2Lt),
                    Lang::F64x2Gt(_) => insn(Instruction::F64x2Gt),
                    Lang::F64x2Le(_) => insn(Instruction::F64x2Le),
                    Lang::F64x2Ge(_) => insn(Instruction::F64x2Ge),

                    Lang::I8x16Abs(_) => insn(Instruction::I8x16Abs),
                    Lang::I8x16Neg(_) => insn(Instruction::I8x16Neg),
                    Lang::I8x16Popcnt(_) => insn(Instruction::I8x16Popcnt),
                    Lang::I8x16AllTrue(_) => insn(Instruction::I8x16AllTrue),
                    Lang::I8x16Bitmask(_) => insn(Instruction::I8x16Bitmask),
                    Lang::I8x16NarrowI16x8S(_) => insn(Instruction::I8x16NarrowI16x8S),
                    Lang::I8x16NarrowI16x8U(_) => insn(Instruction::I8x16NarrowI16x8U),
                    Lang::I8x16Shl(_) => insn(Instruction::I8x16Shl),
                    Lang::I8x16ShrS(_) => insn(Instruction::I8x16ShrS),
                    Lang::I8x16ShrU(_) => insn(Instruction::I8x16ShrU),
                    Lang::I8x16Add(_) => insn(Instruction::I8x16Add),
                    Lang::I8x16AddSatS(_) => insn(Instruction::I8x16AddSatS),
                    Lang::I8x16AddSatU(_) => insn(Instruction::I8x16AddSatU),
                    Lang::I8x16Sub(_) => insn(Instruction::I8x16Sub),
                    Lang::I8x16SubSatS(_) => insn(Instruction::I8x16SubSatS),
                    Lang::I8x16SubSatU(_) => insn(Instruction::I8x16SubSatU),
                    Lang::I8x16MinS(_) => insn(Instruction::I8x16MinS),
                    Lang::I8x16MinU(_) => insn(Instruction::I8x16MinU),
                    Lang::I8x16MaxS(_) => insn(Instruction::I8x16MaxS),
                    Lang::I8x16MaxU(_) => insn(Instruction::I8x16MaxU),
                    Lang::I8x16AvgrU(_) => insn(Instruction::I8x16AvgrU),

                    Lang::I16x8ExtAddPairwiseI8x16S(_) => {
                        insn(Instruction::I16x8ExtAddPairwiseI8x16S)
                    }
                    Lang::I16x8ExtAddPairwiseI8x16U(_) => {
                        insn(Instruction::I16x8ExtAddPairwiseI8x16U)
                    }
                    Lang::I16x8Abs(_) => insn(Instruction::I16x8Abs),
                    Lang::I16x8Neg(_) => insn(Instruction::I16x8Neg),
                    Lang::I16x8Q15MulrSatS(_) => insn(Instruction::I16x8Q15MulrSatS),
                    Lang::I16x8AllTrue(_) => insn(Instruction::I16x8AllTrue),
                    Lang::I16x8Bitmask(_) => insn(Instruction::I16x8Bitmask),
                    Lang::I16x8NarrowI32x4S(_) => insn(Instruction::I16x8NarrowI32x4S),
                    Lang::I16x8NarrowI32x4U(_) => insn(Instruction::I16x8NarrowI32x4U),
                    Lang::I16x8ExtendLowI8x16S(_) => insn(Instruction::I16x8ExtendLowI8x16S),
                    Lang::I16x8ExtendHighI8x16S(_) => insn(Instruction::I16x8ExtendHighI8x16S),
                    Lang::I16x8ExtendLowI8x16U(_) => insn(Instruction::I16x8ExtendLowI8x16U),
                    Lang::I16x8ExtendHighI8x16U(_) => insn(Instruction::I16x8ExtendHighI8x16U),
                    Lang::I16x8Shl(_) => insn(Instruction::I16x8Shl),
                    Lang::I16x8ShrS(_) => insn(Instruction::I16x8ShrS),
                    Lang::I16x8ShrU(_) => insn(Instruction::I16x8ShrU),
                    Lang::I16x8Add(_) => insn(Instruction::I16x8Add),
                    Lang::I16x8AddSatS(_) => insn(Instruction::I16x8AddSatS),
                    Lang::I16x8AddSatU(_) => insn(Instruction::I16x8AddSatU),
                    Lang::I16x8Sub(_) => insn(Instruction::I16x8Sub),
                    Lang::I16x8SubSatS(_) => insn(Instruction::I16x8SubSatS),
                    Lang::I16x8SubSatU(_) => insn(Instruction::I16x8SubSatU),
                    Lang::I16x8Mul(_) => insn(Instruction::I16x8Mul),
                    Lang::I16x8MinS(_) => insn(Instruction::I16x8MinS),
                    Lang::I16x8MinU(_) => insn(Instruction::I16x8MinU),
                    Lang::I16x8MaxS(_) => insn(Instruction::I16x8MaxS),
                    Lang::I16x8MaxU(_) => insn(Instruction::I16x8MaxU),
                    Lang::I16x8AvgrU(_) => insn(Instruction::I16x8AvgrU),
                    Lang::I16x8ExtMulLowI8x16S(_) => insn(Instruction::I16x8ExtMulLowI8x16S),
                    Lang::I16x8ExtMulHighI8x16S(_) => insn(Instruction::I16x8ExtMulHighI8x16S),
                    Lang::I16x8ExtMulLowI8x16U(_) => insn(Instruction::I16x8ExtMulLowI8x16U),
                    Lang::I16x8ExtMulHighI8x16U(_) => insn(Instruction::I16x8ExtMulHighI8x16U),

                    Lang::I32x4ExtAddPairwiseI16x8S(_) => {
                        insn(Instruction::I32x4ExtAddPairwiseI16x8S)
                    }
                    Lang::I32x4ExtAddPairwiseI16x8U(_) => {
                        insn(Instruction::I32x4ExtAddPairwiseI16x8U)
                    }
                    Lang::I32x4Abs(_) => insn(Instruction::I32x4Abs),
                    Lang::I32x4Neg(_) => insn(Instruction::I32x4Neg),
                    Lang::I32x4AllTrue(_) => insn(Instruction::I32x4AllTrue),
                    Lang::I32x4Bitmask(_) => insn(Instruction::I32x4Bitmask),
                    Lang::I32x4ExtendLowI16x8S(_) => insn(Instruction::I32x4ExtendLowI16x8S),
                    Lang::I32x4ExtendHighI16x8S(_) => insn(Instruction::I32x4ExtendHighI16x8S),
                    Lang::I32x4ExtendLowI16x8U(_) => insn(Instruction::I32x4ExtendLowI16x8U),
                    Lang::I32x4ExtendHighI16x8U(_) => insn(Instruction::I32x4ExtendHighI16x8U),
                    Lang::I32x4Shl(_) => insn(Instruction::I32x4Shl),
                    Lang::I32x4ShrS(_) => insn(Instruction::I32x4ShrS),
                    Lang::I32x4ShrU(_) => insn(Instruction::I32x4ShrU),
                    Lang::I32x4Add(_) => insn(Instruction::I32x4Add),
                    Lang::I32x4Sub(_) => insn(Instruction::I32x4Sub),
                    Lang::I32x4Mul(_) => insn(Instruction::I32x4Mul),
                    Lang::I32x4MinS(_) => insn(Instruction::I32x4MinS),
                    Lang::I32x4MinU(_) => insn(Instruction::I32x4MinU),
                    Lang::I32x4MaxS(_) => insn(Instruction::I32x4MaxS),
                    Lang::I32x4MaxU(_) => insn(Instruction::I32x4MaxU),
                    Lang::I32x4DotI16x8S(_) => insn(Instruction::I32x4DotI16x8S),
                    Lang::I32x4ExtMulLowI16x8S(_) => insn(Instruction::I32x4ExtMulLowI16x8S),
                    Lang::I32x4ExtMulHighI16x8S(_) => insn(Instruction::I32x4ExtMulHighI16x8S),
                    Lang::I32x4ExtMulLowI16x8U(_) => insn(Instruction::I32x4ExtMulLowI16x8U),
                    Lang::I32x4ExtMulHighI16x8U(_) => insn(Instruction::I32x4ExtMulHighI16x8U),

                    Lang::I64x2Abs(_) => insn(Instruction::I64x2Abs),
                    Lang::I64x2Neg(_) => insn(Instruction::I64x2Neg),
                    Lang::I64x2AllTrue(_) => insn(Instruction::I64x2AllTrue),
                    Lang::I64x2Bitmask(_) => insn(Instruction::I64x2Bitmask),
                    Lang::I64x2ExtendLowI32x4S(_) => insn(Instruction::I64x2ExtendLowI32x4S),
                    Lang::I64x2ExtendHighI32x4S(_) => insn(Instruction::I64x2ExtendHighI32x4S),
                    Lang::I64x2ExtendLowI32x4U(_) => insn(Instruction::I64x2ExtendLowI32x4U),
                    Lang::I64x2ExtendHighI32x4U(_) => insn(Instruction::I64x2ExtendHighI32x4U),
                    Lang::I64x2Shl(_) => insn(Instruction::I64x2Shl),
                    Lang::I64x2ShrS(_) => insn(Instruction::I64x2ShrS),
                    Lang::I64x2ShrU(_) => insn(Instruction::I64x2ShrU),
                    Lang::I64x2Add(_) => insn(Instruction::I64x2Add),
                    Lang::I64x2Sub(_) => insn(Instruction::I64x2Sub),
                    Lang::I64x2Mul(_) => insn(Instruction::I64x2Mul),
                    Lang::I64x2ExtMulLowI32x4S(_) => insn(Instruction::I64x2ExtMulLowI32x4S),
                    Lang::I64x2ExtMulHighI32x4S(_) => insn(Instruction::I64x2ExtMulHighI32x4S),
                    Lang::I64x2ExtMulLowI32x4U(_) => insn(Instruction::I64x2ExtMulLowI32x4U),
                    Lang::I64x2ExtMulHighI32x4U(_) => insn(Instruction::I64x2ExtMulHighI32x4U),

                    Lang::F32x4Ceil(_) => insn(Instruction::F32x4Ceil),
                    Lang::F32x4Floor(_) => insn(Instruction::F32x4Floor),
                    Lang::F32x4Trunc(_) => insn(Instruction::F32x4Trunc),
                    Lang::F32x4Nearest(_) => insn(Instruction::F32x4Nearest),
                    Lang::F32x4Abs(_) => insn(Instruction::F32x4Abs),
                    Lang::F32x4Neg(_) => insn(Instruction::F32x4Neg),
                    Lang::F32x4Sqrt(_) => insn(Instruction::F32x4Sqrt),
                    Lang::F32x4Add(_) => insn(Instruction::F32x4Add),
                    Lang::F32x4Sub(_) => insn(Instruction::F32x4Sub),
                    Lang::F32x4Mul(_) => insn(Instruction::F32x4Mul),
                    Lang::F32x4Div(_) => insn(Instruction::F32x4Div),
                    Lang::F32x4Min(_) => insn(Instruction::F32x4Min),
                    Lang::F32x4Max(_) => insn(Instruction::F32x4Max),
                    Lang::F32x4PMin(_) => insn(Instruction::F32x4PMin),
                    Lang::F32x4PMax(_) => insn(Instruction::F32x4PMax),
                    Lang::F64x2Ceil(_) => insn(Instruction::F64x2Ceil),
                    Lang::F64x2Floor(_) => insn(Instruction::F64x2Floor),
                    Lang::F64x2Trunc(_) => insn(Instruction::F64x2Trunc),
                    Lang::F64x2Nearest(_) => insn(Instruction::F64x2Nearest),
                    Lang::F64x2Abs(_) => insn(Instruction::F64x2Abs),
                    Lang::F64x2Neg(_) => insn(Instruction::F64x2Neg),
                    Lang::F64x2Sqrt(_) => insn(Instruction::F64x2Sqrt),
                    Lang::F64x2Add(_) => insn(Instruction::F64x2Add),
                    Lang::F64x2Sub(_) => insn(Instruction::F64x2Sub),
                    Lang::F64x2Mul(_) => insn(Instruction::F64x2Mul),
                    Lang::F64x2Div(_) => insn(Instruction::F64x2Div),
                    Lang::F64x2Min(_) => insn(Instruction::F64x2Min),
                    Lang::F64x2Max(_) => insn(Instruction::F64x2Max),
                    Lang::F64x2PMin(_) => insn(Instruction::F64x2PMin),
                    Lang::F64x2PMax(_) => insn(Instruction::F64x2PMax),

                    Lang::I32x4TruncSatF32x4S(_) => insn(Instruction::I32x4TruncSatF32x4S),
                    Lang::I32x4TruncSatF32x4U(_) => insn(Instruction::I32x4TruncSatF32x4U),
                    Lang::F32x4ConvertI32x4S(_) => insn(Instruction::F32x4ConvertI32x4S),
                    Lang::F32x4ConvertI32x4U(_) => insn(Instruction::F32x4ConvertI32x4U),
                    Lang::I32x4TruncSatF64x2SZero(_) => insn(Instruction::I32x4TruncSatF64x2SZero),
                    Lang::I32x4TruncSatF64x2UZero(_) => insn(Instruction::I32x4TruncSatF64x2UZero),
                    Lang::F64x2ConvertLowI32x4S(_) => insn(Instruction::F64x2ConvertLowI32x4S),
                    Lang::F64x2ConvertLowI32x4U(_) => insn(Instruction::F64x2ConvertLowI32x4U),
                    Lang::F32x4DemoteF64x2Zero(_) => insn(Instruction::F32x4DemoteF64x2Zero),
                    Lang::F64x2PromoteLowF32x4(_) => insn(Instruction::F64x2PromoteLowF32x4),

                    Lang::I8x16RelaxedSwizzle(_) => insn(Instruction::I8x16RelaxedSwizzle),
                    Lang::I32x4RelaxedTruncF32x4S(_) => insn(Instruction::I32x4RelaxedTruncF32x4S),
                    Lang::I32x4RelaxedTruncF32x4U(_) => insn(Instruction::I32x4RelaxedTruncF32x4U),
                    Lang::I32x4RelaxedTruncF64x2SZero(_) => {
                        insn(Instruction::I32x4RelaxedTruncF64x2SZero)
                    }
                    Lang::I32x4RelaxedTruncF64x2UZero(_) => {
                        insn(Instruction::I32x4RelaxedTruncF64x2UZero)
                    }
                    Lang::F32x4RelaxedMadd(_) => insn(Instruction::F32x4RelaxedMadd),
                    Lang::F32x4RelaxedNmadd(_) => insn(Instruction::F32x4RelaxedNmadd),
                    Lang::F64x2RelaxedMadd(_) => insn(Instruction::F64x2RelaxedMadd),
                    Lang::F64x2RelaxedNmadd(_) => insn(Instruction::F64x2RelaxedNmadd),
                    Lang::I8x16RelaxedLaneselect(_) => insn(Instruction::I8x16RelaxedLaneselect),
                    Lang::I16x8RelaxedLaneselect(_) => insn(Instruction::I16x8RelaxedLaneselect),
                    Lang::I32x4RelaxedLaneselect(_) => insn(Instruction::I32x4RelaxedLaneselect),
                    Lang::I64x2RelaxedLaneselect(_) => insn(Instruction::I64x2RelaxedLaneselect),
                    Lang::F32x4RelaxedMin(_) => insn(Instruction::F32x4RelaxedMin),
                    Lang::F32x4RelaxedMax(_) => insn(Instruction::F32x4RelaxedMax),
                    Lang::F64x2RelaxedMin(_) => insn(Instruction::F64x2RelaxedMin),
                    Lang::F64x2RelaxedMax(_) => insn(Instruction::F64x2RelaxedMax),
                    Lang::I16x8RelaxedQ15mulrS(_) => insn(Instruction::I16x8RelaxedQ15mulrS),
                    Lang::I16x8RelaxedDotI8x16I7x16S(_) => {
                        insn(Instruction::I16x8RelaxedDotI8x16I7x16S)
                    }
                    Lang::I32x4RelaxedDotI8x16I7x16AddS(_) => {
                        insn(Instruction::I32x4RelaxedDotI8x16I7x16AddS)
                    }
                }
            }
        }
    }
    Ok(resources)
}

impl From<&MemArg> for wasm_encoder::MemArg {
    fn from(arg: &MemArg) -> wasm_encoder::MemArg {
        wasm_encoder::MemArg {
            offset: arg.static_offset,
            align: arg.align.into(),
            memory_index: arg.mem,
        }
    }
}
