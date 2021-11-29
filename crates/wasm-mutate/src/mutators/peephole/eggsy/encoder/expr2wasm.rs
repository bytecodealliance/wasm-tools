use std::num::Wrapping;

use crate::error::EitherType;
use crate::info::ModuleInfo;

use crate::mutators::peephole::eggsy::encoder::TraversalEvent;
use crate::mutators::peephole::{Lang, EG};
use egg::{Id, RecExpr};
use rand::Rng;
use wasm_encoder::{Function, Instruction, MemArg};

pub(crate) fn expr2wasm(
    _info: &ModuleInfo,
    rnd: &mut rand::prelude::SmallRng,
    expr: &RecExpr<Lang>,
    newfunc: &mut Function,
    _egraph: &EG,
) -> crate::Result<()> {
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

    // Enqueue the coding back nodes and infer types
    while let Some(context) = worklist.pop() {
        let rootlang = &nodes[usize::from(context.current_node)];

        match context.traversal_event {
            TraversalEvent::Enter => {
                // Push children
                match rootlang {
                    Lang::I64Add(operands)
                    | Lang::I64Shl(operands)
                    | Lang::I64ShrU(operands)
                    | Lang::I64Sub(operands)
                    | Lang::I64Mul(operands)
                    | Lang::I64And(operands)
                    | Lang::I64Or(operands)
                    | Lang::I64Xor(operands)
                    | Lang::I64ShrS(operands)
                    | Lang::I64DivS(operands)
                    | Lang::I64DivU(operands)
                    | Lang::I64RotR(operands)
                    | Lang::I64RotL(operands)
                    | Lang::I64RemS(operands)
                    | Lang::I64RemU(operands)
                    | Lang::F32Add(operands)
                    | Lang::F64Add(operands)
                    | Lang::F32Sub(operands)
                    | Lang::F64Sub(operands)
                    | Lang::F32Mul(operands)
                    | Lang::F64Mul(operands)
                    | Lang::F32Div(operands)
                    | Lang::F64Div(operands)
                    | Lang::F32Min(operands)
                    | Lang::F64Min(operands)
                    | Lang::F32Max(operands)
                    | Lang::F64Max(operands)
                    | Lang::F32Copysign(operands)
                    | Lang::F64Copysign(operands)
                    | Lang::F32Eq(operands)
                    | Lang::F64Eq(operands)
                    | Lang::F32Ne(operands)
                    | Lang::F64Ne(operands)
                    | Lang::F32Lt(operands)
                    | Lang::F64Lt(operands)
                    | Lang::F32Gt(operands)
                    | Lang::F64Gt(operands)
                    | Lang::F32Le(operands)
                    | Lang::F64Le(operands)
                    | Lang::F32Ge(operands)
                    | Lang::F64Ge(operands) => {
                        for operand in operands.iter().rev() {
                            enqueue(&mut worklist, *operand);
                        }
                    }
                    Lang::I32Add(operands)
                    | Lang::I32Shl(operands)
                    | Lang::I32ShrU(operands)
                    | Lang::I32Sub(operands)
                    | Lang::I32Mul(operands)
                    | Lang::I32And(operands)
                    | Lang::I32Or(operands)
                    | Lang::I32Xor(operands)
                    | Lang::I32ShrS(operands)
                    | Lang::I32DivS(operands)
                    | Lang::I32DivU(operands)
                    | Lang::I32RotR(operands)
                    | Lang::I32RotL(operands)
                    | Lang::I32RemS(operands)
                    | Lang::I32RemU(operands) => {
                        for operand in operands.iter().rev() {
                            enqueue(&mut worklist, *operand);
                        }
                    }
                    Lang::I32Eq(operands)
                    | Lang::I32Ne(operands)
                    | Lang::I32LtS(operands)
                    | Lang::I32LtU(operands)
                    | Lang::I32GtS(operands)
                    | Lang::I32GtU(operands)
                    | Lang::I32LeS(operands)
                    | Lang::I32LeU(operands)
                    | Lang::I32GeS(operands)
                    | Lang::I32GeU(operands)
                    | Lang::I64Eq(operands)
                    | Lang::I64Ne(operands)
                    | Lang::I64LtS(operands)
                    | Lang::I64LtU(operands)
                    | Lang::I64GtS(operands)
                    | Lang::I64GtU(operands)
                    | Lang::I64LeS(operands)
                    | Lang::I64LeU(operands)
                    | Lang::I64GeS(operands)
                    | Lang::I64GeU(operands) => {
                        for operand in operands.iter().rev() {
                            enqueue(&mut worklist, *operand);
                        }
                    }
                    Lang::I32Clz(operands)
                    | Lang::I32Ctz(operands)
                    | Lang::I64Ctz(operands)
                    | Lang::I64Clz(operands)
                    | Lang::F32Abs(operands)
                    | Lang::F64Abs(operands)
                    | Lang::F32Neg(operands)
                    | Lang::F64Neg(operands)
                    | Lang::F32Sqrt(operands)
                    | Lang::F64Sqrt(operands)
                    | Lang::F32Ceil(operands)
                    | Lang::F64Ceil(operands)
                    | Lang::F32Floor(operands)
                    | Lang::F64Floor(operands)
                    | Lang::F32Trunc(operands)
                    | Lang::F64trunc(operands)
                    | Lang::F32Nearest(operands)
                    | Lang::F64Nearest(operands)
                    | Lang::I32TruncF32S(operands)
                    | Lang::I32TruncF32U(operands)
                    | Lang::I32TruncF64S(operands)
                    | Lang::I32TruncF64U(operands)
                    | Lang::I64TruncF32S(operands)
                    | Lang::I64TruncF32U(operands)
                    | Lang::I64TruncF64S(operands)
                    | Lang::I64TruncF64U(operands)
                    | Lang::F32ConvertI32S(operands)
                    | Lang::F32ConvertI32U(operands)
                    | Lang::F32ConvertI64S(operands)
                    | Lang::F32ConvertI64U(operands)
                    | Lang::F32DemoteF64(operands)
                    | Lang::F64ConvertI32S(operands)
                    | Lang::F64ConvertI32U(operands)
                    | Lang::F64ConvertI64S(operands)
                    | Lang::F64ConvertI64U(operands)
                    | Lang::F64PromoteF32(operands)
                    | Lang::I32ReinterpretF32(operands)
                    | Lang::I64ReinterpretF64(operands)
                    | Lang::F32ReinterpretI32(operands)
                    | Lang::F64ReinterpretI64(operands)
                    | Lang::I32TruncSatF32S(operands)
                    | Lang::I32TruncSatF32U(operands)
                    | Lang::I32TruncSatF64S(operands)
                    | Lang::I32TruncSatF64U(operands)
                    | Lang::I64TruncSatF32S(operands)
                    | Lang::I64TruncSatF32U(operands)
                    | Lang::I64TruncSatF64S(operands)
                    | Lang::I64TruncSatF64U(operands)
                    | Lang::I32Popcnt(operands)
                    | Lang::I32Eqz(operands)
                    | Lang::I64Popcnt(operands)
                    | Lang::I64Eqz(operands)
                    | Lang::I32Extend8S(operands)
                    | Lang::I32Extend16S(operands)
                    | Lang::I64Extend32S(operands)
                    | Lang::I64Extend16S(operands)
                    | Lang::I64ExtendI32S(operands)
                    | Lang::I64Extend8S(operands)
                    | Lang::I64ExtendI32U(operands) => {
                        enqueue(&mut worklist, operands[0]);
                    }
                    Lang::LocalTee(_idx, operand) => {
                        enqueue(&mut worklist, *operand);
                    }
                    Lang::LocalSet(_idx, operand) => {
                        enqueue(&mut worklist, *operand);
                    }
                    Lang::GlobalSet(_idx, operand) => {
                        enqueue(&mut worklist, *operand);
                    }
                    Lang::Wrap(operands) => {
                        enqueue(&mut worklist, operands[0]);
                    }
                    Lang::Drop(operands) => {
                        enqueue(&mut worklist, operands[0]);
                    }
                    Lang::Call(_idx, operands) => {
                        for operand in operands.iter().rev() {
                            enqueue(&mut worklist, *operand);
                        }
                    }
                    Lang::Container(operands) => {
                        for operand in operands.iter().rev() {
                            enqueue(&mut worklist, *operand);
                        }
                    }
                    Lang::F32Load {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::F64Load {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I32Load8S {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I32Load8U {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I32Load16S {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I32Load16U {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I64Load8S {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I64Load8U {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I64Load16S {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I64Load16U {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I64Load32S {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I64Load32U {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I32Load {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    }
                    | Lang::I64Load {
                        align: _,
                        mem: _,
                        static_offset: _,
                        offset,
                    } => {
                        enqueue(&mut worklist, *offset);
                    }
                    Lang::I32Store {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    }
                    | Lang::I64Store {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    }
                    | Lang::F32Store {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    }
                    | Lang::F64Store {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    }
                    | Lang::I32Store8 {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    }
                    | Lang::I32Store16 {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    }
                    | Lang::I64Store8 {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    }
                    | Lang::I64Store16 {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    }
                    | Lang::I64Store32 {
                        align: _,
                        mem: _,
                        static_offset: _,
                        value_and_offset,
                    } => {
                        enqueue(&mut worklist, value_and_offset[1]);
                        enqueue(&mut worklist, value_and_offset[0]);
                    }
                    _ => { /* Do nothing */ }
                }
            }
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
                        newfunc.instruction(&Instruction::I32Const(rnd.gen()));
                    }
                    Lang::RandI64 => {
                        newfunc.instruction(&Instruction::I64Const(rnd.gen()));
                    }
                    Lang::Undef => { /* Do nothig */ }
                    Lang::UnfoldI32(value) => {
                        let child = &nodes[usize::from(*value)];
                        match child {
                            Lang::I32(value) => {
                                // Getting type from eclass.

                                let r: i32 = rnd.gen();
                                newfunc.instruction(&Instruction::I32Const(r));
                                newfunc.instruction(&Instruction::I32Const(
                                    (Wrapping(*value as i32) - Wrapping(r)).0,
                                ));
                                newfunc.instruction(&Instruction::I32Add);
                            }
                            _ => {
                                return Err(crate::Error::UnsupportedType(EitherType::EggError(
                                    format!("The current eterm cannot be unfolded {:?}", child,),
                                )))
                            }
                        }
                    }
                    Lang::UnfoldI64(value) => {
                        let child = &nodes[usize::from(*value)];
                        match child {
                            Lang::I64(value) => {
                                // Getting type from eclass.

                                let r: i64 = rnd.gen();
                                newfunc.instruction(&Instruction::I64Const(r));
                                newfunc.instruction(&Instruction::I64Const(
                                    (Wrapping(*value) - Wrapping(r)).0,
                                ));
                                newfunc.instruction(&Instruction::I64Add);
                            }
                            _ => {
                                return Err(crate::Error::UnsupportedType(EitherType::EggError(
                                    format!("The current eterm cannot be unfolded {:?}", child,),
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
                    Lang::F64trunc(_) => {
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
                        newfunc.instruction(&Instruction::F32Neq);
                    }
                    Lang::F64Ne(_) => {
                        newfunc.instruction(&Instruction::F64Neq);
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
                }
            }
        }
    }
    Ok(())
}
