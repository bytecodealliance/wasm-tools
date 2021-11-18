//! Function to construct expressions
use std::{cell::RefCell, collections::HashMap};

use crate::mutators::peephole::eggsy::encoder::TraversalEvent;
use crate::mutators::peephole::Lang;
use egg::{Id, RecExpr};

/// Build RecExpr from tree information
///
pub fn build_expr(root: Id, id_to_node: &[Lang], operands: &[Vec<Id>]) -> RecExpr<Lang> {
    let mut expr = RecExpr::default();
    build_expr_inner(root, id_to_node, operands, &mut expr);
    expr
}

pub(crate) fn build_expr_inner(
    root: Id,
    id_to_node: &[Lang],
    operands: &[Vec<Id>],
    expr: &mut RecExpr<Lang>,
) -> Id {
    // A map from the `Id`s we assigned to each sub-expression when extracting a
    // random expression to the `Id`s assigned to each sub-expression by the
    // `RecExpr`.
    let mut node_to_id: HashMap<Id, Id> = Default::default();

    let mut to_visit = vec![(TraversalEvent::Exit, root), (TraversalEvent::Enter, root)];
    while let Some((event, node)) = to_visit.pop() {
        match event {
            TraversalEvent::Enter => {
                let start_children = to_visit.len();

                for child in operands[usize::from(node)].iter().copied() {
                    to_visit.push((TraversalEvent::Enter, child));
                    to_visit.push((TraversalEvent::Exit, child));
                }

                // Reverse to make it so that we visit children in order
                // (e.g. operands are visited in order).
                to_visit[start_children..].reverse();
            }
            TraversalEvent::Exit => {
                let operands = &operands[usize::from(node)];
                let operand = |i| node_to_id[&operands[i]];
                let term = &id_to_node[usize::from(node)];
                let sub_expr_id = match term {
                    Lang::I32Add(_) => expr.add(Lang::I32Add([operand(0), operand(1)])),
                    Lang::I64Add(_) => expr.add(Lang::I64Add([operand(0), operand(1)])),
                    Lang::I32Sub(_) => expr.add(Lang::I32Sub([operand(0), operand(1)])),
                    Lang::I64Sub(_) => expr.add(Lang::I64Sub([operand(0), operand(1)])),
                    Lang::I32Mul(_) => expr.add(Lang::I32Mul([operand(0), operand(1)])),
                    Lang::I64Mul(_) => expr.add(Lang::I64Mul([operand(0), operand(1)])),
                    Lang::I32And(_) => expr.add(Lang::I32And([operand(0), operand(1)])),
                    Lang::I64And(_) => expr.add(Lang::I64And([operand(0), operand(1)])),
                    Lang::I32Or(_) => expr.add(Lang::I32Or([operand(0), operand(1)])),
                    Lang::I64Or(_) => expr.add(Lang::I64Or([operand(0), operand(1)])),
                    Lang::I32Xor(_) => expr.add(Lang::I32Xor([operand(0), operand(1)])),
                    Lang::I64Xor(_) => expr.add(Lang::I64Xor([operand(0), operand(1)])),
                    Lang::I32Shl(_) => expr.add(Lang::I32Shl([operand(0), operand(1)])),
                    Lang::I64Shl(_) => expr.add(Lang::I64Shl([operand(0), operand(1)])),
                    Lang::I32ShrU(_) => expr.add(Lang::I32ShrU([operand(0), operand(1)])),
                    Lang::I64ShrU(_) => expr.add(Lang::I64ShrU([operand(0), operand(1)])),
                    Lang::I32ShrS(_) => expr.add(Lang::I32ShrS([operand(0), operand(1)])),
                    Lang::I64ShrS(_) => expr.add(Lang::I64ShrS([operand(0), operand(1)])),
                    Lang::I32DivS(_) => expr.add(Lang::I32DivS([operand(0), operand(1)])),
                    Lang::I64DivS(_) => expr.add(Lang::I64DivS([operand(0), operand(1)])),
                    Lang::I32DivU(_) => expr.add(Lang::I32DivU([operand(0), operand(1)])),
                    Lang::I64DivU(_) => expr.add(Lang::I64DivU([operand(0), operand(1)])),
                    Lang::I32Eqz(_) => expr.add(Lang::I32Eqz([operand(0)])),
                    Lang::I64Eqz(_) => expr.add(Lang::I64Eqz([operand(0)])),
                    Lang::I32Eq(_) => expr.add(Lang::I32Eq([operand(0), operand(1)])),
                    Lang::I64Eq(_) => expr.add(Lang::I64Eq([operand(0), operand(1)])),
                    Lang::I32Ne(_) => expr.add(Lang::I32Ne([operand(0), operand(1)])),
                    Lang::I64Ne(_) => expr.add(Lang::I64Ne([operand(0), operand(1)])),
                    Lang::I32LtS(_) => expr.add(Lang::I32LtS([operand(0), operand(1)])),
                    Lang::I64LtS(_) => expr.add(Lang::I64LtS([operand(0), operand(1)])),
                    Lang::I32LtU(_) => expr.add(Lang::I32LtU([operand(0), operand(1)])),
                    Lang::I64LtU(_) => expr.add(Lang::I64LtU([operand(0), operand(1)])),
                    Lang::I32GtS(_) => expr.add(Lang::I32GtS([operand(0), operand(1)])),
                    Lang::I64GtS(_) => expr.add(Lang::I64GtS([operand(0), operand(1)])),
                    Lang::I32GtU(_) => expr.add(Lang::I32GtU([operand(0), operand(1)])),
                    Lang::I64GtU(_) => expr.add(Lang::I64GtU([operand(0), operand(1)])),
                    Lang::I32LeS(_) => expr.add(Lang::I32LeS([operand(0), operand(1)])),
                    Lang::I64LeS(_) => expr.add(Lang::I64LeS([operand(0), operand(1)])),
                    Lang::I32LeU(_) => expr.add(Lang::I32LeU([operand(0), operand(1)])),
                    Lang::I64LeU(_) => expr.add(Lang::I64LeU([operand(0), operand(1)])),
                    Lang::I32GeS(_) => expr.add(Lang::I32GeS([operand(0), operand(1)])),
                    Lang::I64GeS(_) => expr.add(Lang::I64GeS([operand(0), operand(1)])),
                    Lang::I32GeU(_) => expr.add(Lang::I32GeU([operand(0), operand(1)])),
                    Lang::I64GeU(_) => expr.add(Lang::I64GeU([operand(0), operand(1)])),
                    Lang::I32RotL(_) => expr.add(Lang::I32RotL([operand(0), operand(1)])),
                    Lang::I64RotL(_) => expr.add(Lang::I64RotL([operand(0), operand(1)])),
                    Lang::I32RotR(_) => expr.add(Lang::I32RotR([operand(0), operand(1)])),
                    Lang::I64RotR(_) => expr.add(Lang::I64RotR([operand(0), operand(1)])),
                    Lang::I32RemU(_) => expr.add(Lang::I32RemU([operand(0), operand(1)])),
                    Lang::I64RemU(_) => expr.add(Lang::I64RemU([operand(0), operand(1)])),
                    Lang::I32RemS(_) => expr.add(Lang::I32RemS([operand(0), operand(1)])),
                    Lang::I64RemS(_) => expr.add(Lang::I64RemS([operand(0), operand(1)])),
                    Lang::F32Add(_) => expr.add(Lang::F32Add([operand(0), operand(1)])),
                    Lang::F64Add(_) => expr.add(Lang::F64Add([operand(0), operand(1)])),
                    Lang::F32Sub(_) => expr.add(Lang::F32Sub([operand(0), operand(1)])),
                    Lang::F64Sub(_) => expr.add(Lang::F64Sub([operand(0), operand(1)])),
                    Lang::F32Mul(_) => expr.add(Lang::F32Mul([operand(0), operand(1)])),
                    Lang::F64Mul(_) => expr.add(Lang::F64Mul([operand(0), operand(1)])),
                    Lang::F32Div(_) => expr.add(Lang::F32Div([operand(0), operand(1)])),
                    Lang::F64Div(_) => expr.add(Lang::F64Div([operand(0), operand(1)])),
                    Lang::F32Min(_) => expr.add(Lang::F32Min([operand(0), operand(1)])),
                    Lang::F64Min(_) => expr.add(Lang::F64Min([operand(0), operand(1)])),
                    Lang::F32Max(_) => expr.add(Lang::F32Max([operand(0), operand(1)])),
                    Lang::F64Max(_) => expr.add(Lang::F64Max([operand(0), operand(1)])),
                    Lang::F32Copysign(_) => expr.add(Lang::F32Copysign([operand(0), operand(1)])),
                    Lang::F64Copysign(_) => expr.add(Lang::F64Copysign([operand(0), operand(1)])),
                    Lang::F32Eq(_) => expr.add(Lang::F32Eq([operand(0), operand(1)])),
                    Lang::F64Eq(_) => expr.add(Lang::F64Eq([operand(0), operand(1)])),
                    Lang::F32Ne(_) => expr.add(Lang::F32Ne([operand(0), operand(1)])),
                    Lang::F64Ne(_) => expr.add(Lang::F64Ne([operand(0), operand(1)])),
                    Lang::F32Lt(_) => expr.add(Lang::F32Lt([operand(0), operand(1)])),
                    Lang::F64Lt(_) => expr.add(Lang::F64Lt([operand(0), operand(1)])),
                    Lang::F32Gt(_) => expr.add(Lang::F32Gt([operand(0), operand(1)])),
                    Lang::F64Gt(_) => expr.add(Lang::F64Gt([operand(0), operand(1)])),
                    Lang::F32Le(_) => expr.add(Lang::F32Le([operand(0), operand(1)])),
                    Lang::F64Le(_) => expr.add(Lang::F64Le([operand(0), operand(1)])),
                    Lang::F32Ge(_) => expr.add(Lang::F32Ge([operand(0), operand(1)])),
                    Lang::F64Ge(_) => expr.add(Lang::F64Ge([operand(0), operand(1)])),
                    Lang::Wrap(_) => expr.add(Lang::Wrap([operand(0)])),
                    Lang::I32Popcnt(_) => expr.add(Lang::I32Popcnt([operand(0)])),
                    Lang::I64Popcnt(_) => expr.add(Lang::I64Popcnt([operand(0)])),
                    Lang::Call(id, op) => expr.add(Lang::Call(
                        *id,
                        (0..op.len()).map(operand).collect::<Vec<Id>>(),
                    )),

                    Lang::Container(op) => expr.add(Lang::Container(
                        (0..op.len()).map(operand).collect::<Vec<Id>>(),
                    )),
                    Lang::LocalTee(idx, _) => expr.add(Lang::LocalTee(*idx, operand(0))),
                    Lang::UnfoldI32(_) => expr.add(Lang::UnfoldI32(operand(0))),
                    Lang::UnfoldI64(_) => expr.add(Lang::UnfoldI64(operand(0))),
                    Lang::I32Load {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I32Load {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I64Load {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I64Load {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::F32Load {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::F32Load {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::F64Load {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::F64Load {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I32Load8S {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I32Load8S {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I32Load8U {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I32Load8U {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I32Load16S {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I32Load16S {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I32Load16U {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I32Load16U {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I64Load8S {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I64Load8S {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I64Load8U {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I64Load8U {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I64Load16S {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I64Load16S {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I64Load16U {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I64Load16U {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I64Load32S {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I64Load32S {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I64Load32U {
                        static_offset,
                        align,
                        mem,
                        offset: _,
                    } => expr.add(Lang::I64Load32U {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        offset: operand(0),
                    }),
                    Lang::I32Extend8S(_) => expr.add(Lang::I32Extend8S([operand(0)])),
                    Lang::I64Extend8S(_) => expr.add(Lang::I64Extend8S([operand(0)])),
                    Lang::I32Extend16S(_) => expr.add(Lang::I32Extend16S([operand(0)])),
                    Lang::I64Extend16S(_) => expr.add(Lang::I64Extend16S([operand(0)])),
                    Lang::I64Extend32S(_) => expr.add(Lang::I64Extend32S([operand(0)])),
                    Lang::I64ExtendI32S(_) => expr.add(Lang::I64ExtendI32S([operand(0)])),
                    Lang::I64ExtendI32U(_) => expr.add(Lang::I64ExtendI32U([operand(0)])),
                    Lang::I32Clz(_) => expr.add(Lang::I32Clz([operand(0)])),
                    Lang::I32Ctz(_) => expr.add(Lang::I32Ctz([operand(0)])),
                    Lang::I64Ctz(_) => expr.add(Lang::I64Ctz([operand(0)])),
                    Lang::I64Clz(_) => expr.add(Lang::I64Clz([operand(0)])),
                    Lang::F32Abs(_) => expr.add(Lang::F32Abs([operand(0)])),
                    Lang::F64Abs(_) => expr.add(Lang::F64Abs([operand(0)])),
                    Lang::F32Neg(_) => expr.add(Lang::F32Neg([operand(0)])),
                    Lang::F64Neg(_) => expr.add(Lang::F64Neg([operand(0)])),
                    Lang::F32Sqrt(_) => expr.add(Lang::F32Sqrt([operand(0)])),
                    Lang::F64Sqrt(_) => expr.add(Lang::F64Sqrt([operand(0)])),
                    Lang::F32Ceil(_) => expr.add(Lang::F32Ceil([operand(0)])),
                    Lang::F64Ceil(_) => expr.add(Lang::F64Ceil([operand(0)])),
                    Lang::F32Floor(_) => expr.add(Lang::F32Floor([operand(0)])),
                    Lang::F64Floor(_) => expr.add(Lang::F64Floor([operand(0)])),
                    Lang::F32Trunc(_) => expr.add(Lang::F32Trunc([operand(0)])),
                    Lang::F64trunc(_) => expr.add(Lang::F64trunc([operand(0)])),
                    Lang::F32Nearest(_) => expr.add(Lang::F32Nearest([operand(0)])),
                    Lang::F64Nearest(_) => expr.add(Lang::F64Nearest([operand(0)])),
                    Lang::I32TruncF32S(_) => expr.add(Lang::I32TruncF32S([operand(0)])),
                    Lang::I32TruncF32U(_) => expr.add(Lang::I32TruncF32U([operand(0)])),
                    Lang::I32TruncF64S(_) => expr.add(Lang::I32TruncF64S([operand(0)])),
                    Lang::I32TruncF64U(_) => expr.add(Lang::I32TruncF64U([operand(0)])),
                    Lang::I64TruncF32S(_) => expr.add(Lang::I64TruncF32S([operand(0)])),
                    Lang::I64TruncF32U(_) => expr.add(Lang::I64TruncF32U([operand(0)])),
                    Lang::I64TruncF64S(_) => expr.add(Lang::I64TruncF64S([operand(0)])),
                    Lang::I64TruncF64U(_) => expr.add(Lang::I64TruncF64U([operand(0)])),
                    Lang::F32ConvertI32S(_) => expr.add(Lang::F32ConvertI32S([operand(0)])),
                    Lang::F32ConvertI32U(_) => expr.add(Lang::F32ConvertI32U([operand(0)])),
                    Lang::F32ConvertI64S(_) => expr.add(Lang::F32ConvertI64S([operand(0)])),
                    Lang::F32ConvertI64U(_) => expr.add(Lang::F32ConvertI64U([operand(0)])),
                    Lang::F32DemoteF64(_) => expr.add(Lang::F32DemoteF64([operand(0)])),
                    Lang::F64ConvertI32S(_) => expr.add(Lang::F64ConvertI32S([operand(0)])),
                    Lang::F64ConvertI32U(_) => expr.add(Lang::F64ConvertI32U([operand(0)])),
                    Lang::F64ConvertI64S(_) => expr.add(Lang::F64ConvertI64S([operand(0)])),
                    Lang::F64ConvertI64U(_) => expr.add(Lang::F64ConvertI64U([operand(0)])),
                    Lang::F64PromoteF32(_) => expr.add(Lang::F64PromoteF32([operand(0)])),
                    Lang::I32ReinterpretF32(_) => expr.add(Lang::I32ReinterpretF32([operand(0)])),
                    Lang::I64ReinterpretF64(_) => expr.add(Lang::I64ReinterpretF64([operand(0)])),
                    Lang::F32ReinterpretI32(_) => expr.add(Lang::F32ReinterpretI32([operand(0)])),
                    Lang::F64ReinterpretI64(_) => expr.add(Lang::F64ReinterpretI64([operand(0)])),
                    Lang::I32TruncSatF32S(_) => expr.add(Lang::I32TruncSatF32S([operand(0)])),
                    Lang::I32TruncSatF32U(_) => expr.add(Lang::I32TruncSatF32U([operand(0)])),
                    Lang::I32TruncSatF64S(_) => expr.add(Lang::I32TruncSatF64S([operand(0)])),
                    Lang::I32TruncSatF64U(_) => expr.add(Lang::I32TruncSatF64U([operand(0)])),
                    Lang::I64TruncSatF32S(_) => expr.add(Lang::I64TruncSatF32S([operand(0)])),
                    Lang::I64TruncSatF32U(_) => expr.add(Lang::I64TruncSatF32U([operand(0)])),
                    Lang::I64TruncSatF64S(_) => expr.add(Lang::I64TruncSatF64S([operand(0)])),
                    Lang::I64TruncSatF64U(_) => expr.add(Lang::I64TruncSatF64U([operand(0)])),
                    Lang::Drop(_) => expr.add(Lang::Drop([operand(0)])),
                    Lang::GlobalSet(idx, _) => expr.add(Lang::GlobalSet(*idx, operand(0))),
                    Lang::LocalGet(idx) => expr.add(Lang::LocalGet(*idx)),
                    Lang::GlobalGet(idx) => expr.add(Lang::GlobalGet(*idx)),
                    Lang::LocalSet(idx, _) => expr.add(Lang::LocalSet(*idx, operand(0))),
                    Lang::I64Store {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::I64Store {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    Lang::I32Store {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::I32Store {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    Lang::F32Store {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::F32Store {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    Lang::F64Store {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::F64Store {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    Lang::I32Store8 {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::I32Store8 {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    Lang::I32Store16 {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::I32Store16 {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    Lang::I64Store8 {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::I64Store8 {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    Lang::I64Store16 {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::I64Store16 {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    Lang::I64Store32 {
                        static_offset,
                        align,
                        mem,
                        value_and_offset: _,
                    } => expr.add(Lang::I64Store32 {
                        static_offset: *static_offset,
                        align: *align,
                        mem: *mem,
                        value_and_offset: [operand(0), operand(1)],
                    }),
                    i32 @ Lang::I32(_) => expr.add((*i32).clone()),
                    i64 @ Lang::I64(_) => expr.add((*i64).clone()),
                    f32 @ Lang::F32(_) => expr.add((*f32).clone()),
                    f64 @ Lang::F64(_) => expr.add((*f64).clone()),
                    s @ Lang::RandI32 => expr.add((*s).clone()),
                    s @ Lang::RandI64 => expr.add((*s).clone()),
                    u @ Lang::Undef => expr.add((*u).clone()),
                    n @ Lang::Nop => expr.add((*n).clone()),
                };
                // Copy the id to stack entries to a new one
                let old_entry = node_to_id.insert(node, sub_expr_id);
                assert!(old_entry.is_none());
            }
        }
    }
    Id::from(expr.as_ref().len() - 1)
}
