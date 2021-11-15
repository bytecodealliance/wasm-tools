//! Function to construct expressions
use std::collections::HashMap;

use crate::mutators::peephole::eggsy::encoder::TraversalEvent;
use crate::mutators::peephole::Lang;
use egg::{Id, RecExpr};

/// Build RecExpr from tree information
pub fn build_expr(root: Id, id_to_node: &[Lang], operands: &[Vec<Id>]) -> RecExpr<Lang> {
    let mut expr = RecExpr::default();
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
                    Lang::Wrap(_) => expr.add(Lang::Wrap([operand(0)])),
                    Lang::I32Popcnt(_) => expr.add(Lang::I32Popcnt([operand(0)])),
                    Lang::I64Popcnt(_) => expr.add(Lang::I64Popcnt([operand(0)])),
                    Lang::Call(op) => {
                        expr.add(Lang::Call((0..op.len()).map(operand).collect::<Vec<Id>>()))
                    }
                    Lang::LocalTee(_) => expr.add(Lang::LocalTee([operand(0), operand(1)])),
                    Lang::UnfoldI32(_) => expr.add(Lang::UnfoldI32(operand(0))),
                    Lang::UnfoldI64(_) => expr.add(Lang::UnfoldI64(operand(0))),
                    Lang::I32Load(_) => expr.add(Lang::I32Load([
                        operand(0),
                        operand(1),
                        operand(2),
                        operand(3),
                    ])),
                    Lang::I64Load(_) => expr.add(Lang::I64Load([
                        operand(0),
                        operand(1),
                        operand(2),
                        operand(3),
                    ])),
                    Lang::I32Extend8S(_) => expr.add(Lang::I32Extend8S([operand(0)])),
                    Lang::I64Extend8S(_) => expr.add(Lang::I64Extend8S([operand(0)])),
                    Lang::I32Extend16S(_) => expr.add(Lang::I32Extend16S([operand(0)])),
                    Lang::I64Extend16S(_) => expr.add(Lang::I64Extend16S([operand(0)])),
                    Lang::I64Extend32S(_) => expr.add(Lang::I64Extend32S([operand(0)])),
                    Lang::I64ExtendI32S(_) => expr.add(Lang::I64ExtendI32S([operand(0)])),
                    Lang::I64ExtendI32U(_) => expr.add(Lang::I64ExtendI32U([operand(0)])),
                    Lang::Drop(_) => expr.add(Lang::Drop([operand(0)])),
                    Lang::GlobalSet(_) => expr.add(Lang::GlobalSet([operand(0), operand(1)])),
                    Lang::LocalGet(_) => expr.add(Lang::LocalGet([operand(0)])),
                    Lang::GlobalGet(_) => expr.add(Lang::GlobalGet([operand(0)])),
                    Lang::LocalSet(_) => expr.add(Lang::LocalSet([operand(0), operand(1)])),
                    Lang::I64Store(_) => expr.add(Lang::I64Store([
                        operand(0),
                        operand(1),
                        operand(2),
                        operand(3),
                        operand(4),
                    ])),
                    Lang::I32Store(_) => expr.add(Lang::I32Store([
                        operand(0),
                        operand(1),
                        operand(2),
                        operand(3),
                        operand(4),
                    ])),
                    i32 @ Lang::I32(_) => expr.add((*i32).clone()),
                    i64 @ Lang::I64(_) => expr.add((*i64).clone()),
                    s @ Lang::RandI32 => expr.add((*s).clone()),
                    s @ Lang::RandI64 => expr.add((*s).clone()),
                    u @ Lang::Undef => expr.add((*u).clone()),
                    a @ Lang::Arg(_) => expr.add((*a).clone()),
                    c @ Lang::Const(_) => expr.add((*c).clone()),
                };
                // Copy the id to stack entries to a new one
                let old_entry = node_to_id.insert(node, sub_expr_id);
                assert!(old_entry.is_none());
            }
        }
    }
    expr
}
