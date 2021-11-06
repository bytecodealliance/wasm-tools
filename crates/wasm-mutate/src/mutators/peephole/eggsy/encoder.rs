//! Helper methods for encoding eterm expressions to Wasm and back
use std::cell::{Cell, RefCell};
use std::{collections::HashMap, num::Wrapping};

use egg::{Id, RecExpr};
use rand::Rng;
use wasm_encoder::{Function, Instruction, MemArg};
use wasmparser::Operator;

use crate::module::PrimitiveTypeInfo;
use crate::mutators::peephole::dfg::StackType;
use crate::mutators::peephole::{Lang, EG};
use crate::{
    error::EitherType,
    mutators::peephole::{
        dfg::{BBlock, MiniDFG, StackEntry},
        OperatorAndByteOffset,
    },
    ModuleInfo,
};

/// Turns wasm to eterm and back
pub struct Encoder;

/// Traversing node events
enum TraversalEvent {
    Enter,
    Exit,
}

impl Encoder {
    /// Infers types for the rewriting egraph tree.
    ///
    /// During the translation from Wasm to the egraph some type information is missing.
    /// However, it can be inferred by the type relations between the nodes, for example
    /// for `i32.wrap_i64` operator, we always know that it returns an i32 constant and waits
    /// for the operand to be an i64 integer. This information could be propagated to the other
    /// nodes in the tree. This function does exactly that, it fixes some nodes to expect
    /// and receive specific types using fixed operators and function signatures. The type tree
    /// is traversed until is converges, if for some reason a node has not a type (when it should)
    /// this type is gathered from the egraph eclass data.
    fn infer_types(
        info: &ModuleInfo,
        expr: &RecExpr<Lang>,
        node_to_eclass: &[Id],
        egraph: &EG,
    ) -> crate::Result<Vec<Option<PrimitiveTypeInfo>>> {
        let nodes = expr.as_ref();
        let gettpe = |id: Id| {
            let eclass = node_to_eclass[usize::from(id)];
            let data = &egraph[eclass].data;

            let data = data.as_ref()?;
            let entry = &data.get_next_stack_entry(&egraph.analysis);
            Some(entry.return_type.clone())
        };

        let types = nodes
            .iter()
            .map(|_| None)
            .collect::<Vec<Option<PrimitiveTypeInfo>>>();
        let types = RefCell::new(types);
        let changed = false;
        let changed = Cell::new(changed);

        let get = |index: usize| {
            let types = types.borrow();
            types[index].clone()
        };

        let update = |index, ty: Option<PrimitiveTypeInfo>| {
            let mut types = types.borrow_mut();
            let entry: &Option<PrimitiveTypeInfo> = &types[index];
            if entry.is_none() && ty.is_some() {
                types[index] = ty;
                changed.set(true);
            } else {
                assert_eq!(*entry, ty);
            }
        };
        // First pass set type for known nodes (we know the type returning of the operator and the operand)
        // functions, wrap, loads, symbols and extend returns
        for (idx, node) in nodes.iter().enumerate() {
            match node {
                Lang::Wrap(operands) => {
                    update(idx, Some(PrimitiveTypeInfo::I32));
                    update(usize::from(operands[0]), Some(PrimitiveTypeInfo::I64));
                }
                Lang::ILoad(operands) => {
                    update(idx, gettpe(Id::from(idx)));
                    update(usize::from(operands[0]), Some(PrimitiveTypeInfo::I32));
                }
                Lang::Symbol(s) => {
                    let entry = egraph
                        .analysis
                        .get_stack_entry_from_symbol(s.to_string())
                        .ok_or(crate::Error::UnsupportedType(EitherType::EggError(
                            "The current symbol cannot be retrieved".into(),
                        )))
                        .unwrap();

                    update(idx, Some(entry.return_type.clone()));
                }
                Lang::Extend8S(_) => {
                    update(idx, gettpe(Id::from(idx)));
                }
                Lang::Extend16S(_) => {
                    update(idx, gettpe(Id::from(idx)));
                }
                Lang::Call(operands) => {
                    let first = operands[0];
                    let firstnode = &nodes[usize::from(first)];
                    let functionindex = match firstnode {
                        Lang::Arg(val) => {
                            *val as u32
                        }
                        Lang::Num(val) => {
                            *val as u32
                        }
                        _ => unreachable!("The first argument for Call nodes should be an inmmediate node type (Arg)")
                    };
                    let typeinfo = info.get_functype_idx(functionindex as usize);
                    if let crate::module::TypeInfo::Func(tpe) = typeinfo {
                        // Set the type for the operands
                        for (idx, operand) in operands[1..].iter().enumerate() {
                            update(usize::from(*operand), Some(tpe.params[idx].clone()))
                        }

                        // set return type of the Call
                        // Set the first operands only, mltiple return values
                        // is not yet allowed by wasm-mutate
                        update(idx, Some(tpe.returns[0].clone()));
                    }
                }
                Lang::Extend32S(operands) => {
                    update(idx, Some(PrimitiveTypeInfo::I64));
                    update(usize::from(operands[0]), Some(PrimitiveTypeInfo::I64));
                }
                Lang::ExtendI32S(operands) | Lang::ExtendI32U(operands) => {
                    update(idx, Some(PrimitiveTypeInfo::I64));
                    update(usize::from(operands[0]), Some(PrimitiveTypeInfo::I32));
                }
                Lang::Tee(operands) => {
                    let child = get(usize::from(operands[0])).clone();
                    update(idx, child);
                }
                Lang::Eqz(_)
                | Lang::LtS(_)
                | Lang::LtU(_)
                | Lang::GtS(_)
                | Lang::GtU(_)
                | Lang::LeS(_)
                | Lang::LeU(_)
                | Lang::GeS(_)
                | Lang::Eq(_)
                | Lang::Ne(_)
                | Lang::GeU(_) => {
                    // It always return i32
                    update(idx, Some(PrimitiveTypeInfo::I32));
                }
                Lang::Drop => {
                    update(idx, Some(PrimitiveTypeInfo::Empty));
                }
                _ => {}
            }
        }

        // Second pass for hard operators (operator and operands have all the same type)
        for (idx, node) in nodes.iter().enumerate() {
            match node {
                Lang::Sub(operands)
                | Lang::Mul(operands)
                | Lang::And(operands)
                | Lang::Or(operands)
                | Lang::Xor(operands)
                | Lang::ShrU(operands)
                | Lang::DivU(operands)
                | Lang::DivS(operands)
                | Lang::ShrS(operands)
                | Lang::RotR(operands)
                | Lang::RotL(operands)
                | Lang::RemS(operands)
                | Lang::RemU(operands)
                | Lang::Shl(operands)
                | Lang::Add(operands) => {
                    update(
                        idx,
                        get(idx).or(get(usize::from(operands[0]).clone())
                            .or(get(usize::from(operands[1])).clone())
                            .or(gettpe(Id::from(idx)))),
                    );
                    // Set the type for the children
                    update(usize::from(operands[0]), get(idx));
                    update(usize::from(operands[1]), get(idx));
                }

                _ => {
                    // Do nothing
                }
            }
        }
        // Last pass, remaining nodes
        loop {
            // reset the fixed point state
            changed.set(false);

            for (idx, node) in nodes.iter().enumerate() {
                match node {
                    Lang::Sub(operands)
                    | Lang::Mul(operands)
                    | Lang::And(operands)
                    | Lang::Or(operands)
                    | Lang::Xor(operands)
                    | Lang::ShrU(operands)
                    | Lang::DivU(operands)
                    | Lang::DivS(operands)
                    | Lang::ShrS(operands)
                    | Lang::RotR(operands)
                    | Lang::RotL(operands)
                    | Lang::RemS(operands)
                    | Lang::RemU(operands)
                    | Lang::Shl(operands)
                    | Lang::Add(operands) => {
                        update(
                            idx,
                            get(idx)
                                .or(get(usize::from(operands[0])))
                                .or(get(usize::from(operands[1])))
                                .or(gettpe(Id::from(idx))),
                        );
                        update(usize::from(operands[0]), get(idx));
                        update(usize::from(operands[1]), get(idx));
                    }
                    Lang::Wrap(operands) => {
                        update(idx, Some(PrimitiveTypeInfo::I32));
                        update(usize::from(operands[0]), Some(PrimitiveTypeInfo::I64))
                    }
                    Lang::ILoad(operands) => {
                        update(idx, gettpe(Id::from(idx)));
                        update(usize::from(operands[0]), Some(PrimitiveTypeInfo::I32))
                    }
                    Lang::Symbol(s) => {
                        let entry = egraph
                            .analysis
                            .get_stack_entry_from_symbol(s.to_string())
                            .ok_or(crate::Error::UnsupportedType(EitherType::EggError(
                                "The current symbol cannot be retrieved".into(),
                            )))
                            .unwrap();

                        update(idx, Some(entry.return_type.clone()));
                    }
                    Lang::Eqz(operands) => {
                        let encoding_tpe = get(usize::from(operands[0]));
                        // Set the type for the children
                        update(
                            usize::from(operands[0]),
                            get(usize::from(operands[0])).or(encoding_tpe),
                        );
                    }
                    Lang::Tee(operands) => {
                        let encoding_tpe = get(usize::from(operands[0])).or(gettpe(Id::from(idx)));
                        update(
                            usize::from(operands[0]),
                            get(usize::from(operands[0])).or(encoding_tpe),
                        );
                    }
                    Lang::LtS(operands)
                    | Lang::LtU(operands)
                    | Lang::GtS(operands)
                    | Lang::GtU(operands)
                    | Lang::LeS(operands)
                    | Lang::LeU(operands)
                    | Lang::GeS(operands)
                    | Lang::Eq(operands)
                    | Lang::Ne(operands)
                    | Lang::GeU(operands) => {
                        let encoding_tpe =
                            get(usize::from(operands[0])).or(get(usize::from(operands[1]))); // Last from collected info during Wasm2eterm translation
                                                                                             // Set the type for the children
                        update(
                            usize::from(operands[1]),
                            get(usize::from(operands[1])).or(encoding_tpe.clone()),
                        );
                        update(
                            usize::from(operands[0]),
                            get(usize::from(operands[0])).or(encoding_tpe.clone()),
                        );
                    }
                    Lang::Popcnt(operands) => {
                        // if its not inferred from the parent, then is the type of the children
                        update(
                            idx,
                            get(idx)
                                .clone()
                                .or(get(usize::from(operands[0])))
                                .or(gettpe(Id::from(idx))),
                        );
                    }
                    Lang::Unfold(operand) => {
                        // if its not inferred from the parent, then is the type of the children
                        update(
                            idx,
                            get(idx)
                                .or(get(usize::from(*operand)))
                                .or(gettpe(Id::from(idx))),
                        );
                    }
                    Lang::Num(_) => {
                        // If it is not set, then take the information from the egraph
                        update(idx, get(idx).or(gettpe(Id::from(idx))));
                    }
                    Lang::Arg(_)
                    | Lang::Drop
                    | Lang::Rand
                    | Lang::Undef
                    | Lang::Extend8S(_)
                    | Lang::Extend16S(_)
                    | Lang::Extend32S(_)
                    | Lang::ExtendI32S(_)
                    | Lang::ExtendI32U(_)
                    | Lang::Call(_) => {}
                }
            }
            if !changed.get() {
                break;
            }
        }

        let r = &*types.borrow();
        Ok(r.clone())
    }

    pub(crate) fn expr2wasm(
        info: &ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        expr: &RecExpr<Lang>,
        node_to_eclass: &[Id],
        newfunc: &mut Function,
        egraph: &EG,
    ) -> crate::Result<()> {
        let nodes = expr.as_ref();

        let types = Encoder::infer_types(info, expr, node_to_eclass, egraph)?;

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
        // Enqueue the coding back nodes and infer types
        while let Some(context) = worklist.pop() {
            let rootlang = &nodes[usize::from(context.current_node)];

            match context.traversal_event {
                TraversalEvent::Enter => {
                    // Push children
                    match rootlang {
                        Lang::Add(operands)
                        | Lang::Shl(operands)
                        | Lang::ShrU(operands)
                        | Lang::Sub(operands)
                        | Lang::Mul(operands)
                        | Lang::And(operands)
                        | Lang::Or(operands)
                        | Lang::Xor(operands)
                        | Lang::ShrS(operands)
                        | Lang::DivS(operands)
                        | Lang::DivU(operands)
                        | Lang::RotR(operands)
                        | Lang::RotL(operands)
                        | Lang::RemS(operands)
                        | Lang::RemU(operands) => {
                            let operands = *operands;
                            for operand in operands.iter().rev() {
                                worklist.push(Context::new(*operand, TraversalEvent::Exit));
                                worklist.push(Context::new(*operand, TraversalEvent::Enter));
                            }
                        }
                        Lang::Eq(operands)
                        | Lang::Ne(operands)
                        | Lang::LtS(operands)
                        | Lang::LtU(operands)
                        | Lang::GtS(operands)
                        | Lang::GtU(operands)
                        | Lang::LeS(operands)
                        | Lang::LeU(operands)
                        | Lang::GeS(operands)
                        | Lang::GeU(operands) => {
                            let operands = *operands;
                            for operand in operands.iter().rev() {
                                // The type is one of the siblings
                                worklist.push(Context::new(*operand, TraversalEvent::Exit));
                                worklist.push(Context::new(*operand, TraversalEvent::Enter));
                            }
                        }
                        Lang::Popcnt(operands)
                        | Lang::Extend8S(operands)
                        | Lang::Extend16S(operands)
                        | Lang::Extend32S(operands)
                        | Lang::ExtendI32S(operands)
                        | Lang::ExtendI32U(operands)
                        | Lang::Tee(operands)
                        | Lang::Wrap(operands)
                        | Lang::Eqz(operands) => {
                            worklist.push(Context::new(operands[0], TraversalEvent::Exit));
                            worklist.push(Context::new(operands[0], TraversalEvent::Enter));
                        }
                        Lang::Call(operands) => {
                            // The first operand is always the helper Arg to identify the function
                            for operand in operands.iter().skip(1).rev() {
                                worklist.push(Context::new(*operand, TraversalEvent::Exit));
                                worklist.push(Context::new(*operand, TraversalEvent::Enter));
                            }
                        }
                        Lang::ILoad(operands) => {
                            // Only push the first argument, remaining are helpers
                            worklist.push(Context::new(operands[0], TraversalEvent::Exit));
                            worklist.push(Context::new(operands[0], TraversalEvent::Enter));
                        }
                        _ => { /* Do nothing */ }
                    }
                }
                TraversalEvent::Exit => {
                    let tpe = types[usize::from(context.current_node)].as_ref();
                    match rootlang {
                        Lang::Add(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Add);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Add);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Shl(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Shl);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Shl);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::ShrU(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32ShrU);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64ShrU);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Sub(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Sub);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Sub);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Mul(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Mul);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Mul);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::And(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32And);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64And);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Or(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Or);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Or);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Xor(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Xor);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Xor);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::ShrS(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32ShrS);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64ShrS);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::DivS(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32DivS);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64DivS);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::DivU(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32DivU);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64DivU);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::RotR(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Rotr);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Rotr);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::RotL(_operands) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Rotl);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Rotl);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::RemS(_) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32RemS);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64RemS);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::RemU(_) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32RemU);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64RemU);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Eqz(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32Eqz);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64Eqz);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::Eq(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32Eq);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64Eq);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::Ne(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32Neq);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64Neq);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::LtS(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32LtS);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64LtS);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::LtU(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32LtU);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64LtU);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::GtS(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32GtS);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64GtS);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::GtU(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32GtU);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64GtU);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::LeS(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32LeS);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64LeS);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::LeU(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32LeU);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64LeU);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::GeS(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32GeS);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64GeS);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::GeU(operands) => {
                            let tpe = types[usize::from(operands[0])]
                                .clone()
                                .or(types[usize::from(operands[1])].clone())
                                .expect("Inferred from operands");
                            match tpe {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32GeU);
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64GeU);
                                }
                                _ => unreachable!("bad type"),
                            }
                        }
                        Lang::Tee(_) => {
                            let eclass = node_to_eclass[usize::from(context.current_node)];
                            let data = &egraph[eclass].data;
                            let entry =
                                data.clone().unwrap().get_next_stack_entry(&egraph.analysis);
                            if let StackType::LocalTee(local_index) = entry.operator {
                                newfunc.instruction(&Instruction::LocalTee(local_index));
                            } else {
                                unreachable!("Incorrect mapping")
                            }
                        }
                        Lang::Wrap(_) => {
                            newfunc.instruction(&Instruction::I32WrapI64);
                        }
                        Lang::Extend8S(_) => match tpe.expect("Missing type info") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Extend8S);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Extend8S);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Extend16S(_) => match tpe.expect("Missing type info") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Extend16S);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Extend16S);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Extend32S(_) => match tpe.expect("Missing type info") {
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Extend32S);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::ExtendI32S(_) => match tpe.expect("Missing type info") {
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64ExtendI32S);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::ExtendI32U(_) => match tpe.expect("Missing type info") {
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64ExtendI32U);
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Call(operands) => {
                            let first = operands[0];
                            let firstnode = &nodes[usize::from(first)];
                            match firstnode {
                                Lang::Arg(val) => {
                                    newfunc.instruction(&Instruction::Call(*val as u32));
                                }
                                Lang::Num(val) => {
                                    newfunc.instruction(&Instruction::Call(*val as u32));
                                }
                                _ => unreachable!("The first argument for Call nodes should be an inmmediate node type (Arg)")
                            }
                        }
                        Lang::Popcnt(_) => match tpe.expect("Missing type info") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Popcnt);
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Popcnt);
                            }
                            _ => unreachable!("Type cannot be encoded"),
                        },
                        Lang::Drop => {
                            newfunc.instruction(&Instruction::Drop);
                        }
                        Lang::ILoad(operands) => {
                            let offset_operand = &nodes[usize::from(operands[1])];
                            let align_operand = &nodes[usize::from(operands[2])];
                            let memidx_operand = &nodes[usize::from(operands[3])];

                            let toarg = |op: &Lang| {
                                match op {
                                    Lang::Arg(val) => *val,
                                    Lang::Num(val) => *val as u64, // Num needs to be taken into account here because the parsing of rules is done by egg itself
                                    _ => unreachable!(
                                        "This operand should be an Arg node. Current operand {:?}",
                                        op
                                    ),
                                }
                            };

                            let offset_value = toarg(offset_operand);

                            let align_value = toarg(align_operand);

                            let memidx_value = toarg(memidx_operand);

                            let memarg = MemArg {
                                offset: offset_value, // These can be mutated as well
                                align: align_value as u32,
                                memory_index: memidx_value as u32,
                            };
                            match tpe.expect("Missing type info") {
                                PrimitiveTypeInfo::I32 => {
                                    newfunc.instruction(&Instruction::I32Load(memarg));
                                }
                                PrimitiveTypeInfo::I64 => {
                                    newfunc.instruction(&Instruction::I64Load(memarg));
                                }
                                _ => unreachable!("Type cannot be encoded"),
                            }
                        }
                        Lang::Rand => match tpe.expect("Type information missing") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Const(rnd.gen()));
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Const(rnd.gen()));
                            }
                            _ => unreachable!("Type cannot be encoded"),
                        },
                        Lang::Undef => { /* Do nothig */ }
                        Lang::Unfold(value) => {
                            let child = &nodes[usize::from(*value)];

                            match child {
                                Lang::Num(value) => {
                                    // Getting type from eclass.
                                    match tpe.expect("Type information is missing") {
                                        PrimitiveTypeInfo::I64 => {
                                            let r: i64 = rnd.gen();
                                            newfunc.instruction(&Instruction::I64Const(r));
                                            newfunc.instruction(&Instruction::I64Const(
                                                (Wrapping(*value) - Wrapping(r)).0,
                                            ));
                                            newfunc.instruction(&Instruction::I64Add);
                                        }
                                        PrimitiveTypeInfo::I32 => {
                                            let r: i32 = rnd.gen();
                                            newfunc.instruction(&Instruction::I32Const(r));
                                            newfunc.instruction(&Instruction::I32Const(
                                                (Wrapping(*value as i32) - Wrapping(r)).0,
                                            ));
                                            newfunc.instruction(&Instruction::I32Add);
                                        }
                                        _ => {
                                            return Err(crate::Error::UnsupportedType(
                                                EitherType::EggError(format!(
                                                    "The current eterm cannot be unfolded {:?}",
                                                    child,
                                                )),
                                            ))
                                        }
                                    }
                                }
                                _ => {
                                    return Err(crate::Error::UnsupportedType(
                                        EitherType::EggError(format!(
                                            "The current eterm cannot be unfolded {:?}",
                                            child,
                                        )),
                                    ))
                                }
                            }
                        }
                        Lang::Num(v) => match tpe.expect("Type information is missing") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Const(*v as i32));
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Const(*v));
                            }
                            _ => unreachable!("bad type"),
                        },
                        Lang::Symbol(s) => {
                            let entry = &egraph
                                .analysis
                                .get_stack_entry_from_symbol(s.to_string())
                                .ok_or_else(|| {
                                    crate::Error::UnsupportedType(EitherType::EggError(
                                        "The current symbol cannot be retrieved".into(),
                                    ))
                                })?;

                            match entry.operator {
                                StackType::LocalGet(idx) => {
                                    newfunc.instruction(&Instruction::LocalGet(idx));
                                }
                                StackType::GlobalGet(idx) => {
                                    newfunc.instruction(&Instruction::GlobalGet(idx));
                                }
                                _ => {
                                    return Err(crate::Error::UnsupportedType(
                                        EitherType::EggError("Incorrect stack type".into()),
                                    ))
                                }
                            }
                        }
                        Lang::Arg(val) => match tpe.expect("Type information is needed") {
                            PrimitiveTypeInfo::I32 => {
                                newfunc.instruction(&Instruction::I32Const(*val as i32));
                            }
                            PrimitiveTypeInfo::I64 => {
                                newfunc.instruction(&Instruction::I64Const(*val as i64));
                            }
                            _ => unreachable!("Type cannot be encoded"),
                        },
                    }
                }
            }
        }
        Ok(())
    }

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
                        StackType::Load {
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
                        }
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
                    Encoder::expr2wasm(info, rnd, expr, node_to_eclass, newfunc, egraph)?;
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

        // Check the returning type of the root, if it is not integer, return NoApplicablePeephole
        let root = &dfg.entries[stack_entry_index];
        match root.return_type {
            PrimitiveTypeInfo::I32 | PrimitiveTypeInfo::I64 => { /* pass */ }
            _ => return Err(crate::Error::NoMutationsApplicable),
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
                            let lang = Lang::Num(*value as i64);
                            // Place the new id in the id_stack to recover it in the Exit event
                            ids_stack.push(put_enode(lang, &mut r, entry.entry_idx, expr));
                        }
                        StackType::I64(value) => {
                            let lang = Lang::Num(*value);
                            ids_stack.push(put_enode(lang, &mut r, entry.entry_idx, expr));
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
                        StackType::LocalSet(_) => unreachable!(),
                        StackType::LocalTee(_) => {
                            let value = ids_stack.pop().expect("Ids stack should not be empty");
                            ids_stack.push(put_enode(
                                Lang::Tee([value]),
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
                        StackType::GlobalSet(_) => unreachable!(),
                        StackType::Drop => unreachable!(),
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
                        StackType::Load {
                            offset,
                            align,
                            memory,
                        } => {
                            // Write load operands
                            let offsetid = ids_stack.pop().expect("The dynamic offset operand for the load operation is not in the stack");

                            let staticoffsetoid =
                                put_enode(Lang::Arg(*offset as u64), &mut r, entry.entry_idx, expr);
                            let alignid =
                                put_enode(Lang::Arg(*align as u64), &mut r, entry.entry_idx, expr);
                            let memidxid =
                                put_enode(Lang::Arg(*memory as u64), &mut r, entry.entry_idx, expr);
                            ids_stack.push(put_enode(
                                Lang::ILoad([offsetid, staticoffsetoid, alignid, memidxid]),
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
                                Operator::I32Add | Operator::I64Add => put_enode(
                                    // Notice, children are declared in reverse order
                                    Lang::Add([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Shl | Operator::I64Shl => put_enode(
                                    Lang::Shl([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I64ShrU | Operator::I32ShrU => put_enode(
                                    Lang::ShrU([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),

                                Operator::I64ShrS | Operator::I32ShrS => put_enode(
                                    Lang::ShrS([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I64DivS | Operator::I32DivS => put_enode(
                                    Lang::DivS([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I64DivU | Operator::I32DivU => put_enode(
                                    Lang::DivU([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32And | Operator::I64And => put_enode(
                                    Lang::And([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Or | Operator::I64Or => put_enode(
                                    Lang::Or([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Xor | Operator::I64Xor => put_enode(
                                    Lang::Xor([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Sub | Operator::I64Sub => put_enode(
                                    Lang::Sub([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Mul | Operator::I64Mul => put_enode(
                                    Lang::Mul([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Eqz | Operator::I64Eqz => put_enode(
                                    Lang::Eqz([subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Eq | Operator::I64Eq => put_enode(
                                    Lang::Eq([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Ne | Operator::I64Ne => put_enode(
                                    Lang::Ne([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32LtS | Operator::I64LtS => put_enode(
                                    Lang::LtS([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32LtU | Operator::I64LtU => put_enode(
                                    Lang::LtU([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32GtS | Operator::I64GtS => put_enode(
                                    Lang::GtS([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32GtU | Operator::I64GtU => put_enode(
                                    Lang::GtU([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32LeS | Operator::I64LeS => put_enode(
                                    Lang::LeS([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32LeU | Operator::I64LeU => put_enode(
                                    Lang::LeU([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32GeU | Operator::I64GeU => put_enode(
                                    Lang::GeU([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32GeS | Operator::I64GeS => put_enode(
                                    Lang::GeS([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Rotr | Operator::I64Rotr => put_enode(
                                    Lang::RotR([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Rotl | Operator::I64Rotl => put_enode(
                                    Lang::RotL([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32RemS | Operator::I64RemS => put_enode(
                                    Lang::RemS([subexpressions[1], subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32RemU | Operator::I64RemU => put_enode(
                                    Lang::RemU([subexpressions[1], subexpressions[0]]),
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
                                Operator::I32Extend8S | Operator::I64Extend8S => put_enode(
                                    Lang::Extend8S([subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Extend16S | Operator::I64Extend16S => put_enode(
                                    Lang::Extend16S([subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I64Extend32S => put_enode(
                                    Lang::Extend32S([subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I64ExtendI32S => put_enode(
                                    Lang::ExtendI32S([subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I64ExtendI32U => put_enode(
                                    Lang::ExtendI32U([subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I32Popcnt => put_enode(
                                    Lang::Popcnt([subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                Operator::I64Popcnt => put_enode(
                                    Lang::Popcnt([subexpressions[0]]),
                                    &mut r,
                                    entry.entry_idx,
                                    expr,
                                ),
                                _ => panic!("No yet implemented {:?}", operator),
                            };

                            ids_stack.push(nodeid);
                        }
                    }
                }
            }
        }
        Ok(r)
    }

    /// Build RecExpr from tree information
    pub fn build_expr(
        root: Id,
        id_to_node: &[(&Lang, Id)],
        operands: &[Vec<Id>],
    ) -> (RecExpr<Lang>, Vec<Id>) {
        let mut expr = RecExpr::default();

        // A map from the `Id`s we assigned to each sub-expression when extracting a
        // random expression to the `Id`s assigned to each sub-expression by the
        // `RecExpr`.
        let mut node_to_id: HashMap<Id, Id> = Default::default();

        let mut to_visit = vec![(TraversalEvent::Exit, root), (TraversalEvent::Enter, root)];
        let mut node_to_eclass = vec![];
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
                    let (term, eclass) = &id_to_node[usize::from(node)];
                    let sub_expr_id = match term {
                        Lang::Add(_) => expr.add(Lang::Add([operand(0), operand(1)])),
                        Lang::Sub(_) => expr.add(Lang::Sub([operand(0), operand(1)])),
                        Lang::Mul(_) => expr.add(Lang::Mul([operand(0), operand(1)])),
                        Lang::And(_) => expr.add(Lang::And([operand(0), operand(1)])),
                        Lang::Or(_) => expr.add(Lang::Or([operand(0), operand(1)])),
                        Lang::Xor(_) => expr.add(Lang::Xor([operand(0), operand(1)])),
                        Lang::Shl(_) => expr.add(Lang::Shl([operand(0), operand(1)])),
                        Lang::ShrU(_) => expr.add(Lang::ShrU([operand(0), operand(1)])),
                        Lang::ShrS(_) => expr.add(Lang::ShrS([operand(0), operand(1)])),
                        Lang::DivS(_) => expr.add(Lang::DivS([operand(0), operand(1)])),
                        Lang::DivU(_) => expr.add(Lang::DivU([operand(0), operand(1)])),
                        Lang::Eqz(_) => expr.add(Lang::Eqz([operand(0)])),
                        Lang::Eq(_) => expr.add(Lang::Eq([operand(0), operand(1)])),
                        Lang::Ne(_) => expr.add(Lang::Ne([operand(0), operand(1)])),
                        Lang::LtS(_) => expr.add(Lang::LtS([operand(0), operand(1)])),
                        Lang::LtU(_) => expr.add(Lang::LtU([operand(0), operand(1)])),
                        Lang::GtS(_) => expr.add(Lang::GtS([operand(0), operand(1)])),
                        Lang::GtU(_) => expr.add(Lang::GtU([operand(0), operand(1)])),
                        Lang::LeS(_) => expr.add(Lang::LeS([operand(0), operand(1)])),
                        Lang::LeU(_) => expr.add(Lang::LeU([operand(0), operand(1)])),
                        Lang::GeS(_) => expr.add(Lang::GeS([operand(0), operand(1)])),
                        Lang::GeU(_) => expr.add(Lang::GeU([operand(0), operand(1)])),
                        Lang::RotL(_) => expr.add(Lang::RotL([operand(0), operand(1)])),
                        Lang::RotR(_) => expr.add(Lang::RotR([operand(0), operand(1)])),
                        Lang::RemU(_) => expr.add(Lang::RemU([operand(0), operand(1)])),
                        Lang::RemS(_) => expr.add(Lang::RemS([operand(0), operand(1)])),
                        Lang::Wrap(_) => expr.add(Lang::Wrap([operand(0)])),
                        Lang::Extend8S(_) => expr.add(Lang::Extend8S([operand(0)])),
                        Lang::Extend16S(_) => expr.add(Lang::Extend16S([operand(0)])),
                        Lang::Extend32S(_) => expr.add(Lang::Extend32S([operand(0)])),
                        Lang::ExtendI32S(_) => expr.add(Lang::ExtendI32S([operand(0)])),
                        Lang::ExtendI32U(_) => expr.add(Lang::ExtendI32U([operand(0)])),
                        Lang::Popcnt(_) => expr.add(Lang::Popcnt([operand(0)])),
                        Lang::Call(op) => {
                            expr.add(Lang::Call((0..op.len()).map(operand).collect::<Vec<Id>>()))
                        }
                        Lang::Tee(_) => expr.add(Lang::Tee([operand(0)])),
                        Lang::Unfold(_) => expr.add(Lang::Unfold(operand(0))),
                        Lang::ILoad(_) => expr.add(Lang::ILoad([
                            operand(0),
                            operand(1),
                            operand(2),
                            operand(3),
                        ])),
                        d @ Lang::Drop => expr.add((*d).clone()),
                        c @ Lang::Num(_) => expr.add((*c).clone()),
                        s @ Lang::Symbol(_) => expr.add((*s).clone()),
                        s @ Lang::Rand => expr.add((*s).clone()),
                        u @ Lang::Undef => expr.add((*u).clone()),
                        a @ Lang::Arg(_) => expr.add((*a).clone()),
                    };
                    node_to_eclass.push(*eclass);
                    // Copy the id to stack entries to a new one
                    let old_entry = node_to_id.insert(node, sub_expr_id);
                    assert!(old_entry.is_none());
                }
            }
        }

        (expr, node_to_eclass)
    }
}
