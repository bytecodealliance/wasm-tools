//! Helper methods for encoding eterm expressions to Wasm and back

use std::thread::current;
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

use super::analysis::ClassData;

/// Turns wasm to eterm and back
pub struct Encoder;


impl Encoder {

    pub(crate) fn expr2wasm(
        info: &ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        expr: &RecExpr<Lang>,
        node_to_eclass: &Vec<Id>,
        newfunc: &mut Function,
        operators: &Vec<OperatorAndByteOffset>,
        egraph: &EG,
    ) -> crate::Result<()> {
        // This is a patch, this logic should be here
        // If root is Unfold...bypass
        let nodes = expr.as_ref();

        // The last node is the root.
        let root = Id::from(nodes.len() - 1);


        let mut worklist = vec![
            // current, parent
            (root, None, None),
            //(root, root, Event::Exit)
        ];

        let mut to_encode  = vec![];


        let gettpe = |id: Id| {
            let eclass = node_to_eclass[usize::from(id)];
            let data = &egraph[eclass].data;

            data.as_ref().and_then(|data|{
                let entry = &data.get_next_stack_entry(&egraph.analysis);
                Some(entry.return_type.clone())
            })
        };

        let anysibling = |parentid: Id, index_at_parent: Option<usize>| {            
            
            let rootlang = &nodes[usize::from(parentid)];
            println!("Inferrig sibling in {:?}", rootlang);
            match rootlang {
                Lang::Add(operands) | Lang::Eq(operands)
                | Lang::Sub(operands) | Lang::GtS(operands)
                | Lang::Mul(operands) | Lang::GtU(operands)
                | Lang::Xor(operands) | Lang::LtS(operands)
                | Lang::And(operands) | Lang::LtU(operands)
                | Lang::Or(operands) | Lang::GeS(operands)
                | Lang::Shl(operands) | Lang::GeU(operands)
                | Lang::ShrS(operands) | Lang::LeS(operands)
                | Lang::ShrU(operands) | Lang::LeU(operands)
                =>
                {
                    index_at_parent.and_then(|index|{
                        let siblingidx = (usize::from(index) + 1) % operands.len();
                        let sibling = &operands[siblingidx];
                        gettpe(*sibling)
                    })
                    }
                    _ => None
            }
        };

        let infer = |parentid: Option<Id>, index_at_parent: Option<usize>| {

            parentid.and_then(|parentid| {

                let parenttpe = gettpe(parentid);
                let rootlang = &nodes[usize::from(parentid)];
                match rootlang {
                    Lang::ILoad {..} => {
                        // All arguments for this kind are i32.
                        Some(PrimitiveTypeInfo::I32)
                    }
                    Lang::Wrap(_) => {
                        // The expected value is an i64.
                        Some(PrimitiveTypeInfo::I64)
                    }
                    _ => anysibling(parentid, index_at_parent)//.or(parenttpe)
                }
            })
        };

        // Enqueue the coding back nodes and infer types
        while let Some((current_node, parent, index_at_parent)) = worklist.pop() {
            let rootlang = &nodes[usize::from(current_node)];
            match rootlang {
                Lang::Add(operands) | Lang::Shl(operands) | Lang::ShrU(operands)
                | Lang::Sub(operands) | Lang::Mul(operands) | Lang::And(operands) 
                | Lang::Or(operands) | Lang::Xor(operands) | Lang::ShrS(operands) 
                | Lang::DivS(operands) | Lang::DivU(operands) | Lang::RotR(operands)
                | Lang::RotL(operands)| Lang::RemS(operands) | Lang::RemU(operands) => {
                    let tpe = infer(parent, index_at_parent).or(gettpe(current_node));
                    to_encode.push((current_node, tpe.expect("A type is always needed to encode"), rootlang));
                    for (idx, operand) in operands.iter().enumerate() {
                        worklist.push((
                            *operand , Some(current_node), Some(idx)
                        ));
                    }
                },
                Lang::Eqz(operands) => {
                    let tpe = gettpe(operands[0]).expect("Inferred from operands");
                    to_encode.push((current_node, tpe, rootlang));
                    for (idx, operand) in operands.iter().enumerate() {
                        worklist.push((
                            *operand , Some(current_node), Some(idx)
                        ));
                    }
                },
                Lang::Eq(operands) |
                Lang::Ne(operands)  |
                Lang::LtS(operands) |
                Lang::LtU(operands) |
                Lang::GtS(operands) |
                Lang::GtU(operands) |
                Lang::LeS(operands) |
                Lang::LeU(operands) |
                Lang::GeS(operands) | Lang::GeU(operands) => {
                    // Irelops type is expected from operands
                    let tpe = gettpe(operands[0]).or(gettpe(operands[1])).expect("Inferred from operands");
                    to_encode.push((current_node, tpe, rootlang));
                    for (idx, operand) in operands.iter().enumerate() {
                        worklist.push((
                            *operand , Some(current_node), Some(idx)
                        ));
                    }
                },
                Lang::Tee(operands) => {
                    let tpe = gettpe(current_node);
                    to_encode.push((current_node, tpe.expect("A type is always needed to encode"), rootlang));
                    for (idx, operand) in operands.iter().enumerate() {
                        worklist.push((
                            *operand , Some(current_node), Some(idx)
                        ));
                    }
                },
                Lang::Wrap(operands) => {
                    // Wrap is always i64
                    to_encode.push((current_node,PrimitiveTypeInfo::I64, rootlang));
                    for (idx, operand) in operands.iter().enumerate() {
                        worklist.push((
                            *operand , Some(current_node), Some(idx)
                        ));
                    }
                },
                // Conversion operations always return the former type (t2.extend_t1 -> [t2])
                Lang::Extend16S(operands) | Lang::Extend32S(operands) 
                | Lang::ExtendI32S(operands) | Lang::ExtendI32U(operands) | Lang::Extend8S(operands) => {
                    let tpe = gettpe(current_node).expect("Extend operation should have a type");
                    to_encode.push((current_node, tpe , rootlang));
                    for (idx, operand) in operands.iter().enumerate() {
                        worklist.push((
                            *operand , Some(current_node), Some(idx)
                        ));
                    }
                },
                Lang::Call(operands) => {
                    let tpe = gettpe(current_node).expect("Call operation should have a type");
                    to_encode.push((current_node, tpe , rootlang));
                    // The first operand is always the helper Arg to identify the function
                    for (idx, operand) in operands[1..].iter().enumerate() {
                        worklist.push((
                            *operand , Some(current_node), Some(idx)
                        ));
                    }
                },
                Lang::Popcnt(operand) => {
                    to_encode.push((
                        // The type is determined by this equivalence class
                        current_node, gettpe(*operand).expect("The type of the unop is inferred from the operand"), rootlang
                    ));
                    worklist.push((
                        *operand , Some(current_node), Some(0)
                    ));
                },
                Lang::Drop => {
                    
                    to_encode.push((
                        // The type is determined by this equivalence class
                        current_node, PrimitiveTypeInfo::Empty, rootlang
                    ));
                },
                Lang::Symbol(s) => {
                    let entry = &egraph
                    .analysis
                    .get_stack_entry_from_symbol(s.to_string())
                    .ok_or(crate::Error::UnsupportedType(EitherType::EggError(
                        "The current symbol cannot be retrieved".into()
                    )))?;

                    to_encode.push((
                        current_node, entry.return_type.clone() , rootlang
                    ));
                },
                Lang::Num(_) => {
                    // Prioritize, infer from parent specific cases (wrap, iload, etc),
                    // then siblings and last its own type
                    let tpe = infer(parent, index_at_parent).or(gettpe(current_node))
                    .or(parent.and_then(|parent|gettpe(parent)));
                    to_encode.push((
                        // The type is determined by this equivalence class
                        current_node, tpe.expect("A load operation should be encoded with a type"), rootlang
                    ));
                },
                Lang::ILoad(operands) => {
                    // Only write operand 0
                    to_encode.push((
                        // The type is determined by this equivalence class
                        current_node, gettpe(current_node).expect("A load operation should be encoded with a type"), rootlang
                    ));
                    worklist.push((
                        operands[0] , Some(current_node), Some(0)
                    ));
                },
                Lang::Rand => {
                    // Infer from parent in this case
                    let tpe = infer(parent, index_at_parent);
                    println!("rand type {:?}", tpe);
                    to_encode.push((
                        // The type is determined by the operand
                        current_node, tpe.expect("A rand operation needs to be inferred from parent"), rootlang
                    ));
                },
                Lang::Undef => {
                    /* Do nothing */
                },
                Lang::Unfold(operand) => {
                    // Only write operand 0
                    let tpe = infer(parent, index_at_parent).or(gettpe(*operand));
                    to_encode.push((
                        // The type is determined by the operand
                        current_node, tpe.expect("The operand of an unfold operation shoud have a type"), rootlang
                    ));
                },
                Lang::Arg(_) => {
                    let tpe = infer(parent, index_at_parent).or(gettpe(current_node));
                    to_encode.push((
                        // The type is determined by the operand
                        current_node, tpe.expect("Arg nodes are written as Num, and they should have a type"), rootlang
                    ));
                },
            }
        
        }

        println!("nodes {:?}", to_encode);
        while let Some((node, tpe, l)) = to_encode.pop(){
            let rootlang = &nodes[usize::from(node)];

            match rootlang {
                Lang::Add(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Add);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Add);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Sub(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Sub);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Sub);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Mul(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Mul);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Mul);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::And(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32And);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64And);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Or(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Or);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Or);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Xor(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Xor);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Xor);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Shl(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Shl);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Shl);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::ShrU(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32ShrU);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64ShrU);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::DivU(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32DivU);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64DivU);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::DivS(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32DivS);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64DivS);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::ShrS(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32ShrS);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64ShrS);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::RotR(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Rotr);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Rotr);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::RotL(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Rotl);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Rotl);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::RemS(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32RemS);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64RemS);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::RemU(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32RemU);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64RemU);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Eqz(_) => {
                    // Infer from the operand
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Eqz);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Eqz);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Eq(_) => {
                    // Infer from the operands
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Eq);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Eq);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Ne(_) =>{
                    // Infer from the operands
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Neq);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Neq);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::LtS(operands) => {
                    // Infer from the operands
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32LtS);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64LtS);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::LtU(_) => {
                    // Infer from the operands
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32LtU);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64LtU);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::GtS(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32GtS);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64GtS);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::GtU(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32GtU);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64GtU);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::LeS(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32LeS);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64LeS);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::LeU(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32LeU);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64LeU);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::GeS(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32GeS);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64GeS);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::GeU(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32GeU);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64GeU);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Tee(_) => {
                    let eclass = node_to_eclass[usize::from(node)];
                    let data = &egraph[eclass].data;
                    let entry = data.clone().unwrap().get_next_stack_entry(&egraph.analysis);
                    if let StackType::LocalTee(local_index) = entry.operator {
                        newfunc.instruction(&Instruction::LocalTee(local_index));
                    }
                    else {
                        unreachable!("Incorrect mapping")
                    }
                },
                Lang::Wrap(_) => {
                    newfunc.instruction(&Instruction::I32WrapI64);
                },
                Lang::Extend8S(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Extend8S);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Extend8S);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Extend16S(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Extend16S);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Extend16S);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::Extend32S(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Extend32S);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::ExtendI32S(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64ExtendI32S);},
                        _ => unreachable!("bad type")
                    }
                },
                Lang::ExtendI32U(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64ExtendI32U);},
                        _ => unreachable!("bad type")
                    }
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
                },
                Lang::Popcnt(_) => {
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Popcnt);},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Popcnt);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                },
                Lang::Drop => {
                    newfunc.instruction(&Instruction::Drop);
                },
                Lang::ILoad(operands) => {

                    let offset_operand = &nodes[usize::from(operands[1])];
                    let align_operand = &nodes[usize::from(operands[2])];
                    let memidx_operand = &nodes[usize::from(operands[3])];
    
                    let toarg = |op: &Lang| {
                        match op {
                            Lang::Arg(val) => *val,
                            Lang::Num(val) => *val as u64, // Num needs to be taken into account here because the parsing of rules is done by egg itself
                            _ => unreachable!("This operand should be an Arg node. Current operand {:?}",op ),
                        }
                    };
    
                    let offset_value = toarg(offset_operand);
    
                    let align_value = toarg(align_operand);
    
                    let memidx_value = toarg(memidx_operand);
    
                    let memarg = MemArg{
                        offset: offset_value, // These can be mutated as well
                        align: align_value as u32,
                        memory_index: memidx_value as u32,
                    };
                    match tpe {
                        PrimitiveTypeInfo::I32 => {
                            newfunc.instruction(&Instruction::I32Load(
                                memarg
                            ));
                        },
                        PrimitiveTypeInfo::I64 => {
                            newfunc.instruction(&Instruction::I64Load(
                                memarg
                            ));
                        },
                        _ => unreachable!("Type cannot be encoded")
                    }
                },
                Lang::Rand => {

                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Const(rnd.gen()));},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Const(rnd.gen()));},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    
                },
                Lang::Undef => {/* Do nothing */},
                Lang::Unfold(operand) => {
                    let child = &nodes[usize::from(*operand)];
                    match child {
                        Lang::Num(value) => {
                            // Getting type from eclass.
                            match tpe {
                                PrimitiveTypeInfo::I64 => {
                                    let r: i64 = rnd.gen();
                                    newfunc.instruction(&Instruction::I64Const(r));
                                    newfunc.instruction(&Instruction::I64Const((Wrapping(*value) - Wrapping(r)).0));
                                    newfunc.instruction(&Instruction::I64Add);
                                },
                                PrimitiveTypeInfo::I32 => {
                                    let r: i32 = rnd.gen();
                                    newfunc.instruction(&Instruction::I32Const(r));
                                    newfunc.instruction(&Instruction::I32Const((Wrapping(*value as i32) - Wrapping(r)).0));
                                    newfunc.instruction(&Instruction::I32Add);
                                }
                                _ => return Err(crate::Error::UnsupportedType(EitherType::EggError(format!(
                                    "The current eterm cannot be unfolded {:?}",
                                    child,
                                )))),
                            }
                            
                        },
                        _ => return Err(crate::Error::UnsupportedType(EitherType::EggError(format!(
                            "The current eterm cannot be unfolded {:?}",
                            child,
                        )))),
                    }
                },
                Lang::Num(v) => {
                    // Infer from parent or siblings
                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Const(*v as i32));},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Const(*v));},
                        _ => unreachable!("bad type")
                    }
                },

                Lang::Symbol(s) => {
                    println!("Symbol... {} s" ,s );
                    let entry = &egraph
                    .analysis
                    .get_stack_entry_from_symbol(s.to_string())
                    .ok_or(crate::Error::UnsupportedType(EitherType::EggError(
                        "The current symbol cannot be retrieved".into()
                    )))?;
    
                    match entry.operator {
                        StackType::LocalGet(idx) => {
                            newfunc.instruction(&Instruction::LocalGet(idx));
                        },
                        StackType::GlobalGet(idx) => {
                            newfunc.instruction(&Instruction::GlobalGet(idx));
                        }
                        _ => return Err(crate::Error::UnsupportedType(EitherType::EggError(
                            "Incorrect stack type".into()
                        ))),
                    }
                },
                Lang::Arg(val) => {

                    match tpe {
                        PrimitiveTypeInfo::I32 => {newfunc.instruction(&Instruction::I32Const(*val as i32));},
                        PrimitiveTypeInfo::I64 => {newfunc.instruction(&Instruction::I64Const(*val as i64));},
                        _ => unreachable!("Type cannot be encoded")
                    }
                },
            }
        }

        Ok(())
        /*
        Encoder::expr2wasm_aux2(
            info,
            rnd,
            nodes,
            node_to_eclass,
            root,
            newfunc,
            &None,
            None,
            None,
            operators,
            egraph,
        ) */
    }

    fn writestackentry(
        info: &ModuleInfo,
        egraph: &EG,
        entry: &StackEntry,
        operators: &Vec<OperatorAndByteOffset>,
        newfunc: &mut Function,
    ) -> crate::Result<()> {
        // Write the deps in the dfg
        // Process operands
        if let StackType::Undef = entry.operator {
            // Do nothing
            // log
        } else {
            for idx in &entry.operands {
                let entry = &egraph.analysis.get_stack_entry(*idx);
                Encoder::writestackentry(info, egraph, entry, operators, newfunc)?;
            }
            // Write the operator
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
        Ok(())
    }

    /// Reassembles the mutated function and return a `Function` entry
    pub fn build_function(
        info: &ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        insertion_point: usize,
        expr: &RecExpr<Lang>,
        node_to_eclass: &Vec<Id>,
        operators: &Vec<OperatorAndByteOffset>,
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
                    Encoder::expr2wasm(
                        info,
                        rnd,
                        expr,
                        node_to_eclass,
                        newfunc,
                        operators,
                        egraph,
                    )?;
                } else {
                    // Copy the stack entry as it is
                    Encoder::writestackentry(info, &egraph, entry, operators, newfunc)?;
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
        operators: &Vec<OperatorAndByteOffset>,
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
        fn wasm2expraux(
            dfg: &MiniDFG,
            entryidx: usize,
            operators: &Vec<OperatorAndByteOffset>,
            lang_to_stack_entries: &mut HashMap<Lang, (Id, Vec<usize>)>,
            // The wasm expressions will be added here
            expr: &mut RecExpr<Lang>, // Replace this by RecExpr
        ) -> crate::Result<Id> {
            let entry = &dfg.entries[entryidx];

            if let PrimitiveTypeInfo::Empty = entry.return_type {
                // Return if the value returned by this operator is empty
                return Err(crate::Error::NoMutationsApplicable);
            }

            if let StackType::Undef = entry.operator {
                let newid = put_enode(Lang::Undef, lang_to_stack_entries, entry.entry_idx, expr);
                return Ok(newid);
            }
            let op = &entry.operator;

            match op {
                StackType::I32(value) => {
                    let lang = Lang::Num(*value as i64);
                    return Ok(put_enode(
                        lang,
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    ));
                }
                StackType::I64(value) => {
                    let lang = Lang::Num(*value);
                    return Ok(put_enode(
                        lang,
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    ));
                }
                StackType::LocalGet(idx) => {
                    let name = format!("?l{}", idx);
                    return Ok(put_enode(
                        Lang::Symbol(name.clone().into()),
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    ));
                }
                StackType::LocalTee(_) => {
                    let value = wasm2expraux(
                        dfg,
                        entry.operands[0],
                        operators,
                        lang_to_stack_entries,
                        expr,
                    )?;
                    return Ok(put_enode(
                        Lang::Tee([value]),
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    ));
                }
                StackType::GlobalGet(idx) => {
                    let name = format!("?g{}", idx);
                    return Ok(put_enode(
                        Lang::Symbol(name.clone().into()),
                        lang_to_stack_entries,
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
                    let offsetid = wasm2expraux(
                        dfg,
                        entry.operands[0],
                        operators,
                        lang_to_stack_entries,
                        expr,
                    )?;

                    let staticoffsetoid = put_enode(
                        Lang::Arg(*offset as u64),
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    );
                    let alignid = put_enode(
                        Lang::Arg(*align as u64),
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    );
                    let memidxid = put_enode(
                        Lang::Arg(*memory as u64),
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    );
                    return Ok(put_enode(
                        Lang::ILoad([offsetid, staticoffsetoid, alignid, memidxid]),
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    ));
                }
                StackType::IndexAtCode(operatoridx, childcount) => {
                    let mut subexpressions = Vec::new();
                    for operandi in &entry.operands {
                        let eterm =
                            wasm2expraux(dfg, *operandi, operators, lang_to_stack_entries, expr)?;
                        subexpressions.push(eterm);
                    }
                    let len = subexpressions.len();
                    debug_assert_eq!(*childcount, len);

                    let (operator, _) = &operators[*operatoridx];

                    // Mapping operator to Lang
                    let nodeid = match operator {
                        Operator::I32Add | Operator::I64Add => put_enode(
                            Lang::Add([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Shl | Operator::I64Shl => put_enode(
                            Lang::Shl([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I64ShrU | Operator::I32ShrU => put_enode(
                            Lang::ShrU([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),

                        Operator::I64ShrS | Operator::I32ShrS => put_enode(
                            Lang::ShrS([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I64DivS | Operator::I32DivS => put_enode(
                            Lang::DivS([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I64DivU | Operator::I32DivU => put_enode(
                            Lang::DivU([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32And | Operator::I64And => put_enode(
                            Lang::And([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Or | Operator::I64Or => put_enode(
                            Lang::Or([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Xor | Operator::I64Xor => put_enode(
                            Lang::Xor([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Sub | Operator::I64Sub => put_enode(
                            Lang::Sub([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Mul | Operator::I64Mul => put_enode(
                            Lang::Mul([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Eqz | Operator::I64Eqz => put_enode(
                            Lang::Eqz([subexpressions[0]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Eq | Operator::I64Eq => put_enode(
                            Lang::Eq([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Ne | Operator::I64Ne => put_enode(
                            Lang::Ne([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32LtS | Operator::I64LtS => put_enode(
                            Lang::LtS([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32LtU | Operator::I64LtU => put_enode(
                            Lang::LtU([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32GtS | Operator::I64GtS => put_enode(
                            Lang::GtS([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32GtU | Operator::I64GtU => put_enode(
                            Lang::GtU([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32LeS | Operator::I64LeS => put_enode(
                            Lang::LeS([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32LeU | Operator::I64LeU => put_enode(
                            Lang::LeU([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32GeU | Operator::I64GeU => put_enode(
                            Lang::GeU([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32GeS | Operator::I64GeS => put_enode(
                            Lang::GeS([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Rotr | Operator::I64Rotr => put_enode(
                            Lang::RotR([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Rotl | Operator::I64Rotl => put_enode(
                            Lang::RotL([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32RemS | Operator::I64RemS => put_enode(
                            Lang::RemS([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32RemU | Operator::I64RemU => put_enode(
                            Lang::RemU([subexpressions[0], subexpressions[1]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32WrapI64 => put_enode(
                            Lang::Wrap([subexpressions[0]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Extend8S | Operator::I64Extend8S => put_enode(
                            Lang::Extend8S([subexpressions[0]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I32Extend16S | Operator::I64Extend16S => put_enode(
                            Lang::Extend16S([subexpressions[0]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I64Extend32S => put_enode(
                            Lang::Extend32S([subexpressions[0]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I64ExtendI32S => put_enode(
                            Lang::ExtendI32S([subexpressions[0]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        Operator::I64ExtendI32U => put_enode(
                            Lang::ExtendI32U([subexpressions[0]]),
                            lang_to_stack_entries,
                            entry.entry_idx,
                            expr,
                        ),
                        _ => panic!("No yet implemented {:?}", operator),
                    };

                    return Ok(nodeid);
                }
                StackType::Call {
                    function_index,
                    params_count,
                } => {
                    // To differentiate between functions, the first child
                    // is the index of the function in the Wasm module
                    let function_indexid = put_enode(
                        Lang::Arg(*function_index as u64),
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    );
                    let id = vec![function_indexid];
                    let children = (0..*params_count)
                        .map(|i| {
                            wasm2expraux(
                                dfg,
                                entry.operands[i],
                                operators,
                                lang_to_stack_entries,
                                expr,
                            )
                        })
                        .collect::<crate::Result<Vec<Id>>>()?;

                    return Ok(put_enode(
                        Lang::Call(vec![id, children].concat()),
                        lang_to_stack_entries,
                        entry.entry_idx,
                        expr,
                    ));
                }
                _ => panic!("Not yet implemented {:?}", op),
            };
        }

        let mut r = HashMap::new();
        wasm2expraux(dfg, stack_entry_index, operators, &mut r, expr)?;
        Ok(r)
    }
    /// Build RecExpr from tree information
    pub fn build_expr(
        root: Id,
        id_to_node: &Vec<(&Lang, Id)>,
        operands: &Vec<Vec<Id>>,
    ) -> (RecExpr<Lang>, Vec<Id>) {
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
        let mut node_to_eclass = vec![];
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
                        Lang::Popcnt(_) => expr.add(Lang::Popcnt(operand(0))),
                        Lang::Call(op) => expr.add(Lang::Call(
                            (0..op.len()).map(|id| operand(id)).collect::<Vec<Id>>(),
                        )),
                        Lang::Tee(_) => expr.add(Lang::Tee([operand(0)])),
                        Lang::Unfold(op) => expr.add(Lang::Unfold(*op)),
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
