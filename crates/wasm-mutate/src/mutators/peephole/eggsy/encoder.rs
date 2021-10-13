//! Helper methods for encoding eterm expressions to Wasm and back

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

macro_rules! eterm_operator_2_wasm {
    (@expand
        $eclassdata:expr, $newfunc:expr, $parent_eclass:expr, $nodes:expr, $root: expr, $egraph: expr,
        { $($target:tt)* }
    ) => {
           $(
               $target
           )*
    };
    (@expand
        $eclassdata:expr, $newfunc:expr, $parent_eclass: expr, $nodes:expr, $root: expr, $egraph: expr,
        $(
            $case:pat => [$($target:tt)+]
        )*
        ) => {
            match $eclassdata {
                Some(data) => {

                    let entry = data.get_next_stack_entry(&$egraph.analysis);
                    let tpes = &entry.tpes[..];

                    match tpes {
                        $(
                            $case => {
                                $newfunc.instruction(
                                    $(
                                        $target
                                    )+
                                );
                                Ok(())
                            }
                        )*
                        _ => Err(crate::Error::UnsupportedType(EitherType::EggError(format!("There is no conversion between the eclass returning type ({:?}) the the available Wasm operators", tpes))))
                    }
                }
                None => {
                    // Try with the parent eclass information
                    match $parent_eclass {
                        Some(data) => {
                            let entry = &data.get_next_stack_entry(&$egraph.analysis);
                            let tpes = &entry.tpes[..];
                            match tpes {
                                $(
                                    $case => {
                                        $newfunc.instruction(
                                            $(
                                                $target
                                            )+
                                        );
                                        Ok(())
                                    }
                                )*
                                _ => Err(crate::Error::UnsupportedType(EitherType::EggError(format!("There is no conversion between the eclass returning type ({:?}) the the available Wasm operators", tpes))))
                            }
                        }
                        None => Err(crate::Error::UnsupportedType(EitherType::EggError(format!("The current eclass has no data information {:?} {:?}", $eclassdata, $root))))

                    }
                }
            }
    };
    // Write operands in order
    (@write_subtree $eclassdata:expr, $newfunc:expr,  $info:expr, $rnd: expr, $nodes: expr, $root:expr, $node_to_eclass:expr, $operators:expr, $egraph: expr, $parenteclass: expr, [$operands:ident], $childcount: expr
        => {
            $($body:tt)*
        }
    ) => {
        for oidx in (0..$childcount){
            Encoder::expr2wasm_aux2(
                $info,
                $rnd,
                $nodes,
                $node_to_eclass,
                $operands[oidx],
                $newfunc,
                $eclassdata,
                $operators,
                $egraph,
            )?;
        }
        eterm_operator_2_wasm! {
            @expand $eclassdata, $newfunc, $parenteclass, $nodes, $root, $egraph, $(
                $body
            )*
        }
    };
    (@write_subtree $eclassdata:expr, $newfunc:expr,  $info:expr, $rnd: expr, $nodes: expr, $root:expr, $node_to_eclass:expr, $operators:expr, $egraph: expr, $parenteclass: expr, [$operands:ident], $childcount: expr,$name4node:ident, $namefornewfunc:ident, $nameforrnd:ident, $namefor_eclass:ident, $namefor_rootclass:ident, $name4egraph: ident, $name4info: ident, $name4operators: ident, $name4nodetoeclass: ident
        => {
            $($body:tt)*
        }
    ) => {
        for oidx in (0..$childcount){
            Encoder::expr2wasm_aux2(
                $info,
                $rnd,
                $nodes,
                $node_to_eclass,
                $operands[oidx],
                $newfunc,
                $eclassdata,
                $operators,
                $egraph,
            )?;
        }

        let $name4node = $nodes;
        let $namefornewfunc = $newfunc;
        let $nameforrnd = $rnd;
        let $namefor_eclass = $eclassdata;
        let $namefor_rootclass = $parenteclass;
        let $name4egraph = $egraph;
        let $name4info = $info;
        let $name4operators = $operators;
        let $name4nodetoeclass = $node_to_eclass;

        eterm_operator_2_wasm! {
            @expand $eclassdata, $newfunc, $parenteclass, $nodes, $root,  $egraph, $(
                $body
            )*
        }
    };
    // Single operand
    (@write_subtree $eclassdata:expr, $newfunc:expr,  $info:expr, $rnd: expr, $nodes: expr, $root:expr, $node_to_eclass:expr, $operators:expr, $egraph: expr,$parenteclass: expr, ( $operand:expr ), $name4node:ident, $namefornewfunc:ident, $nameforrnd:ident, $namefor_eclass:ident, $namefor_rootclass:ident, $name4egraph: ident, $name4info: ident, $name4operators: ident, $name4nodetoeclass: ident
        => {
            $($body:tt)*
        }
    ) => {
        Encoder::expr2wasm_aux2(
            $info,
            $rnd,
            $nodes,
            $node_to_eclass,
            $operand,
            $newfunc,
            $eclassdata,
            $operators,
            $egraph,
        )?;

        let $name4node = $nodes;
        let $namefornewfunc = $newfunc;
        let $nameforrnd = $rnd;
        let $namefor_eclass = $eclassdata;
        let $namefor_rootclass = $parenteclass;
        let $name4egraph = $egraph;
        let $name4info = $info;
        let $name4operators = $operators;
        let $name4nodetoeclass = $node_to_eclass;
        eterm_operator_2_wasm! {
            @expand $eclassdata, $newfunc , $parenteclass, $nodes, $root,  $egraph,$(
                $body
            )*
        }
    };
    //
    (@write_subtree $eclassdata:expr, $newfunc:expr,  $info:expr, $rnd: expr, $nodes: expr, $root:expr, $node_to_eclass:expr, $operators:expr, $egraph: expr, $parenteclass: expr, $name: ident
        => {
            $($body:tt)*
        }
    ) => {
        eterm_operator_2_wasm! {
            @expand $eclassdata, $newfunc, $parenteclass, $nodes, $root,$egraph, $(
                $body
            )*
        }
    };
    (@write_subtree $eclassdata:expr, $newfunc:expr,  $info:expr, $rnd: expr, $nodes: expr,  $root:expr, $node_to_eclass:expr, $operators:expr, $egraph: expr, $parenteclass: expr, $name: ident,  $name4node:ident, $namefornewfunc:ident, $nameforrnd:ident, $namefor_eclass:ident, $namefor_rootclass:ident, $name4egraph: ident, $name4info: ident, $name4operators: ident, $name4nodetoeclass: ident
        => {
            $($body:tt)*
        }
    ) => {
        let $name4node = $nodes;
        let $namefornewfunc = $newfunc;
        let $nameforrnd = $rnd;
        let $namefor_eclass = $eclassdata;
        let $namefor_rootclass = $parenteclass;
        let $name4egraph = $egraph;
        let $name4info = $info;
        let $name4operators = $operators;
        let $name4nodetoeclass = $node_to_eclass;
        eterm_operator_2_wasm! {
            @expand $eclassdata, $newfunc, $parenteclass, $nodes, $root, $egraph, $(
                $body
            )*
        }
    };
    (
        $(
            [$lang:pat, $($kind:tt)* ] => {
                $($body:tt)*
            }
        )*
        $(,)*
    ) => {

        fn expr2wasm_aux2(info: &ModuleInfo,
            rnd: &mut rand::prelude::SmallRng,
            nodes: &[Lang],
            node_to_eclass: &Vec<Id>,
            current: Id,
            newfunc: &mut Function,
            parent_eclass: &Option<ClassData>,
            // the following four parameters could be moved to the PeepholeAnalysis
            operators: &Vec<OperatorAndByteOffset>,
            egraph: &EG) -> crate::Result<()>{

            let root = &nodes[usize::from(current)];
            // Pass node_to_eclass down as well
            let eclass = node_to_eclass[usize::from(current)];

            let data = &egraph[eclass].data;

            match root {
                $(
                    $lang => {
                        // Write the operands first
                        eterm_operator_2_wasm!{
                            @write_subtree data, newfunc, info, rnd, nodes, root, node_to_eclass, operators, egraph, parent_eclass, $($kind)* => {
                                $($body)*
                            }
                        }
                        /* */
                    }
                )*
                _ => todo!("Not implemented {:?}", root)
            }
        }

    };
}

impl Encoder {
    eterm_operator_2_wasm! {
        [Lang::GtS(operands), [operands], 2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];
            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32GtS);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64GtS);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::GtU(operands), [operands], 2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];
            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32GtU);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64GtU);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::LtS(operands), [operands],2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];
            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32LtS);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64LtS);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::LtU(operands), [operands], 2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];
            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32LtU);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64LtU);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::LeU(operands), [operands], 2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];
            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32LeU);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64LeU);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::LeS(operands), [operands], 2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];
            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32LeS);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64LeS);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::GeS(operands), [operands], 2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];
            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32GeU);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64GeU);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::Eq(operands), [operands], 2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];
            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32Eq);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64Eq);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::Ne(operands), [operands], 2, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];

            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32Neq);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64Neq);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::Eqz(operands), [operands], 1, _nodes, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, node_to_eclass] => {{
            // The type of irelop is the operand, and it always returns i32
            let operandeid = operands[0];
            let eclassid = node_to_eclass[usize::from(operandeid)];
            let operandeclass = &egraph[eclassid];

            match &operandeclass.data {
                Some(data) => {

                    let operandtpe = data.get_next_stack_entry(&egraph.analysis);
                    let operandtpe = &operandtpe.tpes[..];
                    match operandtpe {
                        [PrimitiveTypeInfo::I32] => {newfunc.instruction(Instruction::I32Eqz);},
                        [PrimitiveTypeInfo::I64] => {newfunc.instruction(Instruction::I64Eqz);},
                        _ => unreachable!("Type cannot be encoded")
                    }
                    Ok(())
                }
                None => {
                    unreachable!("The operand for this instruction should have a type information")
                }
            }
        }}
        [Lang::Or(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Or]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Or]

        }
        [Lang::DivS(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32DivS]
            [PrimitiveTypeInfo::I64] => [Instruction::I64DivS]

        }
        [Lang::ShrS(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32ShrS]
            [PrimitiveTypeInfo::I64] => [Instruction::I64ShrS]

        }
        [Lang::DivU(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32DivU]
            [PrimitiveTypeInfo::I64] => [Instruction::I64DivU]

        }
        [Lang::And(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32And]
            [PrimitiveTypeInfo::I64] => [Instruction::I64And]

        }
        [Lang::Xor(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Xor]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Xor]

        }
        [Lang::Mul(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Mul]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Mul]

        }

        [Lang::Add(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Add]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Add]

        }
        [Lang::Sub(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Sub]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Sub]

        }
        [Lang::ShrU(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32ShrU]
            [PrimitiveTypeInfo::I64] => [Instruction::I64ShrU]

        }
        [Lang::Shl(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Shl]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Shl]

        }
        [Lang::ILoad(operands), (operands[0]), /*between parenthesis means that this operand will be written down*/nodes, newfunc, _rnd, eclassdata, _rootclassdata, egraph, _info, _operators, _node_to_eclass] => {{
            let entry = eclassdata.clone().unwrap().get_next_stack_entry(&egraph.analysis);
            if let StackType::Load { offset, align, memory } = entry.operator {

                debug_assert_eq!(4, operands.len());
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
                match entry.tpes[..] {
                    [PrimitiveTypeInfo::I32] => {
                        newfunc.instruction(Instruction::I32Load(
                            memarg
                        ));
                    },
                    [PrimitiveTypeInfo::I64] => {
                        newfunc.instruction(Instruction::I64Load(
                            memarg
                        ));
                    },
                    _ => unreachable!("Type cannot be encoded")
                }
            }
            else {
                unreachable!("Incorrect mapping")
            }
            Ok(())
        }}

        [Lang::Num(value), value] => {
            [PrimitiveTypeInfo::I32] => [Instruction::I32Const(*value as i32)]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Const(*value)]
        }
        [Lang::Rand, value, _n, newfunc, rnd, _eclassdata, _rootdata, _egraph, _info, _operators, _node_to_eclass] => {{
            newfunc.instruction(Instruction::I32Const(rnd.gen()));
            Ok(())
        }}
        [Lang::Arg(value), value, _n, newfunc, _rnd, _eclassdata, _rootdata, _egraph, _info, _operators, _node_to_eclass] => {{
            newfunc.instruction(Instruction::I32Const(*value as i32));
            Ok(())
        }}
        [Lang::Symbol(s), s, _n, newfunc, _rnd, _eclassdata, _rootclassdata, egraph, _info, _operators, _node_to_eclass] => {{
            let entry = &egraph.analysis.get_stack_entry_from_symbol(s.to_string()).ok_or(crate::Error::UnsupportedType(EitherType::EggError(format!("The current eterm cannot be unfolded"))))?;
            if let StackType::LocalGet(idx) = entry.operator {
                newfunc.instruction(Instruction::LocalGet(idx));
                Ok(())
            }
            else{
                Err(crate::Error::UnsupportedType(EitherType::EggError(format!("Incorrect stack type"))))
            }
        }}
        [Lang::Unfold(value), value, n, newfunc, rnd, eclassdata, _rootclassdata, egraph, _info, _operators, _node_to_eclass] => {{
            let child = &n[usize::from(*value)];
            match child {
                Lang::Num(value) => {
                    // getting type from eclass
                    match eclassdata {
                        Some(data) => {
                            let entry = &data.get_next_stack_entry(&egraph.analysis);
                            let tpes = &entry.tpes[..];
                            match tpes{
                                [PrimitiveTypeInfo::I64] => {
                                    let r: i64 = rnd.gen();
                                    newfunc.instruction(Instruction::I64Const(r));
                                    newfunc.instruction(Instruction::I64Const((Wrapping(*value) - Wrapping(r)).0));
                                    newfunc.instruction(Instruction::I64Add);
                                    Ok(())
                                },
                                [PrimitiveTypeInfo::I32] => {
                                    let r: i32 = rnd.gen();
                                    newfunc.instruction(Instruction::I32Const(r));
                                    newfunc.instruction(Instruction::I32Const((Wrapping(*value as i32) - Wrapping(r)).0));
                                    newfunc.instruction(Instruction::I32Add);
                                    Ok(())
                                }
                                _ => Err(crate::Error::UnsupportedType(EitherType::EggError(format!("The current eterm cannot be unfolded {:?}", child))))
                            }
                        },
                        None => Err(crate::Error::UnsupportedType(EitherType::EggError(format!("The current eterm cannot be unfolded {:?}", child))))
                    }
                },
                _ => Err(crate::Error::UnsupportedType(EitherType::EggError(format!("The current eterm cannot be unfolded {:?}", child))))
            }
        }}
    }

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
        Encoder::expr2wasm_aux2(
            info,
            rnd,
            nodes,
            node_to_eclass,
            Id::from(nodes.len() - 1), // first node is the root
            newfunc,
            &None,
            operators,
            egraph,
        )
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
                    newfunc.instruction(Instruction::I32Const(val));
                }
                StackType::I64(val) => {
                    newfunc.instruction(Instruction::I64Const(val));
                }
                StackType::LocalGet(idx) => {
                    newfunc.instruction(Instruction::LocalGet(idx));
                }
                StackType::LocalSet(idx) => {
                    newfunc.instruction(Instruction::LocalSet(idx));
                }
                StackType::Load {
                    offset,
                    align,
                    memory,
                } => {
                    // Here it depends on the type
                    match entry.tpes[..] {
                        [PrimitiveTypeInfo::I32] => {
                            newfunc.instruction(Instruction::I32Load(MemArg {
                                offset: offset,
                                align: align as u32,
                                memory_index: memory,
                            }));
                        }
                        [PrimitiveTypeInfo::I64] => {
                            newfunc.instruction(Instruction::I64Load(MemArg {
                                offset: offset,
                                align: align as u32,
                                memory_index: memory,
                            }));
                        }
                        _ => unreachable!("Type {:?} is not supported", entry.tpes),
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
                if entryidx == insertion_point {
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
                hashset.insert(l.clone(), (newid, vec![entryindex]));
                expr.add(l.clone());
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

                    let offsetid = put_enode(
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
                        Lang::ILoad([offsetid, offsetid, alignid, memidxid]),
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
                        _ => panic!("No yet implemented {:?}", operator),
                    };

                    return Ok(nodeid);
                }
                _ => panic!("Not implemented yet {:?}", op),
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
                        Lang::Popcnt(_) => expr.add(Lang::Popcnt(operand(0))),
                        Lang::Unfold(op) => expr.add(Lang::Unfold(*op)),
                        Lang::ILoad(_) => expr.add(Lang::ILoad([
                            operand(0),
                            operand(1),
                            operand(2),
                            operand(3),
                        ])),
                        c @ Lang::Num(_) => expr.add((*c).clone()),
                        s @ Lang::Symbol(_) => expr.add((*s).clone()),
                        s @ Lang::Rand => expr.add((*s).clone()),
                        u @ Lang::Undef => expr.add((*u).clone()),
                        d @ Lang::Drop => expr.add((*d).clone()),
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
