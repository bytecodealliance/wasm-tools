//! Helper methods for encoding eterm expressions to Wasm and back

use std::{collections::HashMap, num::Wrapping};

#[cfg(not(test))]
use log::debug;
#[cfg(test)]
use std::println as debug;

use egg::{Id, RecExpr};
use rand::Rng;
use wasm_encoder::{Function, Instruction};
use wasmparser::Operator;

use crate::module::PrimitiveTypeInfo;
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

macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

/// Turns wasm to eterm and back
pub struct Encoder;

macro_rules! eterm_operator_2_wasm {
    (@expand
        $eclassdata:expr, $newfunc:expr, $parent_eclass:expr, $nodes:expr, $root: expr,
        { $($target:tt)* }
    ) => {
           $(
               $target
           )*
    };
    (@expand
        $eclassdata:expr, $newfunc:expr, $parent_eclass: expr, $nodes:expr, $root: expr,
        $(
            $case:pat => [$($target:tt)+]
        )*
        ) => {
            match $eclassdata {
                Some(data) => {
                    match data.tpes[..] {
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
                        _ => Err(crate::Error::UnsupportedType(EitherType::EggError(format!("There is no conversion between the eclass returning type ({:?}) the the available Wasm operators", data.tpes))))
                    }
                }
                None => {
                    // Try with the parent eclass information
                    match $parent_eclass {
                        Some(data) => {
                            match data.tpes[..] {
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
                                _ => Err(crate::Error::UnsupportedType(EitherType::EggError(format!("There is no conversion between the eclass returning type ({:?}) the the available Wasm operators", data.tpes))))
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
            @expand $eclassdata, $newfunc, $parenteclass, $nodes, $root, $(
                $body
            )*
        }
    };
    // Single operand
    (@write_subtree $eclassdata:expr, $newfunc:expr,  $info:expr, $rnd: expr, $nodes: expr, $root:expr, $node_to_eclass:expr, $operators:expr, $egraph: expr,$parenteclass: expr, ( $operand:expr ), $name4node:ident, $namefornewfunc:ident, $nameforrnd:ident, $namefor_eclass:ident, $namefor_rootclass:ident, $name4egraph: ident, $name4info: ident, $name4operators: ident
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

        eterm_operator_2_wasm! {
            @expand $eclassdata, $newfunc , $parenteclass, $nodes, $root, $(
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
            @expand $eclassdata, $newfunc, $parenteclass, $nodes, $root, $(
                $body
            )*
        }
    };
    (@write_subtree $eclassdata:expr, $newfunc:expr,  $info:expr, $rnd: expr, $nodes: expr,  $root:expr, $node_to_eclass:expr, $operators:expr, $egraph: expr, $parenteclass: expr, $name: ident, $name4node:ident, $namefornewfunc:ident, $nameforrnd:ident, $namefor_eclass:ident, $namefor_rootclass:ident, $name4egraph: ident, $name4info: ident, $name4operators: ident
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
        eterm_operator_2_wasm! {
            @expand $eclassdata, $newfunc, $parenteclass, $nodes, $root, $(
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
    // Lang to Wasm conversion of simple operators
    // Lang::item(_) => {
    //     [ PrimitiveType::item ] => [ Instruction::type ]
    //  }
    eterm_operator_2_wasm! {
        [Lang::Or(operands), [operands], /* how many operands */ 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Or]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Or]

        }
        [Lang::And(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32And]
            [PrimitiveTypeInfo::I64] => [Instruction::I64And]

        }
        // To add a new mapping
        [Lang::Mul(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Mul]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Mul]

        }

        [Lang::Add(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Add]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Add]

        }

        [Lang::ShrU(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32ShrU]
            [PrimitiveTypeInfo::I64] => [Instruction::I64ShrU]

        }
        [Lang::Shl(operands), [operands], 2] => {

            [PrimitiveTypeInfo::I32] => [Instruction::I32Shl]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Shl]

        }
        [Lang::ILoad(operand), (*operand) /*between parenthesis means that this operand will be written down*/, _nodes, newfunc, _rnd, eclassdata, _rootclassdata, egraph, info, operators] => {{

            let entry = eclassdata.clone().unwrap().get_stack_entry(&egraph.analysis);
            let operatoridx = entry.operator_idx;
            let operators = &operators[operatoridx..=operatoridx + 1/* take to the next operator to save the offset */];
            let range = (operators[0].1, operators[1].1);

            // Copy the mem operation
            let raw_data = &info.get_code_section().data[range.0..range.1];
            debug!("Mem operator raw {:?} {:?}", raw_data, operators);
            newfunc.raw(raw_data.iter().copied());

            Ok(())
        }}

        [Lang::Num(value), value] => {
            [PrimitiveTypeInfo::I32] => [Instruction::I32Const(*value as i32)]
            [PrimitiveTypeInfo::I64] => [Instruction::I64Const(*value)]
        }
        [Lang::Rand, value, n, newfunc, rnd, eclassdata, rootdata, egraph, info, operators] => {{
            newfunc.instruction(Instruction::I32Const(rnd.gen()));
            Ok(())
        }}
        [Lang::Symbol(s), s, n, newfunc, rnd, eclassdata, rootclassdata, egraph, info, operators] => {{
            let entry = &egraph.analysis.get_stack_entry_from_symbol(s.to_string());
            match entry {
                Some(entry) => {
                    // Entry could not be an indepent symbol
                    if entry.is_leaf() {
                        let bytes = &info.get_code_section().data
                            [entry.byte_stream_range.start..entry.byte_stream_range.end];
                        debug!("Symbol {:?}, raw bytes: {:?}", s, bytes);
                        newfunc.raw(bytes.iter().copied());
                        Ok(())
                    } else {
                        return Err(crate::Error::UnsupportedType(EitherType::TypeDef(format!("A leaf stack entry that cannot be mapped directly to a Symbol is not right"))));
                    }
                }
                None => {
                    return Err(crate::Error::UnsupportedType(EitherType::TypeDef(format!("A leaf stack entry that cannot be mapped directly to a Symbol is not right"))));
                }
            }
        }}
        [Lang::Unfold(value), value, n, newfunc, rnd, eclassdata, rootclassdata, egraph, info, operators] => {{
            let child = &n[usize::from(*value)];
            match child {
                Lang::Num(value) => {
                    // getting type from eclass
                    match eclassdata {
                        Some(data) => {
                            match data.tpes[..]{
                                [PrimitiveTypeInfo::I64] => {
                                    let r: i64 = rnd.gen();
                                    debug!("Unfolding {:?}", value);
                                    newfunc.instruction(Instruction::I64Const(r));
                                    newfunc.instruction(Instruction::I64Const((Wrapping(*value) - Wrapping(r)).0));
                                    newfunc.instruction(Instruction::I64Add);
                                    Ok(())
                                },
                                [PrimitiveTypeInfo::I32] => {
                                    let r: i32 = rnd.gen();
                                    debug!("Unfolding {:?}", value);
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

        fn expr2wasm_aux(
            info: &ModuleInfo,
            rnd: &mut rand::prelude::SmallRng,
            nodes: &[Lang],
            node_to_eclass: &Vec<Id>,
            current: Id,
            newfunc: &mut Function,
            // the following four parameters could be moved to the PeepholeAnalysis
            operators: &Vec<OperatorAndByteOffset>,
            egraph: &EG,
        ) -> crate::Result<()> {
            let root = &nodes[usize::from(current)];
            let eclass = node_to_eclass[usize::from(current)];
            let data = &egraph[eclass].data;

            match root {
                Lang::Add(operands)
                | Lang::Sub(operands)
                | Lang::Mul(operands)
                | Lang::Shl(operands)
                | Lang::And(operands)
                | Lang::Xor(operands)
                | Lang::Or(operands)
                | Lang::ShrU(operands) => {
                    // Process operands
                    for op in operands {
                        expr2wasm_aux(
                            info,
                            rnd,
                            nodes,
                            node_to_eclass,
                            *op,
                            newfunc,
                            operators,
                            egraph,
                        )?;
                    }
                    // Write the current operator
                    debug!("Type of the node {:?}", data);
                    //Encoder::write_operator2wasm(data, root, newfunc)?;
                }
                Lang::Popcnt(operand) => {
                    // Process right operand

                    expr2wasm_aux(
                        info,
                        rnd,
                        nodes,
                        node_to_eclass,
                        *operand,
                        newfunc,
                        operators,
                        egraph,
                    )?;
                    newfunc.instruction(Instruction::I32Popcnt);
                }
                Lang::ILoad(operand) => {
                    // Write memory load operations
                    expr2wasm_aux(
                        info,
                        rnd,
                        nodes,
                        node_to_eclass,
                        *operand,
                        newfunc,
                        operators,
                        egraph,
                    )?;

                    match data {
                        Some(x) => {
                            let entry = x.get_stack_entry(&egraph.analysis);
                            let operatoridx = entry.operator_idx;
                            let operators = &operators[operatoridx..=operatoridx + 1/* take to the next operator to save the offset */];
                            let range = (operators[0].1, operators[1].1);

                            // Copy the mem operation
                            let raw_data = &info.get_code_section().data[range.0..range.1];
                            debug!("Mem operator raw {:?} {:?}", raw_data, operators);
                            newfunc.raw(raw_data.iter().copied());
                        }
                        None => unreachable!("There is not information for the memoryload"),
                    }
                }
                Lang::Rand => {
                    newfunc.instruction(Instruction::I32Const(rnd.gen()));
                }
                Lang::Undef => {
                    debug!("Undefined value reached, this means that the operand will come from the evaluation of pred basic blocks");
                }
                Lang::Unfold(operand) => {
                    let child = &nodes[usize::from(*operand)];
                    /*match child {
                        Lang::I32Const(value) => {
                            let r: i32 = rnd.gen();
                            debug!("Unfolding {:?}", value);
                            newfunc.instruction(Instruction::I32Const(r));
                            newfunc.instruction(Instruction::I32Const((Wrapping(*value) - Wrapping(r)).0));
                            newfunc.instruction(Instruction::I32Add);
                        },
                        _ => unreachable!("The operand for this operator should be a constant, check if the rewriting rule is defined with such conditions")
                    }*/
                    todo!();
                }
                Lang::Num(val) => {
                    //newfunc.instruction(Instruction::I32Const(*val));
                }
                Lang::Symbol(s) => {
                    // Copy the byte stream to aavoid mapping
                    let entry = &egraph.analysis.get_stack_entry_from_symbol(s.to_string());
                    match entry {
                        Some(entry) => {
                            // Entry could not be an indepent symbol
                            if entry.is_leaf() {
                                let bytes = &info.get_code_section().data
                                    [entry.byte_stream_range.start..entry.byte_stream_range.end];
                                debug!("Symbol {:?}, raw bytes: {:?}", s, bytes);
                                newfunc.raw(bytes.iter().copied());
                            } else {
                                return Err(crate::Error::UnsupportedType(EitherType::TypeDef(format!("A leaf stack entry that cannot be mapped directly to a Symbol is not right"))));
                            }
                        }
                        None => {
                            // TODO change this to the right error
                            return Err(crate::Error::NotMatchingPeepholes);
                        }
                    }
                }
                Lang::Drop => {
                    newfunc.instruction(Instruction::Drop);
                }
            }

            Ok(())
        }

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
        entryidx: usize,
        newfunc: &mut Function,
    ) -> crate::Result<()> {
        // Write the deps in the dfg
        // Process operands
        if entry.is_undef {
            // Do nothing
            // log
        } else {
            for idx in &entry.operands {
                let entry = &egraph.analysis.get_stack_entry(*idx);
                Encoder::writestackentry(info, egraph, entry, *idx, newfunc)?;
            }
            // Write the operator
            let bytes = &info.get_code_section().data
                [entry.byte_stream_range.start..entry.byte_stream_range.end];
            newfunc.raw(bytes.iter().copied());
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
                    debug!("Writing mutation");
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
                    debug!("writing no mutated DFG at {:?}", entry.operator_idx);
                    Encoder::writestackentry(info, &egraph, entry, entryidx, newfunc)?;
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
    /// This method receives also a random generator, the idea is to map StackEntry operands to symbols in a random way.
    pub fn wasm2expr(
        dfg: &MiniDFG,
        oidx: usize,
        operators: &Vec<OperatorAndByteOffset>,
        // The wasm expressions will be added here
        expr: &mut RecExpr<Lang>, // Replace this by RecExpr
    ) -> crate::Result<(Id, HashMap<String, usize>, HashMap<Id, usize>)> {
        let stack_entry_index = dfg.map[&oidx];
        debug!("Starting at operator {:?}", operators[oidx]);
        fn wasm2expraux(
            dfg: &MiniDFG,
            entryidx: usize,
            operators: &Vec<OperatorAndByteOffset>,
            // The wasm expressions will be added here
            expr: &mut RecExpr<Lang>, // Replace this by RecExpr
        ) -> crate::Result<(Id, HashMap<String, usize>, HashMap<Id, usize>)> {
            let entry = &dfg.entries[entryidx];
            if entry.is_undef {
                let undefid = expr.add(Lang::Undef);
                return Ok((
                    undefid,
                    HashMap::new(),
                    hashmap![undefid => entry.entry_idx],
                ));
            }
            if entry.is_leaf() {
                let (operator, _) = &operators[entry.operator_idx];
                match operator {
                    Operator::I32Const { value } => {
                        let id = expr.add(Lang::Num(*value as i64));
                        Ok((
                            id,
                            HashMap::new(), // No symbol,
                            hashmap![id => entry.entry_idx],
                        ))
                    }
                    Operator::LocalGet { local_index } => {
                        let name = format!("?l{}", local_index);
                        let id = expr.add(Lang::Symbol(name.clone().into()));
                        let mut smap = HashMap::new();
                        smap.insert(name, entryidx);

                        Ok((id, smap, hashmap![id => entry.entry_idx]))
                    }
                    _ => Err(crate::Error::UnsupportedType(EitherType::Operator(
                        format!("Operator {:?} is not supported as symbol", operator),
                    ))),
                }
            } else {
                let (operator, _) = &operators[entry.operator_idx];
                let mut subexpressions = Vec::new();
                let mut smap: HashMap<String, usize> = HashMap::new();
                let mut id_to_stack = hashmap![];
                for operandi in &entry.operands {
                    debug!("operand index {}, entries {:?}", operandi, &dfg.entries);
                    let (eterm, symbols, id_to_stackentry) =
                        wasm2expraux(dfg, *operandi, operators, expr)?;
                    subexpressions.push(eterm);
                    smap.extend(symbols.into_iter());
                    id_to_stack.extend(id_to_stackentry);
                }
                let operatorid = match operator {
                    Operator::I32Shl
                    | Operator::I32Add 
                    | Operator::I64Add
                    | Operator::I32Sub 
                    | Operator::I32ShrU 
                    | Operator::I32And 
                    | Operator::I32Or 
                    | Operator::I32Xor 
                    => {
                        // Check this node has only two child
                        debug_assert_eq!(subexpressions.len(), 2);
                        match operator {
                            Operator::I32Add => Ok(expr.add(Lang::Add([subexpressions[0], subexpressions[1]]))),
                            Operator::I32Shl => Ok(expr.add(Lang::Shl([subexpressions[0], subexpressions[1]]))),
                            Operator::I32Sub => Ok(expr.add(Lang::Sub([subexpressions[0], subexpressions[1]]))),
                            Operator::I32ShrU => Ok(expr.add(Lang::ShrU([subexpressions[0], subexpressions[1]]))),
                            Operator::I32Or => Ok(expr.add(Lang::Or([subexpressions[0], subexpressions[1]]))),
                            Operator::I32And => Ok(expr.add(Lang::And([subexpressions[0], subexpressions[1]]))),
                            Operator::I32Xor => Ok(expr.add(Lang::Xor([subexpressions[0], subexpressions[1]]))),
                            Operator::I64Add => Ok(expr.add(Lang::Add([subexpressions[0], subexpressions[1]]))),
                            _ => unreachable!()
                        }
                    }
                    Operator::I32Load { .. } 
                    | Operator::I64Load { .. } 
                    // TODO add others
                    => {
                        debug_assert_eq!(subexpressions.len(), 1);
                        Ok(expr.add(Lang::ILoad(subexpressions[0])))
                    }
                    Operator::I32Popcnt { .. } 
                    => {
                        debug_assert_eq!(subexpressions.len(), 1);
                        Ok(expr.add(Lang::Popcnt(subexpressions[0])))
                    }
                    Operator::Drop => Ok(expr.add(Lang::Drop)),
                    _ => Err(crate::Error::UnsupportedType(EitherType::Operator(
                        format!("The operator {:?} cannot be mapped to egg lang", operator),
                    ))),
                }?;
                id_to_stack.insert(operatorid, entry.entry_idx);
                Ok((operatorid, smap, id_to_stack))
            }
        }
        wasm2expraux(dfg, stack_entry_index, operators, expr)
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
                    let sub_expr_id = match &id_to_node[usize::from(node)].0 {
                        Lang::Add(_) => expr.add(Lang::Add([operand(0), operand(1)])),
                        Lang::Sub(_) => expr.add(Lang::Sub([operand(0), operand(1)])),
                        Lang::Mul(_) => expr.add(Lang::Mul([operand(0), operand(1)])),
                        Lang::And(_) => expr.add(Lang::And([operand(0), operand(1)])),
                        Lang::Or(_) => expr.add(Lang::Or([operand(0), operand(1)])),
                        Lang::Xor(_) => expr.add(Lang::Xor([operand(0), operand(1)])),
                        Lang::Shl(_) => expr.add(Lang::Shl([operand(0), operand(1)])),
                        Lang::ShrU(_) => expr.add(Lang::ShrU([operand(0), operand(1)])),
                        Lang::Popcnt(_) => expr.add(Lang::Popcnt(operand(0))),
                        Lang::Unfold(op) => expr.add(Lang::Unfold(*op)),
                        Lang::ILoad(_) => expr.add(Lang::ILoad(operand(0))),
                        c @ Lang::Num(_) => expr.add((*c).clone()),
                        s @ Lang::Symbol(_) => expr.add((*s).clone()),
                        s @ Lang::Rand => expr.add((*s).clone()),
                        u @ Lang::Undef => expr.add((*u).clone()),
                        d @ Lang::Drop => expr.add((*d).clone()),
                    };
                    let eclass = &id_to_node[usize::from(node)].1;
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
