use std::{collections::HashMap, hash::Hash};

use wasmparser::{Operator, Range};

use super::TupleType;

/// It executes a minimal symbolic evaluation of the stack to detect operands location in the code for certain operators
/// For example, i.add operator should know who are its operands
pub struct DFGIcator {}

#[derive(Debug)]
pub struct BBlock {
    range: Range,
}

#[derive(Debug, Clone)]
pub struct StackEntry {
    pub operator_idx: Option<usize>,
    pub data: Box<StackEntryData>,
    pub byte_stream_range: Range,
}

#[derive(Debug, Clone)]
pub enum StackEntryData {
    Leaf,
    Node(Vec<usize>),
    Unknown,
}

#[derive(Debug)]
pub struct MiniDFG {
    pub map: HashMap<usize, usize>,
    pub entries: Vec<StackEntry>,
}

impl<'a> DFGIcator {
    pub fn new() -> Self {
        DFGIcator {}
    }

    /// Linear algorithm  to detect basic blocks
    /// This follows the tradition way to detect them
    /// 1 - Every jump instruction creates a new BB right after (in wasm: br, br_if, loop, block, if, else)
    /// 2 - Every operator that could be a target of a jump also starts a BB (end ... :) Wasm always jumps to end)
    /// 3 - The first operator is the start of a BB
    pub fn collect_bbs(
        &self,
        operators: &[TupleType],
        onbb: Option<&dyn Fn(&BBlock)>,
    ) -> crate::Result<Vec<BBlock>> {
        println!("{:?}", operators);

        let (mut bbs, lastidx) = operators.iter().enumerate().fold(
            (Vec::<BBlock>::new(), 0),
            |(mut bbs, start), (opidx, (operator, _))| {
                match operator {
                Operator::If{..}
                | Operator::Else{..}
                | Operator::End
                | Operator::Block { .. }
                | Operator::Loop { .. }
                | Operator::Br { .. }
                | Operator::BrIf { .. }
                | Operator::BrTable { .. }
                // | add other JMP possible instructions
                => {
                    // close current bb here at this instruction
                    // start a new bb in the next instruction
                    let newBB = BBlock{
                        range: Range{start: start, end: opidx + 1}, // The end of the range should not be included
                    };
                    if let Some(cb) = onbb {
                        cb(&newBB);
                    }
                    bbs.push(newBB);
                    (bbs, opidx + 1)
                }
                _ => {
                    (bbs, start)
                }
            }
            },
        );

        // Assert we have all instructions included
        assert_eq!(lastidx, operators.len());

        Ok(bbs)
    }

    fn push_leaf(
        operator_idx: usize,
        start: usize,
        end: usize,
        dfg_map: &mut Vec<StackEntry>,
        operatormap: &mut HashMap<usize, usize>,
        stack: &mut Vec<usize>,
    ) -> usize {
        let leaf = StackEntry {
            operator_idx: Some(operator_idx),
            byte_stream_range: Range { start, end },
            data: Box::new(StackEntryData::Leaf),
        };
        let entry_idx = dfg_map.len();
        operatormap.insert(operator_idx, entry_idx);
        stack.push(entry_idx);
        // Add the data flow link
        dfg_map.push(leaf);
        entry_idx
    }

    fn push_node(
        operator_idx: usize,
        start: usize,
        end: usize,
        dfg_map: &mut Vec<StackEntry>,
        operatormap: &mut HashMap<usize, usize>,
        stack: &mut Vec<usize>,
        operands: Vec<usize>,
    ) -> usize {
        let newnode = StackEntry {
            operator_idx: Some(operator_idx),
            byte_stream_range: Range { start, end },
            data: Box::new(StackEntryData::Node(operands)),
        };

        let entry_idx = dfg_map.len();
        operatormap.insert(operator_idx, entry_idx);
        stack.push(entry_idx);
        // Add the data flow link
        dfg_map.push(newnode);
        entry_idx
    }

    fn pop_operand(
        stack: &mut Vec<usize>,
        dfg_map: &mut Vec<StackEntry>,
        operator_idx: usize,
        operatormap: &mut HashMap<usize, usize>,
        insertindfg: bool,
    ) -> usize {
        let idx = stack
            .pop()
            .or_else(|| {
                // Since this represents the same for all
                // Create 0 element as Unknown
                let leaf = StackEntry {
                    operator_idx: Some(operator_idx),
                    data: Box::new(StackEntryData::Unknown),
                    byte_stream_range: Range::new(0, 0),
                }; // Means not reachable
                let entry_idx = dfg_map.len();
                if insertindfg {
                    operatormap.insert(operator_idx, entry_idx);
                }
                //
                stack.push(entry_idx);
                // Add the data flow link
                dfg_map.push(leaf);
                Some(entry_idx)
            })
            .unwrap();
        idx
    }

    fn getsimpleterm(&mut self, operator: &Operator) -> String {
        match operator {
            Operator::LocalGet { local_index } => format!("?l{}", local_index),
            Operator::GlobalGet { global_index } => format!("?g{}", global_index),
            Operator::LocalSet { .. } => "local.set".into(),
            Operator::I32Load { .. } => "i.load".into(),
            Operator::I32Const { value } => {
                format!("{}", value)
            }
            Operator::I32Add => "i32.add".into(),
            Operator::I32Shl => "i32.shl".into(),
            _ => todo!("{:?} is missing", operator),
        }
        .to_string()
    }

    /// This method should build lane dfg information
    /// It returns a map of operator indexes over the function operators,
    /// in which every key refers to a vector of ranges determining the operands
    /// in the code
    ///
    /// This process can is done inside basic blocsks, control flow information
    /// is not taken into account in the peephole mutators
    pub fn get_fulldfg(&mut self, operators: &'a [TupleType]) -> crate::Result<MiniDFG> {
        let basic_blocks = self.collect_bbs(operators, None)?;

        // TODO, check memory explotion of this
        // lets handle the stack
        let mut dfg_map = Vec::new();

        let mut operatormap: HashMap<usize, usize> = HashMap::new(); // operator index to stack index

        for bb in basic_blocks {
            println!("{:?}", bb);
            let mut stack: Vec<usize> = Vec::new();

            // Create a DFG from the BB
            // Start from the first operator and simulate the stack...
            // If an operator is missing in the stack then it probably comes from a previous BB

            for idx in bb.range.start..bb.range.end - 1 {
                // We dont care about the jump
                let (operator, byte_offset) = &operators[idx];
                let (_, byte_offset_next) = &operators[idx + 1];

                match operator {
                    Operator::GlobalGet { .. } | Operator::LocalGet { .. } => {
                        DFGIcator::push_leaf(
                            idx,
                            *byte_offset,
                            *byte_offset_next,
                            &mut dfg_map,
                            &mut operatormap,
                            &mut stack,
                        );
                    }
                    Operator::I32Const { value } => {
                        DFGIcator::push_leaf(
                            idx,
                            *byte_offset,
                            *byte_offset_next,
                            &mut dfg_map,
                            &mut operatormap,
                            &mut stack,
                        );
                    }
                    // Watch out, type information is missing here
                    Operator::LocalSet { .. } | Operator::GlobalSet { .. } | Operator::Drop => {
                        // It needs the offset arg
                        let idx = DFGIcator::pop_operand(
                            &mut stack,
                            &mut dfg_map,
                            idx,
                            &mut operatormap,
                            true,
                        );

                        println!("{:?}", idx);

                        // Pop but still add if to dfg
                        /*
                        DFGIcator::push_node(
                                // construct the full eterm here ?
                            format!("({} {})", DFGIcator::getsimpleterm(operator, termindex), dfg_map[offset].eterm,),
                            idx,
                            *byte_offset,
                            *byte_offset_next,
                            &mut dfg_map,
                            &mut operatormap,
                            &mut stack,
                            vec![offset]
                        ); */
                    }
                    // All memory loads
                    Operator::I32Load { .. }
                    | Operator::I64Load { .. }
                    | Operator::F32Load { .. }
                    | Operator::F64Load { .. }
                    | Operator::I32Load16S { .. }
                    | Operator::I32Load16U { .. }
                    | Operator::I32Load8U { .. }
                    | Operator::I32Load8S { .. }
                    | Operator::I64Load32S { .. }
                    | Operator::I64Load32U { .. }
                    | Operator::I64Load8S { .. }
                    | Operator::I64Load8U { .. } => {
                        // It needs the offset arg
                        let offset = DFGIcator::pop_operand(
                            &mut stack,
                            &mut dfg_map,
                            idx,
                            &mut operatormap,
                            false,
                        );
                        DFGIcator::push_node(
                            idx,
                            *byte_offset,
                            *byte_offset_next,
                            &mut dfg_map,
                            &mut operatormap,
                            &mut stack,
                            vec![offset],
                        );
                    }
                    Operator::I32Add
                    | Operator::I32Sub
                    | Operator::I32Mul
                    | Operator::I32DivS
                    | Operator::I32DivU
                    | Operator::I32Shl
                    | Operator::I32ShrS
                    | Operator::I32ShrU => {
                        println!("");
                        println!("{:?}", stack);
                        let leftidx = DFGIcator::pop_operand(
                            &mut stack,
                            &mut dfg_map,
                            idx,
                            &mut operatormap,
                            true,
                        );
                        let rightidx = DFGIcator::pop_operand(
                            &mut stack,
                            &mut dfg_map,
                            idx,
                            &mut operatormap,
                            true,
                        );

                        // The operands should not be the same
                        assert_ne!(leftidx, rightidx);

                        DFGIcator::push_node(
                            idx,
                            *byte_offset,
                            *byte_offset_next,
                            &mut dfg_map,
                            &mut operatormap,
                            &mut stack,
                            vec![leftidx, rightidx], // reverse order
                        );
                    }

                    _ => {
                        todo!("Not implemented yet {:?}", operator)
                    }
                }
            }
        }

        println!("{:?}", operatormap);
        for (idx, entry) in dfg_map.iter().enumerate() {
            println!("{} {:?}", idx, entry);
        }

        Ok(MiniDFG {
            entries: dfg_map,
            map: operatormap,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use wasmparser::Parser;

    use crate::{mutators::peephole::TupleType, WasmMutate};

    use super::DFGIcator;

    #[test]
    fn test_dfg_bbextractor() {
        // A decent complex Wasm function
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                i32.load
                if 
                    i32.const 54
                else
                    i32.const 87
                end
                i32.const 56
                i32.add
                loop
                    i32.const 1
                    local.get 0
                    i32.add
                    local.set 0
                end
            )
        )
        "#,
        )
        .unwrap();

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        loop {
            let (payload, size) = match parser.parse(&original[consumed..], true).unwrap() {
                wasmparser::Chunk::NeedMoreData(_) => {
                    panic!("This should not happen")
                }
                wasmparser::Chunk::Parsed { consumed, payload } => (payload, consumed),
            };

            consumed += size;

            match payload {
                wasmparser::Payload::CodeSectionEntry(reader) => {
                    let operators = reader
                        .get_operators_reader()
                        .unwrap()
                        .into_iter_with_offsets()
                        .collect::<wasmparser::Result<Vec<TupleType>>>()
                        .unwrap();

                    let basic_blocks = DFGIcator::new()
                        .collect_bbs(
                            &operators,
                            Some(&|bb| {
                                println!("{:?}", bb);
                                // Create a dfg from the BB
                                // Start from the first operator and simulate the stack...
                                // If an operator is missing in the stack then it comes from a previous BB
                                // If the stack is unconsistent then it means that the basic block is a joint for several...
                                println!("{:?}", &operators[bb.range.start..bb.range.end]);
                            }),
                        )
                        .unwrap();

                    assert_eq!(basic_blocks.len(), 6);
                }
                wasmparser::Payload::End => {
                    break;
                }
                _ => {
                    // Do nothing
                }
            }
        }
    }

    #[test]
    fn test_dfg_build() {
        // A decent complex Wasm function
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                
                local.get 0
                local.get 0
                i32.add
                i32.load
                if 
                    i32.const 54
                else
                    i32.const 87
                end
                i32.const 56
                i32.add
                loop
                    i32.const 1
                    local.get 0
                    i32.add
                    local.set 0
                end
            )
        )
        "#,
        )
        .unwrap();

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        loop {
            let (payload, size) = match parser.parse(&original[consumed..], true).unwrap() {
                wasmparser::Chunk::NeedMoreData(_) => {
                    panic!("This should not happen")
                }
                wasmparser::Chunk::Parsed { consumed, payload } => (payload, consumed),
            };

            consumed += size;

            match payload {
                wasmparser::Payload::CodeSectionEntry(reader) => {
                    let operators = reader
                        .get_operators_reader()
                        .unwrap()
                        .into_iter_with_offsets()
                        .collect::<wasmparser::Result<Vec<TupleType>>>()
                        .unwrap();

                    let roots = DFGIcator::new().get_fulldfg(&operators).unwrap();

                    println!("{:?}", roots);
                    //todo!();
                }
                wasmparser::Payload::End => {
                    break;
                }
                _ => {
                    // Do nothing
                }
            }
        }
    }

    #[test]
    fn test_dfg_build1() {
        // A decent complex Wasm function
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                i32.const 32
                drop
                local.get 0
                local.get 0
                i32.add
                i32.add
            )
        )
        "#,
        )
        .unwrap();

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        loop {
            let (payload, size) = match parser.parse(&original[consumed..], true).unwrap() {
                wasmparser::Chunk::NeedMoreData(_) => {
                    panic!("This should not happen")
                }
                wasmparser::Chunk::Parsed { consumed, payload } => (payload, consumed),
            };

            consumed += size;

            match payload {
                wasmparser::Payload::CodeSectionEntry(reader) => {
                    let operators = reader
                        .get_operators_reader()
                        .unwrap()
                        .into_iter_with_offsets()
                        .collect::<wasmparser::Result<Vec<TupleType>>>()
                        .unwrap();

                    let roots = DFGIcator::new().get_fulldfg(&operators).unwrap();

                    println!("{:?}", roots);
                    //todo!();
                }
                wasmparser::Payload::End => {
                    break;
                }
                _ => {
                    // Do nothing
                }
            }
        }
    }
}
