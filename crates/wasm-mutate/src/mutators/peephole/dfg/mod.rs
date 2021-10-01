use std::{collections::HashMap, hash::Hash};

use wasmparser::{Operator, Range};

use super::OperatorAndByteOffset;

/// It executes a minimal symbolic evaluation of the stack to detect operands location in the code for certain operators
/// For example, i.add operator should know who are its operands
pub struct DFGIcator {}

#[derive(Debug)]
pub struct BBlock {
    pub(crate) range: Range,
}

#[derive(Debug, Clone)]
pub struct StackEntry {
    pub operator_idx: usize,
    pub data: StackEntryData,
    pub byte_stream_range: Range,
}

#[derive(Debug, Clone)]
pub enum StackEntryData {
    Leaf,
    Node { operands: Vec<usize> },
    Unknown,
}

#[derive(Debug)]
pub struct MiniDFG {
    // Some of the operators have no stack entry
    // This will help to decide or not to mutate the operators, avoiding egrapphp creation, etc
    // Each (key, value) entry corresponds to the index of the instruction in the Wasm BasicBlock and the index of the stack entry in the `entries` field
    pub map: HashMap<usize, usize>,
    // Each stack entry represents a position in the operators stream
    // containing its children
    pub entries: Vec<StackEntry>,
    // For each stack entry we keep the parental relation, the ith value is the index of the ith instruction's parent instruction
    // We write each stack entry having no parent, i.e. a root in the dfg
    pub parents: Vec<i32>,
}

impl<'a> DFGIcator {
    pub fn new() -> Self {
        DFGIcator {}
    }

    /// Linear algorithm  to detect the basic block
    /// This follows the tradition way to detect them
    /// 1 - Every jump instruction creates a new BB right after (in wasm: br, br_if, loop, block, if, else)
    /// 2 - Every operator that could be a target of a jump also starts a BB (end ... :) Wasm always jumps to end)
    /// 3 - The first operator is the start of a BB
    ///
    /// However, since we only need the current basic block,
    /// the iteration over the operators will be done upward until the basic block starts
    pub fn get_bb_for_operator(
        &self,
        operator_index: usize,
        operators: &[OperatorAndByteOffset],
    ) -> Option<BBlock> {
        let mut range = Range {
            start: operator_index,
            end: operator_index,
        };
        // We only need the basic block upward
        loop {
            let (operator, _) = &operators[range.start];
            match operator {
                Operator::If { .. }
                | Operator::Else { .. }
                | Operator::End
                | Operator::Block { .. }
                | Operator::Loop { .. }
                | Operator::Br { .. }
                | Operator::BrIf { .. }
                | Operator::BrTable { .. } => {
                    break;
                }
                _ => {
                    if range.start > 0 {
                        range.start -= 1;
                    } else {
                        break;
                    }
                }
            }
        }

        if range.end - range.start > 0 {
            Some(BBlock { range })
        } else {
            // It only contains the jump
            // This will help to filter out which operator can be mmutated or not in the PeepholeMutator process
            None
        }
    }

    fn push_leaf(
        operator_idx: usize,
        start: usize,
        end: usize,
        dfg_map: &mut Vec<StackEntry>,
        operatormap: &mut HashMap<usize, usize>,
        stack: &mut Vec<usize>,
        parents: &mut Vec<i32>,
    ) -> usize {
        let leaf = StackEntry {
            operator_idx: operator_idx,
            byte_stream_range: Range { start, end },
            data: StackEntryData::Leaf,
        };
        let entry_idx = dfg_map.len();
        operatormap.insert(operator_idx, entry_idx);
        stack.push(entry_idx);
        // Add the data flow link
        dfg_map.push(leaf);
        parents.push(-1);
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
        parents: &mut Vec<i32>,
    ) -> usize {
        let newnode = StackEntry {
            operator_idx: operator_idx,
            byte_stream_range: Range { start, end },
            data: StackEntryData::Node { operands },
        };

        let entry_idx = dfg_map.len();
        operatormap.insert(operator_idx, entry_idx);
        stack.push(entry_idx);
        // Add the data flow link
        dfg_map.push(newnode);
        parents.push(-1);
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
                    operator_idx: operator_idx,
                    data: StackEntryData::Unknown,
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

    /// This method should build lane dfg information
    /// It returns a map of operator indexes over the function operators,
    /// in which every key refers to a vector of ranges determining the operands
    /// in the code
    ///
    /// This process can is done inside basic blocsks, control flow information
    /// is not taken into account in the peephole mutators
    pub fn get_dfg(
        &mut self,
        operators: &'a [OperatorAndByteOffset],
        basicblock: &BBlock,
    ) -> crate::Result<MiniDFG> {
        // TODO, check memory explotion of this
        // lets handle the stack
        let mut dfg_map = Vec::new();
        let mut operatormap: HashMap<usize, usize> = HashMap::new(); // operator index to stack index
        let mut stack: Vec<usize> = Vec::new();
        let mut parents: Vec<i32> = Vec::new();

        // Create a DFG from the BB
        // Start from the first operator and simulate the stack...
        // If an operator is missing in the stack then it probably comes from a previous BB

        for idx in basicblock.range.start..basicblock.range.end - 1 {
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
                        &mut parents,
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
                        &mut parents,
                    );
                }
                // Watch out, type information is missing here
                Operator::LocalSet { .. } | Operator::GlobalSet { .. } | Operator::Drop => {
                    // It needs the offset arg
                    let child = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        true,
                    );

                    let newnode = StackEntry {
                        operator_idx: idx,
                        byte_stream_range: Range {
                            start: *byte_offset,
                            end: *byte_offset_next,
                        },
                        data: StackEntryData::Node {
                            operands: vec![child],
                        },
                    };

                    let entry_idx = dfg_map.len();
                    operatormap.insert(idx, entry_idx);
                    // Add the data flow link
                    dfg_map.push(newnode);
                    parents.push(-1);

                    parents[child] = idx as i32;
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
                    let idx = DFGIcator::push_node(
                        idx,
                        *byte_offset,
                        *byte_offset_next,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![offset],
                        &mut parents,
                    );

                    parents[offset] = idx as i32;
                }
                Operator::I32Add
                | Operator::I32Sub
                | Operator::I32Mul
                | Operator::I32DivS
                | Operator::I32DivU
                | Operator::I32Shl
                | Operator::I32ShrS
                | Operator::I32ShrU => {
                    let leftidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        false,
                    );
                    let rightidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        false,
                    );

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = DFGIcator::push_node(
                        idx,
                        *byte_offset,
                        *byte_offset_next,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![rightidx, leftidx], // reverse order
                        &mut parents,
                    );

                    parents[leftidx] = idx as i32;
                    parents[rightidx] = idx as i32;
                }

                _ => {
                    todo!("Not implemented yet {:?}", operator)
                }
            }
        }

        println!("parents {:?}", parents);
        Ok(MiniDFG {
            entries: dfg_map,
            map: operatormap,
            parents,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use wasmparser::Parser;

    use crate::{mutators::peephole::OperatorAndByteOffset, WasmMutate};

    use super::DFGIcator;

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
                        .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()
                        .unwrap();

                    let bb = DFGIcator::new().get_bb_for_operator(0, &operators).unwrap();
                    let roots = DFGIcator::new().get_dfg(&operators, &bb).unwrap();
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
    fn test_dfg_getsinglebb() {
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
                        .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()
                        .unwrap();

                    let root = DFGIcator::new().get_bb_for_operator(5, &operators).unwrap();
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
                        .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()
                        .unwrap();

                    let bb = DFGIcator::new().get_bb_for_operator(0, &operators).unwrap();
                    let roots = DFGIcator::new().get_dfg(&operators, &bb).unwrap();

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
