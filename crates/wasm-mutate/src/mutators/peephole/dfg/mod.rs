use std::{collections::HashMap, convert::TryFrom, hash::Hash};

use wasmparser::{Operator, Range, Type};

use crate::module::PrimitiveTypeInfo;

use super::OperatorAndByteOffset;

// Hack to show debug messages in tests
#[cfg(not(test))]
use log::debug;
#[cfg(test)]
use std::println as debug;

/// It executes a minimal symbolic evaluation of the stack to detect operands location in the code for certain operators
/// For example, i.add operator should know who are its operands
pub struct DFGIcator {}

#[derive(Debug)]
pub struct BBlock {
    pub(crate) range: Range,
}

/// Node of a DFG extracted from a basic block in the Wasm code
#[derive(Debug, Clone)]
pub struct StackEntry {
    /// Index of the operator in which this node start
    pub operator_idx: usize,
    /// Node operand indexes
    pub operands: Vec<usize>,
    /// True if the current node is an undef node, which means that the data for this leaf comes from another basic block
    pub is_undef: bool,
    /// Byte range (not inclusive) in which the operator instruction is written
    pub byte_stream_range: Range,
    /// Node type
    pub tpes: Vec<PrimitiveTypeInfo>,
    /// Index in the MiniDFG entries collection
    pub entry_idx: usize,
}

impl StackEntry {
    pub fn is_leaf(&self) -> bool {
        self.operands.len() == 0
    }
}

#[derive(Debug, Clone, Default)]
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
    pub fn get_bb_from_operator(
        &self,
        operator_index: usize,
        operators: &[OperatorAndByteOffset],
    ) -> Option<BBlock> {
        let mut range = Range {
            start: operator_index,
            end: operator_index + 1, // The range is inclusive in the last operator
        };
        // We only need the basic block upward
        let mut found = false;
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
                    if !found {
                        // If the insertion point is a jump
                        // Break inmediatly
                        return None;
                    }
                    break;
                }
                _ => {
                    found = true;
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
        tpes: Vec<PrimitiveTypeInfo>,
    ) -> usize {
        let entry_idx = dfg_map.len();
        let leaf = StackEntry {
            operator_idx,
            byte_stream_range: Range { start, end },
            operands: vec![],
            is_undef: false,
            tpes,
            entry_idx,
        };
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
        tpes: Vec<PrimitiveTypeInfo>,
    ) -> usize {
        let entry_idx = dfg_map.len();
        let newnode = StackEntry {
            operator_idx: operator_idx,
            byte_stream_range: Range { start, end },
            operands,
            is_undef: false,
            tpes,
            entry_idx,
        };

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
        parents: &mut Vec<i32>,
        insertindfg: bool,
    ) -> usize {
        let idx = stack
            .pop()
            .or_else(|| {
                // Since this represents the same for all
                // Create 0 element as Unknown
                let entry_idx = dfg_map.len();
                let leaf = StackEntry {
                    operator_idx,
                    operands: vec![],
                    is_undef: true,
                    byte_stream_range: Range::new(0, 0),
                    // Check if this can be inferred from the operator
                    tpes: vec![],
                    entry_idx,
                }; // Means not reachable
                if insertindfg {
                    operatormap.insert(operator_idx, entry_idx);
                }
                //
                //stack.push(entry_idx);
                // Add the data flow link
                dfg_map.push(leaf);
                parents.push(-1); // no parent yet
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
        locals: &Vec<PrimitiveTypeInfo>,
    ) -> Option<MiniDFG> {
        // lets handle the stack
        let mut dfg_map = Vec::new();
        let mut operatormap: HashMap<usize, usize> = HashMap::new(); // operator index to stack index
        let mut stack: Vec<usize> = Vec::new();
        let mut parents: Vec<i32> = Vec::new();

        // Create a DFG from the BB
        // Start from the first operator and simulate the stack...
        // If an operator is missing in the stack then it probably comes from a previous BB

        for idx in basicblock.range.start..basicblock.range.end {
            // We dont care about the jump
            let (operator, byte_offset) = &operators[idx];
            // Check if it is not EOF

            let byte_offset_next = if idx == basicblock.range.end {
                byte_offset
            } else {
                &operators[idx + 1].1
            };

            match operator {
                // Until type information is added
                /*Operator::GlobalGet { .. } | */
                Operator::LocalGet { local_index } => {
                    // This is a hack, type checking should be carried with the stack entries
                    DFGIcator::push_leaf(
                        idx,
                        *byte_offset,
                        *byte_offset_next,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        &mut parents,
                        vec![locals[*local_index as usize].clone()],
                    );
                }
                Operator::I32Const { .. } => {
                    DFGIcator::push_leaf(
                        idx,
                        *byte_offset,
                        *byte_offset_next,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        &mut parents,
                        vec![PrimitiveTypeInfo::I32],
                    );
                }
                Operator::LocalSet { .. } | Operator::GlobalSet { .. } | Operator::Drop => {
                    // It needs the offset arg
                    let child = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        true,
                    );
                    let entry_idx = dfg_map.len();
                    let newnode = StackEntry {
                        operator_idx: idx,
                        byte_stream_range: Range {
                            start: *byte_offset,
                            end: *byte_offset_next,
                        },
                        operands: vec![child],
                        is_undef: false,
                        tpes: vec![],
                        entry_idx
                    };

                    // operatormap.insert(idx, entry_idx);
                    // Add the data flow link
                    dfg_map.push(newnode);
                    parents.push(-1);

                    parents[child] = idx as i32;
                }
                // All memory loads
                Operator::I32Load { .. }
                /*
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
                | Operator::I64Load8U { .. } */ => {
                    // It needs the offset arg
                    let offset = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
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
                        // Add type here
                        vec![PrimitiveTypeInfo::I32],
                    );

                    parents[offset] = idx as i32;
                }
                Operator::I64Add => {
                    debug!("stack {:?} map {:?}", stack, dfg_map);
                    let leftidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );
                    debug!("stack {:?} map {:?}", stack, dfg_map);
                    let rightidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );

                    debug!("stack {:?} map {:?}", stack, dfg_map);
                    // The operands should not be the same

                    debug!("left {} right {} stack {:?}", leftidx, rightidx, stack);
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
                        vec![PrimitiveTypeInfo::I64],
                    );

                    parents[leftidx] = idx as i32;
                    parents[rightidx] = idx as i32;
                }
                Operator::I32Add
                | Operator::I32Sub
                | Operator::I32Mul
                | Operator::I32DivS
                | Operator::I32DivU
                | Operator::I32Shl
                | Operator::I32ShrS
                | Operator::I32ShrU => {
                    debug!("stack {:?} map {:?}", stack, dfg_map);
                    let leftidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );
                    debug!("stack {:?} map {:?}", stack, dfg_map);
                    let rightidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );

                    debug!("stack {:?} map {:?}", stack, dfg_map);
                    // The operands should not be the same

                    debug!("left {} right {} stack {:?}", leftidx, rightidx, stack);
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
                        vec![PrimitiveTypeInfo::I32],
                    );

                    parents[leftidx] = idx as i32;
                    parents[rightidx] = idx as i32;
                }
                Operator::Else | Operator::End | Operator::Nop => {
                    // Write this down to do a small change in the original wasm
                    let entry_idx = dfg_map.len();
                    let newnode = StackEntry {
                        operator_idx: idx,
                        byte_stream_range: Range {
                            start: *byte_offset,
                            end: *byte_offset_next,
                        },
                        operands: vec![],
                        is_undef: false,
                        tpes: vec![],
                        entry_idx
                    };
                    dfg_map.push(newnode);
                    parents.push(-1);
                }
                _ => {
                    debug!("Bypassing operator type {:?}", operator);
                    // If the operator is not implemented, break the mutation of this Basic Block
                    return None;
                }
            }
        }
        Some(MiniDFG {
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

                    let root = DFGIcator::new()
                        .get_bb_from_operator(5, &operators)
                        .unwrap();
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

                    let bb = DFGIcator::new()
                        .get_bb_from_operator(0, &operators)
                        .unwrap();
                    let roots = DFGIcator::new().get_dfg(&operators, &bb, &vec![]).unwrap();
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
