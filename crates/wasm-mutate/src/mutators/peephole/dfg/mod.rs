use std::{collections::HashMap, ops::Index};

use wasmparser::{Operator, Range};

use crate::module::PrimitiveTypeInfo;

use super::OperatorAndByteOffset;

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
    /// Wasm operator mapping
    pub operator: StackType,
    /// Node operand indexes
    pub operands: Vec<usize>,
    /// Node type
    pub tpes: Vec<PrimitiveTypeInfo>,
    /// Index in the MiniDFG entries collection
    pub entry_idx: usize,
    /// Color of the dfg part
    pub color: u32,
}

/// This is the IR used to turn wasm to eterm and back
/// It separates the operator logic from the type information and destackifies the Wasm code
#[derive(Debug, Clone)]
pub enum StackType {
    I32(i32),
    I64(i64),
    LocalGet(u32 /*Index*/),
    LocalSet(u32),
    // static ofsset, mem align, mem index
    Load(u64, u8, u32),
    Undef,
    IndexAtCode(usize, usize),
}

#[derive(Debug, Clone, Default)]
pub struct MiniDFG {
    // Some of the operators have no stack entry
    // This will help to decide or not to mutate the operators, avoiding egrapphp creation, etc
    // Each (key, value) entry corresponds to the index of the instruction in
    // the Wasm BasicBlock and the index of the stack entry in the `entries` field
    pub map: HashMap<usize, usize>,
    // Each stack entry represents a position in the operators stream
    // containing its children
    pub entries: Vec<StackEntry>,
    // For each stack entry we keep the parental relation, the ith value is the index of
    // the ith instruction's parent instruction
    // We write each stack entry having no parent, i.e. a root in the dfg
    pub parents: Vec<i32>,
}

impl MiniDFG {
    /// Return true if the coloring of the children subtrees is the same as the root
    /// Notice that this value can be calcuated when the tree is built
    pub fn is_subtree_consistent(&self, current: usize, parent: Option<usize>) -> bool {
        let entry = &self.entries[current];
        match parent {
            None => {
                return entry
                    .operands
                    .iter()
                    .map(|&u| self.is_subtree_consistent(u, Some(current)))
                    .all(|x| x)
            }
            Some(u) => entry.color == self.entries[u].color,
        }
    }
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

    fn push_node(
        operator: StackType,
        operator_idx: usize,
        dfg_map: &mut Vec<StackEntry>,
        operatormap: &mut HashMap<usize, usize>,
        stack: &mut Vec<usize>,
        operands: Vec<usize>,
        parents: &mut Vec<i32>,
        color: u32,
        tpes: Vec<PrimitiveTypeInfo>,
    ) -> usize {
        let entry_idx = dfg_map.len();
        let newnode = StackEntry {
            operator,
            operands,
            tpes,
            entry_idx,
            color,
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
                    operator: StackType::Undef,
                    operands: vec![],
                    // Check if this can be inferred from the operator
                    tpes: vec![],
                    entry_idx,
                    color: 0, // 0 color is undefined
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
        let mut color = 1; // start with color 1 since 0 is undef
                           // Create a DFG from the BB
                           // Start from the first operator and simulate the stack...
                           // If an operator is missing in the stack then it probably comes from a previous BB

        for idx in basicblock.range.start..basicblock.range.end {
            // We dont care about the jump
            let (operator, _) = &operators[idx];
            // Check if it is not EOF

            match operator {
                // Until type information is added
                Operator::LocalGet { local_index } => {
                    // This is a hack, type checking should be carried with the stack entries
                    DFGIcator::push_node(
                        StackType::LocalGet(*local_index),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![],
                        &mut parents,
                        color,
                        vec![locals[*local_index as usize].clone()],
                    );
                }
                Operator::I32Const { value } => {
                    DFGIcator::push_node(
                        StackType::I32(*value),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![],
                        &mut parents,
                        color,
                        vec![PrimitiveTypeInfo::I32],
                    );
                }
                Operator::I64Const { value } => {
                    DFGIcator::push_node(
                        StackType::I64(*value),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![],
                        &mut parents,
                        color,
                        vec![PrimitiveTypeInfo::I64],
                    );
                }
                Operator::LocalSet { local_index } => {
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
                        operator: StackType::LocalSet(*local_index),
                        operands: vec![child],
                        tpes: vec![],
                        entry_idx,
                        color,
                    };

                    // operatormap.insert(idx, entry_idx);
                    dfg_map.push(newnode);
                    parents.push(-1);
                    parents[child] = idx as i32;
                    // Augnment the color since the next operations could be inconsistent
                    color += 1;
                }
                // All memory loads
                Operator::I32Load { memarg } => {
                    // It needs the dynamic offset arg
                    let offset = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );

                    // TODO add staticoffset, align and memory idx as stack entries.

                    let idx = DFGIcator::push_node(
                        StackType::Load(memarg.offset, memarg.align, memarg.memory),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![offset],
                        &mut parents,
                        color,
                        // Add type here
                        vec![PrimitiveTypeInfo::I32],
                    );

                    parents[offset] = idx as i32;
                }
                Operator::I64Load { memarg } => {
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
                        StackType::Load(memarg.offset, memarg.align, memarg.memory),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![offset],
                        &mut parents,
                        color,
                        vec![PrimitiveTypeInfo::I64],
                    );

                    parents[offset] = idx as i32;
                }
                Operator::I32Eqz => {
                    let operand = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );
                    let idx = DFGIcator::push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![operand],
                        &mut parents,
                        color,
                        vec![PrimitiveTypeInfo::I32],
                    );

                    parents[operand] = idx as i32;
                }
                Operator::I64Eqz => {
                    let operand = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );
                    let idx = DFGIcator::push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![operand],
                        &mut parents,
                        color,
                        vec![PrimitiveTypeInfo::I32],
                    );

                    parents[operand] = idx as i32;
                }
                Operator::I64Add
                | Operator::I64Sub
                | Operator::I64Mul
                | Operator::I64DivS
                | Operator::I64DivU
                | Operator::I64Shl
                | Operator::I64ShrS
                | Operator::I64Xor
                | Operator::I64Or
                | Operator::I64And
                | Operator::I64ShrU => {
                    let leftidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );
                    let rightidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );
                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = DFGIcator::push_node(
                        StackType::IndexAtCode(idx, 2),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![rightidx, leftidx], // reverse order
                        &mut parents,
                        color,
                        vec![PrimitiveTypeInfo::I64],
                    );

                    parents[leftidx] = idx as i32;
                    parents[rightidx] = idx as i32;
                }
                Operator::I32Add
                | Operator::I32Sub
                | Operator::I32Eq
                | Operator::I32Ne
                | Operator::I32LtS
                | Operator::I32LtU
                | Operator::I32GtS
                | Operator::I32GtU
                | Operator::I32LeS
                | Operator::I32LeU
                | Operator::I32GeS
                | Operator::I32GeU
                | Operator::I32Mul
                | Operator::I32DivS
                | Operator::I32DivU
                | Operator::I32Shl
                | Operator::I32ShrS
                | Operator::I32Xor
                | Operator::I32Or
                | Operator::I64Eq
                | Operator::I64Ne
                | Operator::I64LtS
                | Operator::I64LtU
                | Operator::I64GtS
                | Operator::I64GtU
                | Operator::I64LeS
                | Operator::I64LeU
                | Operator::I64GeS
                | Operator::I64GeU
                | Operator::I32And
                | Operator::I32ShrU => {
                    let leftidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );
                    let rightidx = DFGIcator::pop_operand(
                        &mut stack,
                        &mut dfg_map,
                        idx,
                        &mut operatormap,
                        &mut parents,
                        false,
                    );

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = DFGIcator::push_node(
                        StackType::IndexAtCode(idx, 2),
                        idx,
                        &mut dfg_map,
                        &mut operatormap,
                        &mut stack,
                        vec![rightidx, leftidx], // reverse order
                        &mut parents,
                        color,
                        vec![PrimitiveTypeInfo::I32],
                    );

                    parents[leftidx] = idx as i32;
                    parents[rightidx] = idx as i32;
                }
                Operator::Else | Operator::End | Operator::Nop | Operator::Drop => {
                    // Write this down to do a small change in the original wasm
                    let entry_idx = dfg_map.len();
                    let newnode = StackEntry {
                        operator: StackType::IndexAtCode(idx, 2),
                        operands: vec![],
                        tpes: vec![],
                        entry_idx,
                        color,
                    };
                    dfg_map.push(newnode);
                    parents.push(-1);
                }
                _ => {
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
    use wasmparser::Parser;

    use crate::mutators::peephole::OperatorAndByteOffset;

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

                    let roots = DFGIcator::new().get_bb_from_operator(5, &operators);

                    assert!(roots.is_some())
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
                    let roots = DFGIcator::new().get_dfg(&operators, &bb, &vec![]);
                    assert!(roots.is_some())
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
    fn test_dfg_build2() {
        // A decent complex Wasm function
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                i32.const 32
                i32.load
                i32.const 100
                i32.load
                i32.const 1
                i32.gt_s
                i32.const 1
                i32.gt_u
                i32.const 1
                i32.lt_u
                i32.const 1
                i32.lt_s
                i32.const 1
                i32.ne
                i32.const 1
                i32.eq
                i32.const 1
                i32.eqz
                i32.const 1
                i32.le_s
                i32.const 1
                i32.le_u
                i32.const 1
                i32.ge_s
                i32.const 1
                i32.ge_u
                local.set 0
                i32.const 1
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
                        .get_bb_from_operator(7, &operators)
                        .unwrap();
                    let roots = DFGIcator::new().get_dfg(&operators, &bb, &vec![]);
                    assert!(roots.is_some());
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
