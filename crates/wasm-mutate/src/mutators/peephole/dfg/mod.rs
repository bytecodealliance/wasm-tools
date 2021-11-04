use std::collections::HashMap;

use wasmparser::{Operator, Range};

use crate::{module::PrimitiveTypeInfo, ModuleInfo};

use super::OperatorAndByteOffset;

/// It executes a minimal symbolic evaluation of the stack to detect operands location in the code for certain operators
/// For example, i.add operator should know who are its operands
pub struct DFGBuilder {
    stack: Vec<usize>,
    dfg_map: Vec<StackEntry>,
    operatormap: HashMap<usize, usize>,
    parents: Vec<i32>,
}

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
    /// The stack entry return types
    pub return_type: PrimitiveTypeInfo,
    /// Index in the MiniDFG entries collection
    pub entry_idx: usize,
    /// Color of the dfg part
    pub color: u32,
    /// Instruction index if its apply
    pub operator_idx: usize,
}

/// This is the IR used to turn wasm to eterm and back
/// It separates the operator logic from the type information and destackifies the Wasm code
#[derive(Debug, Clone)]
pub enum StackType {
    I32(i32),
    I64(i64),
    LocalGet(u32 /*Index*/),
    LocalSet(u32),
    LocalTee(u32),
    GlobalGet(u32),
    GlobalSet(u32),
    Drop,
    Call {
        function_index: u32,
        params_count: usize,
    },
    Load {
        offset: u64,
        align: u8,
        memory: u32,
    },
    Undef,
    IndexAtCode(usize, usize),
}

#[derive(Clone, Default)]
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
    pub fn is_subtree_consistent(&self, current: usize) -> bool {
        let entry = &self.entries[current];
        let mut colors = vec![];
        let mut worklist = vec![entry];

        while let Some(entry) = worklist.pop() {
            colors.push(entry.color);

            entry.operands.iter().for_each(|i| {
                worklist.push(&self.entries[*i]);
            });
        }

        // All nodes in the tree should have the same color
        colors
            .get(0)
            .map(|&val| colors.iter().all(|&x| x == val))
            .or(Some(false))
            .unwrap()
    }
    /// Return true if the coloring of the children subtrees is the same as the root
    /// Notice that this value can be calcuated when the tree is built
    pub fn is_subtree_consistent_from_root(&self) -> bool {
        let current = self.entries.len() - 1;
        self.is_subtree_consistent(current)
    }

    /// Default pretty print
    /// It prints the stack entry instead of the Wasm operator
    pub fn pretty_print_default(&self) -> String {
        self.pretty_print(&|entry: &StackEntry| format!("{:?}", entry.operator))
    }
    /// Pretty prints the DFG forest in a tree structure
    /// It receives a custom function to format how the entry information will be written
    /// For an example, see the implementation of the `pretty_print_default` method.
    pub fn pretty_print(&self, entryformatter: &dyn Fn(&StackEntry) -> String) -> String {
        let mut builder = String::from("");

        builder.push_str("DFG forest\n");

        // To get ansi colors
        fn get_color(color: u32) -> &'static str {
            match color {
                0 => "\u{001b}[31m",    // red
                1 => "\u{001b}[32m",    // green
                2 => "\u{001b}[33m",    // yellow
                3 => "\u{001b}[34m",    // blue
                4 => "\u{001b}[35m",    // magenta
                5 => "\u{001b}[36m",    // cyan
                6 => "\u{001b}[37;1m",  // bright white
                7 => "\u{001b}[32;1m",  // bright green
                8 => "\u{001b}[33;1m",  // bright yellow
                9 => "\u{001b}[36;1m",  // bright cyan
                10 => "\u{001b}[35;1m", // bright magenta
                11 => "\u{001b}[31;1m", // bright red
                _ => "\u{001b}[0m",     // reset color (terminal default)
            }
        }
        fn write_child(
            minidfg: &MiniDFG,
            entryidx: usize,
            preffix: &str,
            entryformatter: &dyn Fn(&StackEntry) -> String,
            childrenpreffix: &str,
            builder: &mut String,
        ) {
            let entry = &minidfg.entries[entryidx];
            builder.push_str(&(&preffix).to_string());
            let color = get_color(entry.color);
            builder.push_str(
                format!(
                    "{}({})(at {}) {:?}\u{001b}[0m\n",
                    color,
                    entry.color,
                    entry.operator_idx,
                    entryformatter(entry)
                )
                .as_str(),
            );

            for (idx, op) in entry.operands.iter().enumerate() {
                if idx < entry.operands.len() - 1 {
                    // Has no next child
                    let preffix = format!("{}{}", childrenpreffix, "├──");
                    let childrenpreffix = format!("{}{}", childrenpreffix, "│   ");
                    write_child(
                        minidfg,
                        *op,
                        &preffix,
                        entryformatter,
                        &childrenpreffix,
                        builder,
                    );
                } else {
                    let preffix = format!("{}{}", childrenpreffix, "└──");
                    let childrenpreffix = format!("{}{}", childrenpreffix, "    ");
                    write_child(
                        minidfg,
                        *op,
                        &preffix,
                        entryformatter,
                        &childrenpreffix,
                        builder,
                    );
                }
            }
        }
        // Get roots
        for (entryidx, idx) in self.parents.iter().enumerate() {
            if *idx == -1 {
                write_child(self, entryidx, "", entryformatter, "", &mut builder);
            }
        }

        builder
    }
}

impl<'a> DFGBuilder {
    pub fn new() -> Self {
        DFGBuilder {
            stack: Vec::new(),
            dfg_map: Vec::new(),
            operatormap: HashMap::new(),
            parents: Vec::new(),
        }
    }

    /// Linear algorithm to construct the basic block
    /// This follows the tradition way
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
                | Operator::Return
                | Operator::Unreachable
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
        &mut self,
        operator: StackType,
        operator_idx: usize,
        operands: Vec<usize>,
        color: u32,
        return_type: PrimitiveTypeInfo,
    ) -> usize {
        let entry_idx = self.dfg_map.len();
        let push_to_stack = if let PrimitiveTypeInfo::Empty = return_type {
            // Avoid to push empty values on to the stack
            false
        } else {
            true
        };
        let newnode = StackEntry {
            operator,
            operands,
            return_type,
            entry_idx,
            color,
            operator_idx,
        };

        self.operatormap.insert(operator_idx, entry_idx);
        if push_to_stack {
            self.stack.push(entry_idx)
        }
        // Add the data flow link
        self.dfg_map.push(newnode);
        self.parents.push(-1);
        entry_idx
    }

    fn pop_operand(&mut self, operator_idx: usize, insertindfg: bool, undef_color:u32 ) -> usize {
        self.stack
            .pop()
            .or_else(|| {
                // Since this represents the same for all
                // Create 0 element as Unknown
                let entry_idx = self.dfg_map.len();
                let leaf = StackEntry {
                    operator: StackType::Undef,
                    operands: vec![],
                    // Check if this can be inferred from the operator
                    return_type: PrimitiveTypeInfo::Empty,
                    entry_idx,
                    color: undef_color, 
                    operator_idx,
                }; // Means not reachable
                if insertindfg {
                    self.operatormap.insert(operator_idx, entry_idx);
                }
                // Add the data flow link
                self.dfg_map.push(leaf);
                self.parents.push(-1); // no parent yet
                Some(entry_idx)
            })
            .unwrap()
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
        info: &ModuleInfo,
        operators: &'a [OperatorAndByteOffset],
        basicblock: &BBlock,
        locals: &Vec<PrimitiveTypeInfo>,
    ) -> Option<MiniDFG> {
        let mut color = 1; // start with color 1 since 0 is undef
                           // Create a DFG from the BB
                           // Start from the first operator and simulate the stack...
                           // If an operator is missing in the stack then it probably comes from a previous BB

        for idx in basicblock.range.start..basicblock.range.end {
            // We dont care about the jump
            let (operator, _) = &operators[idx];
            // Check if it is not EOF

            match operator {
                Operator::Call { function_index } => {
                    let typeinfo = info.get_functype_idx(*function_index as usize);
                    match typeinfo {
                        crate::module::TypeInfo::Func(tpe) => {
                            // Skip if it returns more than one value
                            // since it is not yet supported
                            if tpe.returns.len() > 1 {
                                return None;
                            }

                            // Pop as many parameters from the stack
                            let mut operands = (0..tpe.params.len())
                                .map(|_| self.pop_operand(idx, true, 0))
                                .collect::<Vec<usize>>();
                            // reverse operands
                            operands.reverse();
                            // Add this as a new operator
                            let fidx = self.push_node(
                                StackType::Call {
                                    function_index: *function_index,
                                    params_count: tpe.params.len(),
                                },
                                idx,
                                operands.clone(),
                                color,
                                if tpe.returns.is_empty() {
                                    PrimitiveTypeInfo::Empty
                                } else {
                                    tpe.returns[0].clone()
                                },
                            );
                            // Set the parents for the operands
                            for id in &operands {
                                self.parents[*id] = fidx as i32;
                            }
                            // Change the color
                            color += 1;
                        }
                        _ => unreachable!("It should be a function type"),
                    }
                }
                Operator::LocalGet { local_index } => {
                    self.push_node(
                        StackType::LocalGet(*local_index),
                        idx,
                        vec![],
                        color,
                        locals[*local_index as usize].clone(),
                    );
                }
                Operator::GlobalGet { global_index } => {
                    self.push_node(
                        StackType::GlobalGet(*global_index),
                        idx,
                        vec![],
                        color,
                        info.global_types[*global_index as usize].clone(),
                    );
                }
                Operator::GlobalSet { global_index } => {
                    let child = self.pop_operand(idx, true, 0);// 0 color is undefined

                    self.push_node(
                        StackType::GlobalSet(*global_index),
                        idx,
                        vec![child],
                        color,
                        PrimitiveTypeInfo::Empty,
                    );

                    self.parents[child] = idx as i32;
                    // Augnment the color since the next operations could be inconsistent
                    color += 1;
                }
                Operator::I32Const { value } => {
                    self.push_node(
                        StackType::I32(*value),
                        idx,
                        vec![],
                        color,
                        PrimitiveTypeInfo::I32,
                    );
                }
                Operator::I64Const { value } => {
                    self.push_node(
                        StackType::I64(*value),
                        idx,
                        vec![],
                        color,
                        PrimitiveTypeInfo::I64,
                    );
                }
                Operator::LocalSet { local_index } => {
                    // It needs the offset arg
                    let child = self.pop_operand(idx, true, 0);

                    let idx = self.push_node(
                        StackType::LocalSet(*local_index),
                        idx,
                        vec![child],
                        color,
                        PrimitiveTypeInfo::Empty,
                    );
                    self.parents[child] = idx as i32;
                    // Augnment the color since the next operations could be inconsistent
                    color += 1;
                }
                Operator::LocalTee { local_index } => {
                    // It needs the offset arg
                    let child = self.pop_operand(idx, true, 0);

                    let idx = self.push_node(
                        StackType::LocalTee(*local_index),
                        idx,
                        vec![child],
                        color,
                        locals[*local_index as usize].clone(),
                    );
                    self.parents[child] = idx as i32;
                    // Augnment the color since the next operations could be inconsistent
                    color += 1;
                }
                Operator::I32Store { .. } | Operator::I64Store { .. } => {
                    let value = self.pop_operand(idx, false, 0);
                    let offset = self.pop_operand(idx, false, 0);
                    let idx = self.push_node(
                        StackType::IndexAtCode(idx, 2),
                        idx,
                        vec![offset, value],
                        color,
                        // Add type here
                        PrimitiveTypeInfo::Empty,
                    );

                    self.parents[offset] = idx as i32;
                    self.parents[value] = idx as i32;
                    color += 1;
                }
                // All memory loads
                Operator::I32Load { memarg } => {
                    // It needs the dynamic offset arg
                    let offset = self.pop_operand(idx, false, 0);

                    let idx = self.push_node(
                        StackType::Load {
                            offset: memarg.offset,
                            align: memarg.align,
                            memory: memarg.memory,
                        },
                        idx,
                        vec![offset],
                        color,
                        // Add type here
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[offset] = idx as i32;
                }
                Operator::I64Load { memarg } => {
                    // It needs the offset arg
                    let offset = self.pop_operand(idx, false, 0);
                    let idx = self.push_node(
                        StackType::Load {
                            offset: memarg.offset,
                            align: memarg.align,
                            memory: memarg.memory,
                        },
                        idx,
                        vec![offset],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[offset] = idx as i32;
                }
                Operator::I32Eqz => {
                    let operand = self.pop_operand(idx, false, 0);
                    let idx = self.push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        vec![operand],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[operand] = idx as i32;
                }
                Operator::I64Eqz => {
                    let operand = self.pop_operand(idx, false, 0);
                    let idx = self.push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        vec![operand],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[operand] = idx as i32;
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
                | Operator::I64Rotl
                | Operator::I64Rotr
                | Operator::I64ShrU
                | Operator::I64RemS
                | Operator::I64RemU => {
                    let leftidx = self.pop_operand(idx, false, 0);
                    let rightidx = self.pop_operand(idx, false, 0);
                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        StackType::IndexAtCode(idx, 2),
                        idx,
                        vec![rightidx, leftidx], // reverse order
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
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
                | Operator::I32ShrU
                | Operator::I32Rotl
                | Operator::I32Rotr
                | Operator::I32RemS
                | Operator::I32RemU => {
                    let leftidx = self.pop_operand(idx, false, 0);
                    let rightidx = self.pop_operand(idx, false, 0);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        StackType::IndexAtCode(idx, 2),
                        idx,
                        vec![rightidx, leftidx], // reverse order
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::Drop => {
                    let arg = self.pop_operand(idx, false, 0);

                    let idx = self.push_node(
                        StackType::Drop,
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::Empty,
                    );

                    self.parents[arg] = idx as i32;
                    //color += 1;
                }
                // conversion between integers
                Operator::I32WrapI64 => {
                    let arg = self.pop_operand(idx, false, 0);

                    let idx = self.push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        vec![arg], // reverse order
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I32Extend16S | Operator::I32Extend8S => {
                    let arg = self.pop_operand(idx, false, 0);

                    let idx = self.push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[arg] = idx as i32;
                }

                Operator::I64Extend16S
                | Operator::I64Extend8S
                | Operator::I64ExtendI32S
                | Operator::I64Extend32S
                | Operator::I64ExtendI32U => {
                    let arg = self.pop_operand(idx, false, 0);

                    let idx = self.push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::Else
                | Operator::End
                | Operator::Nop
                | Operator::Br { .. }
                | Operator::BrTable { .. }
                | Operator::BrIf { .. }
                | Operator::Return
                | Operator::Unreachable => {
                    // Write this down to do a small change in the original wasm
                    self.push_node(
                        StackType::IndexAtCode(idx, 0),
                        idx,
                        vec![],
                        color,
                        PrimitiveTypeInfo::Empty,
                    );
                }
                // Unary integer operators.
                Operator::I32Popcnt => {
                    let arg = self.pop_operand(idx, false, 0);
                    self.push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I32,
                    );
                    self.parents[arg] = idx as i32;
                }
                Operator::I64Popcnt => {
                    let arg = self.pop_operand(idx, false, 0);
                    self.push_node(
                        StackType::IndexAtCode(idx, 1),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I64,
                    );
                    self.parents[arg] = idx as i32;
                }
                _ => {
                    // If the operator is not implemented, break the mutation of this Basic Block
                    return None;
                }
            }
        }
        Some(MiniDFG {
            entries: self.dfg_map.clone(),
            map: self.operatormap.clone(),
            parents: self.parents.clone(),
        })
    }
}

impl std::fmt::Debug for MiniDFG {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.pretty_print_default())
    }
}

impl std::fmt::Display for MiniDFG {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::DFGBuilder;
    use crate::{module::PrimitiveTypeInfo, mutators::peephole::OperatorAndByteOffset, ModuleInfo};
    use wasmparser::Parser;

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

                    let roots = DFGBuilder::new().get_bb_from_operator(5, &operators);

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

                    let bb = DFGBuilder::new()
                        .get_bb_from_operator(0, &operators)
                        .unwrap();
                    let roots =
                        DFGBuilder::new().get_dfg(&ModuleInfo::default(), &operators, &bb, &vec![]);
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

                    let bb = DFGBuilder::new()
                        .get_bb_from_operator(7, &operators)
                        .unwrap();
                    let roots =
                        DFGBuilder::new().get_dfg(&ModuleInfo::default(), &operators, &bb, &vec![]);
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

    #[test]
    fn test_dfg_build3() {
        // A decent complex Wasm function
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (global $0 i32 i32.const 0)
            (func (export "exported_func") (result i32) (local i32)
                i32.const 123
                return
                i32.const 312
                i32.const 100
                drop
                local.set 0
                local.get 0
                local.set 0
                i32.const 1230
                local.tee 0
                call 0
                call 0
                i32.add
                drop
                i32.const 900
                global.get 0
                drop
                global.set 0
                global.get 0
                global.set 0
                nop
                nop

                i32.const 10
                i32.const 20
                i32.rotr

                i32.const 10
                i32.const 20
                i32.rotl
                i32.const 100
                i32.const 50
                i32.store
            )
        )
        "#,
        )
        .unwrap();

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        let info = ModuleInfo::new(original).unwrap();
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

                    let bb = DFGBuilder::new()
                        .get_bb_from_operator(30, &operators)
                        .unwrap();
                    let roots = DFGBuilder::new().get_dfg(
                        &info,
                        &operators,
                        &bb,
                        &vec![PrimitiveTypeInfo::I32],
                    );
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

    #[test]
    fn test_dfg_build_calls() {
        // A decent complex Wasm function
        let original = &wat::parse_str(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32 i32 i32) (result i32)
                i32.const 10
                i32.const 10
                i32.const 10
                call 0
            )
        )
        "#,
        )
        .unwrap();

        let info = ModuleInfo::new(original).unwrap();

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        loop {
            let (payload, size) = match parser.parse(&original[consumed..], true).unwrap() {
                wasmparser::Chunk::NeedMoreData(_) => unreachable!(),
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

                    let bb = DFGBuilder::new()
                        .get_bb_from_operator(3, &operators)
                        .unwrap();
                    let roots = DFGBuilder::new().get_dfg(&info, &operators, &bb, &vec![]);
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
