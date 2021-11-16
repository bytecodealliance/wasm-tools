use std::collections::HashMap;

use egg::{Id, RecExpr};
use wasmparser::{Operator, Range};

use crate::mutators::peephole::Lang;
use crate::{module::PrimitiveTypeInfo, ModuleInfo};

use crate::mutators::OperatorAndByteOffset;

use super::eggsy::encoder::rebuild::build_expr;

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
    /// Lang enode operator mapping
    pub operator: Lang,
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

    /// Returns a RecExpr from the stack entry
    /// by cleaning spurious nodes
    ///
    pub fn get_expr(&self, at: usize) -> RecExpr<Lang> {
        let root = self.map[&at];
        let enodes = self.entries[..=root]
            .iter()
            .map(|t| t.operator.clone())
            .collect::<Vec<_>>();
        let operands = self.entries[..=root]
            .iter()
            .map(|t| t.operands.iter().map(|i| Id::from(*i)).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        build_expr(Id::from(root), &enodes, &operands)
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
                    range.start += 1; // Do not include the last jmp
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
        operator: Lang,
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

    fn pop_operand(&mut self, operator_idx: usize, insertindfg: bool) -> usize {
        self.stack
            .pop()
            .or_else(|| {
                // Since this represents the same for all
                // Create 0 element as Unknown
                let entry_idx = self.dfg_map.len();
                let leaf = StackEntry {
                    operator: Lang::Undef,
                    operands: vec![],
                    // Check if this can be inferred from the operator
                    return_type: PrimitiveTypeInfo::Empty,
                    entry_idx,
                    color: 0, // 0 color is undefined
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

    /// Returns the status of the stack
    /// Ideally this should be called after the DFG is contructed, to
    /// for example, get the type of the basic block
    pub fn get_stack_status(&self) -> Vec<StackEntry> {
        return self
            .stack
            .iter()
            .map(|idx| self.dfg_map[*idx].clone())
            .collect::<Vec<_>>();
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
        locals: &[PrimitiveTypeInfo],
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
                                .map(|_| self.pop_operand(idx, true))
                                .collect::<Vec<usize>>();
                            // reverse operands
                            operands.reverse();
                            // Add this as a new operator
                            let fidx = self.push_node(
                                Lang::Call(
                                    *function_index as usize,
                                    operands.iter().map(|i| Id::from(*i)).collect::<Vec<_>>(),
                                ),
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
                        Lang::LocalGet(*local_index),
                        idx,
                        vec![],
                        color,
                        locals[*local_index as usize].clone(),
                    );
                }
                Operator::GlobalGet { global_index } => {
                    let _idx = self.push_node(
                        Lang::GlobalGet(*global_index),
                        idx,
                        vec![],
                        color,
                        info.global_types[*global_index as usize].clone(),
                    );
                }
                Operator::GlobalSet { global_index } => {
                    let child = self.pop_operand(idx, true);

                    let idx = self.push_node(
                        Lang::GlobalSet(*global_index, Id::from(child)),
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
                        Lang::I32(*value),
                        idx,
                        vec![],
                        color,
                        PrimitiveTypeInfo::I32,
                    );
                }
                Operator::I64Const { value } => {
                    self.push_node(
                        Lang::I64(*value),
                        idx,
                        vec![],
                        color,
                        PrimitiveTypeInfo::I64,
                    );
                }
                Operator::LocalSet { local_index } => {
                    // It needs the offset arg
                    let child = self.pop_operand(idx, true);

                    let idx = self.push_node(
                        Lang::LocalSet(*local_index, Id::from(child)),
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
                    let child = self.pop_operand(idx, true);

                    let idx = self.push_node(
                        Lang::LocalTee(*local_index, Id::from(child)),
                        idx,
                        vec![child],
                        color,
                        locals[*local_index as usize].clone(),
                    );
                    self.parents[child] = idx as i32;
                    // Augnment the color since the next operations could be inconsistent
                    color += 1;
                }
                Operator::I32Store { memarg } => {
                    let value = self.pop_operand(idx, false);
                    let offset = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I32Store {
                            value_and_offset: [Id::from(offset), Id::from(value)],
                            static_offset: memarg.offset,
                            align: memarg.align,
                            mem: memarg.memory,
                        },
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
                Operator::I64Store { memarg } => {
                    let value = self.pop_operand(idx, false);
                    let offset = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I64Store {
                            value_and_offset: [Id::from(offset), Id::from(value)],
                            static_offset: memarg.offset,
                            align: memarg.align,
                            mem: memarg.memory,
                        },
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
                    let offset = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I32Load {
                            offset: Id::from(offset),
                            static_offset: memarg.offset,
                            align: memarg.align,
                            mem: memarg.memory,
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
                    let offset = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I64Load {
                            offset: Id::from(offset),
                            static_offset: memarg.offset,
                            align: memarg.align,
                            mem: memarg.memory,
                        },
                        idx,
                        vec![offset],
                        color,
                        // Add type here
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[offset] = idx as i32;
                }
                Operator::I32Eqz => {
                    let operand = self.pop_operand(idx, false);
                    let idx = self.push_node(
                        Lang::I32Eqz([Id::from(operand)]),
                        idx,
                        vec![operand],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[operand] = idx as i32;
                }
                Operator::I64Eqz => {
                    let operand = self.pop_operand(idx, false);
                    let idx = self.push_node(
                        Lang::I64Eqz([Id::from(operand)]),
                        idx,
                        vec![operand],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[operand] = idx as i32;
                }
                Operator::I64Add => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64Add([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Sub => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64Sub([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Mul => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64Mul([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64DivS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64DivS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64DivU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64DivU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Shl => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64Shl([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64ShrS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64ShrS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Xor => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64Xor([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Or => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64Or([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64And => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64And([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Rotl => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64RotL([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Rotr => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64RotR([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64ShrU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64ShrU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64RemS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64RemS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64RemU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64RemU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Add => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32Add([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Sub => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32Sub([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Eq => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32Eq([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Ne => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32Ne([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32LtS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32LtS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32LtU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32LtU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32GtS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32GtS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32GtU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32GtU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32LeS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32LeS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32LeU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32GtU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32GeS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32GeS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32GeU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32GeU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Mul => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32Mul([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32DivS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32DivS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32DivU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32DivU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Shl => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32Shl([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32ShrS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32ShrS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Xor => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32Xor([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Or => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32Or([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Eq => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64Eq([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64Ne => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64Ne([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64LtS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64LtS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64LtU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64LtU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64GtS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64GtS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64GtU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64GtU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64LeS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64LeS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64LeU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64LeU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64GeS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64GeS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I64GeU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I64GeU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32And => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32And([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32ShrU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32ShrU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Rotl => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32RotL([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32Rotr => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32RotR([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32RemS => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32RemS([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::I32RemU => {
                    let leftidx = self.pop_operand(idx, false);
                    let rightidx = self.pop_operand(idx, false);

                    // The operands should not be the same
                    assert_ne!(leftidx, rightidx);

                    let idx = self.push_node(
                        Lang::I32RemU([Id::from(rightidx), Id::from(leftidx)]),
                        idx,
                        vec![rightidx, leftidx],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[leftidx] = idx as i32;
                    self.parents[rightidx] = idx as i32;
                }
                Operator::Drop => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::Drop([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::Empty,
                    );

                    self.parents[arg] = idx as i32;
                }
                // conversion between integers
                Operator::I32WrapI64 => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::Wrap([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I32Extend16S => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I32Extend16S([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I32Extend8S => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I32Extend8S([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[arg] = idx as i32;
                }

                Operator::I64Extend16S => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I64Extend16S([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I64Extend8S => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I64Extend8S([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I64ExtendI32S => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I64ExtendI32S([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I64Extend32S => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I64Extend32S([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I64ExtendI32U => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I64ExtendI32U([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I64,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I32Popcnt => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I32Popcnt([Id::from(arg)]),
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::I64Popcnt => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::I64Popcnt([Id::from(arg)]),
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
    use crate::mutators::OperatorAndByteOffset;
    use crate::ModuleInfo;
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
                        DFGBuilder::new().get_dfg(&ModuleInfo::default(), &operators, &bb, &[]);
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
                        DFGBuilder::new().get_dfg(&ModuleInfo::default(), &operators, &bb, &[]);
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
                    let roots = DFGBuilder::new().get_dfg(&info, &operators, &bb, &[]);
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
