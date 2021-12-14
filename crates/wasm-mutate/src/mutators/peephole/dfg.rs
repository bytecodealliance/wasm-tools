//! DFG extractor for Wasm functions. It converts Wasm operators to the [Lang]
//! intermediate representation.
use std::collections::HashMap;

use egg::{Id, RecExpr};
use wasmparser::{Operator, Range};

use crate::mutators::peephole::Lang;
use crate::{module::PrimitiveTypeInfo, ModuleInfo};
use PrimitiveTypeInfo::*;

use crate::mutators::OperatorAndByteOffset;

use super::eggsy::encoder::rebuild::build_expr;

/// It executes a minimal symbolic evaluation of the stack to detect operands
/// location in the code for certain operators
///
/// For example, the `i32.add` operator should know who are its operands
pub struct DFGBuilder {
    stack: Vec<usize>,
    dfg_map: Vec<StackEntry>,
    operatormap: HashMap<usize, usize>,
    parents: Vec<i32>,
}

/// Basic block of a Wasm's function defined as a range of operators in the Wasm
/// function
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

/// DFG structre for a piece of Wasm's function
#[derive(Clone, Default)]
pub struct MiniDFG {
    /// Some of the operators have no stack entry
    /// This will help to decide or not to mutate the operators, avoiding egrapphp creation, etc
    /// Each (key, value) entry corresponds to the index of the instruction in
    /// the Wasm BasicBlock and the index of the stack entry in the `entries` field
    pub map: HashMap<usize, usize>,
    /// Each stack entry represents a position in the operators stream
    /// containing its children
    pub entries: Vec<StackEntry>,
    /// For each stack entry we keep the parental relation, the ith value is the index of
    /// the ith instruction's parent instruction.
    /// We write each stack entry having no parent, i.e. a root in the dfg
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
    /// Returns a new DFG builder
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
                    let typeinfo = info.get_functype_idx(*function_index);
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
                Operator::F32Const { value } => {
                    self.push_node(
                        Lang::F32(value.bits()),
                        idx,
                        vec![],
                        color,
                        PrimitiveTypeInfo::I32,
                    );
                }
                Operator::F64Const { value } => {
                    self.push_node(
                        Lang::F64(value.bits()),
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
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::I32Store {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }
                Operator::I64Store { memarg } => {
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::I64Store {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }
                Operator::F32Store { memarg } => {
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::F32Store {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }
                Operator::F64Store { memarg } => {
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::F64Store {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }
                Operator::I32Store8 { memarg } => {
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::I32Store8 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }
                Operator::I32Store16 { memarg } => {
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::I32Store16 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }
                Operator::I64Store8 { memarg } => {
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::I64Store8 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }
                Operator::I64Store16 { memarg } => {
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::I64Store16 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }
                Operator::I64Store32 { memarg } => {
                    self.binop_cb(idx, color, Empty, |offset, value| Lang::I64Store32 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    color += 1;
                }

                // All memory loads
                Operator::I32Load { memarg } => {
                    self.unop_cb(idx, color, I32, |offset| Lang::I32Load {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I64Load { memarg } => {
                    self.unop_cb(idx, color, I64, |offset| Lang::I64Load {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::F32Load { memarg } => {
                    self.unop_cb(idx, color, F32, |offset| Lang::F32Load {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::F64Load { memarg } => {
                    self.unop_cb(idx, color, F64, |offset| Lang::F64Load {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I32Load8S { memarg } => {
                    self.unop_cb(idx, color, I32, |offset| Lang::I32Load8S {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I32Load8U { memarg } => {
                    self.unop_cb(idx, color, I32, |offset| Lang::I32Load8U {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I32Load16S { memarg } => {
                    self.unop_cb(idx, color, I32, |offset| Lang::I32Load16S {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I32Load16U { memarg } => {
                    self.unop_cb(idx, color, I32, |offset| Lang::I32Load16U {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I64Load8S { memarg } => {
                    self.unop_cb(idx, color, I64, |offset| Lang::I64Load8S {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I64Load8U { memarg } => {
                    self.unop_cb(idx, color, I64, |offset| Lang::I64Load8U {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I64Load16S { memarg } => {
                    self.unop_cb(idx, color, I64, |offset| Lang::I64Load16S {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I64Load16U { memarg } => {
                    self.unop_cb(idx, color, I64, |offset| Lang::I64Load16U {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I64Load32S { memarg } => {
                    self.unop_cb(idx, color, I64, |offset| Lang::I64Load32S {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }
                Operator::I64Load32U { memarg } => {
                    self.unop_cb(idx, color, I64, |offset| Lang::I64Load32U {
                        offset,
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    })
                }

                Operator::I32Eqz => self.unop(idx, color, Lang::I32Eqz, I32),
                Operator::I64Eqz => self.unop(idx, color, Lang::I64Eqz, I32),

                Operator::F32Eq => self.binop(idx, color, Lang::F32Eq, I32),
                Operator::F32Ne => self.binop(idx, color, Lang::F32Ne, I32),
                Operator::F32Lt => self.binop(idx, color, Lang::F32Lt, I32),
                Operator::F32Gt => self.binop(idx, color, Lang::F32Gt, I32),
                Operator::F32Le => self.binop(idx, color, Lang::F32Le, I32),
                Operator::F32Ge => self.binop(idx, color, Lang::F32Ge, I32),

                Operator::F64Eq => self.binop(idx, color, Lang::F64Eq, I32),
                Operator::F64Ne => self.binop(idx, color, Lang::F64Ne, I32),
                Operator::F64Lt => self.binop(idx, color, Lang::F64Lt, I32),
                Operator::F64Gt => self.binop(idx, color, Lang::F64Gt, I32),
                Operator::F64Le => self.binop(idx, color, Lang::F64Le, I32),
                Operator::F64Ge => self.binop(idx, color, Lang::F64Ge, I32),

                Operator::I32Clz => self.unop(idx, color, Lang::I32Clz, I32),
                Operator::I32Ctz => self.unop(idx, color, Lang::I32Ctz, I32),
                Operator::I64Clz => self.unop(idx, color, Lang::I64Clz, I64),
                Operator::I64Ctz => self.unop(idx, color, Lang::I64Ctz, I64),

                Operator::F32Abs => self.unop(idx, color, Lang::F32Abs, F32),
                Operator::F32Neg => self.unop(idx, color, Lang::F32Neg, F32),
                Operator::F32Ceil => self.unop(idx, color, Lang::F32Ceil, F32),
                Operator::F32Floor => self.unop(idx, color, Lang::F32Floor, F32),
                Operator::F32Trunc => self.unop(idx, color, Lang::F32Trunc, F32),
                Operator::F32Nearest => self.unop(idx, color, Lang::F32Nearest, F32),
                Operator::F32Sqrt => self.unop(idx, color, Lang::F32Sqrt, F32),
                Operator::F32Add => self.binop(idx, color, Lang::F32Add, F32),
                Operator::F32Sub => self.binop(idx, color, Lang::F32Sub, F32),
                Operator::F32Mul => self.binop(idx, color, Lang::F32Mul, F32),
                Operator::F32Div => self.binop(idx, color, Lang::F32Div, F32),
                Operator::F32Min => self.binop(idx, color, Lang::F32Min, F32),
                Operator::F32Max => self.binop(idx, color, Lang::F32Max, F32),
                Operator::F32Copysign => self.binop(idx, color, Lang::F32Copysign, F32),

                Operator::F64Abs => self.unop(idx, color, Lang::F64Abs, F64),
                Operator::F64Neg => self.unop(idx, color, Lang::F64Neg, F64),
                Operator::F64Ceil => self.unop(idx, color, Lang::F64Ceil, F64),
                Operator::F64Floor => self.unop(idx, color, Lang::F64Floor, F64),
                Operator::F64Trunc => self.unop(idx, color, Lang::F64Trunc, F64),
                Operator::F64Nearest => self.unop(idx, color, Lang::F64Nearest, F64),
                Operator::F64Sqrt => self.unop(idx, color, Lang::F64Sqrt, F64),
                Operator::F64Add => self.binop(idx, color, Lang::F64Add, F64),
                Operator::F64Sub => self.binop(idx, color, Lang::F64Sub, F64),
                Operator::F64Mul => self.binop(idx, color, Lang::F64Mul, F64),
                Operator::F64Div => self.binop(idx, color, Lang::F64Div, F64),
                Operator::F64Min => self.binop(idx, color, Lang::F64Min, F64),
                Operator::F64Max => self.binop(idx, color, Lang::F64Max, F64),
                Operator::F64Copysign => self.binop(idx, color, Lang::F64Copysign, F64),

                Operator::I32TruncF32S => self.unop(idx, color, Lang::I32TruncF32S, I32),
                Operator::I32TruncF32U => self.unop(idx, color, Lang::I32TruncF32U, I32),
                Operator::I32TruncF64S => self.unop(idx, color, Lang::I32TruncF64S, I32),
                Operator::I32TruncF64U => self.unop(idx, color, Lang::I32TruncF64U, I32),
                Operator::I64TruncF32S => self.unop(idx, color, Lang::I64TruncF32S, I64),
                Operator::I64TruncF32U => self.unop(idx, color, Lang::I64TruncF32U, I64),
                Operator::I64TruncF64S => self.unop(idx, color, Lang::I64TruncF64S, I64),
                Operator::I64TruncF64U => self.unop(idx, color, Lang::I64TruncF64U, I64),
                Operator::F32ConvertI32S => self.unop(idx, color, Lang::F32ConvertI32S, F32),
                Operator::F32ConvertI32U => self.unop(idx, color, Lang::F32ConvertI32U, F32),
                Operator::F32ConvertI64S => self.unop(idx, color, Lang::F32ConvertI64S, F32),
                Operator::F32ConvertI64U => self.unop(idx, color, Lang::F32ConvertI64U, F32),
                Operator::F64ConvertI32S => self.unop(idx, color, Lang::F64ConvertI32S, F64),
                Operator::F64ConvertI32U => self.unop(idx, color, Lang::F64ConvertI32U, F64),
                Operator::F64ConvertI64S => self.unop(idx, color, Lang::F64ConvertI64S, F64),
                Operator::F64ConvertI64U => self.unop(idx, color, Lang::F64ConvertI64U, F64),
                Operator::F64PromoteF32 => self.unop(idx, color, Lang::F64PromoteF32, F64),
                Operator::F32DemoteF64 => self.unop(idx, color, Lang::F32DemoteF64, F32),
                Operator::I32ReinterpretF32 => self.unop(idx, color, Lang::I32ReinterpretF32, I32),
                Operator::I64ReinterpretF64 => self.unop(idx, color, Lang::I64ReinterpretF64, I64),
                Operator::F32ReinterpretI32 => self.unop(idx, color, Lang::F32ReinterpretI32, F32),
                Operator::F64ReinterpretI64 => self.unop(idx, color, Lang::F64ReinterpretI64, F64),
                Operator::I32TruncSatF32S => self.unop(idx, color, Lang::I32TruncSatF32S, I32),
                Operator::I32TruncSatF32U => self.unop(idx, color, Lang::I32TruncSatF32U, I32),
                Operator::I32TruncSatF64S => self.unop(idx, color, Lang::I32TruncSatF64S, I32),
                Operator::I32TruncSatF64U => self.unop(idx, color, Lang::I32TruncSatF64U, I32),
                Operator::I64TruncSatF32S => self.unop(idx, color, Lang::I64TruncSatF32S, I64),
                Operator::I64TruncSatF32U => self.unop(idx, color, Lang::I64TruncSatF32U, I64),
                Operator::I64TruncSatF64S => self.unop(idx, color, Lang::I64TruncSatF64S, I64),
                Operator::I64TruncSatF64U => self.unop(idx, color, Lang::I64TruncSatF64U, I64),

                Operator::I32Add => self.binop(idx, color, Lang::I32Add, I32),
                Operator::I32Sub => self.binop(idx, color, Lang::I32Sub, I32),
                Operator::I32Eq => self.binop(idx, color, Lang::I32Eq, I32),
                Operator::I32Ne => self.binop(idx, color, Lang::I32Ne, I32),
                Operator::I32LtS => self.binop(idx, color, Lang::I32LtS, I32),
                Operator::I32LtU => self.binop(idx, color, Lang::I32LtU, I32),
                Operator::I32GtS => self.binop(idx, color, Lang::I32GtS, I32),
                Operator::I32GtU => self.binop(idx, color, Lang::I32GtU, I32),
                Operator::I32LeS => self.binop(idx, color, Lang::I32LeS, I32),
                Operator::I32LeU => self.binop(idx, color, Lang::I32LeU, I32),
                Operator::I32GeS => self.binop(idx, color, Lang::I32GeS, I32),
                Operator::I32GeU => self.binop(idx, color, Lang::I32GeU, I32),
                Operator::I32Mul => self.binop(idx, color, Lang::I32Mul, I32),
                Operator::I32DivS => self.binop(idx, color, Lang::I32DivS, I32),
                Operator::I32DivU => self.binop(idx, color, Lang::I32DivU, I32),
                Operator::I32RemS => self.binop(idx, color, Lang::I32RemS, I32),
                Operator::I32RemU => self.binop(idx, color, Lang::I32RemU, I32),
                Operator::I32Shl => self.binop(idx, color, Lang::I32Shl, I32),
                Operator::I32ShrS => self.binop(idx, color, Lang::I32ShrS, I32),
                Operator::I32ShrU => self.binop(idx, color, Lang::I32ShrU, I32),
                Operator::I32Xor => self.binop(idx, color, Lang::I32Xor, I32),
                Operator::I32Or => self.binop(idx, color, Lang::I32Or, I32),
                Operator::I32And => self.binop(idx, color, Lang::I32And, I32),
                Operator::I32Rotl => self.binop(idx, color, Lang::I32RotL, I32),
                Operator::I32Rotr => self.binop(idx, color, Lang::I32RotR, I32),

                Operator::I64Add => self.binop(idx, color, Lang::I64Add, I64),
                Operator::I64Sub => self.binop(idx, color, Lang::I64Sub, I64),
                Operator::I64Eq => self.binop(idx, color, Lang::I64Eq, I64),
                Operator::I64Ne => self.binop(idx, color, Lang::I64Ne, I64),
                Operator::I64LtS => self.binop(idx, color, Lang::I64LtS, I64),
                Operator::I64LtU => self.binop(idx, color, Lang::I64LtU, I64),
                Operator::I64GtS => self.binop(idx, color, Lang::I64GtS, I64),
                Operator::I64GtU => self.binop(idx, color, Lang::I64GtU, I64),
                Operator::I64LeS => self.binop(idx, color, Lang::I64LeS, I64),
                Operator::I64LeU => self.binop(idx, color, Lang::I64LeU, I64),
                Operator::I64GeS => self.binop(idx, color, Lang::I64GeS, I64),
                Operator::I64GeU => self.binop(idx, color, Lang::I64GeU, I64),
                Operator::I64Mul => self.binop(idx, color, Lang::I64Mul, I64),
                Operator::I64DivS => self.binop(idx, color, Lang::I64DivS, I64),
                Operator::I64DivU => self.binop(idx, color, Lang::I64DivU, I64),
                Operator::I64RemS => self.binop(idx, color, Lang::I64RemS, I64),
                Operator::I64RemU => self.binop(idx, color, Lang::I64RemU, I64),
                Operator::I64Shl => self.binop(idx, color, Lang::I64Shl, I64),
                Operator::I64ShrS => self.binop(idx, color, Lang::I64ShrS, I64),
                Operator::I64ShrU => self.binop(idx, color, Lang::I64ShrU, I64),
                Operator::I64Xor => self.binop(idx, color, Lang::I64Xor, I64),
                Operator::I64Or => self.binop(idx, color, Lang::I64Or, I64),
                Operator::I64And => self.binop(idx, color, Lang::I64And, I64),
                Operator::I64Rotl => self.binop(idx, color, Lang::I64RotL, I64),
                Operator::I64Rotr => self.binop(idx, color, Lang::I64RotR, I64),

                Operator::Drop => self.unop(idx, color, Lang::Drop, Empty),

                // conversion between integers
                Operator::I32WrapI64 => self.unop(idx, color, Lang::Wrap, I32),
                Operator::I32Extend8S => self.unop(idx, color, Lang::I32Extend8S, I32),
                Operator::I32Extend16S => self.unop(idx, color, Lang::I32Extend16S, I32),

                Operator::I64Extend8S => self.unop(idx, color, Lang::I64Extend8S, I64),
                Operator::I64Extend16S => self.unop(idx, color, Lang::I64Extend16S, I64),
                Operator::I64Extend32S => self.unop(idx, color, Lang::I64Extend32S, I64),
                Operator::I64ExtendI32S => self.unop(idx, color, Lang::I64ExtendI32S, I64),
                Operator::I64ExtendI32U => self.unop(idx, color, Lang::I64ExtendI32U, I64),

                Operator::I32Popcnt => self.unop(idx, color, Lang::I32Popcnt, I32),
                Operator::I64Popcnt => self.unop(idx, color, Lang::I64Popcnt, I64),

                Operator::Select => {
                    let condition = self.pop_operand(idx, false);
                    let alternative = self.pop_operand(idx, false);
                    let consequent = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::Select([
                            Id::from(condition),
                            Id::from(consequent),
                            Id::from(alternative),
                        ]),
                        idx,
                        vec![condition, consequent, alternative],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[condition] = idx as i32;
                    self.parents[consequent] = idx as i32;
                    self.parents[alternative] = idx as i32;
                }
                Operator::MemoryGrow { mem, mem_byte } => {
                    let arg = self.pop_operand(idx, false);

                    let idx = self.push_node(
                        Lang::MemoryGrow {
                            mem: *mem,
                            mem_byte: *mem_byte,
                            by: Id::from(arg),
                        },
                        idx,
                        vec![arg],
                        color,
                        PrimitiveTypeInfo::I32,
                    );

                    self.parents[arg] = idx as i32;
                }
                Operator::MemorySize { mem, mem_byte } => {
                    let _idx = self.push_node(
                        Lang::MemorySize {
                            mem: *mem,
                            mem_byte: *mem_byte,
                        },
                        idx,
                        vec![],
                        color,
                        PrimitiveTypeInfo::I32,
                    );
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

    fn unop(&mut self, idx: usize, color: u32, op: fn([Id; 1]) -> Lang, ty: PrimitiveTypeInfo) {
        self.unop_cb(idx, color, ty, |id| op([id]))
    }

    fn unop_cb(&mut self, idx: usize, color: u32, ty: PrimitiveTypeInfo, op: impl Fn(Id) -> Lang) {
        let arg = self.pop_operand(idx, false);
        let idx = self.push_node(op(Id::from(arg)), idx, vec![arg], color, ty);
        self.parents[arg] = idx as i32;
    }

    fn binop(&mut self, idx: usize, color: u32, op: fn([Id; 2]) -> Lang, ty: PrimitiveTypeInfo) {
        self.binop_cb(idx, color, ty, |a, b| op([a, b]))
    }

    fn binop_cb(
        &mut self,
        idx: usize,
        color: u32,
        ty: PrimitiveTypeInfo,
        op: impl Fn(Id, Id) -> Lang,
    ) {
        let leftidx = self.pop_operand(idx, false);
        let rightidx = self.pop_operand(idx, false);

        // The operands should not be the same
        assert_ne!(leftidx, rightidx);

        let idx = self.push_node(
            op(Id::from(rightidx), Id::from(leftidx)),
            idx,
            vec![rightidx, leftidx],
            color,
            ty,
        );

        self.parents[leftidx] = idx as i32;
        self.parents[rightidx] = idx as i32;
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
