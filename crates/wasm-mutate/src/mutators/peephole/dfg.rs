//! DFG extractor for Wasm functions. It converts Wasm operators to the [Lang]
//! intermediate representation.
use std::collections::HashMap;

use egg::{Id, Language, RecExpr};
use wasmparser::{Operator, Range};

use crate::mutators::peephole::Lang;
use crate::ModuleInfo;

use crate::mutators::OperatorAndByteOffset;

use super::eggsy::encoder::rebuild::build_expr;

/// It executes a minimal symbolic evaluation of the stack to detect operands
/// location in the code for certain operators
///
/// For example, the `i32.add` operator should know who are its operands
pub struct DFGBuilder {
    /// Current color of the state of the world. This is incremented any time
    /// the Wasm mutates state, for example makes a function call or writes to
    /// memory. We generally only apply mutations to expressions where all
    /// subexpressions have the same color as the root expression. This helps us
    /// avoid incorrectly reordering side-effectful operations.
    color: u32,

    stack: Vec<usize>,
    dfg_map: Vec<StackEntry>,
    operator_index_to_entry_index: HashMap<usize, usize>,
    parents: Vec<Option<usize>>,
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
    pub parents: Vec<Option<usize>>,
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

            for i in entry.operator.children() {
                worklist.push(&self.entries[usize::from(*i)]);
            }
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

        fn get_ansi_term_color(color: u32) -> &'static str {
            match color {
                0 => "\u{001b}[31m",             // red
                1 => "\u{001b}[32m",             // green
                2 => "\u{001b}[33m",             // yellow
                3 => "\u{001b}[34m",             // blue
                4 => "\u{001b}[35m",             // magenta
                5 => "\u{001b}[36m",             // cyan
                6 => "\u{001b}[37;1m",           // bright white
                7 => "\u{001b}[32;1m",           // bright green
                8 => "\u{001b}[33;1m",           // bright yellow
                9 => "\u{001b}[36;1m",           // bright cyan
                10 => "\u{001b}[35;1m",          // bright magenta
                UNDEF_COLOR => "\u{001b}[31;1m", // bright red
                _ => "\u{001b}[0m",              // reset color (terminal default)
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
            let color = get_ansi_term_color(entry.color);
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

            for (idx, op) in entry.operator.children().iter().enumerate() {
                if idx < entry.operator.children().len() - 1 {
                    // Has no next child
                    let preffix = format!("{}{}", childrenpreffix, "├──");
                    let childrenpreffix = format!("{}{}", childrenpreffix, "│   ");
                    write_child(
                        minidfg,
                        usize::from(*op),
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
                        usize::from(*op),
                        &preffix,
                        entryformatter,
                        &childrenpreffix,
                        builder,
                    );
                }
            }
        }

        // Print the roots.
        for (entryidx, parent) in self.parents.iter().enumerate() {
            if parent.is_none() {
                write_child(self, entryidx, "", entryformatter, "", &mut builder);
            }
        }

        builder
    }

    /// Returns a RecExpr from the stack entry
    /// by cleaning spurious nodes
    pub fn get_expr(&self, at: usize) -> RecExpr<Lang> {
        let root = self.map[&at];
        let entries = &self.entries[..=root];
        build_expr(Id::from(root), |id| &entries[usize::from(id)].operator)
    }
}

const UNDEF_COLOR: u32 = u32::MAX;

impl<'a> DFGBuilder {
    /// Returns a new DFG builder
    pub fn new() -> Self {
        DFGBuilder {
            color: 0,
            stack: Vec::new(),
            dfg_map: Vec::new(),
            operator_index_to_entry_index: HashMap::new(),
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

    fn empty_node(&mut self, operator: Lang, operator_idx: usize) -> usize {
        self.register_node(operator, operator_idx, false)
    }

    fn push_node(&mut self, operator: Lang, operator_idx: usize) -> usize {
        self.register_node(operator, operator_idx, true)
    }

    fn register_node(&mut self, operator: Lang, operator_idx: usize, push_to_stack: bool) -> usize {
        let entry_idx = self.dfg_map.len();

        // Add the parent links for this node's operands.
        for id in operator.children() {
            let id = usize::from(*id);
            debug_assert!(self.parents[id].is_none());
            self.parents[id] = Some(entry_idx);
        }

        let new_node = StackEntry {
            operator,
            entry_idx,
            color: self.color,
            operator_idx,
        };

        self.operator_index_to_entry_index
            .insert(operator_idx, entry_idx);

        if push_to_stack {
            self.stack.push(entry_idx)
        } else {
            self.new_color();
        }

        // Add the data flow link
        self.dfg_map.push(new_node);
        self.parents.push(None);

        entry_idx
    }

    fn pop_operand(&mut self, operator_idx: usize, insert_in_dfg: bool) -> usize {
        self.stack
            .pop()
            .or_else(|| {
                let entry_idx = self.dfg_map.len();

                let leaf = StackEntry {
                    operator: Lang::Undef,
                    entry_idx,
                    color: UNDEF_COLOR,
                    operator_idx,
                };

                if insert_in_dfg {
                    self.operator_index_to_entry_index
                        .insert(operator_idx, entry_idx);
                }

                // Add the data flow link. No parent yet.
                self.dfg_map.push(leaf);
                self.parents.push(None);

                Some(entry_idx)
            })
            .unwrap()
    }

    fn new_color(&mut self) {
        self.color += 1;
        assert!(self.color < UNDEF_COLOR);
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
    ) -> Option<MiniDFG> {
        // Create a DFG from the BB. Start from the first operator and simulate
        // the stack. If an operator is missing in the stack then it probably
        // comes from a previous BB.

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
                            self.register_node(
                                Lang::Call(
                                    *function_index as usize,
                                    operands.iter().map(|i| Id::from(*i)).collect::<Vec<_>>(),
                                ),
                                idx,
                                !tpe.returns.is_empty(),
                            );

                            self.new_color();
                        }
                    }
                }
                Operator::LocalGet { local_index } => {
                    self.push_node(Lang::LocalGet(*local_index), idx);
                }
                Operator::GlobalGet { global_index } => {
                    self.push_node(Lang::GlobalGet(*global_index), idx);
                }
                Operator::GlobalSet { global_index } => {
                    let child = self.pop_operand(idx, true);
                    self.empty_node(Lang::GlobalSet(*global_index, Id::from(child)), idx);
                }
                Operator::I32Const { value } => {
                    self.push_node(Lang::I32(*value), idx);
                }
                Operator::I64Const { value } => {
                    self.push_node(Lang::I64(*value), idx);
                }
                Operator::F32Const { value } => {
                    self.push_node(Lang::F32(value.bits()), idx);
                }
                Operator::F64Const { value } => {
                    self.push_node(Lang::F64(value.bits()), idx);
                }
                Operator::LocalSet { local_index } => {
                    let val = self.pop_operand(idx, true);
                    self.empty_node(Lang::LocalSet(*local_index, Id::from(val)), idx);
                }
                Operator::LocalTee { local_index } => {
                    let val = self.pop_operand(idx, true);
                    self.push_node(Lang::LocalTee(*local_index, Id::from(val)), idx);
                    self.new_color();
                }

                Operator::I32Store { memarg } => {
                    self.store(idx, |offset, value| Lang::I32Store {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }
                Operator::I64Store { memarg } => {
                    self.store(idx, |offset, value| Lang::I64Store {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }
                Operator::F32Store { memarg } => {
                    self.store(idx, |offset, value| Lang::F32Store {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }
                Operator::F64Store { memarg } => {
                    self.store(idx, |offset, value| Lang::F64Store {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }
                Operator::I32Store8 { memarg } => {
                    self.store(idx, |offset, value| Lang::I32Store8 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }
                Operator::I32Store16 { memarg } => {
                    self.store(idx, |offset, value| Lang::I32Store16 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }
                Operator::I64Store8 { memarg } => {
                    self.store(idx, |offset, value| Lang::I64Store8 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }
                Operator::I64Store16 { memarg } => {
                    self.store(idx, |offset, value| Lang::I64Store16 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }
                Operator::I64Store32 { memarg } => {
                    self.store(idx, |offset, value| Lang::I64Store32 {
                        value_and_offset: [offset, value],
                        static_offset: memarg.offset,
                        align: memarg.align,
                        mem: memarg.memory,
                    });
                    self.new_color();
                }

                // All memory loads
                Operator::I32Load { memarg } => self.unop_cb(idx, |offset| Lang::I32Load {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I64Load { memarg } => self.unop_cb(idx, |offset| Lang::I64Load {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::F32Load { memarg } => self.unop_cb(idx, |offset| Lang::F32Load {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::F64Load { memarg } => self.unop_cb(idx, |offset| Lang::F64Load {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I32Load8S { memarg } => self.unop_cb(idx, |offset| Lang::I32Load8S {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I32Load8U { memarg } => self.unop_cb(idx, |offset| Lang::I32Load8U {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I32Load16S { memarg } => self.unop_cb(idx, |offset| Lang::I32Load16S {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I32Load16U { memarg } => self.unop_cb(idx, |offset| Lang::I32Load16U {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I64Load8S { memarg } => self.unop_cb(idx, |offset| Lang::I64Load8S {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I64Load8U { memarg } => self.unop_cb(idx, |offset| Lang::I64Load8U {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I64Load16S { memarg } => self.unop_cb(idx, |offset| Lang::I64Load16S {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I64Load16U { memarg } => self.unop_cb(idx, |offset| Lang::I64Load16U {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I64Load32S { memarg } => self.unop_cb(idx, |offset| Lang::I64Load32S {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),
                Operator::I64Load32U { memarg } => self.unop_cb(idx, |offset| Lang::I64Load32U {
                    offset,
                    static_offset: memarg.offset,
                    align: memarg.align,
                    mem: memarg.memory,
                }),

                Operator::I32Eqz => self.unop(idx, Lang::I32Eqz),
                Operator::I64Eqz => self.unop(idx, Lang::I64Eqz),

                Operator::F32Eq => self.binop(idx, Lang::F32Eq),
                Operator::F32Ne => self.binop(idx, Lang::F32Ne),
                Operator::F32Lt => self.binop(idx, Lang::F32Lt),
                Operator::F32Gt => self.binop(idx, Lang::F32Gt),
                Operator::F32Le => self.binop(idx, Lang::F32Le),
                Operator::F32Ge => self.binop(idx, Lang::F32Ge),

                Operator::F64Eq => self.binop(idx, Lang::F64Eq),
                Operator::F64Ne => self.binop(idx, Lang::F64Ne),
                Operator::F64Lt => self.binop(idx, Lang::F64Lt),
                Operator::F64Gt => self.binop(idx, Lang::F64Gt),
                Operator::F64Le => self.binop(idx, Lang::F64Le),
                Operator::F64Ge => self.binop(idx, Lang::F64Ge),

                Operator::I32Clz => self.unop(idx, Lang::I32Clz),
                Operator::I32Ctz => self.unop(idx, Lang::I32Ctz),
                Operator::I64Clz => self.unop(idx, Lang::I64Clz),
                Operator::I64Ctz => self.unop(idx, Lang::I64Ctz),

                Operator::F32Abs => self.unop(idx, Lang::F32Abs),
                Operator::F32Neg => self.unop(idx, Lang::F32Neg),
                Operator::F32Ceil => self.unop(idx, Lang::F32Ceil),
                Operator::F32Floor => self.unop(idx, Lang::F32Floor),
                Operator::F32Trunc => self.unop(idx, Lang::F32Trunc),
                Operator::F32Nearest => self.unop(idx, Lang::F32Nearest),
                Operator::F32Sqrt => self.unop(idx, Lang::F32Sqrt),
                Operator::F32Add => self.binop(idx, Lang::F32Add),
                Operator::F32Sub => self.binop(idx, Lang::F32Sub),
                Operator::F32Mul => self.binop(idx, Lang::F32Mul),
                Operator::F32Div => self.binop(idx, Lang::F32Div),
                Operator::F32Min => self.binop(idx, Lang::F32Min),
                Operator::F32Max => self.binop(idx, Lang::F32Max),
                Operator::F32Copysign => self.binop(idx, Lang::F32Copysign),

                Operator::F64Abs => self.unop(idx, Lang::F64Abs),
                Operator::F64Neg => self.unop(idx, Lang::F64Neg),
                Operator::F64Ceil => self.unop(idx, Lang::F64Ceil),
                Operator::F64Floor => self.unop(idx, Lang::F64Floor),
                Operator::F64Trunc => self.unop(idx, Lang::F64Trunc),
                Operator::F64Nearest => self.unop(idx, Lang::F64Nearest),
                Operator::F64Sqrt => self.unop(idx, Lang::F64Sqrt),
                Operator::F64Add => self.binop(idx, Lang::F64Add),
                Operator::F64Sub => self.binop(idx, Lang::F64Sub),
                Operator::F64Mul => self.binop(idx, Lang::F64Mul),
                Operator::F64Div => self.binop(idx, Lang::F64Div),
                Operator::F64Min => self.binop(idx, Lang::F64Min),
                Operator::F64Max => self.binop(idx, Lang::F64Max),
                Operator::F64Copysign => self.binop(idx, Lang::F64Copysign),

                Operator::I32TruncF32S => self.unop(idx, Lang::I32TruncF32S),
                Operator::I32TruncF32U => self.unop(idx, Lang::I32TruncF32U),
                Operator::I32TruncF64S => self.unop(idx, Lang::I32TruncF64S),
                Operator::I32TruncF64U => self.unop(idx, Lang::I32TruncF64U),
                Operator::I64TruncF32S => self.unop(idx, Lang::I64TruncF32S),
                Operator::I64TruncF32U => self.unop(idx, Lang::I64TruncF32U),
                Operator::I64TruncF64S => self.unop(idx, Lang::I64TruncF64S),
                Operator::I64TruncF64U => self.unop(idx, Lang::I64TruncF64U),
                Operator::F32ConvertI32S => self.unop(idx, Lang::F32ConvertI32S),
                Operator::F32ConvertI32U => self.unop(idx, Lang::F32ConvertI32U),
                Operator::F32ConvertI64S => self.unop(idx, Lang::F32ConvertI64S),
                Operator::F32ConvertI64U => self.unop(idx, Lang::F32ConvertI64U),
                Operator::F64ConvertI32S => self.unop(idx, Lang::F64ConvertI32S),
                Operator::F64ConvertI32U => self.unop(idx, Lang::F64ConvertI32U),
                Operator::F64ConvertI64S => self.unop(idx, Lang::F64ConvertI64S),
                Operator::F64ConvertI64U => self.unop(idx, Lang::F64ConvertI64U),
                Operator::F64PromoteF32 => self.unop(idx, Lang::F64PromoteF32),
                Operator::F32DemoteF64 => self.unop(idx, Lang::F32DemoteF64),
                Operator::I32ReinterpretF32 => self.unop(idx, Lang::I32ReinterpretF32),
                Operator::I64ReinterpretF64 => self.unop(idx, Lang::I64ReinterpretF64),
                Operator::F32ReinterpretI32 => self.unop(idx, Lang::F32ReinterpretI32),
                Operator::F64ReinterpretI64 => self.unop(idx, Lang::F64ReinterpretI64),
                Operator::I32TruncSatF32S => self.unop(idx, Lang::I32TruncSatF32S),
                Operator::I32TruncSatF32U => self.unop(idx, Lang::I32TruncSatF32U),
                Operator::I32TruncSatF64S => self.unop(idx, Lang::I32TruncSatF64S),
                Operator::I32TruncSatF64U => self.unop(idx, Lang::I32TruncSatF64U),
                Operator::I64TruncSatF32S => self.unop(idx, Lang::I64TruncSatF32S),
                Operator::I64TruncSatF32U => self.unop(idx, Lang::I64TruncSatF32U),
                Operator::I64TruncSatF64S => self.unop(idx, Lang::I64TruncSatF64S),
                Operator::I64TruncSatF64U => self.unop(idx, Lang::I64TruncSatF64U),

                Operator::I32Add => self.binop(idx, Lang::I32Add),
                Operator::I32Sub => self.binop(idx, Lang::I32Sub),
                Operator::I32Eq => self.binop(idx, Lang::I32Eq),
                Operator::I32Ne => self.binop(idx, Lang::I32Ne),
                Operator::I32LtS => self.binop(idx, Lang::I32LtS),
                Operator::I32LtU => self.binop(idx, Lang::I32LtU),
                Operator::I32GtS => self.binop(idx, Lang::I32GtS),
                Operator::I32GtU => self.binop(idx, Lang::I32GtU),
                Operator::I32LeS => self.binop(idx, Lang::I32LeS),
                Operator::I32LeU => self.binop(idx, Lang::I32LeU),
                Operator::I32GeS => self.binop(idx, Lang::I32GeS),
                Operator::I32GeU => self.binop(idx, Lang::I32GeU),
                Operator::I32Mul => self.binop(idx, Lang::I32Mul),
                Operator::I32DivS => self.binop(idx, Lang::I32DivS),
                Operator::I32DivU => self.binop(idx, Lang::I32DivU),
                Operator::I32RemS => self.binop(idx, Lang::I32RemS),
                Operator::I32RemU => self.binop(idx, Lang::I32RemU),
                Operator::I32Shl => self.binop(idx, Lang::I32Shl),
                Operator::I32ShrS => self.binop(idx, Lang::I32ShrS),
                Operator::I32ShrU => self.binop(idx, Lang::I32ShrU),
                Operator::I32Xor => self.binop(idx, Lang::I32Xor),
                Operator::I32Or => self.binop(idx, Lang::I32Or),
                Operator::I32And => self.binop(idx, Lang::I32And),
                Operator::I32Rotl => self.binop(idx, Lang::I32RotL),
                Operator::I32Rotr => self.binop(idx, Lang::I32RotR),

                Operator::I64Add => self.binop(idx, Lang::I64Add),
                Operator::I64Sub => self.binop(idx, Lang::I64Sub),
                Operator::I64Eq => self.binop(idx, Lang::I64Eq),
                Operator::I64Ne => self.binop(idx, Lang::I64Ne),
                Operator::I64LtS => self.binop(idx, Lang::I64LtS),
                Operator::I64LtU => self.binop(idx, Lang::I64LtU),
                Operator::I64GtS => self.binop(idx, Lang::I64GtS),
                Operator::I64GtU => self.binop(idx, Lang::I64GtU),
                Operator::I64LeS => self.binop(idx, Lang::I64LeS),
                Operator::I64LeU => self.binop(idx, Lang::I64LeU),
                Operator::I64GeS => self.binop(idx, Lang::I64GeS),
                Operator::I64GeU => self.binop(idx, Lang::I64GeU),
                Operator::I64Mul => self.binop(idx, Lang::I64Mul),
                Operator::I64DivS => self.binop(idx, Lang::I64DivS),
                Operator::I64DivU => self.binop(idx, Lang::I64DivU),
                Operator::I64RemS => self.binop(idx, Lang::I64RemS),
                Operator::I64RemU => self.binop(idx, Lang::I64RemU),
                Operator::I64Shl => self.binop(idx, Lang::I64Shl),
                Operator::I64ShrS => self.binop(idx, Lang::I64ShrS),
                Operator::I64ShrU => self.binop(idx, Lang::I64ShrU),
                Operator::I64Xor => self.binop(idx, Lang::I64Xor),
                Operator::I64Or => self.binop(idx, Lang::I64Or),
                Operator::I64And => self.binop(idx, Lang::I64And),
                Operator::I64Rotl => self.binop(idx, Lang::I64RotL),
                Operator::I64Rotr => self.binop(idx, Lang::I64RotR),

                Operator::Drop => {
                    let arg = self.pop_operand(idx, false);
                    self.empty_node(Lang::Drop([Id::from(arg)]), idx);
                }

                // conversion between integers
                Operator::I32WrapI64 => self.unop(idx, Lang::Wrap),
                Operator::I32Extend8S => self.unop(idx, Lang::I32Extend8S),
                Operator::I32Extend16S => self.unop(idx, Lang::I32Extend16S),

                Operator::I64Extend8S => self.unop(idx, Lang::I64Extend8S),
                Operator::I64Extend16S => self.unop(idx, Lang::I64Extend16S),
                Operator::I64Extend32S => self.unop(idx, Lang::I64Extend32S),
                Operator::I64ExtendI32S => self.unop(idx, Lang::I64ExtendI32S),
                Operator::I64ExtendI32U => self.unop(idx, Lang::I64ExtendI32U),

                Operator::I32Popcnt => self.unop(idx, Lang::I32Popcnt),
                Operator::I64Popcnt => self.unop(idx, Lang::I64Popcnt),

                Operator::Select => {
                    let condition = self.pop_operand(idx, false);
                    let alternative = self.pop_operand(idx, false);
                    let consequent = self.pop_operand(idx, false);
                    self.push_node(
                        Lang::Select([
                            Id::from(consequent),
                            Id::from(alternative),
                            Id::from(condition),
                        ]),
                        idx,
                    );
                }
                Operator::MemoryGrow { mem, mem_byte } => {
                    let arg = self.pop_operand(idx, false);
                    self.push_node(
                        Lang::MemoryGrow {
                            mem: *mem,
                            mem_byte: *mem_byte,
                            by: Id::from(arg),
                        },
                        idx,
                    );
                }
                Operator::MemorySize { mem, mem_byte } => {
                    self.push_node(
                        Lang::MemorySize {
                            mem: *mem,
                            mem_byte: *mem_byte,
                        },
                        idx,
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
            map: self.operator_index_to_entry_index.clone(),
            parents: self.parents.clone(),
        })
    }

    fn unop(&mut self, idx: usize, op: fn([Id; 1]) -> Lang) {
        self.unop_cb(idx, |id| op([id]))
    }

    fn unop_cb(&mut self, idx: usize, op: impl Fn(Id) -> Lang) {
        let arg = self.pop_operand(idx, false);
        self.push_node(op(Id::from(arg)), idx);
    }

    fn binop(&mut self, idx: usize, op: fn([Id; 2]) -> Lang) {
        let leftidx = self.pop_operand(idx, false);
        let rightidx = self.pop_operand(idx, false);

        // The operands should not be the same
        assert_ne!(leftidx, rightidx);

        self.push_node(op([Id::from(rightidx), Id::from(leftidx)]), idx);
    }

    fn store(&mut self, idx: usize, op: impl Fn(Id, Id) -> Lang) {
        let leftidx = self.pop_operand(idx, false);
        let rightidx = self.pop_operand(idx, false);

        // The operands should not be the same
        assert_ne!(leftidx, rightidx);

        self.empty_node(op(Id::from(rightidx), Id::from(leftidx)), idx);
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
                    let roots = DFGBuilder::new().get_dfg(&ModuleInfo::default(), &operators, &bb);
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
                    let roots = DFGBuilder::new().get_dfg(&ModuleInfo::default(), &operators, &bb);
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
                    let roots = DFGBuilder::new().get_dfg(&info, &operators, &bb);
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
