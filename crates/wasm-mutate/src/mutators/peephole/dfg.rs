//! DFG extractor for Wasm functions. It converts Wasm operators to the [Lang]
//! intermediate representation.

use super::eggsy::encoder::rebuild::build_expr;
use crate::mutators::peephole::{
    Lang, MemArg, MemArgLane, MemoryCopy, MemoryInit, RefType, Shuffle, TableCopy, TableInit,
};
use crate::mutators::OperatorAndByteOffset;
use crate::{ModuleInfo, WasmMutate};
use egg::{Id, Language, RecExpr};
use std::collections::HashMap;
use std::ops::Range;
use wasmparser::Operator;
use wasmparser::SimdOperator;

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
    preserve_semantics: bool,
}

/// Basic block of a Wasm's function defined as a range of operators in the Wasm
/// function
#[derive(Debug)]
pub struct BBlock {
    pub(crate) range: Range<usize>,
}

/// Node of a DFG extracted from a basic block in the Wasm code
#[derive(Debug, Clone)]
pub struct StackEntry {
    /// Lang enode operator mapping
    pub operator: Lang,
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
            builder.push_str(preffix);
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
    pub fn new(config: &WasmMutate) -> Self {
        DFGBuilder {
            color: 0,
            stack: Vec::new(),
            dfg_map: Vec::new(),
            operator_index_to_entry_index: HashMap::new(),
            parents: Vec::new(),
            preserve_semantics: config.preserve_semantics,
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
        // The range is inclusive in the last operator
        let mut range = operator_index..operator_index + 1;
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
        // If we're not in semantics preservation mode then there's no need to
        // ever switch colors because the color here is only used to ensure that
        // we don't break edges between effectful instructions, which is all
        // about preserving semantics.
        if self.preserve_semantics {
            self.color += 1;
            assert!(self.color < UNDEF_COLOR);
        }
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

            use Operator as Op;
            use SimdOperator as SimdOp;
            match operator {
                Op::Call { function_index } => {
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
                                    *function_index,
                                    operands.iter().map(|i| Id::from(*i)).collect::<Vec<_>>(),
                                ),
                                idx,
                                !tpe.returns.is_empty(),
                            );

                            self.new_color();
                        }
                    }
                }
                Op::LocalGet { local_index } => {
                    self.push_node(Lang::LocalGet(*local_index), idx);
                }
                Op::GlobalGet { global_index } => {
                    self.push_node(Lang::GlobalGet(*global_index), idx);
                }
                Op::GlobalSet { global_index } => {
                    let child = self.pop_operand(idx, true);
                    self.empty_node(Lang::GlobalSet(*global_index, Id::from(child)), idx);
                }
                Op::I32Const { value } => {
                    self.push_node(Lang::I32(*value), idx);
                }
                Op::I64Const { value } => {
                    self.push_node(Lang::I64(*value), idx);
                }
                Op::F32Const { value } => {
                    self.push_node(Lang::F32((*value).into()), idx);
                }
                Op::F64Const { value } => {
                    self.push_node(Lang::F64((*value).into()), idx);
                }
                Op::Simd(SimdOp::V128Const { value }) => {
                    self.push_node(Lang::V128(value.i128()), idx);
                }
                Op::LocalSet { local_index } => {
                    let val = self.pop_operand(idx, true);
                    self.empty_node(Lang::LocalSet(*local_index, Id::from(val)), idx);
                }
                Op::LocalTee { local_index } => {
                    let val = self.pop_operand(idx, true);
                    self.push_node(Lang::LocalTee(*local_index, Id::from(val)), idx);
                    self.new_color();
                }

                Op::Nop => {
                    self.empty_node(Lang::Nop, idx);
                }

                Op::I32Store { memarg } => self.store(idx, memarg, Lang::I32Store),
                Op::I64Store { memarg } => self.store(idx, memarg, Lang::I64Store),
                Op::F32Store { memarg } => self.store(idx, memarg, Lang::F32Store),
                Op::F64Store { memarg } => self.store(idx, memarg, Lang::F64Store),
                Op::I32Store8 { memarg } => self.store(idx, memarg, Lang::I32Store8),
                Op::I32Store16 { memarg } => self.store(idx, memarg, Lang::I32Store16),
                Op::I64Store8 { memarg } => self.store(idx, memarg, Lang::I64Store8),
                Op::I64Store16 { memarg } => self.store(idx, memarg, Lang::I64Store16),
                Op::I64Store32 { memarg } => self.store(idx, memarg, Lang::I64Store32),

                // All memory loads
                Op::I32Load { memarg } => self.load(idx, memarg, Lang::I32Load),
                Op::I64Load { memarg } => self.load(idx, memarg, Lang::I64Load),
                Op::F32Load { memarg } => self.load(idx, memarg, Lang::F32Load),
                Op::F64Load { memarg } => self.load(idx, memarg, Lang::F64Load),
                Op::I32Load8S { memarg } => self.load(idx, memarg, Lang::I32Load8S),
                Op::I32Load8U { memarg } => self.load(idx, memarg, Lang::I32Load8U),
                Op::I32Load16S { memarg } => self.load(idx, memarg, Lang::I32Load16S),
                Op::I32Load16U { memarg } => self.load(idx, memarg, Lang::I32Load16U),
                Op::I64Load8S { memarg } => self.load(idx, memarg, Lang::I64Load8S),
                Op::I64Load8U { memarg } => self.load(idx, memarg, Lang::I64Load8U),
                Op::I64Load16S { memarg } => self.load(idx, memarg, Lang::I64Load16S),
                Op::I64Load16U { memarg } => self.load(idx, memarg, Lang::I64Load16U),
                Op::I64Load32S { memarg } => self.load(idx, memarg, Lang::I64Load32S),
                Op::I64Load32U { memarg } => self.load(idx, memarg, Lang::I64Load32U),

                Op::I32Eqz => self.unop(idx, Lang::I32Eqz),
                Op::I64Eqz => self.unop(idx, Lang::I64Eqz),

                Op::F32Eq => self.binop(idx, Lang::F32Eq),
                Op::F32Ne => self.binop(idx, Lang::F32Ne),
                Op::F32Lt => self.binop(idx, Lang::F32Lt),
                Op::F32Gt => self.binop(idx, Lang::F32Gt),
                Op::F32Le => self.binop(idx, Lang::F32Le),
                Op::F32Ge => self.binop(idx, Lang::F32Ge),

                Op::F64Eq => self.binop(idx, Lang::F64Eq),
                Op::F64Ne => self.binop(idx, Lang::F64Ne),
                Op::F64Lt => self.binop(idx, Lang::F64Lt),
                Op::F64Gt => self.binop(idx, Lang::F64Gt),
                Op::F64Le => self.binop(idx, Lang::F64Le),
                Op::F64Ge => self.binop(idx, Lang::F64Ge),

                Op::I32Clz => self.unop(idx, Lang::I32Clz),
                Op::I32Ctz => self.unop(idx, Lang::I32Ctz),
                Op::I64Clz => self.unop(idx, Lang::I64Clz),
                Op::I64Ctz => self.unop(idx, Lang::I64Ctz),

                Op::F32Abs => self.unop(idx, Lang::F32Abs),
                Op::F32Neg => self.unop(idx, Lang::F32Neg),
                Op::F32Ceil => self.unop(idx, Lang::F32Ceil),
                Op::F32Floor => self.unop(idx, Lang::F32Floor),
                Op::F32Trunc => self.unop(idx, Lang::F32Trunc),
                Op::F32Nearest => self.unop(idx, Lang::F32Nearest),
                Op::F32Sqrt => self.unop(idx, Lang::F32Sqrt),
                Op::F32Add => self.binop(idx, Lang::F32Add),
                Op::F32Sub => self.binop(idx, Lang::F32Sub),
                Op::F32Mul => self.binop(idx, Lang::F32Mul),
                Op::F32Div => self.binop(idx, Lang::F32Div),
                Op::F32Min => self.binop(idx, Lang::F32Min),
                Op::F32Max => self.binop(idx, Lang::F32Max),
                Op::F32Copysign => self.binop(idx, Lang::F32Copysign),

                Op::F64Abs => self.unop(idx, Lang::F64Abs),
                Op::F64Neg => self.unop(idx, Lang::F64Neg),
                Op::F64Ceil => self.unop(idx, Lang::F64Ceil),
                Op::F64Floor => self.unop(idx, Lang::F64Floor),
                Op::F64Trunc => self.unop(idx, Lang::F64Trunc),
                Op::F64Nearest => self.unop(idx, Lang::F64Nearest),
                Op::F64Sqrt => self.unop(idx, Lang::F64Sqrt),
                Op::F64Add => self.binop(idx, Lang::F64Add),
                Op::F64Sub => self.binop(idx, Lang::F64Sub),
                Op::F64Mul => self.binop(idx, Lang::F64Mul),
                Op::F64Div => self.binop(idx, Lang::F64Div),
                Op::F64Min => self.binop(idx, Lang::F64Min),
                Op::F64Max => self.binop(idx, Lang::F64Max),
                Op::F64Copysign => self.binop(idx, Lang::F64Copysign),

                Op::I32TruncF32S => self.unop(idx, Lang::I32TruncF32S),
                Op::I32TruncF32U => self.unop(idx, Lang::I32TruncF32U),
                Op::I32TruncF64S => self.unop(idx, Lang::I32TruncF64S),
                Op::I32TruncF64U => self.unop(idx, Lang::I32TruncF64U),
                Op::I64TruncF32S => self.unop(idx, Lang::I64TruncF32S),
                Op::I64TruncF32U => self.unop(idx, Lang::I64TruncF32U),
                Op::I64TruncF64S => self.unop(idx, Lang::I64TruncF64S),
                Op::I64TruncF64U => self.unop(idx, Lang::I64TruncF64U),
                Op::F32ConvertI32S => self.unop(idx, Lang::F32ConvertI32S),
                Op::F32ConvertI32U => self.unop(idx, Lang::F32ConvertI32U),
                Op::F32ConvertI64S => self.unop(idx, Lang::F32ConvertI64S),
                Op::F32ConvertI64U => self.unop(idx, Lang::F32ConvertI64U),
                Op::F64ConvertI32S => self.unop(idx, Lang::F64ConvertI32S),
                Op::F64ConvertI32U => self.unop(idx, Lang::F64ConvertI32U),
                Op::F64ConvertI64S => self.unop(idx, Lang::F64ConvertI64S),
                Op::F64ConvertI64U => self.unop(idx, Lang::F64ConvertI64U),
                Op::F64PromoteF32 => self.unop(idx, Lang::F64PromoteF32),
                Op::F32DemoteF64 => self.unop(idx, Lang::F32DemoteF64),
                Op::I32ReinterpretF32 => self.unop(idx, Lang::I32ReinterpretF32),
                Op::I64ReinterpretF64 => self.unop(idx, Lang::I64ReinterpretF64),
                Op::F32ReinterpretI32 => self.unop(idx, Lang::F32ReinterpretI32),
                Op::F64ReinterpretI64 => self.unop(idx, Lang::F64ReinterpretI64),
                Op::I32TruncSatF32S => self.unop(idx, Lang::I32TruncSatF32S),
                Op::I32TruncSatF32U => self.unop(idx, Lang::I32TruncSatF32U),
                Op::I32TruncSatF64S => self.unop(idx, Lang::I32TruncSatF64S),
                Op::I32TruncSatF64U => self.unop(idx, Lang::I32TruncSatF64U),
                Op::I64TruncSatF32S => self.unop(idx, Lang::I64TruncSatF32S),
                Op::I64TruncSatF32U => self.unop(idx, Lang::I64TruncSatF32U),
                Op::I64TruncSatF64S => self.unop(idx, Lang::I64TruncSatF64S),
                Op::I64TruncSatF64U => self.unop(idx, Lang::I64TruncSatF64U),

                Op::I32Add => self.binop(idx, Lang::I32Add),
                Op::I32Sub => self.binop(idx, Lang::I32Sub),
                Op::I32Eq => self.binop(idx, Lang::I32Eq),
                Op::I32Ne => self.binop(idx, Lang::I32Ne),
                Op::I32LtS => self.binop(idx, Lang::I32LtS),
                Op::I32LtU => self.binop(idx, Lang::I32LtU),
                Op::I32GtS => self.binop(idx, Lang::I32GtS),
                Op::I32GtU => self.binop(idx, Lang::I32GtU),
                Op::I32LeS => self.binop(idx, Lang::I32LeS),
                Op::I32LeU => self.binop(idx, Lang::I32LeU),
                Op::I32GeS => self.binop(idx, Lang::I32GeS),
                Op::I32GeU => self.binop(idx, Lang::I32GeU),
                Op::I32Mul => self.binop(idx, Lang::I32Mul),
                Op::I32DivS => self.binop(idx, Lang::I32DivS),
                Op::I32DivU => self.binop(idx, Lang::I32DivU),
                Op::I32RemS => self.binop(idx, Lang::I32RemS),
                Op::I32RemU => self.binop(idx, Lang::I32RemU),
                Op::I32Shl => self.binop(idx, Lang::I32Shl),
                Op::I32ShrS => self.binop(idx, Lang::I32ShrS),
                Op::I32ShrU => self.binop(idx, Lang::I32ShrU),
                Op::I32Xor => self.binop(idx, Lang::I32Xor),
                Op::I32Or => self.binop(idx, Lang::I32Or),
                Op::I32And => self.binop(idx, Lang::I32And),
                Op::I32Rotl => self.binop(idx, Lang::I32RotL),
                Op::I32Rotr => self.binop(idx, Lang::I32RotR),

                Op::I64Add => self.binop(idx, Lang::I64Add),
                Op::I64Sub => self.binop(idx, Lang::I64Sub),
                Op::I64Eq => self.binop(idx, Lang::I64Eq),
                Op::I64Ne => self.binop(idx, Lang::I64Ne),
                Op::I64LtS => self.binop(idx, Lang::I64LtS),
                Op::I64LtU => self.binop(idx, Lang::I64LtU),
                Op::I64GtS => self.binop(idx, Lang::I64GtS),
                Op::I64GtU => self.binop(idx, Lang::I64GtU),
                Op::I64LeS => self.binop(idx, Lang::I64LeS),
                Op::I64LeU => self.binop(idx, Lang::I64LeU),
                Op::I64GeS => self.binop(idx, Lang::I64GeS),
                Op::I64GeU => self.binop(idx, Lang::I64GeU),
                Op::I64Mul => self.binop(idx, Lang::I64Mul),
                Op::I64DivS => self.binop(idx, Lang::I64DivS),
                Op::I64DivU => self.binop(idx, Lang::I64DivU),
                Op::I64RemS => self.binop(idx, Lang::I64RemS),
                Op::I64RemU => self.binop(idx, Lang::I64RemU),
                Op::I64Shl => self.binop(idx, Lang::I64Shl),
                Op::I64ShrS => self.binop(idx, Lang::I64ShrS),
                Op::I64ShrU => self.binop(idx, Lang::I64ShrU),
                Op::I64Xor => self.binop(idx, Lang::I64Xor),
                Op::I64Or => self.binop(idx, Lang::I64Or),
                Op::I64And => self.binop(idx, Lang::I64And),
                Op::I64Rotl => self.binop(idx, Lang::I64RotL),
                Op::I64Rotr => self.binop(idx, Lang::I64RotR),

                Op::Simd(SimdOp::V128Not) => self.unop(idx, Lang::V128Not),
                Op::Simd(SimdOp::V128And) => self.binop(idx, Lang::V128And),
                Op::Simd(SimdOp::V128AndNot) => self.binop(idx, Lang::V128AndNot),
                Op::Simd(SimdOp::V128Or) => self.binop(idx, Lang::V128Or),
                Op::Simd(SimdOp::V128Xor) => self.binop(idx, Lang::V128Xor),
                Op::Simd(SimdOp::V128AnyTrue) => self.unop(idx, Lang::V128AnyTrue),
                Op::Simd(SimdOp::V128Bitselect) => self.ternop(idx, Lang::V128Bitselect),

                Op::Drop => {
                    let arg = self.pop_operand(idx, false);
                    self.empty_node(Lang::Drop([Id::from(arg)]), idx);
                }

                // conversion between integers
                Op::I32WrapI64 => self.unop(idx, Lang::Wrap),
                Op::I32Extend8S => self.unop(idx, Lang::I32Extend8S),
                Op::I32Extend16S => self.unop(idx, Lang::I32Extend16S),

                Op::I64Extend8S => self.unop(idx, Lang::I64Extend8S),
                Op::I64Extend16S => self.unop(idx, Lang::I64Extend16S),
                Op::I64Extend32S => self.unop(idx, Lang::I64Extend32S),
                Op::I64ExtendI32S => self.unop(idx, Lang::I64ExtendI32S),
                Op::I64ExtendI32U => self.unop(idx, Lang::I64ExtendI32U),

                Op::I32Popcnt => self.unop(idx, Lang::I32Popcnt),
                Op::I64Popcnt => self.unop(idx, Lang::I64Popcnt),

                Op::Select => {
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
                Op::MemoryGrow { mem } => {
                    let arg = self.pop_operand(idx, false);
                    self.push_node(Lang::MemoryGrow(*mem, Id::from(arg)), idx);
                }
                Op::MemorySize { mem } => {
                    self.push_node(Lang::MemorySize(*mem), idx);
                }
                Op::TableGrow { table } => {
                    let elem = self.pop_operand(idx, false);
                    let size = self.pop_operand(idx, false);
                    self.push_node(
                        Lang::TableGrow(*table, [Id::from(size), Id::from(elem)]),
                        idx,
                    );
                }
                Op::TableSize { table } => {
                    self.push_node(Lang::TableSize(*table), idx);
                }

                Op::DataDrop { data_index } => {
                    self.empty_node(Lang::DataDrop(*data_index), idx);
                }

                Op::ElemDrop { elem_index } => {
                    self.empty_node(Lang::ElemDrop(*elem_index), idx);
                }

                Op::MemoryInit { mem, data_index } => {
                    let a = Id::from(self.pop_operand(idx, false));
                    let b = Id::from(self.pop_operand(idx, false));
                    let c = Id::from(self.pop_operand(idx, false));
                    self.empty_node(
                        Lang::MemoryInit(
                            MemoryInit {
                                memory: *mem,
                                segment: *data_index,
                            },
                            [c, b, a],
                        ),
                        idx,
                    );
                }
                Op::MemoryCopy { src_mem, dst_mem } => {
                    let a = Id::from(self.pop_operand(idx, false));
                    let b = Id::from(self.pop_operand(idx, false));
                    let c = Id::from(self.pop_operand(idx, false));
                    self.empty_node(
                        Lang::MemoryCopy(
                            MemoryCopy {
                                src: *src_mem,
                                dst: *dst_mem,
                            },
                            [c, b, a],
                        ),
                        idx,
                    );
                }

                Op::MemoryFill { mem } => {
                    let a = Id::from(self.pop_operand(idx, false));
                    let b = Id::from(self.pop_operand(idx, false));
                    let c = Id::from(self.pop_operand(idx, false));
                    self.empty_node(Lang::MemoryFill(*mem, [c, b, a]), idx);
                }

                Op::TableInit { table, elem_index } => {
                    let a = Id::from(self.pop_operand(idx, false));
                    let b = Id::from(self.pop_operand(idx, false));
                    let c = Id::from(self.pop_operand(idx, false));
                    self.empty_node(
                        Lang::TableInit(
                            TableInit {
                                table: *table,
                                segment: *elem_index,
                            },
                            [c, b, a],
                        ),
                        idx,
                    );
                }
                Op::TableCopy {
                    src_table,
                    dst_table,
                } => {
                    let a = Id::from(self.pop_operand(idx, false));
                    let b = Id::from(self.pop_operand(idx, false));
                    let c = Id::from(self.pop_operand(idx, false));
                    self.empty_node(
                        Lang::TableCopy(
                            TableCopy {
                                src: *src_table,
                                dst: *dst_table,
                            },
                            [c, b, a],
                        ),
                        idx,
                    );
                }

                Op::TableFill { table } => {
                    let a = Id::from(self.pop_operand(idx, false));
                    let b = Id::from(self.pop_operand(idx, false));
                    let c = Id::from(self.pop_operand(idx, false));
                    self.empty_node(Lang::TableFill(*table, [c, b, a]), idx);
                }

                Op::TableGet { table } => {
                    let arg = Id::from(self.pop_operand(idx, false));
                    self.push_node(Lang::TableGet(*table, arg), idx);
                }

                Op::TableSet { table } => {
                    let arg1 = Id::from(self.pop_operand(idx, false));
                    let arg2 = Id::from(self.pop_operand(idx, false));
                    self.empty_node(Lang::TableSet(*table, [arg2, arg1]), idx);
                }

                Op::RefNull {
                    hty: wasmparser::HeapType::EXTERN,
                } => {
                    self.push_node(Lang::RefNull(RefType::Extern), idx);
                }
                Op::RefNull {
                    hty: wasmparser::HeapType::FUNC,
                } => {
                    self.push_node(Lang::RefNull(RefType::Func), idx);
                }
                Op::RefFunc { function_index } => {
                    self.push_node(Lang::RefFunc(*function_index), idx);
                }

                Op::RefIsNull => {
                    let arg = Id::from(self.pop_operand(idx, false));
                    self.push_node(Lang::RefIsNull(arg), idx);
                }

                Op::Simd(SimdOp::V128Load { memarg }) => self.load(idx, memarg, Lang::V128Load),
                Op::Simd(SimdOp::V128Load8x8S { memarg }) => self.load(idx, memarg, Lang::V128Load8x8S),
                Op::Simd(SimdOp::V128Load8x8U { memarg }) => self.load(idx, memarg, Lang::V128Load8x8U),
                Op::Simd(SimdOp::V128Load16x4S { memarg }) => self.load(idx, memarg, Lang::V128Load16x4S),
                Op::Simd(SimdOp::V128Load16x4U { memarg }) => self.load(idx, memarg, Lang::V128Load16x4U),
                Op::Simd(SimdOp::V128Load32x2S { memarg }) => self.load(idx, memarg, Lang::V128Load32x2S),
                Op::Simd(SimdOp::V128Load32x2U { memarg }) => self.load(idx, memarg, Lang::V128Load32x2U),
                Op::Simd(SimdOp::V128Load8Splat { memarg }) => self.load(idx, memarg, Lang::V128Load8Splat),
                Op::Simd(SimdOp::V128Load16Splat { memarg }) => {
                    self.load(idx, memarg, Lang::V128Load16Splat)
                }
                Op::Simd(SimdOp::V128Load32Splat { memarg }) => {
                    self.load(idx, memarg, Lang::V128Load32Splat)
                }
                Op::Simd(SimdOp::V128Load64Splat { memarg }) => {
                    self.load(idx, memarg, Lang::V128Load64Splat)
                }
                Op::Simd(SimdOp::V128Load32Zero { memarg }) => self.load(idx, memarg, Lang::V128Load32Zero),
                Op::Simd(SimdOp::V128Load64Zero { memarg }) => self.load(idx, memarg, Lang::V128Load64Zero),
                Op::Simd(SimdOp::V128Store { memarg }) => self.store(idx, memarg, Lang::V128Store),
                Op::Simd(SimdOp::V128Load8Lane { memarg, lane }) => {
                    self.load_lane(idx, memarg, lane, Lang::V128Load8Lane)
                }
                Op::Simd(SimdOp::V128Load16Lane { memarg, lane }) => {
                    self.load_lane(idx, memarg, lane, Lang::V128Load16Lane)
                }
                Op::Simd(SimdOp::V128Load32Lane { memarg, lane }) => {
                    self.load_lane(idx, memarg, lane, Lang::V128Load32Lane)
                }
                Op::Simd(SimdOp::V128Load64Lane { memarg, lane }) => {
                    self.load_lane(idx, memarg, lane, Lang::V128Load64Lane)
                }
                Op::Simd(SimdOp::V128Store8Lane { memarg, lane }) => {
                    self.store_lane(idx, memarg, lane, Lang::V128Store8Lane)
                }
                Op::Simd(SimdOp::V128Store16Lane { memarg, lane }) => {
                    self.store_lane(idx, memarg, lane, Lang::V128Store16Lane)
                }
                Op::Simd(SimdOp::V128Store32Lane { memarg, lane }) => {
                    self.store_lane(idx, memarg, lane, Lang::V128Store32Lane)
                }
                Op::Simd(SimdOp::V128Store64Lane { memarg, lane }) => {
                    self.store_lane(idx, memarg, lane, Lang::V128Store64Lane)
                }

                Op::Simd(SimdOp::I8x16ExtractLaneS { lane }) => {
                    self.extract_lane(idx, lane, Lang::I8x16ExtractLaneS)
                }
                Op::Simd(SimdOp::I8x16ExtractLaneU { lane }) => {
                    self.extract_lane(idx, lane, Lang::I8x16ExtractLaneU)
                }
                Op::Simd(SimdOp::I8x16ReplaceLane { lane }) => {
                    self.replace_lane(idx, lane, Lang::I8x16ReplaceLane)
                }
                Op::Simd(SimdOp::I16x8ExtractLaneS { lane }) => {
                    self.extract_lane(idx, lane, Lang::I16x8ExtractLaneS)
                }
                Op::Simd(SimdOp::I16x8ExtractLaneU { lane }) => {
                    self.extract_lane(idx, lane, Lang::I16x8ExtractLaneU)
                }
                Op::Simd(SimdOp::I16x8ReplaceLane { lane }) => {
                    self.replace_lane(idx, lane, Lang::I16x8ReplaceLane)
                }
                Op::Simd(SimdOp::I32x4ExtractLane { lane }) => {
                    self.extract_lane(idx, lane, Lang::I32x4ExtractLane)
                }
                Op::Simd(SimdOp::I32x4ReplaceLane { lane }) => {
                    self.replace_lane(idx, lane, Lang::I32x4ReplaceLane)
                }
                Op::Simd(SimdOp::I64x2ExtractLane { lane }) => {
                    self.extract_lane(idx, lane, Lang::I64x2ExtractLane)
                }
                Op::Simd(SimdOp::I64x2ReplaceLane { lane }) => {
                    self.replace_lane(idx, lane, Lang::I64x2ReplaceLane)
                }
                Op::Simd(SimdOp::F32x4ExtractLane { lane }) => {
                    self.extract_lane(idx, lane, Lang::F32x4ExtractLane)
                }
                Op::Simd(SimdOp::F32x4ReplaceLane { lane }) => {
                    self.replace_lane(idx, lane, Lang::F32x4ReplaceLane)
                }
                Op::Simd(SimdOp::F64x2ExtractLane { lane }) => {
                    self.extract_lane(idx, lane, Lang::F64x2ExtractLane)
                }
                Op::Simd(SimdOp::F64x2ReplaceLane { lane }) => {
                    self.replace_lane(idx, lane, Lang::F64x2ReplaceLane)
                }

                Op::Simd(SimdOp::I8x16Swizzle) => self.binop(idx, Lang::I8x16Swizzle),
                Op::Simd(SimdOp::I8x16Shuffle { lanes }) => {
                    let a = Id::from(self.pop_operand(idx, false));
                    let b = Id::from(self.pop_operand(idx, false));
                    self.push_node(Lang::I8x16Shuffle(Shuffle { indices: *lanes }, [b, a]), idx);
                }
                Op::Simd(SimdOp::I8x16Splat) => self.unop(idx, Lang::I8x16Splat),
                Op::Simd(SimdOp::I16x8Splat) => self.unop(idx, Lang::I16x8Splat),
                Op::Simd(SimdOp::I32x4Splat) => self.unop(idx, Lang::I32x4Splat),
                Op::Simd(SimdOp::I64x2Splat) => self.unop(idx, Lang::I64x2Splat),
                Op::Simd(SimdOp::F32x4Splat) => self.unop(idx, Lang::F32x4Splat),
                Op::Simd(SimdOp::F64x2Splat) => self.unop(idx, Lang::F64x2Splat),

                Op::Simd(SimdOp::I8x16Eq) => self.binop(idx, Lang::I8x16Eq),
                Op::Simd(SimdOp::I8x16Ne) => self.binop(idx, Lang::I8x16Ne),
                Op::Simd(SimdOp::I8x16LtS) => self.binop(idx, Lang::I8x16LtS),
                Op::Simd(SimdOp::I8x16LtU) => self.binop(idx, Lang::I8x16LtU),
                Op::Simd(SimdOp::I8x16GtS) => self.binop(idx, Lang::I8x16GtS),
                Op::Simd(SimdOp::I8x16GtU) => self.binop(idx, Lang::I8x16GtU),
                Op::Simd(SimdOp::I8x16LeS) => self.binop(idx, Lang::I8x16LeS),
                Op::Simd(SimdOp::I8x16LeU) => self.binop(idx, Lang::I8x16LeU),
                Op::Simd(SimdOp::I8x16GeS) => self.binop(idx, Lang::I8x16GeS),
                Op::Simd(SimdOp::I8x16GeU) => self.binop(idx, Lang::I8x16GeU),
                Op::Simd(SimdOp::I16x8Eq) => self.binop(idx, Lang::I16x8Eq),
                Op::Simd(SimdOp::I16x8Ne) => self.binop(idx, Lang::I16x8Ne),
                Op::Simd(SimdOp::I16x8LtS) => self.binop(idx, Lang::I16x8LtS),
                Op::Simd(SimdOp::I16x8LtU) => self.binop(idx, Lang::I16x8LtU),
                Op::Simd(SimdOp::I16x8GtS) => self.binop(idx, Lang::I16x8GtS),
                Op::Simd(SimdOp::I16x8GtU) => self.binop(idx, Lang::I16x8GtU),
                Op::Simd(SimdOp::I16x8LeS) => self.binop(idx, Lang::I16x8LeS),
                Op::Simd(SimdOp::I16x8LeU) => self.binop(idx, Lang::I16x8LeU),
                Op::Simd(SimdOp::I16x8GeS) => self.binop(idx, Lang::I16x8GeS),
                Op::Simd(SimdOp::I16x8GeU) => self.binop(idx, Lang::I16x8GeU),
                Op::Simd(SimdOp::I32x4Eq) => self.binop(idx, Lang::I32x4Eq),
                Op::Simd(SimdOp::I32x4Ne) => self.binop(idx, Lang::I32x4Ne),
                Op::Simd(SimdOp::I32x4LtS) => self.binop(idx, Lang::I32x4LtS),
                Op::Simd(SimdOp::I32x4LtU) => self.binop(idx, Lang::I32x4LtU),
                Op::Simd(SimdOp::I32x4GtS) => self.binop(idx, Lang::I32x4GtS),
                Op::Simd(SimdOp::I32x4GtU) => self.binop(idx, Lang::I32x4GtU),
                Op::Simd(SimdOp::I32x4LeS) => self.binop(idx, Lang::I32x4LeS),
                Op::Simd(SimdOp::I32x4LeU) => self.binop(idx, Lang::I32x4LeU),
                Op::Simd(SimdOp::I32x4GeS) => self.binop(idx, Lang::I32x4GeS),
                Op::Simd(SimdOp::I32x4GeU) => self.binop(idx, Lang::I32x4GeU),
                Op::Simd(SimdOp::I64x2Eq) => self.binop(idx, Lang::I64x2Eq),
                Op::Simd(SimdOp::I64x2Ne) => self.binop(idx, Lang::I64x2Ne),
                Op::Simd(SimdOp::I64x2LtS) => self.binop(idx, Lang::I64x2LtS),
                Op::Simd(SimdOp::I64x2GtS) => self.binop(idx, Lang::I64x2GtS),
                Op::Simd(SimdOp::I64x2LeS) => self.binop(idx, Lang::I64x2LeS),
                Op::Simd(SimdOp::I64x2GeS) => self.binop(idx, Lang::I64x2GeS),
                Op::Simd(SimdOp::F32x4Eq) => self.binop(idx, Lang::F32x4Eq),
                Op::Simd(SimdOp::F32x4Ne) => self.binop(idx, Lang::F32x4Ne),
                Op::Simd(SimdOp::F32x4Lt) => self.binop(idx, Lang::F32x4Lt),
                Op::Simd(SimdOp::F32x4Gt) => self.binop(idx, Lang::F32x4Gt),
                Op::Simd(SimdOp::F32x4Le) => self.binop(idx, Lang::F32x4Le),
                Op::Simd(SimdOp::F32x4Ge) => self.binop(idx, Lang::F32x4Ge),
                Op::Simd(SimdOp::F64x2Eq) => self.binop(idx, Lang::F64x2Eq),
                Op::Simd(SimdOp::F64x2Ne) => self.binop(idx, Lang::F64x2Ne),
                Op::Simd(SimdOp::F64x2Lt) => self.binop(idx, Lang::F64x2Lt),
                Op::Simd(SimdOp::F64x2Gt) => self.binop(idx, Lang::F64x2Gt),
                Op::Simd(SimdOp::F64x2Le) => self.binop(idx, Lang::F64x2Le),
                Op::Simd(SimdOp::F64x2Ge) => self.binop(idx, Lang::F64x2Ge),

                Op::Simd(SimdOp::I8x16Abs) => self.unop(idx, Lang::I8x16Abs),
                Op::Simd(SimdOp::I8x16Neg) => self.unop(idx, Lang::I8x16Neg),
                Op::Simd(SimdOp::I8x16Popcnt) => self.unop(idx, Lang::I8x16Popcnt),
                Op::Simd(SimdOp::I8x16AllTrue) => self.unop(idx, Lang::I8x16AllTrue),
                Op::Simd(SimdOp::I8x16Bitmask) => self.unop(idx, Lang::I8x16Bitmask),
                Op::Simd(SimdOp::I8x16NarrowI16x8S) => self.binop(idx, Lang::I8x16NarrowI16x8S),
                Op::Simd(SimdOp::I8x16NarrowI16x8U) => self.binop(idx, Lang::I8x16NarrowI16x8U),
                Op::Simd(SimdOp::I8x16Shl) => self.binop(idx, Lang::I8x16Shl),
                Op::Simd(SimdOp::I8x16ShrS) => self.binop(idx, Lang::I8x16ShrS),
                Op::Simd(SimdOp::I8x16ShrU) => self.binop(idx, Lang::I8x16ShrU),
                Op::Simd(SimdOp::I8x16Add) => self.binop(idx, Lang::I8x16Add),
                Op::Simd(SimdOp::I8x16AddSatS) => self.binop(idx, Lang::I8x16AddSatS),
                Op::Simd(SimdOp::I8x16AddSatU) => self.binop(idx, Lang::I8x16AddSatU),
                Op::Simd(SimdOp::I8x16Sub) => self.binop(idx, Lang::I8x16Sub),
                Op::Simd(SimdOp::I8x16SubSatS) => self.binop(idx, Lang::I8x16SubSatS),
                Op::Simd(SimdOp::I8x16SubSatU) => self.binop(idx, Lang::I8x16SubSatU),
                Op::Simd(SimdOp::I8x16MinS) => self.binop(idx, Lang::I8x16MinS),
                Op::Simd(SimdOp::I8x16MinU) => self.binop(idx, Lang::I8x16MinU),
                Op::Simd(SimdOp::I8x16MaxS) => self.binop(idx, Lang::I8x16MaxS),
                Op::Simd(SimdOp::I8x16MaxU) => self.binop(idx, Lang::I8x16MaxU),
                Op::Simd(SimdOp::I8x16AvgrU) => self.binop(idx, Lang::I8x16AvgrU),

                Op::Simd(SimdOp::I16x8ExtAddPairwiseI8x16S) => {
                    self.unop(idx, Lang::I16x8ExtAddPairwiseI8x16S)
                }
                Op::Simd(SimdOp::I16x8ExtAddPairwiseI8x16U) => {
                    self.unop(idx, Lang::I16x8ExtAddPairwiseI8x16U)
                }
                Op::Simd(SimdOp::I16x8Abs) => self.unop(idx, Lang::I16x8Abs),
                Op::Simd(SimdOp::I16x8Neg) => self.unop(idx, Lang::I16x8Neg),
                Op::Simd(SimdOp::I16x8Q15MulrSatS) => self.binop(idx, Lang::I16x8Q15MulrSatS),
                Op::Simd(SimdOp::I16x8AllTrue) => self.unop(idx, Lang::I16x8AllTrue),
                Op::Simd(SimdOp::I16x8Bitmask) => self.unop(idx, Lang::I16x8Bitmask),
                Op::Simd(SimdOp::I16x8NarrowI32x4S) => self.binop(idx, Lang::I16x8NarrowI32x4S),
                Op::Simd(SimdOp::I16x8NarrowI32x4U) => self.binop(idx, Lang::I16x8NarrowI32x4U),
                Op::Simd(SimdOp::I16x8ExtendLowI8x16S) => self.unop(idx, Lang::I16x8ExtendLowI8x16S),
                Op::Simd(SimdOp::I16x8ExtendHighI8x16S) => self.unop(idx, Lang::I16x8ExtendHighI8x16S),
                Op::Simd(SimdOp::I16x8ExtendLowI8x16U) => self.unop(idx, Lang::I16x8ExtendLowI8x16U),
                Op::Simd(SimdOp::I16x8ExtendHighI8x16U) => self.unop(idx, Lang::I16x8ExtendHighI8x16U),
                Op::Simd(SimdOp::I16x8Shl) => self.binop(idx, Lang::I16x8Shl),
                Op::Simd(SimdOp::I16x8ShrS) => self.binop(idx, Lang::I16x8ShrS),
                Op::Simd(SimdOp::I16x8ShrU) => self.binop(idx, Lang::I16x8ShrU),
                Op::Simd(SimdOp::I16x8Add) => self.binop(idx, Lang::I16x8Add),
                Op::Simd(SimdOp::I16x8AddSatS) => self.binop(idx, Lang::I16x8AddSatS),
                Op::Simd(SimdOp::I16x8AddSatU) => self.binop(idx, Lang::I16x8AddSatU),
                Op::Simd(SimdOp::I16x8Sub) => self.binop(idx, Lang::I16x8Sub),
                Op::Simd(SimdOp::I16x8SubSatS) => self.binop(idx, Lang::I16x8SubSatS),
                Op::Simd(SimdOp::I16x8SubSatU) => self.binop(idx, Lang::I16x8SubSatU),
                Op::Simd(SimdOp::I16x8Mul) => self.binop(idx, Lang::I16x8Mul),
                Op::Simd(SimdOp::I16x8MinS) => self.binop(idx, Lang::I16x8MinS),
                Op::Simd(SimdOp::I16x8MinU) => self.binop(idx, Lang::I16x8MinU),
                Op::Simd(SimdOp::I16x8MaxS) => self.binop(idx, Lang::I16x8MaxS),
                Op::Simd(SimdOp::I16x8MaxU) => self.binop(idx, Lang::I16x8MaxU),
                Op::Simd(SimdOp::I16x8AvgrU) => self.binop(idx, Lang::I16x8AvgrU),
                Op::Simd(SimdOp::I16x8ExtMulLowI8x16S) => self.binop(idx, Lang::I16x8ExtMulLowI8x16S),
                Op::Simd(SimdOp::I16x8ExtMulHighI8x16S) => self.binop(idx, Lang::I16x8ExtMulHighI8x16S),
                Op::Simd(SimdOp::I16x8ExtMulLowI8x16U) => self.binop(idx, Lang::I16x8ExtMulLowI8x16U),
                Op::Simd(SimdOp::I16x8ExtMulHighI8x16U) => self.binop(idx, Lang::I16x8ExtMulHighI8x16U),

                Op::Simd(SimdOp::I32x4ExtAddPairwiseI16x8S) => {
                    self.unop(idx, Lang::I32x4ExtAddPairwiseI16x8S)
                }
                Op::Simd(SimdOp::I32x4ExtAddPairwiseI16x8U) => {
                    self.unop(idx, Lang::I32x4ExtAddPairwiseI16x8U)
                }
                Op::Simd(SimdOp::I32x4Abs) => self.unop(idx, Lang::I32x4Abs),
                Op::Simd(SimdOp::I32x4Neg) => self.unop(idx, Lang::I32x4Neg),
                Op::Simd(SimdOp::I32x4AllTrue) => self.unop(idx, Lang::I32x4AllTrue),
                Op::Simd(SimdOp::I32x4Bitmask) => self.unop(idx, Lang::I32x4Bitmask),
                Op::Simd(SimdOp::I32x4ExtendLowI16x8S) => self.unop(idx, Lang::I32x4ExtendLowI16x8S),
                Op::Simd(SimdOp::I32x4ExtendHighI16x8S) => self.unop(idx, Lang::I32x4ExtendHighI16x8S),
                Op::Simd(SimdOp::I32x4ExtendLowI16x8U) => self.unop(idx, Lang::I32x4ExtendLowI16x8U),
                Op::Simd(SimdOp::I32x4ExtendHighI16x8U) => self.unop(idx, Lang::I32x4ExtendHighI16x8U),
                Op::Simd(SimdOp::I32x4Shl) => self.binop(idx, Lang::I32x4Shl),
                Op::Simd(SimdOp::I32x4ShrS) => self.binop(idx, Lang::I32x4ShrS),
                Op::Simd(SimdOp::I32x4ShrU) => self.binop(idx, Lang::I32x4ShrU),
                Op::Simd(SimdOp::I32x4Add) => self.binop(idx, Lang::I32x4Add),
                Op::Simd(SimdOp::I32x4Sub) => self.binop(idx, Lang::I32x4Sub),
                Op::Simd(SimdOp::I32x4Mul) => self.binop(idx, Lang::I32x4Mul),
                Op::Simd(SimdOp::I32x4MinS) => self.binop(idx, Lang::I32x4MinS),
                Op::Simd(SimdOp::I32x4MinU) => self.binop(idx, Lang::I32x4MinU),
                Op::Simd(SimdOp::I32x4MaxS) => self.binop(idx, Lang::I32x4MaxS),
                Op::Simd(SimdOp::I32x4MaxU) => self.binop(idx, Lang::I32x4MaxU),
                Op::Simd(SimdOp::I32x4DotI16x8S) => self.binop(idx, Lang::I32x4DotI16x8S),
                Op::Simd(SimdOp::I32x4ExtMulLowI16x8S) => self.binop(idx, Lang::I32x4ExtMulLowI16x8S),
                Op::Simd(SimdOp::I32x4ExtMulHighI16x8S) => self.binop(idx, Lang::I32x4ExtMulHighI16x8S),
                Op::Simd(SimdOp::I32x4ExtMulLowI16x8U) => self.binop(idx, Lang::I32x4ExtMulLowI16x8U),
                Op::Simd(SimdOp::I32x4ExtMulHighI16x8U) => self.binop(idx, Lang::I32x4ExtMulHighI16x8U),

                Op::Simd(SimdOp::I64x2Abs) => self.unop(idx, Lang::I64x2Abs),
                Op::Simd(SimdOp::I64x2Neg) => self.unop(idx, Lang::I64x2Neg),
                Op::Simd(SimdOp::I64x2AllTrue) => self.unop(idx, Lang::I64x2AllTrue),
                Op::Simd(SimdOp::I64x2Bitmask) => self.unop(idx, Lang::I64x2Bitmask),
                Op::Simd(SimdOp::I64x2ExtendLowI32x4S) => self.unop(idx, Lang::I64x2ExtendLowI32x4S),
                Op::Simd(SimdOp::I64x2ExtendHighI32x4S) => self.unop(idx, Lang::I64x2ExtendHighI32x4S),
                Op::Simd(SimdOp::I64x2ExtendLowI32x4U) => self.unop(idx, Lang::I64x2ExtendLowI32x4U),
                Op::Simd(SimdOp::I64x2ExtendHighI32x4U) => self.unop(idx, Lang::I64x2ExtendHighI32x4U),
                Op::Simd(SimdOp::I64x2Shl) => self.binop(idx, Lang::I64x2Shl),
                Op::Simd(SimdOp::I64x2ShrS) => self.binop(idx, Lang::I64x2ShrS),
                Op::Simd(SimdOp::I64x2ShrU) => self.binop(idx, Lang::I64x2ShrU),
                Op::Simd(SimdOp::I64x2Add) => self.binop(idx, Lang::I64x2Add),
                Op::Simd(SimdOp::I64x2Sub) => self.binop(idx, Lang::I64x2Sub),
                Op::Simd(SimdOp::I64x2Mul) => self.binop(idx, Lang::I64x2Mul),
                Op::Simd(SimdOp::I64x2ExtMulLowI32x4S) => self.binop(idx, Lang::I64x2ExtMulLowI32x4S),
                Op::Simd(SimdOp::I64x2ExtMulHighI32x4S) => self.binop(idx, Lang::I64x2ExtMulHighI32x4S),
                Op::Simd(SimdOp::I64x2ExtMulLowI32x4U) => self.binop(idx, Lang::I64x2ExtMulLowI32x4U),
                Op::Simd(SimdOp::I64x2ExtMulHighI32x4U) => self.binop(idx, Lang::I64x2ExtMulHighI32x4U),

                Op::Simd(SimdOp::F32x4Ceil) => self.unop(idx, Lang::F32x4Ceil),
                Op::Simd(SimdOp::F32x4Floor) => self.unop(idx, Lang::F32x4Floor),
                Op::Simd(SimdOp::F32x4Trunc) => self.unop(idx, Lang::F32x4Trunc),
                Op::Simd(SimdOp::F32x4Nearest) => self.unop(idx, Lang::F32x4Nearest),
                Op::Simd(SimdOp::F32x4Abs) => self.unop(idx, Lang::F32x4Abs),
                Op::Simd(SimdOp::F32x4Neg) => self.unop(idx, Lang::F32x4Neg),
                Op::Simd(SimdOp::F32x4Sqrt) => self.unop(idx, Lang::F32x4Sqrt),
                Op::Simd(SimdOp::F32x4Add) => self.binop(idx, Lang::F32x4Add),
                Op::Simd(SimdOp::F32x4Sub) => self.binop(idx, Lang::F32x4Sub),
                Op::Simd(SimdOp::F32x4Mul) => self.binop(idx, Lang::F32x4Mul),
                Op::Simd(SimdOp::F32x4Div) => self.binop(idx, Lang::F32x4Div),
                Op::Simd(SimdOp::F32x4Min) => self.binop(idx, Lang::F32x4Min),
                Op::Simd(SimdOp::F32x4Max) => self.binop(idx, Lang::F32x4Max),
                Op::Simd(SimdOp::F32x4PMin) => self.binop(idx, Lang::F32x4PMin),
                Op::Simd(SimdOp::F32x4PMax) => self.binop(idx, Lang::F32x4PMax),
                Op::Simd(SimdOp::F64x2Ceil) => self.unop(idx, Lang::F64x2Ceil),
                Op::Simd(SimdOp::F64x2Floor) => self.unop(idx, Lang::F64x2Floor),
                Op::Simd(SimdOp::F64x2Trunc) => self.unop(idx, Lang::F64x2Trunc),
                Op::Simd(SimdOp::F64x2Nearest) => self.unop(idx, Lang::F64x2Nearest),
                Op::Simd(SimdOp::F64x2Abs) => self.unop(idx, Lang::F64x2Abs),
                Op::Simd(SimdOp::F64x2Neg) => self.unop(idx, Lang::F64x2Neg),
                Op::Simd(SimdOp::F64x2Sqrt) => self.unop(idx, Lang::F64x2Sqrt),
                Op::Simd(SimdOp::F64x2Add) => self.binop(idx, Lang::F64x2Add),
                Op::Simd(SimdOp::F64x2Sub) => self.binop(idx, Lang::F64x2Sub),
                Op::Simd(SimdOp::F64x2Mul) => self.binop(idx, Lang::F64x2Mul),
                Op::Simd(SimdOp::F64x2Div) => self.binop(idx, Lang::F64x2Div),
                Op::Simd(SimdOp::F64x2Min) => self.binop(idx, Lang::F64x2Min),
                Op::Simd(SimdOp::F64x2Max) => self.binop(idx, Lang::F64x2Max),
                Op::Simd(SimdOp::F64x2PMin) => self.binop(idx, Lang::F64x2PMin),
                Op::Simd(SimdOp::F64x2PMax) => self.binop(idx, Lang::F64x2PMax),

                Op::Simd(SimdOp::I32x4TruncSatF32x4S) => self.unop(idx, Lang::I32x4TruncSatF32x4S),
                Op::Simd(SimdOp::I32x4TruncSatF32x4U) => self.unop(idx, Lang::I32x4TruncSatF32x4U),
                Op::Simd(SimdOp::F32x4ConvertI32x4S) => self.unop(idx, Lang::F32x4ConvertI32x4S),
                Op::Simd(SimdOp::F32x4ConvertI32x4U) => self.unop(idx, Lang::F32x4ConvertI32x4U),
                Op::Simd(SimdOp::I32x4TruncSatF64x2SZero) => self.unop(idx, Lang::I32x4TruncSatF64x2SZero),
                Op::Simd(SimdOp::I32x4TruncSatF64x2UZero) => self.unop(idx, Lang::I32x4TruncSatF64x2UZero),
                Op::Simd(SimdOp::F64x2ConvertLowI32x4S) => self.unop(idx, Lang::F64x2ConvertLowI32x4S),
                Op::Simd(SimdOp::F64x2ConvertLowI32x4U) => self.unop(idx, Lang::F64x2ConvertLowI32x4U),
                Op::Simd(SimdOp::F32x4DemoteF64x2Zero) => self.unop(idx, Lang::F32x4DemoteF64x2Zero),
                Op::Simd(SimdOp::F64x2PromoteLowF32x4) => self.unop(idx, Lang::F64x2PromoteLowF32x4),

                op => {
                    // If the operator is not implemented, warn and bail out. We
                    // can't use `undef` for these because if we try to rewrite
                    // them with some rule like `x => i32.rand` then we fail to
                    // actually encode the rewritten code to valid Wasm right
                    // now. We would need to be able to walk the new expression
                    // and determine which `undef` values became dead code and
                    // we need to insert `drop`s for before inserting the new
                    // code, and we don't currently have that infrastructure.
                    log::warn!("Wasm operator not implemented: {:?}", op);
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

    fn load(&mut self, idx: usize, memarg: &wasmparser::MemArg, op: fn(MemArg, Id) -> Lang) {
        self.unop_cb(idx, |id| op(memarg.into(), id))
    }

    fn load_lane(
        &mut self,
        idx: usize,
        memarg: &wasmparser::MemArg,
        lane: &u8,
        op: fn(MemArgLane, [Id; 2]) -> Lang,
    ) {
        let leftidx = self.pop_operand(idx, false);
        let rightidx = self.pop_operand(idx, false);
        // The operands should not be the same
        assert_ne!(leftidx, rightidx);

        self.push_node(
            op(
                MemArgLane {
                    lane: *lane,
                    memarg: memarg.into(),
                },
                [Id::from(rightidx), Id::from(leftidx)],
            ),
            idx,
        );
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

    fn ternop(&mut self, idx: usize, op: fn([Id; 3]) -> Lang) {
        let c = self.pop_operand(idx, false);
        let b = self.pop_operand(idx, false);
        let a = self.pop_operand(idx, false);

        // The operands should not be the same.
        assert_ne!(a, b);
        assert_ne!(a, c);
        assert_ne!(b, c);

        self.push_node(op([Id::from(a), Id::from(b), Id::from(c)]), idx);
    }

    fn store(&mut self, idx: usize, memarg: &wasmparser::MemArg, op: fn(MemArg, [Id; 2]) -> Lang) {
        let leftidx = self.pop_operand(idx, false);
        let rightidx = self.pop_operand(idx, false);

        // The operands should not be the same
        assert_ne!(leftidx, rightidx);

        self.empty_node(
            op(memarg.into(), [Id::from(rightidx), Id::from(leftidx)]),
            idx,
        );
    }

    fn store_lane(
        &mut self,
        idx: usize,
        memarg: &wasmparser::MemArg,
        lane: &u8,
        op: fn(MemArgLane, [Id; 2]) -> Lang,
    ) {
        let leftidx = self.pop_operand(idx, false);
        let rightidx = self.pop_operand(idx, false);

        // The operands should not be the same
        assert_ne!(leftidx, rightidx);

        self.empty_node(
            op(
                MemArgLane {
                    lane: *lane,
                    memarg: memarg.into(),
                },
                [Id::from(rightidx), Id::from(leftidx)],
            ),
            idx,
        );
    }

    fn extract_lane(&mut self, idx: usize, lane: &u8, op: fn(u8, Id) -> Lang) {
        let arg = self.pop_operand(idx, false);
        self.push_node(op(*lane, Id::from(arg)), idx);
    }

    fn replace_lane(&mut self, idx: usize, lane: &u8, op: fn(u8, [Id; 2]) -> Lang) {
        let leftidx = self.pop_operand(idx, false);
        let rightidx = self.pop_operand(idx, false);
        self.push_node(op(*lane, [Id::from(rightidx), Id::from(leftidx)]), idx);
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

impl From<&wasmparser::MemArg> for MemArg {
    fn from(mem: &wasmparser::MemArg) -> MemArg {
        MemArg {
            align: mem.align,
            mem: mem.memory,
            static_offset: mem.offset,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DFGBuilder;
    use crate::mutators::OperatorAndByteOffset;
    use crate::{ModuleInfo, WasmMutate};
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

        let config = WasmMutate::default();
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

                    let roots = DFGBuilder::new(&config).get_bb_from_operator(5, &operators);

                    assert!(roots.is_some())
                }
                wasmparser::Payload::End(_) => {
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

        let config = WasmMutate::default();
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

                    let bb = DFGBuilder::new(&config)
                        .get_bb_from_operator(0, &operators)
                        .unwrap();
                    let roots =
                        DFGBuilder::new(&config).get_dfg(&ModuleInfo::default(), &operators, &bb);
                    assert!(roots.is_some())
                }
                wasmparser::Payload::End(_) => {
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

        let config = WasmMutate::default();
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

                    let bb = DFGBuilder::new(&config)
                        .get_bb_from_operator(7, &operators)
                        .unwrap();
                    let roots =
                        DFGBuilder::new(&config).get_dfg(&ModuleInfo::default(), &operators, &bb);
                    assert!(roots.is_some());
                }
                wasmparser::Payload::End(_) => {
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

        let config = WasmMutate::default();
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

                    let bb = DFGBuilder::new(&config)
                        .get_bb_from_operator(3, &operators)
                        .unwrap();
                    let roots = DFGBuilder::new(&config).get_dfg(&info, &operators, &bb);
                    assert!(roots.is_some());
                }
                wasmparser::Payload::End(_) => {
                    break;
                }
                _ => {
                    // Do nothing
                }
            }
        }
    }
}
