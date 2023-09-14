use super::{Printer, State};
use anyhow::{bail, Result};
use std::fmt::Write;
use wasmparser::{BlockType, BrTable, MemArg, VisitOperator};

pub struct PrintOperator<'a, 'b> {
    pub(super) printer: &'a mut Printer,
    nesting_start: u32,
    state: &'b mut State,
}

impl<'a, 'b> PrintOperator<'a, 'b> {
    pub(super) fn new(printer: &'a mut Printer, state: &'b mut State) -> PrintOperator<'a, 'b> {
        PrintOperator {
            nesting_start: printer.nesting,
            printer,
            state,
        }
    }

    fn push_str(&mut self, s: &str) {
        self.printer.result.push_str(s);
    }

    fn result(&mut self) -> &mut String {
        &mut self.printer.result
    }

    fn blockty(&mut self, ty: BlockType) -> Result<()> {
        if let Some(name) = self
            .state
            .core
            .label_names
            .get(&(self.state.core.funcs, self.state.core.labels))
        {
            name.write(&mut self.printer.result);
            self.printer.result.push(' ');
        }
        match ty {
            BlockType::Empty => {}
            BlockType::Type(t) => {
                self.push_str("(result ");
                self.printer.print_valtype(t)?;
                self.push_str(") ");
            }
            BlockType::FuncType(idx) => {
                self.printer
                    .print_core_functype_idx(self.state, idx, None)?;
                self.printer.result.push(' ');
            }
        }
        // Note that 1 is added to the current depth here since if a block type
        // is being printed then a block is being created which will increase
        // the label depth of the block itself.
        let depth = self.cur_depth();
        write!(self.result(), ";; label = @{}", depth + 1)?;
        self.state.core.labels += 1;
        Ok(())
    }

    fn cur_depth(&self) -> u32 {
        self.printer.nesting - self.nesting_start
    }

    fn label(&self, relative: u32) -> String {
        match self.cur_depth().checked_sub(relative) {
            Some(i) => format!("@{}", i),
            None => " INVALID ".to_string(),
        }
    }

    fn tag_index(&mut self, index: u32) -> Result<()> {
        write!(self.result(), "{index}")?;
        Ok(())
    }

    fn relative_depth(&mut self, depth: u32) -> Result<()> {
        let label = self.label(depth);
        write!(self.result(), "{depth} (;{label};)")?;
        Ok(())
    }

    fn targets(&mut self, targets: BrTable<'_>) -> Result<()> {
        for (i, item) in targets.targets().chain([Ok(targets.default())]).enumerate() {
            if i > 0 {
                self.push_str(" ");
            }
            self.relative_depth(item?)?;
        }
        Ok(())
    }

    fn function_index(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.func_names, idx)
    }

    fn local_index(&mut self, idx: u32) -> Result<()> {
        self.printer
            .print_local_idx(self.state, self.state.core.funcs, idx)
    }

    fn global_index(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.global_names, idx)
    }

    fn table_index(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.table_names, idx)
    }

    fn table(&mut self, idx: u32) -> Result<()> {
        self.table_index(idx)
    }

    fn memory_index(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.memory_names, idx)
    }

    fn type_index(&mut self, idx: u32) -> Result<()> {
        self.push_str(" ");
        self.printer.print_core_type_ref(self.state, idx)
    }

    fn data_index(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.data_names, idx)
    }

    fn elem_index(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.element_names, idx)
    }

    fn lane(&mut self, lane: u8) -> Result<()> {
        write!(self.result(), "{lane}")?;
        Ok(())
    }

    fn lanes(&mut self, lanes: [u8; 16]) -> Result<()> {
        for (i, lane) in lanes.iter().enumerate() {
            if i > 0 {
                self.push_str(" ");
            }
            write!(self.result(), "{lane}")?;
        }
        Ok(())
    }

    fn memarg(&mut self, memarg: MemArg) -> Result<()> {
        // Remove the leading ' ' inserted by the macro below since memarg may
        // not actually print anything if all of its parameters are defaulted.
        // Ideally we wouldn't rely on the ability to pop here but this ends up
        // being the easiest.
        assert_eq!(self.printer.result.pop(), Some(' '));

        if memarg.memory != 0 {
            self.result().push(' ');
            self.memory_index(memarg.memory)?;
        }
        if memarg.offset != 0 {
            write!(self.result(), " offset={}", memarg.offset)?;
        }
        if memarg.align != memarg.max_align {
            if memarg.align >= 32 {
                bail!("alignment in memarg too large");
            }
            let align = 1 << memarg.align;
            write!(self.result(), " align={}", align)?;
        }
        Ok(())
    }
}

pub enum OpKind {
    BlockStart,
    BlockMid,
    End,
    Delegate,
    Normal,
}

macro_rules! define_visit {
    // General structure of all the operator printer methods:
    //
    // * Print the name of the insruction as defined in this macro
    // * Print any payload, as necessary
    // * Return the `OpKind`, as defined by this macro
    ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident )*) => ($(
        fn $visit(&mut self $( , $($arg: $argty),* )?) -> Self::Output {
            self.push_str(define_visit!(name $op));
            $(
                define_visit!(payload self $op $($arg)*);
            )?
            Ok(define_visit!(kind $op))
        }
    )*);

    // Macro case to classify instructions based on their `$op` naming into an
    // `OpKind`. There are a few special cases here but the vast majority of
    // operators fall into the `Normal` category.
    (kind Block) => (OpKind::BlockStart);
    (kind Loop) => (OpKind::BlockStart);
    (kind If) => (OpKind::BlockStart);
    (kind Try) => (OpKind::BlockStart);
    (kind Else) => (OpKind::BlockMid);
    (kind Catch) => (OpKind::BlockMid);
    (kind CatchAll) => (OpKind::BlockMid);
    (kind End) => (OpKind::End);
    (kind Delegate) => (OpKind::Delegate);
    (kind $other:tt) => (OpKind::Normal);

    // How to print the payload of an instruction. There are a number of
    // instructions that have special cases such as avoiding printing anything
    // when an index is 0 or similar. The final case in this list is the
    // catch-all which prints each payload individually based on the name of the
    // payload field.
    (payload $self:ident CallIndirect $ty:ident $table:ident $byte:ident) => (
        if $table != 0 {
            $self.push_str(" ");
            $self.table_index($table)?;
        }
        $self.type_index($ty)?;
        let _ = $byte;
    );
    (payload $self:ident ReturnCallIndirect $ty:ident $table:ident) => (
        if $table != 0 {
            $self.push_str(" ");
            $self.table_index($table)?;
        }
        $self.type_index($ty)?;
    );
    (payload $self:ident CallRef $ty:ident) => (
        $self.push_str(" ");
        $self.printer.print_idx(&$self.state.core.type_names, $ty)?;
    );
    (payload $self:ident ReturnCallRef $ty:ident) => (
        $self.push_str(" ");
        $self.printer.print_idx(&$self.state.core.type_names, $ty)?;
    );
    (payload $self:ident TypedSelect $ty:ident) => (
        $self.push_str(" (result ");
        $self.printer.print_valtype($ty)?;
        $self.push_str(")")
    );
    (payload $self:ident RefNull $hty:ident) => (
        $self.push_str(" ");
        $self.printer.print_heaptype($hty)?;
    );
    (payload $self:ident TableInit $segment:ident $table:ident) => (
        $self.push_str(" ");
        if $table != 0 {
            $self.table_index($table)?;
            $self.push_str(" ");
        }
        $self.elem_index($segment)?;
    );
    (payload $self:ident TableCopy $dst:ident $src:ident) => (
        if $src != 0 || $dst != 0 {
            $self.push_str(" ");
            $self.table_index($dst)?;
            $self.push_str(" ");
            $self.table_index($src)?;
        }
    );
    (payload $self:ident $mem_op:ident $mem:ident mem_byte) => (
        if $mem != 0 {
            $self.push_str(" ");
            $self.memory_index($mem)?;
        }
    );
    (payload $self:ident MemoryInit $segment:ident $mem:ident) => (
        if $mem != 0 {
            $self.push_str(" ");
            $self.memory_index($mem)?;
        }
        $self.push_str(" ");
        $self.data_index($segment)?;
    );
    (payload $self:ident MemoryCopy $dst:ident $src:ident) => (
        if $src != 0 || $dst != 0 {
            $self.push_str(" ");
            $self.memory_index($dst)?;
            $self.push_str(" ");
            $self.memory_index($src)?;
        }
    );
    (payload $self:ident MemoryFill $mem:ident) => (
        if $mem != 0 {
            $self.push_str(" ");
            $self.memory_index($mem)?;
        }
    );
    (payload $self:ident MemoryDiscard $mem:ident) => (
        if $mem != 0 {
            $self.push_str(" ");
            $self.memory_index($mem)?;
        }
    );
    (payload $self:ident I32Const $val:ident) => (write!($self.result(), " {}", $val)?);
    (payload $self:ident I64Const $val:ident) => (write!($self.result(), " {}", $val)?);
    (payload $self:ident F32Const $val:ident) => (
        $self.push_str(" ");
        $self.printer.print_f32($val.bits())?;
    );
    (payload $self:ident F64Const $val:ident) => (
        $self.push_str(" ");
        $self.printer.print_f64($val.bits())?;
    );
    (payload $self:ident V128Const $val:ident) => (
        $self.push_str(" i32x4");
        for chunk in $val.bytes().chunks(4) {
            write!(
                $self.result(),
                " 0x{:02x}{:02x}{:02x}{:02x}",
                chunk[3],
                chunk[2],
                chunk[1],
                chunk[0],
            )?;
        }
    );
    (payload $self:ident $op:ident $($arg:ident)*) => (
        $(
            $self.push_str(" ");
            $self.$arg($arg)?;
        )*
    );

    (name Block) => ("block");
    (name If) => ("if");
    (name Else) => ("else");
    (name Loop) => ("loop");
    (name End) => ("end");
    (name Unreachable) => ("unreachable");
    (name Nop) => ("nop");
    (name Br) => ("br");
    (name BrIf) => ("br_if");
    (name BrOnNull) => ("br_on_null");
    (name BrOnNonNull) => ("br_on_non_null");
    (name BrTable) => ("br_table");
    (name Return) => ("return");
    (name Call) => ("call");
    (name CallIndirect) => ("call_indirect");
    (name CallRef) => ("call_ref");
    (name ReturnCall) => ("return_call");
    (name ReturnCallIndirect) => ("return_call_indirect");
    (name ReturnCallRef) => ("return_call_ref");
    (name Drop) => ("drop");
    (name Select) => ("select");
    (name TypedSelect) => ("select");
    (name LocalGet) => ("local.get");
    (name LocalSet) => ("local.set");
    (name LocalTee) => ("local.tee");
    (name GlobalGet) => ("global.get");
    (name GlobalSet) => ("global.set");
    (name TableGet) => ("table.get");
    (name TableSet) => ("table.set");
    (name I32Load) => ("i32.load");
    (name I64Load) => ("i64.load");
    (name F32Load) => ("f32.load");
    (name F64Load) => ("f64.load");
    (name I32Load8S) => ("i32.load8_s");
    (name I32Load8U) => ("i32.load8_u");
    (name I32Load16S) => ("i32.load16_s");
    (name I32Load16U) => ("i32.load16_u");
    (name I64Load8S) => ("i64.load8_s");
    (name I64Load8U) => ("i64.load8_u");
    (name I64Load16S) => ("i64.load16_s");
    (name I64Load16U) => ("i64.load16_u");
    (name I64Load32S) => ("i64.load32_s");
    (name I64Load32U) => ("i64.load32_u");
    (name I32Store) => ("i32.store");
    (name I64Store) => ("i64.store");
    (name F32Store) => ("f32.store");
    (name F64Store) => ("f64.store");
    (name I32Store8) => ("i32.store8");
    (name I32Store16) => ("i32.store16");
    (name I64Store8) => ("i64.store8");
    (name I64Store16) => ("i64.store16");
    (name I64Store32) => ("i64.store32");
    (name MemorySize) => ("memory.size");
    (name MemoryGrow) => ("memory.grow");
    (name MemoryInit) => ("memory.init");
    (name MemoryCopy) => ("memory.copy");
    (name MemoryFill) => ("memory.fill");
    (name MemoryDiscard) => ("memory.discard");
    (name DataDrop) => ("data.drop");
    (name ElemDrop) => ("elem.drop");
    (name TableInit) => ("table.init");
    (name TableCopy) => ("table.copy");
    (name TableFill) => ("table.fill");
    (name TableSize) => ("table.size");
    (name TableGrow) => ("table.grow");
    (name RefAsNonNull) => ("ref.as_non_null");
    (name RefNull) => ("ref.null");
    (name RefIsNull) => ("ref.is_null");
    (name RefFunc) => ("ref.func");
    (name I32Const) => ("i32.const");
    (name I64Const) => ("i64.const");
    (name F32Const) => ("f32.const");
    (name F64Const) => ("f64.const");
    (name I32Clz) => ("i32.clz");
    (name I32Ctz) => ("i32.ctz");
    (name I32Popcnt) => ("i32.popcnt");
    (name I32Add) => ("i32.add");
    (name I32Sub) => ("i32.sub");
    (name I32Mul) => ("i32.mul");
    (name I32DivS) => ("i32.div_s");
    (name I32DivU) => ("i32.div_u");
    (name I32RemS) => ("i32.rem_s");
    (name I32RemU) => ("i32.rem_u");
    (name I32And) => ("i32.and");
    (name I32Or) => ("i32.or");
    (name I32Xor) => ("i32.xor");
    (name I32Shl) => ("i32.shl");
    (name I32ShrS) => ("i32.shr_s");
    (name I32ShrU) => ("i32.shr_u");
    (name I32Rotl) => ("i32.rotl");
    (name I32Rotr) => ("i32.rotr");
    (name I64Clz) => ("i64.clz");
    (name I64Ctz) => ("i64.ctz");
    (name I64Popcnt) => ("i64.popcnt");
    (name I64Add) => ("i64.add");
    (name I64Sub) => ("i64.sub");
    (name I64Mul) => ("i64.mul");
    (name I64DivS) => ("i64.div_s");
    (name I64DivU) => ("i64.div_u");
    (name I64RemS) => ("i64.rem_s");
    (name I64RemU) => ("i64.rem_u");
    (name I64And) => ("i64.and");
    (name I64Or) => ("i64.or");
    (name I64Xor) => ("i64.xor");
    (name I64Shl) => ("i64.shl");
    (name I64ShrS) => ("i64.shr_s");
    (name I64ShrU) => ("i64.shr_u");
    (name I64Rotl) => ("i64.rotl");
    (name I64Rotr) => ("i64.rotr");
    (name F32Abs) => ("f32.abs");
    (name F32Neg) => ("f32.neg");
    (name F32Ceil) => ("f32.ceil");
    (name F32Floor) => ("f32.floor");
    (name F32Trunc) => ("f32.trunc");
    (name F32Nearest) => ("f32.nearest");
    (name F32Sqrt) => ("f32.sqrt");
    (name F32Add) => ("f32.add");
    (name F32Sub) => ("f32.sub");
    (name F32Mul) => ("f32.mul");
    (name F32Div) => ("f32.div");
    (name F32Min) => ("f32.min");
    (name F32Max) => ("f32.max");
    (name F32Copysign) => ("f32.copysign");
    (name F64Abs) => ("f64.abs");
    (name F64Neg) => ("f64.neg");
    (name F64Ceil) => ("f64.ceil");
    (name F64Floor) => ("f64.floor");
    (name F64Trunc) => ("f64.trunc");
    (name F64Nearest) => ("f64.nearest");
    (name F64Sqrt) => ("f64.sqrt");
    (name F64Add) => ("f64.add");
    (name F64Sub) => ("f64.sub");
    (name F64Mul) => ("f64.mul");
    (name F64Div) => ("f64.div");
    (name F64Min) => ("f64.min");
    (name F64Max) => ("f64.max");
    (name F64Copysign) => ("f64.copysign");
    (name I32Eqz) => ("i32.eqz");
    (name I32Eq) => ("i32.eq");
    (name I32Ne) => ("i32.ne");
    (name I32LtS) => ("i32.lt_s");
    (name I32LtU) => ("i32.lt_u");
    (name I32GtS) => ("i32.gt_s");
    (name I32GtU) => ("i32.gt_u");
    (name I32LeS) => ("i32.le_s");
    (name I32LeU) => ("i32.le_u");
    (name I32GeS) => ("i32.ge_s");
    (name I32GeU) => ("i32.ge_u");
    (name I64Eqz) => ("i64.eqz");
    (name I64Eq) => ("i64.eq");
    (name I64Ne) => ("i64.ne");
    (name I64LtS) => ("i64.lt_s");
    (name I64LtU) => ("i64.lt_u");
    (name I64GtS) => ("i64.gt_s");
    (name I64GtU) => ("i64.gt_u");
    (name I64LeS) => ("i64.le_s");
    (name I64LeU) => ("i64.le_u");
    (name I64GeS) => ("i64.ge_s");
    (name I64GeU) => ("i64.ge_u");
    (name F32Eq) => ("f32.eq");
    (name F32Ne) => ("f32.ne");
    (name F32Lt) => ("f32.lt");
    (name F32Gt) => ("f32.gt");
    (name F32Le) => ("f32.le");
    (name F32Ge) => ("f32.ge");
    (name F64Eq) => ("f64.eq");
    (name F64Ne) => ("f64.ne");
    (name F64Lt) => ("f64.lt");
    (name F64Gt) => ("f64.gt");
    (name F64Le) => ("f64.le");
    (name F64Ge) => ("f64.ge");
    (name I32WrapI64) => ("i32.wrap_i64");
    (name I32TruncF32S) => ("i32.trunc_f32_s");
    (name I32TruncF32U) => ("i32.trunc_f32_u");
    (name I32TruncF64S) => ("i32.trunc_f64_s");
    (name I32TruncF64U) => ("i32.trunc_f64_u");
    (name I64ExtendI32S) => ("i64.extend_i32_s");
    (name I64ExtendI32U) => ("i64.extend_i32_u");
    (name I64TruncF32S) => ("i64.trunc_f32_s");
    (name I64TruncF32U) => ("i64.trunc_f32_u");
    (name I64TruncF64S) => ("i64.trunc_f64_s");
    (name I64TruncF64U) => ("i64.trunc_f64_u");
    (name F32ConvertI32S) => ("f32.convert_i32_s");
    (name F32ConvertI32U) => ("f32.convert_i32_u");
    (name F32ConvertI64S) => ("f32.convert_i64_s");
    (name F32ConvertI64U) => ("f32.convert_i64_u");
    (name F32DemoteF64) => ("f32.demote_f64");
    (name F64ConvertI32S) => ("f64.convert_i32_s");
    (name F64ConvertI32U) => ("f64.convert_i32_u");
    (name F64ConvertI64S) => ("f64.convert_i64_s");
    (name F64ConvertI64U) => ("f64.convert_i64_u");
    (name F64PromoteF32) => ("f64.promote_f32");
    (name I32ReinterpretF32) => ("i32.reinterpret_f32");
    (name I64ReinterpretF64) => ("i64.reinterpret_f64");
    (name F32ReinterpretI32) => ("f32.reinterpret_i32");
    (name F64ReinterpretI64) => ("f64.reinterpret_i64");
    (name I32TruncSatF32S) => ("i32.trunc_sat_f32_s");
    (name I32TruncSatF32U) => ("i32.trunc_sat_f32_u");
    (name I32TruncSatF64S) => ("i32.trunc_sat_f64_s");
    (name I32TruncSatF64U) => ("i32.trunc_sat_f64_u");
    (name I64TruncSatF32S) => ("i64.trunc_sat_f32_s");
    (name I64TruncSatF32U) => ("i64.trunc_sat_f32_u");
    (name I64TruncSatF64S) => ("i64.trunc_sat_f64_s");
    (name I64TruncSatF64U) => ("i64.trunc_sat_f64_u");
    (name I32Extend8S) => ("i32.extend8_s");
    (name I32Extend16S) => ("i32.extend16_s");
    (name I64Extend8S) => ("i64.extend8_s");
    (name I64Extend16S) => ("i64.extend16_s");
    (name I64Extend32S) => ("i64.extend32_s");
    (name MemoryAtomicNotify) => ("memory.atomic.notify");
    (name MemoryAtomicWait32) => ("memory.atomic.wait32");
    (name MemoryAtomicWait64) => ("memory.atomic.wait64");
    (name AtomicFence) => ("atomic.fence");
    (name I32AtomicLoad) => ("i32.atomic.load");
    (name I64AtomicLoad) => ("i64.atomic.load");
    (name I32AtomicLoad8U) => ("i32.atomic.load8_u");
    (name I32AtomicLoad16U) => ("i32.atomic.load16_u");
    (name I64AtomicLoad8U) => ("i64.atomic.load8_u");
    (name I64AtomicLoad16U) => ("i64.atomic.load16_u");
    (name I64AtomicLoad32U) => ("i64.atomic.load32_u");
    (name I32AtomicStore) => ("i32.atomic.store");
    (name I64AtomicStore) => ("i64.atomic.store");
    (name I32AtomicStore8) => ("i32.atomic.store8");
    (name I32AtomicStore16) => ("i32.atomic.store16");
    (name I64AtomicStore8) => ("i64.atomic.store8");
    (name I64AtomicStore16) => ("i64.atomic.store16");
    (name I64AtomicStore32) => ("i64.atomic.store32");
    (name I32AtomicRmwAdd) => ("i32.atomic.rmw.add");
    (name I64AtomicRmwAdd) => ("i64.atomic.rmw.add");
    (name I32AtomicRmw8AddU) => ("i32.atomic.rmw8.add_u");
    (name I32AtomicRmw16AddU) => ("i32.atomic.rmw16.add_u");
    (name I64AtomicRmw8AddU) => ("i64.atomic.rmw8.add_u");
    (name I64AtomicRmw16AddU) => ("i64.atomic.rmw16.add_u");
    (name I64AtomicRmw32AddU) => ("i64.atomic.rmw32.add_u");
    (name I32AtomicRmwSub) => ("i32.atomic.rmw.sub");
    (name I64AtomicRmwSub) => ("i64.atomic.rmw.sub");
    (name I32AtomicRmw8SubU) => ("i32.atomic.rmw8.sub_u");
    (name I32AtomicRmw16SubU) => ("i32.atomic.rmw16.sub_u");
    (name I64AtomicRmw8SubU) => ("i64.atomic.rmw8.sub_u");
    (name I64AtomicRmw16SubU) => ("i64.atomic.rmw16.sub_u");
    (name I64AtomicRmw32SubU) => ("i64.atomic.rmw32.sub_u");
    (name I32AtomicRmwAnd) => ("i32.atomic.rmw.and");
    (name I64AtomicRmwAnd) => ("i64.atomic.rmw.and");
    (name I32AtomicRmw8AndU) => ("i32.atomic.rmw8.and_u");
    (name I32AtomicRmw16AndU) => ("i32.atomic.rmw16.and_u");
    (name I64AtomicRmw8AndU) => ("i64.atomic.rmw8.and_u");
    (name I64AtomicRmw16AndU) => ("i64.atomic.rmw16.and_u");
    (name I64AtomicRmw32AndU) => ("i64.atomic.rmw32.and_u");
    (name I32AtomicRmwOr) => ("i32.atomic.rmw.or");
    (name I64AtomicRmwOr) => ("i64.atomic.rmw.or");
    (name I32AtomicRmw8OrU) => ("i32.atomic.rmw8.or_u");
    (name I32AtomicRmw16OrU) => ("i32.atomic.rmw16.or_u");
    (name I64AtomicRmw8OrU) => ("i64.atomic.rmw8.or_u");
    (name I64AtomicRmw16OrU) => ("i64.atomic.rmw16.or_u");
    (name I64AtomicRmw32OrU) => ("i64.atomic.rmw32.or_u");
    (name I32AtomicRmwXor) => ("i32.atomic.rmw.xor");
    (name I64AtomicRmwXor) => ("i64.atomic.rmw.xor");
    (name I32AtomicRmw8XorU) => ("i32.atomic.rmw8.xor_u");
    (name I32AtomicRmw16XorU) => ("i32.atomic.rmw16.xor_u");
    (name I64AtomicRmw8XorU) => ("i64.atomic.rmw8.xor_u");
    (name I64AtomicRmw16XorU) => ("i64.atomic.rmw16.xor_u");
    (name I64AtomicRmw32XorU) => ("i64.atomic.rmw32.xor_u");
    (name I32AtomicRmwXchg) => ("i32.atomic.rmw.xchg");
    (name I64AtomicRmwXchg) => ("i64.atomic.rmw.xchg");
    (name I32AtomicRmw8XchgU) => ("i32.atomic.rmw8.xchg_u");
    (name I32AtomicRmw16XchgU) => ("i32.atomic.rmw16.xchg_u");
    (name I64AtomicRmw8XchgU) => ("i64.atomic.rmw8.xchg_u");
    (name I64AtomicRmw16XchgU) => ("i64.atomic.rmw16.xchg_u");
    (name I64AtomicRmw32XchgU) => ("i64.atomic.rmw32.xchg_u");
    (name I32AtomicRmwCmpxchg) => ("i32.atomic.rmw.cmpxchg");
    (name I64AtomicRmwCmpxchg) => ("i64.atomic.rmw.cmpxchg");
    (name I32AtomicRmw8CmpxchgU) => ("i32.atomic.rmw8.cmpxchg_u");
    (name I32AtomicRmw16CmpxchgU) => ("i32.atomic.rmw16.cmpxchg_u");
    (name I64AtomicRmw8CmpxchgU) => ("i64.atomic.rmw8.cmpxchg_u");
    (name I64AtomicRmw16CmpxchgU) => ("i64.atomic.rmw16.cmpxchg_u");
    (name I64AtomicRmw32CmpxchgU) => ("i64.atomic.rmw32.cmpxchg_u");
    (name V128Load) => ("v128.load");
    (name V128Load8x8S) => ("v128.load8x8_s");
    (name V128Load8x8U) => ("v128.load8x8_u");
    (name V128Load16x4S) => ("v128.load16x4_s");
    (name V128Load16x4U) => ("v128.load16x4_u");
    (name V128Load32x2S) => ("v128.load32x2_s");
    (name V128Load32x2U) => ("v128.load32x2_u");
    (name V128Load8Splat) => ("v128.load8_splat");
    (name V128Load16Splat) => ("v128.load16_splat");
    (name V128Load32Splat) => ("v128.load32_splat");
    (name V128Load64Splat) => ("v128.load64_splat");
    (name V128Load32Zero) => ("v128.load32_zero");
    (name V128Load64Zero) => ("v128.load64_zero");
    (name V128Store) => ("v128.store");
    (name V128Load8Lane) => ("v128.load8_lane");
    (name V128Load16Lane) => ("v128.load16_lane");
    (name V128Load32Lane) => ("v128.load32_lane");
    (name V128Load64Lane) => ("v128.load64_lane");
    (name V128Store8Lane) => ("v128.store8_lane");
    (name V128Store16Lane) => ("v128.store16_lane");
    (name V128Store32Lane) => ("v128.store32_lane");
    (name V128Store64Lane) => ("v128.store64_lane");
    (name V128Const) => ("v128.const");
    (name I8x16Shuffle) => ("i8x16.shuffle");
    (name I8x16ExtractLaneS) => ("i8x16.extract_lane_s");
    (name I8x16ExtractLaneU) => ("i8x16.extract_lane_u");
    (name I8x16ReplaceLane) => ("i8x16.replace_lane");
    (name I16x8ExtractLaneS) => ("i16x8.extract_lane_s");
    (name I16x8ExtractLaneU) => ("i16x8.extract_lane_u");
    (name I16x8ReplaceLane) => ("i16x8.replace_lane");
    (name I32x4ExtractLane) => ("i32x4.extract_lane");
    (name I32x4ReplaceLane) => ("i32x4.replace_lane");
    (name I64x2ExtractLane) => ("i64x2.extract_lane");
    (name I64x2ReplaceLane) => ("i64x2.replace_lane");
    (name F32x4ExtractLane) => ("f32x4.extract_lane");
    (name F32x4ReplaceLane) => ("f32x4.replace_lane");
    (name F64x2ExtractLane) => ("f64x2.extract_lane");
    (name F64x2ReplaceLane) => ("f64x2.replace_lane");
    (name I8x16Swizzle) => ("i8x16.swizzle");
    (name I8x16Splat) => ("i8x16.splat");
    (name I16x8Splat) => ("i16x8.splat");
    (name I32x4Splat) => ("i32x4.splat");
    (name I64x2Splat) => ("i64x2.splat");
    (name F32x4Splat) => ("f32x4.splat");
    (name F64x2Splat) => ("f64x2.splat");
    (name I8x16Eq) => ("i8x16.eq");
    (name I8x16Ne) => ("i8x16.ne");
    (name I8x16LtS) => ("i8x16.lt_s");
    (name I8x16LtU) => ("i8x16.lt_u");
    (name I8x16GtS) => ("i8x16.gt_s");
    (name I8x16GtU) => ("i8x16.gt_u");
    (name I8x16LeS) => ("i8x16.le_s");
    (name I8x16LeU) => ("i8x16.le_u");
    (name I8x16GeS) => ("i8x16.ge_s");
    (name I8x16GeU) => ("i8x16.ge_u");
    (name I16x8Eq) => ("i16x8.eq");
    (name I16x8Ne) => ("i16x8.ne");
    (name I16x8LtS) => ("i16x8.lt_s");
    (name I16x8LtU) => ("i16x8.lt_u");
    (name I16x8GtS) => ("i16x8.gt_s");
    (name I16x8GtU) => ("i16x8.gt_u");
    (name I16x8LeS) => ("i16x8.le_s");
    (name I16x8LeU) => ("i16x8.le_u");
    (name I16x8GeS) => ("i16x8.ge_s");
    (name I16x8GeU) => ("i16x8.ge_u");
    (name I32x4Eq) => ("i32x4.eq");
    (name I32x4Ne) => ("i32x4.ne");
    (name I32x4LtS) => ("i32x4.lt_s");
    (name I32x4LtU) => ("i32x4.lt_u");
    (name I32x4GtS) => ("i32x4.gt_s");
    (name I32x4GtU) => ("i32x4.gt_u");
    (name I32x4LeS) => ("i32x4.le_s");
    (name I32x4LeU) => ("i32x4.le_u");
    (name I32x4GeS) => ("i32x4.ge_s");
    (name I32x4GeU) => ("i32x4.ge_u");
    (name I64x2Eq) => ("i64x2.eq");
    (name I64x2Ne) => ("i64x2.ne");
    (name I64x2LtS) => ("i64x2.lt_s");
    (name I64x2GtS) => ("i64x2.gt_s");
    (name I64x2LeS) => ("i64x2.le_s");
    (name I64x2GeS) => ("i64x2.ge_s");
    (name F32x4Eq) => ("f32x4.eq");
    (name F32x4Ne) => ("f32x4.ne");
    (name F32x4Lt) => ("f32x4.lt");
    (name F32x4Gt) => ("f32x4.gt");
    (name F32x4Le) => ("f32x4.le");
    (name F32x4Ge) => ("f32x4.ge");
    (name F64x2Eq) => ("f64x2.eq");
    (name F64x2Ne) => ("f64x2.ne");
    (name F64x2Lt) => ("f64x2.lt");
    (name F64x2Gt) => ("f64x2.gt");
    (name F64x2Le) => ("f64x2.le");
    (name F64x2Ge) => ("f64x2.ge");
    (name V128Not) => ("v128.not");
    (name V128And) => ("v128.and");
    (name V128AndNot) => ("v128.andnot");
    (name V128Or) => ("v128.or");
    (name V128Xor) => ("v128.xor");
    (name V128Bitselect) => ("v128.bitselect");
    (name V128AnyTrue) => ("v128.any_true");
    (name I8x16Abs) => ("i8x16.abs");
    (name I8x16Neg) => ("i8x16.neg");
    (name I8x16Popcnt) => ("i8x16.popcnt");
    (name I8x16AllTrue) => ("i8x16.all_true");
    (name I8x16Bitmask) => ("i8x16.bitmask");
    (name I8x16NarrowI16x8S) => ("i8x16.narrow_i16x8_s");
    (name I8x16NarrowI16x8U) => ("i8x16.narrow_i16x8_u");
    (name I8x16Shl) => ("i8x16.shl");
    (name I8x16ShrS) => ("i8x16.shr_s");
    (name I8x16ShrU) => ("i8x16.shr_u");
    (name I8x16Add) => ("i8x16.add");
    (name I8x16AddSatS) => ("i8x16.add_sat_s");
    (name I8x16AddSatU) => ("i8x16.add_sat_u");
    (name I8x16Sub) => ("i8x16.sub");
    (name I8x16SubSatS) => ("i8x16.sub_sat_s");
    (name I8x16SubSatU) => ("i8x16.sub_sat_u");
    (name I8x16MinS) => ("i8x16.min_s");
    (name I8x16MinU) => ("i8x16.min_u");
    (name I8x16MaxS) => ("i8x16.max_s");
    (name I8x16MaxU) => ("i8x16.max_u");
    (name I8x16AvgrU) => ("i8x16.avgr_u");
    (name I16x8ExtAddPairwiseI8x16S) => ("i16x8.extadd_pairwise_i8x16_s");
    (name I16x8ExtAddPairwiseI8x16U) => ("i16x8.extadd_pairwise_i8x16_u");
    (name I16x8Abs) => ("i16x8.abs");
    (name I16x8Neg) => ("i16x8.neg");
    (name I16x8Q15MulrSatS) => ("i16x8.q15mulr_sat_s");
    (name I16x8AllTrue) => ("i16x8.all_true");
    (name I16x8Bitmask) => ("i16x8.bitmask");
    (name I16x8NarrowI32x4S) => ("i16x8.narrow_i32x4_s");
    (name I16x8NarrowI32x4U) => ("i16x8.narrow_i32x4_u");
    (name I16x8ExtendLowI8x16S) => ("i16x8.extend_low_i8x16_s");
    (name I16x8ExtendHighI8x16S) => ("i16x8.extend_high_i8x16_s");
    (name I16x8ExtendLowI8x16U) => ("i16x8.extend_low_i8x16_u");
    (name I16x8ExtendHighI8x16U) => ("i16x8.extend_high_i8x16_u");
    (name I16x8Shl) => ("i16x8.shl");
    (name I16x8ShrS) => ("i16x8.shr_s");
    (name I16x8ShrU) => ("i16x8.shr_u");
    (name I16x8Add) => ("i16x8.add");
    (name I16x8AddSatS) => ("i16x8.add_sat_s");
    (name I16x8AddSatU) => ("i16x8.add_sat_u");
    (name I16x8Sub) => ("i16x8.sub");
    (name I16x8SubSatS) => ("i16x8.sub_sat_s");
    (name I16x8SubSatU) => ("i16x8.sub_sat_u");
    (name I16x8Mul) => ("i16x8.mul");
    (name I16x8MinS) => ("i16x8.min_s");
    (name I16x8MinU) => ("i16x8.min_u");
    (name I16x8MaxS) => ("i16x8.max_s");
    (name I16x8MaxU) => ("i16x8.max_u");
    (name I16x8AvgrU) => ("i16x8.avgr_u");
    (name I16x8ExtMulLowI8x16S) => ("i16x8.extmul_low_i8x16_s");
    (name I16x8ExtMulHighI8x16S) => ("i16x8.extmul_high_i8x16_s");
    (name I16x8ExtMulLowI8x16U) => ("i16x8.extmul_low_i8x16_u");
    (name I16x8ExtMulHighI8x16U) => ("i16x8.extmul_high_i8x16_u");
    (name I32x4ExtAddPairwiseI16x8S) => ("i32x4.extadd_pairwise_i16x8_s");
    (name I32x4ExtAddPairwiseI16x8U) => ("i32x4.extadd_pairwise_i16x8_u");
    (name I32x4Abs) => ("i32x4.abs");
    (name I32x4Neg) => ("i32x4.neg");
    (name I32x4AllTrue) => ("i32x4.all_true");
    (name I32x4Bitmask) => ("i32x4.bitmask");
    (name I32x4ExtendLowI16x8S) => ("i32x4.extend_low_i16x8_s");
    (name I32x4ExtendHighI16x8S) => ("i32x4.extend_high_i16x8_s");
    (name I32x4ExtendLowI16x8U) => ("i32x4.extend_low_i16x8_u");
    (name I32x4ExtendHighI16x8U) => ("i32x4.extend_high_i16x8_u");
    (name I32x4Shl) => ("i32x4.shl");
    (name I32x4ShrS) => ("i32x4.shr_s");
    (name I32x4ShrU) => ("i32x4.shr_u");
    (name I32x4Add) => ("i32x4.add");
    (name I32x4Sub) => ("i32x4.sub");
    (name I32x4Mul) => ("i32x4.mul");
    (name I32x4MinS) => ("i32x4.min_s");
    (name I32x4MinU) => ("i32x4.min_u");
    (name I32x4MaxS) => ("i32x4.max_s");
    (name I32x4MaxU) => ("i32x4.max_u");
    (name I32x4DotI16x8S) => ("i32x4.dot_i16x8_s");
    (name I32x4ExtMulLowI16x8S) => ("i32x4.extmul_low_i16x8_s");
    (name I32x4ExtMulHighI16x8S) => ("i32x4.extmul_high_i16x8_s");
    (name I32x4ExtMulLowI16x8U) => ("i32x4.extmul_low_i16x8_u");
    (name I32x4ExtMulHighI16x8U) => ("i32x4.extmul_high_i16x8_u");
    (name I64x2Abs) => ("i64x2.abs");
    (name I64x2Neg) => ("i64x2.neg");
    (name I64x2AllTrue) => ("i64x2.all_true");
    (name I64x2Bitmask) => ("i64x2.bitmask");
    (name I64x2ExtendLowI32x4S) => ("i64x2.extend_low_i32x4_s");
    (name I64x2ExtendHighI32x4S) => ("i64x2.extend_high_i32x4_s");
    (name I64x2ExtendLowI32x4U) => ("i64x2.extend_low_i32x4_u");
    (name I64x2ExtendHighI32x4U) => ("i64x2.extend_high_i32x4_u");
    (name I64x2Shl) => ("i64x2.shl");
    (name I64x2ShrS) => ("i64x2.shr_s");
    (name I64x2ShrU) => ("i64x2.shr_u");
    (name I64x2Add) => ("i64x2.add");
    (name I64x2Sub) => ("i64x2.sub");
    (name I64x2Mul) => ("i64x2.mul");
    (name I64x2ExtMulLowI32x4S) => ("i64x2.extmul_low_i32x4_s");
    (name I64x2ExtMulHighI32x4S) => ("i64x2.extmul_high_i32x4_s");
    (name I64x2ExtMulLowI32x4U) => ("i64x2.extmul_low_i32x4_u");
    (name I64x2ExtMulHighI32x4U) => ("i64x2.extmul_high_i32x4_u");
    (name F32x4Ceil) => ("f32x4.ceil");
    (name F32x4Floor) => ("f32x4.floor");
    (name F32x4Trunc) => ("f32x4.trunc");
    (name F32x4Nearest) => ("f32x4.nearest");
    (name F32x4Abs) => ("f32x4.abs");
    (name F32x4Neg) => ("f32x4.neg");
    (name F32x4Sqrt) => ("f32x4.sqrt");
    (name F32x4Add) => ("f32x4.add");
    (name F32x4Sub) => ("f32x4.sub");
    (name F32x4Mul) => ("f32x4.mul");
    (name F32x4Div) => ("f32x4.div");
    (name F32x4Min) => ("f32x4.min");
    (name F32x4Max) => ("f32x4.max");
    (name F32x4PMin) => ("f32x4.pmin");
    (name F32x4PMax) => ("f32x4.pmax");
    (name F64x2Ceil) => ("f64x2.ceil");
    (name F64x2Floor) => ("f64x2.floor");
    (name F64x2Trunc) => ("f64x2.trunc");
    (name F64x2Nearest) => ("f64x2.nearest");
    (name F64x2Abs) => ("f64x2.abs");
    (name F64x2Neg) => ("f64x2.neg");
    (name F64x2Sqrt) => ("f64x2.sqrt");
    (name F64x2Add) => ("f64x2.add");
    (name F64x2Sub) => ("f64x2.sub");
    (name F64x2Mul) => ("f64x2.mul");
    (name F64x2Div) => ("f64x2.div");
    (name F64x2Min) => ("f64x2.min");
    (name F64x2Max) => ("f64x2.max");
    (name F64x2PMin) => ("f64x2.pmin");
    (name F64x2PMax) => ("f64x2.pmax");
    (name I32x4TruncSatF32x4S) => ("i32x4.trunc_sat_f32x4_s");
    (name I32x4TruncSatF32x4U) => ("i32x4.trunc_sat_f32x4_u");
    (name F32x4ConvertI32x4S) => ("f32x4.convert_i32x4_s");
    (name F32x4ConvertI32x4U) => ("f32x4.convert_i32x4_u");
    (name I32x4TruncSatF64x2SZero) => ("i32x4.trunc_sat_f64x2_s_zero");
    (name I32x4TruncSatF64x2UZero) => ("i32x4.trunc_sat_f64x2_u_zero");
    (name F64x2ConvertLowI32x4S) => ("f64x2.convert_low_i32x4_s");
    (name F64x2ConvertLowI32x4U) => ("f64x2.convert_low_i32x4_u");
    (name F32x4DemoteF64x2Zero) => ("f32x4.demote_f64x2_zero");
    (name F64x2PromoteLowF32x4) => ("f64x2.promote_low_f32x4");
    (name Try) => ("try");
    (name Catch) => ("catch");
    (name Throw) => ("throw");
    (name Rethrow) => ("rethrow");
    (name Delegate) => ("delegate");
    (name CatchAll) => ("catch_all");
    (name I8x16RelaxedSwizzle) => ("i8x16.relaxed_swizzle");
    (name I32x4RelaxedTruncF32x4S) => ("i32x4.relaxed_trunc_f32x4_s");
    (name I32x4RelaxedTruncF32x4U) => ("i32x4.relaxed_trunc_f32x4_u");
    (name I32x4RelaxedTruncF64x2SZero) => ("i32x4.relaxed_trunc_f64x2_s_zero");
    (name I32x4RelaxedTruncF64x2UZero) => ("i32x4.relaxed_trunc_f64x2_u_zero");
    (name F32x4RelaxedMadd) => ("f32x4.relaxed_madd");
    (name F32x4RelaxedNmadd) => ("f32x4.relaxed_nmadd");
    (name F64x2RelaxedMadd) => ("f64x2.relaxed_madd");
    (name F64x2RelaxedNmadd) => ("f64x2.relaxed_nmadd");
    (name I8x16RelaxedLaneselect) => ("i8x16.relaxed_laneselect");
    (name I16x8RelaxedLaneselect) => ("i16x8.relaxed_laneselect");
    (name I32x4RelaxedLaneselect) => ("i32x4.relaxed_laneselect");
    (name I64x2RelaxedLaneselect) => ("i64x2.relaxed_laneselect");
    (name F32x4RelaxedMin) => ("f32x4.relaxed_min");
    (name F32x4RelaxedMax) => ("f32x4.relaxed_max");
    (name F64x2RelaxedMin) => ("f64x2.relaxed_min");
    (name F64x2RelaxedMax) => ("f64x2.relaxed_max");
    (name I16x8RelaxedQ15mulrS) => ("i16x8.relaxed_q15mulr_s");
    (name I16x8RelaxedDotI8x16I7x16S) => ("i16x8.relaxed_dot_i8x16_i7x16_s");
    (name I32x4RelaxedDotI8x16I7x16AddS) => ("i32x4.relaxed_dot_i8x16_i7x16_add_s");
    (name RefI31) => ("ref.i31");
    (name I31GetS) => ("i31.get_s");
    (name I31GetU) => ("i31.get_u");
}

impl<'a> VisitOperator<'a> for PrintOperator<'_, '_> {
    type Output = Result<OpKind>;

    wasmparser::for_each_operator!(define_visit);
}
