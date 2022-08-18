use super::{Printer, State};
use anyhow::Result;
use std::fmt::Write;
use wasmparser::{BlockType, BrTable, Ieee32, Ieee64, MemArg, ValType, VisitOperator, V128};

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

    fn print_blockty(&mut self, ty: &BlockType) -> Result<()> {
        self.printer.print_blockty(self.state, ty, self.cur_depth())
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

    fn print_func_idx(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.func_names, idx)
    }

    fn print_table_idx(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.table_names, idx)
    }

    fn print_global_idx(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.global_names, idx)
    }

    fn print_memory_idx(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.memory_names, idx)
    }

    fn print_data_idx(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.data_names, idx)
    }

    fn print_element_idx(&mut self, idx: u32) -> Result<()> {
        self.printer.print_idx(&self.state.core.element_names, idx)
    }

    fn print_local_idx(&mut self, idx: u32) -> Result<()> {
        self.printer
            .print_local_idx(self.state, self.state.core.funcs, idx)
    }

    fn print_type_ref(&mut self, idx: u32) -> Result<()> {
        self.printer.print_type_ref(self.state, idx, true, None)
    }

    fn print_valtype(&mut self, ty: ValType) -> Result<()> {
        self.printer.print_valtype(ty)
    }

    fn instr(&mut self, name: &str) -> Result<OpKind> {
        self.push_str(name);
        Ok(OpKind::Normal)
    }

    fn mem_instr(&mut self, name: &str, memarg: &MemArg, align: u32) -> Result<OpKind> {
        self.printer.mem_instr(self.state, name, memarg, align)?;
        Ok(OpKind::Normal)
    }
}

pub enum OpKind {
    BlockStart,
    BlockMid,
    End,
    Delegate,
    Normal,
}

impl<'a> VisitOperator<'a> for PrintOperator<'_, '_> {
    type Output = Result<OpKind>;

    fn visit_unreachable(&mut self, _pos: usize) -> Self::Output {
        self.instr("unreachable")
    }
    fn visit_nop(&mut self, _pos: usize) -> Self::Output {
        self.instr("nop")
    }
    fn visit_block(&mut self, _pos: usize, ty: BlockType) -> Self::Output {
        self.push_str("block");
        self.print_blockty(&ty)?;
        Ok(OpKind::BlockStart)
    }
    fn visit_loop(&mut self, _pos: usize, ty: BlockType) -> Self::Output {
        self.push_str("loop");
        self.print_blockty(&ty)?;
        Ok(OpKind::BlockStart)
    }
    fn visit_if(&mut self, _pos: usize, ty: BlockType) -> Self::Output {
        self.push_str("if");
        self.print_blockty(&ty)?;
        Ok(OpKind::BlockStart)
    }
    fn visit_else(&mut self, _pos: usize) -> Self::Output {
        self.push_str("else");
        Ok(OpKind::BlockMid)
    }
    fn visit_try(&mut self, _pos: usize, ty: BlockType) -> Self::Output {
        self.push_str("try");
        self.print_blockty(&ty)?;
        Ok(OpKind::BlockStart)
    }
    fn visit_catch(&mut self, _pos: usize, index: u32) -> Self::Output {
        write!(self.result(), "catch {index}")?;
        Ok(OpKind::BlockMid)
    }
    fn visit_throw(&mut self, _pos: usize, index: u32) -> Self::Output {
        write!(self.result(), "throw {index}")?;
        Ok(OpKind::Normal)
    }
    fn visit_rethrow(&mut self, _pos: usize, relative_depth: u32) -> Self::Output {
        let label = self.label(relative_depth);
        write!(self.result(), "rethrow {relative_depth} (;{label};)")?;
        Ok(OpKind::Normal)
    }
    fn visit_delegate(&mut self, _pos: usize, relative_depth: u32) -> Self::Output {
        let label = self.label(relative_depth);
        write!(self.result(), "delegate {relative_depth} (;{label};)")?;
        Ok(OpKind::Delegate)
    }
    fn visit_catch_all(&mut self, _pos: usize) -> Self::Output {
        self.push_str("catch_all");
        Ok(OpKind::BlockMid)
    }
    fn visit_end(&mut self, _pos: usize) -> Self::Output {
        self.push_str("end");
        Ok(OpKind::End)
    }
    fn visit_br(&mut self, _pos: usize, relative_depth: u32) -> Self::Output {
        let label = self.label(relative_depth);
        write!(self.result(), "br {relative_depth} (;{label};)")?;
        Ok(OpKind::Normal)
    }
    fn visit_br_if(&mut self, _pos: usize, relative_depth: u32) -> Self::Output {
        let label = self.label(relative_depth);
        write!(self.result(), "br_if {relative_depth} (;{label};)")?;
        Ok(OpKind::Normal)
    }
    fn visit_br_table(&mut self, _pos: usize, table: BrTable<'a>) -> Self::Output {
        self.push_str("br_table");
        for item in table.targets().chain(Some(Ok(table.default()))) {
            let item = item?;
            let label = self.label(item);
            write!(self.result(), " {item} (;{label};)")?;
        }
        Ok(OpKind::Normal)
    }
    fn visit_return(&mut self, _pos: usize) -> Self::Output {
        self.instr("return")
    }
    fn visit_call(&mut self, _pos: usize, function_index: u32) -> Self::Output {
        self.push_str("call ");
        self.print_func_idx(function_index)?;
        Ok(OpKind::Normal)
    }
    fn visit_return_call(&mut self, _pos: usize, function_index: u32) -> Self::Output {
        self.push_str("return_call ");
        self.print_func_idx(function_index)?;
        Ok(OpKind::Normal)
    }
    fn visit_call_indirect(
        &mut self,
        _pos: usize,
        index: u32,
        table_index: u32,
        _table_byte: u8,
    ) -> Self::Output {
        self.push_str("call_indirect");
        if table_index != 0 {
            self.push_str(" ");
            self.print_table_idx(table_index)?;
        }
        self.print_type_ref(index)?;
        Ok(OpKind::Normal)
    }
    fn visit_return_call_indirect(
        &mut self,
        _pos: usize,
        index: u32,
        table_index: u32,
    ) -> Self::Output {
        self.push_str("return_call_indirect");
        if table_index != 0 {
            self.push_str(" ");
            self.print_table_idx(table_index)?;
        }
        self.print_type_ref(index)?;
        Ok(OpKind::Normal)
    }

    fn visit_drop(&mut self, _pos: usize) -> Self::Output {
        self.instr("drop")
    }
    fn visit_select(&mut self, _pos: usize) -> Self::Output {
        self.instr("select")
    }
    fn visit_typed_select(&mut self, _pos: usize, ty: ValType) -> Self::Output {
        self.push_str("select (result ");
        self.print_valtype(ty)?;
        self.instr(")")
    }

    fn visit_ref_null(&mut self, _pos: usize, ty: ValType) -> Self::Output {
        self.push_str("ref.null ");
        self.printer.print_reftype(ty)?;
        Ok(OpKind::Normal)
    }
    fn visit_ref_is_null(&mut self, _pos: usize) -> Self::Output {
        self.instr("ref.is_null")
    }
    fn visit_ref_func(&mut self, _pos: usize, function_index: u32) -> Self::Output {
        self.push_str("ref.func ");
        self.print_func_idx(function_index)?;
        Ok(OpKind::Normal)
    }

    fn visit_local_get(&mut self, _pos: usize, local_index: u32) -> Self::Output {
        self.push_str("local.get ");
        self.print_local_idx(local_index)?;
        Ok(OpKind::Normal)
    }
    fn visit_local_set(&mut self, _pos: usize, local_index: u32) -> Self::Output {
        self.push_str("local.set ");
        self.print_local_idx(local_index)?;
        Ok(OpKind::Normal)
    }
    fn visit_local_tee(&mut self, _pos: usize, local_index: u32) -> Self::Output {
        self.push_str("local.tee ");
        self.print_local_idx(local_index)?;
        Ok(OpKind::Normal)
    }
    fn visit_global_get(&mut self, _pos: usize, global_index: u32) -> Self::Output {
        self.push_str("global.get ");
        self.print_global_idx(global_index)?;
        Ok(OpKind::Normal)
    }
    fn visit_global_set(&mut self, _pos: usize, global_index: u32) -> Self::Output {
        self.push_str("global.set ");
        self.print_global_idx(global_index)?;
        Ok(OpKind::Normal)
    }
    fn visit_table_get(&mut self, _pos: usize, table: u32) -> Self::Output {
        self.push_str("table.get ");
        self.print_table_idx(table)?;
        Ok(OpKind::Normal)
    }
    fn visit_table_set(&mut self, _pos: usize, table: u32) -> Self::Output {
        self.push_str("table.set ");
        self.print_table_idx(table)?;
        Ok(OpKind::Normal)
    }
    fn visit_table_init(&mut self, _pos: usize, segment: u32, table: u32) -> Self::Output {
        self.push_str("table.init ");
        if table != 0 {
            self.print_table_idx(table)?;
            self.push_str(" ");
        }
        self.print_element_idx(segment)?;
        Ok(OpKind::Normal)
    }
    fn visit_elem_drop(&mut self, _pos: usize, segment: u32) -> Self::Output {
        self.push_str("elem.drop ");
        self.print_element_idx(segment)?;
        Ok(OpKind::Normal)
    }
    fn visit_table_copy(&mut self, _pos: usize, dst: u32, src: u32) -> Self::Output {
        self.push_str("table.copy");
        if dst != 0 || src != 0 {
            self.push_str(" ");
            self.print_table_idx(dst)?;
            self.push_str(" ");
            self.print_table_idx(src)?;
        }
        Ok(OpKind::Normal)
    }
    fn visit_table_grow(&mut self, _pos: usize, table: u32) -> Self::Output {
        self.push_str("table.grow ");
        self.print_table_idx(table)?;
        Ok(OpKind::Normal)
    }
    fn visit_table_size(&mut self, _pos: usize, table: u32) -> Self::Output {
        self.push_str("table.size ");
        self.print_table_idx(table)?;
        Ok(OpKind::Normal)
    }
    fn visit_table_fill(&mut self, _pos: usize, table: u32) -> Self::Output {
        self.push_str("table.fill ");
        self.print_table_idx(table)?;
        Ok(OpKind::Normal)
    }
    fn visit_i32_load(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.load", &memarg, 4)
    }
    fn visit_i64_load(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.load", &memarg, 8)
    }
    fn visit_f32_load(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("f32.load", &memarg, 4)
    }
    fn visit_f64_load(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("f64.load", &memarg, 8)
    }
    fn visit_i32_load8_s(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.load8_s", &memarg, 1)
    }
    fn visit_i32_load8_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.load8_u", &memarg, 1)
    }
    fn visit_i32_load16_s(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.load16_s", &memarg, 2)
    }
    fn visit_i32_load16_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.load16_u", &memarg, 2)
    }
    fn visit_i64_load8_s(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.load8_s", &memarg, 1)
    }
    fn visit_i64_load8_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.load8_u", &memarg, 1)
    }
    fn visit_i64_load16_s(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.load16_s", &memarg, 2)
    }
    fn visit_i64_load16_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.load16_u", &memarg, 2)
    }
    fn visit_i64_load32_s(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.load32_s", &memarg, 4)
    }
    fn visit_i64_load32_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.load32_u", &memarg, 4)
    }
    fn visit_i32_store(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.store", &memarg, 4)
    }
    fn visit_i64_store(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.store", &memarg, 8)
    }
    fn visit_f32_store(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("f32.store", &memarg, 4)
    }
    fn visit_f64_store(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("f64.store", &memarg, 8)
    }
    fn visit_i32_store8(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.store8", &memarg, 1)
    }
    fn visit_i32_store16(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.store16", &memarg, 2)
    }
    fn visit_i64_store8(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.store8", &memarg, 1)
    }
    fn visit_i64_store16(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.store16", &memarg, 2)
    }
    fn visit_i64_store32(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.store32", &memarg, 4)
    }
    fn visit_memory_size(&mut self, _pos: usize, mem: u32, _mem_byte: u8) -> Self::Output {
        self.push_str("memory.size");
        if mem != 0 {
            self.push_str(" ");
            self.print_memory_idx(mem)?;
        }
        Ok(OpKind::Normal)
    }
    fn visit_memory_grow(&mut self, _pos: usize, mem: u32, _mem_byte: u8) -> Self::Output {
        self.push_str("memory.grow");
        if mem != 0 {
            self.push_str(" ");
            self.print_memory_idx(mem)?;
        }
        Ok(OpKind::Normal)
    }
    fn visit_memory_init(&mut self, _pos: usize, segment: u32, mem: u32) -> Self::Output {
        self.push_str("memory.init ");
        if mem != 0 {
            self.print_memory_idx(mem)?;
            self.push_str(" ");
        }
        self.print_data_idx(segment)?;
        Ok(OpKind::Normal)
    }
    fn visit_data_drop(&mut self, _pos: usize, segment: u32) -> Self::Output {
        self.push_str("data.drop ");
        self.print_data_idx(segment)?;
        Ok(OpKind::Normal)
    }
    fn visit_memory_copy(&mut self, _pos: usize, dst: u32, src: u32) -> Self::Output {
        self.push_str("memory.copy");
        if dst != 0 || src != 0 {
            self.push_str(" ");
            self.print_memory_idx(dst)?;
            self.push_str(" ");
            self.print_memory_idx(src)?;
        }
        Ok(OpKind::Normal)
    }
    fn visit_memory_fill(&mut self, _pos: usize, mem: u32) -> Self::Output {
        self.push_str("memory.fill");
        if mem != 0 {
            self.push_str(" ");
            self.print_memory_idx(mem)?;
        }
        Ok(OpKind::Normal)
    }
    fn visit_i32_const(&mut self, _pos: usize, value: i32) -> Self::Output {
        write!(self.result(), "i32.const {value}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i64_const(&mut self, _pos: usize, value: i64) -> Self::Output {
        write!(self.result(), "i64.const {value}")?;
        Ok(OpKind::Normal)
    }
    fn visit_f32_const(&mut self, _pos: usize, value: Ieee32) -> Self::Output {
        self.push_str("f32.const ");
        self.printer.print_f32(value.bits())?;
        Ok(OpKind::Normal)
    }
    fn visit_f64_const(&mut self, _pos: usize, value: Ieee64) -> Self::Output {
        self.push_str("f64.const ");
        self.printer.print_f64(value.bits())?;
        Ok(OpKind::Normal)
    }
    fn visit_i32_eqz(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.eqz")
    }
    fn visit_i32_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.eq")
    }
    fn visit_i32_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.ne")
    }
    fn visit_i32_lt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.lt_s")
    }
    fn visit_i32_lt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.lt_u")
    }
    fn visit_i32_gt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.gt_s")
    }
    fn visit_i32_gt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.gt_u")
    }
    fn visit_i32_le_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.le_s")
    }
    fn visit_i32_le_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.le_u")
    }
    fn visit_i32_ge_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.ge_s")
    }
    fn visit_i32_ge_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.ge_u")
    }
    fn visit_i64_eqz(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.eqz")
    }
    fn visit_i64_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.eq")
    }
    fn visit_i64_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.ne")
    }
    fn visit_i64_lt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.lt_s")
    }
    fn visit_i64_lt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.lt_u")
    }
    fn visit_i64_gt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.gt_s")
    }
    fn visit_i64_gt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.gt_u")
    }
    fn visit_i64_le_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.le_s")
    }
    fn visit_i64_le_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.le_u")
    }
    fn visit_i64_ge_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.ge_s")
    }
    fn visit_i64_ge_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.ge_u")
    }
    fn visit_f32_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.eq")
    }
    fn visit_f32_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.ne")
    }
    fn visit_f32_lt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.lt")
    }
    fn visit_f32_gt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.gt")
    }
    fn visit_f32_le(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.le")
    }
    fn visit_f32_ge(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.ge")
    }
    fn visit_f64_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.eq")
    }
    fn visit_f64_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.ne")
    }
    fn visit_f64_lt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.lt")
    }
    fn visit_f64_gt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.gt")
    }
    fn visit_f64_le(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.le")
    }
    fn visit_f64_ge(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.ge")
    }
    fn visit_i32_clz(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.clz")
    }
    fn visit_i32_ctz(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.ctz")
    }
    fn visit_i32_popcnt(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.popcnt")
    }
    fn visit_i32_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.add")
    }
    fn visit_i32_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.sub")
    }
    fn visit_i32_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.mul")
    }
    fn visit_i32_div_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.div_s")
    }
    fn visit_i32_div_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.div_u")
    }
    fn visit_i32_rem_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.rem_s")
    }
    fn visit_i32_rem_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.rem_u")
    }
    fn visit_i32_and(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.and")
    }
    fn visit_i32_or(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.or")
    }
    fn visit_i32_xor(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.xor")
    }
    fn visit_i32_shl(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.shl")
    }
    fn visit_i32_shr_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.shr_s")
    }
    fn visit_i32_shr_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.shr_u")
    }
    fn visit_i32_rotl(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.rotl")
    }
    fn visit_i32_rotr(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.rotr")
    }

    fn visit_i64_clz(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.clz")
    }
    fn visit_i64_ctz(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.ctz")
    }
    fn visit_i64_popcnt(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.popcnt")
    }
    fn visit_i64_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.add")
    }
    fn visit_i64_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.sub")
    }
    fn visit_i64_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.mul")
    }
    fn visit_i64_div_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.div_s")
    }
    fn visit_i64_div_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.div_u")
    }
    fn visit_i64_rem_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.rem_s")
    }
    fn visit_i64_rem_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.rem_u")
    }
    fn visit_i64_and(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.and")
    }
    fn visit_i64_or(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.or")
    }
    fn visit_i64_xor(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.xor")
    }
    fn visit_i64_shl(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.shl")
    }
    fn visit_i64_shr_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.shr_s")
    }
    fn visit_i64_shr_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.shr_u")
    }
    fn visit_i64_rotl(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.rotl")
    }
    fn visit_i64_rotr(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.rotr")
    }

    fn visit_f32_abs(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.abs")
    }
    fn visit_f32_neg(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.neg")
    }
    fn visit_f32_ceil(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.ceil")
    }
    fn visit_f32_floor(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.floor")
    }
    fn visit_f32_trunc(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.trunc")
    }
    fn visit_f32_nearest(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.nearest")
    }
    fn visit_f32_sqrt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.sqrt")
    }
    fn visit_f32_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.add")
    }
    fn visit_f32_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.sub")
    }
    fn visit_f32_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.mul")
    }
    fn visit_f32_div(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.div")
    }
    fn visit_f32_min(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.min")
    }
    fn visit_f32_max(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.max")
    }
    fn visit_f32_copysign(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.copysign")
    }

    fn visit_f64_abs(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.abs")
    }
    fn visit_f64_neg(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.neg")
    }
    fn visit_f64_ceil(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.ceil")
    }
    fn visit_f64_floor(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.floor")
    }
    fn visit_f64_trunc(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.trunc")
    }
    fn visit_f64_nearest(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.nearest")
    }
    fn visit_f64_sqrt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.sqrt")
    }
    fn visit_f64_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.add")
    }
    fn visit_f64_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.sub")
    }
    fn visit_f64_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.mul")
    }
    fn visit_f64_div(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.div")
    }
    fn visit_f64_min(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.min")
    }
    fn visit_f64_max(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.max")
    }
    fn visit_f64_copysign(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.copysign")
    }

    fn visit_i32_wrap_i64(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.wrap_i64")
    }
    fn visit_i32_trunc_f32s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.trunc_f32_s")
    }
    fn visit_i32_trunc_f32u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.trunc_f32_u")
    }
    fn visit_i32_trunc_f64s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.trunc_f64_s")
    }
    fn visit_i32_trunc_f64u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.trunc_f64_u")
    }
    fn visit_i64_extend_i32s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.extend_i32_s")
    }
    fn visit_i64_extend_i32u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.extend_i32_u")
    }
    fn visit_i64_trunc_f32s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.trunc_f32_s")
    }
    fn visit_i64_trunc_f32u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.trunc_f32_u")
    }
    fn visit_i64_trunc_f64s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.trunc_f64_s")
    }
    fn visit_i64_trunc_f64u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.trunc_f64_u")
    }

    fn visit_f32_convert_i32s(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.convert_i32_s")
    }
    fn visit_f32_convert_i32u(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.convert_i32_u")
    }
    fn visit_f32_convert_i64s(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.convert_i64_s")
    }
    fn visit_f32_convert_i64u(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.convert_i64_u")
    }
    fn visit_f32_demote_f64(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.demote_f64")
    }
    fn visit_f64_convert_i32_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.convert_i32_s")
    }
    fn visit_f64_convert_i32_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.convert_i32_u")
    }
    fn visit_f64_convert_i64_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.convert_i64_s")
    }
    fn visit_f64_convert_i64_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.convert_i64_u")
    }
    fn visit_f64_promote_f32(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.promote_f32")
    }
    fn visit_i32_reinterpret_f32(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.reinterpret_f32")
    }
    fn visit_i64_reinterpret_f64(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.reinterpret_f64")
    }
    fn visit_f32_reinterpret_i32(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32.reinterpret_i32")
    }
    fn visit_f64_reinterpret_i64(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64.reinterpret_i64")
    }
    fn visit_i32_extend8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.extend8_s")
    }
    fn visit_i32_extend16_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.extend16_s")
    }
    fn visit_i64_extend8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.extend8_s")
    }
    fn visit_i64_extend16_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.extend16_s")
    }
    fn visit_i64_extend32_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.extend32_s")
    }

    fn visit_i32_trunc_sat_f32_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.trunc_sat_f32_s")
    }
    fn visit_i32_trunc_sat_f32_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.trunc_sat_f32_u")
    }
    fn visit_i32_trunc_sat_f64_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.trunc_sat_f64_s")
    }
    fn visit_i32_trunc_sat_f64_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32.trunc_sat_f64_u")
    }
    fn visit_i64_trunc_sat_f32_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.trunc_sat_f32_s")
    }
    fn visit_i64_trunc_sat_f32_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.trunc_sat_f32_u")
    }
    fn visit_i64_trunc_sat_f64_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.trunc_sat_f64_s")
    }
    fn visit_i64_trunc_sat_f64_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64.trunc_sat_f64_u")
    }

    fn visit_memory_atomic_notify(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("memory.atomic.notify", &memarg, 4)
    }
    fn visit_memory_atomic_wait32(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("memory.atomic.wait32", &memarg, 4)
    }
    fn visit_memory_atomic_wait64(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("memory.atomic.wait64", &memarg, 8)
    }

    fn visit_i32_atomic_load(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.load", &memarg, 4)
    }
    fn visit_i64_atomic_load(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.load", &memarg, 8)
    }
    fn visit_i32_atomic_load8_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.load8_u", &memarg, 1)
    }
    fn visit_i32_atomic_load16_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.load16_u", &memarg, 2)
    }
    fn visit_i64_atomic_load8_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.load8_u", &memarg, 1)
    }
    fn visit_i64_atomic_load16_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.load16_u", &memarg, 2)
    }
    fn visit_i64_atomic_load32_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.load32_u", &memarg, 4)
    }
    fn visit_i32_atomic_store(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.store", &memarg, 4)
    }
    fn visit_i32_atomic_store8(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.store8", &memarg, 1)
    }
    fn visit_i32_atomic_store16(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.store16", &memarg, 2)
    }
    fn visit_i64_atomic_store(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.store", &memarg, 8)
    }
    fn visit_i64_atomic_store8(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.store8", &memarg, 1)
    }
    fn visit_i64_atomic_store16(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.store16", &memarg, 2)
    }
    fn visit_i64_atomic_store32(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.store32", &memarg, 4)
    }

    fn visit_i32_atomic_rmw_add(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw.add", &memarg, 4)
    }
    fn visit_i32_atomic_rmw8_add_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw8.add_u", &memarg, 1)
    }
    fn visit_i32_atomic_rmw16_add_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw16.add_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw_add(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw.add", &memarg, 8)
    }
    fn visit_i64_atomic_rmw8_add_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw8.add_u", &memarg, 1)
    }
    fn visit_i64_atomic_rmw16_add_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw16.add_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw32_add_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw32.add_u", &memarg, 4)
    }

    fn visit_i32_atomic_rmw_sub(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw.sub", &memarg, 4)
    }
    fn visit_i32_atomic_rmw8_sub_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw8.sub_u", &memarg, 1)
    }
    fn visit_i32_atomic_rmw16_sub_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw16.sub_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw_sub(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw.sub", &memarg, 8)
    }
    fn visit_i64_atomic_rmw8_sub_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw8.sub_u", &memarg, 1)
    }
    fn visit_i64_atomic_rmw16_sub_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw16.sub_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw32_sub_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw32.sub_u", &memarg, 4)
    }

    fn visit_i32_atomic_rmw_and(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw.and", &memarg, 4)
    }
    fn visit_i32_atomic_rmw8_and_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw8.and_u", &memarg, 1)
    }
    fn visit_i32_atomic_rmw16_and_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw16.and_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw_and(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw.and", &memarg, 8)
    }
    fn visit_i64_atomic_rmw8_and_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw8.and_u", &memarg, 1)
    }
    fn visit_i64_atomic_rmw16_and_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw16.and_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw32_and_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw32.and_u", &memarg, 4)
    }

    fn visit_i32_atomic_rmw_or(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw.or", &memarg, 4)
    }
    fn visit_i32_atomic_rmw8_or_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw8.or_u", &memarg, 1)
    }
    fn visit_i32_atomic_rmw16_or_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw16.or_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw_or(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw.or", &memarg, 8)
    }
    fn visit_i64_atomic_rmw8_or_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw8.or_u", &memarg, 1)
    }
    fn visit_i64_atomic_rmw16_or_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw16.or_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw32_or_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw32.or_u", &memarg, 4)
    }

    fn visit_i32_atomic_rmw_xor(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw.xor", &memarg, 4)
    }
    fn visit_i32_atomic_rmw8_xor_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw8.xor_u", &memarg, 1)
    }
    fn visit_i32_atomic_rmw16_xor_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw16.xor_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw_xor(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw.xor", &memarg, 8)
    }
    fn visit_i64_atomic_rmw8_xor_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw8.xor_u", &memarg, 1)
    }
    fn visit_i64_atomic_rmw16_xor_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw16.xor_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw32_xor_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw32.xor_u", &memarg, 4)
    }

    fn visit_i32_atomic_rmw_xchg(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw.xchg", &memarg, 4)
    }
    fn visit_i32_atomic_rmw8_xchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw8.xchg_u", &memarg, 1)
    }
    fn visit_i32_atomic_rmw16_xchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw16.xchg_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw_xchg(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw.xchg", &memarg, 8)
    }
    fn visit_i64_atomic_rmw8_xchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw8.xchg_u", &memarg, 1)
    }
    fn visit_i64_atomic_rmw16_xchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw16.xchg_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw32_xchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw32.xchg_u", &memarg, 4)
    }

    fn visit_i32_atomic_rmw_cmpxchg(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw.cmpxchg", &memarg, 4)
    }
    fn visit_i32_atomic_rmw8_cmpxchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw8.cmpxchg_u", &memarg, 1)
    }
    fn visit_i32_atomic_rmw16_cmpxchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i32.atomic.rmw16.cmpxchg_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw_cmpxchg(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw.cmpxchg", &memarg, 8)
    }
    fn visit_i64_atomic_rmw8_cmpxchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw8.cmpxchg_u", &memarg, 1)
    }
    fn visit_i64_atomic_rmw16_cmpxchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw16.cmpxchg_u", &memarg, 2)
    }
    fn visit_i64_atomic_rmw32_cmpxchg_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("i64.atomic.rmw32.cmpxchg_u", &memarg, 4)
    }

    fn visit_atomic_fence(&mut self, _pos: usize, _flags: u8) -> Self::Output {
        self.instr("atomic.fence")
    }

    fn visit_v128_load(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load", &memarg, 16)
    }
    fn visit_v128_store(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.store", &memarg, 16)
    }
    fn visit_v128_const(&mut self, _pos: usize, value: V128) -> Self::Output {
        write!(self.result(), "v128.const i32x4")?;
        for chunk in value.bytes().chunks(4) {
            write!(
                self.result(),
                " 0x{:02x}{:02x}{:02x}{:02x}",
                chunk[3],
                chunk[2],
                chunk[1],
                chunk[0],
            )?;
        }
        Ok(OpKind::Normal)
    }
    fn visit_i8x16_splat(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.splat")
    }
    fn visit_i16x8_splat(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.splat")
    }
    fn visit_i32x4_splat(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.splat")
    }
    fn visit_i64x2_splat(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.splat")
    }
    fn visit_f32x4_splat(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.splat")
    }
    fn visit_f64x2_splat(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.splat")
    }
    fn visit_i8x16_extract_lane_s(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i8x16.extract_lane_s {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i8x16_extract_lane_u(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i8x16.extract_lane_u {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i16x8_extract_lane_s(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i16x8.extract_lane_s {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i16x8_extract_lane_u(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i16x8.extract_lane_u {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i32x4_extract_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i32x4.extract_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i64x2_extract_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i64x2.extract_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i8x16_replace_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i8x16.replace_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i16x8_replace_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i16x8.replace_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i32x4_replace_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i32x4.replace_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_i64x2_replace_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "i64x2.replace_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_f32x4_extract_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "f32x4.extract_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_f32x4_replace_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "f32x4.replace_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_f64x2_extract_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "f64x2.extract_lane {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_f64x2_replace_lane(&mut self, _pos: usize, lane: u8) -> Self::Output {
        write!(self.result(), "f64x2.replace_lane {lane}")?;
        Ok(OpKind::Normal)
    }

    fn visit_f32x4_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.eq")
    }
    fn visit_f32x4_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.ne")
    }
    fn visit_f32x4_lt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.lt")
    }
    fn visit_f32x4_gt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.gt")
    }
    fn visit_f32x4_le(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.le")
    }
    fn visit_f32x4_ge(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.ge")
    }

    fn visit_f64x2_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.eq")
    }
    fn visit_f64x2_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.ne")
    }
    fn visit_f64x2_lt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.lt")
    }
    fn visit_f64x2_gt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.gt")
    }
    fn visit_f64x2_le(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.le")
    }
    fn visit_f64x2_ge(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.ge")
    }

    fn visit_f32x4_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.add")
    }
    fn visit_f32x4_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.sub")
    }
    fn visit_f32x4_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.mul")
    }
    fn visit_f32x4_div(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.div")
    }
    fn visit_f32x4_min(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.min")
    }
    fn visit_f32x4_max(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.max")
    }
    fn visit_f32x4_pmin(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.pmin")
    }
    fn visit_f32x4_pmax(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.pmax")
    }

    fn visit_f64x2_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.add")
    }
    fn visit_f64x2_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.sub")
    }
    fn visit_f64x2_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.mul")
    }
    fn visit_f64x2_div(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.div")
    }
    fn visit_f64x2_min(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.min")
    }
    fn visit_f64x2_max(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.max")
    }
    fn visit_f64x2_pmin(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.pmin")
    }
    fn visit_f64x2_pmax(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.pmax")
    }

    fn visit_f32x4_relaxed_min(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.relaxed_min")
    }
    fn visit_f32x4_relaxed_max(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.relaxed_max")
    }
    fn visit_f64x2_relaxed_min(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.relaxed_min")
    }
    fn visit_f64x2_relaxed_max(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.relaxed_max")
    }

    fn visit_i8x16_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.eq")
    }
    fn visit_i8x16_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.ne")
    }
    fn visit_i8x16_lt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.lt_s")
    }
    fn visit_i8x16_lt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.lt_u")
    }
    fn visit_i8x16_gt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.gt_s")
    }
    fn visit_i8x16_gt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.gt_u")
    }
    fn visit_i8x16_le_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.le_s")
    }
    fn visit_i8x16_le_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.le_u")
    }
    fn visit_i8x16_ge_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.ge_s")
    }
    fn visit_i8x16_ge_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.ge_u")
    }

    fn visit_i16x8_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.eq")
    }
    fn visit_i16x8_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.ne")
    }
    fn visit_i16x8_lt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.lt_s")
    }
    fn visit_i16x8_lt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.lt_u")
    }
    fn visit_i16x8_gt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.gt_s")
    }
    fn visit_i16x8_gt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.gt_u")
    }
    fn visit_i16x8_le_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.le_s")
    }
    fn visit_i16x8_le_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.le_u")
    }
    fn visit_i16x8_ge_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.ge_s")
    }
    fn visit_i16x8_ge_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.ge_u")
    }

    fn visit_i32x4_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.eq")
    }
    fn visit_i32x4_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.ne")
    }
    fn visit_i32x4_lt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.lt_s")
    }
    fn visit_i32x4_lt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.lt_u")
    }
    fn visit_i32x4_gt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.gt_s")
    }
    fn visit_i32x4_gt_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.gt_u")
    }
    fn visit_i32x4_le_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.le_s")
    }
    fn visit_i32x4_le_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.le_u")
    }
    fn visit_i32x4_ge_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.ge_s")
    }
    fn visit_i32x4_ge_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.ge_u")
    }

    fn visit_i64x2_eq(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.eq")
    }
    fn visit_i64x2_ne(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.ne")
    }
    fn visit_i64x2_lt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.lt_s")
    }
    fn visit_i64x2_gt_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.gt_s")
    }
    fn visit_i64x2_le_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.le_s")
    }
    fn visit_i64x2_ge_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.ge_s")
    }

    fn visit_v128_and(&mut self, _pos: usize) -> Self::Output {
        self.instr("v128.and")
    }
    fn visit_v128_andnot(&mut self, _pos: usize) -> Self::Output {
        self.instr("v128.andnot")
    }
    fn visit_v128_or(&mut self, _pos: usize) -> Self::Output {
        self.instr("v128.or")
    }
    fn visit_v128_xor(&mut self, _pos: usize) -> Self::Output {
        self.instr("v128.xor")
    }

    fn visit_i8x16_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.add")
    }
    fn visit_i8x16_add_sat_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.add_sat_s")
    }
    fn visit_i8x16_add_sat_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.add_sat_u")
    }
    fn visit_i8x16_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.sub")
    }
    fn visit_i8x16_sub_sat_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.sub_sat_s")
    }
    fn visit_i8x16_sub_sat_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.sub_sat_u")
    }
    fn visit_i8x16_min_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.min_s")
    }
    fn visit_i8x16_min_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.min_u")
    }
    fn visit_i8x16_max_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.max_s")
    }
    fn visit_i8x16_max_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.max_u")
    }

    fn visit_i16x8_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.add")
    }
    fn visit_i16x8_add_sat_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.add_sat_s")
    }
    fn visit_i16x8_add_sat_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.add_sat_u")
    }
    fn visit_i16x8_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.sub")
    }
    fn visit_i16x8_sub_sat_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.sub_sat_s")
    }
    fn visit_i16x8_sub_sat_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.sub_sat_u")
    }
    fn visit_i16x8_min_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.min_s")
    }
    fn visit_i16x8_min_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.min_u")
    }
    fn visit_i16x8_max_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.max_s")
    }
    fn visit_i16x8_max_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.max_u")
    }
    fn visit_i16x8_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.mul")
    }

    fn visit_i32x4_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.add")
    }
    fn visit_i32x4_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.sub")
    }
    fn visit_i32x4_min_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.min_s")
    }
    fn visit_i32x4_min_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.min_u")
    }
    fn visit_i32x4_max_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.max_s")
    }
    fn visit_i32x4_max_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.max_u")
    }
    fn visit_i32x4_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.mul")
    }
    fn visit_i32x4_dot_i16x8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.dot_i16x8_s")
    }

    fn visit_i64x2_add(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.add")
    }
    fn visit_i64x2_sub(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.sub")
    }
    fn visit_i64x2_mul(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.mul")
    }

    fn visit_i8x16_avgr_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.avgr_u")
    }
    fn visit_i16x8_avgr_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.avgr_u")
    }
    fn visit_i8x16_narrow_i16x8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.narrow_i16x8_s")
    }
    fn visit_i8x16_narrow_i16x8_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.narrow_i16x8_u")
    }
    fn visit_i16x8_narrow_i32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.narrow_i32x4_s")
    }
    fn visit_i16x8_narrow_i32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.narrow_i32x4_u")
    }
    fn visit_i16x8_extmul_low_i8x16_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extmul_low_i8x16_s")
    }
    fn visit_i16x8_extmul_high_i8x16_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extmul_high_i8x16_s")
    }
    fn visit_i16x8_extmul_low_i8x16_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extmul_low_i8x16_u")
    }
    fn visit_i16x8_extmul_high_i8x16_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extmul_high_i8x16_u")
    }
    fn visit_i32x4_extmul_low_i16x8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extmul_low_i16x8_s")
    }
    fn visit_i32x4_extmul_high_i16x8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extmul_high_i16x8_s")
    }
    fn visit_i32x4_extmul_low_i16x8_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extmul_low_i16x8_u")
    }
    fn visit_i32x4_extmul_high_i16x8_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extmul_high_i16x8_u")
    }
    fn visit_i64x2_extmul_low_i32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.extmul_low_i32x4_s")
    }
    fn visit_i64x2_extmul_high_i32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.extmul_high_i32x4_s")
    }
    fn visit_i64x2_extmul_low_i32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.extmul_low_i32x4_u")
    }
    fn visit_i64x2_extmul_high_i32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.extmul_high_i32x4_u")
    }
    fn visit_i16x8_q15mulr_sat_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.q15mulr_sat_s")
    }

    fn visit_f32x4_ceil(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.ceil")
    }
    fn visit_f32x4_floor(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.floor")
    }
    fn visit_f32x4_trunc(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.trunc")
    }
    fn visit_f32x4_nearest(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.nearest")
    }

    fn visit_f64x2_ceil(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.ceil")
    }
    fn visit_f64x2_floor(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.floor")
    }
    fn visit_f64x2_trunc(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.trunc")
    }
    fn visit_f64x2_nearest(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.nearest")
    }

    fn visit_f32x4_abs(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.abs")
    }
    fn visit_f32x4_neg(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.neg")
    }
    fn visit_f32x4_sqrt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.sqrt")
    }
    fn visit_f64x2_abs(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.abs")
    }
    fn visit_f64x2_neg(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.neg")
    }
    fn visit_f64x2_sqrt(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.sqrt")
    }

    fn visit_f32x4_demote_f64x2_zero(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.demote_f64x2_zero")
    }
    fn visit_f64x2_promote_low_f32x4(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.promote_low_f32x4")
    }
    fn visit_f64x2_convert_low_i32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.convert_low_i32x4_s")
    }
    fn visit_f64x2_convert_low_i32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.convert_low_i32x4_u")
    }
    fn visit_i32x4_trunc_sat_f32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.trunc_sat_f32x4_s")
    }
    fn visit_i32x4_trunc_sat_f32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.trunc_sat_f32x4_u")
    }
    fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.trunc_sat_f64x2_s_zero")
    }
    fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.trunc_sat_f64x2_u_zero")
    }
    fn visit_f32x4_convert_i32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.convert_i32x4_s")
    }
    fn visit_f32x4_convert_i32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.convert_i32x4_u")
    }
    fn visit_v128_not(&mut self, _pos: usize) -> Self::Output {
        self.instr("v128.not")
    }
    fn visit_i8x16_abs(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.abs")
    }
    fn visit_i8x16_neg(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.neg")
    }
    fn visit_i8x16_popcnt(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.popcnt")
    }
    fn visit_i16x8_abs(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.abs")
    }
    fn visit_i16x8_neg(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.neg")
    }
    fn visit_i32x4_abs(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.abs")
    }
    fn visit_i32x4_neg(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.neg")
    }
    fn visit_i64x2_abs(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.abs")
    }
    fn visit_i64x2_neg(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.neg")
    }
    fn visit_i16x8_extend_low_i8x16_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extend_low_i8x16_s")
    }
    fn visit_i16x8_extend_high_i8x16_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extend_high_i8x16_s")
    }
    fn visit_i16x8_extend_low_i8x16_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extend_low_i8x16_u")
    }
    fn visit_i16x8_extend_high_i8x16_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extend_high_i8x16_u")
    }
    fn visit_i32x4_extend_low_i16x8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extend_low_i16x8_s")
    }
    fn visit_i32x4_extend_high_i16x8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extend_high_i16x8_s")
    }
    fn visit_i32x4_extend_low_i16x8_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extend_low_i16x8_u")
    }
    fn visit_i32x4_extend_high_i16x8_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extend_high_i16x8_u")
    }
    fn visit_i64x2_extend_low_i32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.extend_low_i32x4_s")
    }
    fn visit_i64x2_extend_high_i32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.extend_high_i32x4_s")
    }
    fn visit_i64x2_extend_low_i32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.extend_low_i32x4_u")
    }
    fn visit_i64x2_extend_high_i32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.extend_high_i32x4_u")
    }
    fn visit_i16x8_extadd_pairwise_i8x16_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extadd_pairwise_i8x16_s")
    }
    fn visit_i16x8_extadd_pairwise_i8x16_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.extadd_pairwise_i8x16_u")
    }
    fn visit_i32x4_extadd_pairwise_i16x8_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extadd_pairwise_i16x8_s")
    }
    fn visit_i32x4_extadd_pairwise_i16x8_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.extadd_pairwise_i16x8_u")
    }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.relaxed_trunc_f32x4_s")
    }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.relaxed_trunc_f32x4_u")
    }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.relaxed_trunc_f64x2_s_zero")
    }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.relaxed_trunc_f64x2_u_zero")
    }
    fn visit_v128_bitselect(&mut self, _pos: usize) -> Self::Output {
        self.instr("v128.bitselect")
    }
    fn visit_f32x4_fma(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.fma")
    }
    fn visit_f32x4_fms(&mut self, _pos: usize) -> Self::Output {
        self.instr("f32x4.fms")
    }
    fn visit_f64x2_fma(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.fma")
    }
    fn visit_f64x2_fms(&mut self, _pos: usize) -> Self::Output {
        self.instr("f64x2.fms")
    }
    fn visit_i8x16_laneselect(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.laneselect")
    }
    fn visit_i16x8_laneselect(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.laneselect")
    }
    fn visit_i32x4_laneselect(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.laneselect")
    }
    fn visit_i64x2_laneselect(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.laneselect")
    }
    fn visit_v128_any_true(&mut self, _pos: usize) -> Self::Output {
        self.instr("v128.any_true")
    }
    fn visit_i8x16_all_true(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.all_true")
    }
    fn visit_i8x16_bitmask(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.bitmask")
    }
    fn visit_i16x8_all_true(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.all_true")
    }
    fn visit_i16x8_bitmask(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.bitmask")
    }
    fn visit_i32x4_all_true(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.all_true")
    }
    fn visit_i32x4_bitmask(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.bitmask")
    }
    fn visit_i64x2_all_true(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.all_true")
    }
    fn visit_i64x2_bitmask(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.bitmask")
    }
    fn visit_i8x16_shl(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.shl")
    }
    fn visit_i8x16_shr_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.shr_s")
    }
    fn visit_i8x16_shr_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.shr_u")
    }
    fn visit_i16x8_shl(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.shl")
    }
    fn visit_i16x8_shr_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.shr_s")
    }
    fn visit_i16x8_shr_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i16x8.shr_u")
    }
    fn visit_i32x4_shl(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.shl")
    }
    fn visit_i32x4_shr_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.shr_s")
    }
    fn visit_i32x4_shr_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i32x4.shr_u")
    }
    fn visit_i64x2_shl(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.shl")
    }
    fn visit_i64x2_shr_s(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.shr_s")
    }
    fn visit_i64x2_shr_u(&mut self, _pos: usize) -> Self::Output {
        self.instr("i64x2.shr_u")
    }

    fn visit_i8x16_swizzle(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.swizzle")
    }
    fn visit_i8x16_relaxed_swizzle(&mut self, _pos: usize) -> Self::Output {
        self.instr("i8x16.relaxed_swizzle")
    }
    fn visit_i8x16_shuffle(&mut self, _pos: usize, lanes: [u8; 16]) -> Self::Output {
        self.push_str("i8x16.shuffle");
        for lane in lanes {
            write!(self.result(), " {}", lane)?;
        }
        Ok(OpKind::Normal)
    }
    fn visit_v128_load8_splat(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load8_splat", &memarg, 1)
    }
    fn visit_v128_load16_splat(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load16_splat", &memarg, 2)
    }
    fn visit_v128_load32_splat(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load32_splat", &memarg, 4)
    }
    fn visit_v128_load32_zero(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load32_zero", &memarg, 4)
    }
    fn visit_v128_load64_splat(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load64_splat", &memarg, 8)
    }
    fn visit_v128_load64_zero(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load64_zero", &memarg, 8)
    }
    fn visit_v128_load8x8_s(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load8x8_s", &memarg, 8)
    }
    fn visit_v128_load8x8_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load8x8_u", &memarg, 8)
    }
    fn visit_v128_load16x4_s(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load16x4_s", &memarg, 8)
    }
    fn visit_v128_load16x4_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load16x4_u", &memarg, 8)
    }
    fn visit_v128_load32x2_s(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load32x2_s", &memarg, 8)
    }
    fn visit_v128_load32x2_u(&mut self, _pos: usize, memarg: MemArg) -> Self::Output {
        self.mem_instr("v128.load32x2_u", &memarg, 8)
    }
    fn visit_v128_load8_lane(&mut self, _pos: usize, memarg: MemArg, lane: u8) -> Self::Output {
        self.mem_instr("v128.load8_lane", &memarg, 1)?;
        write!(self.result(), " {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_v128_load16_lane(&mut self, _pos: usize, memarg: MemArg, lane: u8) -> Self::Output {
        self.mem_instr("v128.load16_lane", &memarg, 2)?;
        write!(self.result(), " {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_v128_load32_lane(&mut self, _pos: usize, memarg: MemArg, lane: u8) -> Self::Output {
        self.mem_instr("v128.load32_lane", &memarg, 4)?;
        write!(self.result(), " {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_v128_load64_lane(&mut self, _pos: usize, memarg: MemArg, lane: u8) -> Self::Output {
        self.mem_instr("v128.load64_lane", &memarg, 8)?;
        write!(self.result(), " {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_v128_store8_lane(&mut self, _pos: usize, memarg: MemArg, lane: u8) -> Self::Output {
        self.mem_instr("v128.store8_lane", &memarg, 1)?;
        write!(self.result(), " {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_v128_store16_lane(&mut self, _pos: usize, memarg: MemArg, lane: u8) -> Self::Output {
        self.mem_instr("v128.store16_lane", &memarg, 2)?;
        write!(self.result(), " {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_v128_store32_lane(&mut self, _pos: usize, memarg: MemArg, lane: u8) -> Self::Output {
        self.mem_instr("v128.store32_lane", &memarg, 4)?;
        write!(self.result(), " {lane}")?;
        Ok(OpKind::Normal)
    }
    fn visit_v128_store64_lane(&mut self, _pos: usize, memarg: MemArg, lane: u8) -> Self::Output {
        self.mem_instr("v128.store64_lane", &memarg, 8)?;
        write!(self.result(), " {lane}")?;
        Ok(OpKind::Normal)
    }
}
