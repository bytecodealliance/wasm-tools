use super::operators::OperatorValidator;
use crate::{BinaryReader, Result, ValType};
use crate::{FunctionBody, Operator, WasmFeatures, WasmModuleResources};

/// Validation context for a WebAssembly function.
///
/// This structure is created by
/// [`Validator::code_section_entry`](crate::Validator::code_section_entry)
/// and is created per-function in a WebAssembly module. This structure is
/// suitable for sending to other threads while the original
/// [`Validator`](crate::Validator) continues processing other functions.
pub struct FuncValidator<T> {
    validator: OperatorValidator,
    resources: T,
    index: u32,
}

impl<T: WasmModuleResources> FuncValidator<T> {
    /// Creates a new `FuncValidator`.
    ///
    /// The returned `FuncValidator` can be used to validate a function with
    /// the type `ty` specified. The `resources` indicate what the containing
    /// module has for the function to use, and the `features` configure what
    /// WebAssembly proposals are enabled for this function.
    ///
    /// The returned validator can be used to then parse a [`FunctionBody`], for
    /// example, to read locals and validate operators.
    pub fn new(
        index: u32,
        ty: u32,
        offset: usize,
        resources: T,
        features: &WasmFeatures,
    ) -> Result<FuncValidator<T>> {
        Ok(FuncValidator {
            validator: OperatorValidator::new_func(ty, offset, features, &resources)?,
            resources,
            index,
        })
    }

    /// Get the current height of the operand stack.
    ///
    /// This returns the height of the whole operand stack for this function,
    /// not just for the current control frame.
    pub fn operand_stack_height(&self) -> u32 {
        self.validator.operand_stack_height() as u32
    }

    /// Convenience function to validate an entire function's body.
    ///
    /// You may not end up using this in final implementations because you'll
    /// often want to interleave validation with parsing.
    pub fn validate(&mut self, body: &FunctionBody<'_>) -> Result<()> {
        let mut reader = body.get_binary_reader();
        self.read_locals(&mut reader)?;
        reader.allow_memarg64(self.validator.features.memory64);
        while !reader.eof() {
            reader.visit_operator(self)??;
        }
        self.finish(reader.original_position())
    }

    /// Reads the local defintions from the given `BinaryReader`, often sourced
    /// from a `FunctionBody`.
    ///
    /// This function will automatically advance the `BinaryReader` forward,
    /// leaving reading operators up to the caller afterwards.
    pub fn read_locals(&mut self, reader: &mut BinaryReader<'_>) -> Result<()> {
        for _ in 0..reader.read_var_u32()? {
            let offset = reader.original_position();
            let cnt = reader.read_var_u32()?;
            let ty = reader.read_val_type()?;
            self.define_locals(offset, cnt, ty)?;
        }
        Ok(())
    }

    /// Defines locals into this validator.
    ///
    /// This should be used if the application is already reading local
    /// definitions and there's no need to re-parse the function again.
    pub fn define_locals(&mut self, offset: usize, count: u32, ty: ValType) -> Result<()> {
        self.validator.define_locals(offset, count, ty)
    }

    /// Validates the next operator in a function.
    ///
    /// This functions is expected to be called once-per-operator in a
    /// WebAssembly function. Each operator's offset in the original binary and
    /// the operator itself are passed to this function to provide more useful
    /// error messages.
    pub fn op(&mut self, offset: usize, operator: &Operator<'_>) -> Result<()> {
        self.validator
            .with_resources(&self.resources)
            .visit_operator(offset, operator)
    }

    /// Function that must be called after the last opcode has been processed.
    ///
    /// This will validate that the function was properly terminated with the
    /// `end` opcode. If this function is not called then the function will not
    /// be properly validated.
    ///
    /// The `offset` provided to this function will be used as a position for an
    /// error if validation fails.
    pub fn finish(&mut self, offset: usize) -> Result<()> {
        self.validator.finish(offset)
    }

    /// Returns the underlying module resources that this validator is using.
    pub fn resources(&self) -> &T {
        &self.resources
    }

    /// The index of the function within the module's function index space that
    /// is being validated.
    pub fn index(&self) -> u32 {
        self.index
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::WasmFuncType;

    struct EmptyResources;

    impl WasmModuleResources for EmptyResources {
        type FuncType = EmptyFuncType;

        fn table_at(&self, _at: u32) -> Option<crate::TableType> {
            todo!()
        }
        fn memory_at(&self, _at: u32) -> Option<crate::MemoryType> {
            todo!()
        }
        fn tag_at(&self, _at: u32) -> Option<&Self::FuncType> {
            todo!()
        }
        fn global_at(&self, _at: u32) -> Option<crate::GlobalType> {
            todo!()
        }
        fn func_type_at(&self, _type_idx: u32) -> Option<&Self::FuncType> {
            Some(&EmptyFuncType)
        }
        fn type_of_function(&self, _func_idx: u32) -> Option<&Self::FuncType> {
            todo!()
        }
        fn element_type_at(&self, _at: u32) -> Option<ValType> {
            todo!()
        }
        fn element_count(&self) -> u32 {
            todo!()
        }
        fn data_count(&self) -> Option<u32> {
            todo!()
        }
        fn is_function_referenced(&self, _idx: u32) -> bool {
            todo!()
        }
    }

    struct EmptyFuncType;

    impl WasmFuncType for EmptyFuncType {
        fn len_inputs(&self) -> usize {
            0
        }
        fn len_outputs(&self) -> usize {
            0
        }
        fn input_at(&self, _at: u32) -> Option<ValType> {
            todo!()
        }
        fn output_at(&self, _at: u32) -> Option<ValType> {
            todo!()
        }
    }

    #[test]
    fn operand_stack_height() {
        let mut v = FuncValidator::new(0, 0, 0, &EmptyResources, &Default::default()).unwrap();

        // Initially zero values on the stack.
        assert_eq!(v.operand_stack_height(), 0);

        // Pushing a constant value makes use have one value on the stack.
        assert!(v.op(0, &Operator::I32Const { value: 0 }).is_ok());
        assert_eq!(v.operand_stack_height(), 1);

        // Entering a new control block does not affect the stack height.
        assert!(v
            .op(
                1,
                &Operator::Block {
                    ty: crate::BlockType::Empty
                }
            )
            .is_ok());
        assert_eq!(v.operand_stack_height(), 1);

        // Pushing another constant value makes use have two values on the stack.
        assert!(v.op(2, &Operator::I32Const { value: 99 }).is_ok());
        assert_eq!(v.operand_stack_height(), 2);
    }
}

use crate::{
    BlockType, BrTable, Ieee32, Ieee64, MemoryImmediate, SIMDLaneIndex, VisitOperator, V128,
};

macro_rules! forward {
    ( $this:ident.$visit_fn:ident($offset:expr $(, $param:expr)*) $(,)? ) => {{
        $this.validator.with_resources(&$this.resources).$visit_fn($offset, $( $param ),*)
    }};
}

#[allow(unused_variables)]
#[rustfmt::skip]
impl<'a, T> VisitOperator<'a> for FuncValidator<T>
where
    T: WasmModuleResources,
{
    type Output = Result<()>;

    fn visit_nop(&mut self, offset: usize) -> Self::Output { forward!(self.visit_nop(offset)) }
    fn visit_unreachable(&mut self, offset: usize) -> Self::Output { forward!(self.visit_unreachable(offset)) }
    fn visit_block(&mut self, offset: usize, ty: BlockType) -> Self::Output { forward!(self.visit_block(offset, ty)) }
    fn visit_loop(&mut self, offset: usize, ty: BlockType) -> Self::Output { forward!(self.visit_loop(offset, ty)) }
    fn visit_if(&mut self, offset: usize, ty: BlockType) -> Self::Output { forward!(self.visit_if(offset, ty)) }
    fn visit_else(&mut self, offset: usize) -> Self::Output { forward!(self.visit_else(offset)) }
    fn visit_try(&mut self, offset: usize, ty: BlockType) -> Self::Output { forward!(self.visit_try(offset, ty)) }
    fn visit_catch(&mut self, offset: usize, index: u32) -> Self::Output { forward!(self.visit_catch(offset, index)) }
    fn visit_throw(&mut self, offset: usize, index: u32) -> Self::Output { forward!(self.visit_throw(offset, index)) }
    fn visit_rethrow(&mut self, offset: usize, relative_depth: u32) -> Self::Output { forward!(self.visit_rethrow(offset, relative_depth)) }
    fn visit_delegate(&mut self, offset: usize, relative_depth: u32) -> Self::Output { forward!(self.visit_delegate(offset, relative_depth)) }
    fn visit_catch_all(&mut self, offset: usize) -> Self::Output { forward!(self.visit_catch_all(offset)) }
    fn visit_end(&mut self, offset: usize) -> Self::Output { forward!(self.visit_end(offset)) }
    fn visit_br(&mut self, offset: usize, relative_depth: u32) -> Self::Output { forward!(self.visit_br(offset, relative_depth)) }
    fn visit_br_if(&mut self, offset: usize, relative_depth: u32) -> Self::Output { forward!(self.visit_br_if(offset, relative_depth)) }
    fn visit_br_table(&mut self, offset: usize, table: &BrTable) -> Self::Output { forward!(self.visit_br_table(offset, table)) }
    fn visit_return(&mut self, offset: usize) -> Self::Output { forward!(self.visit_return(offset)) }
    fn visit_call(&mut self, offset: usize, function_index: u32) -> Self::Output { forward!(self.visit_call(offset, function_index)) }
    fn visit_return_call(&mut self, offset: usize, function_index: u32) -> Self::Output { forward!(self.visit_return_call(offset, function_index)) }
    fn visit_call_indirect(&mut self, offset: usize, index: u32, table_index: u32, table_byte: u8) -> Self::Output { forward!(self.visit_call_indirect(offset, index, table_index, table_byte)) }
    fn visit_return_call_indirect(&mut self, offset: usize, index: u32, table_index: u32) -> Self::Output { forward!(self.visit_return_call_indirect(offset, index, table_index)) }
    fn visit_drop(&mut self, offset: usize) -> Self::Output { forward!(self.visit_drop(offset)) }
    fn visit_select(&mut self, offset: usize) -> Self::Output { forward!(self.visit_select(offset)) }
    fn visit_typed_select(&mut self, offset: usize, ty: ValType) -> Self::Output { forward!(self.visit_typed_select(offset, ty)) }
    fn visit_local_get(&mut self, offset: usize, local_index: u32) -> Self::Output { forward!(self.visit_local_get(offset, local_index)) }
    fn visit_local_set(&mut self, offset: usize, local_index: u32) -> Self::Output { forward!(self.visit_local_set(offset, local_index)) }
    fn visit_local_tee(&mut self, offset: usize, local_index: u32) -> Self::Output { forward!(self.visit_local_tee(offset, local_index)) }
    fn visit_global_get(&mut self, offset: usize, global_index: u32) -> Self::Output { forward!(self.visit_global_get(offset, global_index)) }
    fn visit_global_set(&mut self, offset: usize, global_index: u32) -> Self::Output { forward!(self.visit_global_set(offset, global_index)) }
    fn visit_i32_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_load(offset, memarg)) }
    fn visit_i64_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_load(offset, memarg)) }
    fn visit_f32_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_f32_load(offset, memarg)) }
    fn visit_f64_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_f64_load(offset, memarg)) }
    fn visit_i32_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_load8_s(offset, memarg)) }
    fn visit_i32_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_load8_u(offset, memarg)) }
    fn visit_i32_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_load16_s(offset, memarg)) }
    fn visit_i32_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_load16_u(offset, memarg)) }
    fn visit_i64_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_load8_s(offset, memarg)) }
    fn visit_i64_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_load8_u(offset, memarg)) }
    fn visit_i64_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_load16_s(offset, memarg)) }
    fn visit_i64_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_load16_u(offset, memarg)) }
    fn visit_i64_load32_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_load32_s(offset, memarg)) }
    fn visit_i64_load32_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_load32_u(offset, memarg)) }
    fn visit_i32_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_store(offset, memarg)) }
    fn visit_i64_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_store(offset, memarg)) }
    fn visit_f32_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_f32_store(offset, memarg)) }
    fn visit_f64_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_f64_store(offset, memarg)) }
    fn visit_i32_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_store8(offset, memarg)) }
    fn visit_i32_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_store16(offset, memarg)) }
    fn visit_i64_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_store8(offset, memarg)) }
    fn visit_i64_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_store16(offset, memarg)) }
    fn visit_i64_store32(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_store32(offset, memarg)) }
    fn visit_memory_size(&mut self, offset: usize, mem: u32, mem_byte: u8) -> Self::Output { forward!(self.visit_memory_size(offset, mem, mem_byte)) }
    fn visit_memory_grow(&mut self, offset: usize, mem: u32, mem_byte: u8) -> Self::Output { forward!(self.visit_memory_grow(offset, mem, mem_byte)) }
    fn visit_i32_const(&mut self, offset: usize, value: i32) -> Self::Output { forward!(self.visit_i32_const(offset, value)) }
    fn visit_i64_const(&mut self, offset: usize, value: i64) -> Self::Output { forward!(self.visit_i64_const(offset, value)) }
    fn visit_f32_const(&mut self, offset: usize, value: Ieee32) -> Self::Output { forward!(self.visit_f32_const(offset, value)) }
    fn visit_f64_const(&mut self, offset: usize, value: Ieee64) -> Self::Output { forward!(self.visit_f64_const(offset, value)) }
    fn visit_i32_eqz(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_eqz(offset)) }
    fn visit_i32_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_eq(offset)) }
    fn visit_i32_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_ne(offset)) }
    fn visit_i32_lt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_lt_s(offset)) }
    fn visit_i32_lt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_lt_u(offset)) }
    fn visit_i32_gt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_gt_s(offset)) }
    fn visit_i32_gt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_gt_u(offset)) }
    fn visit_i32_le_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_le_s(offset)) }
    fn visit_i32_le_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_le_u(offset)) }
    fn visit_i32_ge_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_ge_s(offset)) }
    fn visit_i32_ge_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_ge_u(offset)) }
    fn visit_i64_eqz(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_eqz(offset)) }
    fn visit_i64_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_eq(offset)) }
    fn visit_i64_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_ne(offset)) }
    fn visit_i64_lt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_lt_s(offset)) }
    fn visit_i64_lt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_lt_u(offset)) }
    fn visit_i64_gt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_gt_s(offset)) }
    fn visit_i64_gt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_gt_u(offset)) }
    fn visit_i64_le_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_le_s(offset)) }
    fn visit_i64_le_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_le_u(offset)) }
    fn visit_i64_ge_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_ge_s(offset)) }
    fn visit_i64_ge_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_ge_u(offset)) }
    fn visit_f32_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_eq(offset)) }
    fn visit_f32_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_ne(offset)) }
    fn visit_f32_lt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_lt(offset)) }
    fn visit_f32_gt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_gt(offset)) }
    fn visit_f32_le(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_le(offset)) }
    fn visit_f32_ge(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_ge(offset)) }
    fn visit_f64_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_eq(offset)) }
    fn visit_f64_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_ne(offset)) }
    fn visit_f64_lt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_lt(offset)) }
    fn visit_f64_gt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_gt(offset)) }
    fn visit_f64_le(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_le(offset)) }
    fn visit_f64_ge(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_ge(offset)) }
    fn visit_i32_clz(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_clz(offset)) }
    fn visit_i32_ctz(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_ctz(offset)) }
    fn visit_i32_popcnt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_popcnt(offset)) }
    fn visit_i32_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_add(offset)) }
    fn visit_i32_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_sub(offset)) }
    fn visit_i32_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_mul(offset)) }
    fn visit_i32_div_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_div_s(offset)) }
    fn visit_i32_div_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_div_u(offset)) }
    fn visit_i32_rem_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_rem_s(offset)) }
    fn visit_i32_rem_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_rem_u(offset)) }
    fn visit_i32_and(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_and(offset)) }
    fn visit_i32_or(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_or(offset)) }
    fn visit_i32_xor(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_xor(offset)) }
    fn visit_i32_shl(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_shl(offset)) }
    fn visit_i32_shr_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_shr_s(offset)) }
    fn visit_i32_shr_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_shr_u(offset)) }
    fn visit_i32_rotl(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_rotl(offset)) }
    fn visit_i32_rotr(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_rotr(offset)) }
    fn visit_i64_clz(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_clz(offset)) }
    fn visit_i64_ctz(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_ctz(offset)) }
    fn visit_i64_popcnt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_popcnt(offset)) }
    fn visit_i64_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_add(offset)) }
    fn visit_i64_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_sub(offset)) }
    fn visit_i64_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_mul(offset)) }
    fn visit_i64_div_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_div_s(offset)) }
    fn visit_i64_div_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_div_u(offset)) }
    fn visit_i64_rem_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_rem_s(offset)) }
    fn visit_i64_rem_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_rem_u(offset)) }
    fn visit_i64_and(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_and(offset)) }
    fn visit_i64_or(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_or(offset)) }
    fn visit_i64_xor(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_xor(offset)) }
    fn visit_i64_shl(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_shl(offset)) }
    fn visit_i64_shr_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_shr_s(offset)) }
    fn visit_i64_shr_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_shr_u(offset)) }
    fn visit_i64_rotl(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_rotl(offset)) }
    fn visit_i64_rotr(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_rotr(offset)) }
    fn visit_f32_abs(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_abs(offset)) }
    fn visit_f32_neg(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_neg(offset)) }
    fn visit_f32_ceil(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_ceil(offset)) }
    fn visit_f32_floor(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_floor(offset)) }
    fn visit_f32_trunc(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_trunc(offset)) }
    fn visit_f32_nearest(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_nearest(offset)) }
    fn visit_f32_sqrt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_sqrt(offset)) }
    fn visit_f32_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_add(offset)) }
    fn visit_f32_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_sub(offset)) }
    fn visit_f32_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_mul(offset)) }
    fn visit_f32_div(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_div(offset)) }
    fn visit_f32_min(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_min(offset)) }
    fn visit_f32_max(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_max(offset)) }
    fn visit_f32_copysign(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_copysign(offset)) }
    fn visit_f64_abs(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_abs(offset)) }
    fn visit_f64_neg(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_neg(offset)) }
    fn visit_f64_ceil(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_ceil(offset)) }
    fn visit_f64_floor(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_floor(offset)) }
    fn visit_f64_trunc(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_trunc(offset)) }
    fn visit_f64_nearest(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_nearest(offset)) }
    fn visit_f64_sqrt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_sqrt(offset)) }
    fn visit_f64_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_add(offset)) }
    fn visit_f64_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_sub(offset)) }
    fn visit_f64_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_mul(offset)) }
    fn visit_f64_div(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_div(offset)) }
    fn visit_f64_min(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_min(offset)) }
    fn visit_f64_max(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_max(offset)) }
    fn visit_f64_copysign(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_copysign(offset)) }
    fn visit_i32_wrap_i64(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_wrap_i64(offset)) }
    fn visit_i32_trunc_f32s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_trunc_f32s(offset)) }
    fn visit_i32_trunc_f32u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_trunc_f32u(offset)) }
    fn visit_i32_trunc_f64s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_trunc_f64s(offset)) }
    fn visit_i32_trunc_f64u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_trunc_f64u(offset)) }
    fn visit_i64_extend_i32s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_extend_i32s(offset)) }
    fn visit_i64_extend_i32u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_extend_i32u(offset)) }
    fn visit_i64_trunc_f32s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_trunc_f32s(offset)) }
    fn visit_i64_trunc_f32u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_trunc_f32u(offset)) }
    fn visit_i64_trunc_f64s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_trunc_f64s(offset)) }
    fn visit_i64_trunc_f64u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_trunc_f64u(offset)) }
    fn visit_f32_convert_i32s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_convert_i32s(offset)) }
    fn visit_f32_convert_i32u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_convert_i32u(offset)) }
    fn visit_f32_convert_i64s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_convert_i64s(offset)) }
    fn visit_f32_convert_i64u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_convert_i64u(offset)) }
    fn visit_f32_demote_f64(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_demote_f64(offset)) }
    fn visit_f64_convert_i32s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_convert_i32s(offset)) }
    fn visit_f64_convert_i32u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_convert_i32u(offset)) }
    fn visit_f64_convert_i64s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_convert_i64s(offset)) }
    fn visit_f64_convert_i64u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_convert_i64u(offset)) }
    fn visit_f64_promote_f32(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_promote_f32(offset)) }
    fn visit_i32_reinterpret_f32(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_reinterpret_f32(offset)) }
    fn visit_i64_reinterpret_f64(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_reinterpret_f64(offset)) }
    fn visit_f32_reinterpret_i32(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32_reinterpret_i32(offset)) }
    fn visit_f64_reinterpret_i64(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64_reinterpret_i64(offset)) }
    fn visit_i32_trunc_sat_f32s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_trunc_sat_f32s(offset)) }
    fn visit_i32_trunc_sat_f32u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_trunc_sat_f32u(offset)) }
    fn visit_i32_trunc_sat_f64s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_trunc_sat_f64s(offset)) }
    fn visit_i32_trunc_sat_f64u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_trunc_sat_f64u(offset)) }
    fn visit_i64_trunc_sat_f32s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_trunc_sat_f32s(offset)) }
    fn visit_i64_trunc_sat_f32u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_trunc_sat_f32u(offset)) }
    fn visit_i64_trunc_sat_f64s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_trunc_sat_f64s(offset)) }
    fn visit_i64_trunc_sat_f64u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_trunc_sat_f64u(offset)) }
    fn visit_i32_extend8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_extend8_s(offset)) }
    fn visit_i32_extend16_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32_extend16_s(offset)) }
    fn visit_i64_extend8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_extend8_s(offset)) }
    fn visit_i64_extend16_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_extend16_s(offset)) }
    fn visit_i64_extend32_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64_extend32_s(offset)) }
    fn visit_i32_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_load(offset, memarg)) }
    fn visit_i32_atomic_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_load16_u(offset, memarg)) }
    fn visit_i32_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_load8_u(offset, memarg)) }
    fn visit_i64_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_load(offset, memarg)) }
    fn visit_i64_atomic_load32_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_load32_u(offset, memarg)) }
    fn visit_i64_atomic_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_load16_u(offset, memarg)) }
    fn visit_i64_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_load8_u(offset, memarg)) }
    fn visit_i32_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_store(offset, memarg)) }
    fn visit_i32_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_store16(offset, memarg)) }
    fn visit_i32_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_store8(offset, memarg)) }
    fn visit_i64_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_store(offset, memarg)) }
    fn visit_i64_atomic_store32(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_store32(offset, memarg)) }
    fn visit_i64_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_store16(offset, memarg)) }
    fn visit_i64_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_store8(offset, memarg)) }
    fn visit_i32_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw_add(offset, memarg)) }
    fn visit_i32_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw_sub(offset, memarg)) }
    fn visit_i32_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw_and(offset, memarg)) }
    fn visit_i32_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw_or(offset, memarg)) }
    fn visit_i32_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw_xor(offset, memarg)) }
    fn visit_i32_atomic_rmw16_add_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw16_add_u(offset, memarg)) }
    fn visit_i32_atomic_rmw16_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw16_sub_u(offset, memarg)) }
    fn visit_i32_atomic_rmw16_and_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw16_and_u(offset, memarg)) }
    fn visit_i32_atomic_rmw16_or_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw16_or_u(offset, memarg)) }
    fn visit_i32_atomic_rmw16_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw16_xor_u(offset, memarg)) }
    fn visit_i32_atomic_rmw8_add_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw8_add_u(offset, memarg)) }
    fn visit_i32_atomic_rmw8_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw8_sub_u(offset, memarg)) }
    fn visit_i32_atomic_rmw8_and_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw8_and_u(offset, memarg)) }
    fn visit_i32_atomic_rmw8_or_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw8_or_u(offset, memarg)) }
    fn visit_i32_atomic_rmw8_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw8_xor_u(offset, memarg)) }
    fn visit_i64_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw_add(offset, memarg)) }
    fn visit_i64_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw_sub(offset, memarg)) }
    fn visit_i64_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw_and(offset, memarg)) }
    fn visit_i64_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw_or(offset, memarg)) }
    fn visit_i64_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw_xor(offset, memarg)) }
    fn visit_i64_atomic_rmw32_add_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw32_add_u(offset, memarg)) }
    fn visit_i64_atomic_rmw32_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw32_sub_u(offset, memarg)) }
    fn visit_i64_atomic_rmw32_and_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw32_and_u(offset, memarg)) }
    fn visit_i64_atomic_rmw32_or_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw32_or_u(offset, memarg)) }
    fn visit_i64_atomic_rmw32_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw32_xor_u(offset, memarg)) }
    fn visit_i64_atomic_rmw16_add_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw16_add_u(offset, memarg)) }
    fn visit_i64_atomic_rmw16_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw16_sub_u(offset, memarg)) }
    fn visit_i64_atomic_rmw16_and_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw16_and_u(offset, memarg)) }
    fn visit_i64_atomic_rmw16_or_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw16_or_u(offset, memarg)) }
    fn visit_i64_atomic_rmw16_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw16_xor_u(offset, memarg)) }
    fn visit_i64_atomic_rmw8_add_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw8_add_u(offset, memarg)) }
    fn visit_i64_atomic_rmw8_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw8_sub_u(offset, memarg)) }
    fn visit_i64_atomic_rmw8_and_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw8_and_u(offset, memarg)) }
    fn visit_i64_atomic_rmw8_or_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw8_or_u(offset, memarg)) }
    fn visit_i64_atomic_rmw8_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw8_xor_u(offset, memarg)) }
    fn visit_i32_atomic_rmw_xchg(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw_xchg(offset, memarg)) }
    fn visit_i32_atomic_rmw16_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw16_xchg_u(offset, memarg)) }
    fn visit_i32_atomic_rmw8_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw8_xchg_u(offset, memarg)) }
    fn visit_i32_atomic_rmw_cmpxchg(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw_cmpxchg(offset, memarg)) }
    fn visit_i32_atomic_rmw16_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw16_cmpxchg_u(offset, memarg)) }
    fn visit_i32_atomic_rmw8_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i32_atomic_rmw8_cmpxchg_u(offset, memarg)) }
    fn visit_i64_atomic_rmw_xchg(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw_xchg(offset, memarg)) }
    fn visit_i64_atomic_rmw32_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw32_xchg_u(offset, memarg)) }
    fn visit_i64_atomic_rmw16_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw16_xchg_u(offset, memarg)) }
    fn visit_i64_atomic_rmw8_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw8_xchg_u(offset, memarg)) }
    fn visit_i64_atomic_rmw_cmpxchg(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw_cmpxchg(offset, memarg)) }
    fn visit_i64_atomic_rmw32_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw32_cmpxchg_u(offset, memarg)) }
    fn visit_i64_atomic_rmw16_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw16_cmpxchg_u(offset, memarg)) }
    fn visit_i64_atomic_rmw8_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_i64_atomic_rmw8_cmpxchg_u(offset, memarg)) }
    fn visit_memory_atomic_notify(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_memory_atomic_notify(offset, memarg)) }
    fn visit_memory_atomic_wait32(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_memory_atomic_wait32(offset, memarg)) }
    fn visit_memory_atomic_wait64(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_memory_atomic_wait64(offset, memarg)) }
    fn visit_atomic_fence(&mut self, offset: usize,  flags: u8) -> Self::Output { forward!(self.visit_atomic_fence(offset, flags)) }
    fn visit_ref_null(&mut self, offset: usize, ty: ValType) -> Self::Output { forward!(self.visit_ref_null(offset, ty)) }
    fn visit_ref_is_null(&mut self, offset: usize) -> Self::Output { forward!(self.visit_ref_is_null(offset)) }
    fn visit_ref_func(&mut self, offset: usize,  function_index: u32) -> Self::Output { forward!(self.visit_ref_func(offset, function_index)) }
    fn visit_v128_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load(offset, memarg)) }
    fn visit_v128_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_store(offset, memarg)) }
    fn visit_v128_const(&mut self, offset: usize, value: V128) -> Self::Output { forward!(self.visit_v128_const(offset, value)) }
    fn visit_i8x16_splat(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_splat(offset)) }
    fn visit_i16x8_splat(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_splat(offset)) }
    fn visit_i32x4_splat(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_splat(offset)) }
    fn visit_i64x2_splat(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_splat(offset)) }
    fn visit_f32x4_splat(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_splat(offset)) }
    fn visit_f64x2_splat(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_splat(offset)) }
    fn visit_i8x16_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i8x16_extract_lane_s(offset, lane)) }
    fn visit_i8x16_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i8x16_extract_lane_u(offset, lane)) }
    fn visit_i16x8_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i16x8_extract_lane_s(offset, lane)) }
    fn visit_i16x8_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i16x8_extract_lane_u(offset, lane)) }
    fn visit_i32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i32x4_extract_lane(offset, lane)) }
    fn visit_i8x16_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i8x16_replace_lane(offset, lane)) }
    fn visit_i16x8_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i16x8_replace_lane(offset, lane)) }
    fn visit_i32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i32x4_replace_lane(offset, lane)) }
    fn visit_i64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i64x2_extract_lane(offset, lane)) }
    fn visit_i64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_i64x2_replace_lane(offset, lane)) }
    fn visit_f32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_f32x4_extract_lane(offset, lane)) }
    fn visit_f32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_f32x4_replace_lane(offset, lane)) }
    fn visit_f64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_f64x2_extract_lane(offset, lane)) }
    fn visit_f64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_f64x2_replace_lane(offset, lane)) }
    fn visit_f32x4_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_eq(offset)) }
    fn visit_f32x4_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_ne(offset)) }
    fn visit_f32x4_lt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_lt(offset)) }
    fn visit_f32x4_gt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_gt(offset)) }
    fn visit_f32x4_le(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_le(offset)) }
    fn visit_f32x4_ge(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_ge(offset)) }
    fn visit_f64x2_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_eq(offset)) }
    fn visit_f64x2_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_ne(offset)) }
    fn visit_f64x2_lt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_lt(offset)) }
    fn visit_f64x2_gt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_gt(offset)) }
    fn visit_f64x2_le(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_le(offset)) }
    fn visit_f64x2_ge(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_ge(offset)) }
    fn visit_f32x4_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_add(offset)) }
    fn visit_f32x4_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_sub(offset)) }
    fn visit_f32x4_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_mul(offset)) }
    fn visit_f32x4_div(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_div(offset)) }
    fn visit_f32x4_min(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_min(offset)) }
    fn visit_f32x4_max(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_max(offset)) }
    fn visit_f32x4_p_min(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_p_min(offset)) }
    fn visit_f32x4_p_max(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_p_max(offset)) }
    fn visit_f64x2_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_add(offset)) }
    fn visit_f64x2_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_sub(offset)) }
    fn visit_f64x2_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_mul(offset)) }
    fn visit_f64x2_div(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_div(offset)) }
    fn visit_f64x2_min(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_min(offset)) }
    fn visit_f64x2_max(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_max(offset)) }
    fn visit_f64x2_p_min(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_p_min(offset)) }
    fn visit_f64x2_p_max(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_p_max(offset)) }
    fn visit_f32x4_relaxed_min(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_relaxed_min(offset)) }
    fn visit_f32x4_relaxed_max(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_relaxed_max(offset)) }
    fn visit_f64x2_relaxed_min(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_relaxed_min(offset)) }
    fn visit_f64x2_relaxed_max(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_relaxed_max(offset)) }
    fn visit_i8x16_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_eq(offset)) }
    fn visit_i8x16_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_ne(offset)) }
    fn visit_i8x16_lt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_lt_s(offset)) }
    fn visit_i8x16_lt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_lt_u(offset)) }
    fn visit_i8x16_gt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_gt_s(offset)) }
    fn visit_i8x16_gt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_gt_u(offset)) }
    fn visit_i8x16_le_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_le_s(offset)) }
    fn visit_i8x16_le_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_le_u(offset)) }
    fn visit_i8x16_ge_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_ge_s(offset)) }
    fn visit_i8x16_ge_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_ge_u(offset)) }
    fn visit_i16x8_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_eq(offset)) }
    fn visit_i16x8_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ne(offset)) }
    fn visit_i16x8_lt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_lt_s(offset)) }
    fn visit_i16x8_lt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_lt_u(offset)) }
    fn visit_i16x8_gt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_gt_s(offset)) }
    fn visit_i16x8_gt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_gt_u(offset)) }
    fn visit_i16x8_le_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_le_s(offset)) }
    fn visit_i16x8_le_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_le_u(offset)) }
    fn visit_i16x8_ge_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ge_s(offset)) }
    fn visit_i16x8_ge_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ge_u(offset)) }
    fn visit_i32x4_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_eq(offset)) }
    fn visit_i32x4_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ne(offset)) }
    fn visit_i32x4_lt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_lt_s(offset)) }
    fn visit_i32x4_lt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_lt_u(offset)) }
    fn visit_i32x4_gt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_gt_s(offset)) }
    fn visit_i32x4_gt_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_gt_u(offset)) }
    fn visit_i32x4_le_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_le_s(offset)) }
    fn visit_i32x4_le_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_le_u(offset)) }
    fn visit_i32x4_ge_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ge_s(offset)) }
    fn visit_i32x4_ge_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ge_u(offset)) }
    fn visit_i64x2_eq(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_eq(offset)) }
    fn visit_i64x2_ne(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_ne(offset)) }
    fn visit_i64x2_lt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_lt_s(offset)) }
    fn visit_i64x2_gt_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_gt_s(offset)) }
    fn visit_i64x2_le_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_le_s(offset)) }
    fn visit_i64x2_ge_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_ge_s(offset)) }
    fn visit_v128_and(&mut self, offset: usize) -> Self::Output { forward!(self.visit_v128_and(offset)) }
    fn visit_v128_and_not(&mut self, offset: usize) -> Self::Output { forward!(self.visit_v128_and_not(offset)) }
    fn visit_v128_or(&mut self, offset: usize) -> Self::Output { forward!(self.visit_v128_or(offset)) }
    fn visit_v128_xor(&mut self, offset: usize) -> Self::Output { forward!(self.visit_v128_xor(offset)) }
    fn visit_i8x16_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_add(offset)) }
    fn visit_i8x16_add_sat_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_add_sat_s(offset)) }
    fn visit_i8x16_add_sat_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_add_sat_u(offset)) }
    fn visit_i8x16_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_sub(offset)) }
    fn visit_i8x16_sub_sat_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_sub_sat_s(offset)) }
    fn visit_i8x16_sub_sat_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_sub_sat_u(offset)) }
    fn visit_i8x16_min_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_min_s(offset)) }
    fn visit_i8x16_min_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_min_u(offset)) }
    fn visit_i8x16_max_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_max_s(offset)) }
    fn visit_i8x16_max_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_max_u(offset)) }
    fn visit_i16x8_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_add(offset)) }
    fn visit_i16x8_add_sat_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_add_sat_s(offset)) }
    fn visit_i16x8_add_sat_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_add_sat_u(offset)) }
    fn visit_i16x8_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_sub(offset)) }
    fn visit_i16x8_sub_sat_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_sub_sat_s(offset)) }
    fn visit_i16x8_sub_sat_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_sub_sat_u(offset)) }
    fn visit_i16x8_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_mul(offset)) }
    fn visit_i16x8_min_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_min_s(offset)) }
    fn visit_i16x8_min_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_min_u(offset)) }
    fn visit_i16x8_max_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_max_s(offset)) }
    fn visit_i16x8_max_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_max_u(offset)) }
    fn visit_i32x4_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_add(offset)) }
    fn visit_i32x4_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_sub(offset)) }
    fn visit_i32x4_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_mul(offset)) }
    fn visit_i32x4_min_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_min_s(offset)) }
    fn visit_i32x4_min_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_min_u(offset)) }
    fn visit_i32x4_max_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_max_s(offset)) }
    fn visit_i32x4_max_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_max_u(offset)) }
    fn visit_i32x4_dot_i16x8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_dot_i16x8_s(offset)) }
    fn visit_i64x2_add(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_add(offset)) }
    fn visit_i64x2_sub(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_sub(offset)) }
    fn visit_i64x2_mul(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_mul(offset)) }
    fn visit_i8x16_rounding_average_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_rounding_average_u(offset)) }
    fn visit_i16x8_rounding_average_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_rounding_average_u(offset)) }
    fn visit_i8x16_narrow_i16x8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_narrow_i16x8_s(offset)) }
    fn visit_i8x16_narrow_i16x8_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_narrow_i16x8_u(offset)) }
    fn visit_i16x8_narrow_i32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_narrow_i32x4_s(offset)) }
    fn visit_i16x8_narrow_i32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_narrow_i32x4_u(offset)) }
    fn visit_i16x8_ext_mul_low_i8x16_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ext_mul_low_i8x16_s(offset)) }
    fn visit_i16x8_ext_mul_high_i8x16_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ext_mul_high_i8x16_s(offset)) }
    fn visit_i16x8_ext_mul_low_i8x16_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ext_mul_low_i8x16_u(offset)) }
    fn visit_i16x8_ext_mul_high_i8x16_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ext_mul_high_i8x16_u(offset)) }
    fn visit_i32x4_ext_mul_low_i16x8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ext_mul_low_i16x8_s(offset)) }
    fn visit_i32x4_ext_mul_high_i16x8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ext_mul_high_i16x8_s(offset)) }
    fn visit_i32x4_ext_mul_low_i16x8_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ext_mul_low_i16x8_u(offset)) }
    fn visit_i32x4_ext_mul_high_i16x8_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ext_mul_high_i16x8_u(offset)) }
    fn visit_i64x2_ext_mul_low_i32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_ext_mul_low_i32x4_s(offset)) }
    fn visit_i64x2_ext_mul_high_i32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_ext_mul_high_i32x4_s(offset)) }
    fn visit_i64x2_ext_mul_low_i32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_ext_mul_low_i32x4_u(offset)) }
    fn visit_i64x2_ext_mul_high_i32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_ext_mul_high_i32x4_u(offset)) }
    fn visit_i16x8_q15_mulr_sat_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_q15_mulr_sat_s(offset)) }
    fn visit_f32x4_ceil(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_ceil(offset)) }
    fn visit_f32x4_floor(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_floor(offset)) }
    fn visit_f32x4_trunc(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_trunc(offset)) }
    fn visit_f32x4_nearest(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_nearest(offset)) }
    fn visit_f64x2_ceil(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_ceil(offset)) }
    fn visit_f64x2_floor(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_floor(offset)) }
    fn visit_f64x2_trunc(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_trunc(offset)) }
    fn visit_f64x2_nearest(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_nearest(offset)) }
    fn visit_f32x4_abs(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_abs(offset)) }
    fn visit_f32x4_neg(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_neg(offset)) }
    fn visit_f32x4_sqrt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_sqrt(offset)) }
    fn visit_f64x2_abs(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_abs(offset)) }
    fn visit_f64x2_neg(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_neg(offset)) }
    fn visit_f64x2_sqrt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_sqrt(offset)) }
    fn visit_f32x4_demote_f64x2_zero(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_demote_f64x2_zero(offset)) }
    fn visit_f64x2_promote_low_f32x4(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_promote_low_f32x4(offset)) }
    fn visit_f64x2_convert_low_i32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_convert_low_i32x4_s(offset)) }
    fn visit_f64x2_convert_low_i32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_convert_low_i32x4_u(offset)) }
    fn visit_i32x4_trunc_sat_f32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_trunc_sat_f32x4_s(offset)) }
    fn visit_i32x4_trunc_sat_f32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_trunc_sat_f32x4_u(offset)) }
    fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_trunc_sat_f64x2_s_zero(offset)) }
    fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_trunc_sat_f64x2_u_zero(offset)) }
    fn visit_f32x4_convert_i32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_convert_i32x4_s(offset)) }
    fn visit_f32x4_convert_i32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_convert_i32x4_u(offset)) }
    fn visit_v128_not(&mut self, offset: usize) -> Self::Output { forward!(self.visit_v128_not(offset)) }
    fn visit_i8x16_abs(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_abs(offset)) }
    fn visit_i8x16_neg(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_neg(offset)) }
    fn visit_i8x16_popcnt(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_popcnt(offset)) }
    fn visit_i16x8_abs(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_abs(offset)) }
    fn visit_i16x8_neg(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_neg(offset)) }
    fn visit_i32x4_abs(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_abs(offset)) }
    fn visit_i32x4_neg(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_neg(offset)) }
    fn visit_i64x2_abs(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_abs(offset)) }
    fn visit_i64x2_neg(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_neg(offset)) }
    fn visit_i16x8_extend_low_i8x16_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_extend_low_i8x16_s(offset)) }
    fn visit_i16x8_extend_high_i8x16_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_extend_high_i8x16_s(offset)) }
    fn visit_i16x8_extend_low_i8x16_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_extend_low_i8x16_u(offset)) }
    fn visit_i16x8_extend_high_i8x16_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_extend_high_i8x16_u(offset)) }
    fn visit_i32x4_extend_low_i16x8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_extend_low_i16x8_s(offset)) }
    fn visit_i32x4_extend_high_i16x8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_extend_high_i16x8_s(offset)) }
    fn visit_i32x4_extend_low_i16x8_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_extend_low_i16x8_u(offset)) }
    fn visit_i32x4_extend_high_i16x8_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_extend_high_i16x8_u(offset)) }
    fn visit_i64x2_extend_low_i32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_extend_low_i32x4_s(offset)) }
    fn visit_i64x2_extend_high_i32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_extend_high_i32x4_s(offset)) }
    fn visit_i64x2_extend_low_i32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_extend_low_i32x4_u(offset)) }
    fn visit_i64x2_extend_high_i32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_extend_high_i32x4_u(offset)) }
    fn visit_i16x8_ext_add_pairwise_i8x16_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ext_add_pairwise_i8x16_s(offset)) }
    fn visit_i16x8_ext_add_pairwise_i8x16_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_ext_add_pairwise_i8x16_u(offset)) }
    fn visit_i32x4_ext_add_pairwise_i16x8_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ext_add_pairwise_i16x8_s(offset)) }
    fn visit_i32x4_ext_add_pairwise_i16x8_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_ext_add_pairwise_i16x8_u(offset)) }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_relaxed_trunc_sat_f32x4_s(offset)) }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_relaxed_trunc_sat_f32x4_u(offset)) }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(offset)) }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(offset)) }
    fn visit_v128_bitselect(&mut self, offset: usize) -> Self::Output { forward!(self.visit_v128_bitselect(offset)) }
    fn visit_f32x4_fma(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_fma(offset)) }
    fn visit_f32x4_fms(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f32x4_fms(offset)) }
    fn visit_f64x2_fma(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_fma(offset)) }
    fn visit_f64x2_fms(&mut self, offset: usize) -> Self::Output { forward!(self.visit_f64x2_fms(offset)) }
    fn visit_i8x16_lane_select(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_lane_select(offset)) }
    fn visit_i16x8_lane_select(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_lane_select(offset)) }
    fn visit_i32x4_lane_select(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_lane_select(offset)) }
    fn visit_i64x2_lane_select(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_lane_select(offset)) }
    fn visit_v128_any_true(&mut self, offset: usize) -> Self::Output { forward!(self.visit_v128_any_true(offset)) }
    fn visit_i8x16_all_true(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_all_true(offset)) }
    fn visit_i8x16_bitmask(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_bitmask(offset)) }
    fn visit_i16x8_all_true(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_all_true(offset)) }
    fn visit_i16x8_bitmask(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_bitmask(offset)) }
    fn visit_i32x4_all_true(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_all_true(offset)) }
    fn visit_i32x4_bitmask(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_bitmask(offset)) }
    fn visit_i64x2_all_true(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_all_true(offset)) }
    fn visit_i64x2_bitmask(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_bitmask(offset)) }
    fn visit_i8x16_shl(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_shl(offset)) }
    fn visit_i8x16_shr_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_shr_s(offset)) }
    fn visit_i8x16_shr_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_shr_u(offset)) }
    fn visit_i16x8_shl(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_shl(offset)) }
    fn visit_i16x8_shr_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_shr_s(offset)) }
    fn visit_i16x8_shr_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i16x8_shr_u(offset)) }
    fn visit_i32x4_shl(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_shl(offset)) }
    fn visit_i32x4_shr_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_shr_s(offset)) }
    fn visit_i32x4_shr_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i32x4_shr_u(offset)) }
    fn visit_i64x2_shl(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_shl(offset)) }
    fn visit_i64x2_shr_s(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_shr_s(offset)) }
    fn visit_i64x2_shr_u(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i64x2_shr_u(offset)) }
    fn visit_i8x16_swizzle(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_swizzle(offset)) }
    fn visit_i8x16_relaxed_swizzle(&mut self, offset: usize) -> Self::Output { forward!(self.visit_i8x16_relaxed_swizzle(offset)) }
    fn visit_i8x16_shuffle(&mut self, offset: usize, lanes: [SIMDLaneIndex; 16]) -> Self::Output { forward!(self.visit_i8x16_shuffle(offset, lanes)) }
    fn visit_v128_load8_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load8_splat(offset, memarg)) }
    fn visit_v128_load16_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load16_splat(offset, memarg)) }
    fn visit_v128_load32_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load32_splat(offset, memarg)) }
    fn visit_v128_load32_zero(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load32_zero(offset, memarg)) }
    fn visit_v128_load64_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load64_splat(offset, memarg)) }
    fn visit_v128_load64_zero(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load64_zero(offset, memarg)) }
    fn visit_v128_load8x8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load8x8_s(offset, memarg)) }
    fn visit_v128_load8x8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load8x8_u(offset, memarg)) }
    fn visit_v128_load16x4_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load16x4_s(offset, memarg)) }
    fn visit_v128_load16x4_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load16x4_u(offset, memarg)) }
    fn visit_v128_load32x2_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load32x2_s(offset, memarg)) }
    fn visit_v128_load32x2_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output { forward!(self.visit_v128_load32x2_u(offset, memarg)) }
    fn visit_v128_load8_lane(&mut self, offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_v128_load8_lane(offset, memarg, lane)) }
    fn visit_v128_load16_lane(&mut self, offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_v128_load16_lane(offset, memarg, lane)) }
    fn visit_v128_load32_lane(&mut self, offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_v128_load32_lane(offset, memarg, lane)) }
    fn visit_v128_load64_lane(&mut self, offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_v128_load64_lane(offset, memarg, lane)) }
    fn visit_v128_store8_lane(&mut self, offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_v128_store8_lane(offset, memarg, lane)) }
    fn visit_v128_store16_lane(&mut self, offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_v128_store16_lane(offset, memarg, lane)) }
    fn visit_v128_store32_lane(&mut self, offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_v128_store32_lane(offset, memarg, lane)) }
    fn visit_v128_store64_lane(&mut self, offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { forward!(self.visit_v128_store64_lane(offset, memarg, lane)) }
    fn visit_memory_init(&mut self, offset: usize, mem: u32, segment: u32) -> Self::Output { forward!(self.visit_memory_init(offset, mem, segment)) }
    fn visit_data_drop(&mut self, offset: usize, segment: u32) -> Self::Output { forward!(self.visit_data_drop(offset, segment)) }
    fn visit_memory_copy(&mut self, offset: usize, src: u32, dst: u32) -> Self::Output { forward!(self.visit_memory_copy(offset, src, dst)) }
    fn visit_memory_fill(&mut self, offset: usize, mem: u32) -> Self::Output { forward!(self.visit_memory_fill(offset, mem)) }
    fn visit_table_init(&mut self, offset: usize, segment: u32, table: u32) -> Self::Output { forward!(self.visit_table_init(offset, segment, table)) }
    fn visit_elem_drop(&mut self, offset: usize, segment: u32) -> Self::Output { forward!(self.visit_elem_drop(offset, segment)) }
    fn visit_table_copy(&mut self, offset: usize, dst_table: u32, src_table: u32) -> Self::Output { forward!(self.visit_table_copy(offset, dst_table, src_table)) }
    fn visit_table_get(&mut self, offset: usize, table: u32) -> Self::Output { forward!(self.visit_table_get(offset, table)) }
    fn visit_table_set(&mut self, offset: usize, table: u32) -> Self::Output { forward!(self.visit_table_set(offset, table)) }
    fn visit_table_grow(&mut self, offset: usize, table: u32) -> Self::Output { forward!(self.visit_table_grow(offset, table)) }
    fn visit_table_size(&mut self, offset: usize, table: u32) -> Self::Output { forward!(self.visit_table_size(offset, table)) }
    fn visit_table_fill(&mut self, offset: usize, table: u32) -> Self::Output { forward!(self.visit_table_fill(offset, table)) }
}
