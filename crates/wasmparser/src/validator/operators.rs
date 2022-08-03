/* Copyright 2019 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// The basic validation algorithm here is copied from the "Validation
// Algorithm" section of the WebAssembly specification -
// https://webassembly.github.io/spec/core/appendix/algorithm.html.
//
// That algorithm is followed pretty closely here, namely `push_operand`,
// `pop_operand`, `push_ctrl`, and `pop_ctrl`. If anything here is a bit
// confusing it's recommended to read over that section to see how it maps to
// the various methods here.

use crate::{
    limits::MAX_WASM_FUNCTION_LOCALS, BinaryReaderError, BlockType, BrTable, Ieee32, Ieee64,
    MemoryImmediate, Operator, Result, SIMDLaneIndex, ValType, VisitOperator, WasmFeatures,
    WasmFuncType, WasmModuleResources, V128,
};

/// A wrapper around a `BinaryReaderError` where the inner error's offset is a
/// temporary placeholder value. This can be converted into a proper
/// `BinaryReaderError` via the `set_offset` method, which replaces the
/// placeholder offset with an actual offset.
pub(crate) struct OperatorValidatorError(pub(crate) BinaryReaderError);

/// Create an `OperatorValidatorError` with a format string.
macro_rules! format_op_err {
    ( $( $arg:expr ),* $(,)* ) => {
        OperatorValidatorError::new(format!( $( $arg ),* ))
    }
}

/// Early return an `Err(OperatorValidatorError)` with a format string.
macro_rules! bail_op_err {
    ( $( $arg:expr ),* $(,)* ) => {
        return Err(format_op_err!( $( $arg ),* ))
    }
}

impl OperatorValidatorError {
    /// Create a new `OperatorValidatorError` with a placeholder offset.
    pub(crate) fn new(message: impl Into<String>) -> Self {
        let offset = std::usize::MAX;
        let e = BinaryReaderError::new(message, offset);
        OperatorValidatorError(e)
    }

    /// Convert this `OperatorValidatorError` into a `BinaryReaderError` by
    /// supplying an actual offset to replace the internal placeholder offset.
    pub(crate) fn set_offset(mut self, offset: usize) -> BinaryReaderError {
        debug_assert_eq!(self.0.inner.offset, std::usize::MAX);
        self.0.inner.offset = offset;
        self.0
    }
}

type OperatorValidatorResult<T> = std::result::Result<T, OperatorValidatorError>;

pub(crate) struct OperatorValidator {
    // The total number of locals that this function contains
    num_locals: u32,
    // This is a "compressed" list of locals for this function. The list of
    // locals are represented as a list of tuples. The second element is the
    // type of the local, and the first element is monotonically increasing as
    // you visit elements of this list. The first element is the maximum index
    // of the local, after the previous index, of the type specified.
    //
    // This allows us to do a binary search on the list for a local's index for
    // `local.{get,set,tee}`. We do a binary search for the index desired, and
    // it either lies in a "hole" where the maximum index is specified later,
    // or it's at the end of the list meaning it's out of bounds.
    locals: Vec<(u32, ValType)>,

    // The `operands` is the current type stack, and the `control` list is the
    // list of blocks that we're currently in.
    pub(crate) operands: Vec<Option<ValType>>,
    control: Vec<Frame>,

    // This is a list of flags for wasm features which are used to gate various
    // instructions.
    pub(crate) features: WasmFeatures,

    // Temporary storage used during the validation of `br_table`.
    br_table_tmp: Vec<Option<ValType>>,
}

// This structure corresponds to `ctrl_frame` as specified at in the validation
// appendix of the wasm spec
struct Frame {
    // Indicator for what kind of instruction pushed this frame.
    kind: FrameKind,
    // The type signature of this frame, represented as a singular return type
    // or a type index pointing into the module's types.
    block_type: BlockType,
    // The index, below which, this frame cannot modify the operand stack.
    height: usize,
    // Whether this frame is unreachable so far.
    unreachable: bool,
}

#[derive(PartialEq, Copy, Clone)]
enum FrameKind {
    Block,
    If,
    Else,
    Loop,
    Try,
    Catch,
    CatchAll,
}

impl OperatorValidator {
    /// Creates a new operator validator which will be used to validate a
    /// function whose type is the `ty` index specified.
    ///
    /// The `resources` are used to learn about the function type underlying
    /// `ty`.
    pub fn new_func(
        ty: u32,
        offset: usize,
        features: &WasmFeatures,
        resources: &impl WasmModuleResources,
    ) -> Result<OperatorValidator> {
        let locals = func_type_at(resources, ty)
            .map_err(|e| e.set_offset(offset))?
            .inputs()
            .enumerate()
            .map(|(i, ty)| (i as u32, ty))
            .collect::<Vec<_>>();
        Ok(OperatorValidator {
            num_locals: locals.len() as u32,
            locals,
            operands: Vec::new(),
            control: vec![Frame {
                kind: FrameKind::Block,
                block_type: BlockType::FuncType(ty),
                height: 0,
                unreachable: false,
            }],
            features: *features,
            br_table_tmp: Vec::new(),
        })
    }

    /// Creates a new operator validator which will be used to validate an
    /// `init_expr` constant expression which should result in the `ty`
    /// specified.
    pub fn new_init_expr(features: &WasmFeatures, ty: ValType) -> OperatorValidator {
        OperatorValidator {
            num_locals: 0,
            locals: Vec::new(),
            operands: Vec::new(),
            control: vec![Frame {
                kind: FrameKind::Block,
                block_type: BlockType::Type(ty),
                height: 0,
                unreachable: false,
            }],
            features: *features,
            br_table_tmp: Vec::new(),
        }
    }

    pub fn define_locals(&mut self, offset: usize, count: u32, ty: ValType) -> Result<()> {
        self.features
            .check_value_type(ty)
            .map_err(|e| BinaryReaderError::new(e, offset))?;
        if count == 0 {
            return Ok(());
        }
        match self.num_locals.checked_add(count) {
            Some(n) => self.num_locals = n,
            None => return Err(BinaryReaderError::new("locals overflow", offset)),
        }
        if self.num_locals > (MAX_WASM_FUNCTION_LOCALS as u32) {
            return Err(BinaryReaderError::new(
                "too many locals: locals exceed maximum",
                offset,
            ));
        }
        self.locals.push((self.num_locals - 1, ty));
        Ok(())
    }

    /// Fetches the type for the local at `idx`, returning an error if it's out
    /// of bounds.
    fn local(&self, idx: u32) -> OperatorValidatorResult<ValType> {
        match self.locals.binary_search_by_key(&idx, |(idx, _)| *idx) {
            // If this index would be inserted at the end of the list, then the
            // index is out of bounds and we return an error.
            Err(i) if i == self.locals.len() => {
                bail_op_err!("unknown local {}: local index out of bounds", idx)
            }
            // If `Ok` is returned we found the index exactly, or if `Err` is
            // returned the position is the one which is the least index
            // greater that `idx`, which is still the type of `idx` according
            // to our "compressed" representation. In both cases we access the
            // list at index `i`.
            Ok(i) | Err(i) => Ok(self.locals[i].1),
        }
    }

    /// Pushes a type onto the operand stack.
    ///
    /// This is used by instructions to represent a value that is pushed to the
    /// operand stack. This can fail, but only if `Type` is feature gated.
    /// Otherwise the push operation always succeeds.
    fn push_operand(&mut self, ty: ValType) -> OperatorValidatorResult<()> {
        self.features
            .check_value_type(ty)
            .map_err(OperatorValidatorError::new)?;
        self.operands.push(Some(ty));
        Ok(())
    }

    /// Attempts to pop a type from the operand stack.
    ///
    /// This function is used to remove types from the operand stack. The
    /// `expected` argument can be used to indicate that a type is required, or
    /// simply that something is needed to be popped.
    ///
    /// If `expected` is `Some(T)` then this will be guaranteed to return
    /// `Some(T)`, and it will only return success if the current block is
    /// unreachable or if `T` was found at the top of the operand stack.
    ///
    /// If `expected` is `None` then it indicates that something must be on the
    /// operand stack, but it doesn't matter what's on the operand stack. This
    /// is useful for polymorphic instructions like `select`.
    ///
    /// If `Some(T)` is returned then `T` was popped from the operand stack and
    /// matches `expected`. If `None` is returned then it means that `None` was
    /// expected and a type was successfully popped, but its exact type is
    /// indeterminate because the current block is unreachable.
    fn pop_operand(
        &mut self,
        expected: Option<ValType>,
    ) -> OperatorValidatorResult<Option<ValType>> {
        let control = self.control.last().unwrap();
        let actual = if self.operands.len() == control.height {
            if control.unreachable {
                None
            } else {
                let desc = match expected {
                    Some(ty) => ty_to_str(ty),
                    None => "a type",
                };
                bail_op_err!("type mismatch: expected {} but nothing on stack", desc)
            }
        } else {
            self.operands.pop().unwrap()
        };
        if let (Some(actual_ty), Some(expected_ty)) = (actual, expected) {
            if actual_ty != expected_ty {
                bail_op_err!(
                    "type mismatch: expected {}, found {}",
                    ty_to_str(expected_ty),
                    ty_to_str(actual_ty)
                )
            }
        }
        Ok(actual)
    }

    /// Flags the current control frame as unreachable, additionally truncating
    /// the currently active operand stack.
    fn unreachable(&mut self) {
        let control = self.control.last_mut().unwrap();
        self.operands.truncate(control.height);
        control.unreachable = true;
    }

    /// Pushes a new frame onto the control stack.
    ///
    /// This operation is used when entering a new block such as an if, loop,
    /// or block itself. The `kind` of block is specified which indicates how
    /// breaks interact with this block's type. Additionally the type signature
    /// of the block is specified by `ty`.
    fn push_ctrl(
        &mut self,
        kind: FrameKind,
        ty: BlockType,
        resources: &impl WasmModuleResources,
    ) -> OperatorValidatorResult<()> {
        // Push a new frame which has a snapshot of the height of the current
        // operand stack.
        self.control.push(Frame {
            kind,
            block_type: ty,
            height: self.operands.len(),
            unreachable: false,
        });
        // All of the parameters are now also available in this control frame,
        // so we push them here in order.
        for ty in params(ty, resources)? {
            self.push_operand(ty)?;
        }
        Ok(())
    }

    /// Pops a frame from the control stack.
    ///
    /// This function is used when exiting a block and leaves a block scope.
    /// Internally this will validate that blocks have the correct result type.
    fn pop_ctrl(&mut self, resources: &impl WasmModuleResources) -> OperatorValidatorResult<Frame> {
        // Read the expected type and expected height of the operand stack the
        // end of the frame.
        let frame = self.control.last().unwrap();
        let ty = frame.block_type;
        let height = frame.height;

        // Pop all the result types, in reverse order, from the operand stack.
        // These types will, possibly, be transferred to the next frame.
        for ty in results(ty, resources)?.rev() {
            self.pop_operand(Some(ty))?;
        }

        // Make sure that the operand stack has returned to is original
        // height...
        if self.operands.len() != height {
            bail_op_err!("type mismatch: values remaining on stack at end of block");
        }

        // And then we can remove it!
        Ok(self.control.pop().unwrap())
    }

    /// Validates a relative jump to the `depth` specified.
    ///
    /// Returns the type signature of the block that we're jumping to as well
    /// as the kind of block if the jump is valid. Otherwise returns an error.
    fn jump(&self, depth: u32) -> OperatorValidatorResult<(BlockType, FrameKind)> {
        match (self.control.len() - 1).checked_sub(depth as usize) {
            Some(i) => {
                let frame = &self.control[i];
                Ok((frame.block_type, frame.kind))
            }
            None => bail_op_err!("unknown label: branch depth too large"),
        }
    }

    /// Validates that `memory_index` is valid in this module, and returns the
    /// type of address used to index the memory specified.
    fn check_memory_index(
        &self,
        memory_index: u32,
        resources: impl WasmModuleResources,
    ) -> OperatorValidatorResult<ValType> {
        if memory_index > 0 && !self.features.multi_memory {
            return Err(OperatorValidatorError::new(
                "multi-memory support is not enabled",
            ));
        }
        match resources.memory_at(memory_index) {
            Some(mem) => Ok(mem.index_type()),
            None => bail_op_err!("unknown memory {}", memory_index),
        }
    }

    /// Validates a `memarg for alignment and such (also the memory it
    /// references), and returns the type of index used to address the memory.
    fn check_memarg(
        &self,
        memarg: MemoryImmediate,
        max_align: u8,
        resources: impl WasmModuleResources,
    ) -> OperatorValidatorResult<ValType> {
        let index_ty = self.check_memory_index(memarg.memory, resources)?;
        let align = memarg.align;
        if align > max_align {
            return Err(OperatorValidatorError::new(
                "alignment must not be larger than natural",
            ));
        }
        if index_ty == ValType::I32 && memarg.offset > u64::from(u32::MAX) {
            return Err(OperatorValidatorError::new(
                "offset out of range: must be <= 2**32",
            ));
        }
        Ok(index_ty)
    }

    #[cfg(feature = "deterministic")]
    fn check_non_deterministic_enabled(&self) -> OperatorValidatorResult<()> {
        if !self.features.deterministic_only {
            return Err(OperatorValidatorError::new(
                "deterministic_only support is not enabled",
            ));
        }
        Ok(())
    }

    #[inline(always)]
    #[cfg(not(feature = "deterministic"))]
    fn check_non_deterministic_enabled(&self) -> OperatorValidatorResult<()> {
        Ok(())
    }

    fn check_threads_enabled(&self) -> OperatorValidatorResult<()> {
        if !self.features.threads {
            return Err(OperatorValidatorError::new(
                "threads support is not enabled",
            ));
        }
        Ok(())
    }

    fn check_reference_types_enabled(&self) -> OperatorValidatorResult<()> {
        if !self.features.reference_types {
            return Err(OperatorValidatorError::new(
                "reference types support is not enabled",
            ));
        }
        Ok(())
    }

    /// Checks if Wasm proposal `saturating_float_to_int` is enabled.
    fn check_saturating_float_to_int_enabled(&self) -> OperatorValidatorResult<()> {
        if !self.features.saturating_float_to_int {
            return Err(OperatorValidatorError::new(
                "saturating float to int conversions support is not enabled",
            ));
        }
        Ok(())
    }

    /// Checks if Wasm proposal `sign_extension` is enabled.
    fn check_sign_extension_enabled(&self) -> OperatorValidatorResult<()> {
        if !self.features.sign_extension {
            return Err(OperatorValidatorError::new(
                "sign extension operations support is not enabled",
            ));
        }
        Ok(())
    }

    fn check_simd_enabled(&self) -> OperatorValidatorResult<()> {
        if !self.features.simd {
            return Err(OperatorValidatorError::new("SIMD support is not enabled"));
        }
        Ok(())
    }

    fn check_relaxed_simd_enabled(&self) -> OperatorValidatorResult<()> {
        // Relaxed SIMD operators make sense only with SIMD and be non-deterministic.
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled()?;
        if !self.features.relaxed_simd {
            return Err(OperatorValidatorError::new(
                "Relaxed SIMD support is not enabled",
            ));
        }
        Ok(())
    }

    fn check_exceptions_enabled(&self) -> OperatorValidatorResult<()> {
        if !self.features.exceptions {
            return Err(OperatorValidatorError::new(
                "Exceptions support is not enabled",
            ));
        }
        Ok(())
    }

    fn check_bulk_memory_enabled(&self) -> OperatorValidatorResult<()> {
        if !self.features.bulk_memory {
            return Err(OperatorValidatorError::new(
                "bulk memory support is not enabled",
            ));
        }
        Ok(())
    }

    fn check_shared_memarg_wo_align(
        &self,
        memarg: MemoryImmediate,
        resources: impl WasmModuleResources,
    ) -> OperatorValidatorResult<ValType> {
        self.check_memory_index(memarg.memory, resources)
    }

    fn check_simd_lane_index(&self, index: SIMDLaneIndex, max: u8) -> OperatorValidatorResult<()> {
        if index >= max {
            return Err(OperatorValidatorError::new("SIMD index out of bounds"));
        }
        Ok(())
    }

    /// Validates a block type, primarily with various in-flight proposals.
    fn check_block_type(
        &self,
        ty: BlockType,
        resources: impl WasmModuleResources,
    ) -> OperatorValidatorResult<()> {
        match ty {
            BlockType::Empty
            | BlockType::Type(ValType::I32)
            | BlockType::Type(ValType::I64)
            | BlockType::Type(ValType::F32)
            | BlockType::Type(ValType::F64) => Ok(()),
            BlockType::Type(ValType::ExternRef) | BlockType::Type(ValType::FuncRef) => {
                self.check_reference_types_enabled()
            }
            BlockType::Type(ValType::V128) => self.check_simd_enabled(),
            BlockType::FuncType(idx) => {
                if !self.features.multi_value {
                    return Err(OperatorValidatorError::new(
                        "blocks, loops, and ifs may only produce a resulttype \
                         when multi-value is not enabled",
                    ));
                }
                func_type_at(&resources, idx)?;
                Ok(())
            }
        }
    }

    /// Validates a `call` instruction, ensuring that the function index is
    /// in-bounds and the right types are on the stack to call the function.
    fn check_call(
        &mut self,
        function_index: u32,
        resources: &impl WasmModuleResources,
    ) -> OperatorValidatorResult<()> {
        let ty = match resources.type_of_function(function_index) {
            Some(i) => i,
            None => {
                bail_op_err!(
                    "unknown function {}: function index out of bounds",
                    function_index
                );
            }
        };
        for ty in ty.inputs().rev() {
            self.pop_operand(Some(ty))?;
        }
        for ty in ty.outputs() {
            self.push_operand(ty)?;
        }
        Ok(())
    }

    /// Validates a call to an indirect function, very similar to `check_call`.
    fn check_call_indirect(
        &mut self,
        index: u32,
        table_index: u32,
        resources: &impl WasmModuleResources,
    ) -> OperatorValidatorResult<()> {
        match resources.table_at(table_index) {
            None => {
                return Err(OperatorValidatorError::new(
                    "unknown table: table index out of bounds",
                ));
            }
            Some(tab) => {
                if tab.element_type != ValType::FuncRef {
                    return Err(OperatorValidatorError::new(
                        "indirect calls must go through a table of funcref",
                    ));
                }
            }
        }
        let ty = func_type_at(&resources, index)?;
        self.pop_operand(Some(ValType::I32))?;
        for ty in ty.inputs().rev() {
            self.pop_operand(Some(ty))?;
        }
        for ty in ty.outputs() {
            self.push_operand(ty)?;
        }
        Ok(())
    }

    /// Validates a `return` instruction, popping types from the operand
    /// stack that the function needs.
    fn check_return(
        &mut self,
        resources: &impl WasmModuleResources,
    ) -> OperatorValidatorResult<()> {
        for ty in results(self.control[0].block_type, resources)?.rev() {
            self.pop_operand(Some(ty))?;
        }
        self.unreachable();
        Ok(())
    }

    /// Checks the validity of a common comparison operator.
    fn check_cmp_op(&mut self, ty: ValType) -> OperatorValidatorResult<()> {
        self.pop_operand(Some(ty))?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }

    /// Checks the validity of a common float comparison operator.
    fn check_fcmp_op(&mut self, ty: ValType) -> OperatorValidatorResult<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled()?;
        self.check_cmp_op(ty)
    }

    /// Checks the validity of a common unary operator.
    fn check_unary_op(&mut self, ty: ValType) -> OperatorValidatorResult<()> {
        self.pop_operand(Some(ty))?;
        self.push_operand(ty)?;
        Ok(())
    }

    /// Checks the validity of a common unary float operator.
    fn check_funary_op(&mut self, ty: ValType) -> OperatorValidatorResult<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled()?;
        self.check_unary_op(ty)
    }

    /// Checks the validity of a common conversion operator.
    fn check_conversion_op(&mut self, into: ValType, from: ValType) -> OperatorValidatorResult<()> {
        self.pop_operand(Some(from))?;
        self.push_operand(into)?;
        Ok(())
    }

    /// Checks the validity of a common conversion operator.
    fn check_fconversion_op(
        &mut self,
        into: ValType,
        from: ValType,
    ) -> OperatorValidatorResult<()> {
        debug_assert!(matches!(into, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled()?;
        self.check_conversion_op(into, from)
    }

    /// Checks the validity of a common binary operator.
    fn check_binary_op(&mut self, ty: ValType) -> OperatorValidatorResult<()> {
        self.pop_operand(Some(ty))?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ty)?;
        Ok(())
    }

    /// Checks the validity of a common binary float operator.
    fn check_fbinary_op(&mut self, ty: ValType) -> OperatorValidatorResult<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled()?;
        self.check_binary_op(ty)
    }

    /// Checks the validity of an atomic load operator.
    fn check_atomic_load<T>(
        &mut self,
        resources: &T,
        memarg: MemoryImmediate,
        load_ty: ValType,
    ) -> OperatorValidatorResult<()>
    where
        T: WasmModuleResources,
    {
        self.check_threads_enabled()?;
        let ty = self.check_shared_memarg_wo_align(memarg, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(load_ty)?;
        Ok(())
    }

    /// Checks the validity of an atomic store operator.
    fn check_atomic_store<T>(
        &mut self,
        resources: &T,
        memarg: MemoryImmediate,
        store_ty: ValType,
    ) -> OperatorValidatorResult<()>
    where
        T: WasmModuleResources,
    {
        self.check_threads_enabled()?;
        let ty = self.check_shared_memarg_wo_align(memarg, resources)?;
        self.pop_operand(Some(store_ty))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }

    /// Checks the validity of a common atomic binary operator.
    fn check_atomic_binary_op<T>(
        &mut self,
        resources: &T,
        memarg: MemoryImmediate,
        op_ty: ValType,
    ) -> OperatorValidatorResult<()>
    where
        T: WasmModuleResources,
    {
        self.check_threads_enabled()?;
        let ty = self.check_shared_memarg_wo_align(memarg, resources)?;
        self.pop_operand(Some(op_ty))?;
        self.pop_operand(Some(ty))?;
        self.push_operand(op_ty)?;
        Ok(())
    }

    /// Checks the validity of an atomic compare exchange operator.
    fn check_atomic_binary_cmpxchg<T>(
        &mut self,
        resources: &T,
        memarg: MemoryImmediate,
        op_ty: ValType,
    ) -> OperatorValidatorResult<()>
    where
        T: WasmModuleResources,
    {
        self.check_threads_enabled()?;
        let ty = self.check_shared_memarg_wo_align(memarg, resources)?;
        self.pop_operand(Some(op_ty))?;
        self.pop_operand(Some(op_ty))?;
        self.pop_operand(Some(ty))?;
        self.push_operand(op_ty)?;
        Ok(())
    }

    /// Checks a [`V128`] splat operator.
    fn check_v128_splat(&mut self, src_ty: ValType) -> OperatorValidatorResult<()> {
        self.check_simd_enabled()?;
        self.pop_operand(Some(src_ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_binary_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary float operator.
    fn check_v128_fbinary_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_non_deterministic_enabled()?;
        self.check_v128_binary_op()
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_relaxed_binary_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_relaxed_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_unary_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_funary_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_non_deterministic_enabled()?;
        self.check_v128_unary_op()
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_relaxed_unary_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_relaxed_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_relaxed_ternary_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_relaxed_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_bitmask_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_shift_op(&mut self) -> OperatorValidatorResult<()> {
        self.check_simd_enabled()?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] common load operator.
    fn check_v128_load_op<T>(
        &mut self,
        resources: T,
        memarg: MemoryImmediate,
    ) -> OperatorValidatorResult<()>
    where
        T: WasmModuleResources,
    {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 3, resources)?;
        self.pop_operand(Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    #[rustfmt::skip]
    pub fn process_operator(
        &mut self,
        operator: &Operator,
        resources: &impl WasmModuleResources,
    ) -> OperatorValidatorResult<()> {
        match *operator {
            Operator::Unreachable => self.visit_unreachable(resources),
            Operator::Nop => self.visit_nop(resources),
            Operator::Block { ty } => self.visit_block(resources, ty),
            Operator::Loop { ty } => self.visit_loop(resources, ty),
            Operator::If { ty } => self.visit_if(resources, ty),
            Operator::Else => self.visit_else(resources),
            Operator::Try { ty } => self.visit_try(resources, ty),
            Operator::Catch { index } => self.visit_catch(resources, index),
            Operator::Throw { index } => self.visit_throw(resources, index),
            Operator::Rethrow { relative_depth } => self.visit_rethrow(resources, relative_depth),
            Operator::End => self.visit_end(resources),
            Operator::Br { relative_depth } => self.visit_br(resources, relative_depth),
            Operator::BrIf { relative_depth } => self.visit_br_if(resources, relative_depth),
            Operator::BrTable { ref table } => self.visit_br_table(resources, table),
            Operator::Return => self.visit_return(resources),
            Operator::Call { function_index } => self.visit_call(resources, function_index),
            Operator::CallIndirect { index, table_index, table_byte } => self.visit_call_indirect(resources, index, table_index, table_byte),
            Operator::ReturnCall { function_index } => self.visit_return_call(resources, function_index),
            Operator::ReturnCallIndirect { index, table_index } => self.visit_return_call_indirect(resources, index, table_index),
            Operator::Delegate { relative_depth } => self.visit_delegate(resources, relative_depth),
            Operator::CatchAll => self.visit_catch_all(resources),
            Operator::Drop => self.visit_drop(resources),
            Operator::Select => self.visit_select(resources),
            Operator::TypedSelect { ty } => self.visit_typed_select(resources, ty),
            Operator::LocalGet { local_index } => self.visit_local_get(resources, local_index),
            Operator::LocalSet { local_index } => self.visit_local_set(resources, local_index),
            Operator::LocalTee { local_index } => self.visit_local_tee(resources, local_index),
            Operator::GlobalGet { global_index } => self.visit_global_get(resources, global_index),
            Operator::GlobalSet { global_index } => self.visit_global_set(resources, global_index),
            Operator::I32Load { memarg } => self.visit_i32_load(resources, memarg),
            Operator::I64Load { memarg } => self.visit_i64_load(resources, memarg),
            Operator::F32Load { memarg } => self.visit_f32_load(resources, memarg),
            Operator::F64Load { memarg } => self.visit_f64_load(resources, memarg),
            Operator::I32Load8S { memarg } => self.visit_i32_load8_s(resources, memarg),
            Operator::I32Load8U { memarg } => self.visit_i32_load8_u(resources, memarg),
            Operator::I32Load16S { memarg } => self.visit_i32_load16_s(resources, memarg),
            Operator::I32Load16U { memarg } => self.visit_i32_load16_u(resources, memarg),
            Operator::I64Load8S { memarg } => self.visit_i64_load8_s(resources, memarg),
            Operator::I64Load8U { memarg } => self.visit_i64_load8_u(resources, memarg),
            Operator::I64Load16S { memarg } => self.visit_i64_load16_s(resources, memarg),
            Operator::I64Load16U { memarg } => self.visit_i64_load16_u(resources, memarg),
            Operator::I64Load32S { memarg } => self.visit_i64_load32_s(resources, memarg),
            Operator::I64Load32U { memarg } => self.visit_i64_load32_u(resources, memarg),
            Operator::I32Store { memarg } => self.visit_i32_store(resources, memarg),
            Operator::I64Store { memarg } => self.visit_i64_store(resources, memarg),
            Operator::F32Store { memarg } => self.visit_f32_store(resources, memarg),
            Operator::F64Store { memarg } => self.visit_f64_store(resources, memarg),
            Operator::I32Store8 { memarg } => self.visit_i32_store8(resources, memarg),
            Operator::I32Store16 { memarg } => self.visit_i32_store16(resources, memarg),
            Operator::I64Store8 { memarg } => self.visit_i64_store8(resources, memarg),
            Operator::I64Store16 { memarg } => self.visit_i64_store16(resources, memarg),
            Operator::I64Store32 { memarg } => self.visit_i64_store32(resources, memarg),
            Operator::MemorySize { mem, mem_byte } => self.visit_memory_size(resources, mem, mem_byte),
            Operator::MemoryGrow { mem, mem_byte } => self.visit_memory_grow(resources, mem, mem_byte),
            Operator::I32Const { value } => self.visit_i32_const(resources, value),
            Operator::I64Const { value } => self.visit_i64_const(resources, value),
            Operator::F32Const { value } => self.visit_f32_const(resources, value),
            Operator::F64Const { value } => self.visit_f64_const(resources, value),
            Operator::RefNull { ty } => self.visit_ref_null(resources, ty),
            Operator::RefIsNull => self.visit_ref_is_null(resources),
            Operator::RefFunc { function_index } => self.visit_ref_func(resources, function_index),
            Operator::I32Eqz => self.visit_i32_eqz(resources),
            Operator::I32Eq => self.visit_i32_eq(resources),
            Operator::I32Ne => self.visit_i32_ne(resources),
            Operator::I32LtS => self.visit_i32_lt_s(resources),
            Operator::I32LtU => self.visit_i32_lt_u(resources),
            Operator::I32GtS => self.visit_i32_gt_s(resources),
            Operator::I32GtU => self.visit_i32_gt_u(resources),
            Operator::I32LeS => self.visit_i32_le_s(resources),
            Operator::I32LeU => self.visit_i32_le_u(resources),
            Operator::I32GeS => self.visit_i32_ge_s(resources),
            Operator::I32GeU => self.visit_i32_ge_u(resources),
            Operator::I64Eqz => self.visit_i64_eqz(resources),
            Operator::I64Eq => self.visit_i64_eq(resources),
            Operator::I64Ne => self.visit_i64_ne(resources),
            Operator::I64LtS => self.visit_i64_lt_s(resources),
            Operator::I64LtU => self.visit_i64_lt_u(resources),
            Operator::I64GtS => self.visit_i64_gt_s(resources),
            Operator::I64GtU => self.visit_i64_gt_u(resources),
            Operator::I64LeS => self.visit_i64_le_s(resources),
            Operator::I64LeU => self.visit_i64_le_u(resources),
            Operator::I64GeS => self.visit_i64_ge_s(resources),
            Operator::I64GeU => self.visit_i64_ge_u(resources),
            Operator::F32Eq => self.visit_f32_eq(resources),
            Operator::F32Ne => self.visit_f32_ne(resources),
            Operator::F32Lt => self.visit_f32_lt(resources),
            Operator::F32Gt => self.visit_f32_gt(resources),
            Operator::F32Le => self.visit_f32_le(resources),
            Operator::F32Ge => self.visit_f32_ge(resources),
            Operator::F64Eq => self.visit_f64_eq(resources),
            Operator::F64Ne => self.visit_f64_ne(resources),
            Operator::F64Lt => self.visit_f64_lt(resources),
            Operator::F64Gt => self.visit_f64_gt(resources),
            Operator::F64Le => self.visit_f64_le(resources),
            Operator::F64Ge => self.visit_f64_ge(resources),
            Operator::I32Clz => self.visit_i32_clz(resources),
            Operator::I32Ctz => self.visit_i32_ctz(resources),
            Operator::I32Popcnt => self.visit_i32_popcnt(resources),
            Operator::I32Add => self.visit_i32_add(resources),
            Operator::I32Sub => self.visit_i32_sub(resources),
            Operator::I32Mul => self.visit_i32_mul(resources),
            Operator::I32DivS => self.visit_i32_div_s(resources),
            Operator::I32DivU => self.visit_i32_div_u(resources),
            Operator::I32RemS => self.visit_i32_rem_s(resources),
            Operator::I32RemU => self.visit_i32_rem_u(resources),
            Operator::I32And => self.visit_i32_and(resources),
            Operator::I32Or => self.visit_i32_or(resources),
            Operator::I32Xor => self.visit_i32_xor(resources),
            Operator::I32Shl => self.visit_i32_shl(resources),
            Operator::I32ShrS => self.visit_i32_shr_s(resources),
            Operator::I32ShrU => self.visit_i32_shr_u(resources),
            Operator::I32Rotl => self.visit_i32_rotl(resources),
            Operator::I32Rotr => self.visit_i32_rotr(resources),
            Operator::I64Clz => self.visit_i64_clz(resources),
            Operator::I64Ctz => self.visit_i64_ctz(resources),
            Operator::I64Popcnt => self.visit_i64_popcnt(resources),
            Operator::I64Add => self.visit_i64_add(resources),
            Operator::I64Sub => self.visit_i64_sub(resources),
            Operator::I64Mul => self.visit_i64_mul(resources),
            Operator::I64DivS => self.visit_i64_div_s(resources),
            Operator::I64DivU => self.visit_i64_div_u(resources),
            Operator::I64RemS => self.visit_i64_rem_s(resources),
            Operator::I64RemU => self.visit_i64_rem_u(resources),
            Operator::I64And => self.visit_i64_and(resources),
            Operator::I64Or => self.visit_i64_or(resources),
            Operator::I64Xor => self.visit_i64_xor(resources),
            Operator::I64Shl => self.visit_i64_shl(resources),
            Operator::I64ShrS => self.visit_i64_shr_s(resources),
            Operator::I64ShrU => self.visit_i64_shr_u(resources),
            Operator::I64Rotl => self.visit_i64_rotl(resources),
            Operator::I64Rotr => self.visit_i64_rotr(resources),
            Operator::F32Abs => self.visit_f32_abs(resources),
            Operator::F32Neg => self.visit_f32_neg(resources),
            Operator::F32Ceil => self.visit_f32_ceil(resources),
            Operator::F32Floor => self.visit_f32_floor(resources),
            Operator::F32Trunc => self.visit_f32_trunc(resources),
            Operator::F32Nearest => self.visit_f32_nearest(resources),
            Operator::F32Sqrt => self.visit_f32_sqrt(resources),
            Operator::F32Add => self.visit_f32_add(resources),
            Operator::F32Sub => self.visit_f32_sub(resources),
            Operator::F32Mul => self.visit_f32_mul(resources),
            Operator::F32Div => self.visit_f32_div(resources),
            Operator::F32Min => self.visit_f32_min(resources),
            Operator::F32Max => self.visit_f32_max(resources),
            Operator::F32Copysign => self.visit_f32_copysign(resources),
            Operator::F64Abs => self.visit_f64_abs(resources),
            Operator::F64Neg => self.visit_f64_neg(resources),
            Operator::F64Ceil => self.visit_f64_ceil(resources),
            Operator::F64Floor => self.visit_f64_floor(resources),
            Operator::F64Trunc => self.visit_f64_trunc(resources),
            Operator::F64Nearest => self.visit_f64_nearest(resources),
            Operator::F64Sqrt => self.visit_f64_sqrt(resources),
            Operator::F64Add => self.visit_f64_add(resources),
            Operator::F64Sub => self.visit_f64_sub(resources),
            Operator::F64Mul => self.visit_f64_mul(resources),
            Operator::F64Div => self.visit_f64_div(resources),
            Operator::F64Min => self.visit_f64_min(resources),
            Operator::F64Max => self.visit_f64_max(resources),
            Operator::F64Copysign => self.visit_f64_copysign(resources),
            Operator::I32WrapI64 => self.visit_i32_wrap_i64(resources),
            Operator::I32TruncF32S => self.visit_i32_trunc_f32s(resources),
            Operator::I32TruncF32U => self.visit_i32_trunc_f32u(resources),
            Operator::I32TruncF64S => self.visit_i32_trunc_f64s(resources),
            Operator::I32TruncF64U => self.visit_i32_trunc_f64u(resources),
            Operator::I64ExtendI32S => self.visit_i64_extend_i32s(resources),
            Operator::I64ExtendI32U => self.visit_i64_extend_i32u(resources),
            Operator::I64TruncF32S => self.visit_i64_trunc_f32s(resources),
            Operator::I64TruncF32U => self.visit_i64_trunc_f32u(resources),
            Operator::I64TruncF64S => self.visit_i64_trunc_f64s(resources),
            Operator::I64TruncF64U => self.visit_i64_trunc_f64u(resources),
            Operator::F32ConvertI32S => self.visit_f32_convert_i32s(resources),
            Operator::F32ConvertI32U => self.visit_f32_convert_i32u(resources),
            Operator::F32ConvertI64S => self.visit_f32_convert_i64s(resources),
            Operator::F32ConvertI64U => self.visit_f32_convert_i64u(resources),
            Operator::F32DemoteF64 => self.visit_f32_demote_f64(resources),
            Operator::F64ConvertI32S => self.visit_f64_convert_i32s(resources),
            Operator::F64ConvertI32U => self.visit_f64_convert_i32u(resources),
            Operator::F64ConvertI64S => self.visit_f64_convert_i64s(resources),
            Operator::F64ConvertI64U => self.visit_f64_convert_i64u(resources),
            Operator::F64PromoteF32 => self.visit_f64_promote_f32(resources),
            Operator::I32ReinterpretF32 => self.visit_i32_reinterpret_f32(resources),
            Operator::I64ReinterpretF64 => self.visit_i64_reinterpret_f64(resources),
            Operator::F32ReinterpretI32 => self.visit_f32_reinterpret_i32(resources),
            Operator::F64ReinterpretI64 => self.visit_f64_reinterpret_i64(resources),
            Operator::I32Extend8S => self.visit_i32_extend8_s(resources),
            Operator::I32Extend16S => self.visit_i32_extend16_s(resources),
            Operator::I64Extend8S => self.visit_i64_extend8_s(resources),
            Operator::I64Extend16S => self.visit_i64_extend16_s(resources),
            Operator::I64Extend32S => self.visit_i64_extend32_s(resources),
            Operator::I32TruncSatF32S => self.visit_i32_trunc_sat_f32s(resources),
            Operator::I32TruncSatF32U => self.visit_i32_trunc_sat_f32u(resources),
            Operator::I32TruncSatF64S => self.visit_i32_trunc_sat_f64s(resources),
            Operator::I32TruncSatF64U => self.visit_i32_trunc_sat_f64u(resources),
            Operator::I64TruncSatF32S => self.visit_i64_trunc_sat_f32s(resources),
            Operator::I64TruncSatF32U => self.visit_i64_trunc_sat_f32u(resources),
            Operator::I64TruncSatF64S => self.visit_i64_trunc_sat_f64s(resources),
            Operator::I64TruncSatF64U => self.visit_i64_trunc_sat_f64u(resources),
            Operator::MemoryInit { segment, mem } => self.visit_memory_init(resources, segment, mem),
            Operator::DataDrop { segment } => self.visit_data_drop(resources, segment),
            Operator::MemoryCopy { src, dst } => self.visit_memory_copy(resources, src, dst),
            Operator::MemoryFill { mem } => self.visit_memory_fill(resources, mem),
            Operator::TableInit { segment, table } => self.visit_table_init(resources, segment, table),
            Operator::ElemDrop { segment } => self.visit_elem_drop(resources, segment),
            Operator::TableCopy { dst_table, src_table } => self.visit_table_copy(resources, dst_table, src_table),
            Operator::TableFill { table } => self.visit_table_fill(resources, table),
            Operator::TableGet { table } => self.visit_table_get(resources, table),
            Operator::TableSet { table } => self.visit_table_set(resources, table),
            Operator::TableGrow { table } => self.visit_table_grow(resources, table),
            Operator::TableSize { table } => self.visit_table_size(resources, table),
            Operator::MemoryAtomicNotify { memarg } => self.visit_memory_atomic_notify(resources, memarg),
            Operator::MemoryAtomicWait32 { memarg } => self.visit_memory_atomic_wait32(resources, memarg),
            Operator::MemoryAtomicWait64 { memarg } => self.visit_memory_atomic_wait64(resources, memarg),
            Operator::AtomicFence { flags } => self.visit_atomic_fence(resources, flags),
            Operator::I32AtomicLoad { memarg } => self.visit_i32_atomic_load(resources, memarg),
            Operator::I64AtomicLoad { memarg } => self.visit_i64_atomic_load(resources, memarg),
            Operator::I32AtomicLoad8U { memarg } => self.visit_i32_atomic_load8_u(resources, memarg),
            Operator::I32AtomicLoad16U { memarg } => self.visit_i32_atomic_load16_u(resources, memarg),
            Operator::I64AtomicLoad8U { memarg } => self.visit_i64_atomic_load8_u(resources, memarg),
            Operator::I64AtomicLoad16U { memarg } => self.visit_i64_atomic_load16_u(resources, memarg),
            Operator::I64AtomicLoad32U { memarg } => self.visit_i64_atomic_load32_u(resources, memarg),
            Operator::I32AtomicStore { memarg } => self.visit_i32_atomic_store(resources, memarg),
            Operator::I64AtomicStore { memarg } => self.visit_i64_atomic_store(resources, memarg),
            Operator::I32AtomicStore8 { memarg } => self.visit_i32_atomic_store8(resources, memarg),
            Operator::I32AtomicStore16 { memarg } => self.visit_i32_atomic_store16(resources, memarg),
            Operator::I64AtomicStore8 { memarg } => self.visit_i64_atomic_store8(resources, memarg),
            Operator::I64AtomicStore16 { memarg } => self.visit_i64_atomic_store16(resources, memarg),
            Operator::I64AtomicStore32 { memarg } => self.visit_i64_atomic_store32(resources, memarg),
            Operator::I32AtomicRmwAdd { memarg } => self.visit_i32_atomic_rmw_add(resources, memarg),
            Operator::I64AtomicRmwAdd { memarg } => self.visit_i64_atomic_rmw_add(resources, memarg),
            Operator::I32AtomicRmw8AddU { memarg } => self.visit_i32_atomic_rmw8_add_u(resources, memarg),
            Operator::I32AtomicRmw16AddU { memarg } => self.visit_i32_atomic_rmw16_add_u(resources, memarg),
            Operator::I64AtomicRmw8AddU { memarg } => self.visit_i64_atomic_rmw8_add_u(resources, memarg),
            Operator::I64AtomicRmw16AddU { memarg } => self.visit_i64_atomic_rmw16_add_u(resources, memarg),
            Operator::I64AtomicRmw32AddU { memarg } => self.visit_i64_atomic_rmw32_add_u(resources, memarg),
            Operator::I32AtomicRmwSub { memarg } => self.visit_i32_atomic_rmw_sub(resources, memarg),
            Operator::I64AtomicRmwSub { memarg } => self.visit_i64_atomic_rmw_sub(resources, memarg),
            Operator::I32AtomicRmw8SubU { memarg } => self.visit_i32_atomic_rmw8_sub_u(resources, memarg),
            Operator::I32AtomicRmw16SubU { memarg } => self.visit_i32_atomic_rmw16_sub_u(resources, memarg),
            Operator::I64AtomicRmw8SubU { memarg } => self.visit_i64_atomic_rmw8_sub_u(resources, memarg),
            Operator::I64AtomicRmw16SubU { memarg } => self.visit_i64_atomic_rmw16_sub_u(resources, memarg),
            Operator::I64AtomicRmw32SubU { memarg } => self.visit_i64_atomic_rmw32_sub_u(resources, memarg),
            Operator::I32AtomicRmwAnd { memarg } => self.visit_i32_atomic_rmw_and(resources, memarg),
            Operator::I64AtomicRmwAnd { memarg } => self.visit_i64_atomic_rmw_and(resources, memarg),
            Operator::I32AtomicRmw8AndU { memarg } => self.visit_i32_atomic_rmw8_and_u(resources, memarg),
            Operator::I32AtomicRmw16AndU { memarg } => self.visit_i32_atomic_rmw16_and_u(resources, memarg),
            Operator::I64AtomicRmw8AndU { memarg } => self.visit_i64_atomic_rmw8_and_u(resources, memarg),
            Operator::I64AtomicRmw16AndU { memarg } => self.visit_i64_atomic_rmw16_and_u(resources, memarg),
            Operator::I64AtomicRmw32AndU { memarg } => self.visit_i64_atomic_rmw32_and_u(resources, memarg),
            Operator::I32AtomicRmwOr { memarg } => self.visit_i32_atomic_rmw_or(resources, memarg),
            Operator::I64AtomicRmwOr { memarg } => self.visit_i64_atomic_rmw_or(resources, memarg),
            Operator::I32AtomicRmw8OrU { memarg } => self.visit_i32_atomic_rmw8_or_u(resources, memarg),
            Operator::I32AtomicRmw16OrU { memarg } => self.visit_i32_atomic_rmw16_or_u(resources, memarg),
            Operator::I64AtomicRmw8OrU { memarg } => self.visit_i64_atomic_rmw8_or_u(resources, memarg),
            Operator::I64AtomicRmw16OrU { memarg } => self.visit_i64_atomic_rmw16_or_u(resources, memarg),
            Operator::I64AtomicRmw32OrU { memarg } => self.visit_i64_atomic_rmw32_or_u(resources, memarg),
            Operator::I32AtomicRmwXor { memarg } => self.visit_i32_atomic_rmw_xor(resources, memarg),
            Operator::I64AtomicRmwXor { memarg } => self.visit_i64_atomic_rmw_xor(resources, memarg),
            Operator::I32AtomicRmw8XorU { memarg } => self.visit_i32_atomic_rmw8_xor_u(resources, memarg),
            Operator::I32AtomicRmw16XorU { memarg } => self.visit_i32_atomic_rmw16_xor_u(resources, memarg),
            Operator::I64AtomicRmw8XorU { memarg } => self.visit_i64_atomic_rmw8_xor_u(resources, memarg),
            Operator::I64AtomicRmw16XorU { memarg } => self.visit_i64_atomic_rmw16_xor_u(resources, memarg),
            Operator::I64AtomicRmw32XorU { memarg } => self.visit_i64_atomic_rmw32_xor_u(resources, memarg),
            Operator::I32AtomicRmwXchg { memarg } => self.visit_i32_atomic_rmw_xchg(resources, memarg),
            Operator::I64AtomicRmwXchg { memarg } => self.visit_i64_atomic_rmw_xchg(resources, memarg),
            Operator::I32AtomicRmw8XchgU { memarg } => self.visit_i32_atomic_rmw8_xchg_u(resources, memarg),
            Operator::I32AtomicRmw16XchgU { memarg } => self.visit_i32_atomic_rmw16_xchg_u(resources, memarg),
            Operator::I64AtomicRmw8XchgU { memarg } => self.visit_i64_atomic_rmw8_xchg_u(resources, memarg),
            Operator::I64AtomicRmw16XchgU { memarg } => self.visit_i64_atomic_rmw16_xchg_u(resources, memarg),
            Operator::I64AtomicRmw32XchgU { memarg } => self.visit_i64_atomic_rmw32_xchg_u(resources, memarg),
            Operator::I32AtomicRmwCmpxchg { memarg } => self.visit_i32_atomic_rmw_cmpxchg(resources, memarg),
            Operator::I64AtomicRmwCmpxchg { memarg } => self.visit_i64_atomic_rmw_cmpxchg(resources, memarg),
            Operator::I32AtomicRmw8CmpxchgU { memarg } => self.visit_i32_atomic_rmw8_cmpxchg_u(resources, memarg),
            Operator::I32AtomicRmw16CmpxchgU { memarg } => self.visit_i32_atomic_rmw16_cmpxchg_u(resources, memarg),
            Operator::I64AtomicRmw8CmpxchgU { memarg } => self.visit_i64_atomic_rmw8_cmpxchg_u(resources, memarg),
            Operator::I64AtomicRmw16CmpxchgU { memarg } => self.visit_i64_atomic_rmw16_cmpxchg_u(resources, memarg),
            Operator::I64AtomicRmw32CmpxchgU { memarg } => self.visit_i64_atomic_rmw32_cmpxchg_u(resources, memarg),
            Operator::V128Load { memarg } => self.visit_v128_load(resources, memarg),
            Operator::V128Load8x8S { memarg } => self.visit_v128_load8x8_s(resources, memarg),
            Operator::V128Load8x8U { memarg } => self.visit_v128_load8x8_u(resources, memarg),
            Operator::V128Load16x4S { memarg } => self.visit_v128_load16x4_s(resources, memarg),
            Operator::V128Load16x4U { memarg } => self.visit_v128_load16x4_u(resources, memarg),
            Operator::V128Load32x2S { memarg } => self.visit_v128_load32x2_s(resources, memarg),
            Operator::V128Load32x2U { memarg } => self.visit_v128_load32x2_u(resources, memarg),
            Operator::V128Load8Splat { memarg } => self.visit_v128_load8_splat(resources, memarg),
            Operator::V128Load16Splat { memarg } => self.visit_v128_load16_splat(resources, memarg),
            Operator::V128Load32Splat { memarg } => self.visit_v128_load32_splat(resources, memarg),
            Operator::V128Load64Splat { memarg } => self.visit_v128_load64_splat(resources, memarg),
            Operator::V128Load32Zero { memarg } => self.visit_v128_load32_zero(resources, memarg),
            Operator::V128Load64Zero { memarg } => self.visit_v128_load64_zero(resources, memarg),
            Operator::V128Store { memarg } => self.visit_v128_store(resources, memarg),
            Operator::V128Load8Lane { memarg, lane } => self.visit_v128_load8_lane(resources, memarg, lane),
            Operator::V128Load16Lane { memarg, lane } => self.visit_v128_load16_lane(resources, memarg, lane),
            Operator::V128Load32Lane { memarg, lane } => self.visit_v128_load32_lane(resources, memarg, lane),
            Operator::V128Load64Lane { memarg, lane } => self.visit_v128_load64_lane(resources, memarg, lane),
            Operator::V128Store8Lane { memarg, lane } => self.visit_v128_store8_lane(resources, memarg, lane),
            Operator::V128Store16Lane { memarg, lane } => self.visit_v128_store16_lane(resources, memarg, lane),
            Operator::V128Store32Lane { memarg, lane } => self.visit_v128_store32_lane(resources, memarg, lane),
            Operator::V128Store64Lane { memarg, lane } => self.visit_v128_store64_lane(resources, memarg, lane),
            Operator::V128Const { value } => self.visit_v128_const(resources, value),
            Operator::I8x16Shuffle { lanes } => self.visit_i8x16_shuffle(resources, lanes),
            Operator::I8x16ExtractLaneS { lane } => self.visit_i8x16_extract_lane_s(resources, lane),
            Operator::I8x16ExtractLaneU { lane } => self.visit_i8x16_extract_lane_u(resources, lane),
            Operator::I8x16ReplaceLane { lane } => self.visit_i8x16_replace_lane(resources, lane),
            Operator::I16x8ExtractLaneS { lane } => self.visit_i16x8_extract_lane_s(resources, lane),
            Operator::I16x8ExtractLaneU { lane } => self.visit_i16x8_extract_lane_u(resources, lane),
            Operator::I16x8ReplaceLane { lane } => self.visit_i16x8_replace_lane(resources, lane),
            Operator::I32x4ExtractLane { lane } => self.visit_i32x4_extract_lane(resources, lane),
            Operator::I32x4ReplaceLane { lane } => self.visit_i32x4_replace_lane(resources, lane),
            Operator::I64x2ExtractLane { lane } => self.visit_i64x2_extract_lane(resources, lane),
            Operator::I64x2ReplaceLane { lane } => self.visit_i64x2_replace_lane(resources, lane),
            Operator::F32x4ExtractLane { lane } => self.visit_f32x4_extract_lane(resources, lane),
            Operator::F32x4ReplaceLane { lane } => self.visit_f32x4_replace_lane(resources, lane),
            Operator::F64x2ExtractLane { lane } => self.visit_f64x2_extract_lane(resources, lane),
            Operator::F64x2ReplaceLane { lane } => self.visit_f64x2_replace_lane(resources, lane),
            Operator::I8x16Swizzle => self.visit_i8x16_swizzle(resources),
            Operator::I8x16Splat => self.visit_i8x16_splat(resources),
            Operator::I16x8Splat => self.visit_i16x8_splat(resources),
            Operator::I32x4Splat => self.visit_i32x4_splat(resources),
            Operator::I64x2Splat => self.visit_i64x2_splat(resources),
            Operator::F32x4Splat => self.visit_f32x4_splat(resources),
            Operator::F64x2Splat => self.visit_f64x2_splat(resources),
            Operator::I8x16Eq => self.visit_i8x16_eq(resources),
            Operator::I8x16Ne => self.visit_i8x16_ne(resources),
            Operator::I8x16LtS => self.visit_i8x16_lt_s(resources),
            Operator::I8x16LtU => self.visit_i8x16_lt_u(resources),
            Operator::I8x16GtS => self.visit_i8x16_gt_s(resources),
            Operator::I8x16GtU => self.visit_i8x16_gt_u(resources),
            Operator::I8x16LeS => self.visit_i8x16_le_s(resources),
            Operator::I8x16LeU => self.visit_i8x16_le_u(resources),
            Operator::I8x16GeS => self.visit_i8x16_ge_s(resources),
            Operator::I8x16GeU => self.visit_i8x16_ge_u(resources),
            Operator::I16x8Eq => self.visit_i16x8_eq(resources),
            Operator::I16x8Ne => self.visit_i16x8_ne(resources),
            Operator::I16x8LtS => self.visit_i16x8_lt_s(resources),
            Operator::I16x8LtU => self.visit_i16x8_lt_u(resources),
            Operator::I16x8GtS => self.visit_i16x8_gt_s(resources),
            Operator::I16x8GtU => self.visit_i16x8_gt_u(resources),
            Operator::I16x8LeS => self.visit_i16x8_le_s(resources),
            Operator::I16x8LeU => self.visit_i16x8_le_u(resources),
            Operator::I16x8GeS => self.visit_i16x8_ge_s(resources),
            Operator::I16x8GeU => self.visit_i16x8_ge_u(resources),
            Operator::I32x4Eq => self.visit_i32x4_eq(resources),
            Operator::I32x4Ne => self.visit_i32x4_ne(resources),
            Operator::I32x4LtS => self.visit_i32x4_lt_s(resources),
            Operator::I32x4LtU => self.visit_i32x4_lt_u(resources),
            Operator::I32x4GtS => self.visit_i32x4_gt_s(resources),
            Operator::I32x4GtU => self.visit_i32x4_gt_u(resources),
            Operator::I32x4LeS => self.visit_i32x4_le_s(resources),
            Operator::I32x4LeU => self.visit_i32x4_le_u(resources),
            Operator::I32x4GeS => self.visit_i32x4_ge_s(resources),
            Operator::I32x4GeU => self.visit_i32x4_ge_u(resources),
            Operator::I64x2Eq => self.visit_i64x2_eq(resources),
            Operator::I64x2Ne => self.visit_i64x2_ne(resources),
            Operator::I64x2LtS => self.visit_i64x2_lt_s(resources),
            Operator::I64x2GtS => self.visit_i64x2_gt_s(resources),
            Operator::I64x2LeS => self.visit_i64x2_le_s(resources),
            Operator::I64x2GeS => self.visit_i64x2_ge_s(resources),
            Operator::F32x4Eq => self.visit_f32x4_eq(resources),
            Operator::F32x4Ne => self.visit_f32x4_ne(resources),
            Operator::F32x4Lt => self.visit_f32x4_lt(resources),
            Operator::F32x4Gt => self.visit_f32x4_gt(resources),
            Operator::F32x4Le => self.visit_f32x4_le(resources),
            Operator::F32x4Ge => self.visit_f32x4_ge(resources),
            Operator::F64x2Eq => self.visit_f64x2_eq(resources),
            Operator::F64x2Ne => self.visit_f64x2_ne(resources),
            Operator::F64x2Lt => self.visit_f64x2_lt(resources),
            Operator::F64x2Gt => self.visit_f64x2_gt(resources),
            Operator::F64x2Le => self.visit_f64x2_le(resources),
            Operator::F64x2Ge => self.visit_f64x2_ge(resources),
            Operator::V128Not => self.visit_v128_not(resources),
            Operator::V128And => self.visit_v128_and(resources),
            Operator::V128AndNot => self.visit_v128_and_not(resources),
            Operator::V128Or => self.visit_v128_or(resources),
            Operator::V128Xor => self.visit_v128_xor(resources),
            Operator::V128Bitselect => self.visit_v128_bitselect(resources),
            Operator::V128AnyTrue => self.visit_v128_any_true(resources),
            Operator::I8x16Abs => self.visit_i8x16_abs(resources),
            Operator::I8x16Neg => self.visit_i8x16_neg(resources),
            Operator::I8x16Popcnt => self.visit_i8x16_popcnt(resources),
            Operator::I8x16AllTrue => self.visit_i8x16_all_true(resources),
            Operator::I8x16Bitmask => self.visit_i8x16_bitmask(resources),
            Operator::I8x16NarrowI16x8S => self.visit_i8x16_narrow_i16x8_s(resources),
            Operator::I8x16NarrowI16x8U => self.visit_i8x16_narrow_i16x8_u(resources),
            Operator::I8x16Shl => self.visit_i8x16_shl(resources),
            Operator::I8x16ShrS => self.visit_i8x16_shr_s(resources),
            Operator::I8x16ShrU => self.visit_i8x16_shr_u(resources),
            Operator::I8x16Add => self.visit_i8x16_add(resources),
            Operator::I8x16AddSatS => self.visit_i8x16_add_sat_s(resources),
            Operator::I8x16AddSatU => self.visit_i8x16_add_sat_u(resources),
            Operator::I8x16Sub => self.visit_i8x16_sub(resources),
            Operator::I8x16SubSatS => self.visit_i8x16_sub_sat_s(resources),
            Operator::I8x16SubSatU => self.visit_i8x16_sub_sat_u(resources),
            Operator::I8x16MinS => self.visit_i8x16_min_s(resources),
            Operator::I8x16MinU => self.visit_i8x16_min_u(resources),
            Operator::I8x16MaxS => self.visit_i8x16_max_s(resources),
            Operator::I8x16MaxU => self.visit_i8x16_max_u(resources),
            Operator::I8x16RoundingAverageU => self.visit_i8x16_rounding_average_u(resources),
            Operator::I16x8ExtAddPairwiseI8x16S => self.visit_i16x8_ext_add_pairwise_i8x16_s(resources),
            Operator::I16x8ExtAddPairwiseI8x16U => self.visit_i16x8_ext_add_pairwise_i8x16_u(resources),
            Operator::I16x8Abs => self.visit_i16x8_abs(resources),
            Operator::I16x8Neg => self.visit_i16x8_neg(resources),
            Operator::I16x8Q15MulrSatS => self.visit_i16x8_q15_mulr_sat_s(resources),
            Operator::I16x8AllTrue => self.visit_i16x8_all_true(resources),
            Operator::I16x8Bitmask => self.visit_i16x8_bitmask(resources),
            Operator::I16x8NarrowI32x4S => self.visit_i16x8_narrow_i32x4_s(resources),
            Operator::I16x8NarrowI32x4U => self.visit_i16x8_narrow_i32x4_u(resources),
            Operator::I16x8ExtendLowI8x16S => self.visit_i16x8_extend_low_i8x16_s(resources),
            Operator::I16x8ExtendHighI8x16S => self.visit_i16x8_extend_high_i8x16_s(resources),
            Operator::I16x8ExtendLowI8x16U => self.visit_i16x8_extend_low_i8x16_u(resources),
            Operator::I16x8ExtendHighI8x16U => self.visit_i16x8_extend_high_i8x16_u(resources),
            Operator::I16x8Shl => self.visit_i16x8_shl(resources),
            Operator::I16x8ShrS => self.visit_i16x8_shr_s(resources),
            Operator::I16x8ShrU => self.visit_i16x8_shr_u(resources),
            Operator::I16x8Add => self.visit_i16x8_add(resources),
            Operator::I16x8AddSatS => self.visit_i16x8_add_sat_s(resources),
            Operator::I16x8AddSatU => self.visit_i16x8_add_sat_u(resources),
            Operator::I16x8Sub => self.visit_i16x8_sub(resources),
            Operator::I16x8SubSatS => self.visit_i16x8_sub_sat_s(resources),
            Operator::I16x8SubSatU => self.visit_i16x8_sub_sat_u(resources),
            Operator::I16x8Mul => self.visit_i16x8_mul(resources),
            Operator::I16x8MinS => self.visit_i16x8_min_s(resources),
            Operator::I16x8MinU => self.visit_i16x8_min_u(resources),
            Operator::I16x8MaxS => self.visit_i16x8_max_s(resources),
            Operator::I16x8MaxU => self.visit_i16x8_max_u(resources),
            Operator::I16x8RoundingAverageU => self.visit_i16x8_rounding_average_u(resources),
            Operator::I16x8ExtMulLowI8x16S => self.visit_i16x8_ext_mul_low_i8x16_s(resources),
            Operator::I16x8ExtMulHighI8x16S => self.visit_i16x8_ext_mul_high_i8x16_s(resources),
            Operator::I16x8ExtMulLowI8x16U => self.visit_i16x8_ext_mul_low_i8x16_u(resources),
            Operator::I16x8ExtMulHighI8x16U => self.visit_i16x8_ext_mul_high_i8x16_u(resources),
            Operator::I32x4ExtAddPairwiseI16x8S => self.visit_i32x4_ext_add_pairwise_i16x8_s(resources),
            Operator::I32x4ExtAddPairwiseI16x8U => self.visit_i32x4_ext_add_pairwise_i16x8_u(resources),
            Operator::I32x4Abs => self.visit_i32x4_abs(resources),
            Operator::I32x4Neg => self.visit_i32x4_neg(resources),
            Operator::I32x4AllTrue => self.visit_i32x4_all_true(resources),
            Operator::I32x4Bitmask => self.visit_i32x4_bitmask(resources),
            Operator::I32x4ExtendLowI16x8S => self.visit_i32x4_extend_low_i16x8_s(resources),
            Operator::I32x4ExtendHighI16x8S => self.visit_i32x4_extend_high_i16x8_s(resources),
            Operator::I32x4ExtendLowI16x8U => self.visit_i32x4_extend_low_i16x8_u(resources),
            Operator::I32x4ExtendHighI16x8U => self.visit_i32x4_extend_high_i16x8_u(resources),
            Operator::I32x4Shl => self.visit_i32x4_shl(resources),
            Operator::I32x4ShrS => self.visit_i32x4_shr_s(resources),
            Operator::I32x4ShrU => self.visit_i32x4_shr_u(resources),
            Operator::I32x4Add => self.visit_i32x4_add(resources),
            Operator::I32x4Sub => self.visit_i32x4_sub(resources),
            Operator::I32x4Mul => self.visit_i32x4_mul(resources),
            Operator::I32x4MinS => self.visit_i32x4_min_s(resources),
            Operator::I32x4MinU => self.visit_i32x4_min_u(resources),
            Operator::I32x4MaxS => self.visit_i32x4_max_s(resources),
            Operator::I32x4MaxU => self.visit_i32x4_max_u(resources),
            Operator::I32x4DotI16x8S => self.visit_i32x4_dot_i16x8_s(resources),
            Operator::I32x4ExtMulLowI16x8S => self.visit_i32x4_ext_mul_low_i16x8_s(resources),
            Operator::I32x4ExtMulHighI16x8S => self.visit_i32x4_ext_mul_high_i16x8_s(resources),
            Operator::I32x4ExtMulLowI16x8U => self.visit_i32x4_ext_mul_low_i16x8_u(resources),
            Operator::I32x4ExtMulHighI16x8U => self.visit_i32x4_ext_mul_high_i16x8_u(resources),
            Operator::I64x2Abs => self.visit_i64x2_abs(resources),
            Operator::I64x2Neg => self.visit_i64x2_neg(resources),
            Operator::I64x2AllTrue => self.visit_i64x2_all_true(resources),
            Operator::I64x2Bitmask => self.visit_i64x2_bitmask(resources),
            Operator::I64x2ExtendLowI32x4S => self.visit_i64x2_extend_low_i32x4_s(resources),
            Operator::I64x2ExtendHighI32x4S => self.visit_i64x2_extend_high_i32x4_s(resources),
            Operator::I64x2ExtendLowI32x4U => self.visit_i64x2_extend_low_i32x4_u(resources),
            Operator::I64x2ExtendHighI32x4U => self.visit_i64x2_extend_high_i32x4_u(resources),
            Operator::I64x2Shl => self.visit_i64x2_shl(resources),
            Operator::I64x2ShrS => self.visit_i64x2_shr_s(resources),
            Operator::I64x2ShrU => self.visit_i64x2_shr_u(resources),
            Operator::I64x2Add => self.visit_i64x2_add(resources),
            Operator::I64x2Sub => self.visit_i64x2_sub(resources),
            Operator::I64x2Mul => self.visit_i64x2_mul(resources),
            Operator::I64x2ExtMulLowI32x4S => self.visit_i64x2_ext_mul_low_i32x4_s(resources),
            Operator::I64x2ExtMulHighI32x4S => self.visit_i64x2_ext_mul_high_i32x4_s(resources),
            Operator::I64x2ExtMulLowI32x4U => self.visit_i64x2_ext_mul_low_i32x4_u(resources),
            Operator::I64x2ExtMulHighI32x4U => self.visit_i64x2_ext_mul_high_i32x4_u(resources),
            Operator::F32x4Ceil => self.visit_f32x4_ceil(resources),
            Operator::F32x4Floor => self.visit_f32x4_floor(resources),
            Operator::F32x4Trunc => self.visit_f32x4_trunc(resources),
            Operator::F32x4Nearest => self.visit_f32x4_nearest(resources),
            Operator::F32x4Abs => self.visit_f32x4_abs(resources),
            Operator::F32x4Neg => self.visit_f32x4_neg(resources),
            Operator::F32x4Sqrt => self.visit_f32x4_sqrt(resources),
            Operator::F32x4Add => self.visit_f32x4_add(resources),
            Operator::F32x4Sub => self.visit_f32x4_sub(resources),
            Operator::F32x4Mul => self.visit_f32x4_mul(resources),
            Operator::F32x4Div => self.visit_f32x4_div(resources),
            Operator::F32x4Min => self.visit_f32x4_min(resources),
            Operator::F32x4Max => self.visit_f32x4_max(resources),
            Operator::F32x4PMin => self.visit_f32x4_p_min(resources),
            Operator::F32x4PMax => self.visit_f32x4_p_max(resources),
            Operator::F64x2Ceil => self.visit_f64x2_ceil(resources),
            Operator::F64x2Floor => self.visit_f64x2_floor(resources),
            Operator::F64x2Trunc => self.visit_f64x2_trunc(resources),
            Operator::F64x2Nearest => self.visit_f64x2_nearest(resources),
            Operator::F64x2Abs => self.visit_f64x2_abs(resources),
            Operator::F64x2Neg => self.visit_f64x2_neg(resources),
            Operator::F64x2Sqrt => self.visit_f64x2_sqrt(resources),
            Operator::F64x2Add => self.visit_f64x2_add(resources),
            Operator::F64x2Sub => self.visit_f64x2_sub(resources),
            Operator::F64x2Mul => self.visit_f64x2_mul(resources),
            Operator::F64x2Div => self.visit_f64x2_div(resources),
            Operator::F64x2Min => self.visit_f64x2_min(resources),
            Operator::F64x2Max => self.visit_f64x2_max(resources),
            Operator::F64x2PMin => self.visit_f64x2_p_min(resources),
            Operator::F64x2PMax => self.visit_f64x2_p_max(resources),
            Operator::I32x4TruncSatF32x4S => self.visit_i32x4_trunc_sat_f32x4_s(resources),
            Operator::I32x4TruncSatF32x4U => self.visit_i32x4_trunc_sat_f32x4_u(resources),
            Operator::F32x4ConvertI32x4S => self.visit_f32x4_convert_i32x4_s(resources),
            Operator::F32x4ConvertI32x4U => self.visit_f32x4_convert_i32x4_u(resources),
            Operator::I32x4TruncSatF64x2SZero => self.visit_i32x4_trunc_sat_f64x2_s_zero(resources),
            Operator::I32x4TruncSatF64x2UZero => self.visit_i32x4_trunc_sat_f64x2_u_zero(resources),
            Operator::F64x2ConvertLowI32x4S => self.visit_f64x2_convert_low_i32x4_s(resources),
            Operator::F64x2ConvertLowI32x4U => self.visit_f64x2_convert_low_i32x4_u(resources),
            Operator::F32x4DemoteF64x2Zero => self.visit_f32x4_demote_f64x2_zero(resources),
            Operator::F64x2PromoteLowF32x4 => self.visit_f64x2_promote_low_f32x4(resources),
            Operator::I8x16RelaxedSwizzle => self.visit_i8x16_relaxed_swizzle(resources),
            Operator::I32x4RelaxedTruncSatF32x4S => self.visit_i32x4_relaxed_trunc_sat_f32x4_s(resources),
            Operator::I32x4RelaxedTruncSatF32x4U => self.visit_i32x4_relaxed_trunc_sat_f32x4_u(resources),
            Operator::I32x4RelaxedTruncSatF64x2SZero => self.visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(resources),
            Operator::I32x4RelaxedTruncSatF64x2UZero => self.visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(resources),
            Operator::F32x4Fma => self.visit_f32x4_fma(resources),
            Operator::F32x4Fms => self.visit_f32x4_fms(resources),
            Operator::F64x2Fma => self.visit_f64x2_fma(resources),
            Operator::F64x2Fms => self.visit_f64x2_fms(resources),
            Operator::I8x16LaneSelect => self.visit_i8x16_lane_select(resources),
            Operator::I16x8LaneSelect => self.visit_i16x8_lane_select(resources),
            Operator::I32x4LaneSelect => self.visit_i32x4_lane_select(resources),
            Operator::I64x2LaneSelect => self.visit_i64x2_lane_select(resources),
            Operator::F32x4RelaxedMin => self.visit_f32x4_relaxed_min(resources),
            Operator::F32x4RelaxedMax => self.visit_f32x4_relaxed_max(resources),
            Operator::F64x2RelaxedMin => self.visit_f64x2_relaxed_min(resources),
            Operator::F64x2RelaxedMax => self.visit_f64x2_relaxed_max(resources),
        }
    }

    pub fn finish(&mut self) -> OperatorValidatorResult<()> {
        if self.control.last().is_some() {
            bail_op_err!("control frames remain at end of function: END opcode expected");
        }
        Ok(())
    }
}

fn func_type_at<T: WasmModuleResources>(
    resources: &T,
    at: u32,
) -> OperatorValidatorResult<&T::FuncType> {
    resources
        .func_type_at(at)
        .ok_or_else(|| OperatorValidatorError::new("unknown type: type index out of bounds"))
}

fn tag_at<T: WasmModuleResources>(resources: &T, at: u32) -> OperatorValidatorResult<&T::FuncType> {
    resources.tag_at(at).ok_or_else(|| {
        OperatorValidatorError::new(format!("unknown tag {}: tag index out of bounds", at))
    })
}

enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B> Iterator for Either<A, B>
where
    A: Iterator,
    B: Iterator<Item = A::Item>,
{
    type Item = A::Item;
    fn next(&mut self) -> Option<A::Item> {
        match self {
            Either::A(a) => a.next(),
            Either::B(b) => b.next(),
        }
    }
}

impl<A, B> DoubleEndedIterator for Either<A, B>
where
    A: DoubleEndedIterator,
    B: DoubleEndedIterator<Item = A::Item>,
{
    fn next_back(&mut self) -> Option<A::Item> {
        match self {
            Either::A(a) => a.next_back(),
            Either::B(b) => b.next_back(),
        }
    }
}

impl<A, B> ExactSizeIterator for Either<A, B>
where
    A: ExactSizeIterator,
    B: ExactSizeIterator<Item = A::Item>,
{
    fn len(&self) -> usize {
        match self {
            Either::A(a) => a.len(),
            Either::B(b) => b.len(),
        }
    }
}

trait PreciseIterator: ExactSizeIterator + DoubleEndedIterator {}
impl<T: ExactSizeIterator + DoubleEndedIterator> PreciseIterator for T {}

fn params(
    ty: BlockType,
    resources: &impl WasmModuleResources,
) -> OperatorValidatorResult<impl PreciseIterator<Item = ValType> + '_> {
    Ok(match ty {
        BlockType::Empty | BlockType::Type(_) => Either::B(None.into_iter()),
        BlockType::FuncType(t) => Either::A(func_type_at(resources, t)?.inputs()),
    })
}

fn results(
    ty: BlockType,
    resources: &impl WasmModuleResources,
) -> OperatorValidatorResult<impl PreciseIterator<Item = ValType> + '_> {
    Ok(match ty {
        BlockType::Empty => Either::B(None.into_iter()),
        BlockType::Type(t) => Either::B(Some(t).into_iter()),
        BlockType::FuncType(t) => Either::A(func_type_at(resources, t)?.outputs()),
    })
}

fn label_types(
    ty: BlockType,
    resources: &impl WasmModuleResources,
    kind: FrameKind,
) -> OperatorValidatorResult<impl PreciseIterator<Item = ValType> + '_> {
    Ok(match kind {
        FrameKind::Loop => Either::A(params(ty, resources)?),
        _ => Either::B(results(ty, resources)?),
    })
}

fn ty_to_str(ty: ValType) -> &'static str {
    match ty {
        ValType::I32 => "i32",
        ValType::I64 => "i64",
        ValType::F32 => "f32",
        ValType::F64 => "f64",
        ValType::V128 => "v128",
        ValType::FuncRef => "funcref",
        ValType::ExternRef => "externref",
    }
}

impl<'a, I> VisitOperator<&'a I> for OperatorValidator
where
    I: WasmModuleResources,
{
    type Output = OperatorValidatorResult<()>;

    fn visit_nop(&mut self, _resources: &I) -> Self::Output {
        Ok(())
    }
    fn visit_unreachable(&mut self, _resources: &I) -> Self::Output {
        self.unreachable();
        Ok(())
    }
    fn visit_block(&mut self, resources: &I, ty: BlockType) -> Self::Output {
        self.check_block_type(ty, resources)?;
        for ty in params(ty, resources)?.rev() {
            self.pop_operand(Some(ty))?;
        }
        self.push_ctrl(FrameKind::Block, ty, resources)?;
        Ok(())
    }
    fn visit_loop(&mut self, resources: &I, ty: BlockType) -> Self::Output {
        self.check_block_type(ty, resources)?;
        for ty in params(ty, resources)?.rev() {
            self.pop_operand(Some(ty))?;
        }
        self.push_ctrl(FrameKind::Loop, ty, resources)?;
        Ok(())
    }
    fn visit_if(&mut self, resources: &I, ty: BlockType) -> Self::Output {
        self.check_block_type(ty, resources)?;
        self.pop_operand(Some(ValType::I32))?;
        for ty in params(ty, resources)?.rev() {
            self.pop_operand(Some(ty))?;
        }
        self.push_ctrl(FrameKind::If, ty, resources)?;
        Ok(())
    }
    fn visit_else(&mut self, resources: &I) -> Self::Output {
        let frame = self.pop_ctrl(resources)?;
        if frame.kind != FrameKind::If {
            bail_op_err!("else found outside of an `if` block");
        }
        self.push_ctrl(FrameKind::Else, frame.block_type, resources)?;
        Ok(())
    }
    fn visit_try(&mut self, resources: &I, ty: BlockType) -> Self::Output {
        self.check_exceptions_enabled()?;
        self.check_block_type(ty, resources)?;
        for ty in params(ty, resources)?.rev() {
            self.pop_operand(Some(ty))?;
        }
        self.push_ctrl(FrameKind::Try, ty, resources)?;
        Ok(())
    }
    fn visit_catch(&mut self, resources: &I, index: u32) -> Self::Output {
        self.check_exceptions_enabled()?;
        let frame = self.pop_ctrl(resources)?;
        if frame.kind != FrameKind::Try && frame.kind != FrameKind::Catch {
            bail_op_err!("catch found outside of an `try` block");
        }
        // Start a new frame and push `exnref` value.
        self.control.push(Frame {
            kind: FrameKind::Catch,
            block_type: frame.block_type,
            height: self.operands.len(),
            unreachable: false,
        });
        // Push exception argument types.
        let ty = tag_at(&resources, index)?;
        for ty in ty.inputs() {
            self.push_operand(ty)?;
        }
        Ok(())
    }
    fn visit_throw(&mut self, resources: &I, index: u32) -> Self::Output {
        self.check_exceptions_enabled()?;
        // Check values associated with the exception.
        let ty = tag_at(&resources, index)?;
        for ty in ty.inputs().rev() {
            self.pop_operand(Some(ty))?;
        }
        if ty.outputs().len() > 0 {
            bail_op_err!("result type expected to be empty for exception");
        }
        self.unreachable();
        Ok(())
    }
    fn visit_rethrow(&mut self, _resources: &I, relative_depth: u32) -> Self::Output {
        self.check_exceptions_enabled()?;
        // This is not a jump, but we need to check that the `rethrow`
        // targets an actual `catch` to get the exception.
        let (_, kind) = self.jump(relative_depth)?;
        if kind != FrameKind::Catch && kind != FrameKind::CatchAll {
            bail_op_err!("invalid rethrow label: target was not a `catch` block");
        }
        self.unreachable();
        Ok(())
    }
    fn visit_delegate(&mut self, resources: &I, relative_depth: u32) -> Self::Output {
        self.check_exceptions_enabled()?;
        let frame = self.pop_ctrl(resources)?;
        if frame.kind != FrameKind::Try {
            bail_op_err!("delegate found outside of an `try` block");
        }
        // This operation is not a jump, but we need to check the
        // depth for validity
        let _ = self.jump(relative_depth)?;
        for ty in results(frame.block_type, resources)? {
            self.push_operand(ty)?;
        }
        Ok(())
    }
    fn visit_catch_all(&mut self, resources: &I) -> Self::Output {
        self.check_exceptions_enabled()?;
        let frame = self.pop_ctrl(resources)?;
        if frame.kind == FrameKind::CatchAll {
            bail_op_err!("only one catch_all allowed per `try` block");
        } else if frame.kind != FrameKind::Try && frame.kind != FrameKind::Catch {
            bail_op_err!("catch_all found outside of a `try` block");
        }
        self.control.push(Frame {
            kind: FrameKind::CatchAll,
            block_type: frame.block_type,
            height: self.operands.len(),
            unreachable: false,
        });
        Ok(())
    }
    fn visit_end(&mut self, resources: &I) -> Self::Output {
        let mut frame = self.pop_ctrl(resources)?;

        // Note that this `if` isn't included in the appendix right
        // now, but it's used to allow for `if` statements that are
        // missing an `else` block which have the same parameter/return
        // types on the block (since that's valid).
        if frame.kind == FrameKind::If {
            self.push_ctrl(FrameKind::Else, frame.block_type, resources)?;
            frame = self.pop_ctrl(resources)?;
        }
        for ty in results(frame.block_type, resources)? {
            self.push_operand(ty)?;
        }
        Ok(())
    }
    fn visit_br(&mut self, resources: &I, relative_depth: u32) -> Self::Output {
        let (ty, kind) = self.jump(relative_depth)?;
        for ty in label_types(ty, resources, kind)?.rev() {
            self.pop_operand(Some(ty))?;
        }
        self.unreachable();
        Ok(())
    }
    fn visit_br_if(&mut self, resources: &I, relative_depth: u32) -> Self::Output {
        self.pop_operand(Some(ValType::I32))?;
        let (ty, kind) = self.jump(relative_depth)?;
        for ty in label_types(ty, resources, kind)?.rev() {
            self.pop_operand(Some(ty))?;
        }
        for ty in label_types(ty, resources, kind)? {
            self.push_operand(ty)?;
        }
        Ok(())
    }
    fn visit_br_table(&mut self, resources: &I, table: &BrTable) -> Self::Output {
        self.pop_operand(Some(ValType::I32))?;
        let default = self.jump(table.default())?;
        let default_types = label_types(default.0, resources, default.1)?;
        for element in table.targets() {
            let relative_depth = element.map_err(|mut e| {
                e.inner.offset = usize::max_value();
                OperatorValidatorError(e)
            })?;
            let block = self.jump(relative_depth)?;
            let tys = label_types(block.0, resources, block.1)?;
            if tys.len() != default_types.len() {
                bail_op_err!(
                    "type mismatch: br_table target labels have different number of types"
                );
            }
            debug_assert!(self.br_table_tmp.is_empty());
            for ty in tys.rev() {
                let ty = self.pop_operand(Some(ty))?;
                self.br_table_tmp.push(ty);
            }
            self.operands.extend(self.br_table_tmp.drain(..).rev());
        }
        for ty in default_types.rev() {
            self.pop_operand(Some(ty))?;
        }
        self.unreachable();
        Ok(())
    }
    fn visit_return(&mut self, resources: &I) -> Self::Output {
        self.check_return(resources)?;
        Ok(())
    }
    fn visit_call(&mut self, resources: &I, function_index: u32) -> Self::Output {
        self.check_call(function_index, resources)?;
        Ok(())
    }
    fn visit_return_call(&mut self, resources: &I, function_index: u32) -> Self::Output {
        if !self.features.tail_call {
            return Err(OperatorValidatorError::new(
                "tail calls support is not enabled",
            ));
        }
        self.check_call(function_index, resources)?;
        self.check_return(resources)?;
        Ok(())
    }
    fn visit_call_indirect(
        &mut self,
        resources: &I,
        index: u32,
        table_index: u32,
        table_byte: u8,
    ) -> Self::Output {
        if table_byte != 0 && !self.features.reference_types {
            return Err(OperatorValidatorError::new(
                "reference-types not enabled: zero byte expected",
            ));
        }
        self.check_call_indirect(index, table_index, resources)?;
        Ok(())
    }
    fn visit_return_call_indirect(
        &mut self,
        resources: &I,
        index: u32,
        table_index: u32,
    ) -> Self::Output {
        if !self.features.tail_call {
            return Err(OperatorValidatorError::new(
                "tail calls support is not enabled",
            ));
        }
        self.check_call_indirect(index, table_index, resources)?;
        self.check_return(resources)?;
        Ok(())
    }
    fn visit_drop(&mut self, _resources: &I) -> Self::Output {
        self.pop_operand(None)?;
        Ok(())
    }
    fn visit_select(&mut self, _resources: &I) -> Self::Output {
        self.pop_operand(Some(ValType::I32))?;
        let ty1 = self.pop_operand(None)?;
        let ty2 = self.pop_operand(None)?;
        fn is_num(ty: Option<ValType>) -> bool {
            matches!(
                ty,
                Some(ValType::I32)
                    | Some(ValType::I64)
                    | Some(ValType::F32)
                    | Some(ValType::F64)
                    | Some(ValType::V128)
                    | None
            )
        }
        if !is_num(ty1) || !is_num(ty2) {
            bail_op_err!("type mismatch: select only takes integral types")
        }
        if ty1 != ty2 && ty1 != None && ty2 != None {
            bail_op_err!("type mismatch: select operands have different types")
        }
        self.operands.push(ty1.or(ty2));
        Ok(())
    }
    fn visit_typed_select(&mut self, _resources: &I, ty: ValType) -> Self::Output {
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_local_get(&mut self, _resources: &I, local_index: u32) -> Self::Output {
        let ty = self.local(local_index)?;
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_local_set(&mut self, _resources: &I, local_index: u32) -> Self::Output {
        let ty = self.local(local_index)?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_local_tee(&mut self, _resources: &I, local_index: u32) -> Self::Output {
        let ty = self.local(local_index)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_global_get(&mut self, resources: &I, global_index: u32) -> Self::Output {
        if let Some(ty) = resources.global_at(global_index) {
            self.push_operand(ty.content_type)?;
        } else {
            return Err(OperatorValidatorError::new(
                "unknown global: global index out of bounds",
            ));
        };
        Ok(())
    }
    fn visit_global_set(&mut self, resources: &I, global_index: u32) -> Self::Output {
        if let Some(ty) = resources.global_at(global_index) {
            if !ty.mutable {
                return Err(OperatorValidatorError::new(
                    "global is immutable: cannot modify it with `global.set`",
                ));
            }
            self.pop_operand(Some(ty.content_type))?;
        } else {
            return Err(OperatorValidatorError::new(
                "unknown global: global index out of bounds",
            ));
        };
        Ok(())
    }
    fn visit_i32_load(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i64_load(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 3, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_f32_load(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        let ty = self.check_memarg(memarg, 2, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::F32)?;
        Ok(())
    }
    fn visit_f64_load(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        let ty = self.check_memarg(memarg, 3, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::F64)?;
        Ok(())
    }
    fn visit_i32_load8_s(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i32_load8_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i32_load8_s(resources, memarg)
    }
    fn visit_i32_load16_s(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i32_load16_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i32_load16_s(resources, memarg)
    }
    fn visit_i64_load8_s(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load8_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load8_s(resources, memarg)
    }
    fn visit_i64_load16_s(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load16_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load16_s(resources, memarg)
    }
    fn visit_i64_load32_s(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load32_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load32_s(resources, memarg)
    }
    fn visit_i32_store(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, resources)?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_i64_store(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 3, resources)?;
        self.pop_operand(Some(ValType::I64))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_f32_store(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        let ty = self.check_memarg(memarg, 2, resources)?;
        self.pop_operand(Some(ValType::F32))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_f64_store(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        let ty = self.check_memarg(memarg, 3, resources)?;
        self.pop_operand(Some(ValType::F64))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_i32_store8(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, resources)?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_i32_store16(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, resources)?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_i64_store8(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, resources)?;
        self.pop_operand(Some(ValType::I64))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_i64_store16(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, resources)?;
        self.pop_operand(Some(ValType::I64))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_i64_store32(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, resources)?;
        self.pop_operand(Some(ValType::I64))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_memory_size(&mut self, resources: &I, mem: u32, mem_byte: u8) -> Self::Output {
        if mem_byte != 0 && !self.features.multi_memory {
            return Err(OperatorValidatorError::new(
                "multi-memory not enabled: zero byte expected",
            ));
        }
        let index_ty = self.check_memory_index(mem, resources)?;
        self.push_operand(index_ty)?;
        Ok(())
    }
    fn visit_memory_grow(&mut self, resources: &I, mem: u32, mem_byte: u8) -> Self::Output {
        if mem_byte != 0 && !self.features.multi_memory {
            return Err(OperatorValidatorError::new(
                "multi-memory not enabled: zero byte expected",
            ));
        }
        let index_ty = self.check_memory_index(mem, resources)?;
        self.pop_operand(Some(index_ty))?;
        self.push_operand(index_ty)?;
        Ok(())
    }
    fn visit_i32_const(&mut self, _resources: &I, _value: i32) -> Self::Output {
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i64_const(&mut self, _resources: &I, _value: i64) -> Self::Output {
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_f32_const(&mut self, _resources: &I, _value: Ieee32) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.push_operand(ValType::F32)?;
        Ok(())
    }
    fn visit_f64_const(&mut self, _resources: &I, _value: Ieee64) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.push_operand(ValType::F64)?;
        Ok(())
    }
    fn visit_i32_eqz(&mut self, _resources: &I) -> Self::Output {
        self.pop_operand(Some(ValType::I32))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i32_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_lt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_lt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_gt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_gt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_le_s(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_le_u(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_ge_s(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i32_ge_u(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I32)
    }
    fn visit_i64_eqz(&mut self, _resources: &I) -> Self::Output {
        self.pop_operand(Some(ValType::I64))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i64_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_lt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_lt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_gt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_gt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_le_s(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_le_u(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_ge_s(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_i64_ge_u(&mut self, _resources: &I) -> Self::Output {
        self.check_cmp_op(ValType::I64)
    }
    fn visit_f32_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F32)
    }
    fn visit_f32_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F32)
    }
    fn visit_f32_lt(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F32)
    }
    fn visit_f32_gt(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F32)
    }
    fn visit_f32_le(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F32)
    }
    fn visit_f32_ge(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F32)
    }
    fn visit_f64_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F64)
    }
    fn visit_f64_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F64)
    }
    fn visit_f64_lt(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F64)
    }
    fn visit_f64_gt(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F64)
    }
    fn visit_f64_le(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F64)
    }
    fn visit_f64_ge(&mut self, _resources: &I) -> Self::Output {
        self.check_fcmp_op(ValType::F64)
    }
    fn visit_i32_clz(&mut self, _resources: &I) -> Self::Output {
        self.check_unary_op(ValType::I32)
    }
    fn visit_i32_ctz(&mut self, _resources: &I) -> Self::Output {
        self.check_unary_op(ValType::I32)
    }
    fn visit_i32_popcnt(&mut self, _resources: &I) -> Self::Output {
        self.check_unary_op(ValType::I32)
    }
    fn visit_i32_add(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_div_s(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_div_u(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_rem_s(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_rem_u(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_and(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_or(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_xor(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_shl(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_shr_s(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_shr_u(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_rotl(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i32_rotr(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I32)
    }
    fn visit_i64_clz(&mut self, _resources: &I) -> Self::Output {
        self.check_unary_op(ValType::I64)
    }
    fn visit_i64_ctz(&mut self, _resources: &I) -> Self::Output {
        self.check_unary_op(ValType::I64)
    }
    fn visit_i64_popcnt(&mut self, _resources: &I) -> Self::Output {
        self.check_unary_op(ValType::I64)
    }
    fn visit_i64_add(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_div_s(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_div_u(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_rem_s(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_rem_u(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_and(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_or(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_xor(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_shl(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_shr_s(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_shr_u(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_rotl(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_i64_rotr(&mut self, _resources: &I) -> Self::Output {
        self.check_binary_op(ValType::I64)
    }
    fn visit_f32_abs(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F32)
    }
    fn visit_f32_neg(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F32)
    }
    fn visit_f32_ceil(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F32)
    }
    fn visit_f32_floor(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F32)
    }
    fn visit_f32_trunc(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F32)
    }
    fn visit_f32_nearest(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F32)
    }
    fn visit_f32_sqrt(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F32)
    }
    fn visit_f32_add(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F32)
    }
    fn visit_f32_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F32)
    }
    fn visit_f32_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F32)
    }
    fn visit_f32_div(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F32)
    }
    fn visit_f32_min(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F32)
    }
    fn visit_f32_max(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F32)
    }
    fn visit_f32_copysign(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F32)
    }
    fn visit_f64_abs(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F64)
    }
    fn visit_f64_neg(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F64)
    }
    fn visit_f64_ceil(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F64)
    }
    fn visit_f64_floor(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F64)
    }
    fn visit_f64_trunc(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F64)
    }
    fn visit_f64_nearest(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F64)
    }
    fn visit_f64_sqrt(&mut self, _resources: &I) -> Self::Output {
        self.check_funary_op(ValType::F64)
    }
    fn visit_f64_add(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F64)
    }
    fn visit_f64_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F64)
    }
    fn visit_f64_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F64)
    }
    fn visit_f64_div(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F64)
    }
    fn visit_f64_min(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F64)
    }
    fn visit_f64_max(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F64)
    }
    fn visit_f64_copysign(&mut self, _resources: &I) -> Self::Output {
        self.check_fbinary_op(ValType::F64)
    }
    fn visit_i32_wrap_i64(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I32, ValType::I64)
    }
    fn visit_i32_trunc_f32s(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I32, ValType::F32)
    }
    fn visit_i32_trunc_f32u(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I32, ValType::F32)
    }
    fn visit_i32_trunc_f64s(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I32, ValType::F64)
    }
    fn visit_i32_trunc_f64u(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I32, ValType::F64)
    }
    fn visit_i64_extend_i32s(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I64, ValType::I32)
    }
    fn visit_i64_extend_i32u(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I64, ValType::I32)
    }
    fn visit_i64_trunc_f32s(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I64, ValType::F32)
    }
    fn visit_i64_trunc_f32u(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I64, ValType::F32)
    }
    fn visit_i64_trunc_f64s(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I64, ValType::F64)
    }
    fn visit_i64_trunc_f64u(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I64, ValType::F64)
    }
    fn visit_f32_convert_i32s(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F32, ValType::I32)
    }
    fn visit_f32_convert_i32u(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F32, ValType::I32)
    }
    fn visit_f32_convert_i64s(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F32, ValType::I64)
    }
    fn visit_f32_convert_i64u(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F32, ValType::I64)
    }
    fn visit_f32_demote_f64(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F32, ValType::F64)
    }
    fn visit_f64_convert_i32s(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F64, ValType::I32)
    }
    fn visit_f64_convert_i32u(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F64, ValType::I32)
    }
    fn visit_f64_convert_i64s(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F64, ValType::I64)
    }
    fn visit_f64_convert_i64u(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F64, ValType::I64)
    }
    fn visit_f64_promote_f32(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F64, ValType::F32)
    }
    fn visit_i32_reinterpret_f32(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I32, ValType::F32)
    }
    fn visit_i64_reinterpret_f64(&mut self, _resources: &I) -> Self::Output {
        self.check_conversion_op(ValType::I64, ValType::F64)
    }
    fn visit_f32_reinterpret_i32(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F32, ValType::I32)
    }
    fn visit_f64_reinterpret_i64(&mut self, _resources: &I) -> Self::Output {
        self.check_fconversion_op(ValType::F64, ValType::I64)
    }
    fn visit_i32_trunc_sat_f32s(&mut self, _resources: &I) -> Self::Output {
        self.check_saturating_float_to_int_enabled()?;
        self.check_conversion_op(ValType::I32, ValType::F32)
    }
    fn visit_i32_trunc_sat_f32u(&mut self, _resources: &I) -> Self::Output {
        self.check_saturating_float_to_int_enabled()?;
        self.check_conversion_op(ValType::I32, ValType::F32)
    }
    fn visit_i32_trunc_sat_f64s(&mut self, _resources: &I) -> Self::Output {
        self.check_saturating_float_to_int_enabled()?;
        self.check_conversion_op(ValType::I32, ValType::F64)
    }
    fn visit_i32_trunc_sat_f64u(&mut self, _resources: &I) -> Self::Output {
        self.check_saturating_float_to_int_enabled()?;
        self.check_conversion_op(ValType::I32, ValType::F64)
    }
    fn visit_i64_trunc_sat_f32s(&mut self, _resources: &I) -> Self::Output {
        self.check_saturating_float_to_int_enabled()?;
        self.check_conversion_op(ValType::I64, ValType::F32)
    }
    fn visit_i64_trunc_sat_f32u(&mut self, _resources: &I) -> Self::Output {
        self.check_saturating_float_to_int_enabled()?;
        self.check_conversion_op(ValType::I64, ValType::F32)
    }
    fn visit_i64_trunc_sat_f64s(&mut self, _resources: &I) -> Self::Output {
        self.check_saturating_float_to_int_enabled()?;
        self.check_conversion_op(ValType::I64, ValType::F64)
    }
    fn visit_i64_trunc_sat_f64u(&mut self, _resources: &I) -> Self::Output {
        self.check_saturating_float_to_int_enabled()?;
        self.check_conversion_op(ValType::I64, ValType::F64)
    }
    fn visit_i32_extend8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_sign_extension_enabled()?;
        self.check_unary_op(ValType::I32)
    }
    fn visit_i32_extend16_s(&mut self, _resources: &I) -> Self::Output {
        self.check_sign_extension_enabled()?;
        self.check_unary_op(ValType::I32)
    }
    fn visit_i64_extend8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_sign_extension_enabled()?;
        self.check_unary_op(ValType::I64)
    }
    fn visit_i64_extend16_s(&mut self, _resources: &I) -> Self::Output {
        self.check_sign_extension_enabled()?;
        self.check_unary_op(ValType::I64)
    }
    fn visit_i64_extend32_s(&mut self, _resources: &I) -> Self::Output {
        self.check_sign_extension_enabled()?;
        self.check_unary_op(ValType::I64)
    }
    fn visit_i32_atomic_load(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_load(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_load16_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_load(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_load8_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_load(resources, memarg, ValType::I32)
    }
    fn visit_i64_atomic_load(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_load(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_load32_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_load(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_load16_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_load(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_load8_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_load(resources, memarg, ValType::I64)
    }
    fn visit_i32_atomic_store(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_store16(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_store8(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(resources, memarg, ValType::I32)
    }
    fn visit_i64_atomic_store(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_store32(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_store16(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_store8(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(resources, memarg, ValType::I64)
    }
    fn visit_i32_atomic_rmw_add(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_sub(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_and(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_or(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_xor(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_add_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_sub_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_and_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_or_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_xor_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_add_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_sub_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_and_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_or_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_xor_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i64_atomic_rmw_add(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_sub(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_and(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_or(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_xor(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_add_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_sub_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_and_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_or_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_xor_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_add_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_sub_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_and_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_or_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_xor_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_add_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_sub_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_and_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_or_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_xor_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i32_atomic_rmw_xchg(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_xchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_xchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_cmpxchg(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_cmpxchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(resources, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_cmpxchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(resources, memarg, ValType::I32)
    }
    fn visit_i64_atomic_rmw_xchg(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_xchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_xchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_xchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_cmpxchg(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_cmpxchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_cmpxchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(resources, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_cmpxchg_u(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(resources, memarg, ValType::I64)
    }
    fn visit_memory_atomic_notify(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(resources, memarg, ValType::I32)
    }
    fn visit_memory_atomic_wait32(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_threads_enabled()?;
        let ty = self.check_shared_memarg_wo_align(memarg, resources)?;
        self.pop_operand(Some(ValType::I64))?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_memory_atomic_wait64(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_threads_enabled()?;
        let ty = self.check_shared_memarg_wo_align(memarg, resources)?;
        self.pop_operand(Some(ValType::I64))?;
        self.pop_operand(Some(ValType::I64))?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_atomic_fence(&mut self, _resources: &I, flags: u8) -> Self::Output {
        self.check_threads_enabled()?;
        if flags != 0 {
            return Err(OperatorValidatorError::new(
                "non-zero flags for fence not supported yet",
            ));
        }
        Ok(())
    }
    fn visit_ref_null(&mut self, _resources: &I, ty: ValType) -> Self::Output {
        self.check_reference_types_enabled()?;
        match ty {
            ValType::FuncRef | ValType::ExternRef => {}
            _ => {
                return Err(OperatorValidatorError::new(
                    "invalid reference type in ref.null",
                ))
            }
        }
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_ref_is_null(&mut self, _resources: &I) -> Self::Output {
        self.check_reference_types_enabled()?;
        match self.pop_operand(None)? {
            None | Some(ValType::FuncRef) | Some(ValType::ExternRef) => {}
            _ => {
                return Err(OperatorValidatorError::new(
                    "type mismatch: invalid reference type in ref.is_null",
                ))
            }
        }
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_ref_func(&mut self, resources: &I, function_index: u32) -> Self::Output {
        self.check_reference_types_enabled()?;
        if resources.type_of_function(function_index).is_none() {
            return Err(OperatorValidatorError::new(format!(
                "unknown function {}: function index out of bounds",
                function_index,
            )));
        }
        if !resources.is_function_referenced(function_index) {
            return Err(OperatorValidatorError::new("undeclared function reference"));
        }
        self.push_operand(ValType::FuncRef)?;
        Ok(())
    }
    fn visit_v128_load(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled()?;
        let ty = self.check_memarg(memarg, 4, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_store(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled()?;
        let ty = self.check_memarg(memarg, 4, resources)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_v128_const(&mut self, _resources: &I, _value: V128) -> Self::Output {
        self.check_simd_enabled()?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i8x16_splat(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_splat(ValType::I32)
    }
    fn visit_i16x8_splat(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_splat(ValType::I32)
    }
    fn visit_i32x4_splat(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_splat(ValType::I32)
    }
    fn visit_i64x2_splat(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_splat(ValType::I64)
    }
    fn visit_f32x4_splat(&mut self, _resources: &I) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_v128_splat(ValType::F32)
    }
    fn visit_f64x2_splat(&mut self, _resources: &I) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_v128_splat(ValType::F64)
    }
    fn visit_i8x16_extract_lane_s(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 16)?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i8x16_extract_lane_u(&mut self, resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.visit_i8x16_extract_lane_s(resources, lane)
    }
    fn visit_i16x8_extract_lane_s(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 8)?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i16x8_extract_lane_u(&mut self, resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.visit_i16x8_extract_lane_s(resources, lane)
    }
    fn visit_i32x4_extract_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 4)?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i8x16_replace_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 16)?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i16x8_replace_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 8)?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i32x4_replace_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 4)?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i64x2_extract_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 2)?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_i64x2_replace_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 2)?;
        self.pop_operand(Some(ValType::I64))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_f32x4_extract_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 4)?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::F32)?;
        Ok(())
    }
    fn visit_f32x4_replace_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 4)?;
        self.pop_operand(Some(ValType::F32))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_f64x2_extract_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 2)?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::F64)?;
        Ok(())
    }
    fn visit_f64x2_replace_lane(&mut self, _resources: &I, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled()?;
        self.check_simd_lane_index(lane, 2)?;
        self.pop_operand(Some(ValType::F64))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_f32x4_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_lt(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_gt(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_le(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_ge(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_lt(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_gt(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_le(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_ge(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_add(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_div(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_min(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_max(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_p_min(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_p_max(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_add(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_div(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_min(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_max(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_p_min(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f64x2_p_max(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_fbinary_op()
    }
    fn visit_f32x4_relaxed_min(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_binary_op()
    }
    fn visit_f32x4_relaxed_max(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_binary_op()
    }
    fn visit_f64x2_relaxed_min(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_binary_op()
    }
    fn visit_f64x2_relaxed_max(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_binary_op()
    }
    fn visit_i8x16_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_lt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_lt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_gt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_gt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_le_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_le_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_ge_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_ge_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_lt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_lt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_gt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_gt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_le_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_le_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_ge_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_ge_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_lt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_lt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_gt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_gt_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_le_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_le_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_ge_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_ge_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_eq(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_ne(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_lt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_gt_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_le_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_ge_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_v128_and(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_v128_and_not(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_v128_or(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_v128_xor(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_add(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_add_sat_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_add_sat_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_sub_sat_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_sub_sat_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_min_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_min_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_max_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_max_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_add(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_add_sat_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_add_sat_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_sub_sat_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_sub_sat_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_min_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_min_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_max_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_max_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_add(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_min_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_min_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_max_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_max_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_dot_i16x8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_add(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_sub(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_mul(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_rounding_average_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_rounding_average_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_narrow_i16x8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i8x16_narrow_i16x8_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_narrow_i32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_narrow_i32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_ext_mul_low_i8x16_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_ext_mul_high_i8x16_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_ext_mul_low_i8x16_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_ext_mul_high_i8x16_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_ext_mul_low_i16x8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_ext_mul_high_i16x8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_ext_mul_low_i16x8_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i32x4_ext_mul_high_i16x8_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_ext_mul_low_i32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_ext_mul_high_i32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_ext_mul_low_i32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i64x2_ext_mul_high_i32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_i16x8_q15_mulr_sat_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_binary_op()
    }
    fn visit_f32x4_ceil(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_floor(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_trunc(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_nearest(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_ceil(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_floor(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_trunc(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_nearest(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_abs(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_neg(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_sqrt(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_abs(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_neg(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_sqrt(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_demote_f64x2_zero(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_promote_low_f32x4(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_convert_low_i32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f64x2_convert_low_i32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_i32x4_trunc_sat_f32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_i32x4_trunc_sat_f32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_convert_i32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_f32x4_convert_i32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_funary_op()
    }
    fn visit_v128_not(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i8x16_abs(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i8x16_neg(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i8x16_popcnt(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i16x8_abs(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i16x8_neg(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_abs(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_neg(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i64x2_abs(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i64x2_neg(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i16x8_extend_low_i8x16_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i16x8_extend_high_i8x16_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i16x8_extend_low_i8x16_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i16x8_extend_high_i8x16_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_extend_low_i16x8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_extend_high_i16x8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_extend_low_i16x8_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_extend_high_i16x8_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i64x2_extend_low_i32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i64x2_extend_high_i32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i64x2_extend_low_i32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i64x2_extend_high_i32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i16x8_ext_add_pairwise_i8x16_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i16x8_ext_add_pairwise_i8x16_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_ext_add_pairwise_i16x8_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_ext_add_pairwise_i16x8_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_unary_op()
    }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_unary_op()
    }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_unary_op()
    }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_unary_op()
    }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_unary_op()
    }
    fn visit_v128_bitselect(&mut self, _resources: &I) -> Self::Output {
        self.check_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_f32x4_fma(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_ternary_op()
    }
    fn visit_f32x4_fms(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_ternary_op()
    }
    fn visit_f64x2_fma(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_ternary_op()
    }
    fn visit_f64x2_fms(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_ternary_op()
    }
    fn visit_i8x16_lane_select(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_ternary_op()
    }
    fn visit_i16x8_lane_select(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_ternary_op()
    }
    fn visit_i32x4_lane_select(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_ternary_op()
    }
    fn visit_i64x2_lane_select(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_relaxed_ternary_op()
    }
    fn visit_v128_any_true(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i8x16_all_true(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i8x16_bitmask(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i16x8_all_true(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i16x8_bitmask(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i32x4_all_true(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i32x4_bitmask(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i64x2_all_true(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i64x2_bitmask(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_bitmask_op()
    }
    fn visit_i8x16_shl(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i8x16_shr_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i8x16_shr_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i16x8_shl(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i16x8_shr_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i16x8_shr_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i32x4_shl(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i32x4_shr_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i32x4_shr_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i64x2_shl(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i64x2_shr_s(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i64x2_shr_u(&mut self, _resources: &I) -> Self::Output {
        self.check_v128_shift_op()
    }
    fn visit_i8x16_swizzle(&mut self, _resources: &I) -> Self::Output {
        self.check_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i8x16_relaxed_swizzle(&mut self, _resources: &I) -> Self::Output {
        self.check_relaxed_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i8x16_shuffle(&mut self, _resources: &I, lanes: [SIMDLaneIndex; 16]) -> Self::Output {
        self.check_simd_enabled()?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(ValType::V128))?;
        for i in lanes {
            self.check_simd_lane_index(i, 32)?;
        }
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load8_splat(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled()?;
        let ty = self.check_memarg(memarg, 0, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load16_splat(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled()?;
        let ty = self.check_memarg(memarg, 1, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load32_splat(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled()?;
        let ty = self.check_memarg(memarg, 2, resources)?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load32_zero(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.visit_v128_load32_splat(resources, memarg)
    }
    fn visit_v128_load64_splat(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(resources, memarg)
    }
    fn visit_v128_load64_zero(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(resources, memarg)
    }
    fn visit_v128_load8x8_s(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(resources, memarg)
    }
    fn visit_v128_load8x8_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(resources, memarg)
    }
    fn visit_v128_load16x4_s(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(resources, memarg)
    }
    fn visit_v128_load16x4_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(resources, memarg)
    }
    fn visit_v128_load32x2_s(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(resources, memarg)
    }
    fn visit_v128_load32x2_u(&mut self, resources: &I, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(resources, memarg)
    }
    fn visit_v128_load8_lane(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 0, resources)?;
        self.check_simd_lane_index(lane, 16)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load16_lane(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 1, resources)?;
        self.check_simd_lane_index(lane, 8)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load32_lane(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 2, resources)?;
        self.check_simd_lane_index(lane, 4)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load64_lane(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 3, resources)?;
        self.check_simd_lane_index(lane, 2)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_store8_lane(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 0, resources)?;
        self.check_simd_lane_index(lane, 16)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(idx))?;
        Ok(())
    }
    fn visit_v128_store16_lane(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 1, resources)?;
        self.check_simd_lane_index(lane, 8)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(idx))?;
        Ok(())
    }
    fn visit_v128_store32_lane(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 2, resources)?;
        self.check_simd_lane_index(lane, 4)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(idx))?;
        Ok(())
    }
    fn visit_v128_store64_lane(
        &mut self,
        resources: &I,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled()?;
        let idx = self.check_memarg(memarg, 3, resources)?;
        self.check_simd_lane_index(lane, 2)?;
        self.pop_operand(Some(ValType::V128))?;
        self.pop_operand(Some(idx))?;
        Ok(())
    }
    fn visit_memory_init(&mut self, resources: &I, mem: u32, segment: u32) -> Self::Output {
        self.check_bulk_memory_enabled()?;
        let ty = self.check_memory_index(mem, resources)?;
        match resources.data_count() {
            None => bail_op_err!("data count section required"),
            Some(count) if segment < count => {}
            Some(_) => bail_op_err!("unknown data segment {}", segment),
        }
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_data_drop(&mut self, resources: &I, segment: u32) -> Self::Output {
        self.check_bulk_memory_enabled()?;
        match resources.data_count() {
            None => bail_op_err!("data count section required"),
            Some(count) if segment < count => {}
            Some(_) => bail_op_err!("unknown data segment {}", segment),
        }
        Ok(())
    }
    fn visit_memory_copy(&mut self, resources: &I, src: u32, dst: u32) -> Self::Output {
        self.check_bulk_memory_enabled()?;
        let dst_ty = self.check_memory_index(dst, resources)?;
        let src_ty = self.check_memory_index(src, resources)?;

        // The length operand here is the smaller of src/dst, which is
        // i32 if one is i32
        self.pop_operand(Some(match src_ty {
            ValType::I32 => ValType::I32,
            _ => dst_ty,
        }))?;

        // ... and the offset into each memory is required to be
        // whatever the indexing type is for that memory
        self.pop_operand(Some(src_ty))?;
        self.pop_operand(Some(dst_ty))?;
        Ok(())
    }
    fn visit_memory_fill(&mut self, resources: &I, mem: u32) -> Self::Output {
        self.check_bulk_memory_enabled()?;
        let ty = self.check_memory_index(mem, resources)?;
        self.pop_operand(Some(ty))?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        Ok(())
    }
    fn visit_table_init(&mut self, resources: &I, segment: u32, table: u32) -> Self::Output {
        self.check_bulk_memory_enabled()?;
        if table > 0 {
            self.check_reference_types_enabled()?;
        }
        let table = match resources.table_at(table) {
            Some(table) => table,
            None => bail_op_err!("unknown table {}: table index out of bounds", table),
        };
        let segment_ty = match resources.element_type_at(segment) {
            Some(ty) => ty,
            None => bail_op_err!(
                "unknown elem segment {}: segment index out of bounds",
                segment
            ),
        };
        if segment_ty != table.element_type {
            return Err(OperatorValidatorError::new("type mismatch"));
        }
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::I32))?;
        Ok(())
    }
    fn visit_elem_drop(&mut self, resources: &I, segment: u32) -> Self::Output {
        self.check_bulk_memory_enabled()?;
        if segment >= resources.element_count() {
            bail_op_err!(
                "unknown elem segment {}: segment index out of bounds",
                segment
            );
        }
        Ok(())
    }
    fn visit_table_copy(&mut self, resources: &I, dst_table: u32, src_table: u32) -> Self::Output {
        self.check_bulk_memory_enabled()?;
        if src_table > 0 || dst_table > 0 {
            self.check_reference_types_enabled()?;
        }
        let (src, dst) = match (resources.table_at(src_table), resources.table_at(dst_table)) {
            (Some(a), Some(b)) => (a, b),
            _ => return Err(OperatorValidatorError::new("table index out of bounds")),
        };
        if src.element_type != dst.element_type {
            return Err(OperatorValidatorError::new("type mismatch"));
        }
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ValType::I32))?;
        Ok(())
    }
    fn visit_table_get(&mut self, resources: &I, table: u32) -> Self::Output {
        self.check_reference_types_enabled()?;
        let ty = match resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => return Err(OperatorValidatorError::new("table index out of bounds")),
        };
        self.pop_operand(Some(ValType::I32))?;
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_table_set(&mut self, resources: &I, table: u32) -> Self::Output {
        self.check_reference_types_enabled()?;
        let ty = match resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => return Err(OperatorValidatorError::new("table index out of bounds")),
        };
        self.pop_operand(Some(ty))?;
        self.pop_operand(Some(ValType::I32))?;
        Ok(())
    }
    fn visit_table_grow(&mut self, resources: &I, table: u32) -> Self::Output {
        self.check_reference_types_enabled()?;
        let ty = match resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => return Err(OperatorValidatorError::new("table index out of bounds")),
        };
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_table_size(&mut self, resources: &I, table: u32) -> Self::Output {
        self.check_reference_types_enabled()?;
        if resources.table_at(table).is_none() {
            return Err(OperatorValidatorError::new("table index out of bounds"));
        }
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_table_fill(&mut self, resources: &I, table: u32) -> Self::Output {
        self.check_bulk_memory_enabled()?;
        let ty = match resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => return Err(OperatorValidatorError::new("table index out of bounds")),
        };
        self.pop_operand(Some(ValType::I32))?;
        self.pop_operand(Some(ty))?;
        self.pop_operand(Some(ValType::I32))?;
        Ok(())
    }
}
