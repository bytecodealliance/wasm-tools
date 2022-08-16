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
    MemoryImmediate, Result, SIMDLaneIndex, ValType, VisitOperator, WasmFeatures, WasmFuncType,
    WasmModuleResources, V128,
};
use std::ops::{Deref, DerefMut};

/// Create a `BinaryReaderError` with a format string.
macro_rules! format_op_err {
    ( $offset:expr, $( $arg:expr ),* $(,)* ) => {
        BinaryReaderError::new(format!( $( $arg ),* ), $offset)
    }
}

/// Early return an `Err(BinaryReaderError)` with a format string.
macro_rules! bail_op_err {
    ( $offset:expr, $( $arg:expr ),* $(,)* ) => {
        return Err(format_op_err!( $offset, $( $arg ),* ))
    }
}

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

    // This is a list of flags for wasm features which are used to gate various
    // instructions.
    pub(crate) features: WasmFeatures,

    // Temporary storage used during the validation of `br_table`.
    br_table_tmp: Vec<Option<ValType>>,

    /// The `control` list is the list of blocks that we're currently in.
    control: Vec<Frame>,
    /// The `operands` is the current type stack.
    operands: Vec<Option<ValType>>,

    /// Offset of the `end` instruction which emptied the `control` stack, which
    /// must be the end of the function.
    end_which_emptied_control: Option<usize>,
}

// This structure corresponds to `ctrl_frame` as specified at in the validation
// appendix of the wasm spec
#[derive(Debug)]
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

#[derive(PartialEq, Copy, Clone, Debug)]
enum FrameKind {
    Block,
    If,
    Else,
    Loop,
    Try,
    Catch,
    CatchAll,
}

struct OperatorValidatorTemp<'validator, 'resources, T> {
    inner: &'validator mut OperatorValidator,
    resources: &'resources T,
}

impl OperatorValidator {
    /// Creates a new operator validator which will be used to validate a
    /// function whose type is the `ty` index specified.
    ///
    /// The `resources` are used to learn about the function type underlying
    /// `ty`.
    pub fn new_func<T>(
        ty: u32,
        offset: usize,
        features: &WasmFeatures,
        resources: &T,
    ) -> Result<Self>
    where
        T: WasmModuleResources,
    {
        let mut ret = OperatorValidator {
            num_locals: 0,
            locals: Vec::new(),
            features: *features,
            br_table_tmp: Vec::new(),
            operands: Vec::new(),
            control: vec![Frame {
                kind: FrameKind::Block,
                block_type: BlockType::FuncType(ty),
                height: 0,
                unreachable: false,
            }],
            end_which_emptied_control: None,
        };
        let locals = OperatorValidatorTemp {
            inner: &mut ret,
            resources,
        }
        .func_type_at(ty, offset)?
        .inputs()
        .enumerate()
        .map(|(i, ty)| (i as u32, ty))
        .collect::<Vec<_>>();
        ret.num_locals = locals.len() as u32;
        ret.locals = locals;
        Ok(ret)
    }

    /// Creates a new operator validator which will be used to validate an
    /// `init_expr` constant expression which should result in the `ty`
    /// specified.
    pub fn new_const_expr(features: &WasmFeatures, ty: ValType) -> Self {
        OperatorValidator {
            num_locals: 0,
            locals: Vec::new(),
            features: *features,
            br_table_tmp: Vec::new(),
            operands: Vec::new(),
            control: vec![Frame {
                kind: FrameKind::Block,
                block_type: BlockType::Type(ty),
                height: 0,
                unreachable: false,
            }],
            end_which_emptied_control: None,
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

    pub fn operand_stack_height(&self) -> usize {
        self.operands.len()
    }

    /// Create a temporary [`OperatorValidatorTemp`] for validation.
    pub fn with_resources<'validator, 'resources, T>(
        &'validator mut self,
        resources: &'resources T,
    ) -> impl VisitOperator<Output = Result<()>> + 'validator
    where
        T: WasmModuleResources,
        'resources: 'validator,
    {
        OperatorValidatorTemp {
            inner: self,
            resources,
        }
    }

    pub fn finish(&mut self, offset: usize) -> Result<()> {
        if self.control.last().is_some() {
            bail_op_err!(
                offset,
                "control frames remain at end of function: END opcode expected"
            );
        }

        // The `end` opcode is one byte which means that the `offset` here
        // should point just beyond the `end` opcode which emptied the control
        // stack. If not that means more instructions were present after the
        // control stack was emptied.
        if offset != self.end_which_emptied_control.unwrap() + 1 {
            return Err(self.err_beyond_end(offset));
        }
        Ok(())
    }

    fn err_beyond_end(&self, offset: usize) -> BinaryReaderError {
        format_op_err!(offset, "operators remaining after end of function")
    }
}

impl<R> Deref for OperatorValidatorTemp<'_, '_, R> {
    type Target = OperatorValidator;
    fn deref(&self) -> &OperatorValidator {
        &self.inner
    }
}

impl<R> DerefMut for OperatorValidatorTemp<'_, '_, R> {
    fn deref_mut(&mut self) -> &mut OperatorValidator {
        &mut self.inner
    }
}

impl<'resources, R: WasmModuleResources> OperatorValidatorTemp<'_, 'resources, R> {
    /// Pushes a type onto the operand stack.
    ///
    /// This is used by instructions to represent a value that is pushed to the
    /// operand stack. This can fail, but only if `Type` is feature gated.
    /// Otherwise the push operation always succeeds.
    fn push_operand<T>(&mut self, ty: T) -> Result<()>
    where
        T: Into<Option<ValType>>,
    {
        let maybe_ty = ty.into();
        self.operands.push(maybe_ty);
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
    fn pop_operand(&mut self, offset: usize, expected: Option<ValType>) -> Result<Option<ValType>> {
        let control = match self.control.last() {
            Some(c) => c,
            None => return Err(self.err_beyond_end(offset)),
        };
        let actual = if self.operands.len() == control.height {
            if control.unreachable {
                None
            } else {
                let desc = match expected {
                    Some(ty) => ty_to_str(ty),
                    None => "a type",
                };
                bail_op_err!(
                    offset,
                    "type mismatch: expected {} but nothing on stack",
                    desc
                )
            }
        } else {
            self.operands.pop().unwrap()
        };
        if let (Some(actual_ty), Some(expected_ty)) = (actual, expected) {
            if actual_ty != expected_ty {
                bail_op_err!(
                    offset,
                    "type mismatch: expected {}, found {}",
                    ty_to_str(expected_ty),
                    ty_to_str(actual_ty)
                )
            }
        }
        Ok(actual)
    }

    /// Fetches the type for the local at `idx`, returning an error if it's out
    /// of bounds.
    fn local(&self, offset: usize, idx: u32) -> Result<ValType> {
        match self
            .inner
            .locals
            .binary_search_by_key(&idx, |(idx, _)| *idx)
        {
            // If this index would be inserted at the end of the list, then the
            // index is out of bounds and we return an error.
            Err(i) if i == self.locals.len() => {
                bail_op_err!(offset, "unknown local {}: local index out of bounds", idx)
            }
            // If `Ok` is returned we found the index exactly, or if `Err` is
            // returned the position is the one which is the least index
            // greater that `idx`, which is still the type of `idx` according
            // to our "compressed" representation. In both cases we access the
            // list at index `i`.
            Ok(i) | Err(i) => Ok(self.locals[i].1),
        }
    }

    /// Flags the current control frame as unreachable, additionally truncating
    /// the currently active operand stack.
    fn unreachable(&mut self, offset: usize) -> Result<()> {
        let control = match self.control.last_mut() {
            Some(frame) => frame,
            None => return Err(self.err_beyond_end(offset)),
        };
        control.unreachable = true;
        let new_height = control.height;
        self.operands.truncate(new_height);
        Ok(())
    }

    /// Pushes a new frame onto the control stack.
    ///
    /// This operation is used when entering a new block such as an if, loop,
    /// or block itself. The `kind` of block is specified which indicates how
    /// breaks interact with this block's type. Additionally the type signature
    /// of the block is specified by `ty`.
    fn push_ctrl(&mut self, offset: usize, kind: FrameKind, ty: BlockType) -> Result<()> {
        // Push a new frame which has a snapshot of the height of the current
        // operand stack.
        let height = self.operands.len();
        self.control.push(Frame {
            kind,
            block_type: ty,
            height,
            unreachable: false,
        });
        // All of the parameters are now also available in this control frame,
        // so we push them here in order.
        for ty in self.params(offset, ty)? {
            self.push_operand(ty)?;
        }
        Ok(())
    }

    /// Pops a frame from the control stack.
    ///
    /// This function is used when exiting a block and leaves a block scope.
    /// Internally this will validate that blocks have the correct result type.
    fn pop_ctrl(&mut self, offset: usize) -> Result<Frame> {
        // Read the expected type and expected height of the operand stack the
        // end of the frame.
        let frame = match self.control.last() {
            Some(f) => f,
            None => return Err(self.err_beyond_end(offset)),
        };
        let ty = frame.block_type;
        let height = frame.height;

        // Pop all the result types, in reverse order, from the operand stack.
        // These types will, possibly, be transferred to the next frame.
        for ty in self.results(offset, ty)?.rev() {
            self.pop_operand(offset, Some(ty))?;
        }

        // Make sure that the operand stack has returned to is original
        // height...
        if self.operands.len() != height {
            bail_op_err!(
                offset,
                "type mismatch: values remaining on stack at end of block"
            );
        }

        // And then we can remove it!
        Ok(self.control.pop().unwrap())
    }

    /// Validates a relative jump to the `depth` specified.
    ///
    /// Returns the type signature of the block that we're jumping to as well
    /// as the kind of block if the jump is valid. Otherwise returns an error.
    fn jump(&self, offset: usize, depth: u32) -> Result<(BlockType, FrameKind)> {
        if self.control.len() == 0 {
            return Err(self.err_beyond_end(offset));
        }
        match (self.control.len() - 1).checked_sub(depth as usize) {
            Some(i) => {
                let frame = &self.control[i];
                Ok((frame.block_type, frame.kind))
            }
            None => bail_op_err!(offset, "unknown label: branch depth too large"),
        }
    }

    /// Validates that `memory_index` is valid in this module, and returns the
    /// type of address used to index the memory specified.
    fn check_memory_index(&self, offset: usize, memory_index: u32) -> Result<ValType> {
        if memory_index > 0 && !self.features.multi_memory {
            bail_op_err!(offset, "multi-memory support is not enabled");
        }
        match self.resources.memory_at(memory_index) {
            Some(mem) => Ok(mem.index_type()),
            None => bail_op_err!(offset, "unknown memory {}", memory_index),
        }
    }

    /// Validates a `memarg for alignment and such (also the memory it
    /// references), and returns the type of index used to address the memory.
    fn check_memarg(
        &self,
        memarg: MemoryImmediate,
        max_align: u8,
        offset: usize,
    ) -> Result<ValType> {
        let index_ty = self.check_memory_index(offset, memarg.memory)?;
        let align = memarg.align;
        if align > max_align {
            bail_op_err!(offset, "alignment must not be larger than natural");
        }
        if index_ty == ValType::I32 && memarg.offset > u64::from(u32::MAX) {
            bail_op_err!(offset, "offset out of range: must be <= 2**32");
        }
        Ok(index_ty)
    }

    #[cfg_attr(not(feature = "deterministic"), inline(always))]
    fn check_non_deterministic_enabled(&self, offset: usize) -> Result<()> {
        if cfg!(feature = "deterministic") && !self.features.deterministic_only {
            bail_op_err!(offset, "deterministic_only support is not enabled");
        }
        Ok(())
    }

    fn check_threads_enabled(&self, offset: usize) -> Result<()> {
        if !self.features.threads {
            bail_op_err!(offset, "threads support is not enabled")
        }
        Ok(())
    }

    fn check_reference_types_enabled(&self, offset: usize) -> Result<()> {
        if !self.features.reference_types {
            bail_op_err!(offset, "reference types support is not enabled")
        }
        Ok(())
    }

    /// Checks if Wasm proposal `saturating_float_to_int` is enabled.
    fn check_saturating_float_to_int_enabled(&self, offset: usize) -> Result<()> {
        if !self.features.saturating_float_to_int {
            bail_op_err!(
                offset,
                "saturating float to int conversions support is not enabled"
            );
        }
        Ok(())
    }

    /// Checks if Wasm proposal `sign_extension` is enabled.
    fn check_sign_extension_enabled(&self, offset: usize) -> Result<()> {
        if !self.features.sign_extension {
            bail_op_err!(offset, "sign extension operations support is not enabled");
        }
        Ok(())
    }

    fn check_simd_enabled(&self, offset: usize) -> Result<()> {
        if !self.features.simd {
            bail_op_err!(offset, "SIMD support is not enabled");
        }
        Ok(())
    }

    fn check_relaxed_simd_enabled(&self, offset: usize) -> Result<()> {
        // Relaxed SIMD operators make sense only with SIMD and be non-deterministic.
        self.check_non_deterministic_enabled(offset)?;
        self.check_simd_enabled(offset)?;
        if !self.features.relaxed_simd {
            bail_op_err!(offset, "Relaxed SIMD support is not enabled");
        }
        Ok(())
    }

    fn check_exceptions_enabled(&self, offset: usize) -> Result<()> {
        if !self.features.exceptions {
            bail_op_err!(offset, "Exceptions support is not enabled");
        }
        Ok(())
    }

    fn check_bulk_memory_enabled(&self, offset: usize) -> Result<()> {
        if !self.features.bulk_memory {
            bail_op_err!(offset, "bulk memory support is not enabled");
        }
        Ok(())
    }

    fn check_shared_memarg_wo_align(
        &self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Result<ValType> {
        self.check_memory_index(offset, memarg.memory)
    }

    fn check_simd_lane_index(&self, offset: usize, index: SIMDLaneIndex, max: u8) -> Result<()> {
        if index >= max {
            bail_op_err!(offset, "SIMD index out of bounds");
        }
        Ok(())
    }

    /// Validates a block type, primarily with various in-flight proposals.
    fn check_block_type(&self, offset: usize, ty: BlockType) -> Result<()> {
        match ty {
            BlockType::Empty => Ok(()),
            BlockType::Type(ty) => self
                .features
                .check_value_type(ty)
                .map_err(|e| BinaryReaderError::new(e, offset)),
            BlockType::FuncType(idx) => {
                if !self.features.multi_value {
                    bail_op_err!(
                        offset,
                        "blocks, loops, and ifs may only produce a resulttype \
                         when multi-value is not enabled",
                    );
                }
                self.func_type_at(idx, offset)?;
                Ok(())
            }
        }
    }

    /// Validates a `call` instruction, ensuring that the function index is
    /// in-bounds and the right types are on the stack to call the function.
    fn check_call(&mut self, offset: usize, function_index: u32) -> Result<()> {
        let ty = match self.resources.type_of_function(function_index) {
            Some(i) => i,
            None => {
                bail_op_err!(
                    offset,
                    "unknown function {}: function index out of bounds",
                    function_index
                );
            }
        };
        for ty in ty.inputs().rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        for ty in ty.outputs() {
            self.push_operand(ty)?;
        }
        Ok(())
    }

    /// Validates a call to an indirect function, very similar to `check_call`.
    fn check_call_indirect(&mut self, offset: usize, index: u32, table_index: u32) -> Result<()> {
        match self.resources.table_at(table_index) {
            None => {
                bail_op_err!(offset, "unknown table: table index out of bounds");
            }
            Some(tab) => {
                if tab.element_type != ValType::FuncRef {
                    bail_op_err!(offset, "indirect calls must go through a table of funcref");
                }
            }
        }
        let ty = self.func_type_at(index, offset)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        for ty in ty.inputs().rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        for ty in ty.outputs() {
            self.push_operand(ty)?;
        }
        Ok(())
    }

    /// Validates a `return` instruction, popping types from the operand
    /// stack that the function needs.
    fn check_return(&mut self, offset: usize) -> Result<()> {
        if self.control.is_empty() {
            return Err(self.err_beyond_end(offset));
        }
        for ty in self.results(offset, self.control[0].block_type)?.rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        self.unreachable(offset)?;
        Ok(())
    }

    /// Checks the validity of a common comparison operator.
    fn check_cmp_op(&mut self, offset: usize, ty: ValType) -> Result<()> {
        self.pop_operand(offset, Some(ty))?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }

    /// Checks the validity of a common float comparison operator.
    fn check_fcmp_op(&mut self, offset: usize, ty: ValType) -> Result<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled(offset)?;
        self.check_cmp_op(offset, ty)
    }

    /// Checks the validity of a common unary operator.
    fn check_unary_op(&mut self, offset: usize, ty: ValType) -> Result<()> {
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ty)?;
        Ok(())
    }

    /// Checks the validity of a common unary float operator.
    fn check_funary_op(&mut self, offset: usize, ty: ValType) -> Result<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled(offset)?;
        self.check_unary_op(offset, ty)
    }

    /// Checks the validity of a common conversion operator.
    fn check_conversion_op(&mut self, offset: usize, into: ValType, from: ValType) -> Result<()> {
        self.pop_operand(offset, Some(from))?;
        self.push_operand(into)?;
        Ok(())
    }

    /// Checks the validity of a common conversion operator.
    fn check_fconversion_op(&mut self, offset: usize, into: ValType, from: ValType) -> Result<()> {
        debug_assert!(matches!(into, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled(offset)?;
        self.check_conversion_op(offset, into, from)
    }

    /// Checks the validity of a common binary operator.
    fn check_binary_op(&mut self, offset: usize, ty: ValType) -> Result<()> {
        self.pop_operand(offset, Some(ty))?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ty)?;
        Ok(())
    }

    /// Checks the validity of a common binary float operator.
    fn check_fbinary_op(&mut self, offset: usize, ty: ValType) -> Result<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled(offset)?;
        self.check_binary_op(offset, ty)
    }

    /// Checks the validity of an atomic load operator.
    fn check_atomic_load(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        load_ty: ValType,
    ) -> Result<()> {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(load_ty)?;
        Ok(())
    }

    /// Checks the validity of an atomic store operator.
    fn check_atomic_store(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        store_ty: ValType,
    ) -> Result<()> {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.pop_operand(offset, Some(store_ty))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }

    /// Checks the validity of a common atomic binary operator.
    fn check_atomic_binary_op(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        op_ty: ValType,
    ) -> Result<()> {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.pop_operand(offset, Some(op_ty))?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(op_ty)?;
        Ok(())
    }

    /// Checks the validity of an atomic compare exchange operator.
    fn check_atomic_binary_cmpxchg(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        op_ty: ValType,
    ) -> Result<()> {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.pop_operand(offset, Some(op_ty))?;
        self.pop_operand(offset, Some(op_ty))?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(op_ty)?;
        Ok(())
    }

    /// Checks a [`V128`] splat operator.
    fn check_v128_splat(&mut self, offset: usize, src_ty: ValType) -> Result<()> {
        self.check_simd_enabled(offset)?;
        self.pop_operand(offset, Some(src_ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_binary_op(&mut self, offset: usize) -> Result<()> {
        self.check_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary float operator.
    fn check_v128_fbinary_op(&mut self, offset: usize) -> Result<()> {
        self.check_non_deterministic_enabled(offset)?;
        self.check_v128_binary_op(offset)
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_relaxed_binary_op(&mut self, offset: usize) -> Result<()> {
        self.check_relaxed_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_unary_op(&mut self, offset: usize) -> Result<()> {
        self.check_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_funary_op(&mut self, offset: usize) -> Result<()> {
        self.check_non_deterministic_enabled(offset)?;
        self.check_v128_unary_op(offset)
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_relaxed_unary_op(&mut self, offset: usize) -> Result<()> {
        self.check_relaxed_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_relaxed_ternary_op(&mut self, offset: usize) -> Result<()> {
        self.check_relaxed_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_bitmask_op(&mut self, offset: usize) -> Result<()> {
        self.check_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_shift_op(&mut self, offset: usize) -> Result<()> {
        self.check_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] common load operator.
    fn check_v128_load_op(&mut self, offset: usize, memarg: MemoryImmediate) -> Result<()> {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 3, offset)?;
        self.pop_operand(offset, Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }

    fn func_type_at(&self, at: u32, offset: usize) -> Result<&'resources R::FuncType> {
        self.resources
            .func_type_at(at)
            .ok_or_else(|| format_op_err!(offset, "unknown type: type index out of bounds"))
    }

    fn tag_at(&self, at: u32, offset: usize) -> Result<&'resources R::FuncType> {
        self.resources
            .tag_at(at)
            .ok_or_else(|| format_op_err!(offset, "unknown tag {}: tag index out of bounds", at))
    }

    fn params(
        &self,
        offset: usize,
        ty: BlockType,
    ) -> Result<impl PreciseIterator<Item = ValType> + 'resources> {
        Ok(match ty {
            BlockType::Empty | BlockType::Type(_) => Either::B(None.into_iter()),
            BlockType::FuncType(t) => Either::A(self.func_type_at(t, offset)?.inputs()),
        })
    }

    fn results(
        &self,
        offset: usize,
        ty: BlockType,
    ) -> Result<impl PreciseIterator<Item = ValType> + 'resources> {
        Ok(match ty {
            BlockType::Empty => Either::B(None.into_iter()),
            BlockType::Type(t) => Either::B(Some(t).into_iter()),
            BlockType::FuncType(t) => Either::A(self.func_type_at(t, offset)?.outputs()),
        })
    }

    fn label_types(
        &self,
        offset: usize,
        ty: BlockType,
        kind: FrameKind,
    ) -> Result<impl PreciseIterator<Item = ValType> + 'resources> {
        Ok(match kind {
            FrameKind::Loop => Either::A(self.params(offset, ty)?),
            _ => Either::B(self.results(offset, ty)?),
        })
    }
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

impl<'a, T> VisitOperator<'a> for OperatorValidatorTemp<'_, '_, T>
where
    T: WasmModuleResources,
{
    type Output = Result<()>;

    fn visit_nop(&mut self, _: usize) -> Self::Output {
        Ok(())
    }
    fn visit_unreachable(&mut self, offset: usize) -> Self::Output {
        self.unreachable(offset)?;
        Ok(())
    }
    fn visit_block(&mut self, offset: usize, ty: BlockType) -> Self::Output {
        self.check_block_type(offset, ty)?;
        for ty in self.params(offset, ty)?.rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        self.push_ctrl(offset, FrameKind::Block, ty)?;
        Ok(())
    }
    fn visit_loop(&mut self, offset: usize, ty: BlockType) -> Self::Output {
        self.check_block_type(offset, ty)?;
        for ty in self.params(offset, ty)?.rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        self.push_ctrl(offset, FrameKind::Loop, ty)?;
        Ok(())
    }
    fn visit_if(&mut self, offset: usize, ty: BlockType) -> Self::Output {
        self.check_block_type(offset, ty)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        for ty in self.params(offset, ty)?.rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        self.push_ctrl(offset, FrameKind::If, ty)?;
        Ok(())
    }
    fn visit_else(&mut self, offset: usize) -> Self::Output {
        let frame = self.pop_ctrl(offset)?;
        if frame.kind != FrameKind::If {
            bail_op_err!(offset, "else found outside of an `if` block");
        }
        self.push_ctrl(offset, FrameKind::Else, frame.block_type)?;
        Ok(())
    }
    fn visit_try(&mut self, offset: usize, ty: BlockType) -> Self::Output {
        self.check_exceptions_enabled(offset)?;
        self.check_block_type(offset, ty)?;
        for ty in self.params(offset, ty)?.rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        self.push_ctrl(offset, FrameKind::Try, ty)?;
        Ok(())
    }
    fn visit_catch(&mut self, offset: usize, index: u32) -> Self::Output {
        self.check_exceptions_enabled(offset)?;
        let frame = self.pop_ctrl(offset)?;
        if frame.kind != FrameKind::Try && frame.kind != FrameKind::Catch {
            bail_op_err!(offset, "catch found outside of an `try` block");
        }
        // Start a new frame and push `exnref` value.
        let height = self.operands.len();
        self.control.push(Frame {
            kind: FrameKind::Catch,
            block_type: frame.block_type,
            height,
            unreachable: false,
        });
        // Push exception argument types.
        let ty = self.tag_at(index, offset)?;
        for ty in ty.inputs() {
            self.push_operand(ty)?;
        }
        Ok(())
    }
    fn visit_throw(&mut self, offset: usize, index: u32) -> Self::Output {
        self.check_exceptions_enabled(offset)?;
        // Check values associated with the exception.
        let ty = self.tag_at(index, offset)?;
        for ty in ty.inputs().rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        if ty.outputs().len() > 0 {
            bail_op_err!(offset, "result type expected to be empty for exception");
        }
        self.unreachable(offset)?;
        Ok(())
    }
    fn visit_rethrow(&mut self, offset: usize, relative_depth: u32) -> Self::Output {
        self.check_exceptions_enabled(offset)?;
        // This is not a jump, but we need to check that the `rethrow`
        // targets an actual `catch` to get the exception.
        let (_, kind) = self.jump(offset, relative_depth)?;
        if kind != FrameKind::Catch && kind != FrameKind::CatchAll {
            bail_op_err!(
                offset,
                "invalid rethrow label: target was not a `catch` block"
            );
        }
        self.unreachable(offset)?;
        Ok(())
    }
    fn visit_delegate(&mut self, offset: usize, relative_depth: u32) -> Self::Output {
        self.check_exceptions_enabled(offset)?;
        let frame = self.pop_ctrl(offset)?;
        if frame.kind != FrameKind::Try {
            bail_op_err!(offset, "delegate found outside of an `try` block");
        }
        // This operation is not a jump, but we need to check the
        // depth for validity
        let _ = self.jump(offset, relative_depth)?;
        for ty in self.results(offset, frame.block_type)? {
            self.push_operand(ty)?;
        }
        Ok(())
    }
    fn visit_catch_all(&mut self, offset: usize) -> Self::Output {
        self.check_exceptions_enabled(offset)?;
        let frame = self.pop_ctrl(offset)?;
        if frame.kind == FrameKind::CatchAll {
            bail_op_err!(offset, "only one catch_all allowed per `try` block");
        } else if frame.kind != FrameKind::Try && frame.kind != FrameKind::Catch {
            bail_op_err!(offset, "catch_all found outside of a `try` block");
        }
        let height = self.operands.len();
        self.control.push(Frame {
            kind: FrameKind::CatchAll,
            block_type: frame.block_type,
            height,
            unreachable: false,
        });
        Ok(())
    }
    fn visit_end(&mut self, offset: usize) -> Self::Output {
        let mut frame = self.pop_ctrl(offset)?;

        // Note that this `if` isn't included in the appendix right
        // now, but it's used to allow for `if` statements that are
        // missing an `else` block which have the same parameter/return
        // types on the block (since that's valid).
        if frame.kind == FrameKind::If {
            self.push_ctrl(offset, FrameKind::Else, frame.block_type)?;
            frame = self.pop_ctrl(offset)?;
        }
        for ty in self.results(offset, frame.block_type)? {
            self.push_operand(ty)?;
        }

        if self.control.is_empty() && self.end_which_emptied_control.is_none() {
            self.end_which_emptied_control = Some(offset);
        }
        Ok(())
    }
    fn visit_br(&mut self, offset: usize, relative_depth: u32) -> Self::Output {
        let (ty, kind) = self.jump(offset, relative_depth)?;
        for ty in self.label_types(offset, ty, kind)?.rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        self.unreachable(offset)?;
        Ok(())
    }
    fn visit_br_if(&mut self, offset: usize, relative_depth: u32) -> Self::Output {
        self.pop_operand(offset, Some(ValType::I32))?;
        let (ty, kind) = self.jump(offset, relative_depth)?;
        for ty in self.label_types(offset, ty, kind)?.rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        for ty in self.label_types(offset, ty, kind)? {
            self.push_operand(ty)?;
        }
        Ok(())
    }
    fn visit_br_table(&mut self, offset: usize, table: &BrTable) -> Self::Output {
        self.pop_operand(offset, Some(ValType::I32))?;
        let default = self.jump(offset, table.default())?;
        let default_types = self.label_types(offset, default.0, default.1)?;
        for element in table.targets() {
            let relative_depth = element?;
            let block = self.jump(offset, relative_depth)?;
            let tys = self.label_types(offset, block.0, block.1)?;
            if tys.len() != default_types.len() {
                bail_op_err!(
                    offset,
                    "type mismatch: br_table target labels have different number of types"
                );
            }
            debug_assert!(self.br_table_tmp.is_empty());
            for ty in tys.rev() {
                let ty = self.pop_operand(offset, Some(ty))?;
                self.br_table_tmp.push(ty);
            }
            for ty in self.inner.br_table_tmp.drain(..).rev() {
                self.inner.operands.push(ty);
            }
        }
        for ty in default_types.rev() {
            self.pop_operand(offset, Some(ty))?;
        }
        self.unreachable(offset)?;
        Ok(())
    }
    fn visit_return(&mut self, offset: usize) -> Self::Output {
        self.check_return(offset)?;
        Ok(())
    }
    fn visit_call(&mut self, offset: usize, function_index: u32) -> Self::Output {
        self.check_call(offset, function_index)?;
        Ok(())
    }
    fn visit_return_call(&mut self, offset: usize, function_index: u32) -> Self::Output {
        if !self.features.tail_call {
            bail_op_err!(offset, "tail calls support is not enabled");
        }
        self.check_call(offset, function_index)?;
        self.check_return(offset)?;
        Ok(())
    }
    fn visit_call_indirect(
        &mut self,
        offset: usize,
        index: u32,
        table_index: u32,
        table_byte: u8,
    ) -> Self::Output {
        if table_byte != 0 && !self.features.reference_types {
            bail_op_err!(offset, "reference-types not enabled: zero byte expected");
        }
        self.check_call_indirect(offset, index, table_index)?;
        Ok(())
    }
    fn visit_return_call_indirect(
        &mut self,
        offset: usize,
        index: u32,
        table_index: u32,
    ) -> Self::Output {
        if !self.features.tail_call {
            bail_op_err!(offset, "tail calls support is not enabled");
        }
        self.check_call_indirect(offset, index, table_index)?;
        self.check_return(offset)?;
        Ok(())
    }
    fn visit_drop(&mut self, offset: usize) -> Self::Output {
        self.pop_operand(offset, None)?;
        Ok(())
    }
    fn visit_select(&mut self, offset: usize) -> Self::Output {
        self.pop_operand(offset, Some(ValType::I32))?;
        let ty1 = self.pop_operand(offset, None)?;
        let ty2 = self.pop_operand(offset, None)?;
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
            bail_op_err!(offset, "type mismatch: select only takes integral types")
        }
        if ty1 != ty2 && ty1 != None && ty2 != None {
            bail_op_err!(
                offset,
                "type mismatch: select operands have different types"
            )
        }
        self.push_operand(ty1.or(ty2))?;
        Ok(())
    }
    fn visit_typed_select(&mut self, offset: usize, ty: ValType) -> Self::Output {
        self.features
            .check_value_type(ty)
            .map_err(|e| BinaryReaderError::new(e, offset))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_local_get(&mut self, offset: usize, local_index: u32) -> Self::Output {
        let ty = self.local(offset, local_index)?;
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_local_set(&mut self, offset: usize, local_index: u32) -> Self::Output {
        let ty = self.local(offset, local_index)?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_local_tee(&mut self, offset: usize, local_index: u32) -> Self::Output {
        let ty = self.local(offset, local_index)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_global_get(&mut self, offset: usize, global_index: u32) -> Self::Output {
        if let Some(ty) = self.resources.global_at(global_index) {
            self.push_operand(ty.content_type)?;
        } else {
            bail_op_err!(offset, "unknown global: global index out of bounds");
        };
        Ok(())
    }
    fn visit_global_set(&mut self, offset: usize, global_index: u32) -> Self::Output {
        if let Some(ty) = self.resources.global_at(global_index) {
            if !ty.mutable {
                bail_op_err!(
                    offset,
                    "global is immutable: cannot modify it with `global.set`"
                );
            }
            self.pop_operand(offset, Some(ty.content_type))?;
        } else {
            bail_op_err!(offset, "unknown global: global index out of bounds");
        };
        Ok(())
    }
    fn visit_i32_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i64_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 3, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_f32_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::F32)?;
        Ok(())
    }
    fn visit_f64_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        let ty = self.check_memarg(memarg, 3, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::F64)?;
        Ok(())
    }
    fn visit_i32_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i32_load8_u(&mut self, input: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i32_load8_s(input, memarg)
    }
    fn visit_i32_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i32_load16_u(&mut self, input: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i32_load16_s(input, memarg)
    }
    fn visit_i64_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load8_u(&mut self, input: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load8_s(input, memarg)
    }
    fn visit_i64_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load16_u(&mut self, input: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load16_s(input, memarg)
    }
    fn visit_i64_load32_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load32_u(&mut self, input: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load32_s(input, memarg)
    }
    fn visit_i32_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i64_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 3, offset)?;
        self.pop_operand(offset, Some(ValType::I64))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_f32_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.pop_operand(offset, Some(ValType::F32))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_f64_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        let ty = self.check_memarg(memarg, 3, offset)?;
        self.pop_operand(offset, Some(ValType::F64))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i32_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i32_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i64_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.pop_operand(offset, Some(ValType::I64))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i64_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.pop_operand(offset, Some(ValType::I64))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i64_store32(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.pop_operand(offset, Some(ValType::I64))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_memory_size(&mut self, offset: usize, mem: u32, mem_byte: u8) -> Self::Output {
        if mem_byte != 0 && !self.features.multi_memory {
            bail_op_err!(offset, "multi-memory not enabled: zero byte expected");
        }
        let index_ty = self.check_memory_index(offset, mem)?;
        self.push_operand(index_ty)?;
        Ok(())
    }
    fn visit_memory_grow(&mut self, offset: usize, mem: u32, mem_byte: u8) -> Self::Output {
        if mem_byte != 0 && !self.features.multi_memory {
            bail_op_err!(offset, "multi-memory not enabled: zero byte expected");
        }
        let index_ty = self.check_memory_index(offset, mem)?;
        self.pop_operand(offset, Some(index_ty))?;
        self.push_operand(index_ty)?;
        Ok(())
    }
    fn visit_i32_const(&mut self, _offset: usize, _value: i32) -> Self::Output {
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i64_const(&mut self, _offset: usize, _value: i64) -> Self::Output {
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_f32_const(&mut self, offset: usize, _value: Ieee32) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        self.push_operand(ValType::F32)?;
        Ok(())
    }
    fn visit_f64_const(&mut self, offset: usize, _value: Ieee64) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        self.push_operand(ValType::F64)?;
        Ok(())
    }
    fn visit_i32_eqz(&mut self, offset: usize) -> Self::Output {
        self.pop_operand(offset, Some(ValType::I32))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i32_eq(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_ne(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_lt_s(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_lt_u(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_gt_s(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_gt_u(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_le_s(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_le_u(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_ge_s(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i32_ge_u(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I32)
    }
    fn visit_i64_eqz(&mut self, offset: usize) -> Self::Output {
        self.pop_operand(offset, Some(ValType::I64))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i64_eq(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_ne(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_lt_s(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_lt_u(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_gt_s(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_gt_u(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_le_s(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_le_u(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_ge_s(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_i64_ge_u(&mut self, offset: usize) -> Self::Output {
        self.check_cmp_op(offset, ValType::I64)
    }
    fn visit_f32_eq(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F32)
    }
    fn visit_f32_ne(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F32)
    }
    fn visit_f32_lt(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F32)
    }
    fn visit_f32_gt(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F32)
    }
    fn visit_f32_le(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F32)
    }
    fn visit_f32_ge(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F32)
    }
    fn visit_f64_eq(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F64)
    }
    fn visit_f64_ne(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F64)
    }
    fn visit_f64_lt(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F64)
    }
    fn visit_f64_gt(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F64)
    }
    fn visit_f64_le(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F64)
    }
    fn visit_f64_ge(&mut self, offset: usize) -> Self::Output {
        self.check_fcmp_op(offset, ValType::F64)
    }
    fn visit_i32_clz(&mut self, offset: usize) -> Self::Output {
        self.check_unary_op(offset, ValType::I32)
    }
    fn visit_i32_ctz(&mut self, offset: usize) -> Self::Output {
        self.check_unary_op(offset, ValType::I32)
    }
    fn visit_i32_popcnt(&mut self, offset: usize) -> Self::Output {
        self.check_unary_op(offset, ValType::I32)
    }
    fn visit_i32_add(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_sub(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_mul(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_div_s(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_div_u(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_rem_s(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_rem_u(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_and(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_or(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_xor(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_shl(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_shr_s(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_shr_u(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_rotl(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i32_rotr(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I32)
    }
    fn visit_i64_clz(&mut self, offset: usize) -> Self::Output {
        self.check_unary_op(offset, ValType::I64)
    }
    fn visit_i64_ctz(&mut self, offset: usize) -> Self::Output {
        self.check_unary_op(offset, ValType::I64)
    }
    fn visit_i64_popcnt(&mut self, offset: usize) -> Self::Output {
        self.check_unary_op(offset, ValType::I64)
    }
    fn visit_i64_add(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_sub(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_mul(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_div_s(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_div_u(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_rem_s(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_rem_u(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_and(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_or(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_xor(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_shl(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_shr_s(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_shr_u(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_rotl(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_i64_rotr(&mut self, offset: usize) -> Self::Output {
        self.check_binary_op(offset, ValType::I64)
    }
    fn visit_f32_abs(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F32)
    }
    fn visit_f32_neg(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F32)
    }
    fn visit_f32_ceil(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F32)
    }
    fn visit_f32_floor(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F32)
    }
    fn visit_f32_trunc(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F32)
    }
    fn visit_f32_nearest(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F32)
    }
    fn visit_f32_sqrt(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F32)
    }
    fn visit_f32_add(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F32)
    }
    fn visit_f32_sub(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F32)
    }
    fn visit_f32_mul(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F32)
    }
    fn visit_f32_div(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F32)
    }
    fn visit_f32_min(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F32)
    }
    fn visit_f32_max(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F32)
    }
    fn visit_f32_copysign(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F32)
    }
    fn visit_f64_abs(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F64)
    }
    fn visit_f64_neg(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F64)
    }
    fn visit_f64_ceil(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F64)
    }
    fn visit_f64_floor(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F64)
    }
    fn visit_f64_trunc(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F64)
    }
    fn visit_f64_nearest(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F64)
    }
    fn visit_f64_sqrt(&mut self, offset: usize) -> Self::Output {
        self.check_funary_op(offset, ValType::F64)
    }
    fn visit_f64_add(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F64)
    }
    fn visit_f64_sub(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F64)
    }
    fn visit_f64_mul(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F64)
    }
    fn visit_f64_div(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F64)
    }
    fn visit_f64_min(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F64)
    }
    fn visit_f64_max(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F64)
    }
    fn visit_f64_copysign(&mut self, offset: usize) -> Self::Output {
        self.check_fbinary_op(offset, ValType::F64)
    }
    fn visit_i32_wrap_i64(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I32, ValType::I64)
    }
    fn visit_i32_trunc_f32s(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I32, ValType::F32)
    }
    fn visit_i32_trunc_f32u(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I32, ValType::F32)
    }
    fn visit_i32_trunc_f64s(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I32, ValType::F64)
    }
    fn visit_i32_trunc_f64u(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I32, ValType::F64)
    }
    fn visit_i64_extend_i32s(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I64, ValType::I32)
    }
    fn visit_i64_extend_i32u(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I64, ValType::I32)
    }
    fn visit_i64_trunc_f32s(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I64, ValType::F32)
    }
    fn visit_i64_trunc_f32u(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I64, ValType::F32)
    }
    fn visit_i64_trunc_f64s(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I64, ValType::F64)
    }
    fn visit_i64_trunc_f64u(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I64, ValType::F64)
    }
    fn visit_f32_convert_i32s(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F32, ValType::I32)
    }
    fn visit_f32_convert_i32u(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F32, ValType::I32)
    }
    fn visit_f32_convert_i64s(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F32, ValType::I64)
    }
    fn visit_f32_convert_i64u(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F32, ValType::I64)
    }
    fn visit_f32_demote_f64(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F32, ValType::F64)
    }
    fn visit_f64_convert_i32s(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F64, ValType::I32)
    }
    fn visit_f64_convert_i32u(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F64, ValType::I32)
    }
    fn visit_f64_convert_i64s(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F64, ValType::I64)
    }
    fn visit_f64_convert_i64u(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F64, ValType::I64)
    }
    fn visit_f64_promote_f32(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F64, ValType::F32)
    }
    fn visit_i32_reinterpret_f32(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I32, ValType::F32)
    }
    fn visit_i64_reinterpret_f64(&mut self, offset: usize) -> Self::Output {
        self.check_conversion_op(offset, ValType::I64, ValType::F64)
    }
    fn visit_f32_reinterpret_i32(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F32, ValType::I32)
    }
    fn visit_f64_reinterpret_i64(&mut self, offset: usize) -> Self::Output {
        self.check_fconversion_op(offset, ValType::F64, ValType::I64)
    }
    fn visit_i32_trunc_sat_f32s(&mut self, offset: usize) -> Self::Output {
        self.check_saturating_float_to_int_enabled(offset)?;
        self.check_conversion_op(offset, ValType::I32, ValType::F32)
    }
    fn visit_i32_trunc_sat_f32u(&mut self, offset: usize) -> Self::Output {
        self.check_saturating_float_to_int_enabled(offset)?;
        self.check_conversion_op(offset, ValType::I32, ValType::F32)
    }
    fn visit_i32_trunc_sat_f64s(&mut self, offset: usize) -> Self::Output {
        self.check_saturating_float_to_int_enabled(offset)?;
        self.check_conversion_op(offset, ValType::I32, ValType::F64)
    }
    fn visit_i32_trunc_sat_f64u(&mut self, offset: usize) -> Self::Output {
        self.check_saturating_float_to_int_enabled(offset)?;
        self.check_conversion_op(offset, ValType::I32, ValType::F64)
    }
    fn visit_i64_trunc_sat_f32s(&mut self, offset: usize) -> Self::Output {
        self.check_saturating_float_to_int_enabled(offset)?;
        self.check_conversion_op(offset, ValType::I64, ValType::F32)
    }
    fn visit_i64_trunc_sat_f32u(&mut self, offset: usize) -> Self::Output {
        self.check_saturating_float_to_int_enabled(offset)?;
        self.check_conversion_op(offset, ValType::I64, ValType::F32)
    }
    fn visit_i64_trunc_sat_f64s(&mut self, offset: usize) -> Self::Output {
        self.check_saturating_float_to_int_enabled(offset)?;
        self.check_conversion_op(offset, ValType::I64, ValType::F64)
    }
    fn visit_i64_trunc_sat_f64u(&mut self, offset: usize) -> Self::Output {
        self.check_saturating_float_to_int_enabled(offset)?;
        self.check_conversion_op(offset, ValType::I64, ValType::F64)
    }
    fn visit_i32_extend8_s(&mut self, offset: usize) -> Self::Output {
        self.check_sign_extension_enabled(offset)?;
        self.check_unary_op(offset, ValType::I32)
    }
    fn visit_i32_extend16_s(&mut self, offset: usize) -> Self::Output {
        self.check_sign_extension_enabled(offset)?;
        self.check_unary_op(offset, ValType::I32)
    }
    fn visit_i64_extend8_s(&mut self, offset: usize) -> Self::Output {
        self.check_sign_extension_enabled(offset)?;
        self.check_unary_op(offset, ValType::I64)
    }
    fn visit_i64_extend16_s(&mut self, offset: usize) -> Self::Output {
        self.check_sign_extension_enabled(offset)?;
        self.check_unary_op(offset, ValType::I64)
    }
    fn visit_i64_extend32_s(&mut self, offset: usize) -> Self::Output {
        self.check_sign_extension_enabled(offset)?;
        self.check_unary_op(offset, ValType::I64)
    }
    fn visit_i32_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_load(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_load16_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_load(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_load(offset, memarg, ValType::I32)
    }
    fn visit_i64_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_load(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_load32_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_load(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_load16_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_load(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_load(offset, memarg, ValType::I64)
    }
    fn visit_i32_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(offset, memarg, ValType::I32)
    }
    fn visit_i64_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_store32(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_store(offset, memarg, ValType::I64)
    }
    fn visit_i32_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i64_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i32_atomic_rmw_xchg(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw_cmpxchg(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw16_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(offset, memarg, ValType::I32)
    }
    fn visit_i32_atomic_rmw8_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(offset, memarg, ValType::I32)
    }
    fn visit_i64_atomic_rmw_xchg(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw_cmpxchg(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw32_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw16_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(offset, memarg, ValType::I64)
    }
    fn visit_i64_atomic_rmw8_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_cmpxchg(offset, memarg, ValType::I64)
    }
    fn visit_memory_atomic_notify(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_atomic_binary_op(offset, memarg, ValType::I32)
    }
    fn visit_memory_atomic_wait32(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.pop_operand(offset, Some(ValType::I64))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_memory_atomic_wait64(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.pop_operand(offset, Some(ValType::I64))?;
        self.pop_operand(offset, Some(ValType::I64))?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_atomic_fence(&mut self, offset: usize, flags: u8) -> Self::Output {
        self.check_threads_enabled(offset)?;
        if flags != 0 {
            bail_op_err!(offset, "non-zero flags for fence not supported yet");
        }
        Ok(())
    }
    fn visit_ref_null(&mut self, offset: usize, ty: ValType) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        self.features
            .check_value_type(ty)
            .map_err(|e| BinaryReaderError::new(e, offset))?;
        if !ty.is_reference_type() {
            bail_op_err!(offset, "invalid non-reference type in ref.null");
        }
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_ref_is_null(&mut self, offset: usize) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        match self.pop_operand(offset, None)? {
            None => {}
            Some(t) => {
                if !t.is_reference_type() {
                    bail_op_err!(
                        offset,
                        "type mismatch: invalid reference type in ref.is_null"
                    );
                }
            }
        }
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_ref_func(&mut self, offset: usize, function_index: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        if self.resources.type_of_function(function_index).is_none() {
            bail_op_err!(
                offset,
                "unknown function {}: function index out of bounds",
                function_index,
            );
        }
        if !self.resources.is_function_referenced(function_index) {
            bail_op_err!(offset, "undeclared function reference");
        }
        self.push_operand(ValType::FuncRef)?;
        Ok(())
    }
    fn visit_v128_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 4, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 4, offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_v128_const(&mut self, offset: usize, _value: V128) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i8x16_splat(&mut self, offset: usize) -> Self::Output {
        self.check_v128_splat(offset, ValType::I32)
    }
    fn visit_i16x8_splat(&mut self, offset: usize) -> Self::Output {
        self.check_v128_splat(offset, ValType::I32)
    }
    fn visit_i32x4_splat(&mut self, offset: usize) -> Self::Output {
        self.check_v128_splat(offset, ValType::I32)
    }
    fn visit_i64x2_splat(&mut self, offset: usize) -> Self::Output {
        self.check_v128_splat(offset, ValType::I64)
    }
    fn visit_f32x4_splat(&mut self, offset: usize) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        self.check_v128_splat(offset, ValType::F32)
    }
    fn visit_f64x2_splat(&mut self, offset: usize) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        self.check_v128_splat(offset, ValType::F64)
    }
    fn visit_i8x16_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 16)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i8x16_extract_lane_u(&mut self, input: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.visit_i8x16_extract_lane_s(input, lane)
    }
    fn visit_i16x8_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 8)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i16x8_extract_lane_u(&mut self, input: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.visit_i16x8_extract_lane_s(input, lane)
    }
    fn visit_i32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_i8x16_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 16)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i16x8_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 8)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::I64)?;
        Ok(())
    }
    fn visit_i64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.pop_operand(offset, Some(ValType::I64))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_f32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::F32)?;
        Ok(())
    }
    fn visit_f32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.pop_operand(offset, Some(ValType::F32))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_f64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::F64)?;
        Ok(())
    }
    fn visit_f64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled(offset)?;
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.pop_operand(offset, Some(ValType::F64))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_f32x4_eq(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_ne(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_lt(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_gt(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_le(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_ge(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_eq(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_ne(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_lt(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_gt(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_le(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_ge(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_add(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_sub(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_mul(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_div(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_min(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_max(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_p_min(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_p_max(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_add(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_sub(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_mul(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_div(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_min(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_max(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_p_min(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f64x2_p_max(&mut self, offset: usize) -> Self::Output {
        self.check_v128_fbinary_op(offset)
    }
    fn visit_f32x4_relaxed_min(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_binary_op(offset)
    }
    fn visit_f32x4_relaxed_max(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_binary_op(offset)
    }
    fn visit_f64x2_relaxed_min(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_binary_op(offset)
    }
    fn visit_f64x2_relaxed_max(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_binary_op(offset)
    }
    fn visit_i8x16_eq(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_ne(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_lt_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_lt_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_gt_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_gt_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_le_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_le_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_ge_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_ge_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_eq(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_ne(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_lt_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_lt_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_gt_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_gt_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_le_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_le_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_ge_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_ge_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_eq(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_ne(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_lt_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_lt_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_gt_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_gt_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_le_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_le_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_ge_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_ge_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_eq(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_ne(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_lt_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_gt_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_le_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_ge_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_v128_and(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_v128_and_not(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_v128_or(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_v128_xor(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_add(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_add_sat_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_add_sat_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_sub(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_sub_sat_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_sub_sat_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_min_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_min_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_max_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_max_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_add(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_add_sat_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_add_sat_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_sub(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_sub_sat_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_sub_sat_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_mul(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_min_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_min_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_max_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_max_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_add(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_sub(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_mul(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_min_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_min_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_max_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_max_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_dot_i16x8_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_add(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_sub(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_mul(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_rounding_average_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_rounding_average_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_narrow_i16x8_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i8x16_narrow_i16x8_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_narrow_i32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_narrow_i32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_ext_mul_low_i8x16_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_ext_mul_high_i8x16_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_ext_mul_low_i8x16_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_ext_mul_high_i8x16_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_ext_mul_low_i16x8_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_ext_mul_high_i16x8_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_ext_mul_low_i16x8_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i32x4_ext_mul_high_i16x8_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_ext_mul_low_i32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_ext_mul_high_i32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_ext_mul_low_i32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i64x2_ext_mul_high_i32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_i16x8_q15_mulr_sat_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_binary_op(offset)
    }
    fn visit_f32x4_ceil(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_floor(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_trunc(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_nearest(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_ceil(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_floor(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_trunc(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_nearest(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_abs(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_neg(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_sqrt(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_abs(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_neg(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_sqrt(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_demote_f64x2_zero(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_promote_low_f32x4(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_convert_low_i32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f64x2_convert_low_i32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_i32x4_trunc_sat_f32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_i32x4_trunc_sat_f32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_convert_i32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_f32x4_convert_i32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_funary_op(offset)
    }
    fn visit_v128_not(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i8x16_abs(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i8x16_neg(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i8x16_popcnt(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i16x8_abs(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i16x8_neg(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_abs(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_neg(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i64x2_abs(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i64x2_neg(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i16x8_extend_low_i8x16_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i16x8_extend_high_i8x16_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i16x8_extend_low_i8x16_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i16x8_extend_high_i8x16_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_extend_low_i16x8_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_extend_high_i16x8_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_extend_low_i16x8_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_extend_high_i16x8_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i64x2_extend_low_i32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i64x2_extend_high_i32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i64x2_extend_low_i32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i64x2_extend_high_i32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i16x8_ext_add_pairwise_i8x16_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i16x8_ext_add_pairwise_i8x16_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_ext_add_pairwise_i16x8_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_ext_add_pairwise_i16x8_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_unary_op(offset)
    }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_unary_op(offset)
    }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_unary_op(offset)
    }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_unary_op(offset)
    }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_unary_op(offset)
    }
    fn visit_v128_bitselect(&mut self, offset: usize) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_f32x4_fma(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_ternary_op(offset)
    }
    fn visit_f32x4_fms(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_ternary_op(offset)
    }
    fn visit_f64x2_fma(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_ternary_op(offset)
    }
    fn visit_f64x2_fms(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_ternary_op(offset)
    }
    fn visit_i8x16_lane_select(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_ternary_op(offset)
    }
    fn visit_i16x8_lane_select(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_ternary_op(offset)
    }
    fn visit_i32x4_lane_select(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_ternary_op(offset)
    }
    fn visit_i64x2_lane_select(&mut self, offset: usize) -> Self::Output {
        self.check_v128_relaxed_ternary_op(offset)
    }
    fn visit_v128_any_true(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i8x16_all_true(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i8x16_bitmask(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i16x8_all_true(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i16x8_bitmask(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i32x4_all_true(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i32x4_bitmask(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i64x2_all_true(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i64x2_bitmask(&mut self, offset: usize) -> Self::Output {
        self.check_v128_bitmask_op(offset)
    }
    fn visit_i8x16_shl(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i8x16_shr_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i8x16_shr_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i16x8_shl(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i16x8_shr_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i16x8_shr_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i32x4_shl(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i32x4_shr_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i32x4_shr_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i64x2_shl(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i64x2_shr_s(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i64x2_shr_u(&mut self, offset: usize) -> Self::Output {
        self.check_v128_shift_op(offset)
    }
    fn visit_i8x16_swizzle(&mut self, offset: usize) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i8x16_relaxed_swizzle(&mut self, offset: usize) -> Self::Output {
        self.check_relaxed_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_i8x16_shuffle(&mut self, offset: usize, lanes: [SIMDLaneIndex; 16]) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(ValType::V128))?;
        for i in lanes {
            self.check_simd_lane_index(offset, i, 32)?;
        }
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load8_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load16_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load32_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load32_zero(&mut self, input: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_v128_load32_splat(input, memarg)
    }
    fn visit_v128_load64_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(offset, memarg)
    }
    fn visit_v128_load64_zero(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(offset, memarg)
    }
    fn visit_v128_load8x8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(offset, memarg)
    }
    fn visit_v128_load8x8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(offset, memarg)
    }
    fn visit_v128_load16x4_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(offset, memarg)
    }
    fn visit_v128_load16x4_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(offset, memarg)
    }
    fn visit_v128_load32x2_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(offset, memarg)
    }
    fn visit_v128_load32x2_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_v128_load_op(offset, memarg)
    }
    fn visit_v128_load8_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 0, offset)?;
        self.check_simd_lane_index(offset, lane, 16)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load16_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 1, offset)?;
        self.check_simd_lane_index(offset, lane, 8)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load32_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 2, offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load64_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 3, offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(idx))?;
        self.push_operand(ValType::V128)?;
        Ok(())
    }
    fn visit_v128_store8_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 0, offset)?;
        self.check_simd_lane_index(offset, lane, 16)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(idx))?;
        Ok(())
    }
    fn visit_v128_store16_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 1, offset)?;
        self.check_simd_lane_index(offset, lane, 8)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(idx))?;
        Ok(())
    }
    fn visit_v128_store32_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 2, offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(idx))?;
        Ok(())
    }
    fn visit_v128_store64_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 3, offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.pop_operand(offset, Some(ValType::V128))?;
        self.pop_operand(offset, Some(idx))?;
        Ok(())
    }
    fn visit_memory_init(&mut self, offset: usize, segment: u32, mem: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        let ty = self.check_memory_index(offset, mem)?;
        match self.resources.data_count() {
            None => bail_op_err!(offset, "data count section required"),
            Some(count) if segment < count => {}
            Some(_) => bail_op_err!(offset, "unknown data segment {}", segment),
        }
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_data_drop(&mut self, offset: usize, segment: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        match self.resources.data_count() {
            None => bail_op_err!(offset, "data count section required"),
            Some(count) if segment < count => {}
            Some(_) => bail_op_err!(offset, "unknown data segment {}", segment),
        }
        Ok(())
    }
    fn visit_memory_copy(&mut self, offset: usize, dst: u32, src: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        let dst_ty = self.check_memory_index(offset, dst)?;
        let src_ty = self.check_memory_index(offset, src)?;

        // The length operand here is the smaller of src/dst, which is
        // i32 if one is i32
        self.pop_operand(
            offset,
            Some(match src_ty {
                ValType::I32 => ValType::I32,
                _ => dst_ty,
            }),
        )?;

        // ... and the offset into each memory is required to be
        // whatever the indexing type is for that memory
        self.pop_operand(offset, Some(src_ty))?;
        self.pop_operand(offset, Some(dst_ty))?;
        Ok(())
    }
    fn visit_memory_fill(&mut self, offset: usize, mem: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        let ty = self.check_memory_index(offset, mem)?;
        self.pop_operand(offset, Some(ty))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_table_init(&mut self, offset: usize, segment: u32, table: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        if table > 0 {
            self.check_reference_types_enabled(offset)?;
        }
        let table = match self.resources.table_at(table) {
            Some(table) => table,
            None => bail_op_err!(offset, "unknown table {}: table index out of bounds", table),
        };
        let segment_ty = match self.resources.element_type_at(segment) {
            Some(ty) => ty,
            None => bail_op_err!(
                offset,
                "unknown elem segment {}: segment index out of bounds",
                segment
            ),
        };
        if segment_ty != table.element_type {
            bail_op_err!(offset, "type mismatch");
        }
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        Ok(())
    }
    fn visit_elem_drop(&mut self, offset: usize, segment: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        if segment >= self.resources.element_count() {
            bail_op_err!(
                offset,
                "unknown elem segment {}: segment index out of bounds",
                segment
            );
        }
        Ok(())
    }
    fn visit_table_copy(&mut self, offset: usize, dst_table: u32, src_table: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        if src_table > 0 || dst_table > 0 {
            self.check_reference_types_enabled(offset)?;
        }
        let (src, dst) = match (
            self.resources.table_at(src_table),
            self.resources.table_at(dst_table),
        ) {
            (Some(a), Some(b)) => (a, b),
            _ => bail_op_err!(offset, "table index out of bounds"),
        };
        if src.element_type != dst.element_type {
            bail_op_err!(offset, "type mismatch");
        }
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        Ok(())
    }
    fn visit_table_get(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        let ty = match self.resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => bail_op_err!(offset, "table index out of bounds"),
        };
        self.pop_operand(offset, Some(ValType::I32))?;
        self.push_operand(ty)?;
        Ok(())
    }
    fn visit_table_set(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        let ty = match self.resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => bail_op_err!(offset, "table index out of bounds"),
        };
        self.pop_operand(offset, Some(ty))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        Ok(())
    }
    fn visit_table_grow(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        let ty = match self.resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => bail_op_err!(offset, "table index out of bounds"),
        };
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_table_size(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        if self.resources.table_at(table).is_none() {
            bail_op_err!(offset, "table index out of bounds");
        }
        self.push_operand(ValType::I32)?;
        Ok(())
    }
    fn visit_table_fill(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        let ty = match self.resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => bail_op_err!(offset, "table index out of bounds"),
        };
        self.pop_operand(offset, Some(ValType::I32))?;
        self.pop_operand(offset, Some(ty))?;
        self.pop_operand(offset, Some(ValType::I32))?;
        Ok(())
    }
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
