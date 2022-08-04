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
    ( $offset:expr, $( $arg:expr ),* $(,)* ) => {
        BinaryReaderError::new(format!( $( $arg ),* ), $offset)
    }
}

/// Early return an `Err(OperatorValidatorError)` with a format string.
macro_rules! bail_op_err {
    ( $offset:expr, $( $arg:expr ),* $(,)* ) => {
        return Err(format_op_err!( $offset, $( $arg ),* ))
    }
}

type OperatorValidatorResult<T> = std::result::Result<T, BinaryReaderError>;

/// The Wasm control list and type stack.
pub(crate) struct Stack {
    /// This is a list of flags for wasm features which are used to gate various
    /// instructions.
    features: WasmFeatures,
    /// The `control` list is the list of blocks that we're currently in.
    control: Vec<Frame>,
    /// The `operands` is the current type stack.
    operands: Vec<Option<ValType>>,
}

impl Stack {
    /// Creates a new [`Stack`] initialized with its root control frame.
    pub fn new(features: WasmFeatures, block_type: BlockType) -> Self {
        Self {
            features,
            control: vec![Frame {
                kind: FrameKind::Block,
                block_type,
                height: 0,
                unreachable: false,
            }],
            operands: Vec::new(),
        }
    }

    /// Returns the number of control frames in the control list.
    pub fn len_control(&self) -> usize {
        self.control.len()
    }

    /// Returns the number of operands in the operand stack.
    pub fn len_operands(&self) -> usize {
        self.operands.len()
    }

    /// Returns a shared reference to the last [`Frame`] if any.
    pub fn last_control(&self) -> Option<&Frame> {
        self.control.last()
    }

    /// Returns an exclusive reference to the last [`Frame`] if any.
    pub fn last_control_mut(&mut self) -> Option<&mut Frame> {
        self.control.last_mut()
    }

    /// Returns the control frame at the given `index` if any.
    pub fn control_at(&self, index: usize) -> Option<&Frame> {
        self.control.get(index)
    }

    /// Returns a shared reference to the first [`Frame`].
    ///
    /// # Panics
    ///
    /// If the control list is empty.
    pub fn first_control(&self) -> &Frame {
        self.control.first().unwrap()
    }

    /// Pushes a type onto the operand stack.
    ///
    /// This is used by instructions to represent a value that is pushed to the
    /// operand stack. This can fail, but only if `Type` is feature gated.
    /// Otherwise the push operation always succeeds.
    fn push_operand<T>(&mut self, offset: usize, ty: T) -> OperatorValidatorResult<()>
    where
        T: Into<Option<ValType>>,
    {
        let maybe_ty = ty.into();
        if let Some(ty) = maybe_ty {
            self.features
                .check_value_type(ty)
                .map_err(|error| format_op_err!(offset, "{error}"))?;
        }
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
    fn pop_operand(
        &mut self,
        offset: usize,
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

    /// Truncates the operand stack to the `new_height`.
    pub fn truncate_operands(&mut self, new_height: usize) {
        self.operands.truncate(new_height);
    }

    /// Pushes a new control frame to the control list.
    ///
    /// # Note
    ///
    /// The pushed control frame has a snapshot of the height of the current
    /// operand stack.
    pub fn push_control(&mut self, kind: FrameKind, block_type: BlockType, unreachable: bool) {
        self.control.push(Frame {
            kind,
            block_type,
            height: self.operands.len(),
            unreachable,
        });
    }

    /// Pops the last control frame from the control list.
    pub fn pop_control(&mut self) -> Option<Frame> {
        self.control.pop()
    }
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

pub(crate) struct OperatorValidator<T> {
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

    /// The operand and control stack.
    pub(crate) stack: Stack,

    /// Resources for Wasm validation.
    resources: T,
}

impl<T> OperatorValidator<T>
where
    T: WasmModuleResources,
{
    /// Creates a new operator validator which will be used to validate a
    /// function whose type is the `ty` index specified.
    ///
    /// The `resources` are used to learn about the function type underlying
    /// `ty`.
    pub fn new_func(ty: u32, offset: usize, features: &WasmFeatures, resources: T) -> Result<Self> {
        let locals = func_type_at(&resources, ty, offset)?
            .inputs()
            .enumerate()
            .map(|(i, ty)| (i as u32, ty))
            .collect::<Vec<_>>();
        Ok(OperatorValidator {
            num_locals: locals.len() as u32,
            locals,
            stack: Stack::new(*features, BlockType::FuncType(ty)),
            features: *features,
            br_table_tmp: Vec::new(),
            resources,
        })
    }

    /// Creates a new operator validator which will be used to validate an
    /// `init_expr` constant expression which should result in the `ty`
    /// specified.
    pub fn new_init_expr(features: &WasmFeatures, ty: ValType, resources: T) -> Self {
        OperatorValidator {
            num_locals: 0,
            locals: Vec::new(),
            stack: Stack::new(*features, BlockType::Type(ty)),
            features: *features,
            br_table_tmp: Vec::new(),
            resources,
        }
    }

    /// Returns a shared reference to the Wasm validation resources.
    pub fn resources(&self) -> &T {
        &self.resources
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
    fn local(&self, offset: usize, idx: u32) -> OperatorValidatorResult<ValType> {
        match self.locals.binary_search_by_key(&idx, |(idx, _)| *idx) {
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
        offset: usize,
        expected: Option<ValType>,
    ) -> OperatorValidatorResult<Option<ValType>> {
        self.stack.pop_operand(offset, expected)
    }

    /// Flags the current control frame as unreachable, additionally truncating
    /// the currently active operand stack.
    fn unreachable(&mut self) {
        let control = self.stack.last_control_mut().unwrap();
        control.unreachable = true;
        let new_height = control.height;
        self.stack.truncate_operands(new_height);
    }

    /// Pushes a new frame onto the control stack.
    ///
    /// This operation is used when entering a new block such as an if, loop,
    /// or block itself. The `kind` of block is specified which indicates how
    /// breaks interact with this block's type. Additionally the type signature
    /// of the block is specified by `ty`.
    fn push_ctrl(
        &mut self,
        offset: usize,
        kind: FrameKind,
        ty: BlockType,
    ) -> OperatorValidatorResult<()> {
        // Push a new frame which has a snapshot of the height of the current
        // operand stack.
        self.stack.push_control(kind, ty, false);
        // All of the parameters are now also available in this control frame,
        // so we push them here in order.
        for ty in params(offset, ty, &self.resources)? {
            self.stack.push_operand(offset, ty)?;
        }
        Ok(())
    }

    /// Pops a frame from the control stack.
    ///
    /// This function is used when exiting a block and leaves a block scope.
    /// Internally this will validate that blocks have the correct result type.
    fn pop_ctrl(&mut self, offset: usize) -> OperatorValidatorResult<Frame> {
        // Read the expected type and expected height of the operand stack the
        // end of the frame.
        let frame = self.stack.last_control().unwrap();
        let ty = frame.block_type;
        let height = frame.height;

        // Pop all the result types, in reverse order, from the operand stack.
        // These types will, possibly, be transferred to the next frame.
        for ty in results(offset, ty, &self.resources)?.rev() {
            self.stack.pop_operand(offset, Some(ty))?;
        }

        // Make sure that the operand stack has returned to is original
        // height...
        if self.stack.len_operands() != height {
            bail_op_err!(
                offset,
                "type mismatch: values remaining on stack at end of block"
            );
        }

        // And then we can remove it!
        Ok(self.stack.pop_control().unwrap())
    }

    /// Validates a relative jump to the `depth` specified.
    ///
    /// Returns the type signature of the block that we're jumping to as well
    /// as the kind of block if the jump is valid. Otherwise returns an error.
    fn jump(&self, offset: usize, depth: u32) -> OperatorValidatorResult<(BlockType, FrameKind)> {
        match (self.stack.len_control() - 1).checked_sub(depth as usize) {
            Some(i) => {
                let frame = self.stack.control_at(i).unwrap();
                Ok((frame.block_type, frame.kind))
            }
            None => bail_op_err!(offset, "unknown label: branch depth too large"),
        }
    }

    /// Validates that `memory_index` is valid in this module, and returns the
    /// type of address used to index the memory specified.
    fn check_memory_index(
        &self,
        offset: usize,
        memory_index: u32,
    ) -> OperatorValidatorResult<ValType> {
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
    ) -> OperatorValidatorResult<ValType> {
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

    #[cfg(feature = "deterministic")]
    fn check_non_deterministic_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        if !self.features.deterministic_only {
            bail_op_err!(offset, "deterministic_only support is not enabled");
        }
        Ok(())
    }

    #[inline(always)]
    #[cfg(not(feature = "deterministic"))]
    fn check_non_deterministic_enabled(&self) -> OperatorValidatorResult<()> {
        Ok(())
    }

    fn check_threads_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        if !self.features.threads {
            bail_op_err!(offset, "threads support is not enabled")
        }
        Ok(())
    }

    fn check_reference_types_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        if !self.features.reference_types {
            bail_op_err!(offset, "reference types support is not enabled")
        }
        Ok(())
    }

    /// Checks if Wasm proposal `saturating_float_to_int` is enabled.
    fn check_saturating_float_to_int_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        if !self.features.saturating_float_to_int {
            bail_op_err!(
                offset,
                "saturating float to int conversions support is not enabled"
            );
        }
        Ok(())
    }

    /// Checks if Wasm proposal `sign_extension` is enabled.
    fn check_sign_extension_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        if !self.features.sign_extension {
            bail_op_err!(offset, "sign extension operations support is not enabled");
        }
        Ok(())
    }

    fn check_simd_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        if !self.features.simd {
            bail_op_err!(offset, "SIMD support is not enabled");
        }
        Ok(())
    }

    fn check_relaxed_simd_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        // Relaxed SIMD operators make sense only with SIMD and be non-deterministic.
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled(offset)?;
        if !self.features.relaxed_simd {
            bail_op_err!(offset, "Relaxed SIMD support is not enabled");
        }
        Ok(())
    }

    fn check_exceptions_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        if !self.features.exceptions {
            bail_op_err!(offset, "Exceptions support is not enabled");
        }
        Ok(())
    }

    fn check_bulk_memory_enabled(&self, offset: usize) -> OperatorValidatorResult<()> {
        if !self.features.bulk_memory {
            bail_op_err!(offset, "bulk memory support is not enabled");
        }
        Ok(())
    }

    fn check_shared_memarg_wo_align(
        &self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> OperatorValidatorResult<ValType> {
        self.check_memory_index(offset, memarg.memory)
    }

    fn check_simd_lane_index(
        &self,
        offset: usize,
        index: SIMDLaneIndex,
        max: u8,
    ) -> OperatorValidatorResult<()> {
        if index >= max {
            bail_op_err!(offset, "SIMD index out of bounds");
        }
        Ok(())
    }

    /// Validates a block type, primarily with various in-flight proposals.
    fn check_block_type(&self, offset: usize, ty: BlockType) -> OperatorValidatorResult<()> {
        match ty {
            BlockType::Empty
            | BlockType::Type(ValType::I32)
            | BlockType::Type(ValType::I64)
            | BlockType::Type(ValType::F32)
            | BlockType::Type(ValType::F64) => Ok(()),
            BlockType::Type(ValType::ExternRef) | BlockType::Type(ValType::FuncRef) => {
                self.check_reference_types_enabled(offset)
            }
            BlockType::Type(ValType::V128) => self.check_simd_enabled(offset),
            BlockType::FuncType(idx) => {
                if !self.features.multi_value {
                    bail_op_err!(
                        offset,
                        "blocks, loops, and ifs may only produce a resulttype \
                         when multi-value is not enabled",
                    );
                }
                func_type_at(&self.resources, idx, offset)?;
                Ok(())
            }
        }
    }

    /// Validates a `call` instruction, ensuring that the function index is
    /// in-bounds and the right types are on the stack to call the function.
    fn check_call(&mut self, offset: usize, function_index: u32) -> OperatorValidatorResult<()> {
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
            self.stack.pop_operand(offset, Some(ty))?;
        }
        for ty in ty.outputs() {
            self.stack.push_operand(offset, ty)?;
        }
        Ok(())
    }

    /// Validates a call to an indirect function, very similar to `check_call`.
    fn check_call_indirect(
        &mut self,
        offset: usize,
        index: u32,
        table_index: u32,
    ) -> OperatorValidatorResult<()> {
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
        let ty = func_type_at(&self.resources, index, offset)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        for ty in ty.inputs().rev() {
            self.stack.pop_operand(offset, Some(ty))?;
        }
        for ty in ty.outputs() {
            self.stack.push_operand(offset, ty)?;
        }
        Ok(())
    }

    /// Validates a `return` instruction, popping types from the operand
    /// stack that the function needs.
    fn check_return(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        for ty in results(
            offset,
            self.stack.first_control().block_type,
            &self.resources,
        )?
        .rev()
        {
            self.stack.pop_operand(offset, Some(ty))?;
        }
        self.unreachable();
        Ok(())
    }

    /// Checks the validity of a common comparison operator.
    fn check_cmp_op(&mut self, offset: usize, ty: ValType) -> OperatorValidatorResult<()> {
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }

    /// Checks the validity of a common float comparison operator.
    fn check_fcmp_op(&mut self, offset: usize, ty: ValType) -> OperatorValidatorResult<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled()?;
        self.check_cmp_op(offset, ty)
    }

    /// Checks the validity of a common unary operator.
    fn check_unary_op(&mut self, offset: usize, ty: ValType) -> OperatorValidatorResult<()> {
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ty)?;
        Ok(())
    }

    /// Checks the validity of a common unary float operator.
    fn check_funary_op(&mut self, offset: usize, ty: ValType) -> OperatorValidatorResult<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled()?;
        self.check_unary_op(offset, ty)
    }

    /// Checks the validity of a common conversion operator.
    fn check_conversion_op(
        &mut self,
        offset: usize,
        into: ValType,
        from: ValType,
    ) -> OperatorValidatorResult<()> {
        self.stack.pop_operand(offset, Some(from))?;
        self.stack.push_operand(offset, into)?;
        Ok(())
    }

    /// Checks the validity of a common conversion operator.
    fn check_fconversion_op(
        &mut self,
        offset: usize,
        into: ValType,
        from: ValType,
    ) -> OperatorValidatorResult<()> {
        debug_assert!(matches!(into, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled()?;
        self.check_conversion_op(offset, into, from)
    }

    /// Checks the validity of a common binary operator.
    fn check_binary_op(&mut self, offset: usize, ty: ValType) -> OperatorValidatorResult<()> {
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ty)?;
        Ok(())
    }

    /// Checks the validity of a common binary float operator.
    fn check_fbinary_op(&mut self, offset: usize, ty: ValType) -> OperatorValidatorResult<()> {
        debug_assert!(matches!(ty, ValType::F32 | ValType::F64));
        self.check_non_deterministic_enabled()?;
        self.check_binary_op(offset, ty)
    }

    /// Checks the validity of an atomic load operator.
    fn check_atomic_load(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        load_ty: ValType,
    ) -> OperatorValidatorResult<()> {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, load_ty)?;
        Ok(())
    }

    /// Checks the validity of an atomic store operator.
    fn check_atomic_store(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        store_ty: ValType,
    ) -> OperatorValidatorResult<()> {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.stack.pop_operand(offset, Some(store_ty))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }

    /// Checks the validity of a common atomic binary operator.
    fn check_atomic_binary_op(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        op_ty: ValType,
    ) -> OperatorValidatorResult<()> {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.stack.pop_operand(offset, Some(op_ty))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, op_ty)?;
        Ok(())
    }

    /// Checks the validity of an atomic compare exchange operator.
    fn check_atomic_binary_cmpxchg(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        op_ty: ValType,
    ) -> OperatorValidatorResult<()> {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.stack.pop_operand(offset, Some(op_ty))?;
        self.stack.pop_operand(offset, Some(op_ty))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, op_ty)?;
        Ok(())
    }

    /// Checks a [`V128`] splat operator.
    fn check_v128_splat(&mut self, offset: usize, src_ty: ValType) -> OperatorValidatorResult<()> {
        self.check_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(src_ty))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_binary_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary float operator.
    fn check_v128_fbinary_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_non_deterministic_enabled()?;
        self.check_v128_binary_op(offset)
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_relaxed_binary_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_relaxed_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_unary_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_funary_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_non_deterministic_enabled()?;
        self.check_v128_unary_op(offset)
    }

    /// Checks a [`V128`] binary operator.
    fn check_v128_relaxed_unary_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_relaxed_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_relaxed_ternary_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_relaxed_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_bitmask_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }

    /// Checks a [`V128`] relaxed ternary operator.
    fn check_v128_shift_op(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        self.check_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }

    /// Checks a [`V128`] common load operator.
    fn check_v128_load_op(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> OperatorValidatorResult<()> {
        self.check_simd_enabled(offset)?;
        let idx = self.check_memarg(memarg, 3, offset)?;
        self.stack.pop_operand(offset, Some(idx))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }

    #[rustfmt::skip]
    pub fn process_operator(
        &mut self,
        operator: &Operator,
        offset: usize,
    ) -> OperatorValidatorResult<()> {
        match *operator {
            Operator::Unreachable => self.visit_unreachable(offset),
            Operator::Nop => self.visit_nop(offset),
            Operator::Block { ty } => self.visit_block(offset, ty),
            Operator::Loop { ty } => self.visit_loop(offset, ty),
            Operator::If { ty } => self.visit_if(offset, ty),
            Operator::Else => self.visit_else(offset),
            Operator::Try { ty } => self.visit_try(offset, ty),
            Operator::Catch { index } => self.visit_catch(offset, index),
            Operator::Throw { index } => self.visit_throw(offset, index),
            Operator::Rethrow { relative_depth } => self.visit_rethrow(offset, relative_depth),
            Operator::End => self.visit_end(offset),
            Operator::Br { relative_depth } => self.visit_br(offset, relative_depth),
            Operator::BrIf { relative_depth } => self.visit_br_if(offset, relative_depth),
            Operator::BrTable { ref table } => self.visit_br_table(offset, table),
            Operator::Return => self.visit_return(offset),
            Operator::Call { function_index } => self.visit_call(offset, function_index),
            Operator::CallIndirect { index, table_index, table_byte } => self.visit_call_indirect(offset, index, table_index, table_byte),
            Operator::ReturnCall { function_index } => self.visit_return_call(offset, function_index),
            Operator::ReturnCallIndirect { index, table_index } => self.visit_return_call_indirect(offset, index, table_index),
            Operator::Delegate { relative_depth } => self.visit_delegate(offset, relative_depth),
            Operator::CatchAll => self.visit_catch_all(offset),
            Operator::Drop => self.visit_drop(offset),
            Operator::Select => self.visit_select(offset),
            Operator::TypedSelect { ty } => self.visit_typed_select(offset, ty),
            Operator::LocalGet { local_index } => self.visit_local_get(offset, local_index),
            Operator::LocalSet { local_index } => self.visit_local_set(offset, local_index),
            Operator::LocalTee { local_index } => self.visit_local_tee(offset, local_index),
            Operator::GlobalGet { global_index } => self.visit_global_get(offset, global_index),
            Operator::GlobalSet { global_index } => self.visit_global_set(offset, global_index),
            Operator::I32Load { memarg } => self.visit_i32_load(offset, memarg),
            Operator::I64Load { memarg } => self.visit_i64_load(offset, memarg),
            Operator::F32Load { memarg } => self.visit_f32_load(offset, memarg),
            Operator::F64Load { memarg } => self.visit_f64_load(offset, memarg),
            Operator::I32Load8S { memarg } => self.visit_i32_load8_s(offset, memarg),
            Operator::I32Load8U { memarg } => self.visit_i32_load8_u(offset, memarg),
            Operator::I32Load16S { memarg } => self.visit_i32_load16_s(offset, memarg),
            Operator::I32Load16U { memarg } => self.visit_i32_load16_u(offset, memarg),
            Operator::I64Load8S { memarg } => self.visit_i64_load8_s(offset, memarg),
            Operator::I64Load8U { memarg } => self.visit_i64_load8_u(offset, memarg),
            Operator::I64Load16S { memarg } => self.visit_i64_load16_s(offset, memarg),
            Operator::I64Load16U { memarg } => self.visit_i64_load16_u(offset, memarg),
            Operator::I64Load32S { memarg } => self.visit_i64_load32_s(offset, memarg),
            Operator::I64Load32U { memarg } => self.visit_i64_load32_u(offset, memarg),
            Operator::I32Store { memarg } => self.visit_i32_store(offset, memarg),
            Operator::I64Store { memarg } => self.visit_i64_store(offset, memarg),
            Operator::F32Store { memarg } => self.visit_f32_store(offset, memarg),
            Operator::F64Store { memarg } => self.visit_f64_store(offset, memarg),
            Operator::I32Store8 { memarg } => self.visit_i32_store8(offset, memarg),
            Operator::I32Store16 { memarg } => self.visit_i32_store16(offset, memarg),
            Operator::I64Store8 { memarg } => self.visit_i64_store8(offset, memarg),
            Operator::I64Store16 { memarg } => self.visit_i64_store16(offset, memarg),
            Operator::I64Store32 { memarg } => self.visit_i64_store32(offset, memarg),
            Operator::MemorySize { mem, mem_byte } => self.visit_memory_size(offset, mem, mem_byte),
            Operator::MemoryGrow { mem, mem_byte } => self.visit_memory_grow(offset, mem, mem_byte),
            Operator::I32Const { value } => self.visit_i32_const(offset, value),
            Operator::I64Const { value } => self.visit_i64_const(offset, value),
            Operator::F32Const { value } => self.visit_f32_const(offset, value),
            Operator::F64Const { value } => self.visit_f64_const(offset, value),
            Operator::RefNull { ty } => self.visit_ref_null(offset, ty),
            Operator::RefIsNull => self.visit_ref_is_null(offset),
            Operator::RefFunc { function_index } => self.visit_ref_func(offset, function_index),
            Operator::I32Eqz => self.visit_i32_eqz(offset),
            Operator::I32Eq => self.visit_i32_eq(offset),
            Operator::I32Ne => self.visit_i32_ne(offset),
            Operator::I32LtS => self.visit_i32_lt_s(offset),
            Operator::I32LtU => self.visit_i32_lt_u(offset),
            Operator::I32GtS => self.visit_i32_gt_s(offset),
            Operator::I32GtU => self.visit_i32_gt_u(offset),
            Operator::I32LeS => self.visit_i32_le_s(offset),
            Operator::I32LeU => self.visit_i32_le_u(offset),
            Operator::I32GeS => self.visit_i32_ge_s(offset),
            Operator::I32GeU => self.visit_i32_ge_u(offset),
            Operator::I64Eqz => self.visit_i64_eqz(offset),
            Operator::I64Eq => self.visit_i64_eq(offset),
            Operator::I64Ne => self.visit_i64_ne(offset),
            Operator::I64LtS => self.visit_i64_lt_s(offset),
            Operator::I64LtU => self.visit_i64_lt_u(offset),
            Operator::I64GtS => self.visit_i64_gt_s(offset),
            Operator::I64GtU => self.visit_i64_gt_u(offset),
            Operator::I64LeS => self.visit_i64_le_s(offset),
            Operator::I64LeU => self.visit_i64_le_u(offset),
            Operator::I64GeS => self.visit_i64_ge_s(offset),
            Operator::I64GeU => self.visit_i64_ge_u(offset),
            Operator::F32Eq => self.visit_f32_eq(offset),
            Operator::F32Ne => self.visit_f32_ne(offset),
            Operator::F32Lt => self.visit_f32_lt(offset),
            Operator::F32Gt => self.visit_f32_gt(offset),
            Operator::F32Le => self.visit_f32_le(offset),
            Operator::F32Ge => self.visit_f32_ge(offset),
            Operator::F64Eq => self.visit_f64_eq(offset),
            Operator::F64Ne => self.visit_f64_ne(offset),
            Operator::F64Lt => self.visit_f64_lt(offset),
            Operator::F64Gt => self.visit_f64_gt(offset),
            Operator::F64Le => self.visit_f64_le(offset),
            Operator::F64Ge => self.visit_f64_ge(offset),
            Operator::I32Clz => self.visit_i32_clz(offset),
            Operator::I32Ctz => self.visit_i32_ctz(offset),
            Operator::I32Popcnt => self.visit_i32_popcnt(offset),
            Operator::I32Add => self.visit_i32_add(offset),
            Operator::I32Sub => self.visit_i32_sub(offset),
            Operator::I32Mul => self.visit_i32_mul(offset),
            Operator::I32DivS => self.visit_i32_div_s(offset),
            Operator::I32DivU => self.visit_i32_div_u(offset),
            Operator::I32RemS => self.visit_i32_rem_s(offset),
            Operator::I32RemU => self.visit_i32_rem_u(offset),
            Operator::I32And => self.visit_i32_and(offset),
            Operator::I32Or => self.visit_i32_or(offset),
            Operator::I32Xor => self.visit_i32_xor(offset),
            Operator::I32Shl => self.visit_i32_shl(offset),
            Operator::I32ShrS => self.visit_i32_shr_s(offset),
            Operator::I32ShrU => self.visit_i32_shr_u(offset),
            Operator::I32Rotl => self.visit_i32_rotl(offset),
            Operator::I32Rotr => self.visit_i32_rotr(offset),
            Operator::I64Clz => self.visit_i64_clz(offset),
            Operator::I64Ctz => self.visit_i64_ctz(offset),
            Operator::I64Popcnt => self.visit_i64_popcnt(offset),
            Operator::I64Add => self.visit_i64_add(offset),
            Operator::I64Sub => self.visit_i64_sub(offset),
            Operator::I64Mul => self.visit_i64_mul(offset),
            Operator::I64DivS => self.visit_i64_div_s(offset),
            Operator::I64DivU => self.visit_i64_div_u(offset),
            Operator::I64RemS => self.visit_i64_rem_s(offset),
            Operator::I64RemU => self.visit_i64_rem_u(offset),
            Operator::I64And => self.visit_i64_and(offset),
            Operator::I64Or => self.visit_i64_or(offset),
            Operator::I64Xor => self.visit_i64_xor(offset),
            Operator::I64Shl => self.visit_i64_shl(offset),
            Operator::I64ShrS => self.visit_i64_shr_s(offset),
            Operator::I64ShrU => self.visit_i64_shr_u(offset),
            Operator::I64Rotl => self.visit_i64_rotl(offset),
            Operator::I64Rotr => self.visit_i64_rotr(offset),
            Operator::F32Abs => self.visit_f32_abs(offset),
            Operator::F32Neg => self.visit_f32_neg(offset),
            Operator::F32Ceil => self.visit_f32_ceil(offset),
            Operator::F32Floor => self.visit_f32_floor(offset),
            Operator::F32Trunc => self.visit_f32_trunc(offset),
            Operator::F32Nearest => self.visit_f32_nearest(offset),
            Operator::F32Sqrt => self.visit_f32_sqrt(offset),
            Operator::F32Add => self.visit_f32_add(offset),
            Operator::F32Sub => self.visit_f32_sub(offset),
            Operator::F32Mul => self.visit_f32_mul(offset),
            Operator::F32Div => self.visit_f32_div(offset),
            Operator::F32Min => self.visit_f32_min(offset),
            Operator::F32Max => self.visit_f32_max(offset),
            Operator::F32Copysign => self.visit_f32_copysign(offset),
            Operator::F64Abs => self.visit_f64_abs(offset),
            Operator::F64Neg => self.visit_f64_neg(offset),
            Operator::F64Ceil => self.visit_f64_ceil(offset),
            Operator::F64Floor => self.visit_f64_floor(offset),
            Operator::F64Trunc => self.visit_f64_trunc(offset),
            Operator::F64Nearest => self.visit_f64_nearest(offset),
            Operator::F64Sqrt => self.visit_f64_sqrt(offset),
            Operator::F64Add => self.visit_f64_add(offset),
            Operator::F64Sub => self.visit_f64_sub(offset),
            Operator::F64Mul => self.visit_f64_mul(offset),
            Operator::F64Div => self.visit_f64_div(offset),
            Operator::F64Min => self.visit_f64_min(offset),
            Operator::F64Max => self.visit_f64_max(offset),
            Operator::F64Copysign => self.visit_f64_copysign(offset),
            Operator::I32WrapI64 => self.visit_i32_wrap_i64(offset),
            Operator::I32TruncF32S => self.visit_i32_trunc_f32s(offset),
            Operator::I32TruncF32U => self.visit_i32_trunc_f32u(offset),
            Operator::I32TruncF64S => self.visit_i32_trunc_f64s(offset),
            Operator::I32TruncF64U => self.visit_i32_trunc_f64u(offset),
            Operator::I64ExtendI32S => self.visit_i64_extend_i32s(offset),
            Operator::I64ExtendI32U => self.visit_i64_extend_i32u(offset),
            Operator::I64TruncF32S => self.visit_i64_trunc_f32s(offset),
            Operator::I64TruncF32U => self.visit_i64_trunc_f32u(offset),
            Operator::I64TruncF64S => self.visit_i64_trunc_f64s(offset),
            Operator::I64TruncF64U => self.visit_i64_trunc_f64u(offset),
            Operator::F32ConvertI32S => self.visit_f32_convert_i32s(offset),
            Operator::F32ConvertI32U => self.visit_f32_convert_i32u(offset),
            Operator::F32ConvertI64S => self.visit_f32_convert_i64s(offset),
            Operator::F32ConvertI64U => self.visit_f32_convert_i64u(offset),
            Operator::F32DemoteF64 => self.visit_f32_demote_f64(offset),
            Operator::F64ConvertI32S => self.visit_f64_convert_i32s(offset),
            Operator::F64ConvertI32U => self.visit_f64_convert_i32u(offset),
            Operator::F64ConvertI64S => self.visit_f64_convert_i64s(offset),
            Operator::F64ConvertI64U => self.visit_f64_convert_i64u(offset),
            Operator::F64PromoteF32 => self.visit_f64_promote_f32(offset),
            Operator::I32ReinterpretF32 => self.visit_i32_reinterpret_f32(offset),
            Operator::I64ReinterpretF64 => self.visit_i64_reinterpret_f64(offset),
            Operator::F32ReinterpretI32 => self.visit_f32_reinterpret_i32(offset),
            Operator::F64ReinterpretI64 => self.visit_f64_reinterpret_i64(offset),
            Operator::I32Extend8S => self.visit_i32_extend8_s(offset),
            Operator::I32Extend16S => self.visit_i32_extend16_s(offset),
            Operator::I64Extend8S => self.visit_i64_extend8_s(offset),
            Operator::I64Extend16S => self.visit_i64_extend16_s(offset),
            Operator::I64Extend32S => self.visit_i64_extend32_s(offset),
            Operator::I32TruncSatF32S => self.visit_i32_trunc_sat_f32s(offset),
            Operator::I32TruncSatF32U => self.visit_i32_trunc_sat_f32u(offset),
            Operator::I32TruncSatF64S => self.visit_i32_trunc_sat_f64s(offset),
            Operator::I32TruncSatF64U => self.visit_i32_trunc_sat_f64u(offset),
            Operator::I64TruncSatF32S => self.visit_i64_trunc_sat_f32s(offset),
            Operator::I64TruncSatF32U => self.visit_i64_trunc_sat_f32u(offset),
            Operator::I64TruncSatF64S => self.visit_i64_trunc_sat_f64s(offset),
            Operator::I64TruncSatF64U => self.visit_i64_trunc_sat_f64u(offset),
            Operator::MemoryInit { segment, mem } => self.visit_memory_init(offset, segment, mem),
            Operator::DataDrop { segment } => self.visit_data_drop(offset, segment),
            Operator::MemoryCopy { src, dst } => self.visit_memory_copy(offset, src, dst),
            Operator::MemoryFill { mem } => self.visit_memory_fill(offset, mem),
            Operator::TableInit { segment, table } => self.visit_table_init(offset, segment, table),
            Operator::ElemDrop { segment } => self.visit_elem_drop(offset, segment),
            Operator::TableCopy { dst_table, src_table } => self.visit_table_copy(offset, dst_table, src_table),
            Operator::TableFill { table } => self.visit_table_fill(offset, table),
            Operator::TableGet { table } => self.visit_table_get(offset, table),
            Operator::TableSet { table } => self.visit_table_set(offset, table),
            Operator::TableGrow { table } => self.visit_table_grow(offset, table),
            Operator::TableSize { table } => self.visit_table_size(offset, table),
            Operator::MemoryAtomicNotify { memarg } => self.visit_memory_atomic_notify(offset, memarg),
            Operator::MemoryAtomicWait32 { memarg } => self.visit_memory_atomic_wait32(offset, memarg),
            Operator::MemoryAtomicWait64 { memarg } => self.visit_memory_atomic_wait64(offset, memarg),
            Operator::AtomicFence { flags } => self.visit_atomic_fence(offset, flags),
            Operator::I32AtomicLoad { memarg } => self.visit_i32_atomic_load(offset, memarg),
            Operator::I64AtomicLoad { memarg } => self.visit_i64_atomic_load(offset, memarg),
            Operator::I32AtomicLoad8U { memarg } => self.visit_i32_atomic_load8_u(offset, memarg),
            Operator::I32AtomicLoad16U { memarg } => self.visit_i32_atomic_load16_u(offset, memarg),
            Operator::I64AtomicLoad8U { memarg } => self.visit_i64_atomic_load8_u(offset, memarg),
            Operator::I64AtomicLoad16U { memarg } => self.visit_i64_atomic_load16_u(offset, memarg),
            Operator::I64AtomicLoad32U { memarg } => self.visit_i64_atomic_load32_u(offset, memarg),
            Operator::I32AtomicStore { memarg } => self.visit_i32_atomic_store(offset, memarg),
            Operator::I64AtomicStore { memarg } => self.visit_i64_atomic_store(offset, memarg),
            Operator::I32AtomicStore8 { memarg } => self.visit_i32_atomic_store8(offset, memarg),
            Operator::I32AtomicStore16 { memarg } => self.visit_i32_atomic_store16(offset, memarg),
            Operator::I64AtomicStore8 { memarg } => self.visit_i64_atomic_store8(offset, memarg),
            Operator::I64AtomicStore16 { memarg } => self.visit_i64_atomic_store16(offset, memarg),
            Operator::I64AtomicStore32 { memarg } => self.visit_i64_atomic_store32(offset, memarg),
            Operator::I32AtomicRmwAdd { memarg } => self.visit_i32_atomic_rmw_add(offset, memarg),
            Operator::I64AtomicRmwAdd { memarg } => self.visit_i64_atomic_rmw_add(offset, memarg),
            Operator::I32AtomicRmw8AddU { memarg } => self.visit_i32_atomic_rmw8_add_u(offset, memarg),
            Operator::I32AtomicRmw16AddU { memarg } => self.visit_i32_atomic_rmw16_add_u(offset, memarg),
            Operator::I64AtomicRmw8AddU { memarg } => self.visit_i64_atomic_rmw8_add_u(offset, memarg),
            Operator::I64AtomicRmw16AddU { memarg } => self.visit_i64_atomic_rmw16_add_u(offset, memarg),
            Operator::I64AtomicRmw32AddU { memarg } => self.visit_i64_atomic_rmw32_add_u(offset, memarg),
            Operator::I32AtomicRmwSub { memarg } => self.visit_i32_atomic_rmw_sub(offset, memarg),
            Operator::I64AtomicRmwSub { memarg } => self.visit_i64_atomic_rmw_sub(offset, memarg),
            Operator::I32AtomicRmw8SubU { memarg } => self.visit_i32_atomic_rmw8_sub_u(offset, memarg),
            Operator::I32AtomicRmw16SubU { memarg } => self.visit_i32_atomic_rmw16_sub_u(offset, memarg),
            Operator::I64AtomicRmw8SubU { memarg } => self.visit_i64_atomic_rmw8_sub_u(offset, memarg),
            Operator::I64AtomicRmw16SubU { memarg } => self.visit_i64_atomic_rmw16_sub_u(offset, memarg),
            Operator::I64AtomicRmw32SubU { memarg } => self.visit_i64_atomic_rmw32_sub_u(offset, memarg),
            Operator::I32AtomicRmwAnd { memarg } => self.visit_i32_atomic_rmw_and(offset, memarg),
            Operator::I64AtomicRmwAnd { memarg } => self.visit_i64_atomic_rmw_and(offset, memarg),
            Operator::I32AtomicRmw8AndU { memarg } => self.visit_i32_atomic_rmw8_and_u(offset, memarg),
            Operator::I32AtomicRmw16AndU { memarg } => self.visit_i32_atomic_rmw16_and_u(offset, memarg),
            Operator::I64AtomicRmw8AndU { memarg } => self.visit_i64_atomic_rmw8_and_u(offset, memarg),
            Operator::I64AtomicRmw16AndU { memarg } => self.visit_i64_atomic_rmw16_and_u(offset, memarg),
            Operator::I64AtomicRmw32AndU { memarg } => self.visit_i64_atomic_rmw32_and_u(offset, memarg),
            Operator::I32AtomicRmwOr { memarg } => self.visit_i32_atomic_rmw_or(offset, memarg),
            Operator::I64AtomicRmwOr { memarg } => self.visit_i64_atomic_rmw_or(offset, memarg),
            Operator::I32AtomicRmw8OrU { memarg } => self.visit_i32_atomic_rmw8_or_u(offset, memarg),
            Operator::I32AtomicRmw16OrU { memarg } => self.visit_i32_atomic_rmw16_or_u(offset, memarg),
            Operator::I64AtomicRmw8OrU { memarg } => self.visit_i64_atomic_rmw8_or_u(offset, memarg),
            Operator::I64AtomicRmw16OrU { memarg } => self.visit_i64_atomic_rmw16_or_u(offset, memarg),
            Operator::I64AtomicRmw32OrU { memarg } => self.visit_i64_atomic_rmw32_or_u(offset, memarg),
            Operator::I32AtomicRmwXor { memarg } => self.visit_i32_atomic_rmw_xor(offset, memarg),
            Operator::I64AtomicRmwXor { memarg } => self.visit_i64_atomic_rmw_xor(offset, memarg),
            Operator::I32AtomicRmw8XorU { memarg } => self.visit_i32_atomic_rmw8_xor_u(offset, memarg),
            Operator::I32AtomicRmw16XorU { memarg } => self.visit_i32_atomic_rmw16_xor_u(offset, memarg),
            Operator::I64AtomicRmw8XorU { memarg } => self.visit_i64_atomic_rmw8_xor_u(offset, memarg),
            Operator::I64AtomicRmw16XorU { memarg } => self.visit_i64_atomic_rmw16_xor_u(offset, memarg),
            Operator::I64AtomicRmw32XorU { memarg } => self.visit_i64_atomic_rmw32_xor_u(offset, memarg),
            Operator::I32AtomicRmwXchg { memarg } => self.visit_i32_atomic_rmw_xchg(offset, memarg),
            Operator::I64AtomicRmwXchg { memarg } => self.visit_i64_atomic_rmw_xchg(offset, memarg),
            Operator::I32AtomicRmw8XchgU { memarg } => self.visit_i32_atomic_rmw8_xchg_u(offset, memarg),
            Operator::I32AtomicRmw16XchgU { memarg } => self.visit_i32_atomic_rmw16_xchg_u(offset, memarg),
            Operator::I64AtomicRmw8XchgU { memarg } => self.visit_i64_atomic_rmw8_xchg_u(offset, memarg),
            Operator::I64AtomicRmw16XchgU { memarg } => self.visit_i64_atomic_rmw16_xchg_u(offset, memarg),
            Operator::I64AtomicRmw32XchgU { memarg } => self.visit_i64_atomic_rmw32_xchg_u(offset, memarg),
            Operator::I32AtomicRmwCmpxchg { memarg } => self.visit_i32_atomic_rmw_cmpxchg(offset, memarg),
            Operator::I64AtomicRmwCmpxchg { memarg } => self.visit_i64_atomic_rmw_cmpxchg(offset, memarg),
            Operator::I32AtomicRmw8CmpxchgU { memarg } => self.visit_i32_atomic_rmw8_cmpxchg_u(offset, memarg),
            Operator::I32AtomicRmw16CmpxchgU { memarg } => self.visit_i32_atomic_rmw16_cmpxchg_u(offset, memarg),
            Operator::I64AtomicRmw8CmpxchgU { memarg } => self.visit_i64_atomic_rmw8_cmpxchg_u(offset, memarg),
            Operator::I64AtomicRmw16CmpxchgU { memarg } => self.visit_i64_atomic_rmw16_cmpxchg_u(offset, memarg),
            Operator::I64AtomicRmw32CmpxchgU { memarg } => self.visit_i64_atomic_rmw32_cmpxchg_u(offset, memarg),
            Operator::V128Load { memarg } => self.visit_v128_load(offset, memarg),
            Operator::V128Load8x8S { memarg } => self.visit_v128_load8x8_s(offset, memarg),
            Operator::V128Load8x8U { memarg } => self.visit_v128_load8x8_u(offset, memarg),
            Operator::V128Load16x4S { memarg } => self.visit_v128_load16x4_s(offset, memarg),
            Operator::V128Load16x4U { memarg } => self.visit_v128_load16x4_u(offset, memarg),
            Operator::V128Load32x2S { memarg } => self.visit_v128_load32x2_s(offset, memarg),
            Operator::V128Load32x2U { memarg } => self.visit_v128_load32x2_u(offset, memarg),
            Operator::V128Load8Splat { memarg } => self.visit_v128_load8_splat(offset, memarg),
            Operator::V128Load16Splat { memarg } => self.visit_v128_load16_splat(offset, memarg),
            Operator::V128Load32Splat { memarg } => self.visit_v128_load32_splat(offset, memarg),
            Operator::V128Load64Splat { memarg } => self.visit_v128_load64_splat(offset, memarg),
            Operator::V128Load32Zero { memarg } => self.visit_v128_load32_zero(offset, memarg),
            Operator::V128Load64Zero { memarg } => self.visit_v128_load64_zero(offset, memarg),
            Operator::V128Store { memarg } => self.visit_v128_store(offset, memarg),
            Operator::V128Load8Lane { memarg, lane } => self.visit_v128_load8_lane(offset, memarg, lane),
            Operator::V128Load16Lane { memarg, lane } => self.visit_v128_load16_lane(offset, memarg, lane),
            Operator::V128Load32Lane { memarg, lane } => self.visit_v128_load32_lane(offset, memarg, lane),
            Operator::V128Load64Lane { memarg, lane } => self.visit_v128_load64_lane(offset, memarg, lane),
            Operator::V128Store8Lane { memarg, lane } => self.visit_v128_store8_lane(offset, memarg, lane),
            Operator::V128Store16Lane { memarg, lane } => self.visit_v128_store16_lane(offset, memarg, lane),
            Operator::V128Store32Lane { memarg, lane } => self.visit_v128_store32_lane(offset, memarg, lane),
            Operator::V128Store64Lane { memarg, lane } => self.visit_v128_store64_lane(offset, memarg, lane),
            Operator::V128Const { value } => self.visit_v128_const(offset, value),
            Operator::I8x16Shuffle { lanes } => self.visit_i8x16_shuffle(offset, lanes),
            Operator::I8x16ExtractLaneS { lane } => self.visit_i8x16_extract_lane_s(offset, lane),
            Operator::I8x16ExtractLaneU { lane } => self.visit_i8x16_extract_lane_u(offset, lane),
            Operator::I8x16ReplaceLane { lane } => self.visit_i8x16_replace_lane(offset, lane),
            Operator::I16x8ExtractLaneS { lane } => self.visit_i16x8_extract_lane_s(offset, lane),
            Operator::I16x8ExtractLaneU { lane } => self.visit_i16x8_extract_lane_u(offset, lane),
            Operator::I16x8ReplaceLane { lane } => self.visit_i16x8_replace_lane(offset, lane),
            Operator::I32x4ExtractLane { lane } => self.visit_i32x4_extract_lane(offset, lane),
            Operator::I32x4ReplaceLane { lane } => self.visit_i32x4_replace_lane(offset, lane),
            Operator::I64x2ExtractLane { lane } => self.visit_i64x2_extract_lane(offset, lane),
            Operator::I64x2ReplaceLane { lane } => self.visit_i64x2_replace_lane(offset, lane),
            Operator::F32x4ExtractLane { lane } => self.visit_f32x4_extract_lane(offset, lane),
            Operator::F32x4ReplaceLane { lane } => self.visit_f32x4_replace_lane(offset, lane),
            Operator::F64x2ExtractLane { lane } => self.visit_f64x2_extract_lane(offset, lane),
            Operator::F64x2ReplaceLane { lane } => self.visit_f64x2_replace_lane(offset, lane),
            Operator::I8x16Swizzle => self.visit_i8x16_swizzle(offset),
            Operator::I8x16Splat => self.visit_i8x16_splat(offset),
            Operator::I16x8Splat => self.visit_i16x8_splat(offset),
            Operator::I32x4Splat => self.visit_i32x4_splat(offset),
            Operator::I64x2Splat => self.visit_i64x2_splat(offset),
            Operator::F32x4Splat => self.visit_f32x4_splat(offset),
            Operator::F64x2Splat => self.visit_f64x2_splat(offset),
            Operator::I8x16Eq => self.visit_i8x16_eq(offset),
            Operator::I8x16Ne => self.visit_i8x16_ne(offset),
            Operator::I8x16LtS => self.visit_i8x16_lt_s(offset),
            Operator::I8x16LtU => self.visit_i8x16_lt_u(offset),
            Operator::I8x16GtS => self.visit_i8x16_gt_s(offset),
            Operator::I8x16GtU => self.visit_i8x16_gt_u(offset),
            Operator::I8x16LeS => self.visit_i8x16_le_s(offset),
            Operator::I8x16LeU => self.visit_i8x16_le_u(offset),
            Operator::I8x16GeS => self.visit_i8x16_ge_s(offset),
            Operator::I8x16GeU => self.visit_i8x16_ge_u(offset),
            Operator::I16x8Eq => self.visit_i16x8_eq(offset),
            Operator::I16x8Ne => self.visit_i16x8_ne(offset),
            Operator::I16x8LtS => self.visit_i16x8_lt_s(offset),
            Operator::I16x8LtU => self.visit_i16x8_lt_u(offset),
            Operator::I16x8GtS => self.visit_i16x8_gt_s(offset),
            Operator::I16x8GtU => self.visit_i16x8_gt_u(offset),
            Operator::I16x8LeS => self.visit_i16x8_le_s(offset),
            Operator::I16x8LeU => self.visit_i16x8_le_u(offset),
            Operator::I16x8GeS => self.visit_i16x8_ge_s(offset),
            Operator::I16x8GeU => self.visit_i16x8_ge_u(offset),
            Operator::I32x4Eq => self.visit_i32x4_eq(offset),
            Operator::I32x4Ne => self.visit_i32x4_ne(offset),
            Operator::I32x4LtS => self.visit_i32x4_lt_s(offset),
            Operator::I32x4LtU => self.visit_i32x4_lt_u(offset),
            Operator::I32x4GtS => self.visit_i32x4_gt_s(offset),
            Operator::I32x4GtU => self.visit_i32x4_gt_u(offset),
            Operator::I32x4LeS => self.visit_i32x4_le_s(offset),
            Operator::I32x4LeU => self.visit_i32x4_le_u(offset),
            Operator::I32x4GeS => self.visit_i32x4_ge_s(offset),
            Operator::I32x4GeU => self.visit_i32x4_ge_u(offset),
            Operator::I64x2Eq => self.visit_i64x2_eq(offset),
            Operator::I64x2Ne => self.visit_i64x2_ne(offset),
            Operator::I64x2LtS => self.visit_i64x2_lt_s(offset),
            Operator::I64x2GtS => self.visit_i64x2_gt_s(offset),
            Operator::I64x2LeS => self.visit_i64x2_le_s(offset),
            Operator::I64x2GeS => self.visit_i64x2_ge_s(offset),
            Operator::F32x4Eq => self.visit_f32x4_eq(offset),
            Operator::F32x4Ne => self.visit_f32x4_ne(offset),
            Operator::F32x4Lt => self.visit_f32x4_lt(offset),
            Operator::F32x4Gt => self.visit_f32x4_gt(offset),
            Operator::F32x4Le => self.visit_f32x4_le(offset),
            Operator::F32x4Ge => self.visit_f32x4_ge(offset),
            Operator::F64x2Eq => self.visit_f64x2_eq(offset),
            Operator::F64x2Ne => self.visit_f64x2_ne(offset),
            Operator::F64x2Lt => self.visit_f64x2_lt(offset),
            Operator::F64x2Gt => self.visit_f64x2_gt(offset),
            Operator::F64x2Le => self.visit_f64x2_le(offset),
            Operator::F64x2Ge => self.visit_f64x2_ge(offset),
            Operator::V128Not => self.visit_v128_not(offset),
            Operator::V128And => self.visit_v128_and(offset),
            Operator::V128AndNot => self.visit_v128_and_not(offset),
            Operator::V128Or => self.visit_v128_or(offset),
            Operator::V128Xor => self.visit_v128_xor(offset),
            Operator::V128Bitselect => self.visit_v128_bitselect(offset),
            Operator::V128AnyTrue => self.visit_v128_any_true(offset),
            Operator::I8x16Abs => self.visit_i8x16_abs(offset),
            Operator::I8x16Neg => self.visit_i8x16_neg(offset),
            Operator::I8x16Popcnt => self.visit_i8x16_popcnt(offset),
            Operator::I8x16AllTrue => self.visit_i8x16_all_true(offset),
            Operator::I8x16Bitmask => self.visit_i8x16_bitmask(offset),
            Operator::I8x16NarrowI16x8S => self.visit_i8x16_narrow_i16x8_s(offset),
            Operator::I8x16NarrowI16x8U => self.visit_i8x16_narrow_i16x8_u(offset),
            Operator::I8x16Shl => self.visit_i8x16_shl(offset),
            Operator::I8x16ShrS => self.visit_i8x16_shr_s(offset),
            Operator::I8x16ShrU => self.visit_i8x16_shr_u(offset),
            Operator::I8x16Add => self.visit_i8x16_add(offset),
            Operator::I8x16AddSatS => self.visit_i8x16_add_sat_s(offset),
            Operator::I8x16AddSatU => self.visit_i8x16_add_sat_u(offset),
            Operator::I8x16Sub => self.visit_i8x16_sub(offset),
            Operator::I8x16SubSatS => self.visit_i8x16_sub_sat_s(offset),
            Operator::I8x16SubSatU => self.visit_i8x16_sub_sat_u(offset),
            Operator::I8x16MinS => self.visit_i8x16_min_s(offset),
            Operator::I8x16MinU => self.visit_i8x16_min_u(offset),
            Operator::I8x16MaxS => self.visit_i8x16_max_s(offset),
            Operator::I8x16MaxU => self.visit_i8x16_max_u(offset),
            Operator::I8x16RoundingAverageU => self.visit_i8x16_rounding_average_u(offset),
            Operator::I16x8ExtAddPairwiseI8x16S => self.visit_i16x8_ext_add_pairwise_i8x16_s(offset),
            Operator::I16x8ExtAddPairwiseI8x16U => self.visit_i16x8_ext_add_pairwise_i8x16_u(offset),
            Operator::I16x8Abs => self.visit_i16x8_abs(offset),
            Operator::I16x8Neg => self.visit_i16x8_neg(offset),
            Operator::I16x8Q15MulrSatS => self.visit_i16x8_q15_mulr_sat_s(offset),
            Operator::I16x8AllTrue => self.visit_i16x8_all_true(offset),
            Operator::I16x8Bitmask => self.visit_i16x8_bitmask(offset),
            Operator::I16x8NarrowI32x4S => self.visit_i16x8_narrow_i32x4_s(offset),
            Operator::I16x8NarrowI32x4U => self.visit_i16x8_narrow_i32x4_u(offset),
            Operator::I16x8ExtendLowI8x16S => self.visit_i16x8_extend_low_i8x16_s(offset),
            Operator::I16x8ExtendHighI8x16S => self.visit_i16x8_extend_high_i8x16_s(offset),
            Operator::I16x8ExtendLowI8x16U => self.visit_i16x8_extend_low_i8x16_u(offset),
            Operator::I16x8ExtendHighI8x16U => self.visit_i16x8_extend_high_i8x16_u(offset),
            Operator::I16x8Shl => self.visit_i16x8_shl(offset),
            Operator::I16x8ShrS => self.visit_i16x8_shr_s(offset),
            Operator::I16x8ShrU => self.visit_i16x8_shr_u(offset),
            Operator::I16x8Add => self.visit_i16x8_add(offset),
            Operator::I16x8AddSatS => self.visit_i16x8_add_sat_s(offset),
            Operator::I16x8AddSatU => self.visit_i16x8_add_sat_u(offset),
            Operator::I16x8Sub => self.visit_i16x8_sub(offset),
            Operator::I16x8SubSatS => self.visit_i16x8_sub_sat_s(offset),
            Operator::I16x8SubSatU => self.visit_i16x8_sub_sat_u(offset),
            Operator::I16x8Mul => self.visit_i16x8_mul(offset),
            Operator::I16x8MinS => self.visit_i16x8_min_s(offset),
            Operator::I16x8MinU => self.visit_i16x8_min_u(offset),
            Operator::I16x8MaxS => self.visit_i16x8_max_s(offset),
            Operator::I16x8MaxU => self.visit_i16x8_max_u(offset),
            Operator::I16x8RoundingAverageU => self.visit_i16x8_rounding_average_u(offset),
            Operator::I16x8ExtMulLowI8x16S => self.visit_i16x8_ext_mul_low_i8x16_s(offset),
            Operator::I16x8ExtMulHighI8x16S => self.visit_i16x8_ext_mul_high_i8x16_s(offset),
            Operator::I16x8ExtMulLowI8x16U => self.visit_i16x8_ext_mul_low_i8x16_u(offset),
            Operator::I16x8ExtMulHighI8x16U => self.visit_i16x8_ext_mul_high_i8x16_u(offset),
            Operator::I32x4ExtAddPairwiseI16x8S => self.visit_i32x4_ext_add_pairwise_i16x8_s(offset),
            Operator::I32x4ExtAddPairwiseI16x8U => self.visit_i32x4_ext_add_pairwise_i16x8_u(offset),
            Operator::I32x4Abs => self.visit_i32x4_abs(offset),
            Operator::I32x4Neg => self.visit_i32x4_neg(offset),
            Operator::I32x4AllTrue => self.visit_i32x4_all_true(offset),
            Operator::I32x4Bitmask => self.visit_i32x4_bitmask(offset),
            Operator::I32x4ExtendLowI16x8S => self.visit_i32x4_extend_low_i16x8_s(offset),
            Operator::I32x4ExtendHighI16x8S => self.visit_i32x4_extend_high_i16x8_s(offset),
            Operator::I32x4ExtendLowI16x8U => self.visit_i32x4_extend_low_i16x8_u(offset),
            Operator::I32x4ExtendHighI16x8U => self.visit_i32x4_extend_high_i16x8_u(offset),
            Operator::I32x4Shl => self.visit_i32x4_shl(offset),
            Operator::I32x4ShrS => self.visit_i32x4_shr_s(offset),
            Operator::I32x4ShrU => self.visit_i32x4_shr_u(offset),
            Operator::I32x4Add => self.visit_i32x4_add(offset),
            Operator::I32x4Sub => self.visit_i32x4_sub(offset),
            Operator::I32x4Mul => self.visit_i32x4_mul(offset),
            Operator::I32x4MinS => self.visit_i32x4_min_s(offset),
            Operator::I32x4MinU => self.visit_i32x4_min_u(offset),
            Operator::I32x4MaxS => self.visit_i32x4_max_s(offset),
            Operator::I32x4MaxU => self.visit_i32x4_max_u(offset),
            Operator::I32x4DotI16x8S => self.visit_i32x4_dot_i16x8_s(offset),
            Operator::I32x4ExtMulLowI16x8S => self.visit_i32x4_ext_mul_low_i16x8_s(offset),
            Operator::I32x4ExtMulHighI16x8S => self.visit_i32x4_ext_mul_high_i16x8_s(offset),
            Operator::I32x4ExtMulLowI16x8U => self.visit_i32x4_ext_mul_low_i16x8_u(offset),
            Operator::I32x4ExtMulHighI16x8U => self.visit_i32x4_ext_mul_high_i16x8_u(offset),
            Operator::I64x2Abs => self.visit_i64x2_abs(offset),
            Operator::I64x2Neg => self.visit_i64x2_neg(offset),
            Operator::I64x2AllTrue => self.visit_i64x2_all_true(offset),
            Operator::I64x2Bitmask => self.visit_i64x2_bitmask(offset),
            Operator::I64x2ExtendLowI32x4S => self.visit_i64x2_extend_low_i32x4_s(offset),
            Operator::I64x2ExtendHighI32x4S => self.visit_i64x2_extend_high_i32x4_s(offset),
            Operator::I64x2ExtendLowI32x4U => self.visit_i64x2_extend_low_i32x4_u(offset),
            Operator::I64x2ExtendHighI32x4U => self.visit_i64x2_extend_high_i32x4_u(offset),
            Operator::I64x2Shl => self.visit_i64x2_shl(offset),
            Operator::I64x2ShrS => self.visit_i64x2_shr_s(offset),
            Operator::I64x2ShrU => self.visit_i64x2_shr_u(offset),
            Operator::I64x2Add => self.visit_i64x2_add(offset),
            Operator::I64x2Sub => self.visit_i64x2_sub(offset),
            Operator::I64x2Mul => self.visit_i64x2_mul(offset),
            Operator::I64x2ExtMulLowI32x4S => self.visit_i64x2_ext_mul_low_i32x4_s(offset),
            Operator::I64x2ExtMulHighI32x4S => self.visit_i64x2_ext_mul_high_i32x4_s(offset),
            Operator::I64x2ExtMulLowI32x4U => self.visit_i64x2_ext_mul_low_i32x4_u(offset),
            Operator::I64x2ExtMulHighI32x4U => self.visit_i64x2_ext_mul_high_i32x4_u(offset),
            Operator::F32x4Ceil => self.visit_f32x4_ceil(offset),
            Operator::F32x4Floor => self.visit_f32x4_floor(offset),
            Operator::F32x4Trunc => self.visit_f32x4_trunc(offset),
            Operator::F32x4Nearest => self.visit_f32x4_nearest(offset),
            Operator::F32x4Abs => self.visit_f32x4_abs(offset),
            Operator::F32x4Neg => self.visit_f32x4_neg(offset),
            Operator::F32x4Sqrt => self.visit_f32x4_sqrt(offset),
            Operator::F32x4Add => self.visit_f32x4_add(offset),
            Operator::F32x4Sub => self.visit_f32x4_sub(offset),
            Operator::F32x4Mul => self.visit_f32x4_mul(offset),
            Operator::F32x4Div => self.visit_f32x4_div(offset),
            Operator::F32x4Min => self.visit_f32x4_min(offset),
            Operator::F32x4Max => self.visit_f32x4_max(offset),
            Operator::F32x4PMin => self.visit_f32x4_p_min(offset),
            Operator::F32x4PMax => self.visit_f32x4_p_max(offset),
            Operator::F64x2Ceil => self.visit_f64x2_ceil(offset),
            Operator::F64x2Floor => self.visit_f64x2_floor(offset),
            Operator::F64x2Trunc => self.visit_f64x2_trunc(offset),
            Operator::F64x2Nearest => self.visit_f64x2_nearest(offset),
            Operator::F64x2Abs => self.visit_f64x2_abs(offset),
            Operator::F64x2Neg => self.visit_f64x2_neg(offset),
            Operator::F64x2Sqrt => self.visit_f64x2_sqrt(offset),
            Operator::F64x2Add => self.visit_f64x2_add(offset),
            Operator::F64x2Sub => self.visit_f64x2_sub(offset),
            Operator::F64x2Mul => self.visit_f64x2_mul(offset),
            Operator::F64x2Div => self.visit_f64x2_div(offset),
            Operator::F64x2Min => self.visit_f64x2_min(offset),
            Operator::F64x2Max => self.visit_f64x2_max(offset),
            Operator::F64x2PMin => self.visit_f64x2_p_min(offset),
            Operator::F64x2PMax => self.visit_f64x2_p_max(offset),
            Operator::I32x4TruncSatF32x4S => self.visit_i32x4_trunc_sat_f32x4_s(offset),
            Operator::I32x4TruncSatF32x4U => self.visit_i32x4_trunc_sat_f32x4_u(offset),
            Operator::F32x4ConvertI32x4S => self.visit_f32x4_convert_i32x4_s(offset),
            Operator::F32x4ConvertI32x4U => self.visit_f32x4_convert_i32x4_u(offset),
            Operator::I32x4TruncSatF64x2SZero => self.visit_i32x4_trunc_sat_f64x2_s_zero(offset),
            Operator::I32x4TruncSatF64x2UZero => self.visit_i32x4_trunc_sat_f64x2_u_zero(offset),
            Operator::F64x2ConvertLowI32x4S => self.visit_f64x2_convert_low_i32x4_s(offset),
            Operator::F64x2ConvertLowI32x4U => self.visit_f64x2_convert_low_i32x4_u(offset),
            Operator::F32x4DemoteF64x2Zero => self.visit_f32x4_demote_f64x2_zero(offset),
            Operator::F64x2PromoteLowF32x4 => self.visit_f64x2_promote_low_f32x4(offset),
            Operator::I8x16RelaxedSwizzle => self.visit_i8x16_relaxed_swizzle(offset),
            Operator::I32x4RelaxedTruncSatF32x4S => self.visit_i32x4_relaxed_trunc_sat_f32x4_s(offset),
            Operator::I32x4RelaxedTruncSatF32x4U => self.visit_i32x4_relaxed_trunc_sat_f32x4_u(offset),
            Operator::I32x4RelaxedTruncSatF64x2SZero => self.visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(offset),
            Operator::I32x4RelaxedTruncSatF64x2UZero => self.visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(offset),
            Operator::F32x4Fma => self.visit_f32x4_fma(offset),
            Operator::F32x4Fms => self.visit_f32x4_fms(offset),
            Operator::F64x2Fma => self.visit_f64x2_fma(offset),
            Operator::F64x2Fms => self.visit_f64x2_fms(offset),
            Operator::I8x16LaneSelect => self.visit_i8x16_lane_select(offset),
            Operator::I16x8LaneSelect => self.visit_i16x8_lane_select(offset),
            Operator::I32x4LaneSelect => self.visit_i32x4_lane_select(offset),
            Operator::I64x2LaneSelect => self.visit_i64x2_lane_select(offset),
            Operator::F32x4RelaxedMin => self.visit_f32x4_relaxed_min(offset),
            Operator::F32x4RelaxedMax => self.visit_f32x4_relaxed_max(offset),
            Operator::F64x2RelaxedMin => self.visit_f64x2_relaxed_min(offset),
            Operator::F64x2RelaxedMax => self.visit_f64x2_relaxed_max(offset),
        }
    }

    pub fn finish(&mut self, offset: usize) -> OperatorValidatorResult<()> {
        if self.stack.last_control().is_some() {
            bail_op_err!(
                offset,
                "control frames remain at end of function: END opcode expected"
            );
        }
        Ok(())
    }
}

fn func_type_at<T: WasmModuleResources>(
    resources: &T,
    at: u32,
    offset: usize,
) -> OperatorValidatorResult<&T::FuncType> {
    resources
        .func_type_at(at)
        .ok_or_else(|| format_op_err!(offset, "unknown type: type index out of bounds"))
}

fn tag_at<T: WasmModuleResources>(
    resources: &T,
    at: u32,
    offset: usize,
) -> OperatorValidatorResult<&T::FuncType> {
    resources
        .tag_at(at)
        .ok_or_else(|| format_op_err!(offset, "unknown tag {}: tag index out of bounds", at))
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
    offset: usize,
    ty: BlockType,
    resources: &impl WasmModuleResources,
) -> OperatorValidatorResult<impl PreciseIterator<Item = ValType> + '_> {
    Ok(match ty {
        BlockType::Empty | BlockType::Type(_) => Either::B(None.into_iter()),
        BlockType::FuncType(t) => Either::A(func_type_at(resources, t, offset)?.inputs()),
    })
}

fn results(
    offset: usize,
    ty: BlockType,
    resources: &impl WasmModuleResources,
) -> OperatorValidatorResult<impl PreciseIterator<Item = ValType> + '_> {
    Ok(match ty {
        BlockType::Empty => Either::B(None.into_iter()),
        BlockType::Type(t) => Either::B(Some(t).into_iter()),
        BlockType::FuncType(t) => Either::A(func_type_at(resources, t, offset)?.outputs()),
    })
}

fn label_types(
    offset: usize,
    ty: BlockType,
    resources: &impl WasmModuleResources,
    kind: FrameKind,
) -> OperatorValidatorResult<impl PreciseIterator<Item = ValType> + '_> {
    Ok(match kind {
        FrameKind::Loop => Either::A(params(offset, ty, resources)?),
        _ => Either::B(results(offset, ty, resources)?),
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

impl<T> VisitOperator<usize> for OperatorValidator<T>
where
    T: WasmModuleResources,
{
    type Output = OperatorValidatorResult<()>;

    fn visit_nop(&mut self, _offset: usize) -> Self::Output {
        Ok(())
    }
    fn visit_unreachable(&mut self, _offset: usize) -> Self::Output {
        self.unreachable();
        Ok(())
    }
    fn visit_block(&mut self, offset: usize, ty: BlockType) -> Self::Output {
        self.check_block_type(offset, ty)?;
        for ty in params(offset, ty, &self.resources)?.rev() {
            self.stack.pop_operand(offset, Some(ty))?;
        }
        self.push_ctrl(offset, FrameKind::Block, ty)?;
        Ok(())
    }
    fn visit_loop(&mut self, offset: usize, ty: BlockType) -> Self::Output {
        self.check_block_type(offset, ty)?;
        for ty in params(offset, ty, &self.resources)?.rev() {
            self.stack.pop_operand(offset, Some(ty))?;
        }
        self.push_ctrl(offset, FrameKind::Loop, ty)?;
        Ok(())
    }
    fn visit_if(&mut self, offset: usize, ty: BlockType) -> Self::Output {
        self.check_block_type(offset, ty)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        for ty in params(offset, ty, &self.resources)?.rev() {
            self.stack.pop_operand(offset, Some(ty))?;
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
        for ty in params(offset, ty, &self.resources)?.rev() {
            self.stack.pop_operand(offset, Some(ty))?;
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
        self.stack
            .push_control(FrameKind::Catch, frame.block_type, false);
        // Push exception argument types.
        let ty = tag_at(&self.resources, index, offset)?;
        for ty in ty.inputs() {
            self.stack.push_operand(offset, ty)?;
        }
        Ok(())
    }
    fn visit_throw(&mut self, offset: usize, index: u32) -> Self::Output {
        self.check_exceptions_enabled(offset)?;
        // Check values associated with the exception.
        let ty = tag_at(&self.resources, index, offset)?;
        for ty in ty.inputs().rev() {
            self.stack.pop_operand(offset, Some(ty))?;
        }
        if ty.outputs().len() > 0 {
            bail_op_err!(offset, "result type expected to be empty for exception");
        }
        self.unreachable();
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
        self.unreachable();
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
        for ty in results(offset, frame.block_type, &self.resources)? {
            self.stack.push_operand(offset, ty)?;
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
        self.stack
            .push_control(FrameKind::CatchAll, frame.block_type, false);
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
        for ty in results(offset, frame.block_type, &self.resources)? {
            self.stack.push_operand(offset, ty)?;
        }
        Ok(())
    }
    fn visit_br(&mut self, offset: usize, relative_depth: u32) -> Self::Output {
        let (ty, kind) = self.jump(offset, relative_depth)?;
        for ty in label_types(offset, ty, &self.resources, kind)?.rev() {
            self.stack.pop_operand(offset, Some(ty))?;
        }
        self.unreachable();
        Ok(())
    }
    fn visit_br_if(&mut self, offset: usize, relative_depth: u32) -> Self::Output {
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        let (ty, kind) = self.jump(offset, relative_depth)?;
        for ty in label_types(offset, ty, &self.resources, kind)?.rev() {
            self.stack.pop_operand(offset, Some(ty))?;
        }
        for ty in label_types(offset, ty, &self.resources, kind)? {
            self.stack.push_operand(offset, ty)?;
        }
        Ok(())
    }
    fn visit_br_table(&mut self, offset: usize, table: &BrTable) -> Self::Output {
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        let default = self.jump(offset, table.default())?;
        let default_types = label_types(offset, default.0, &self.resources, default.1)?;
        for element in table.targets() {
            let relative_depth = element?;
            let block = self.jump(offset, relative_depth)?;
            let tys = label_types(offset, block.0, &self.resources, block.1)?;
            if tys.len() != default_types.len() {
                bail_op_err!(
                    offset,
                    "type mismatch: br_table target labels have different number of types"
                );
            }
            debug_assert!(self.br_table_tmp.is_empty());
            for ty in tys.rev() {
                let ty = self.stack.pop_operand(offset, Some(ty))?;
                self.br_table_tmp.push(ty);
            }
            for ty in self.br_table_tmp.drain(..).rev() {
                self.stack.push_operand(offset, ty)?;
            }
        }
        for ty in default_types.rev() {
            self.stack.pop_operand(offset, Some(ty))?;
        }
        self.unreachable();
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
        self.stack.pop_operand(offset, None)?;
        Ok(())
    }
    fn visit_select(&mut self, offset: usize) -> Self::Output {
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        let ty1 = self.stack.pop_operand(offset, None)?;
        let ty2 = self.stack.pop_operand(offset, None)?;
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
        self.stack.push_operand(offset, ty1.or(ty2));
        Ok(())
    }
    fn visit_typed_select(&mut self, offset: usize, ty: ValType) -> Self::Output {
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ty)?;
        Ok(())
    }
    fn visit_local_get(&mut self, offset: usize, local_index: u32) -> Self::Output {
        let ty = self.local(offset, local_index)?;
        self.stack.push_operand(offset, ty)?;
        Ok(())
    }
    fn visit_local_set(&mut self, offset: usize, local_index: u32) -> Self::Output {
        let ty = self.local(offset, local_index)?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_local_tee(&mut self, offset: usize, local_index: u32) -> Self::Output {
        let ty = self.local(offset, local_index)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ty)?;
        Ok(())
    }
    fn visit_global_get(&mut self, offset: usize, global_index: u32) -> Self::Output {
        if let Some(ty) = self.resources.global_at(global_index) {
            self.stack.push_operand(offset, ty.content_type)?;
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
            self.stack.pop_operand(offset, Some(ty.content_type))?;
        } else {
            bail_op_err!(offset, "unknown global: global index out of bounds");
        };
        Ok(())
    }
    fn visit_i32_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_i64_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 3, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I64)?;
        Ok(())
    }
    fn visit_f32_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::F32)?;
        Ok(())
    }
    fn visit_f64_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        let ty = self.check_memarg(memarg, 3, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::F64)?;
        Ok(())
    }
    fn visit_i32_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_i32_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i32_load8_s(offset, memarg)
    }
    fn visit_i32_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_i32_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i32_load16_s(offset, memarg)
    }
    fn visit_i64_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load8_s(offset, memarg)
    }
    fn visit_i64_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load16_s(offset, memarg)
    }
    fn visit_i64_load32_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I64)?;
        Ok(())
    }
    fn visit_i64_load32_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_i64_load32_s(offset, memarg)
    }
    fn visit_i32_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i64_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 3, offset)?;
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_f32_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.stack.pop_operand(offset, Some(ValType::F32))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_f64_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        let ty = self.check_memarg(memarg, 3, offset)?;
        self.stack.pop_operand(offset, Some(ValType::F64))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i32_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i32_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i64_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i64_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_i64_store32(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_memory_size(&mut self, offset: usize, mem: u32, mem_byte: u8) -> Self::Output {
        if mem_byte != 0 && !self.features.multi_memory {
            bail_op_err!(offset, "multi-memory not enabled: zero byte expected");
        }
        let index_ty = self.check_memory_index(offset, mem)?;
        self.stack.push_operand(offset, index_ty)?;
        Ok(())
    }
    fn visit_memory_grow(&mut self, offset: usize, mem: u32, mem_byte: u8) -> Self::Output {
        if mem_byte != 0 && !self.features.multi_memory {
            bail_op_err!(offset, "multi-memory not enabled: zero byte expected");
        }
        let index_ty = self.check_memory_index(offset, mem)?;
        self.stack.pop_operand(offset, Some(index_ty))?;
        self.stack.push_operand(offset, index_ty)?;
        Ok(())
    }
    fn visit_i32_const(&mut self, offset: usize, _value: i32) -> Self::Output {
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_i64_const(&mut self, offset: usize, _value: i64) -> Self::Output {
        self.stack.push_operand(offset, ValType::I64)?;
        Ok(())
    }
    fn visit_f32_const(&mut self, offset: usize, _value: Ieee32) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.stack.push_operand(offset, ValType::F32)?;
        Ok(())
    }
    fn visit_f64_const(&mut self, offset: usize, _value: Ieee64) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.stack.push_operand(offset, ValType::F64)?;
        Ok(())
    }
    fn visit_i32_eqz(&mut self, offset: usize) -> Self::Output {
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.push_operand(offset, ValType::I32)?;
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
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.push_operand(offset, ValType::I32)?;
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
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_memory_atomic_wait64(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output {
        self.check_threads_enabled(offset)?;
        let ty = self.check_shared_memarg_wo_align(offset, memarg)?;
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I32)?;
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
        match ty {
            ValType::FuncRef | ValType::ExternRef => {}
            _ => {
                bail_op_err!(offset, "invalid reference type in ref.null");
            }
        }
        self.stack.push_operand(offset, ty)?;
        Ok(())
    }
    fn visit_ref_is_null(&mut self, offset: usize) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        match self.stack.pop_operand(offset, None)? {
            None | Some(ValType::FuncRef) | Some(ValType::ExternRef) => {}
            _ => {
                bail_op_err!(
                    offset,
                    "type mismatch: invalid reference type in ref.is_null"
                );
            }
        }
        self.stack.push_operand(offset, ValType::I32)?;
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
        self.stack.push_operand(offset, ValType::FuncRef)?;
        Ok(())
    }
    fn visit_v128_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 4, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_v128_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 4, offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ty))?;
        Ok(())
    }
    fn visit_v128_const(&mut self, offset: usize, _value: V128) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.stack.push_operand(offset, ValType::V128)?;
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
        self.check_non_deterministic_enabled()?;
        self.check_v128_splat(offset, ValType::F32)
    }
    fn visit_f64x2_splat(&mut self, offset: usize) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_v128_splat(offset, ValType::F64)
    }
    fn visit_i8x16_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 16)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_i8x16_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.visit_i8x16_extract_lane_s(offset, lane)
    }
    fn visit_i16x8_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 8)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_i16x8_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.visit_i16x8_extract_lane_s(offset, lane)
    }
    fn visit_i32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_i8x16_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 16)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_i16x8_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 8)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_i32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_i64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::I64)?;
        Ok(())
    }
    fn visit_i64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.stack.pop_operand(offset, Some(ValType::I64))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_f32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::F32)?;
        Ok(())
    }
    fn visit_f32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 4)?;
        self.stack.pop_operand(offset, Some(ValType::F32))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_f64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::F64)?;
        Ok(())
    }
    fn visit_f64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output {
        self.check_non_deterministic_enabled()?;
        self.check_simd_enabled(offset)?;
        self.check_simd_lane_index(offset, lane, 2)?;
        self.stack.pop_operand(offset, Some(ValType::F64))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_i8x16_relaxed_swizzle(&mut self, offset: usize) -> Self::Output {
        self.check_relaxed_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_i8x16_shuffle(&mut self, offset: usize, lanes: [SIMDLaneIndex; 16]) -> Self::Output {
        self.check_simd_enabled(offset)?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        for i in lanes {
            self.check_simd_lane_index(offset, i, 32)?;
        }
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load8_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 0, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load16_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 1, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load32_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.check_simd_enabled(offset)?;
        let ty = self.check_memarg(memarg, 2, offset)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::V128)?;
        Ok(())
    }
    fn visit_v128_load32_zero(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output {
        self.visit_v128_load32_splat(offset, memarg)
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(idx))?;
        self.stack.push_operand(offset, ValType::V128)?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(idx))?;
        self.stack.push_operand(offset, ValType::V128)?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(idx))?;
        self.stack.push_operand(offset, ValType::V128)?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(idx))?;
        self.stack.push_operand(offset, ValType::V128)?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(idx))?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(idx))?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(idx))?;
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
        self.stack.pop_operand(offset, Some(ValType::V128))?;
        self.stack.pop_operand(offset, Some(idx))?;
        Ok(())
    }
    fn visit_memory_init(&mut self, offset: usize, mem: u32, segment: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        let ty = self.check_memory_index(offset, mem)?;
        match self.resources.data_count() {
            None => bail_op_err!(offset, "data count section required"),
            Some(count) if segment < count => {}
            Some(_) => bail_op_err!(offset, "unknown data segment {}", segment),
        }
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
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
    fn visit_memory_copy(&mut self, offset: usize, src: u32, dst: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        let dst_ty = self.check_memory_index(offset, dst)?;
        let src_ty = self.check_memory_index(offset, src)?;

        // The length operand here is the smaller of src/dst, which is
        // i32 if one is i32
        self.stack.pop_operand(
            offset,
            Some(match src_ty {
                ValType::I32 => ValType::I32,
                _ => dst_ty,
            }),
        )?;

        // ... and the offset into each memory is required to be
        // whatever the indexing type is for that memory
        self.stack.pop_operand(offset, Some(src_ty))?;
        self.stack.pop_operand(offset, Some(dst_ty))?;
        Ok(())
    }
    fn visit_memory_fill(&mut self, offset: usize, mem: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        let ty = self.check_memory_index(offset, mem)?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
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
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
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
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        Ok(())
    }
    fn visit_table_get(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        let ty = match self.resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => bail_op_err!(offset, "table index out of bounds"),
        };
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.push_operand(offset, ty)?;
        Ok(())
    }
    fn visit_table_set(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        let ty = match self.resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => bail_op_err!(offset, "table index out of bounds"),
        };
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        Ok(())
    }
    fn visit_table_grow(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        let ty = match self.resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => bail_op_err!(offset, "table index out of bounds"),
        };
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_table_size(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_reference_types_enabled(offset)?;
        if self.resources.table_at(table).is_none() {
            bail_op_err!(offset, "table index out of bounds");
        }
        self.stack.push_operand(offset, ValType::I32)?;
        Ok(())
    }
    fn visit_table_fill(&mut self, offset: usize, table: u32) -> Self::Output {
        self.check_bulk_memory_enabled(offset)?;
        let ty = match self.resources.table_at(table) {
            Some(ty) => ty.element_type,
            None => bail_op_err!(offset, "table index out of bounds"),
        };
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        self.stack.pop_operand(offset, Some(ty))?;
        self.stack.pop_operand(offset, Some(ValType::I32))?;
        Ok(())
    }
}
