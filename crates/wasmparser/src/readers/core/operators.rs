/* Copyright 2018 Mozilla Foundation
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

use crate::limits::{
    MAX_WASM_BR_TABLE_SIZE, MAX_WASM_CATCHES, MAX_WASM_HANDLERS, MAX_WASM_SELECT_RESULT_SIZE,
};
use crate::prelude::*;
use crate::{BinaryReader, BinaryReaderError, FromReader, RefType, Result, ValType};
use core::fmt;

/// Represents a block type.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BlockType {
    /// The block produces consumes nor produces any values.
    Empty,
    /// The block produces a singular value of the given type ([] -> \[t]).
    Type(ValType),
    /// The block is described by a function type.
    ///
    /// The index is to a function type in the types section.
    FuncType(u32),
}

/// The kind of a control flow `Frame`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FrameKind {
    /// A Wasm `block` control block.
    Block,
    /// A Wasm `if` control block.
    If,
    /// A Wasm `else` control block.
    Else,
    /// A Wasm `loop` control block.
    Loop,
    /// A Wasm `try` control block.
    ///
    /// # Note
    ///
    /// This belongs to the Wasm exception handling proposal.
    TryTable,
    /// A Wasm legacy `try` control block.
    ///
    /// # Note
    ///
    /// See: `WasmFeatures::legacy_exceptions` Note in `crates/wasmparser/src/features.rs`
    LegacyTry,
    /// A Wasm legacy `catch` control block.
    ///
    /// # Note
    ///
    /// See: `WasmFeatures::legacy_exceptions` Note in `crates/wasmparser/src/features.rs`
    LegacyCatch,
    /// A Wasm legacy `catch_all` control block.
    ///
    /// # Note
    ///
    /// See: `WasmFeatures::legacy_exceptions` Note in `crates/wasmparser/src/features.rs`
    LegacyCatchAll,
}

/// Represents a memory immediate in a WebAssembly memory instruction.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct MemArg {
    /// Alignment, stored as `n` where the actual alignment is `2^n`
    pub align: u8,
    /// Maximum alignment, stored as `n` where the actual alignment is `2^n`.
    ///
    /// Note that this field is not actually read from the binary format, it
    /// will be a constant depending on which instruction this `MemArg` is a
    /// payload for.
    pub max_align: u8,
    /// A fixed byte-offset that this memory immediate specifies.
    ///
    /// Note that the memory64 proposal can specify a full 64-bit byte offset
    /// while otherwise only 32-bit offsets are allowed. Once validated
    /// memory immediates for 32-bit memories are guaranteed to be at most
    /// `u32::MAX` whereas 64-bit memories can use the full 64-bits.
    pub offset: u64,
    /// The index of the memory this immediate points to.
    ///
    /// Note that this points within the module's own memory index space, and
    /// is always zero unless the multi-memory proposal of WebAssembly is
    /// enabled.
    pub memory: u32,
}

/// A br_table entries representation.
#[derive(Clone)]
pub struct BrTable<'a> {
    pub(crate) reader: crate::BinaryReader<'a>,
    pub(crate) cnt: u32,
    pub(crate) default: u32,
}

impl PartialEq<Self> for BrTable<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.cnt == other.cnt
            && self.default == other.default
            && self.reader.remaining_buffer() == other.reader.remaining_buffer()
    }
}

impl Eq for BrTable<'_> {}

impl<'a> BrTable<'a> {
    /// Returns the number of `br_table` entries, not including the default
    /// label
    pub fn len(&self) -> u32 {
        self.cnt
    }

    /// Returns whether `BrTable` doesn't have any labels apart from the default one.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the default target of this `br_table` instruction.
    pub fn default(&self) -> u32 {
        self.default
    }

    /// Returns the list of targets that this `br_table` instruction will be
    /// jumping to.
    ///
    /// This method will return an iterator which parses each target of this
    /// `br_table` except the default target. The returned iterator will
    /// yield `self.len()` elements.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use wasmparser::{BinaryReader, OperatorsReader, Operator};
    ///
    /// let buf = [0x0e, 0x02, 0x01, 0x02, 0x00];
    /// let mut reader = OperatorsReader::new(BinaryReader::new(&buf, 0));
    /// let op = reader.read().unwrap();
    /// if let Operator::BrTable { targets } = op {
    ///     let targets = targets.targets().collect::<Result<Vec<_>, _>>().unwrap();
    ///     assert_eq!(targets, [1, 2]);
    /// }
    /// ```
    pub fn targets(&self) -> BrTableTargets {
        BrTableTargets {
            reader: self.reader.clone(),
            remaining: self.cnt,
        }
    }
}

/// An iterator over the targets of a [`BrTable`].
///
/// # Note
///
/// This iterator parses each target of the underlying `br_table`
/// except for the default target.
/// The iterator will yield exactly as many targets as the `br_table` has.
pub struct BrTableTargets<'a> {
    reader: crate::BinaryReader<'a>,
    remaining: u32,
}

impl<'a> Iterator for BrTableTargets<'a> {
    type Item = Result<u32>;

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = usize::try_from(self.remaining).unwrap_or_else(|error| {
            panic!("could not convert remaining `u32` into `usize`: {error}")
        });
        (remaining, Some(remaining))
    }

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            if !self.reader.eof() {
                return Some(Err(BinaryReaderError::new(
                    "trailing data in br_table",
                    self.reader.original_position(),
                )));
            }
            return None;
        }
        self.remaining -= 1;
        Some(self.reader.read_var_u32())
    }
}

impl fmt::Debug for BrTable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("BrTable");
        f.field("count", &self.cnt);
        f.field("default", &self.default);
        match self.targets().collect::<Result<Vec<_>>>() {
            Ok(targets) => {
                f.field("targets", &targets);
            }
            Err(_) => {
                f.field("reader", &self.reader);
            }
        }
        f.finish()
    }
}

/// An IEEE binary32 immediate floating point value, represented as a u32
/// containing the bit pattern.
///
/// All bit patterns are allowed.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ieee32(pub(crate) u32);

impl Ieee32 {
    /// Gets the underlying bits of the 32-bit float.
    pub fn bits(self) -> u32 {
        self.0
    }
}

impl From<f32> for Ieee32 {
    fn from(value: f32) -> Self {
        Ieee32 {
            0: u32::from_le_bytes(value.to_le_bytes()),
        }
    }
}

impl From<Ieee32> for f32 {
    fn from(bits: Ieee32) -> f32 {
        f32::from_bits(bits.bits())
    }
}

/// An IEEE binary64 immediate floating point value, represented as a u64
/// containing the bit pattern.
///
/// All bit patterns are allowed.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ieee64(pub(crate) u64);

impl Ieee64 {
    /// Gets the underlying bits of the 64-bit float.
    pub fn bits(self) -> u64 {
        self.0
    }
}

impl From<f64> for Ieee64 {
    fn from(value: f64) -> Self {
        Ieee64 {
            0: u64::from_le_bytes(value.to_le_bytes()),
        }
    }
}

impl From<Ieee64> for f64 {
    fn from(bits: Ieee64) -> f64 {
        f64::from_bits(bits.bits())
    }
}

/// Represents a 128-bit vector value.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct V128(pub(crate) [u8; 16]);

impl V128 {
    /// Gets the bytes of the vector value.
    pub fn bytes(&self) -> &[u8; 16] {
        &self.0
    }

    /// Gets a signed 128-bit integer value from the vector's bytes.
    pub fn i128(&self) -> i128 {
        i128::from_le_bytes(self.0)
    }
}

impl From<V128> for i128 {
    fn from(bits: V128) -> i128 {
        bits.i128()
    }
}

impl From<V128> for u128 {
    fn from(bits: V128) -> u128 {
        u128::from_le_bytes(bits.0)
    }
}

/// Represents the memory ordering for atomic instructions.
///
/// For an in-depth explanation of memory orderings, see the C++ documentation
/// for [`memory_order`] or the Rust documentation for [`atomic::Ordering`].
///
/// [`memory_order`]: https://en.cppreference.com/w/cpp/atomic/memory_order
/// [`atomic::Ordering`]: https://doc.rust-lang.org/std/sync/atomic/enum.Ordering.html
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Ordering {
    /// For a load, it acquires; this orders all operations before the last
    /// "releasing" store. For a store, it releases; this orders all operations
    /// before it at the next "acquiring" load.
    AcqRel,
    /// Like `AcqRel` but all threads see all sequentially consistent operations
    /// in the same order.
    SeqCst,
}

macro_rules! define_operator {
    ($(@$proposal:ident $op:ident $({ $($payload:tt)* })? => $visit:ident ($($ann:tt)*))*) => {
        /// Instructions as defined [here].
        ///
        /// [here]: https://webassembly.github.io/spec/core/binary/instructions.html
        #[derive(Debug, Clone, Eq, PartialEq)]
        #[allow(missing_docs)]
        #[non_exhaustive]
        pub enum Operator<'a> {
            $(
                $op $({ $($payload)* })?,
            )*
        }
    }
}
crate::for_each_operator!(define_operator);

/// A reader for a core WebAssembly function's operators.
#[derive(Clone)]
pub struct OperatorsReader<'a> {
    reader: BinaryReader<'a>,
    blocks: Vec<FrameKind>,
}

impl<'a> OperatorsReader<'a> {
    /// Creates a new reader for an expression (instruction sequence)
    pub fn new(reader: BinaryReader<'a>) -> OperatorsReader<'a> {
        OperatorsReader {
            reader,
            blocks: vec![FrameKind::Block],
        }
    }

    /// Determines if the reader is at the end of the operators.
    pub fn eof(&self) -> bool {
        self.reader.eof()
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Function that must be called after the last opcode has been processed.
    ///
    /// This function returns an error if there is extra data after the operators.
    /// It does *not* check the binary format requirement that if the data count
    /// section is absent, a data index may not occur in the code section.
    pub fn finish(&self) -> Result<()> {
        self.ensure_stack_empty()?;
        if !self.eof() {
            bail!(
                self.original_position(),
                "unexpected data at the end of operators"
            );
        }
        Ok(())
    }

    fn ensure_stack_empty(&self) -> Result<()> {
        if !self.blocks.is_empty() {
            bail!(
                self.original_position(),
                "control frames remain at end of function body or expression"
            );
        }
        Ok(())
    }

    /// Reads the next available `Operator`.
    ///
    /// # Errors
    ///
    /// If `OperatorsReader` has less bytes remaining than required to parse
    /// the `Operator`.
    pub fn read(&mut self) -> Result<Operator<'a>> {
        self.visit_operator(&mut OperatorFactory::new())
    }

    /// Converts to an iterator of operators paired with offsets.
    pub fn into_iter_with_offsets(self) -> OperatorsIteratorWithOffsets<'a> {
        OperatorsIteratorWithOffsets {
            reader: self,
            err: false,
        }
    }

    /// Reads an operator with its offset.
    pub fn read_with_offset(&mut self) -> Result<(Operator<'a>, usize)> {
        let pos = self.reader.original_position();
        Ok((self.read()?, pos))
    }

    fn enter(&mut self, k: FrameKind) {
        self.blocks.push(k)
    }

    fn expect_block(&mut self, k: FrameKind, found: &str) -> Result<()> {
        match self.blocks.last() {
            None => bail!(
                self.original_position(),
                "empty stack found where {:?} expected",
                k
            ),
            Some(x) if *x == k => Ok(()),
            Some(_) => bail!(
                self.original_position(),
                "`{}` found outside `{:?}` block",
                found,
                k
            ),
        }
    }

    fn end(&mut self) -> Result<()> {
        assert!(!self.blocks.is_empty());
        self.blocks.pop();
        Ok(())
    }

    /// Visit the next available operator with the specified [`VisitOperator`] instance.
    ///
    /// Note that this does not implicitly propagate any additional information such as instruction
    /// offsets. In order to do so, consider storing such data within the visitor before visiting.
    ///
    /// # Errors
    ///
    /// If `OperatorsReader` has less bytes remaining than required to parse the `Operator`.
    ///
    /// # Examples
    ///
    /// Store an offset for use in diagnostics or any other purposes:
    ///
    /// ```
    /// # use wasmparser::{OperatorsReader, VisitOperator, Result, for_each_visit_operator};
    ///
    /// pub fn dump(mut reader: OperatorsReader) -> Result<()> {
    ///     let mut visitor = Dumper { offset: 0 };
    ///     while !reader.eof() {
    ///         visitor.offset = reader.original_position();
    ///         reader.visit_operator(&mut visitor)?;
    ///     }
    ///     Ok(())
    /// }
    ///
    /// struct Dumper {
    ///     offset: usize
    /// }
    ///
    /// macro_rules! define_visit_operator {
    ///     ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($ann:tt)*))*) => {
    ///         $(
    ///             fn $visit(&mut self $($(,$arg: $argty)*)?) -> Self::Output {
    ///                 println!("{}: {}", self.offset, stringify!($visit));
    ///             }
    ///         )*
    ///     }
    /// }
    ///
    /// impl<'a> VisitOperator<'a> for Dumper {
    ///     type Output = ();
    ///     for_each_visit_operator!(define_visit_operator);
    /// }
    ///
    /// ```
    pub fn visit_operator<T>(&mut self, visitor: &mut T) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        if self.blocks.is_empty() {
            bail!(
                self.original_position(),
                "operators remaining after end of function body or expression"
            );
        }
        let pos = self.reader.original_position();
        let code = self.reader.read_u8()?;
        Ok(match code {
            0x00 => visitor.visit_unreachable(),
            0x01 => visitor.visit_nop(),
            0x02 => {
                self.enter(FrameKind::Block);
                visitor.visit_block(self.reader.read_block_type()?)
            }
            0x03 => {
                self.enter(FrameKind::Loop);
                visitor.visit_loop(self.reader.read_block_type()?)
            }
            0x04 => {
                self.enter(FrameKind::If);
                visitor.visit_if(self.reader.read_block_type()?)
            }
            0x05 => {
                self.expect_block(FrameKind::If, "else")?;
                visitor.visit_else()
            }
            0x06 => {
                if !self.reader.legacy_exceptions() {
                    bail!(
                        pos,
                        "legacy_exceptions feature required for try instruction"
                    );
                }
                self.enter(FrameKind::LegacyTry);
                visitor.visit_try(self.reader.read_block_type()?)
            }
            0x07 => {
                if !self.reader.legacy_exceptions() {
                    bail!(
                        pos,
                        "legacy_exceptions feature required for catch instruction"
                    );
                }
                self.expect_block(FrameKind::LegacyTry, "catch")?;
                visitor.visit_catch(self.reader.read_var_u32()?)
            }
            0x08 => visitor.visit_throw(self.reader.read_var_u32()?),
            0x09 => visitor.visit_rethrow(self.reader.read_var_u32()?),
            0x0a => visitor.visit_throw_ref(),
            0x0b => {
                self.end()?;
                visitor.visit_end()
            }
            0x0c => visitor.visit_br(self.reader.read_var_u32()?),
            0x0d => visitor.visit_br_if(self.reader.read_var_u32()?),
            0x0e => visitor.visit_br_table(self.read_br_table()?),
            0x0f => visitor.visit_return(),
            0x10 => visitor.visit_call(self.reader.read_var_u32()?),
            0x11 => {
                let index = self.reader.read_var_u32()?;
                let table = self.read_call_indirect_table_immediate()?;
                visitor.visit_call_indirect(index, table)
            }
            0x12 => visitor.visit_return_call(self.reader.read_var_u32()?),
            0x13 => visitor.visit_return_call_indirect(
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x14 => visitor.visit_call_ref(self.reader.read()?),
            0x15 => visitor.visit_return_call_ref(self.reader.read()?),
            0x18 => {
                self.expect_block(FrameKind::LegacyTry, "delegate")?;
                self.blocks.pop();
                visitor.visit_delegate(self.reader.read_var_u32()?)
            }
            0x19 => {
                if !self.reader.legacy_exceptions() {
                    bail!(
                        pos,
                        "legacy_exceptions feature required for catch_all instruction"
                    );
                }
                self.expect_block(FrameKind::LegacyTry, "catch_all")?;
                visitor.visit_catch_all()
            }
            0x1a => visitor.visit_drop(),
            0x1b => visitor.visit_select(),
            0x1c => {
                let result_count = self
                    .reader
                    .read_size(MAX_WASM_SELECT_RESULT_SIZE, "select types")?;
                if result_count == 1 {
                    visitor.visit_typed_select(self.reader.read()?)
                } else {
                    let mut results = Vec::new();
                    results.reserve_exact(result_count);
                    for _ in 0..result_count {
                        results.push(self.reader.read()?);
                    }
                    visitor.visit_typed_select_multi(results)
                }
            }
            0x1f => {
                self.enter(FrameKind::TryTable);
                visitor.visit_try_table(self.reader.read()?)
            }

            0x20 => visitor.visit_local_get(self.reader.read_var_u32()?),
            0x21 => visitor.visit_local_set(self.reader.read_var_u32()?),
            0x22 => visitor.visit_local_tee(self.reader.read_var_u32()?),
            0x23 => visitor.visit_global_get(self.reader.read_var_u32()?),
            0x24 => visitor.visit_global_set(self.reader.read_var_u32()?),
            0x25 => visitor.visit_table_get(self.reader.read_var_u32()?),
            0x26 => visitor.visit_table_set(self.reader.read_var_u32()?),

            0x28 => visitor.visit_i32_load(self.read_memarg(2)?),
            0x29 => visitor.visit_i64_load(self.read_memarg(3)?),
            0x2a => visitor.visit_f32_load(self.read_memarg(2)?),
            0x2b => visitor.visit_f64_load(self.read_memarg(3)?),
            0x2c => visitor.visit_i32_load8_s(self.read_memarg(0)?),
            0x2d => visitor.visit_i32_load8_u(self.read_memarg(0)?),
            0x2e => visitor.visit_i32_load16_s(self.read_memarg(1)?),
            0x2f => visitor.visit_i32_load16_u(self.read_memarg(1)?),
            0x30 => visitor.visit_i64_load8_s(self.read_memarg(0)?),
            0x31 => visitor.visit_i64_load8_u(self.read_memarg(0)?),
            0x32 => visitor.visit_i64_load16_s(self.read_memarg(1)?),
            0x33 => visitor.visit_i64_load16_u(self.read_memarg(1)?),
            0x34 => visitor.visit_i64_load32_s(self.read_memarg(2)?),
            0x35 => visitor.visit_i64_load32_u(self.read_memarg(2)?),
            0x36 => visitor.visit_i32_store(self.read_memarg(2)?),
            0x37 => visitor.visit_i64_store(self.read_memarg(3)?),
            0x38 => visitor.visit_f32_store(self.read_memarg(2)?),
            0x39 => visitor.visit_f64_store(self.read_memarg(3)?),
            0x3a => visitor.visit_i32_store8(self.read_memarg(0)?),
            0x3b => visitor.visit_i32_store16(self.read_memarg(1)?),
            0x3c => visitor.visit_i64_store8(self.read_memarg(0)?),
            0x3d => visitor.visit_i64_store16(self.read_memarg(1)?),
            0x3e => visitor.visit_i64_store32(self.read_memarg(2)?),
            0x3f => {
                let mem = self.read_memory_index_or_zero_if_not_multi_memory()?;
                visitor.visit_memory_size(mem)
            }
            0x40 => {
                let mem = self.read_memory_index_or_zero_if_not_multi_memory()?;
                visitor.visit_memory_grow(mem)
            }

            0x41 => visitor.visit_i32_const(self.reader.read_var_i32()?),
            0x42 => visitor.visit_i64_const(self.reader.read_var_i64()?),
            0x43 => visitor.visit_f32_const(self.reader.read_f32()?),
            0x44 => visitor.visit_f64_const(self.reader.read_f64()?),

            0x45 => visitor.visit_i32_eqz(),
            0x46 => visitor.visit_i32_eq(),
            0x47 => visitor.visit_i32_ne(),
            0x48 => visitor.visit_i32_lt_s(),
            0x49 => visitor.visit_i32_lt_u(),
            0x4a => visitor.visit_i32_gt_s(),
            0x4b => visitor.visit_i32_gt_u(),
            0x4c => visitor.visit_i32_le_s(),
            0x4d => visitor.visit_i32_le_u(),
            0x4e => visitor.visit_i32_ge_s(),
            0x4f => visitor.visit_i32_ge_u(),
            0x50 => visitor.visit_i64_eqz(),
            0x51 => visitor.visit_i64_eq(),
            0x52 => visitor.visit_i64_ne(),
            0x53 => visitor.visit_i64_lt_s(),
            0x54 => visitor.visit_i64_lt_u(),
            0x55 => visitor.visit_i64_gt_s(),
            0x56 => visitor.visit_i64_gt_u(),
            0x57 => visitor.visit_i64_le_s(),
            0x58 => visitor.visit_i64_le_u(),
            0x59 => visitor.visit_i64_ge_s(),
            0x5a => visitor.visit_i64_ge_u(),
            0x5b => visitor.visit_f32_eq(),
            0x5c => visitor.visit_f32_ne(),
            0x5d => visitor.visit_f32_lt(),
            0x5e => visitor.visit_f32_gt(),
            0x5f => visitor.visit_f32_le(),
            0x60 => visitor.visit_f32_ge(),
            0x61 => visitor.visit_f64_eq(),
            0x62 => visitor.visit_f64_ne(),
            0x63 => visitor.visit_f64_lt(),
            0x64 => visitor.visit_f64_gt(),
            0x65 => visitor.visit_f64_le(),
            0x66 => visitor.visit_f64_ge(),
            0x67 => visitor.visit_i32_clz(),
            0x68 => visitor.visit_i32_ctz(),
            0x69 => visitor.visit_i32_popcnt(),
            0x6a => visitor.visit_i32_add(),
            0x6b => visitor.visit_i32_sub(),
            0x6c => visitor.visit_i32_mul(),
            0x6d => visitor.visit_i32_div_s(),
            0x6e => visitor.visit_i32_div_u(),
            0x6f => visitor.visit_i32_rem_s(),
            0x70 => visitor.visit_i32_rem_u(),
            0x71 => visitor.visit_i32_and(),
            0x72 => visitor.visit_i32_or(),
            0x73 => visitor.visit_i32_xor(),
            0x74 => visitor.visit_i32_shl(),
            0x75 => visitor.visit_i32_shr_s(),
            0x76 => visitor.visit_i32_shr_u(),
            0x77 => visitor.visit_i32_rotl(),
            0x78 => visitor.visit_i32_rotr(),
            0x79 => visitor.visit_i64_clz(),
            0x7a => visitor.visit_i64_ctz(),
            0x7b => visitor.visit_i64_popcnt(),
            0x7c => visitor.visit_i64_add(),
            0x7d => visitor.visit_i64_sub(),
            0x7e => visitor.visit_i64_mul(),
            0x7f => visitor.visit_i64_div_s(),
            0x80 => visitor.visit_i64_div_u(),
            0x81 => visitor.visit_i64_rem_s(),
            0x82 => visitor.visit_i64_rem_u(),
            0x83 => visitor.visit_i64_and(),
            0x84 => visitor.visit_i64_or(),
            0x85 => visitor.visit_i64_xor(),
            0x86 => visitor.visit_i64_shl(),
            0x87 => visitor.visit_i64_shr_s(),
            0x88 => visitor.visit_i64_shr_u(),
            0x89 => visitor.visit_i64_rotl(),
            0x8a => visitor.visit_i64_rotr(),
            0x8b => visitor.visit_f32_abs(),
            0x8c => visitor.visit_f32_neg(),
            0x8d => visitor.visit_f32_ceil(),
            0x8e => visitor.visit_f32_floor(),
            0x8f => visitor.visit_f32_trunc(),
            0x90 => visitor.visit_f32_nearest(),
            0x91 => visitor.visit_f32_sqrt(),
            0x92 => visitor.visit_f32_add(),
            0x93 => visitor.visit_f32_sub(),
            0x94 => visitor.visit_f32_mul(),
            0x95 => visitor.visit_f32_div(),
            0x96 => visitor.visit_f32_min(),
            0x97 => visitor.visit_f32_max(),
            0x98 => visitor.visit_f32_copysign(),
            0x99 => visitor.visit_f64_abs(),
            0x9a => visitor.visit_f64_neg(),
            0x9b => visitor.visit_f64_ceil(),
            0x9c => visitor.visit_f64_floor(),
            0x9d => visitor.visit_f64_trunc(),
            0x9e => visitor.visit_f64_nearest(),
            0x9f => visitor.visit_f64_sqrt(),
            0xa0 => visitor.visit_f64_add(),
            0xa1 => visitor.visit_f64_sub(),
            0xa2 => visitor.visit_f64_mul(),
            0xa3 => visitor.visit_f64_div(),
            0xa4 => visitor.visit_f64_min(),
            0xa5 => visitor.visit_f64_max(),
            0xa6 => visitor.visit_f64_copysign(),
            0xa7 => visitor.visit_i32_wrap_i64(),
            0xa8 => visitor.visit_i32_trunc_f32_s(),
            0xa9 => visitor.visit_i32_trunc_f32_u(),
            0xaa => visitor.visit_i32_trunc_f64_s(),
            0xab => visitor.visit_i32_trunc_f64_u(),
            0xac => visitor.visit_i64_extend_i32_s(),
            0xad => visitor.visit_i64_extend_i32_u(),
            0xae => visitor.visit_i64_trunc_f32_s(),
            0xaf => visitor.visit_i64_trunc_f32_u(),
            0xb0 => visitor.visit_i64_trunc_f64_s(),
            0xb1 => visitor.visit_i64_trunc_f64_u(),
            0xb2 => visitor.visit_f32_convert_i32_s(),
            0xb3 => visitor.visit_f32_convert_i32_u(),
            0xb4 => visitor.visit_f32_convert_i64_s(),
            0xb5 => visitor.visit_f32_convert_i64_u(),
            0xb6 => visitor.visit_f32_demote_f64(),
            0xb7 => visitor.visit_f64_convert_i32_s(),
            0xb8 => visitor.visit_f64_convert_i32_u(),
            0xb9 => visitor.visit_f64_convert_i64_s(),
            0xba => visitor.visit_f64_convert_i64_u(),
            0xbb => visitor.visit_f64_promote_f32(),
            0xbc => visitor.visit_i32_reinterpret_f32(),
            0xbd => visitor.visit_i64_reinterpret_f64(),
            0xbe => visitor.visit_f32_reinterpret_i32(),
            0xbf => visitor.visit_f64_reinterpret_i64(),

            0xc0 => visitor.visit_i32_extend8_s(),
            0xc1 => visitor.visit_i32_extend16_s(),
            0xc2 => visitor.visit_i64_extend8_s(),
            0xc3 => visitor.visit_i64_extend16_s(),
            0xc4 => visitor.visit_i64_extend32_s(),

            0xd0 => visitor.visit_ref_null(self.reader.read()?),
            0xd1 => visitor.visit_ref_is_null(),
            0xd2 => visitor.visit_ref_func(self.reader.read_var_u32()?),
            0xd3 => visitor.visit_ref_eq(),
            0xd4 => visitor.visit_ref_as_non_null(),
            0xd5 => visitor.visit_br_on_null(self.reader.read_var_u32()?),
            0xd6 => visitor.visit_br_on_non_null(self.reader.read_var_u32()?),

            0xe0 => visitor.visit_cont_new(self.reader.read_var_u32()?),
            0xe1 => {
                visitor.visit_cont_bind(self.reader.read_var_u32()?, self.reader.read_var_u32()?)
            }
            0xe2 => visitor.visit_suspend(self.reader.read_var_u32()?),
            0xe3 => visitor.visit_resume(self.reader.read_var_u32()?, self.reader.read()?),
            0xe4 => visitor.visit_resume_throw(
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
                self.reader.read()?,
            ),
            0xe5 => visitor.visit_switch(self.reader.read_var_u32()?, self.reader.read_var_u32()?),

            0xfb => self.visit_0xfb_operator(pos, visitor)?,
            0xfc => self.visit_0xfc_operator(pos, visitor)?,
            0xfd => {
                #[cfg(feature = "simd")]
                if let Some(mut visitor) = visitor.simd_visitor() {
                    return self.visit_0xfd_operator(pos, &mut visitor);
                }
                bail!(pos, "unexpected SIMD opcode: 0x{code:x}")
            }
            0xfe => self.visit_0xfe_operator(pos, visitor)?,

            _ => bail!(pos, "illegal opcode: 0x{code:x}"),
        })
    }

    fn visit_0xfb_operator<T>(
        &mut self,
        pos: usize,
        visitor: &mut T,
    ) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        let code = self.reader.read_var_u32()?;
        Ok(match code {
            0x0 => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_struct_new(type_index)
            }
            0x01 => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_struct_new_default(type_index)
            }
            0x02 => {
                let type_index = self.reader.read_var_u32()?;
                let field_index = self.reader.read_var_u32()?;
                visitor.visit_struct_get(type_index, field_index)
            }
            0x03 => {
                let type_index = self.reader.read_var_u32()?;
                let field_index = self.reader.read_var_u32()?;
                visitor.visit_struct_get_s(type_index, field_index)
            }
            0x04 => {
                let type_index = self.reader.read_var_u32()?;
                let field_index = self.reader.read_var_u32()?;
                visitor.visit_struct_get_u(type_index, field_index)
            }
            0x05 => {
                let type_index = self.reader.read_var_u32()?;
                let field_index = self.reader.read_var_u32()?;
                visitor.visit_struct_set(type_index, field_index)
            }
            0x06 => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_array_new(type_index)
            }
            0x07 => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_array_new_default(type_index)
            }
            0x08 => {
                let type_index = self.reader.read_var_u32()?;
                let n = self.reader.read_var_u32()?;
                visitor.visit_array_new_fixed(type_index, n)
            }
            0x09 => {
                let type_index = self.reader.read_var_u32()?;
                let data_index = self.reader.read_var_u32()?;
                visitor.visit_array_new_data(type_index, data_index)
            }
            0x0a => {
                let type_index = self.reader.read_var_u32()?;
                let elem_index = self.reader.read_var_u32()?;
                visitor.visit_array_new_elem(type_index, elem_index)
            }
            0x0b => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_array_get(type_index)
            }
            0x0c => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_array_get_s(type_index)
            }
            0x0d => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_array_get_u(type_index)
            }
            0x0e => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_array_set(type_index)
            }
            0x0f => visitor.visit_array_len(),
            0x10 => {
                let type_index = self.reader.read_var_u32()?;
                visitor.visit_array_fill(type_index)
            }
            0x11 => {
                let type_index_dst = self.reader.read_var_u32()?;
                let type_index_src = self.reader.read_var_u32()?;
                visitor.visit_array_copy(type_index_dst, type_index_src)
            }
            0x12 => {
                let type_index = self.reader.read_var_u32()?;
                let data_index = self.reader.read_var_u32()?;
                visitor.visit_array_init_data(type_index, data_index)
            }
            0x13 => {
                let type_index = self.reader.read_var_u32()?;
                let elem_index = self.reader.read_var_u32()?;
                visitor.visit_array_init_elem(type_index, elem_index)
            }
            0x14 => visitor.visit_ref_test_non_null(self.reader.read()?),
            0x15 => visitor.visit_ref_test_nullable(self.reader.read()?),
            0x16 => visitor.visit_ref_cast_non_null(self.reader.read()?),
            0x17 => visitor.visit_ref_cast_nullable(self.reader.read()?),
            0x18 => {
                let pos = self.original_position();
                let cast_flags = self.reader.read_u8()?;
                let relative_depth = self.reader.read_var_u32()?;
                let (from_type_nullable, to_type_nullable) = match cast_flags {
                    0b00 => (false, false),
                    0b01 => (true, false),
                    0b10 => (false, true),
                    0b11 => (true, true),
                    _ => bail!(pos, "invalid cast flags: {cast_flags:08b}"),
                };
                let from_heap_type = self.reader.read()?;
                let from_ref_type =
                    RefType::new(from_type_nullable, from_heap_type).ok_or_else(|| {
                        format_err!(pos, "implementation error: type index too large")
                    })?;
                let to_heap_type = self.reader.read()?;
                let to_ref_type =
                    RefType::new(to_type_nullable, to_heap_type).ok_or_else(|| {
                        format_err!(pos, "implementation error: type index too large")
                    })?;
                visitor.visit_br_on_cast(relative_depth, from_ref_type, to_ref_type)
            }
            0x19 => {
                let pos = self.original_position();
                let cast_flags = self.reader.read_u8()?;
                let relative_depth = self.reader.read_var_u32()?;
                let (from_type_nullable, to_type_nullable) = match cast_flags {
                    0 => (false, false),
                    1 => (true, false),
                    2 => (false, true),
                    3 => (true, true),
                    _ => bail!(pos, "invalid cast flags: {cast_flags:08b}"),
                };
                let from_heap_type = self.reader.read()?;
                let from_ref_type =
                    RefType::new(from_type_nullable, from_heap_type).ok_or_else(|| {
                        format_err!(pos, "implementation error: type index too large")
                    })?;
                let to_heap_type = self.reader.read()?;
                let to_ref_type =
                    RefType::new(to_type_nullable, to_heap_type).ok_or_else(|| {
                        format_err!(pos, "implementation error: type index too large")
                    })?;
                visitor.visit_br_on_cast_fail(relative_depth, from_ref_type, to_ref_type)
            }

            0x1a => visitor.visit_any_convert_extern(),
            0x1b => visitor.visit_extern_convert_any(),

            0x1c => visitor.visit_ref_i31(),
            0x1d => visitor.visit_i31_get_s(),
            0x1e => visitor.visit_i31_get_u(),

            _ => bail!(pos, "unknown 0xfb subopcode: 0x{code:x}"),
        })
    }

    fn visit_0xfc_operator<T>(
        &mut self,
        pos: usize,
        visitor: &mut T,
    ) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        let code = self.reader.read_var_u32()?;
        Ok(match code {
            0x00 => visitor.visit_i32_trunc_sat_f32_s(),
            0x01 => visitor.visit_i32_trunc_sat_f32_u(),
            0x02 => visitor.visit_i32_trunc_sat_f64_s(),
            0x03 => visitor.visit_i32_trunc_sat_f64_u(),
            0x04 => visitor.visit_i64_trunc_sat_f32_s(),
            0x05 => visitor.visit_i64_trunc_sat_f32_u(),
            0x06 => visitor.visit_i64_trunc_sat_f64_s(),
            0x07 => visitor.visit_i64_trunc_sat_f64_u(),

            0x08 => {
                let segment = self.reader.read_var_u32()?;
                let mem = self.reader.read_var_u32()?;
                visitor.visit_memory_init(segment, mem)
            }
            0x09 => {
                let segment = self.reader.read_var_u32()?;
                visitor.visit_data_drop(segment)
            }
            0x0a => {
                let dst = self.reader.read_var_u32()?;
                let src = self.reader.read_var_u32()?;
                visitor.visit_memory_copy(dst, src)
            }
            0x0b => {
                let mem = self.reader.read_var_u32()?;
                visitor.visit_memory_fill(mem)
            }
            0x0c => {
                let segment = self.reader.read_var_u32()?;
                let table = self.reader.read_var_u32()?;
                visitor.visit_table_init(segment, table)
            }
            0x0d => {
                let segment = self.reader.read_var_u32()?;
                visitor.visit_elem_drop(segment)
            }
            0x0e => {
                let dst_table = self.reader.read_var_u32()?;
                let src_table = self.reader.read_var_u32()?;
                visitor.visit_table_copy(dst_table, src_table)
            }

            0x0f => {
                let table = self.reader.read_var_u32()?;
                visitor.visit_table_grow(table)
            }
            0x10 => {
                let table = self.reader.read_var_u32()?;
                visitor.visit_table_size(table)
            }

            0x11 => {
                let table = self.reader.read_var_u32()?;
                visitor.visit_table_fill(table)
            }

            0x12 => {
                let mem = self.reader.read_var_u32()?;
                visitor.visit_memory_discard(mem)
            }

            0x13 => visitor.visit_i64_add128(),
            0x14 => visitor.visit_i64_sub128(),
            0x15 => visitor.visit_i64_mul_wide_s(),
            0x16 => visitor.visit_i64_mul_wide_u(),

            _ => bail!(pos, "unknown 0xfc subopcode: 0x{code:x}"),
        })
    }

    #[cfg(feature = "simd")]
    pub(super) fn visit_0xfd_operator<T>(
        &mut self,
        pos: usize,
        visitor: &mut T,
    ) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitSimdOperator<'a>,
    {
        let code = self.reader.read_var_u32()?;
        Ok(match code {
            0x00 => visitor.visit_v128_load(self.read_memarg(4)?),
            0x01 => visitor.visit_v128_load8x8_s(self.read_memarg(3)?),
            0x02 => visitor.visit_v128_load8x8_u(self.read_memarg(3)?),
            0x03 => visitor.visit_v128_load16x4_s(self.read_memarg(3)?),
            0x04 => visitor.visit_v128_load16x4_u(self.read_memarg(3)?),
            0x05 => visitor.visit_v128_load32x2_s(self.read_memarg(3)?),
            0x06 => visitor.visit_v128_load32x2_u(self.read_memarg(3)?),
            0x07 => visitor.visit_v128_load8_splat(self.read_memarg(0)?),
            0x08 => visitor.visit_v128_load16_splat(self.read_memarg(1)?),
            0x09 => visitor.visit_v128_load32_splat(self.read_memarg(2)?),
            0x0a => visitor.visit_v128_load64_splat(self.read_memarg(3)?),

            0x0b => visitor.visit_v128_store(self.read_memarg(4)?),
            0x0c => visitor.visit_v128_const(self.read_v128()?),
            0x0d => {
                let mut lanes: [u8; 16] = [0; 16];
                for lane in &mut lanes {
                    *lane = self.read_lane_index()?
                }
                visitor.visit_i8x16_shuffle(lanes)
            }

            0x0e => visitor.visit_i8x16_swizzle(),
            0x0f => visitor.visit_i8x16_splat(),
            0x10 => visitor.visit_i16x8_splat(),
            0x11 => visitor.visit_i32x4_splat(),
            0x12 => visitor.visit_i64x2_splat(),
            0x13 => visitor.visit_f32x4_splat(),
            0x14 => visitor.visit_f64x2_splat(),

            0x15 => visitor.visit_i8x16_extract_lane_s(self.read_lane_index()?),
            0x16 => visitor.visit_i8x16_extract_lane_u(self.read_lane_index()?),
            0x17 => visitor.visit_i8x16_replace_lane(self.read_lane_index()?),
            0x18 => visitor.visit_i16x8_extract_lane_s(self.read_lane_index()?),
            0x19 => visitor.visit_i16x8_extract_lane_u(self.read_lane_index()?),
            0x1a => visitor.visit_i16x8_replace_lane(self.read_lane_index()?),
            0x1b => visitor.visit_i32x4_extract_lane(self.read_lane_index()?),

            0x1c => visitor.visit_i32x4_replace_lane(self.read_lane_index()?),
            0x1d => visitor.visit_i64x2_extract_lane(self.read_lane_index()?),
            0x1e => visitor.visit_i64x2_replace_lane(self.read_lane_index()?),
            0x1f => visitor.visit_f32x4_extract_lane(self.read_lane_index()?),
            0x20 => visitor.visit_f32x4_replace_lane(self.read_lane_index()?),
            0x21 => visitor.visit_f64x2_extract_lane(self.read_lane_index()?),
            0x22 => visitor.visit_f64x2_replace_lane(self.read_lane_index()?),

            0x23 => visitor.visit_i8x16_eq(),
            0x24 => visitor.visit_i8x16_ne(),
            0x25 => visitor.visit_i8x16_lt_s(),
            0x26 => visitor.visit_i8x16_lt_u(),
            0x27 => visitor.visit_i8x16_gt_s(),
            0x28 => visitor.visit_i8x16_gt_u(),
            0x29 => visitor.visit_i8x16_le_s(),
            0x2a => visitor.visit_i8x16_le_u(),
            0x2b => visitor.visit_i8x16_ge_s(),
            0x2c => visitor.visit_i8x16_ge_u(),
            0x2d => visitor.visit_i16x8_eq(),
            0x2e => visitor.visit_i16x8_ne(),
            0x2f => visitor.visit_i16x8_lt_s(),
            0x30 => visitor.visit_i16x8_lt_u(),
            0x31 => visitor.visit_i16x8_gt_s(),
            0x32 => visitor.visit_i16x8_gt_u(),
            0x33 => visitor.visit_i16x8_le_s(),
            0x34 => visitor.visit_i16x8_le_u(),
            0x35 => visitor.visit_i16x8_ge_s(),
            0x36 => visitor.visit_i16x8_ge_u(),
            0x37 => visitor.visit_i32x4_eq(),
            0x38 => visitor.visit_i32x4_ne(),
            0x39 => visitor.visit_i32x4_lt_s(),
            0x3a => visitor.visit_i32x4_lt_u(),
            0x3b => visitor.visit_i32x4_gt_s(),
            0x3c => visitor.visit_i32x4_gt_u(),
            0x3d => visitor.visit_i32x4_le_s(),
            0x3e => visitor.visit_i32x4_le_u(),
            0x3f => visitor.visit_i32x4_ge_s(),
            0x40 => visitor.visit_i32x4_ge_u(),
            0x41 => visitor.visit_f32x4_eq(),
            0x42 => visitor.visit_f32x4_ne(),
            0x43 => visitor.visit_f32x4_lt(),
            0x44 => visitor.visit_f32x4_gt(),
            0x45 => visitor.visit_f32x4_le(),
            0x46 => visitor.visit_f32x4_ge(),
            0x47 => visitor.visit_f64x2_eq(),
            0x48 => visitor.visit_f64x2_ne(),
            0x49 => visitor.visit_f64x2_lt(),
            0x4a => visitor.visit_f64x2_gt(),
            0x4b => visitor.visit_f64x2_le(),
            0x4c => visitor.visit_f64x2_ge(),
            0x4d => visitor.visit_v128_not(),
            0x4e => visitor.visit_v128_and(),
            0x4f => visitor.visit_v128_andnot(),
            0x50 => visitor.visit_v128_or(),
            0x51 => visitor.visit_v128_xor(),
            0x52 => visitor.visit_v128_bitselect(),
            0x53 => visitor.visit_v128_any_true(),

            0x54 => {
                let memarg = self.read_memarg(0)?;
                let lane = self.read_lane_index()?;
                visitor.visit_v128_load8_lane(memarg, lane)
            }
            0x55 => {
                let memarg = self.read_memarg(1)?;
                let lane = self.read_lane_index()?;
                visitor.visit_v128_load16_lane(memarg, lane)
            }
            0x56 => {
                let memarg = self.read_memarg(2)?;
                let lane = self.read_lane_index()?;
                visitor.visit_v128_load32_lane(memarg, lane)
            }
            0x57 => {
                let memarg = self.read_memarg(3)?;
                let lane = self.read_lane_index()?;
                visitor.visit_v128_load64_lane(memarg, lane)
            }
            0x58 => {
                let memarg = self.read_memarg(0)?;
                let lane = self.read_lane_index()?;
                visitor.visit_v128_store8_lane(memarg, lane)
            }
            0x59 => {
                let memarg = self.read_memarg(1)?;
                let lane = self.read_lane_index()?;
                visitor.visit_v128_store16_lane(memarg, lane)
            }
            0x5a => {
                let memarg = self.read_memarg(2)?;
                let lane = self.read_lane_index()?;
                visitor.visit_v128_store32_lane(memarg, lane)
            }
            0x5b => {
                let memarg = self.read_memarg(3)?;
                let lane = self.read_lane_index()?;
                visitor.visit_v128_store64_lane(memarg, lane)
            }

            0x5c => visitor.visit_v128_load32_zero(self.read_memarg(2)?),
            0x5d => visitor.visit_v128_load64_zero(self.read_memarg(3)?),
            0x5e => visitor.visit_f32x4_demote_f64x2_zero(),
            0x5f => visitor.visit_f64x2_promote_low_f32x4(),
            0x60 => visitor.visit_i8x16_abs(),
            0x61 => visitor.visit_i8x16_neg(),
            0x62 => visitor.visit_i8x16_popcnt(),
            0x63 => visitor.visit_i8x16_all_true(),
            0x64 => visitor.visit_i8x16_bitmask(),
            0x65 => visitor.visit_i8x16_narrow_i16x8_s(),
            0x66 => visitor.visit_i8x16_narrow_i16x8_u(),
            0x67 => visitor.visit_f32x4_ceil(),
            0x68 => visitor.visit_f32x4_floor(),
            0x69 => visitor.visit_f32x4_trunc(),
            0x6a => visitor.visit_f32x4_nearest(),
            0x6b => visitor.visit_i8x16_shl(),
            0x6c => visitor.visit_i8x16_shr_s(),
            0x6d => visitor.visit_i8x16_shr_u(),
            0x6e => visitor.visit_i8x16_add(),
            0x6f => visitor.visit_i8x16_add_sat_s(),
            0x70 => visitor.visit_i8x16_add_sat_u(),
            0x71 => visitor.visit_i8x16_sub(),
            0x72 => visitor.visit_i8x16_sub_sat_s(),
            0x73 => visitor.visit_i8x16_sub_sat_u(),
            0x74 => visitor.visit_f64x2_ceil(),
            0x75 => visitor.visit_f64x2_floor(),
            0x76 => visitor.visit_i8x16_min_s(),
            0x77 => visitor.visit_i8x16_min_u(),
            0x78 => visitor.visit_i8x16_max_s(),
            0x79 => visitor.visit_i8x16_max_u(),
            0x7a => visitor.visit_f64x2_trunc(),
            0x7b => visitor.visit_i8x16_avgr_u(),
            0x7c => visitor.visit_i16x8_extadd_pairwise_i8x16_s(),
            0x7d => visitor.visit_i16x8_extadd_pairwise_i8x16_u(),
            0x7e => visitor.visit_i32x4_extadd_pairwise_i16x8_s(),
            0x7f => visitor.visit_i32x4_extadd_pairwise_i16x8_u(),
            0x80 => visitor.visit_i16x8_abs(),
            0x81 => visitor.visit_i16x8_neg(),
            0x82 => visitor.visit_i16x8_q15mulr_sat_s(),
            0x83 => visitor.visit_i16x8_all_true(),
            0x84 => visitor.visit_i16x8_bitmask(),
            0x85 => visitor.visit_i16x8_narrow_i32x4_s(),
            0x86 => visitor.visit_i16x8_narrow_i32x4_u(),
            0x87 => visitor.visit_i16x8_extend_low_i8x16_s(),
            0x88 => visitor.visit_i16x8_extend_high_i8x16_s(),
            0x89 => visitor.visit_i16x8_extend_low_i8x16_u(),
            0x8a => visitor.visit_i16x8_extend_high_i8x16_u(),
            0x8b => visitor.visit_i16x8_shl(),
            0x8c => visitor.visit_i16x8_shr_s(),
            0x8d => visitor.visit_i16x8_shr_u(),
            0x8e => visitor.visit_i16x8_add(),
            0x8f => visitor.visit_i16x8_add_sat_s(),
            0x90 => visitor.visit_i16x8_add_sat_u(),
            0x91 => visitor.visit_i16x8_sub(),
            0x92 => visitor.visit_i16x8_sub_sat_s(),
            0x93 => visitor.visit_i16x8_sub_sat_u(),
            0x94 => visitor.visit_f64x2_nearest(),
            0x95 => visitor.visit_i16x8_mul(),
            0x96 => visitor.visit_i16x8_min_s(),
            0x97 => visitor.visit_i16x8_min_u(),
            0x98 => visitor.visit_i16x8_max_s(),
            0x99 => visitor.visit_i16x8_max_u(),
            0x9b => visitor.visit_i16x8_avgr_u(),
            0x9c => visitor.visit_i16x8_extmul_low_i8x16_s(),
            0x9d => visitor.visit_i16x8_extmul_high_i8x16_s(),
            0x9e => visitor.visit_i16x8_extmul_low_i8x16_u(),
            0x9f => visitor.visit_i16x8_extmul_high_i8x16_u(),
            0xa0 => visitor.visit_i32x4_abs(),
            0xa1 => visitor.visit_i32x4_neg(),
            0xa3 => visitor.visit_i32x4_all_true(),
            0xa4 => visitor.visit_i32x4_bitmask(),
            0xa7 => visitor.visit_i32x4_extend_low_i16x8_s(),
            0xa8 => visitor.visit_i32x4_extend_high_i16x8_s(),
            0xa9 => visitor.visit_i32x4_extend_low_i16x8_u(),
            0xaa => visitor.visit_i32x4_extend_high_i16x8_u(),
            0xab => visitor.visit_i32x4_shl(),
            0xac => visitor.visit_i32x4_shr_s(),
            0xad => visitor.visit_i32x4_shr_u(),
            0xae => visitor.visit_i32x4_add(),
            0xb1 => visitor.visit_i32x4_sub(),
            0xb5 => visitor.visit_i32x4_mul(),
            0xb6 => visitor.visit_i32x4_min_s(),
            0xb7 => visitor.visit_i32x4_min_u(),
            0xb8 => visitor.visit_i32x4_max_s(),
            0xb9 => visitor.visit_i32x4_max_u(),
            0xba => visitor.visit_i32x4_dot_i16x8_s(),
            0xbc => visitor.visit_i32x4_extmul_low_i16x8_s(),
            0xbd => visitor.visit_i32x4_extmul_high_i16x8_s(),
            0xbe => visitor.visit_i32x4_extmul_low_i16x8_u(),
            0xbf => visitor.visit_i32x4_extmul_high_i16x8_u(),
            0xc0 => visitor.visit_i64x2_abs(),
            0xc1 => visitor.visit_i64x2_neg(),
            0xc3 => visitor.visit_i64x2_all_true(),
            0xc4 => visitor.visit_i64x2_bitmask(),
            0xc7 => visitor.visit_i64x2_extend_low_i32x4_s(),
            0xc8 => visitor.visit_i64x2_extend_high_i32x4_s(),
            0xc9 => visitor.visit_i64x2_extend_low_i32x4_u(),
            0xca => visitor.visit_i64x2_extend_high_i32x4_u(),
            0xcb => visitor.visit_i64x2_shl(),
            0xcc => visitor.visit_i64x2_shr_s(),
            0xcd => visitor.visit_i64x2_shr_u(),
            0xce => visitor.visit_i64x2_add(),
            0xd1 => visitor.visit_i64x2_sub(),
            0xd5 => visitor.visit_i64x2_mul(),
            0xd6 => visitor.visit_i64x2_eq(),
            0xd7 => visitor.visit_i64x2_ne(),
            0xd8 => visitor.visit_i64x2_lt_s(),
            0xd9 => visitor.visit_i64x2_gt_s(),
            0xda => visitor.visit_i64x2_le_s(),
            0xdb => visitor.visit_i64x2_ge_s(),
            0xdc => visitor.visit_i64x2_extmul_low_i32x4_s(),
            0xdd => visitor.visit_i64x2_extmul_high_i32x4_s(),
            0xde => visitor.visit_i64x2_extmul_low_i32x4_u(),
            0xdf => visitor.visit_i64x2_extmul_high_i32x4_u(),
            0xe0 => visitor.visit_f32x4_abs(),
            0xe1 => visitor.visit_f32x4_neg(),
            0xe3 => visitor.visit_f32x4_sqrt(),
            0xe4 => visitor.visit_f32x4_add(),
            0xe5 => visitor.visit_f32x4_sub(),
            0xe6 => visitor.visit_f32x4_mul(),
            0xe7 => visitor.visit_f32x4_div(),
            0xe8 => visitor.visit_f32x4_min(),
            0xe9 => visitor.visit_f32x4_max(),
            0xea => visitor.visit_f32x4_pmin(),
            0xeb => visitor.visit_f32x4_pmax(),
            0xec => visitor.visit_f64x2_abs(),
            0xed => visitor.visit_f64x2_neg(),
            0xef => visitor.visit_f64x2_sqrt(),
            0xf0 => visitor.visit_f64x2_add(),
            0xf1 => visitor.visit_f64x2_sub(),
            0xf2 => visitor.visit_f64x2_mul(),
            0xf3 => visitor.visit_f64x2_div(),
            0xf4 => visitor.visit_f64x2_min(),
            0xf5 => visitor.visit_f64x2_max(),
            0xf6 => visitor.visit_f64x2_pmin(),
            0xf7 => visitor.visit_f64x2_pmax(),
            0xf8 => visitor.visit_i32x4_trunc_sat_f32x4_s(),
            0xf9 => visitor.visit_i32x4_trunc_sat_f32x4_u(),
            0xfa => visitor.visit_f32x4_convert_i32x4_s(),
            0xfb => visitor.visit_f32x4_convert_i32x4_u(),
            0xfc => visitor.visit_i32x4_trunc_sat_f64x2_s_zero(),
            0xfd => visitor.visit_i32x4_trunc_sat_f64x2_u_zero(),
            0xfe => visitor.visit_f64x2_convert_low_i32x4_s(),
            0xff => visitor.visit_f64x2_convert_low_i32x4_u(),
            0x100 => visitor.visit_i8x16_relaxed_swizzle(),
            0x101 => visitor.visit_i32x4_relaxed_trunc_f32x4_s(),
            0x102 => visitor.visit_i32x4_relaxed_trunc_f32x4_u(),
            0x103 => visitor.visit_i32x4_relaxed_trunc_f64x2_s_zero(),
            0x104 => visitor.visit_i32x4_relaxed_trunc_f64x2_u_zero(),
            0x105 => visitor.visit_f32x4_relaxed_madd(),
            0x106 => visitor.visit_f32x4_relaxed_nmadd(),
            0x107 => visitor.visit_f64x2_relaxed_madd(),
            0x108 => visitor.visit_f64x2_relaxed_nmadd(),
            0x109 => visitor.visit_i8x16_relaxed_laneselect(),
            0x10a => visitor.visit_i16x8_relaxed_laneselect(),
            0x10b => visitor.visit_i32x4_relaxed_laneselect(),
            0x10c => visitor.visit_i64x2_relaxed_laneselect(),
            0x10d => visitor.visit_f32x4_relaxed_min(),
            0x10e => visitor.visit_f32x4_relaxed_max(),
            0x10f => visitor.visit_f64x2_relaxed_min(),
            0x110 => visitor.visit_f64x2_relaxed_max(),
            0x111 => visitor.visit_i16x8_relaxed_q15mulr_s(),
            0x112 => visitor.visit_i16x8_relaxed_dot_i8x16_i7x16_s(),
            0x113 => visitor.visit_i32x4_relaxed_dot_i8x16_i7x16_add_s(),

            _ => bail!(pos, "unknown 0xfd subopcode: 0x{code:x}"),
        })
    }

    fn visit_0xfe_operator<T>(
        &mut self,
        pos: usize,
        visitor: &mut T,
    ) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        let code = self.reader.read_var_u32()?;
        Ok(match code {
            0x00 => visitor.visit_memory_atomic_notify(self.read_memarg(2)?),
            0x01 => visitor.visit_memory_atomic_wait32(self.read_memarg(2)?),
            0x02 => visitor.visit_memory_atomic_wait64(self.read_memarg(3)?),
            0x03 => {
                if self.reader.read_u8()? != 0 {
                    bail!(pos, "nonzero byte after `atomic.fence`");
                }
                visitor.visit_atomic_fence()
            }
            0x10 => visitor.visit_i32_atomic_load(self.read_memarg(2)?),
            0x11 => visitor.visit_i64_atomic_load(self.read_memarg(3)?),
            0x12 => visitor.visit_i32_atomic_load8_u(self.read_memarg(0)?),
            0x13 => visitor.visit_i32_atomic_load16_u(self.read_memarg(1)?),
            0x14 => visitor.visit_i64_atomic_load8_u(self.read_memarg(0)?),
            0x15 => visitor.visit_i64_atomic_load16_u(self.read_memarg(1)?),
            0x16 => visitor.visit_i64_atomic_load32_u(self.read_memarg(2)?),
            0x17 => visitor.visit_i32_atomic_store(self.read_memarg(2)?),
            0x18 => visitor.visit_i64_atomic_store(self.read_memarg(3)?),
            0x19 => visitor.visit_i32_atomic_store8(self.read_memarg(0)?),
            0x1a => visitor.visit_i32_atomic_store16(self.read_memarg(1)?),
            0x1b => visitor.visit_i64_atomic_store8(self.read_memarg(0)?),
            0x1c => visitor.visit_i64_atomic_store16(self.read_memarg(1)?),
            0x1d => visitor.visit_i64_atomic_store32(self.read_memarg(2)?),
            0x1e => visitor.visit_i32_atomic_rmw_add(self.read_memarg(2)?),
            0x1f => visitor.visit_i64_atomic_rmw_add(self.read_memarg(3)?),
            0x20 => visitor.visit_i32_atomic_rmw8_add_u(self.read_memarg(0)?),
            0x21 => visitor.visit_i32_atomic_rmw16_add_u(self.read_memarg(1)?),
            0x22 => visitor.visit_i64_atomic_rmw8_add_u(self.read_memarg(0)?),
            0x23 => visitor.visit_i64_atomic_rmw16_add_u(self.read_memarg(1)?),
            0x24 => visitor.visit_i64_atomic_rmw32_add_u(self.read_memarg(2)?),
            0x25 => visitor.visit_i32_atomic_rmw_sub(self.read_memarg(2)?),
            0x26 => visitor.visit_i64_atomic_rmw_sub(self.read_memarg(3)?),
            0x27 => visitor.visit_i32_atomic_rmw8_sub_u(self.read_memarg(0)?),
            0x28 => visitor.visit_i32_atomic_rmw16_sub_u(self.read_memarg(1)?),
            0x29 => visitor.visit_i64_atomic_rmw8_sub_u(self.read_memarg(0)?),
            0x2a => visitor.visit_i64_atomic_rmw16_sub_u(self.read_memarg(1)?),
            0x2b => visitor.visit_i64_atomic_rmw32_sub_u(self.read_memarg(2)?),
            0x2c => visitor.visit_i32_atomic_rmw_and(self.read_memarg(2)?),
            0x2d => visitor.visit_i64_atomic_rmw_and(self.read_memarg(3)?),
            0x2e => visitor.visit_i32_atomic_rmw8_and_u(self.read_memarg(0)?),
            0x2f => visitor.visit_i32_atomic_rmw16_and_u(self.read_memarg(1)?),
            0x30 => visitor.visit_i64_atomic_rmw8_and_u(self.read_memarg(0)?),
            0x31 => visitor.visit_i64_atomic_rmw16_and_u(self.read_memarg(1)?),
            0x32 => visitor.visit_i64_atomic_rmw32_and_u(self.read_memarg(2)?),
            0x33 => visitor.visit_i32_atomic_rmw_or(self.read_memarg(2)?),
            0x34 => visitor.visit_i64_atomic_rmw_or(self.read_memarg(3)?),
            0x35 => visitor.visit_i32_atomic_rmw8_or_u(self.read_memarg(0)?),
            0x36 => visitor.visit_i32_atomic_rmw16_or_u(self.read_memarg(1)?),
            0x37 => visitor.visit_i64_atomic_rmw8_or_u(self.read_memarg(0)?),
            0x38 => visitor.visit_i64_atomic_rmw16_or_u(self.read_memarg(1)?),
            0x39 => visitor.visit_i64_atomic_rmw32_or_u(self.read_memarg(2)?),
            0x3a => visitor.visit_i32_atomic_rmw_xor(self.read_memarg(2)?),
            0x3b => visitor.visit_i64_atomic_rmw_xor(self.read_memarg(3)?),
            0x3c => visitor.visit_i32_atomic_rmw8_xor_u(self.read_memarg(0)?),
            0x3d => visitor.visit_i32_atomic_rmw16_xor_u(self.read_memarg(1)?),
            0x3e => visitor.visit_i64_atomic_rmw8_xor_u(self.read_memarg(0)?),
            0x3f => visitor.visit_i64_atomic_rmw16_xor_u(self.read_memarg(1)?),
            0x40 => visitor.visit_i64_atomic_rmw32_xor_u(self.read_memarg(2)?),
            0x41 => visitor.visit_i32_atomic_rmw_xchg(self.read_memarg(2)?),
            0x42 => visitor.visit_i64_atomic_rmw_xchg(self.read_memarg(3)?),
            0x43 => visitor.visit_i32_atomic_rmw8_xchg_u(self.read_memarg(0)?),
            0x44 => visitor.visit_i32_atomic_rmw16_xchg_u(self.read_memarg(1)?),
            0x45 => visitor.visit_i64_atomic_rmw8_xchg_u(self.read_memarg(0)?),
            0x46 => visitor.visit_i64_atomic_rmw16_xchg_u(self.read_memarg(1)?),
            0x47 => visitor.visit_i64_atomic_rmw32_xchg_u(self.read_memarg(2)?),
            0x48 => visitor.visit_i32_atomic_rmw_cmpxchg(self.read_memarg(2)?),
            0x49 => visitor.visit_i64_atomic_rmw_cmpxchg(self.read_memarg(3)?),
            0x4a => visitor.visit_i32_atomic_rmw8_cmpxchg_u(self.read_memarg(0)?),
            0x4b => visitor.visit_i32_atomic_rmw16_cmpxchg_u(self.read_memarg(1)?),
            0x4c => visitor.visit_i64_atomic_rmw8_cmpxchg_u(self.read_memarg(0)?),
            0x4d => visitor.visit_i64_atomic_rmw16_cmpxchg_u(self.read_memarg(1)?),
            0x4e => visitor.visit_i64_atomic_rmw32_cmpxchg_u(self.read_memarg(2)?),

            // Decode shared-everything-threads proposal.
            0x4f => {
                visitor.visit_global_atomic_get(self.read_ordering()?, self.reader.read_var_u32()?)
            }
            0x50 => {
                visitor.visit_global_atomic_set(self.read_ordering()?, self.reader.read_var_u32()?)
            }
            0x51 => visitor
                .visit_global_atomic_rmw_add(self.read_ordering()?, self.reader.read_var_u32()?),
            0x52 => visitor
                .visit_global_atomic_rmw_sub(self.read_ordering()?, self.reader.read_var_u32()?),
            0x53 => visitor
                .visit_global_atomic_rmw_and(self.read_ordering()?, self.reader.read_var_u32()?),
            0x54 => visitor
                .visit_global_atomic_rmw_or(self.read_ordering()?, self.reader.read_var_u32()?),
            0x55 => visitor
                .visit_global_atomic_rmw_xor(self.read_ordering()?, self.reader.read_var_u32()?),
            0x56 => visitor
                .visit_global_atomic_rmw_xchg(self.read_ordering()?, self.reader.read_var_u32()?),
            0x57 => visitor.visit_global_atomic_rmw_cmpxchg(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
            ),
            0x58 => {
                visitor.visit_table_atomic_get(self.read_ordering()?, self.reader.read_var_u32()?)
            }
            0x59 => {
                visitor.visit_table_atomic_set(self.read_ordering()?, self.reader.read_var_u32()?)
            }
            0x5a => visitor
                .visit_table_atomic_rmw_xchg(self.read_ordering()?, self.reader.read_var_u32()?),
            0x5b => visitor
                .visit_table_atomic_rmw_cmpxchg(self.read_ordering()?, self.reader.read_var_u32()?),
            0x5c => visitor.visit_struct_atomic_get(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x5d => visitor.visit_struct_atomic_get_s(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x5e => visitor.visit_struct_atomic_get_u(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x5f => visitor.visit_struct_atomic_set(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x60 => visitor.visit_struct_atomic_rmw_add(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x61 => visitor.visit_struct_atomic_rmw_sub(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x62 => visitor.visit_struct_atomic_rmw_and(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x63 => visitor.visit_struct_atomic_rmw_or(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x64 => visitor.visit_struct_atomic_rmw_xor(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x65 => visitor.visit_struct_atomic_rmw_xchg(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x66 => visitor.visit_struct_atomic_rmw_cmpxchg(
                self.read_ordering()?,
                self.reader.read_var_u32()?,
                self.reader.read_var_u32()?,
            ),
            0x67 => {
                visitor.visit_array_atomic_get(self.read_ordering()?, self.reader.read_var_u32()?)
            }
            0x68 => {
                visitor.visit_array_atomic_get_s(self.read_ordering()?, self.reader.read_var_u32()?)
            }
            0x69 => {
                visitor.visit_array_atomic_get_u(self.read_ordering()?, self.reader.read_var_u32()?)
            }
            0x6a => {
                visitor.visit_array_atomic_set(self.read_ordering()?, self.reader.read_var_u32()?)
            }
            0x6b => visitor
                .visit_array_atomic_rmw_add(self.read_ordering()?, self.reader.read_var_u32()?),
            0x6c => visitor
                .visit_array_atomic_rmw_sub(self.read_ordering()?, self.reader.read_var_u32()?),
            0x6d => visitor
                .visit_array_atomic_rmw_and(self.read_ordering()?, self.reader.read_var_u32()?),
            0x6e => visitor
                .visit_array_atomic_rmw_or(self.read_ordering()?, self.reader.read_var_u32()?),
            0x6f => visitor
                .visit_array_atomic_rmw_xor(self.read_ordering()?, self.reader.read_var_u32()?),
            0x70 => visitor
                .visit_array_atomic_rmw_xchg(self.read_ordering()?, self.reader.read_var_u32()?),
            0x71 => visitor
                .visit_array_atomic_rmw_cmpxchg(self.read_ordering()?, self.reader.read_var_u32()?),
            0x72 => visitor.visit_ref_i31_shared(),

            _ => bail!(pos, "unknown 0xfe subopcode: 0x{code:x}"),
        })
    }

    pub(crate) fn skip_const_expr(&mut self) -> Result<()> {
        // TODO add skip_operator() method and/or validate ConstExpr operators.
        loop {
            if let Operator::End = self.read()? {
                self.ensure_stack_empty()?;
                return Ok(());
            }
        }
    }

    /// Gets a binary reader from this operators reader.
    pub fn get_binary_reader(&self) -> BinaryReader<'a> {
        self.reader.clone()
    }

    /// Returns whether there is an `end` opcode followed by eof remaining in
    /// this reader.
    pub fn is_end_then_eof(&self) -> bool {
        self.reader.is_end_then_eof()
    }

    fn read_memarg(&mut self, max_align: u8) -> Result<MemArg> {
        let flags_pos = self.original_position();
        let mut flags = self.reader.read_var_u32()?;

        let memory = if self.reader.multi_memory() && flags & (1 << 6) != 0 {
            flags ^= 1 << 6;
            self.reader.read_var_u32()?
        } else {
            0
        };
        let max_flag_bits = if self.reader.multi_memory() { 6 } else { 5 };
        if flags >= (1 << max_flag_bits) {
            return Err(BinaryReaderError::new(
                "malformed memop alignment: alignment too large",
                flags_pos,
            ));
        }
        let align = flags as u8;
        let offset = if self.reader.memory64() {
            self.reader.read_var_u64()?
        } else {
            u64::from(self.reader.read_var_u32()?)
        };
        Ok(MemArg {
            align,
            max_align,
            offset,
            memory,
        })
    }

    fn read_ordering(&mut self) -> Result<Ordering> {
        let byte = self.reader.read_var_u32()?;
        match byte {
            0 => Ok(Ordering::SeqCst),
            1 => Ok(Ordering::AcqRel),
            x => Err(BinaryReaderError::new(
                &format!("invalid atomic consistency ordering {x}"),
                self.original_position() - 1,
            )),
        }
    }

    fn read_br_table(&mut self) -> Result<BrTable<'a>> {
        let cnt = self.reader.read_size(MAX_WASM_BR_TABLE_SIZE, "br_table")?;
        let reader = self.reader.skip(|reader| {
            for _ in 0..cnt {
                reader.read_var_u32()?;
            }
            Ok(())
        })?;
        let default = self.reader.read_var_u32()?;
        Ok(BrTable {
            reader,
            cnt: cnt as u32,
            default,
        })
    }

    #[cfg(feature = "simd")]
    fn read_lane_index(&mut self) -> Result<u8> {
        self.reader.read_u8()
    }

    #[cfg(feature = "simd")]
    fn read_v128(&mut self) -> Result<V128> {
        let mut bytes = [0; 16];
        bytes.clone_from_slice(self.reader.read_bytes(16)?);
        Ok(V128(bytes))
    }

    fn read_memory_index_or_zero_if_not_multi_memory(&mut self) -> Result<u32> {
        if self.reader.multi_memory() {
            self.reader.read_var_u32()
        } else {
            // Before bulk memory this byte was required to be a single zero
            // byte, not a LEB-encoded zero, so require a precise zero byte.
            match self.reader.read_u8()? {
                0 => Ok(0),
                _ => bail!(self.original_position() - 1, "zero byte expected"),
            }
        }
    }

    fn read_call_indirect_table_immediate(&mut self) -> Result<u32> {
        // If the `call_indirect_overlong` feature is enabled, then read this
        // immediate as a LEB. This feature is enabled as part of the
        // `reference_types` feature or the `lime1` feature.
        if self.reader.call_indirect_overlong() {
            return self.reader.read_var_u32();
        }

        // Before reference types this byte was required to be a single zero
        // byte, not a LEB-encoded zero, so require a precise zero byte.
        match self.reader.read_u8()? {
            0 => Ok(0),
            _ => bail!(self.original_position() - 1, "zero byte expected"),
        }
    }
}

impl<'a> IntoIterator for OperatorsReader<'a> {
    type Item = Result<Operator<'a>>;
    type IntoIter = OperatorsIterator<'a>;

    /// Reads content of the code section.
    ///
    /// # Examples
    /// ```
    /// # use wasmparser::{Operator, CodeSectionReader, Result, BinaryReader};
    /// # let data: &[u8] = &[
    /// #     0x01, 0x03, 0x00, 0x01, 0x0b];
    /// let reader = BinaryReader::new(data, 0);
    /// let code_reader = CodeSectionReader::new(reader).unwrap();
    /// for body in code_reader {
    ///     let body = body.expect("function body");
    ///     let mut op_reader = body.get_operators_reader().expect("op reader");
    ///     let ops = op_reader.into_iter().collect::<Result<Vec<Operator>>>().expect("ops");
    ///     assert!(
    ///         if let [Operator::Nop, Operator::End] = ops.as_slice() { true } else { false },
    ///         "found {:?}",
    ///         ops
    ///     );
    /// }
    /// ```
    fn into_iter(self) -> Self::IntoIter {
        OperatorsIterator {
            reader: self,
            err: false,
        }
    }
}

/// An iterator over a function's operators.
pub struct OperatorsIterator<'a> {
    reader: OperatorsReader<'a>,
    err: bool,
}

impl<'a> Iterator for OperatorsIterator<'a> {
    type Item = Result<Operator<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.err || self.reader.eof() {
            return None;
        }
        let result = self.reader.read();
        self.err = result.is_err();
        Some(result)
    }
}

/// An iterator over a function's operators with offsets.
pub struct OperatorsIteratorWithOffsets<'a> {
    reader: OperatorsReader<'a>,
    err: bool,
}

impl<'a> Iterator for OperatorsIteratorWithOffsets<'a> {
    type Item = Result<(Operator<'a>, usize)>;

    /// Reads content of the code section with offsets.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::{Operator, CodeSectionReader, Result, BinaryReader};
    /// # let data: &[u8] = &[
    /// #     0x01, 0x03, 0x00, /* offset = 23 */ 0x01, 0x0b];
    /// let reader = BinaryReader::new(data, 20);
    /// let code_reader = CodeSectionReader::new(reader).unwrap();
    /// for body in code_reader {
    ///     let body = body.expect("function body");
    ///     let mut op_reader = body.get_operators_reader().expect("op reader");
    ///     let ops = op_reader.into_iter_with_offsets().collect::<Result<Vec<(Operator, usize)>>>().expect("ops");
    ///     assert!(
    ///         if let [(Operator::Nop, 23), (Operator::End, 24)] = ops.as_slice() { true } else { false },
    ///         "found {:?}",
    ///         ops
    ///     );
    /// }
    /// ```
    fn next(&mut self) -> Option<Self::Item> {
        if self.err || self.reader.eof() {
            return None;
        }
        let result = self.reader.read_with_offset();
        self.err = result.is_err();
        Some(result)
    }
}

macro_rules! define_visit_operator {
    ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($ann:tt)*))*) => {
        $(
            fn $visit(&mut self $($(,$arg: $argty)*)?) -> Self::Output;
        )*
    }
}

/// Trait implemented by types that can visit all [`Operator`] variants.
#[allow(missing_docs)]
pub trait VisitOperator<'a> {
    /// The result type of the visitor.
    type Output: 'a;

    /// Visits the [`Operator`] `op` using the given `offset`.
    ///
    /// # Note
    ///
    /// This is a convenience method that is intended for non-performance
    /// critical use cases. For performance critical implementations users
    /// are recommended to directly use the respective `visit` methods or
    /// implement [`VisitOperator`] on their own.
    fn visit_operator(&mut self, op: &Operator<'a>) -> Self::Output {
        macro_rules! visit_operator {
            ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($ann:tt)*))*) => {{
                match op {
                    $( Operator::$op $({ $($arg),* })? => self.$visit($($($arg.clone()),*)?), )*
                    #[cfg(feature = "simd")]
                    other => visit_simd_operator(self, other),
                }
            }};
        }
        crate::for_each_visit_operator!(visit_operator)
    }

    /// Returns a mutable reference to a [`VisitSimdOperator`] visitor.
    ///
    /// - If an implementer does _not_ want to support Wasm `simd` proposal
    ///   nothing has to be done since the default implementation already suffices.
    /// - If an implementer _does_ want to support Wasm `simd` proposal this
    ///   method usually is implemented as `Some(self)` where the implementing
    ///   type (`Self`) typically also implements `VisitSimdOperator`.
    ///
    /// # Example
    ///
    /// ```
    /// # macro_rules! define_visit_operator {
    /// #     ($( @$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($ann:tt)*))*) => {
    /// #         $( fn $visit(&mut self $($(,$arg: $argty)*)?) {} )*
    /// #     }
    /// # }
    /// # use wasmparser::{VisitOperator, VisitSimdOperator};
    /// pub struct MyVisitor;
    ///
    /// impl<'a> VisitOperator<'a> for MyVisitor {
    ///     type Output = ();
    ///
    ///     fn simd_visitor(&mut self) -> Option<&mut dyn VisitSimdOperator<'a, Output = Self::Output>> {
    ///         Some(self)
    ///     }
    ///
    ///     // implement remaining visitation methods here ...
    ///     # wasmparser::for_each_visit_operator!(define_visit_operator);
    /// }
    ///
    /// impl VisitSimdOperator<'_> for MyVisitor {
    ///     // implement SIMD visitation methods here ...
    ///     # wasmparser::for_each_visit_simd_operator!(define_visit_operator);
    /// }
    /// ```
    #[cfg(feature = "simd")]
    fn simd_visitor(&mut self) -> Option<&mut dyn VisitSimdOperator<'a, Output = Self::Output>> {
        None
    }

    crate::for_each_visit_operator!(define_visit_operator);
}

/// Special handler for visiting `simd` and `relaxed-simd` [`Operator`] variants.
#[cfg(feature = "simd")]
fn visit_simd_operator<'a, V>(visitor: &mut V, op: &Operator<'a>) -> V::Output
where
    V: VisitOperator<'a> + ?Sized,
{
    let Some(simd_visitor) = visitor.simd_visitor() else {
        panic!("missing SIMD visitor to visit operator: {op:?}")
    };
    macro_rules! visit_simd_operator {
        ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($ann:tt)*))*) => {{
            match op {
                $( Operator::$op $({ $($arg),* })? => simd_visitor.$visit($($($arg.clone()),*)?), )*
                unexpected => unreachable!("unexpected non-SIMD operator: {unexpected:?}"),
            }
        }};
    }
    crate::for_each_visit_simd_operator!(visit_simd_operator)
}

/// Trait implemented by types that can visit all Wasm `simd` and `relaxed-simd` [`Operator`]s.
#[cfg(feature = "simd")]
#[allow(missing_docs)]
pub trait VisitSimdOperator<'a>: VisitOperator<'a> {
    crate::for_each_visit_simd_operator!(define_visit_operator);
}

macro_rules! define_visit_operator_delegate {
    ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($ann:tt)*))*) => {
        $(
            fn $visit(&mut self $($(,$arg: $argty)*)?) -> Self::Output {
                V::$visit(&mut *self, $($($arg),*)?)
            }
        )*
    }
}

impl<'a, 'b, V: VisitOperator<'a> + ?Sized> VisitOperator<'a> for &'b mut V {
    type Output = V::Output;
    fn visit_operator(&mut self, op: &Operator<'a>) -> Self::Output {
        V::visit_operator(*self, op)
    }
    #[cfg(feature = "simd")]
    fn simd_visitor(&mut self) -> Option<&mut dyn VisitSimdOperator<'a, Output = V::Output>> {
        V::simd_visitor(*self)
    }
    crate::for_each_visit_operator!(define_visit_operator_delegate);
}

#[cfg(feature = "simd")]
impl<'a, 'b, V: VisitSimdOperator<'a> + ?Sized> VisitSimdOperator<'a> for &'b mut V {
    crate::for_each_visit_simd_operator!(define_visit_operator_delegate);
}

impl<'a, V: VisitOperator<'a> + ?Sized> VisitOperator<'a> for Box<V> {
    type Output = V::Output;
    fn visit_operator(&mut self, op: &Operator<'a>) -> Self::Output {
        V::visit_operator(&mut *self, op)
    }
    #[cfg(feature = "simd")]
    fn simd_visitor(&mut self) -> Option<&mut dyn VisitSimdOperator<'a, Output = V::Output>> {
        V::simd_visitor(&mut *self)
    }
    crate::for_each_visit_operator!(define_visit_operator_delegate);
}

#[cfg(feature = "simd")]
impl<'a, V: VisitSimdOperator<'a> + ?Sized> VisitSimdOperator<'a> for Box<V> {
    crate::for_each_visit_simd_operator!(define_visit_operator_delegate);
}

/// A `try_table` entries representation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TryTable {
    /// The block type describing the try block itself.
    pub ty: BlockType,
    /// Outer blocks which will receive exceptions.
    pub catches: Vec<Catch>,
}

/// Catch clauses that can be specified in [`TryTable`].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum Catch {
    /// Equivalent of `catch`
    One { tag: u32, label: u32 },
    /// Equivalent of `catch_ref`
    OneRef { tag: u32, label: u32 },
    /// Equivalent of `catch_all`
    All { label: u32 },
    /// Equivalent of `catch_all_ref`
    AllRef { label: u32 },
}

impl<'a> FromReader<'a> for TryTable {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let ty = reader.read_block_type()?;
        let catches = reader
            .read_iter(MAX_WASM_CATCHES, "catches")?
            .collect::<Result<_>>()?;
        Ok(TryTable { ty, catches })
    }
}

impl<'a> FromReader<'a> for Catch {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            0x00 => Catch::One {
                tag: reader.read_var_u32()?,
                label: reader.read_var_u32()?,
            },
            0x01 => Catch::OneRef {
                tag: reader.read_var_u32()?,
                label: reader.read_var_u32()?,
            },
            0x02 => Catch::All {
                label: reader.read_var_u32()?,
            },
            0x03 => Catch::AllRef {
                label: reader.read_var_u32()?,
            },

            x => return reader.invalid_leading_byte(x, "catch"),
        })
    }
}

/// A representation of dispatch tables on `resume` and `resume_throw`
/// instructions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResumeTable {
    /// Either the outer blocks which will handle suspensions or
    /// "switch-to" handlers.
    pub handlers: Vec<Handle>,
}

/// Handle clauses that can be specified in [`ResumeTable`].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum Handle {
    /// Equivalent of `(on $tag $lbl)`.
    OnLabel { tag: u32, label: u32 },
    /// Equivalent of `(on $tag switch)`.
    OnSwitch { tag: u32 },
}

impl ResumeTable {
    /// Returns the number of entries in the table.
    pub fn len(&self) -> usize {
        self.handlers.len()
    }
}

impl<'a> FromReader<'a> for ResumeTable {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let handlers = reader
            .read_iter(MAX_WASM_HANDLERS, "resume table")?
            .collect::<Result<_>>()?;
        let table = ResumeTable { handlers };
        Ok(table)
    }
}

impl<'a> FromReader<'a> for Handle {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            0x00 => Handle::OnLabel {
                tag: reader.read_var_u32()?,
                label: reader.read_var_u32()?,
            },
            0x01 => Handle::OnSwitch {
                tag: reader.read_var_u32()?,
            },
            x => return reader.invalid_leading_byte(x, "on clause"),
        })
    }
}

/// A factory to construct [`Operator`] instances via the [`VisitOperator`] trait.
struct OperatorFactory<'a> {
    marker: core::marker::PhantomData<fn() -> &'a ()>,
}

impl<'a> OperatorFactory<'a> {
    /// Creates a new [`OperatorFactory`].
    fn new() -> Self {
        Self {
            marker: core::marker::PhantomData,
        }
    }
}

macro_rules! define_visit_operator {
    ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($ann:tt)*))*) => {
        $(
            fn $visit(&mut self $($(,$arg: $argty)*)?) -> Operator<'a> {
                Operator::$op $({ $($arg),* })?
            }
        )*
    }
}

impl<'a> VisitOperator<'a> for OperatorFactory<'a> {
    type Output = Operator<'a>;

    #[cfg(feature = "simd")]
    fn simd_visitor(&mut self) -> Option<&mut dyn VisitSimdOperator<'a, Output = Self::Output>> {
        Some(self)
    }

    crate::for_each_visit_operator!(define_visit_operator);
}

#[cfg(feature = "simd")]
impl<'a> VisitSimdOperator<'a> for OperatorFactory<'a> {
    crate::for_each_visit_simd_operator!(define_visit_operator);
}
