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

use crate::{BinaryReader, BinaryReaderError, Result, ValType};

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

/// Represents a memory immediate in a WebAssembly memory instruction.
#[derive(Debug, Copy, Clone)]
pub struct MemoryImmediate {
    /// Alignment, stored as `n` where the actual alignment is `2^n`
    pub align: u8,
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

/// Represents a SIMD lane index.
pub type SIMDLaneIndex = u8;

/// Instructions as defined [here].
///
/// [here]: https://webassembly.github.io/spec/core/binary/instructions.html
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Operator<'a> {
    Unreachable,
    Nop,
    Block {
        ty: BlockType,
    },
    Loop {
        ty: BlockType,
    },
    If {
        ty: BlockType,
    },
    Else,
    Try {
        ty: BlockType,
    },
    Catch {
        index: u32,
    },
    Throw {
        index: u32,
    },
    Rethrow {
        relative_depth: u32,
    },
    End,
    Br {
        relative_depth: u32,
    },
    BrIf {
        relative_depth: u32,
    },
    BrTable {
        table: BrTable<'a>,
    },
    Return,
    Call {
        function_index: u32,
    },
    CallIndirect {
        index: u32,
        table_index: u32,
        table_byte: u8,
    },
    ReturnCall {
        function_index: u32,
    },
    ReturnCallIndirect {
        index: u32,
        table_index: u32,
    },
    Delegate {
        relative_depth: u32,
    },
    CatchAll,
    Drop,
    Select,
    TypedSelect {
        ty: ValType,
    },
    LocalGet {
        local_index: u32,
    },
    LocalSet {
        local_index: u32,
    },
    LocalTee {
        local_index: u32,
    },
    GlobalGet {
        global_index: u32,
    },
    GlobalSet {
        global_index: u32,
    },
    I32Load {
        memarg: MemoryImmediate,
    },
    I64Load {
        memarg: MemoryImmediate,
    },
    F32Load {
        memarg: MemoryImmediate,
    },
    F64Load {
        memarg: MemoryImmediate,
    },
    I32Load8S {
        memarg: MemoryImmediate,
    },
    I32Load8U {
        memarg: MemoryImmediate,
    },
    I32Load16S {
        memarg: MemoryImmediate,
    },
    I32Load16U {
        memarg: MemoryImmediate,
    },
    I64Load8S {
        memarg: MemoryImmediate,
    },
    I64Load8U {
        memarg: MemoryImmediate,
    },
    I64Load16S {
        memarg: MemoryImmediate,
    },
    I64Load16U {
        memarg: MemoryImmediate,
    },
    I64Load32S {
        memarg: MemoryImmediate,
    },
    I64Load32U {
        memarg: MemoryImmediate,
    },
    I32Store {
        memarg: MemoryImmediate,
    },
    I64Store {
        memarg: MemoryImmediate,
    },
    F32Store {
        memarg: MemoryImmediate,
    },
    F64Store {
        memarg: MemoryImmediate,
    },
    I32Store8 {
        memarg: MemoryImmediate,
    },
    I32Store16 {
        memarg: MemoryImmediate,
    },
    I64Store8 {
        memarg: MemoryImmediate,
    },
    I64Store16 {
        memarg: MemoryImmediate,
    },
    I64Store32 {
        memarg: MemoryImmediate,
    },
    MemorySize {
        mem: u32,
        mem_byte: u8,
    },
    MemoryGrow {
        mem: u32,
        mem_byte: u8,
    },
    I32Const {
        value: i32,
    },
    I64Const {
        value: i64,
    },
    F32Const {
        value: Ieee32,
    },
    F64Const {
        value: Ieee64,
    },
    RefNull {
        ty: ValType,
    },
    RefIsNull,
    RefFunc {
        function_index: u32,
    },
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,
    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,
    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,
    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,

    // 0xFC operators
    // Non-trapping Float-to-int Conversions
    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,

    // 0xFC operators
    // bulk memory https://github.com/WebAssembly/bulk-memory-operations/blob/master/proposals/bulk-memory-operations/Overview.md
    MemoryInit {
        segment: u32,
        mem: u32,
    },
    DataDrop {
        segment: u32,
    },
    MemoryCopy {
        src: u32,
        dst: u32,
    },
    MemoryFill {
        mem: u32,
    },
    TableInit {
        segment: u32,
        table: u32,
    },
    ElemDrop {
        segment: u32,
    },
    TableCopy {
        dst_table: u32,
        src_table: u32,
    },
    TableFill {
        table: u32,
    },
    TableGet {
        table: u32,
    },
    TableSet {
        table: u32,
    },
    TableGrow {
        table: u32,
    },
    TableSize {
        table: u32,
    },

    // 0xFE operators
    // https://github.com/WebAssembly/threads/blob/master/proposals/threads/Overview.md
    MemoryAtomicNotify {
        memarg: MemoryImmediate,
    },
    MemoryAtomicWait32 {
        memarg: MemoryImmediate,
    },
    MemoryAtomicWait64 {
        memarg: MemoryImmediate,
    },
    AtomicFence {
        flags: u8,
    },
    I32AtomicLoad {
        memarg: MemoryImmediate,
    },
    I64AtomicLoad {
        memarg: MemoryImmediate,
    },
    I32AtomicLoad8U {
        memarg: MemoryImmediate,
    },
    I32AtomicLoad16U {
        memarg: MemoryImmediate,
    },
    I64AtomicLoad8U {
        memarg: MemoryImmediate,
    },
    I64AtomicLoad16U {
        memarg: MemoryImmediate,
    },
    I64AtomicLoad32U {
        memarg: MemoryImmediate,
    },
    I32AtomicStore {
        memarg: MemoryImmediate,
    },
    I64AtomicStore {
        memarg: MemoryImmediate,
    },
    I32AtomicStore8 {
        memarg: MemoryImmediate,
    },
    I32AtomicStore16 {
        memarg: MemoryImmediate,
    },
    I64AtomicStore8 {
        memarg: MemoryImmediate,
    },
    I64AtomicStore16 {
        memarg: MemoryImmediate,
    },
    I64AtomicStore32 {
        memarg: MemoryImmediate,
    },
    I32AtomicRmwAdd {
        memarg: MemoryImmediate,
    },
    I64AtomicRmwAdd {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw8AddU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw16AddU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw8AddU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw16AddU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw32AddU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmwSub {
        memarg: MemoryImmediate,
    },
    I64AtomicRmwSub {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw8SubU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw16SubU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw8SubU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw16SubU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw32SubU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmwAnd {
        memarg: MemoryImmediate,
    },
    I64AtomicRmwAnd {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw8AndU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw16AndU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw8AndU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw16AndU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw32AndU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmwOr {
        memarg: MemoryImmediate,
    },
    I64AtomicRmwOr {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw8OrU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw16OrU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw8OrU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw16OrU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw32OrU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmwXor {
        memarg: MemoryImmediate,
    },
    I64AtomicRmwXor {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw8XorU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw16XorU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw8XorU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw16XorU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw32XorU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmwXchg {
        memarg: MemoryImmediate,
    },
    I64AtomicRmwXchg {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw8XchgU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw16XchgU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw8XchgU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw16XchgU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw32XchgU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmwCmpxchg {
        memarg: MemoryImmediate,
    },
    I64AtomicRmwCmpxchg {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw8CmpxchgU {
        memarg: MemoryImmediate,
    },
    I32AtomicRmw16CmpxchgU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw8CmpxchgU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw16CmpxchgU {
        memarg: MemoryImmediate,
    },
    I64AtomicRmw32CmpxchgU {
        memarg: MemoryImmediate,
    },

    // 0xFD operators
    // SIMD https://webassembly.github.io/simd/core/binary/instructions.html
    V128Load {
        memarg: MemoryImmediate,
    },
    V128Load8x8S {
        memarg: MemoryImmediate,
    },
    V128Load8x8U {
        memarg: MemoryImmediate,
    },
    V128Load16x4S {
        memarg: MemoryImmediate,
    },
    V128Load16x4U {
        memarg: MemoryImmediate,
    },
    V128Load32x2S {
        memarg: MemoryImmediate,
    },
    V128Load32x2U {
        memarg: MemoryImmediate,
    },
    V128Load8Splat {
        memarg: MemoryImmediate,
    },
    V128Load16Splat {
        memarg: MemoryImmediate,
    },
    V128Load32Splat {
        memarg: MemoryImmediate,
    },
    V128Load64Splat {
        memarg: MemoryImmediate,
    },
    V128Load32Zero {
        memarg: MemoryImmediate,
    },
    V128Load64Zero {
        memarg: MemoryImmediate,
    },
    V128Store {
        memarg: MemoryImmediate,
    },
    V128Load8Lane {
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    },
    V128Load16Lane {
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    },
    V128Load32Lane {
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    },
    V128Load64Lane {
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    },
    V128Store8Lane {
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    },
    V128Store16Lane {
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    },
    V128Store32Lane {
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    },
    V128Store64Lane {
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    },
    V128Const {
        value: V128,
    },
    I8x16Shuffle {
        lanes: [SIMDLaneIndex; 16],
    },
    I8x16ExtractLaneS {
        lane: SIMDLaneIndex,
    },
    I8x16ExtractLaneU {
        lane: SIMDLaneIndex,
    },
    I8x16ReplaceLane {
        lane: SIMDLaneIndex,
    },
    I16x8ExtractLaneS {
        lane: SIMDLaneIndex,
    },
    I16x8ExtractLaneU {
        lane: SIMDLaneIndex,
    },
    I16x8ReplaceLane {
        lane: SIMDLaneIndex,
    },
    I32x4ExtractLane {
        lane: SIMDLaneIndex,
    },
    I32x4ReplaceLane {
        lane: SIMDLaneIndex,
    },
    I64x2ExtractLane {
        lane: SIMDLaneIndex,
    },
    I64x2ReplaceLane {
        lane: SIMDLaneIndex,
    },
    F32x4ExtractLane {
        lane: SIMDLaneIndex,
    },
    F32x4ReplaceLane {
        lane: SIMDLaneIndex,
    },
    F64x2ExtractLane {
        lane: SIMDLaneIndex,
    },
    F64x2ReplaceLane {
        lane: SIMDLaneIndex,
    },
    I8x16Swizzle,
    I8x16Splat,
    I16x8Splat,
    I32x4Splat,
    I64x2Splat,
    F32x4Splat,
    F64x2Splat,
    I8x16Eq,
    I8x16Ne,
    I8x16LtS,
    I8x16LtU,
    I8x16GtS,
    I8x16GtU,
    I8x16LeS,
    I8x16LeU,
    I8x16GeS,
    I8x16GeU,
    I16x8Eq,
    I16x8Ne,
    I16x8LtS,
    I16x8LtU,
    I16x8GtS,
    I16x8GtU,
    I16x8LeS,
    I16x8LeU,
    I16x8GeS,
    I16x8GeU,
    I32x4Eq,
    I32x4Ne,
    I32x4LtS,
    I32x4LtU,
    I32x4GtS,
    I32x4GtU,
    I32x4LeS,
    I32x4LeU,
    I32x4GeS,
    I32x4GeU,
    I64x2Eq,
    I64x2Ne,
    I64x2LtS,
    I64x2GtS,
    I64x2LeS,
    I64x2GeS,
    F32x4Eq,
    F32x4Ne,
    F32x4Lt,
    F32x4Gt,
    F32x4Le,
    F32x4Ge,
    F64x2Eq,
    F64x2Ne,
    F64x2Lt,
    F64x2Gt,
    F64x2Le,
    F64x2Ge,
    V128Not,
    V128And,
    V128AndNot,
    V128Or,
    V128Xor,
    V128Bitselect,
    V128AnyTrue,
    I8x16Abs,
    I8x16Neg,
    I8x16Popcnt,
    I8x16AllTrue,
    I8x16Bitmask,
    I8x16NarrowI16x8S,
    I8x16NarrowI16x8U,
    I8x16Shl,
    I8x16ShrS,
    I8x16ShrU,
    I8x16Add,
    I8x16AddSatS,
    I8x16AddSatU,
    I8x16Sub,
    I8x16SubSatS,
    I8x16SubSatU,
    I8x16MinS,
    I8x16MinU,
    I8x16MaxS,
    I8x16MaxU,
    I8x16RoundingAverageU,
    I16x8ExtAddPairwiseI8x16S,
    I16x8ExtAddPairwiseI8x16U,
    I16x8Abs,
    I16x8Neg,
    I16x8Q15MulrSatS,
    I16x8AllTrue,
    I16x8Bitmask,
    I16x8NarrowI32x4S,
    I16x8NarrowI32x4U,
    I16x8ExtendLowI8x16S,
    I16x8ExtendHighI8x16S,
    I16x8ExtendLowI8x16U,
    I16x8ExtendHighI8x16U,
    I16x8Shl,
    I16x8ShrS,
    I16x8ShrU,
    I16x8Add,
    I16x8AddSatS,
    I16x8AddSatU,
    I16x8Sub,
    I16x8SubSatS,
    I16x8SubSatU,
    I16x8Mul,
    I16x8MinS,
    I16x8MinU,
    I16x8MaxS,
    I16x8MaxU,
    I16x8RoundingAverageU,
    I16x8ExtMulLowI8x16S,
    I16x8ExtMulHighI8x16S,
    I16x8ExtMulLowI8x16U,
    I16x8ExtMulHighI8x16U,
    I32x4ExtAddPairwiseI16x8S,
    I32x4ExtAddPairwiseI16x8U,
    I32x4Abs,
    I32x4Neg,
    I32x4AllTrue,
    I32x4Bitmask,
    I32x4ExtendLowI16x8S,
    I32x4ExtendHighI16x8S,
    I32x4ExtendLowI16x8U,
    I32x4ExtendHighI16x8U,
    I32x4Shl,
    I32x4ShrS,
    I32x4ShrU,
    I32x4Add,
    I32x4Sub,
    I32x4Mul,
    I32x4MinS,
    I32x4MinU,
    I32x4MaxS,
    I32x4MaxU,
    I32x4DotI16x8S,
    I32x4ExtMulLowI16x8S,
    I32x4ExtMulHighI16x8S,
    I32x4ExtMulLowI16x8U,
    I32x4ExtMulHighI16x8U,
    I64x2Abs,
    I64x2Neg,
    I64x2AllTrue,
    I64x2Bitmask,
    I64x2ExtendLowI32x4S,
    I64x2ExtendHighI32x4S,
    I64x2ExtendLowI32x4U,
    I64x2ExtendHighI32x4U,
    I64x2Shl,
    I64x2ShrS,
    I64x2ShrU,
    I64x2Add,
    I64x2Sub,
    I64x2Mul,
    I64x2ExtMulLowI32x4S,
    I64x2ExtMulHighI32x4S,
    I64x2ExtMulLowI32x4U,
    I64x2ExtMulHighI32x4U,
    F32x4Ceil,
    F32x4Floor,
    F32x4Trunc,
    F32x4Nearest,
    F32x4Abs,
    F32x4Neg,
    F32x4Sqrt,
    F32x4Add,
    F32x4Sub,
    F32x4Mul,
    F32x4Div,
    F32x4Min,
    F32x4Max,
    F32x4PMin,
    F32x4PMax,
    F64x2Ceil,
    F64x2Floor,
    F64x2Trunc,
    F64x2Nearest,
    F64x2Abs,
    F64x2Neg,
    F64x2Sqrt,
    F64x2Add,
    F64x2Sub,
    F64x2Mul,
    F64x2Div,
    F64x2Min,
    F64x2Max,
    F64x2PMin,
    F64x2PMax,
    I32x4TruncSatF32x4S,
    I32x4TruncSatF32x4U,
    F32x4ConvertI32x4S,
    F32x4ConvertI32x4U,
    I32x4TruncSatF64x2SZero,
    I32x4TruncSatF64x2UZero,
    F64x2ConvertLowI32x4S,
    F64x2ConvertLowI32x4U,
    F32x4DemoteF64x2Zero,
    F64x2PromoteLowF32x4,
    I8x16RelaxedSwizzle,
    I32x4RelaxedTruncSatF32x4S,
    I32x4RelaxedTruncSatF32x4U,
    I32x4RelaxedTruncSatF64x2SZero,
    I32x4RelaxedTruncSatF64x2UZero,
    F32x4Fma,
    F32x4Fms,
    F64x2Fma,
    F64x2Fms,
    I8x16LaneSelect,
    I16x8LaneSelect,
    I32x4LaneSelect,
    I64x2LaneSelect,
    F32x4RelaxedMin,
    F32x4RelaxedMax,
    F64x2RelaxedMin,
    F64x2RelaxedMax,
}

/// A reader for a core WebAssembly function's operators.
#[derive(Clone)]
pub struct OperatorsReader<'a> {
    pub(crate) reader: BinaryReader<'a>,
}

impl<'a> OperatorsReader<'a> {
    pub(crate) fn new<'b>(data: &'a [u8], offset: usize) -> OperatorsReader<'b>
    where
        'a: 'b,
    {
        OperatorsReader {
            reader: BinaryReader::new_with_offset(data, offset),
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

    /// Whether or not to allow 64-bit memory arguments in the
    /// the operators being read.
    ///
    /// This is intended to be `true` when support for the memory64
    /// WebAssembly proposal is also enabled.
    pub fn allow_memarg64(&mut self, allow: bool) {
        self.reader.allow_memarg64(allow);
    }

    /// Ensures the reader is at the end.
    ///
    /// This function returns an error if there is extra data after the operators.
    pub fn ensure_end(&self) -> Result<()> {
        if self.eof() {
            return Ok(());
        }
        Err(BinaryReaderError::new(
            "unexpected data at the end of operators",
            self.reader.original_position(),
        ))
    }

    /// Reads an operator from the reader.
    pub fn read<'b>(&mut self) -> Result<Operator<'b>>
    where
        'a: 'b,
    {
        self.reader.read_operator()
    }

    /// Converts to an iterator of operators paired with offsets.
    pub fn into_iter_with_offsets<'b>(self) -> OperatorsIteratorWithOffsets<'b>
    where
        'a: 'b,
    {
        OperatorsIteratorWithOffsets {
            reader: self,
            err: false,
        }
    }

    /// Reads an operator with its offset.
    pub fn read_with_offset<'b>(&mut self) -> Result<(Operator<'b>, usize)>
    where
        'a: 'b,
    {
        let pos = self.reader.original_position();
        Ok((self.read()?, pos))
    }

    /// Visits an operator with its offset.
    pub fn visit_with_offset<T>(
        &mut self,
        visitor: &mut T,
    ) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        self.reader.visit_operator(visitor)
    }

    /// Gets a binary reader from this operators reader.
    pub fn get_binary_reader(&self) -> BinaryReader<'a> {
        self.reader.clone()
    }
}

impl<'a> IntoIterator for OperatorsReader<'a> {
    type Item = Result<Operator<'a>>;
    type IntoIter = OperatorsIterator<'a>;

    /// Reads content of the code section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::{Operator, CodeSectionReader, Result};
    /// # let data: &[u8] = &[
    /// #     0x01, 0x03, 0x00, 0x01, 0x0b];
    /// let mut code_reader = CodeSectionReader::new(data, 0).unwrap();
    /// for _ in 0..code_reader.get_count() {
    ///     let body = code_reader.read().expect("function body");
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
    /// use wasmparser::{Operator, CodeSectionReader, Result};
    /// # let data: &[u8] = &[
    /// #     0x01, 0x03, 0x00, /* offset = 23 */ 0x01, 0x0b];
    /// let mut code_reader = CodeSectionReader::new(data, 20).unwrap();
    /// for _ in 0..code_reader.get_count() {
    ///     let body = code_reader.read().expect("function body");
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
    #[rustfmt::skip]
    fn visit_operator(&mut self, offset: usize, op: &Operator<'a>) -> Self::Output {
        match *op {
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
            Operator::MemoryCopy { src, dst } => self.visit_memory_copy(offset, dst, src),
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

    fn visit_nop(&mut self, offset: usize) -> Self::Output;
    fn visit_unreachable(&mut self, offset: usize) -> Self::Output;
    fn visit_block(&mut self, offset: usize, ty: BlockType) -> Self::Output;
    fn visit_loop(&mut self, offset: usize, ty: BlockType) -> Self::Output;
    fn visit_if(&mut self, offset: usize, ty: BlockType) -> Self::Output;
    fn visit_else(&mut self, offset: usize) -> Self::Output;
    fn visit_try(&mut self, offset: usize, ty: BlockType) -> Self::Output;
    fn visit_catch(&mut self, offset: usize, index: u32) -> Self::Output;
    fn visit_throw(&mut self, offset: usize, index: u32) -> Self::Output;
    fn visit_rethrow(&mut self, offset: usize, relative_depth: u32) -> Self::Output;
    fn visit_delegate(&mut self, offset: usize, relative_depth: u32) -> Self::Output;
    fn visit_catch_all(&mut self, offset: usize) -> Self::Output;
    fn visit_end(&mut self, offset: usize) -> Self::Output;
    fn visit_br(&mut self, offset: usize, relative_depth: u32) -> Self::Output;
    fn visit_br_if(&mut self, offset: usize, relative_depth: u32) -> Self::Output;
    fn visit_br_table(&mut self, offset: usize, table: &BrTable<'a>) -> Self::Output;
    fn visit_return(&mut self, offset: usize) -> Self::Output;
    fn visit_call(&mut self, offset: usize, function_index: u32) -> Self::Output;
    fn visit_return_call(&mut self, offset: usize, function_index: u32) -> Self::Output;
    fn visit_call_indirect(
        &mut self,
        offset: usize,
        index: u32,
        table_index: u32,
        table_byte: u8,
    ) -> Self::Output;
    fn visit_return_call_indirect(
        &mut self,
        offset: usize,
        index: u32,
        table_index: u32,
    ) -> Self::Output;
    fn visit_drop(&mut self, offset: usize) -> Self::Output;
    fn visit_select(&mut self, offset: usize) -> Self::Output;
    fn visit_typed_select(&mut self, offset: usize, ty: ValType) -> Self::Output;
    fn visit_local_get(&mut self, offset: usize, local_index: u32) -> Self::Output;
    fn visit_local_set(&mut self, offset: usize, local_index: u32) -> Self::Output;
    fn visit_local_tee(&mut self, offset: usize, local_index: u32) -> Self::Output;
    fn visit_global_get(&mut self, offset: usize, global_index: u32) -> Self::Output;
    fn visit_global_set(&mut self, offset: usize, global_index: u32) -> Self::Output;
    fn visit_i32_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_f32_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_f64_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load32_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load32_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_f32_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_f64_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_store32(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_memory_size(&mut self, offset: usize, mem: u32, mem_byte: u8) -> Self::Output;
    fn visit_memory_grow(&mut self, offset: usize, mem: u32, mem_byte: u8) -> Self::Output;
    fn visit_i32_const(&mut self, offset: usize, value: i32) -> Self::Output;
    fn visit_i64_const(&mut self, offset: usize, value: i64) -> Self::Output;
    fn visit_f32_const(&mut self, offset: usize, value: Ieee32) -> Self::Output;
    fn visit_f64_const(&mut self, offset: usize, value: Ieee64) -> Self::Output;
    fn visit_i32_eqz(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_lt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_lt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_gt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_gt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_le_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_le_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_ge_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_ge_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_eqz(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_lt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_lt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_gt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_gt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_le_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_le_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_ge_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_ge_u(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_lt(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_gt(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_le(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_ge(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_lt(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_gt(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_le(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_ge(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_clz(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_ctz(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_popcnt(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_add(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_div_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_div_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_rem_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_rem_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_and(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_or(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_xor(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_shl(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_shr_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_shr_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_rotl(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_rotr(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_clz(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_ctz(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_popcnt(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_add(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_div_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_div_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_rem_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_rem_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_and(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_or(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_xor(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_shl(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_shr_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_shr_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_rotl(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_rotr(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_abs(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_neg(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_ceil(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_floor(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_trunc(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_nearest(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_sqrt(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_add(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_div(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_min(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_max(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_copysign(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_abs(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_neg(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_ceil(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_floor(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_trunc(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_nearest(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_sqrt(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_add(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_div(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_min(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_max(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_copysign(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_wrap_i64(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_trunc_f32s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_trunc_f32u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_trunc_f64s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_trunc_f64u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_extend_i32s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_extend_i32u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_trunc_f32s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_trunc_f32u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_trunc_f64s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_trunc_f64u(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_convert_i32s(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_convert_i32u(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_convert_i64s(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_convert_i64u(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_demote_f64(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_convert_i32s(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_convert_i32u(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_convert_i64s(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_convert_i64u(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_promote_f32(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_reinterpret_f32(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_reinterpret_f64(&mut self, offset: usize) -> Self::Output;
    fn visit_f32_reinterpret_i32(&mut self, offset: usize) -> Self::Output;
    fn visit_f64_reinterpret_i64(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_trunc_sat_f32s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_trunc_sat_f32u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_trunc_sat_f64s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_trunc_sat_f64u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_trunc_sat_f32s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_trunc_sat_f32u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_trunc_sat_f64s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_trunc_sat_f64u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_extend8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_extend16_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_extend8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_extend16_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64_extend32_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_load16_u(&mut self, offset: usize, memarg: MemoryImmediate)
        -> Self::Output;
    fn visit_i32_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_load32_u(&mut self, offset: usize, memarg: MemoryImmediate)
        -> Self::Output;
    fn visit_i64_atomic_load16_u(&mut self, offset: usize, memarg: MemoryImmediate)
        -> Self::Output;
    fn visit_i64_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_store32(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw16_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw16_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw16_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw16_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw16_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw8_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw8_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw8_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw8_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw8_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw32_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw32_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw32_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw32_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw32_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw16_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw16_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw16_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw16_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw16_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw8_add_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw8_sub_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw8_and_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw8_or_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw8_xor_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw_xchg(&mut self, offset: usize, memarg: MemoryImmediate)
        -> Self::Output;
    fn visit_i32_atomic_rmw16_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw8_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw_cmpxchg(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw16_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i32_atomic_rmw8_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw_xchg(&mut self, offset: usize, memarg: MemoryImmediate)
        -> Self::Output;
    fn visit_i64_atomic_rmw32_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw16_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw8_xchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw_cmpxchg(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw32_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw16_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_i64_atomic_rmw8_cmpxchg_u(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_memory_atomic_notify(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_memory_atomic_wait32(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_memory_atomic_wait64(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
    ) -> Self::Output;
    fn visit_atomic_fence(&mut self, offset: usize, flags: u8) -> Self::Output;
    fn visit_ref_null(&mut self, offset: usize, ty: ValType) -> Self::Output;
    fn visit_ref_is_null(&mut self, offset: usize) -> Self::Output;
    fn visit_ref_func(&mut self, offset: usize, function_index: u32) -> Self::Output;
    fn visit_v128_load(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_store(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_const(&mut self, offset: usize, value: V128) -> Self::Output;
    fn visit_i8x16_splat(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_splat(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_splat(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_splat(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_splat(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_splat(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i8x16_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i16x8_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i16x8_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i8x16_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i16x8_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f32x4_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_lt(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_gt(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_le(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_ge(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_lt(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_gt(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_le(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_ge(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_add(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_div(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_min(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_max(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_p_min(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_p_max(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_add(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_div(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_min(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_max(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_p_min(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_p_max(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_relaxed_min(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_relaxed_max(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_relaxed_min(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_relaxed_max(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_lt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_lt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_gt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_gt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_le_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_le_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_ge_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_ge_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_lt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_lt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_gt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_gt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_le_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_le_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ge_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ge_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_lt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_lt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_gt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_gt_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_le_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_le_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ge_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ge_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_eq(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_ne(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_lt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_gt_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_le_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_ge_s(&mut self, offset: usize) -> Self::Output;
    fn visit_v128_and(&mut self, offset: usize) -> Self::Output;
    fn visit_v128_and_not(&mut self, offset: usize) -> Self::Output;
    fn visit_v128_or(&mut self, offset: usize) -> Self::Output;
    fn visit_v128_xor(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_add(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_add_sat_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_add_sat_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_sub_sat_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_sub_sat_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_min_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_min_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_max_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_max_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_add(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_add_sat_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_add_sat_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_sub_sat_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_sub_sat_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_min_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_min_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_max_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_max_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_add(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_min_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_min_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_max_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_max_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_dot_i16x8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_add(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_sub(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_mul(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_rounding_average_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_rounding_average_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_narrow_i16x8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_narrow_i16x8_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_narrow_i32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_narrow_i32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ext_mul_low_i8x16_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ext_mul_high_i8x16_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ext_mul_low_i8x16_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ext_mul_high_i8x16_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ext_mul_low_i16x8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ext_mul_high_i16x8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ext_mul_low_i16x8_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ext_mul_high_i16x8_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_ext_mul_low_i32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_ext_mul_high_i32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_ext_mul_low_i32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_ext_mul_high_i32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_q15_mulr_sat_s(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_ceil(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_floor(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_trunc(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_nearest(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_ceil(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_floor(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_trunc(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_nearest(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_abs(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_neg(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_sqrt(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_abs(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_neg(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_sqrt(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_demote_f64x2_zero(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_promote_low_f32x4(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_convert_low_i32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_convert_low_i32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_trunc_sat_f32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_trunc_sat_f32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_convert_i32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_convert_i32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_v128_not(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_abs(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_neg(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_popcnt(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_abs(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_neg(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_abs(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_neg(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_abs(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_neg(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_extend_low_i8x16_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_extend_high_i8x16_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_extend_low_i8x16_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_extend_high_i8x16_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_extend_low_i16x8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_extend_high_i16x8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_extend_low_i16x8_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_extend_high_i16x8_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_extend_low_i32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_extend_high_i32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_extend_low_i32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_extend_high_i32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ext_add_pairwise_i8x16_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_ext_add_pairwise_i8x16_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ext_add_pairwise_i16x8_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_ext_add_pairwise_i16x8_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, offset: usize) -> Self::Output;
    fn visit_v128_bitselect(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_fma(&mut self, offset: usize) -> Self::Output;
    fn visit_f32x4_fms(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_fma(&mut self, offset: usize) -> Self::Output;
    fn visit_f64x2_fms(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_lane_select(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_lane_select(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_lane_select(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_lane_select(&mut self, offset: usize) -> Self::Output;
    fn visit_v128_any_true(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_all_true(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_bitmask(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_all_true(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_bitmask(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_all_true(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_bitmask(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_all_true(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_bitmask(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_shl(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_shr_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_shr_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_shl(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_shr_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i16x8_shr_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_shl(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_shr_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i32x4_shr_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_shl(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_shr_s(&mut self, offset: usize) -> Self::Output;
    fn visit_i64x2_shr_u(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_swizzle(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_relaxed_swizzle(&mut self, offset: usize) -> Self::Output;
    fn visit_i8x16_shuffle(&mut self, offset: usize, lanes: [SIMDLaneIndex; 16]) -> Self::Output;
    fn visit_v128_load8_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load16_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load32_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load32_zero(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load64_splat(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load64_zero(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load8x8_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load8x8_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load16x4_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load16x4_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load32x2_s(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load32x2_u(&mut self, offset: usize, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load8_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output;
    fn visit_v128_load16_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output;
    fn visit_v128_load32_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output;
    fn visit_v128_load64_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output;
    fn visit_v128_store8_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output;
    fn visit_v128_store16_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output;
    fn visit_v128_store32_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output;
    fn visit_v128_store64_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) -> Self::Output;
    fn visit_memory_init(&mut self, offset: usize, segment: u32, mem: u32) -> Self::Output;
    fn visit_data_drop(&mut self, offset: usize, segment: u32) -> Self::Output;
    fn visit_memory_copy(&mut self, offset: usize, dst: u32, src: u32) -> Self::Output;
    fn visit_memory_fill(&mut self, offset: usize, mem: u32) -> Self::Output;
    fn visit_table_init(&mut self, offset: usize, segment: u32, table: u32) -> Self::Output;
    fn visit_elem_drop(&mut self, offset: usize, segment: u32) -> Self::Output;
    fn visit_table_copy(&mut self, offset: usize, dst_table: u32, src_table: u32) -> Self::Output;
    fn visit_table_get(&mut self, offset: usize, table: u32) -> Self::Output;
    fn visit_table_set(&mut self, offset: usize, table: u32) -> Self::Output;
    fn visit_table_grow(&mut self, offset: usize, table: u32) -> Self::Output;
    fn visit_table_size(&mut self, offset: usize, table: u32) -> Self::Output;
    fn visit_table_fill(&mut self, offset: usize, table: u32) -> Self::Output;
}
