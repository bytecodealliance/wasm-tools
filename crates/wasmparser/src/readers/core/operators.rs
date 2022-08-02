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
    pub fn visit_with_offset<T>(&mut self, visitor: &mut T) -> Result<<T as VisitOperator<usize>>::Output>
    where
        T: VisitOperator<usize>,
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
pub trait VisitOperator<Input> {
    /// The result type of the visitor.
    type Output;

    fn visit_nop(&mut self, input: Input) -> Self::Output;
    fn visit_unreachable(&mut self, input: Input) -> Self::Output;
    fn visit_block(&mut self, input: Input, ty: BlockType) -> Self::Output;
    fn visit_loop(&mut self, input: Input, ty: BlockType) -> Self::Output;
    fn visit_if(&mut self, input: Input, ty: BlockType) -> Self::Output;
    fn visit_else(&mut self, input: Input) -> Self::Output;
    fn visit_try(&mut self, input: Input, ty: BlockType) -> Self::Output;
    fn visit_catch(&mut self, input: Input, index: u32) -> Self::Output;
    fn visit_throw(&mut self, input: Input, index: u32) -> Self::Output;
    fn visit_rethrow(&mut self, input: Input, relative_depth: u32) -> Self::Output;
    fn visit_delegate(&mut self, input: Input, relative_depth: u32) -> Self::Output;
    fn visit_catch_all(&mut self, input: Input) -> Self::Output;
    fn visit_end(&mut self, input: Input) -> Self::Output;
    fn visit_br(&mut self, input: Input, relative_depth: u32) -> Self::Output;
    fn visit_br_if(&mut self, input: Input, relative_depth: u32) -> Self::Output;
    fn visit_br_table(&mut self, input: Input, table: BrTable) -> Self::Output;
    fn visit_return(&mut self, input: Input) -> Self::Output;
    fn visit_call(&mut self, input: Input, function_index: u32) -> Self::Output;
    fn visit_return_call(&mut self, input: Input, function_index: u32) -> Self::Output;
    fn visit_call_indirect(&mut self, input: Input, index: u32, table_index: u32, table_byte: u8) -> Self::Output;
    fn visit_return_call_indirect(&mut self, input: Input, index: u32, table_index: u32) -> Self::Output;
    fn visit_drop(&mut self, input: Input) -> Self::Output;
    fn visit_select(&mut self, input: Input) -> Self::Output;
    fn visit_typed_select(&mut self, input: Input, ty: ValType) -> Self::Output;
    fn visit_local_get(&mut self, input: Input, local_index: u32) -> Self::Output;
    fn visit_local_set(&mut self, input: Input, local_index: u32) -> Self::Output;
    fn visit_local_tee(&mut self, input: Input, local_index: u32) -> Self::Output;
    fn visit_global_get(&mut self, input: Input, global_index: u32) -> Self::Output;
    fn visit_global_set(&mut self, input: Input, global_index: u32) -> Self::Output;
    fn visit_i32_load(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_f32_load(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_f64_load(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_load8_s(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_load8_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_load16_s(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_load16_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load8_s(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load8_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load16_s(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load16_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load32_s(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_load32_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_store(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_store(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_f32_store(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_f64_store(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_store8(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_store16(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_store8(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_store16(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_store32(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_memory_size(&mut self, input: Input, mem: u32, mem_byte: u8) -> Self::Output;
    fn visit_memory_grow(&mut self, input: Input, mem: u32, mem_byte: u8) -> Self::Output;
    fn visit_i32_const(&mut self, input: Input, value: i32) -> Self::Output;
    fn visit_i64_const(&mut self, input: Input, value: i64) -> Self::Output;
    fn visit_f32_const(&mut self, input: Input, value: Ieee32) -> Self::Output;
    fn visit_f64_const(&mut self, input: Input, value: Ieee64) -> Self::Output;
    fn visit_i32_eqz(&mut self, input: Input) -> Self::Output;
    fn visit_i32_eq(&mut self, input: Input) -> Self::Output;
    fn visit_i32_ne(&mut self, input: Input) -> Self::Output;
    fn visit_i32_lt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_lt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_gt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_gt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_le_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_le_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_ge_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_ge_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_eqz(&mut self, input: Input) -> Self::Output;
    fn visit_i64_eq(&mut self, input: Input) -> Self::Output;
    fn visit_i64_ne(&mut self, input: Input) -> Self::Output;
    fn visit_i64_lt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_lt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_gt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_gt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_le_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_le_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_ge_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_ge_u(&mut self, input: Input) -> Self::Output;
    fn visit_f32_eq(&mut self, input: Input) -> Self::Output;
    fn visit_f32_ne(&mut self, input: Input) -> Self::Output;
    fn visit_f32_lt(&mut self, input: Input) -> Self::Output;
    fn visit_f32_gt(&mut self, input: Input) -> Self::Output;
    fn visit_f32_le(&mut self, input: Input) -> Self::Output;
    fn visit_f32_ge(&mut self, input: Input) -> Self::Output;
    fn visit_f64_eq(&mut self, input: Input) -> Self::Output;
    fn visit_f64_ne(&mut self, input: Input) -> Self::Output;
    fn visit_f64_lt(&mut self, input: Input) -> Self::Output;
    fn visit_f64_gt(&mut self, input: Input) -> Self::Output;
    fn visit_f64_le(&mut self, input: Input) -> Self::Output;
    fn visit_f64_ge(&mut self, input: Input) -> Self::Output;
    fn visit_i32_clz(&mut self, input: Input) -> Self::Output;
    fn visit_i32_ctz(&mut self, input: Input) -> Self::Output;
    fn visit_i32_popcnt(&mut self, input: Input) -> Self::Output;
    fn visit_i32_add(&mut self, input: Input) -> Self::Output;
    fn visit_i32_sub(&mut self, input: Input) -> Self::Output;
    fn visit_i32_mul(&mut self, input: Input) -> Self::Output;
    fn visit_i32_div_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_div_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_rem_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_rem_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_and(&mut self, input: Input) -> Self::Output;
    fn visit_i32_or(&mut self, input: Input) -> Self::Output;
    fn visit_i32_xor(&mut self, input: Input) -> Self::Output;
    fn visit_i32_shl(&mut self, input: Input) -> Self::Output;
    fn visit_i32_shr_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_shr_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_rotl(&mut self, input: Input) -> Self::Output;
    fn visit_i32_rotr(&mut self, input: Input) -> Self::Output;
    fn visit_i64_clz(&mut self, input: Input) -> Self::Output;
    fn visit_i64_ctz(&mut self, input: Input) -> Self::Output;
    fn visit_i64_popcnt(&mut self, input: Input) -> Self::Output;
    fn visit_i64_add(&mut self, input: Input) -> Self::Output;
    fn visit_i64_sub(&mut self, input: Input) -> Self::Output;
    fn visit_i64_mul(&mut self, input: Input) -> Self::Output;
    fn visit_i64_div_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_div_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_rem_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_rem_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_and(&mut self, input: Input) -> Self::Output;
    fn visit_i64_or(&mut self, input: Input) -> Self::Output;
    fn visit_i64_xor(&mut self, input: Input) -> Self::Output;
    fn visit_i64_shl(&mut self, input: Input) -> Self::Output;
    fn visit_i64_shr_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_shr_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_rotl(&mut self, input: Input) -> Self::Output;
    fn visit_i64_rotr(&mut self, input: Input) -> Self::Output;
    fn visit_f32_abs(&mut self, input: Input) -> Self::Output;
    fn visit_f32_neg(&mut self, input: Input) -> Self::Output;
    fn visit_f32_ceil(&mut self, input: Input) -> Self::Output;
    fn visit_f32_floor(&mut self, input: Input) -> Self::Output;
    fn visit_f32_trunc(&mut self, input: Input) -> Self::Output;
    fn visit_f32_nearest(&mut self, input: Input) -> Self::Output;
    fn visit_f32_sqrt(&mut self, input: Input) -> Self::Output;
    fn visit_f32_add(&mut self, input: Input) -> Self::Output;
    fn visit_f32_sub(&mut self, input: Input) -> Self::Output;
    fn visit_f32_mul(&mut self, input: Input) -> Self::Output;
    fn visit_f32_div(&mut self, input: Input) -> Self::Output;
    fn visit_f32_min(&mut self, input: Input) -> Self::Output;
    fn visit_f32_max(&mut self, input: Input) -> Self::Output;
    fn visit_f32_copysign(&mut self, input: Input) -> Self::Output;
    fn visit_f64_abs(&mut self, input: Input) -> Self::Output;
    fn visit_f64_neg(&mut self, input: Input) -> Self::Output;
    fn visit_f64_ceil(&mut self, input: Input) -> Self::Output;
    fn visit_f64_floor(&mut self, input: Input) -> Self::Output;
    fn visit_f64_trunc(&mut self, input: Input) -> Self::Output;
    fn visit_f64_nearest(&mut self, input: Input) -> Self::Output;
    fn visit_f64_sqrt(&mut self, input: Input) -> Self::Output;
    fn visit_f64_add(&mut self, input: Input) -> Self::Output;
    fn visit_f64_sub(&mut self, input: Input) -> Self::Output;
    fn visit_f64_mul(&mut self, input: Input) -> Self::Output;
    fn visit_f64_div(&mut self, input: Input) -> Self::Output;
    fn visit_f64_min(&mut self, input: Input) -> Self::Output;
    fn visit_f64_max(&mut self, input: Input) -> Self::Output;
    fn visit_f64_copysign(&mut self, input: Input) -> Self::Output;
    fn visit_i32_wrap_i64(&mut self, input: Input) -> Self::Output;
    fn visit_i32_trunc_f32s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_trunc_f32u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_trunc_f64s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_trunc_f64u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_extend_i32s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_extend_i32u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_trunc_f32s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_trunc_f32u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_trunc_f64s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_trunc_f64u(&mut self, input: Input) -> Self::Output;
    fn visit_f32_convert_i32s(&mut self, input: Input) -> Self::Output;
    fn visit_f32_convert_i32u(&mut self, input: Input) -> Self::Output;
    fn visit_f32_convert_i64s(&mut self, input: Input) -> Self::Output;
    fn visit_f32_convert_i64u(&mut self, input: Input) -> Self::Output;
    fn visit_f32_demote_f64(&mut self, input: Input) -> Self::Output;
    fn visit_f64_convert_i32s(&mut self, input: Input) -> Self::Output;
    fn visit_f64_convert_i32u(&mut self, input: Input) -> Self::Output;
    fn visit_f64_convert_i64s(&mut self, input: Input) -> Self::Output;
    fn visit_f64_convert_i64u(&mut self, input: Input) -> Self::Output;
    fn visit_f64_promote_f32(&mut self, input: Input) -> Self::Output;
    fn visit_i32_reinterpret_f32(&mut self, input: Input) -> Self::Output;
    fn visit_i64_reinterpret_f64(&mut self, input: Input) -> Self::Output;
    fn visit_f32_reinterpret_i32(&mut self, input: Input) -> Self::Output;
    fn visit_f64_reinterpret_i64(&mut self, input: Input) -> Self::Output;
    fn visit_i32_trunc_sat_f32s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_trunc_sat_f32u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_trunc_sat_f64s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_trunc_sat_f64u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_trunc_sat_f32s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_trunc_sat_f32u(&mut self, input: Input) -> Self::Output;
    fn visit_i64_trunc_sat_f64s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_trunc_sat_f64u(&mut self, input: Input) -> Self::Output;
    fn visit_i32_extend8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_extend16_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_extend8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_extend16_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64_extend32_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32_atomic_load(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_load16_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_load8_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_load(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_load32_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_load16_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_load8_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_store(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_store16(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_store8(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_store(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_store32(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_store16(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_store8(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_add(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_sub(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_and(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_or(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_xor(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw16_add_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw16_sub_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw16_and_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw16_or_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw16_xor_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw8_add_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw8_sub_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw8_and_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw8_or_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw8_xor_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_add(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_sub(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_and(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_or(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_xor(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw32_add_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw32_sub_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw32_and_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw32_or_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw32_xor_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw16_add_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw16_sub_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw16_and_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw16_or_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw16_xor_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw8_add_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw8_sub_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw8_and_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw8_or_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw8_xor_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_xchg(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw16_xchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw8_xchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw_cmpxchg(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw16_cmpxchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i32_atomic_rmw8_cmpxchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_xchg(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw32_xchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw16_xchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw8_xchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw_cmpxchg(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw32_cmpxchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw16_cmpxchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_i64_atomic_rmw8_cmpxchg_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_memory_atomic_notify(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_memory_atomic_wait32(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_memory_atomic_wait64(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_atomic_fence(&mut self, input: Input,  flags: u8) -> Self::Output;
    fn visit_ref_null(&mut self, input: Input,  ty: ValType) -> Self::Output;
    fn visit_ref_is_null(&mut self, input: Input) -> Self::Output;
    fn visit_ref_func(&mut self, input: Input,  function_index: u32) -> Self::Output;
    fn visit_v128_load(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_store(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_const(&mut self, input: Input, value: V128) -> Self::Output;
    fn visit_i8x16_splat(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_splat(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_splat(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_splat(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_splat(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_splat(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_extract_lane_s(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i8x16_extract_lane_u(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i16x8_extract_lane_s(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i16x8_extract_lane_u(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i32x4_extract_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i8x16_replace_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i16x8_replace_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i32x4_replace_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i64x2_extract_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_i64x2_replace_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f32x4_extract_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f32x4_replace_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f64x2_extract_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f64x2_replace_lane(&mut self, input: Input,  lane: SIMDLaneIndex) -> Self::Output;
    fn visit_f32x4_eq(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_ne(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_lt(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_gt(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_le(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_ge(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_eq(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_ne(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_lt(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_gt(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_le(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_ge(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_add(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_sub(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_mul(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_div(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_min(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_max(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_p_min(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_p_max(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_add(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_sub(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_mul(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_div(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_min(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_max(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_p_min(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_p_max(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_relaxed_min(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_relaxed_max(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_relaxed_min(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_relaxed_max(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_eq(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_ne(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_lt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_lt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_gt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_gt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_le_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_le_u(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_ge_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_ge_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_eq(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ne(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_lt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_lt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_gt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_gt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_le_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_le_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ge_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ge_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_eq(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ne(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_lt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_lt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_gt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_gt_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_le_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_le_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ge_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ge_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_eq(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_ne(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_lt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_gt_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_le_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_ge_s(&mut self, input: Input) -> Self::Output;
    fn visit_v128_and(&mut self, input: Input) -> Self::Output;
    fn visit_v128_and_not(&mut self, input: Input) -> Self::Output;
    fn visit_v128_or(&mut self, input: Input) -> Self::Output;
    fn visit_v128_xor(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_add(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_add_sat_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_add_sat_u(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_sub(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_sub_sat_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_sub_sat_u(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_min_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_min_u(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_max_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_max_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_add(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_add_sat_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_add_sat_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_sub(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_sub_sat_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_sub_sat_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_mul(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_min_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_min_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_max_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_max_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_add(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_sub(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_mul(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_min_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_min_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_max_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_max_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_dot_i16x8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_add(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_sub(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_mul(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_rounding_average_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_rounding_average_u(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_narrow_i16x8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_narrow_i16x8_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_narrow_i32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_narrow_i32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ext_mul_low_i8x16_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ext_mul_high_i8x16_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ext_mul_low_i8x16_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ext_mul_high_i8x16_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ext_mul_low_i16x8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ext_mul_high_i16x8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ext_mul_low_i16x8_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ext_mul_high_i16x8_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_ext_mul_low_i32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_ext_mul_high_i32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_ext_mul_low_i32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_ext_mul_high_i32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_q15_mulr_sat_s(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_ceil(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_floor(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_trunc(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_nearest(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_ceil(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_floor(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_trunc(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_nearest(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_abs(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_neg(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_sqrt(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_abs(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_neg(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_sqrt(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_demote_f64x2_zero(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_promote_low_f32x4(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_convert_low_i32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_convert_low_i32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_trunc_sat_f32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_trunc_sat_f32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_convert_i32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_convert_i32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_v128_not(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_abs(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_neg(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_popcnt(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_abs(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_neg(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_abs(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_neg(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_abs(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_neg(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_extend_low_i8x16_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_extend_high_i8x16_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_extend_low_i8x16_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_extend_high_i8x16_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_extend_low_i16x8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_extend_high_i16x8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_extend_low_i16x8_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_extend_high_i16x8_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_extend_low_i32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_extend_high_i32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_extend_low_i32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_extend_high_i32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ext_add_pairwise_i8x16_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_ext_add_pairwise_i8x16_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ext_add_pairwise_i16x8_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_ext_add_pairwise_i16x8_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, input: Input) -> Self::Output;
    fn visit_v128_bitselect(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_fma(&mut self, input: Input) -> Self::Output;
    fn visit_f32x4_fms(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_fma(&mut self, input: Input) -> Self::Output;
    fn visit_f64x2_fms(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_lane_select(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_lane_select(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_lane_select(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_lane_select(&mut self, input: Input) -> Self::Output;
    fn visit_v128_any_true(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_all_true(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_bitmask(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_all_true(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_bitmask(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_all_true(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_bitmask(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_all_true(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_bitmask(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_shl(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_shr_s(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_shr_u(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_shl(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_shr_s(&mut self, input: Input) -> Self::Output;
    fn visit_i16x8_shr_u(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_shl(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_shr_s(&mut self, input: Input) -> Self::Output;
    fn visit_i32x4_shr_u(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_shl(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_shr_s(&mut self, input: Input) -> Self::Output;
    fn visit_i64x2_shr_u(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_swizzle(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_relaxed_swizzle(&mut self, input: Input) -> Self::Output;
    fn visit_i8x16_shuffle(&mut self, input: Input,  lanes: [SIMDLaneIndex; 16]) -> Self::Output;
    fn visit_v128_load8_splat(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load16_splat(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load32_splat(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load32_zero(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load64_splat(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load64_zero(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load8x8_s(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load8x8_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load16x4_s(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load16x4_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load32x2_s(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load32x2_u(&mut self, input: Input, memarg: MemoryImmediate) -> Self::Output;
    fn visit_v128_load8_lane(&mut self, input: Input, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_v128_load16_lane(&mut self, input: Input, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_v128_load32_lane(&mut self, input: Input, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_v128_load64_lane(&mut self, input: Input, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_v128_store8_lane(&mut self, input: Input, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_v128_store16_lane(&mut self, input: Input, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_v128_store32_lane(&mut self, input: Input, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_v128_store64_lane(&mut self, input: Input, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output;
    fn visit_memory_init(&mut self, input: Input, mem: u32, segment: u32) -> Self::Output;
    fn visit_data_drop(&mut self, input: Input, segment: u32) -> Self::Output;
    fn visit_memory_copy(&mut self, input: Input, src: u32, dst: u32) -> Self::Output;
    fn visit_memory_fill(&mut self, input: Input, mem: u32) -> Self::Output;
    fn visit_table_init(&mut self, input: Input, segment: u32, table: u32) -> Self::Output;
    fn visit_elem_drop(&mut self, input: Input, segment: u32) -> Self::Output;
    fn visit_table_copy(&mut self, input: Input, dst_table: u32, src_table: u32) -> Self::Output;
    fn visit_table_get(&mut self, input: Input, table: u32) -> Self::Output;
    fn visit_table_set(&mut self, input: Input, table: u32) -> Self::Output;
    fn visit_table_grow(&mut self, input: Input, table: u32) -> Self::Output;
    fn visit_table_size(&mut self, input: Input, table: u32) -> Self::Output;
    fn visit_table_fill(&mut self, input: Input, table: u32) -> Self::Output;
}
