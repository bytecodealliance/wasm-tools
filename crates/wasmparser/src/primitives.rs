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

use std::error::Error;
use std::fmt;
use std::result;

#[derive(Debug, Clone)]
pub struct BinaryReaderError {
    // Wrap the actual error data in a `Box` so that the error is just one
    // word. This means that we can continue returning small `Result`s in
    // registers.
    pub(crate) inner: Box<BinaryReaderErrorInner>,
}

#[derive(Debug, Clone)]
pub(crate) struct BinaryReaderErrorInner {
    pub(crate) message: String,
    pub(crate) offset: usize,
    pub(crate) needed_hint: Option<usize>,
}

pub type Result<T, E = BinaryReaderError> = result::Result<T, E>;

impl Error for BinaryReaderError {}

impl fmt::Display for BinaryReaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} (at offset {})",
            self.inner.message, self.inner.offset
        )
    }
}

impl BinaryReaderError {
    pub(crate) fn new(message: impl Into<String>, offset: usize) -> Self {
        let message = message.into();
        BinaryReaderError {
            inner: Box::new(BinaryReaderErrorInner {
                message,
                offset,
                needed_hint: None,
            }),
        }
    }

    pub(crate) fn eof(offset: usize, needed_hint: usize) -> Self {
        BinaryReaderError {
            inner: Box::new(BinaryReaderErrorInner {
                message: "Unexpected EOF".to_string(),
                offset,
                needed_hint: Some(needed_hint),
            }),
        }
    }

    /// Get this error's message.
    pub fn message(&self) -> &str {
        &self.inner.message
    }

    /// Get the offset within the Wasm binary where the error occured.
    pub fn offset(&self) -> usize {
        self.inner.offset
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CustomSectionKind {
    Unknown,
    Name,
    Producers,
    SourceMappingURL,
    Reloc,
    Linking,
}

/// Section code as defined [here].
///
/// [here]: https://webassembly.github.io/spec/core/binary/modules.html#sections
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SectionCode<'a> {
    Custom {
        name: &'a str,
        kind: CustomSectionKind,
    },
    Type,       // Function signature declarations
    Alias,      // Aliased indices from nested/parent modules
    Import,     // Import declarations
    Module,     // Module declarations
    Instance,   // Instance definitions
    Function,   // Function declarations
    Table,      // Indirect function table and other tables
    Memory,     // Memory attributes
    Global,     // Global declarations
    Export,     // Exports
    Start,      // Start function declaration
    Element,    // Elements section
    ModuleCode, // Module definitions
    Code,       // Function bodies (code)
    Data,       // Data segments
    DataCount,  // Count of passive data segments
    Event,      // Event declarations
}

/// Types as defined [here].
///
/// [here]: https://webassembly.github.io/spec/core/syntax/types.html#types
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    #[cfg(feature = "simd-proposal")]
    V128,
    FuncRef,
    ExternRef,
    ExnRef,
    Func,
    EmptyBlockType,
}

/// Either a value type or a function type.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeOrFuncType {
    /// A value type.
    ///
    /// When used as the type for a block, this type is the optional result
    /// type: `[] -> [t?]`.
    Type(Type),

    /// A function type (referenced as an index into the types section).
    FuncType(u32),
}

/// External types as defined [here].
///
/// [here]: https://webassembly.github.io/spec/core/syntax/types.html#external-types
#[derive(Debug, Copy, Clone)]
pub enum ExternalKind {
    Function,
    Table,
    Memory,
    Event,
    Global,
    Type,
    Module,
    Instance,
}

#[derive(Debug, Clone)]
pub enum TypeDef<'a> {
    Func(FuncType),
    Instance(InstanceType<'a>),
    Module(ModuleType<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FuncType {
    pub params: Box<[Type]>,
    pub returns: Box<[Type]>,
}

#[derive(Debug, Clone)]
pub struct InstanceType<'a> {
    pub exports: Box<[ExportType<'a>]>,
}

#[derive(Debug, Clone)]
pub struct ModuleType<'a> {
    pub imports: Box<[crate::Import<'a>]>,
    pub exports: Box<[ExportType<'a>]>,
}

#[derive(Debug, Clone)]
pub struct ExportType<'a> {
    pub name: &'a str,
    pub ty: ImportSectionEntryType,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ResizableLimits {
    pub initial: u32,
    pub maximum: Option<u32>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ResizableLimits64 {
    pub initial: u64,
    pub maximum: Option<u64>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TableType {
    pub element_type: Type,
    pub limits: ResizableLimits,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MemoryType {
    M32 {
        limits: ResizableLimits,
        shared: bool,
    },
    M64 {
        limits: ResizableLimits64,
        shared: bool,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EventType {
    pub type_index: u32,
}

impl MemoryType {
    pub fn index_type(&self) -> Type {
        match self {
            MemoryType::M32 { .. } => Type::I32,
            MemoryType::M64 { .. } => Type::I64,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct GlobalType {
    pub content_type: Type,
    pub mutable: bool,
}

#[derive(Debug, Copy, Clone)]
pub enum ImportSectionEntryType {
    Function(u32),
    Table(TableType),
    Memory(MemoryType),
    Event(EventType),
    Global(GlobalType),
    Module(u32),
    Instance(u32),
}

#[derive(Debug, Copy, Clone)]
pub struct MemoryImmediate {
    /// Alignment, stored as `n` where the actual alignment is `2^n`
    pub align: u8,
    pub offset: u32,
    pub memory: u32,
}

#[derive(Debug, Copy, Clone)]
pub struct Naming<'a> {
    pub index: u32,
    pub name: &'a str,
}

#[derive(Debug, Copy, Clone)]
pub enum NameType {
    Module,
    Function,
    Local,
}

#[derive(Debug, Copy, Clone)]
pub enum LinkingType {
    StackPointer(u32),
}

#[derive(Debug, Copy, Clone)]
pub enum RelocType {
    FunctionIndexLEB,
    TableIndexSLEB,
    TableIndexI32,
    GlobalAddrLEB,
    GlobalAddrSLEB,
    GlobalAddrI32,
    TypeIndexLEB,
    GlobalIndexLEB,
}

/// A br_table entries representation.
#[derive(Clone)]
pub struct BrTable<'a> {
    pub(crate) reader: crate::BinaryReader<'a>,
    pub(crate) cnt: usize,
}

/// An IEEE binary32 immediate floating point value, represented as a u32
/// containing the bitpattern.
///
/// All bit patterns are allowed.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ieee32(pub(crate) u32);

impl Ieee32 {
    pub fn bits(self) -> u32 {
        self.0
    }
}

/// An IEEE binary64 immediate floating point value, represented as a u64
/// containing the bitpattern.
///
/// All bit patterns are allowed.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ieee64(pub(crate) u64);

impl Ieee64 {
    pub fn bits(self) -> u64 {
        self.0
    }
}

#[cfg(feature = "simd-proposal")]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct V128(pub(crate) [u8; 16]);

#[cfg(feature = "simd-proposal")]
impl V128 {
    pub fn bytes(&self) -> &[u8; 16] {
        &self.0
    }
}

#[cfg(feature = "simd-proposal")]
pub type SIMDLaneIndex = u8;

/// Instructions as defined [here].
///
/// [here]: https://webassembly.github.io/spec/core/binary/instructions.html
#[derive(Debug, Clone)]
pub enum Operator<'a> {
    Unreachable,
    Nop,
    Block { ty: TypeOrFuncType },
    Loop { ty: TypeOrFuncType },
    If { ty: TypeOrFuncType },
    Else,
    Try { ty: TypeOrFuncType },
    Catch { index: u32 },
    Throw { index: u32 },
    Rethrow { relative_depth: u32 },
    Unwind,
    End,
    Br { relative_depth: u32 },
    BrIf { relative_depth: u32 },
    BrTable { table: BrTable<'a> },
    Return,
    Call { function_index: u32 },
    CallIndirect { index: u32, table_index: u32 },
    ReturnCall { function_index: u32 },
    ReturnCallIndirect { index: u32, table_index: u32 },
    Drop,
    Select,
    TypedSelect { ty: Type },
    LocalGet { local_index: u32 },
    LocalSet { local_index: u32 },
    LocalTee { local_index: u32 },
    GlobalGet { global_index: u32 },
    GlobalSet { global_index: u32 },
    I32Load { memarg: MemoryImmediate },
    I64Load { memarg: MemoryImmediate },
    F32Load { memarg: MemoryImmediate },
    F64Load { memarg: MemoryImmediate },
    I32Load8S { memarg: MemoryImmediate },
    I32Load8U { memarg: MemoryImmediate },
    I32Load16S { memarg: MemoryImmediate },
    I32Load16U { memarg: MemoryImmediate },
    I64Load8S { memarg: MemoryImmediate },
    I64Load8U { memarg: MemoryImmediate },
    I64Load16S { memarg: MemoryImmediate },
    I64Load16U { memarg: MemoryImmediate },
    I64Load32S { memarg: MemoryImmediate },
    I64Load32U { memarg: MemoryImmediate },
    I32Store { memarg: MemoryImmediate },
    I64Store { memarg: MemoryImmediate },
    F32Store { memarg: MemoryImmediate },
    F64Store { memarg: MemoryImmediate },
    I32Store8 { memarg: MemoryImmediate },
    I32Store16 { memarg: MemoryImmediate },
    I64Store8 { memarg: MemoryImmediate },
    I64Store16 { memarg: MemoryImmediate },
    I64Store32 { memarg: MemoryImmediate },
    MemorySize { mem: u32, mem_byte: u8 },
    MemoryGrow { mem: u32, mem_byte: u8 },
    I32Const { value: i32 },
    I64Const { value: i64 },
    F32Const { value: Ieee32 },
    F64Const { value: Ieee64 },
    RefNull { ty: Type },
    RefIsNull,
    RefFunc { function_index: u32 },
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
    MemoryInit { segment: u32, mem: u32 },
    DataDrop { segment: u32 },
    MemoryCopy { src: u32, dst: u32 },
    MemoryFill { mem: u32 },
    TableInit { segment: u32, table: u32 },
    ElemDrop { segment: u32 },
    TableCopy { dst_table: u32, src_table: u32 },
    TableFill { table: u32 },
    TableGet { table: u32 },
    TableSet { table: u32 },
    TableGrow { table: u32 },
    TableSize { table: u32 },

    // 0xFE operators
    // https://github.com/WebAssembly/threads/blob/master/proposals/threads/Overview.md
    MemoryAtomicNotify { memarg: MemoryImmediate },
    MemoryAtomicWait32 { memarg: MemoryImmediate },
    MemoryAtomicWait64 { memarg: MemoryImmediate },
    AtomicFence { flags: u8 },
    I32AtomicLoad { memarg: MemoryImmediate },
    I64AtomicLoad { memarg: MemoryImmediate },
    I32AtomicLoad8U { memarg: MemoryImmediate },
    I32AtomicLoad16U { memarg: MemoryImmediate },
    I64AtomicLoad8U { memarg: MemoryImmediate },
    I64AtomicLoad16U { memarg: MemoryImmediate },
    I64AtomicLoad32U { memarg: MemoryImmediate },
    I32AtomicStore { memarg: MemoryImmediate },
    I64AtomicStore { memarg: MemoryImmediate },
    I32AtomicStore8 { memarg: MemoryImmediate },
    I32AtomicStore16 { memarg: MemoryImmediate },
    I64AtomicStore8 { memarg: MemoryImmediate },
    I64AtomicStore16 { memarg: MemoryImmediate },
    I64AtomicStore32 { memarg: MemoryImmediate },
    I32AtomicRmwAdd { memarg: MemoryImmediate },
    I64AtomicRmwAdd { memarg: MemoryImmediate },
    I32AtomicRmw8AddU { memarg: MemoryImmediate },
    I32AtomicRmw16AddU { memarg: MemoryImmediate },
    I64AtomicRmw8AddU { memarg: MemoryImmediate },
    I64AtomicRmw16AddU { memarg: MemoryImmediate },
    I64AtomicRmw32AddU { memarg: MemoryImmediate },
    I32AtomicRmwSub { memarg: MemoryImmediate },
    I64AtomicRmwSub { memarg: MemoryImmediate },
    I32AtomicRmw8SubU { memarg: MemoryImmediate },
    I32AtomicRmw16SubU { memarg: MemoryImmediate },
    I64AtomicRmw8SubU { memarg: MemoryImmediate },
    I64AtomicRmw16SubU { memarg: MemoryImmediate },
    I64AtomicRmw32SubU { memarg: MemoryImmediate },
    I32AtomicRmwAnd { memarg: MemoryImmediate },
    I64AtomicRmwAnd { memarg: MemoryImmediate },
    I32AtomicRmw8AndU { memarg: MemoryImmediate },
    I32AtomicRmw16AndU { memarg: MemoryImmediate },
    I64AtomicRmw8AndU { memarg: MemoryImmediate },
    I64AtomicRmw16AndU { memarg: MemoryImmediate },
    I64AtomicRmw32AndU { memarg: MemoryImmediate },
    I32AtomicRmwOr { memarg: MemoryImmediate },
    I64AtomicRmwOr { memarg: MemoryImmediate },
    I32AtomicRmw8OrU { memarg: MemoryImmediate },
    I32AtomicRmw16OrU { memarg: MemoryImmediate },
    I64AtomicRmw8OrU { memarg: MemoryImmediate },
    I64AtomicRmw16OrU { memarg: MemoryImmediate },
    I64AtomicRmw32OrU { memarg: MemoryImmediate },
    I32AtomicRmwXor { memarg: MemoryImmediate },
    I64AtomicRmwXor { memarg: MemoryImmediate },
    I32AtomicRmw8XorU { memarg: MemoryImmediate },
    I32AtomicRmw16XorU { memarg: MemoryImmediate },
    I64AtomicRmw8XorU { memarg: MemoryImmediate },
    I64AtomicRmw16XorU { memarg: MemoryImmediate },
    I64AtomicRmw32XorU { memarg: MemoryImmediate },
    I32AtomicRmwXchg { memarg: MemoryImmediate },
    I64AtomicRmwXchg { memarg: MemoryImmediate },
    I32AtomicRmw8XchgU { memarg: MemoryImmediate },
    I32AtomicRmw16XchgU { memarg: MemoryImmediate },
    I64AtomicRmw8XchgU { memarg: MemoryImmediate },
    I64AtomicRmw16XchgU { memarg: MemoryImmediate },
    I64AtomicRmw32XchgU { memarg: MemoryImmediate },
    I32AtomicRmwCmpxchg { memarg: MemoryImmediate },
    I64AtomicRmwCmpxchg { memarg: MemoryImmediate },
    I32AtomicRmw8CmpxchgU { memarg: MemoryImmediate },
    I32AtomicRmw16CmpxchgU { memarg: MemoryImmediate },
    I64AtomicRmw8CmpxchgU { memarg: MemoryImmediate },
    I64AtomicRmw16CmpxchgU { memarg: MemoryImmediate },
    I64AtomicRmw32CmpxchgU { memarg: MemoryImmediate },

    // 0xFD operators
    // SIMD https://github.com/WebAssembly/simd/blob/master/proposals/simd/BinarySIMD.md
    #[cfg(feature = "simd-proposal")] V128Load { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Store { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Const { value: V128 },
    #[cfg(feature = "simd-proposal")] I8x16Splat,
    #[cfg(feature = "simd-proposal")] I8x16ExtractLaneS { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I8x16ExtractLaneU { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I8x16ReplaceLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I16x8Splat,
    #[cfg(feature = "simd-proposal")] I16x8ExtractLaneS { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I16x8ExtractLaneU { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I16x8ReplaceLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I32x4Splat,
    #[cfg(feature = "simd-proposal")] I32x4ExtractLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I32x4ReplaceLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I64x2Splat,
    #[cfg(feature = "simd-proposal")] I64x2ExtractLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I64x2ReplaceLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] F32x4Splat,
    #[cfg(feature = "simd-proposal")] F32x4ExtractLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] F32x4ReplaceLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] F64x2Splat,
    #[cfg(feature = "simd-proposal")] F64x2ExtractLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] F64x2ReplaceLane { lane: SIMDLaneIndex },
    #[cfg(feature = "simd-proposal")] I8x16Eq,
    #[cfg(feature = "simd-proposal")] I8x16Ne,
    #[cfg(feature = "simd-proposal")] I8x16LtS,
    #[cfg(feature = "simd-proposal")] I8x16LtU,
    #[cfg(feature = "simd-proposal")] I8x16GtS,
    #[cfg(feature = "simd-proposal")] I8x16GtU,
    #[cfg(feature = "simd-proposal")] I8x16LeS,
    #[cfg(feature = "simd-proposal")] I8x16LeU,
    #[cfg(feature = "simd-proposal")] I8x16GeS,
    #[cfg(feature = "simd-proposal")] I8x16GeU,
    #[cfg(feature = "simd-proposal")] I16x8Eq,
    #[cfg(feature = "simd-proposal")] I16x8Ne,
    #[cfg(feature = "simd-proposal")] I16x8LtS,
    #[cfg(feature = "simd-proposal")] I16x8LtU,
    #[cfg(feature = "simd-proposal")] I16x8GtS,
    #[cfg(feature = "simd-proposal")] I16x8GtU,
    #[cfg(feature = "simd-proposal")] I16x8LeS,
    #[cfg(feature = "simd-proposal")] I16x8LeU,
    #[cfg(feature = "simd-proposal")] I16x8GeS,
    #[cfg(feature = "simd-proposal")] I16x8GeU,
    #[cfg(feature = "simd-proposal")] I32x4Eq,
    #[cfg(feature = "simd-proposal")] I32x4Ne,
    #[cfg(feature = "simd-proposal")] I32x4LtS,
    #[cfg(feature = "simd-proposal")] I32x4LtU,
    #[cfg(feature = "simd-proposal")] I32x4GtS,
    #[cfg(feature = "simd-proposal")] I32x4GtU,
    #[cfg(feature = "simd-proposal")] I32x4LeS,
    #[cfg(feature = "simd-proposal")] I32x4LeU,
    #[cfg(feature = "simd-proposal")] I32x4GeS,
    #[cfg(feature = "simd-proposal")] I32x4GeU,
    #[cfg(feature = "simd-proposal")] F32x4Eq,
    #[cfg(feature = "simd-proposal")] F32x4Ne,
    #[cfg(feature = "simd-proposal")] F32x4Lt,
    #[cfg(feature = "simd-proposal")] F32x4Gt,
    #[cfg(feature = "simd-proposal")] F32x4Le,
    #[cfg(feature = "simd-proposal")] F32x4Ge,
    #[cfg(feature = "simd-proposal")] F64x2Eq,
    #[cfg(feature = "simd-proposal")] F64x2Ne,
    #[cfg(feature = "simd-proposal")] F64x2Lt,
    #[cfg(feature = "simd-proposal")] F64x2Gt,
    #[cfg(feature = "simd-proposal")] F64x2Le,
    #[cfg(feature = "simd-proposal")] F64x2Ge,
    #[cfg(feature = "simd-proposal")] V128Not,
    #[cfg(feature = "simd-proposal")] V128And,
    #[cfg(feature = "simd-proposal")] V128AndNot,
    #[cfg(feature = "simd-proposal")] V128Or,
    #[cfg(feature = "simd-proposal")] V128Xor,
    #[cfg(feature = "simd-proposal")] V128Bitselect,
    #[cfg(feature = "simd-proposal")] I8x16Abs,
    #[cfg(feature = "simd-proposal")] I8x16Neg,
    #[cfg(feature = "simd-proposal")] I8x16AnyTrue,
    #[cfg(feature = "simd-proposal")] I8x16AllTrue,
    #[cfg(feature = "simd-proposal")] I8x16Bitmask,
    #[cfg(feature = "simd-proposal")] I8x16Shl,
    #[cfg(feature = "simd-proposal")] I8x16ShrS,
    #[cfg(feature = "simd-proposal")] I8x16ShrU,
    #[cfg(feature = "simd-proposal")] I8x16Add,
    #[cfg(feature = "simd-proposal")] I8x16AddSatS,
    #[cfg(feature = "simd-proposal")] I8x16AddSatU,
    #[cfg(feature = "simd-proposal")] I8x16Sub,
    #[cfg(feature = "simd-proposal")] I8x16SubSatS,
    #[cfg(feature = "simd-proposal")] I8x16SubSatU,
    #[cfg(feature = "simd-proposal")] I8x16MinS,
    #[cfg(feature = "simd-proposal")] I8x16MinU,
    #[cfg(feature = "simd-proposal")] I8x16MaxS,
    #[cfg(feature = "simd-proposal")] I8x16MaxU,
    #[cfg(feature = "simd-proposal")] I16x8Abs,
    #[cfg(feature = "simd-proposal")] I16x8Neg,
    #[cfg(feature = "simd-proposal")] I16x8AnyTrue,
    #[cfg(feature = "simd-proposal")] I16x8AllTrue,
    #[cfg(feature = "simd-proposal")] I16x8Bitmask,
    #[cfg(feature = "simd-proposal")] I16x8Shl,
    #[cfg(feature = "simd-proposal")] I16x8ShrS,
    #[cfg(feature = "simd-proposal")] I16x8ShrU,
    #[cfg(feature = "simd-proposal")] I16x8Add,
    #[cfg(feature = "simd-proposal")] I16x8AddSatS,
    #[cfg(feature = "simd-proposal")] I16x8AddSatU,
    #[cfg(feature = "simd-proposal")] I16x8Sub,
    #[cfg(feature = "simd-proposal")] I16x8SubSatS,
    #[cfg(feature = "simd-proposal")] I16x8SubSatU,
    #[cfg(feature = "simd-proposal")] I16x8Mul,
    #[cfg(feature = "simd-proposal")] I16x8MinS,
    #[cfg(feature = "simd-proposal")] I16x8MinU,
    #[cfg(feature = "simd-proposal")] I16x8MaxS,
    #[cfg(feature = "simd-proposal")] I16x8MaxU,
    #[cfg(feature = "simd-proposal")] I32x4Abs,
    #[cfg(feature = "simd-proposal")] I32x4Neg,
    #[cfg(feature = "simd-proposal")] I32x4AnyTrue,
    #[cfg(feature = "simd-proposal")] I32x4AllTrue,
    #[cfg(feature = "simd-proposal")] I32x4Bitmask,
    #[cfg(feature = "simd-proposal")] I32x4Shl,
    #[cfg(feature = "simd-proposal")] I32x4ShrS,
    #[cfg(feature = "simd-proposal")] I32x4ShrU,
    #[cfg(feature = "simd-proposal")] I32x4Add,
    #[cfg(feature = "simd-proposal")] I32x4Sub,
    #[cfg(feature = "simd-proposal")] I32x4Mul,
    #[cfg(feature = "simd-proposal")] I32x4MinS,
    #[cfg(feature = "simd-proposal")] I32x4MinU,
    #[cfg(feature = "simd-proposal")] I32x4MaxS,
    #[cfg(feature = "simd-proposal")] I32x4MaxU,
    #[cfg(feature = "simd-proposal")] I32x4DotI16x8S,
    #[cfg(feature = "simd-proposal")] I64x2Neg,
    #[cfg(feature = "simd-proposal")] I64x2Shl,
    #[cfg(feature = "simd-proposal")] I64x2ShrS,
    #[cfg(feature = "simd-proposal")] I64x2ShrU,
    #[cfg(feature = "simd-proposal")] I64x2Add,
    #[cfg(feature = "simd-proposal")] I64x2Sub,
    #[cfg(feature = "simd-proposal")] I64x2Mul,
    #[cfg(feature = "simd-proposal")] F32x4Ceil,
    #[cfg(feature = "simd-proposal")] F32x4Floor,
    #[cfg(feature = "simd-proposal")] F32x4Trunc,
    #[cfg(feature = "simd-proposal")] F32x4Nearest,
    #[cfg(feature = "simd-proposal")] F64x2Ceil,
    #[cfg(feature = "simd-proposal")] F64x2Floor,
    #[cfg(feature = "simd-proposal")] F64x2Trunc,
    #[cfg(feature = "simd-proposal")] F64x2Nearest,
    #[cfg(feature = "simd-proposal")] F32x4Abs,
    #[cfg(feature = "simd-proposal")] F32x4Neg,
    #[cfg(feature = "simd-proposal")] F32x4Sqrt,
    #[cfg(feature = "simd-proposal")] F32x4Add,
    #[cfg(feature = "simd-proposal")] F32x4Sub,
    #[cfg(feature = "simd-proposal")] F32x4Mul,
    #[cfg(feature = "simd-proposal")] F32x4Div,
    #[cfg(feature = "simd-proposal")] F32x4Min,
    #[cfg(feature = "simd-proposal")] F32x4Max,
    #[cfg(feature = "simd-proposal")] F32x4PMin,
    #[cfg(feature = "simd-proposal")] F32x4PMax,
    #[cfg(feature = "simd-proposal")] F64x2Abs,
    #[cfg(feature = "simd-proposal")] F64x2Neg,
    #[cfg(feature = "simd-proposal")] F64x2Sqrt,
    #[cfg(feature = "simd-proposal")] F64x2Add,
    #[cfg(feature = "simd-proposal")] F64x2Sub,
    #[cfg(feature = "simd-proposal")] F64x2Mul,
    #[cfg(feature = "simd-proposal")] F64x2Div,
    #[cfg(feature = "simd-proposal")] F64x2Min,
    #[cfg(feature = "simd-proposal")] F64x2Max,
    #[cfg(feature = "simd-proposal")] F64x2PMin,
    #[cfg(feature = "simd-proposal")] F64x2PMax,
    #[cfg(feature = "simd-proposal")] I32x4TruncSatF32x4S,
    #[cfg(feature = "simd-proposal")] I32x4TruncSatF32x4U,
    #[cfg(feature = "simd-proposal")] F32x4ConvertI32x4S,
    #[cfg(feature = "simd-proposal")] F32x4ConvertI32x4U,
    #[cfg(feature = "simd-proposal")] I8x16Swizzle,
    #[cfg(feature = "simd-proposal")] I8x16Shuffle { lanes: [SIMDLaneIndex; 16] },
    #[cfg(feature = "simd-proposal")] V128Load8Splat { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load16Splat { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load32Splat { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load32Zero { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load64Splat { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load64Zero { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] I8x16NarrowI16x8S,
    #[cfg(feature = "simd-proposal")] I8x16NarrowI16x8U,
    #[cfg(feature = "simd-proposal")] I16x8NarrowI32x4S,
    #[cfg(feature = "simd-proposal")] I16x8NarrowI32x4U,
    #[cfg(feature = "simd-proposal")] I16x8WidenLowI8x16S,
    #[cfg(feature = "simd-proposal")] I16x8WidenHighI8x16S,
    #[cfg(feature = "simd-proposal")] I16x8WidenLowI8x16U,
    #[cfg(feature = "simd-proposal")] I16x8WidenHighI8x16U,
    #[cfg(feature = "simd-proposal")] I32x4WidenLowI16x8S,
    #[cfg(feature = "simd-proposal")] I32x4WidenHighI16x8S,
    #[cfg(feature = "simd-proposal")] I32x4WidenLowI16x8U,
    #[cfg(feature = "simd-proposal")] I32x4WidenHighI16x8U,
    #[cfg(feature = "simd-proposal")] I16x8ExtMulLowI8x16S,
    #[cfg(feature = "simd-proposal")] I16x8ExtMulHighI8x16S,
    #[cfg(feature = "simd-proposal")] I16x8ExtMulLowI8x16U,
    #[cfg(feature = "simd-proposal")] I16x8ExtMulHighI8x16U,
    #[cfg(feature = "simd-proposal")] I32x4ExtMulLowI16x8S,
    #[cfg(feature = "simd-proposal")] I32x4ExtMulHighI16x8S,
    #[cfg(feature = "simd-proposal")] I32x4ExtMulLowI16x8U,
    #[cfg(feature = "simd-proposal")] I32x4ExtMulHighI16x8U,
    #[cfg(feature = "simd-proposal")] I64x2ExtMulLowI32x4S,
    #[cfg(feature = "simd-proposal")] I64x2ExtMulHighI32x4S,
    #[cfg(feature = "simd-proposal")] I64x2ExtMulLowI32x4U,
    #[cfg(feature = "simd-proposal")] I64x2ExtMulHighI32x4U,
    #[cfg(feature = "simd-proposal")] V128Load8x8S { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load8x8U { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load16x4S { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load16x4U { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load32x2S { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] V128Load32x2U { memarg: MemoryImmediate },
    #[cfg(feature = "simd-proposal")] I8x16RoundingAverageU,
    #[cfg(feature = "simd-proposal")] I16x8RoundingAverageU,
}
