use super::*;

/// An encoder for the code section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{
///     CodeSection, Function, FunctionSection, Instruction, Module,
///     TypeSection, ValType
/// };
///
/// let mut types = TypeSection::new();
/// types.function(vec![], vec![ValType::I32]);
///
/// let mut functions = FunctionSection::new();
/// let type_index = 0;
/// functions.function(type_index);
///
/// let locals = vec![];
/// let mut func = Function::new(locals);
/// func.instruction(Instruction::I32Const(42));
/// let mut code = CodeSection::new();
/// code.function(&func);
///
/// let mut module = Module::new();
/// module
///     .section(&types)
///     .section(&functions)
///     .section(&code);
///
/// let wasm_bytes = module.finish();
/// ```
#[derive(Clone, Debug)]
pub struct CodeSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl CodeSection {
    /// Create a new code section encoder.
    pub fn new() -> CodeSection {
        CodeSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Write a function body into this code section.
    pub fn function(&mut self, func: &Function) -> &mut Self {
        func.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Section for CodeSection {
    fn id(&self) -> u8 {
        SectionId::Code.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

/// An encoder for a function body within the code section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{CodeSection, Function, Instruction};
///
/// // Define the function body for:
/// //
/// //     (func (param i32 i32) (result i32)
/// //       local.get 0
/// //       local.get 1
/// //       i32.add)
/// let locals = vec![];
/// let mut func = Function::new(locals);
/// func.instruction(Instruction::LocalGet(0));
/// func.instruction(Instruction::LocalGet(1));
/// func.instruction(Instruction::I32Add);
///
/// // Add our function to the code section.
/// let mut code = CodeSection::new();
/// code.function(&func);
/// ```
#[derive(Clone, Debug)]
pub struct Function {
    bytes: Vec<u8>,
}

impl Function {
    /// Create a new function body with the given locals.
    pub fn new<L>(locals: L) -> Self
    where
        L: IntoIterator<Item = (u32, ValType)>,
        L::IntoIter: ExactSizeIterator,
    {
        let locals = locals.into_iter();
        let mut bytes = vec![];
        bytes.extend(encoders::u32(u32::try_from(locals.len()).unwrap()));
        for (count, ty) in locals {
            bytes.extend(encoders::u32(count));
            bytes.push(ty.into());
        }
        Function { bytes }
    }

    /// Write an instruction into this function body.
    pub fn instruction(&mut self, instruction: Instruction) -> &mut Self {
        instruction.encode(&mut self.bytes);
        self
    }

    /// Add raw bytes to this function's body.
    pub fn raw<B>(&mut self, bytes: B) -> &mut Self
    where
        B: IntoIterator<Item = u8>,
    {
        self.bytes.extend(bytes);
        self
    }

    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.extend(
            encoders::u32(u32::try_from(self.bytes.len()).unwrap())
                .chain(self.bytes.iter().copied()),
        );
    }
}

/// The immediate for a memory instruction.
#[derive(Clone, Copy, Debug)]
pub struct MemArg {
    /// A static offset to add to the instruction's dynamic address operand.
    pub offset: u32,
    /// The expected alignment of the instruction's dynamic address operand
    /// (expressed the exponent of a power of two).
    pub align: u32,
    /// The index of the memory this instruction is operating upon.
    pub memory_index: u32,
}

impl MemArg {
    fn encode(&self, bytes: &mut Vec<u8>) {
        if self.memory_index == 0 {
            bytes.extend(encoders::u32(self.align));
            bytes.extend(encoders::u32(self.offset));
        } else {
            bytes.extend(encoders::u32(self.align | (1 << 6)));
            bytes.extend(encoders::u32(self.offset));
            bytes.extend(encoders::u32(self.memory_index));
        }
    }
}

/// The type for a `block`/`if`/`loop`.
#[derive(Clone, Copy, Debug)]
pub enum BlockType {
    /// `[] -> []`
    Empty,
    /// `[] -> [t]`
    Result(ValType),
    /// The `n`th function type.
    FunctionType(u32),
}

impl BlockType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match *self {
            BlockType::Empty => bytes.push(0x40),
            BlockType::Result(ty) => bytes.push(ty.into()),
            BlockType::FunctionType(f) => bytes.extend(encoders::s33(f.into())),
        }
    }
}

/// WebAssembly instructions.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
#[allow(missing_docs, non_camel_case_types)]
pub enum Instruction<'a> {
    // Control instructions.
    Unreachable,
    Nop,
    Block(BlockType),
    Loop(BlockType),
    If(BlockType),
    Else,
    End,
    Br(u32),
    BrIf(u32),
    BrTable(&'a [u32], u32),
    Return,
    Call(u32),
    CallIndirect { ty: u32, table: u32 },

    // Parametric instructions.
    Drop,
    Select,

    // Variable instructions.
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    GlobalGet(u32),
    GlobalSet(u32),

    // Memory instructions.
    I32Load(MemArg),
    I64Load(MemArg),
    F32Load(MemArg),
    F64Load(MemArg),
    I32Load8_S(MemArg),
    I32Load8_U(MemArg),
    I32Load16_S(MemArg),
    I32Load16_U(MemArg),
    I64Load8_S(MemArg),
    I64Load8_U(MemArg),
    I64Load16_S(MemArg),
    I64Load16_U(MemArg),
    I64Load32_S(MemArg),
    I64Load32_U(MemArg),
    I32Store(MemArg),
    I64Store(MemArg),
    F32Store(MemArg),
    F64Store(MemArg),
    I32Store8(MemArg),
    I32Store16(MemArg),
    I64Store8(MemArg),
    I64Store16(MemArg),
    I64Store32(MemArg),
    MemorySize(u32),
    MemoryGrow(u32),
    MemoryInit { mem: u32, data: u32 },
    DataDrop(u32),
    MemoryCopy { src: u32, dst: u32 },
    MemoryFill(u32),

    // Numeric instructions.
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    I32Eqz,
    I32Eq,
    I32Neq,
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
    I64Neq,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
    F32Eq,
    F32Neq,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F64Eq,
    F64Neq,
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
    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,

    // SIMD instructions.
    V128Const(i128),

    // Reference types instructions.
    TypedSelect(ValType),
    RefNull(ValType),
    RefIsNull,
    RefFunc(u32),

    // Bulk memory instructions.
    TableInit { segment: u32, table: u32 },
    ElemDrop { segment: u32 },
    TableFill { table: u32 },
    TableSet { table: u32 },
    TableGet { table: u32 },
    TableGrow { table: u32 },
    TableSize { table: u32 },
    TableCopy { src: u32, dst: u32 },
}

impl Instruction<'_> {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match *self {
            // Control instructions.
            Instruction::Unreachable => bytes.push(0x00),
            Instruction::Nop => bytes.push(0x01),
            Instruction::Block(bt) => {
                bytes.push(0x02);
                bt.encode(bytes);
            }
            Instruction::Loop(bt) => {
                bytes.push(0x03);
                bt.encode(bytes);
            }
            Instruction::If(bt) => {
                bytes.push(0x04);
                bt.encode(bytes);
            }
            Instruction::Else => bytes.push(0x05),
            Instruction::End => bytes.push(0x0B),
            Instruction::Br(l) => {
                bytes.push(0x0C);
                bytes.extend(encoders::u32(l));
            }
            Instruction::BrIf(l) => {
                bytes.push(0x0D);
                bytes.extend(encoders::u32(l));
            }
            Instruction::BrTable(ls, l) => {
                bytes.push(0x0E);
                bytes.extend(encoders::u32(u32::try_from(ls.len()).unwrap()));
                for l in ls {
                    bytes.extend(encoders::u32(*l));
                }
                bytes.extend(encoders::u32(l));
            }
            Instruction::Return => bytes.push(0x0F),
            Instruction::Call(f) => {
                bytes.push(0x10);
                bytes.extend(encoders::u32(f));
            }
            Instruction::CallIndirect { ty, table } => {
                bytes.push(0x11);
                bytes.extend(encoders::u32(ty));
                bytes.extend(encoders::u32(table));
            }

            // Parametric instructions.
            Instruction::Drop => bytes.push(0x1A),
            Instruction::Select => bytes.push(0x1B),
            Instruction::TypedSelect(ty) => {
                bytes.push(0x1c);
                bytes.extend(encoders::u32(1));
                bytes.push(ty.into());
            }

            // Variable instructions.
            Instruction::LocalGet(l) => {
                bytes.push(0x20);
                bytes.extend(encoders::u32(l));
            }
            Instruction::LocalSet(l) => {
                bytes.push(0x21);
                bytes.extend(encoders::u32(l));
            }
            Instruction::LocalTee(l) => {
                bytes.push(0x22);
                bytes.extend(encoders::u32(l));
            }
            Instruction::GlobalGet(g) => {
                bytes.push(0x23);
                bytes.extend(encoders::u32(g));
            }
            Instruction::GlobalSet(g) => {
                bytes.push(0x24);
                bytes.extend(encoders::u32(g));
            }
            Instruction::TableGet { table } => {
                bytes.push(0x25);
                bytes.extend(encoders::u32(table));
            }
            Instruction::TableSet { table } => {
                bytes.push(0x26);
                bytes.extend(encoders::u32(table));
            }

            // Memory instructions.
            Instruction::I32Load(m) => {
                bytes.push(0x28);
                m.encode(bytes);
            }
            Instruction::I64Load(m) => {
                bytes.push(0x29);
                m.encode(bytes);
            }
            Instruction::F32Load(m) => {
                bytes.push(0x2A);
                m.encode(bytes);
            }
            Instruction::F64Load(m) => {
                bytes.push(0x2B);
                m.encode(bytes);
            }
            Instruction::I32Load8_S(m) => {
                bytes.push(0x2C);
                m.encode(bytes);
            }
            Instruction::I32Load8_U(m) => {
                bytes.push(0x2D);
                m.encode(bytes);
            }
            Instruction::I32Load16_S(m) => {
                bytes.push(0x2E);
                m.encode(bytes);
            }
            Instruction::I32Load16_U(m) => {
                bytes.push(0x2F);
                m.encode(bytes);
            }
            Instruction::I64Load8_S(m) => {
                bytes.push(0x30);
                m.encode(bytes);
            }
            Instruction::I64Load8_U(m) => {
                bytes.push(0x31);
                m.encode(bytes);
            }
            Instruction::I64Load16_S(m) => {
                bytes.push(0x32);
                m.encode(bytes);
            }
            Instruction::I64Load16_U(m) => {
                bytes.push(0x33);
                m.encode(bytes);
            }
            Instruction::I64Load32_S(m) => {
                bytes.push(0x34);
                m.encode(bytes);
            }
            Instruction::I64Load32_U(m) => {
                bytes.push(0x35);
                m.encode(bytes);
            }
            Instruction::I32Store(m) => {
                bytes.push(0x36);
                m.encode(bytes);
            }
            Instruction::I64Store(m) => {
                bytes.push(0x37);
                m.encode(bytes);
            }
            Instruction::F32Store(m) => {
                bytes.push(0x38);
                m.encode(bytes);
            }
            Instruction::F64Store(m) => {
                bytes.push(0x39);
                m.encode(bytes);
            }
            Instruction::I32Store8(m) => {
                bytes.push(0x3A);
                m.encode(bytes);
            }
            Instruction::I32Store16(m) => {
                bytes.push(0x3B);
                m.encode(bytes);
            }
            Instruction::I64Store8(m) => {
                bytes.push(0x3C);
                m.encode(bytes);
            }
            Instruction::I64Store16(m) => {
                bytes.push(0x3D);
                m.encode(bytes);
            }
            Instruction::I64Store32(m) => {
                bytes.push(0x3E);
                m.encode(bytes);
            }
            Instruction::MemorySize(i) => {
                bytes.push(0x3F);
                bytes.extend(encoders::u32(i));
            }
            Instruction::MemoryGrow(i) => {
                bytes.push(0x40);
                bytes.extend(encoders::u32(i));
            }
            Instruction::MemoryInit { mem, data } => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(8));
                bytes.extend(encoders::u32(data));
                bytes.extend(encoders::u32(mem));
            }
            Instruction::DataDrop(data) => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(9));
                bytes.extend(encoders::u32(data));
            }
            Instruction::MemoryCopy { src, dst } => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(10));
                bytes.extend(encoders::u32(dst));
                bytes.extend(encoders::u32(src));
            }
            Instruction::MemoryFill(mem) => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(11));
                bytes.extend(encoders::u32(mem));
            }

            // Numeric instructions.
            Instruction::I32Const(x) => {
                bytes.push(0x41);
                bytes.extend(encoders::s32(x));
            }
            Instruction::I64Const(x) => {
                bytes.push(0x42);
                bytes.extend(encoders::s64(x));
            }
            Instruction::F32Const(x) => {
                bytes.push(0x43);
                let x = x.to_bits();
                bytes.extend(x.to_le_bytes().iter().copied());
            }
            Instruction::F64Const(x) => {
                bytes.push(0x44);
                let x = x.to_bits();
                bytes.extend(x.to_le_bytes().iter().copied());
            }
            Instruction::I32Eqz => bytes.push(0x45),
            Instruction::I32Eq => bytes.push(0x46),
            Instruction::I32Neq => bytes.push(0x47),
            Instruction::I32LtS => bytes.push(0x48),
            Instruction::I32LtU => bytes.push(0x49),
            Instruction::I32GtS => bytes.push(0x4A),
            Instruction::I32GtU => bytes.push(0x4B),
            Instruction::I32LeS => bytes.push(0x4C),
            Instruction::I32LeU => bytes.push(0x4D),
            Instruction::I32GeS => bytes.push(0x4E),
            Instruction::I32GeU => bytes.push(0x4F),
            Instruction::I64Eqz => bytes.push(0x50),
            Instruction::I64Eq => bytes.push(0x51),
            Instruction::I64Neq => bytes.push(0x52),
            Instruction::I64LtS => bytes.push(0x53),
            Instruction::I64LtU => bytes.push(0x54),
            Instruction::I64GtS => bytes.push(0x55),
            Instruction::I64GtU => bytes.push(0x56),
            Instruction::I64LeS => bytes.push(0x57),
            Instruction::I64LeU => bytes.push(0x58),
            Instruction::I64GeS => bytes.push(0x59),
            Instruction::I64GeU => bytes.push(0x5A),
            Instruction::F32Eq => bytes.push(0x5B),
            Instruction::F32Neq => bytes.push(0x5C),
            Instruction::F32Lt => bytes.push(0x5D),
            Instruction::F32Gt => bytes.push(0x5E),
            Instruction::F32Le => bytes.push(0x5F),
            Instruction::F32Ge => bytes.push(0x60),
            Instruction::F64Eq => bytes.push(0x61),
            Instruction::F64Neq => bytes.push(0x62),
            Instruction::F64Lt => bytes.push(0x63),
            Instruction::F64Gt => bytes.push(0x64),
            Instruction::F64Le => bytes.push(0x65),
            Instruction::F64Ge => bytes.push(0x66),
            Instruction::I32Clz => bytes.push(0x67),
            Instruction::I32Ctz => bytes.push(0x68),
            Instruction::I32Popcnt => bytes.push(0x69),
            Instruction::I32Add => bytes.push(0x6A),
            Instruction::I32Sub => bytes.push(0x6B),
            Instruction::I32Mul => bytes.push(0x6C),
            Instruction::I32DivS => bytes.push(0x6D),
            Instruction::I32DivU => bytes.push(0x6E),
            Instruction::I32RemS => bytes.push(0x6F),
            Instruction::I32RemU => bytes.push(0x70),
            Instruction::I32And => bytes.push(0x71),
            Instruction::I32Or => bytes.push(0x72),
            Instruction::I32Xor => bytes.push(0x73),
            Instruction::I32Shl => bytes.push(0x74),
            Instruction::I32ShrS => bytes.push(0x75),
            Instruction::I32ShrU => bytes.push(0x76),
            Instruction::I32Rotl => bytes.push(0x77),
            Instruction::I32Rotr => bytes.push(0x78),
            Instruction::I64Clz => bytes.push(0x79),
            Instruction::I64Ctz => bytes.push(0x7A),
            Instruction::I64Popcnt => bytes.push(0x7B),
            Instruction::I64Add => bytes.push(0x7C),
            Instruction::I64Sub => bytes.push(0x7D),
            Instruction::I64Mul => bytes.push(0x7E),
            Instruction::I64DivS => bytes.push(0x7F),
            Instruction::I64DivU => bytes.push(0x80),
            Instruction::I64RemS => bytes.push(0x81),
            Instruction::I64RemU => bytes.push(0x82),
            Instruction::I64And => bytes.push(0x83),
            Instruction::I64Or => bytes.push(0x84),
            Instruction::I64Xor => bytes.push(0x85),
            Instruction::I64Shl => bytes.push(0x86),
            Instruction::I64ShrS => bytes.push(0x87),
            Instruction::I64ShrU => bytes.push(0x88),
            Instruction::I64Rotl => bytes.push(0x89),
            Instruction::I64Rotr => bytes.push(0x8A),
            Instruction::F32Abs => bytes.push(0x8B),
            Instruction::F32Neg => bytes.push(0x8C),
            Instruction::F32Ceil => bytes.push(0x8D),
            Instruction::F32Floor => bytes.push(0x8E),
            Instruction::F32Trunc => bytes.push(0x8F),
            Instruction::F32Nearest => bytes.push(0x90),
            Instruction::F32Sqrt => bytes.push(0x91),
            Instruction::F32Add => bytes.push(0x92),
            Instruction::F32Sub => bytes.push(0x93),
            Instruction::F32Mul => bytes.push(0x94),
            Instruction::F32Div => bytes.push(0x95),
            Instruction::F32Min => bytes.push(0x96),
            Instruction::F32Max => bytes.push(0x97),
            Instruction::F32Copysign => bytes.push(0x98),
            Instruction::F64Abs => bytes.push(0x99),
            Instruction::F64Neg => bytes.push(0x9A),
            Instruction::F64Ceil => bytes.push(0x9B),
            Instruction::F64Floor => bytes.push(0x9C),
            Instruction::F64Trunc => bytes.push(0x9D),
            Instruction::F64Nearest => bytes.push(0x9E),
            Instruction::F64Sqrt => bytes.push(0x9F),
            Instruction::F64Add => bytes.push(0xA0),
            Instruction::F64Sub => bytes.push(0xA1),
            Instruction::F64Mul => bytes.push(0xA2),
            Instruction::F64Div => bytes.push(0xA3),
            Instruction::F64Min => bytes.push(0xA4),
            Instruction::F64Max => bytes.push(0xA5),
            Instruction::F64Copysign => bytes.push(0xA6),
            Instruction::I32WrapI64 => bytes.push(0xA7),
            Instruction::I32TruncF32S => bytes.push(0xA8),
            Instruction::I32TruncF32U => bytes.push(0xA9),
            Instruction::I32TruncF64S => bytes.push(0xAA),
            Instruction::I32TruncF64U => bytes.push(0xAB),
            Instruction::I64ExtendI32S => bytes.push(0xAC),
            Instruction::I64ExtendI32U => bytes.push(0xAD),
            Instruction::I64TruncF32S => bytes.push(0xAE),
            Instruction::I64TruncF32U => bytes.push(0xAF),
            Instruction::I64TruncF64S => bytes.push(0xB0),
            Instruction::I64TruncF64U => bytes.push(0xB1),
            Instruction::F32ConvertI32S => bytes.push(0xB2),
            Instruction::F32ConvertI32U => bytes.push(0xB3),
            Instruction::F32ConvertI64S => bytes.push(0xB4),
            Instruction::F32ConvertI64U => bytes.push(0xB5),
            Instruction::F32DemoteF64 => bytes.push(0xB6),
            Instruction::F64ConvertI32S => bytes.push(0xB7),
            Instruction::F64ConvertI32U => bytes.push(0xB8),
            Instruction::F64ConvertI64S => bytes.push(0xB9),
            Instruction::F64ConvertI64U => bytes.push(0xBA),
            Instruction::F64PromoteF32 => bytes.push(0xBB),
            Instruction::I32ReinterpretF32 => bytes.push(0xBC),
            Instruction::I64ReinterpretF64 => bytes.push(0xBD),
            Instruction::F32ReinterpretI32 => bytes.push(0xBE),
            Instruction::F64ReinterpretI64 => bytes.push(0xBF),
            Instruction::I32Extend8S => bytes.push(0xC0),
            Instruction::I32Extend16S => bytes.push(0xC1),
            Instruction::I64Extend8S => bytes.push(0xC2),
            Instruction::I64Extend16S => bytes.push(0xC3),
            Instruction::I64Extend32S => bytes.push(0xC4),

            Instruction::I32TruncSatF32S => {
                bytes.push(0xFC);
                bytes.extend(encoders::u32(0));
            }
            Instruction::I32TruncSatF32U => {
                bytes.push(0xFC);
                bytes.extend(encoders::u32(1));
            }
            Instruction::I32TruncSatF64S => {
                bytes.push(0xFC);
                bytes.extend(encoders::u32(2));
            }
            Instruction::I32TruncSatF64U => {
                bytes.push(0xFC);
                bytes.extend(encoders::u32(3));
            }
            Instruction::I64TruncSatF32S => {
                bytes.push(0xFC);
                bytes.extend(encoders::u32(4));
            }
            Instruction::I64TruncSatF32U => {
                bytes.push(0xFC);
                bytes.extend(encoders::u32(5));
            }
            Instruction::I64TruncSatF64S => {
                bytes.push(0xFC);
                bytes.extend(encoders::u32(6));
            }
            Instruction::I64TruncSatF64U => {
                bytes.push(0xFC);
                bytes.extend(encoders::u32(7));
            }

            // Reference types instructions.
            Instruction::RefNull(ty) => {
                bytes.push(0xd0);
                bytes.push(ty.into());
            }
            Instruction::RefIsNull => bytes.push(0xd1),
            Instruction::RefFunc(f) => {
                bytes.push(0xd2);
                bytes.extend(encoders::u32(f));
            }

            // Bulk memory instructions.
            Instruction::TableInit { segment, table } => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(0x0c));
                bytes.extend(encoders::u32(segment));
                bytes.extend(encoders::u32(table));
            }
            Instruction::ElemDrop { segment } => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(0x0d));
                bytes.extend(encoders::u32(segment));
            }
            Instruction::TableCopy { src, dst } => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(0x0e));
                bytes.extend(encoders::u32(dst));
                bytes.extend(encoders::u32(src));
            }
            Instruction::TableGrow { table } => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(0x0f));
                bytes.extend(encoders::u32(table));
            }
            Instruction::TableSize { table } => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(0x10));
                bytes.extend(encoders::u32(table));
            }
            Instruction::TableFill { table } => {
                bytes.push(0xfc);
                bytes.extend(encoders::u32(0x11));
                bytes.extend(encoders::u32(table));
            }

            // SIMD instructions.
            Instruction::V128Const(x) => {
                bytes.push(0xFD);
                bytes.extend(encoders::u32(12));
                bytes.extend(x.to_le_bytes().iter().copied());
            }
        }
    }
}
