/* Copyright 2017 Mozilla Foundation
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
// See https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md

use std::mem::transmute;
use std::result;

type Result<T> = result::Result<T, &'static str>;

#[derive(Debug)]
pub enum CustomSectionKind {
    Unknown,
    Name,
    SourceMappingURL,
    Reloc,
    Linking,
}

#[derive(Debug)]
pub enum SectionCode<'a> {
    Custom { name: &'a [u8], kind: CustomSectionKind },
    Type, // Function signature declarations
    Import, // Import declarations
    Function, // Function declarations
    Table, // Indirect function table and other tables
    Memory, // Memory attributes
    Global, // Global declarations
    Export, //Exports
    Start, // Start function declaration
    Element, // Elements section
    Code, // Function bodies (code)
    Data, // Data segments
}

impl<'a> SectionCode<'a> {
    pub fn from_u32(code: u32) -> Result<SectionCode<'a>> {
        match code {
            0 => Err("Custom section: use SectionCode::Custom"),
            1 => Ok(SectionCode::Type),
            2 => Ok(SectionCode::Import),
            3 => Ok(SectionCode::Function),
            4 => Ok(SectionCode::Table),
            5 => Ok(SectionCode::Memory),
            6 => Ok(SectionCode::Global),
            7 => Ok(SectionCode::Export),
            8 => Ok(SectionCode::Start),
            9 => Ok(SectionCode::Element),
            10 => Ok(SectionCode::Code),
            11 => Ok(SectionCode::Data),
            _ => Err("Invalid section code"),
        }
    }
    pub fn is_custom_section_code(code: u32) -> bool {
        code == 0
    }
    pub fn is_known_section_code(code: u32) -> bool {
        code <= 11
    }
}

#[derive(Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    AnyFunc,
    Func,
    EmptyBlockType,
}

impl Type {
    pub fn from_i7(code: i32) -> Result<Type> {
        match code {
            -0x01 => Ok(Type::I32),
            -0x02 => Ok(Type::I64),
            -0x03 => Ok(Type::F32),
            -0x04 => Ok(Type::F64),
            -0x10 => Ok(Type::AnyFunc),
            -0x20 => Ok(Type::Func),
            -0x40 => Ok(Type::EmptyBlockType),
            _ => Err("Invalid type"),
        }
    }
}

#[derive(Debug)]
pub enum NameType {
    Module,
    Function,
    Local,
}

impl NameType {
    pub fn from_u7(code: u32) -> Result<NameType> {
        match code {
            0 => Ok(NameType::Module),
            1 => Ok(NameType::Function),
            2 => Ok(NameType::Local),
            _ => Err("Invalid name type"),
        }
    }
}

#[derive(Debug)]
pub struct Naming<'a> {
    index: u32,
    name: &'a [u8],
}

#[derive(Debug)]
pub struct LocalName<'a> {
    index: u32,
    locals: Vec<Naming<'a>>,
}

#[derive(Debug)]
pub enum NameEntry<'a> {
    Module(&'a [u8]),
    Function(Vec<Naming<'a>>),
    Local(Vec<LocalName<'a>>),
}

#[derive(Debug)]
pub enum ExternalKind {
    Function,
    Table,
    Memory,
    Global,
}

impl ExternalKind {
    pub fn from_u8(code: u32) -> Result<ExternalKind> {
        match code {
            0 => Ok(ExternalKind::Function),
            1 => Ok(ExternalKind::Table),
            2 => Ok(ExternalKind::Memory),
            3 => Ok(ExternalKind::Global),
            _ => Err("Invalid external kind"),
        }
    }
}

#[derive(Debug)]
pub struct FuncType {
    form: Type,
    params: Vec<Type>,
    returns: Vec<Type>,
}

#[derive(Debug)]
pub struct ResizableLimits {
    flags: u32,
    initial: u32,
    maximum: Option<u32>,
}

#[derive(Debug)]
pub struct TableType {
    element_type: Type,
    limits: ResizableLimits,
}

#[derive(Debug)]
pub struct MemoryType {
    limits: ResizableLimits,
}

#[derive(Debug)]
pub struct GlobalType {
    content_type: Type,
    mutability: u32,
}

#[derive(Debug)]
pub struct MemoryImmediate {
    flags: u32,
    offset: u32,
}

#[derive(Debug)]
pub struct BrTable {
    targets_table: Vec<u32>,
    default_target: u32,
}

#[derive(Debug)]
pub enum ImportSectionEntryType {
    Function(u32),
    Table(TableType),
    Memory(MemoryType),
    Global(GlobalType),
}

#[derive(Debug)]
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

impl RelocType {
    pub fn from_u7(code: u32) -> Result<RelocType> {
        match code {
            0 => Ok(RelocType::FunctionIndexLEB),
            1 => Ok(RelocType::TableIndexSLEB),
            2 => Ok(RelocType::TableIndexI32),
            3 => Ok(RelocType::GlobalAddrLEB),
            4 => Ok(RelocType::GlobalAddrSLEB),
            5 => Ok(RelocType::GlobalAddrI32),
            6 => Ok(RelocType::TypeIndexLEB),
            7 => Ok(RelocType::GlobalIndexLEB),
            _ => Err("Invalid reloc type"),
        }
    }
}

#[derive(Debug)]
pub enum LinkingType {
    StackPointer(u32),
}

#[derive(Debug)]
pub struct RelocEntry {
    ty: RelocType,
    offset: u32,
    index: u32,
    addend: Option<u32>,
}

#[derive(Debug)]
pub enum Operator {
    Unreachable,
    Nop,
    Block(Type),
    Loop(Type),
    If(Type),
    Else,
    End,
    Br(u32),
    BrIf(u32),
    BrTable(BrTable),
    Return,
    Call(u32),
    CallIndirect { index: u32, table_index: u32 },
    Drop,
    Select,
    GetLocal(u32),
    SetLocal(u32),
    TeeLocal(u32),
    GetGlobal(u32),
    SetGlobal(u32),
    I32Load(MemoryImmediate),
    I64Load(MemoryImmediate),
    F32Load(MemoryImmediate),
    F64Load(MemoryImmediate),
    I32Load8S(MemoryImmediate),
    I32Load8U(MemoryImmediate),
    I32Load16S(MemoryImmediate),
    I32Load16U(MemoryImmediate),
    I64Load8S(MemoryImmediate),
    I64Load8U(MemoryImmediate),
    I64Load16S(MemoryImmediate),
    I64Load16U(MemoryImmediate),
    I64Load32S(MemoryImmediate),
    I64Load32U(MemoryImmediate),
    I32Store(MemoryImmediate),
    I64Store(MemoryImmediate),
    F32Store(MemoryImmediate),
    F64Store(MemoryImmediate),
    I32Store8(MemoryImmediate),
    I32Store16(MemoryImmediate),
    I64Store8(MemoryImmediate),
    I64Store16(MemoryImmediate),
    I64Store32(MemoryImmediate),
    CurrentMemory(u32),
    GrowMemory(u32),
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
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
    I32TruncSF32,
    I32TruncUF32,
    I32TruncSF64,
    I32TruncUF64,
    I64ExtendSI32,
    I64ExtendUI32,
    I64TruncSF32,
    I64TruncUF32,
    I64TruncSF64,
    I64TruncUF64,
    F32ConvertSI32,
    F32ConvertUI32,
    F32ConvertSI64,
    F32ConvertUI64,
    F32DemoteF64,
    F64ConvertSI32,
    F64ConvertUI32,
    F64ConvertSI64,
    F64ConvertUI64,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
}

fn is_name(name: &[u8], expected: &'static str) -> bool {
    if name.len() != expected.len() {
        return false;
    }
    let expected_bytes = expected.as_bytes();
    for i in 0..name.len() {
        if name[i] != expected_bytes[i] {
            return false;
        }
    }
    true
}

fn is_name_prefix(name: &[u8], prefix: &'static str) -> bool {
    if name.len() < prefix.len() {
        return false;
    }
    let expected_bytes = prefix.as_bytes();
    for i in 0..expected_bytes.len() {
        if name[i] != expected_bytes[i] {
            return false;
        }
    }
    true
}

#[derive(Debug)]
pub enum ParserState<'a> {
    Error(&'a str),
    Initial,
    BeginWasm { magic_number: u32, version: u32, },
    EndWasm,
    BeginSection(SectionCode<'a>),
    EndSection,
    SkippingSection,
    ReadingSectionRawData,
    SectionRawData,

    TypeSectionEnty(FuncType),
    ImportSectionEntry { module: &'a [u8], field: &'a [u8], ty: ImportSectionEntryType, },
    FunctionSectionEnty(u32),
    TableSectionEntry(TableType),
    MemorySectionEntry(MemoryType),
    GlobalSectionEntry,
    ExportSectionEntry { field: &'a [u8], kind: ExternalKind, index: u32, },
    DataSectionEntry,
    NameSectionEntry(NameEntry<'a>),
    StartSectionEntry(u32),

    BeginInitExpressionBody,
    InitExpressionOperator(Operator),
    EndInitExpressionBody,

    BeginFunctionBody(Vec<(u32, Type)>),
    CodeOperator(Operator),
    EndFunctionBody,
    SkippingFunctionBody,

    BeginElementSectionEntry(u32),
    ElementSectionEntryBody(Vec<u32>),
    EndElementSectionEntry,

    BeginDataSectionEntry(u32),
    DataSectionEntryBody(&'a [u8]),
    EndDataSectionEntry,

    BeginGlobalSectionEntry(GlobalType),
    EndGlobalSectionEntry,

    RelocSectionHeader(SectionCode<'a>),
    RelocSectionEntry(RelocEntry),
    LinkingSectionEntry(LinkingType),

    SourceMappingURL(&'a [u8]),
}

enum InitExpressionContinuation {
    GlobalSection,
    ElementSection,
    DataSection,
}

pub struct Parser<'a> {
    buffer: &'a [u8],
    position: usize,
    end: usize,
    state: ParserState<'a>,
    section_range: Option<(usize, usize)>,
    function_range: Option<(usize, usize)>,
    init_expr_continuation: Option<InitExpressionContinuation>,
    section_entries_left: u32,
}

const WASM_MAGIC_NUMBER: u32 = 0x6d736100;
const WASM_EXPERIMENTAL_VERSION: u32 = 0xd;
const WASM_SUPPORTED_VERSION: u32 = 0x1;

impl<'a> Parser<'a> {
    pub fn new(data: &[u8]) -> Parser {
        Parser {
            buffer: data,
            position: 0,
            end: data.len(),
            state: ParserState::Initial,
            section_range: None,
            function_range: None,
            init_expr_continuation: None,
            section_entries_left: 0,
        }
    }

    fn ensure_has_bytes(&mut self, len: usize) -> Result<()> {
        if self.position + len <= self.end {
            Ok(())
        } else {
            Err("Unexpected EOF")
        }
    }

    fn read_u32(&mut self) -> Result<u32> {
        self.ensure_has_bytes(4)?;
        let b1 = self.buffer[self.position] as u32;
        let b2 = self.buffer[self.position + 1] as u32;
        let b3 = self.buffer[self.position + 2] as u32;
        let b4 = self.buffer[self.position + 3] as u32;
        self.position += 4;
        Ok(b1 | (b2 << 8) | (b3 << 16) | (b4 << 24))
    }

    fn peek_u32(&self) -> Option<u32> {
        if self.position + 4 > self.end {
            return None;
        }
        let b1 = self.buffer[self.position] as u32;
        let b2 = self.buffer[self.position + 1] as u32;
        let b3 = self.buffer[self.position + 2] as u32;
        let b4 = self.buffer[self.position + 3] as u32;
        Some(b1 | (b2 << 8) | (b3 << 16) | (b4 << 24))
    }

    fn read_u64(&mut self) -> Result<u64> {
        let w1 = self.read_u32()? as u64;
        let w2 = self.read_u32()? as u64;
        Ok(w1 | (w2 << 32))
    }

    fn read_u8(&mut self) -> Result<u32> {
        self.ensure_has_bytes(1)?;
        let b = self.buffer[self.position] as u32;
        self.position += 1;
        Ok(b)
    }

    fn read_var_u1(&mut self) -> Result<u32> {
        let b = self.read_u8()?;
        if (b & 0xFE) != 0 {
            return Err("Invalid var_u1");
        }
        Ok(b)
    }

    fn read_var_i7(&mut self) -> Result<i32> {
        let b = self.read_u8()?;
        if (b & 0x80) != 0 {
            return Err("Invalid var_i7");
        }
        Ok((b << 25) as i32 >> 25)
    }

    fn read_var_u7(&mut self) -> Result<u32> {
        let b = self.read_u8()?;
        if (b & 0x80) != 0 {
            return Err("Invalid var_u7");
        }
        Ok(b)
    }

    fn read_var_u32(&mut self) -> Result<u32> {
        let mut result = 0;
        let mut shift = 0;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as u32) << shift;
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
            if shift >= 32 {
                return Err("Invalid var_u32");
            }
        }
        Ok(result)
    }

    fn read_var_i32(&mut self) -> Result<i32> {
        let mut result: i32 = 0;
        let mut shift = 0;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as i32) << shift;
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
            if shift >= 32 {
                return Err("Invalid var_i32");
            }
        }
        if shift >= 32 {
            return Ok(result);
        }
        let ashift = 32 - shift;
        Ok((result << ashift) >> ashift)
    }

    fn read_var_i64(&mut self) -> Result<i64> {
        let mut result: i64 = 0;
        let mut shift = 0;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as i64) << shift;
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
            if shift >= 64 {
                return Err("Invalid var_i64");
            }
        }
        if shift >= 64 {
            return Ok(result);
        }
        let ashift = 64 - shift;
        Ok((result << ashift) >> ashift)
    }

    fn read_f32(&mut self) -> Result<f32> {
        let value = self.read_u32()?;
        Ok(unsafe { transmute::<u32, f32>(value) })
    }

    fn read_f64(&mut self) -> Result<f64> {
        let value = self.read_u64()?;
        Ok(unsafe { transmute::<u64, f64>(value) })
    }

    fn read_string(&mut self) -> Result<&'a [u8]> {
        let len = self.read_var_u32()? as usize;
        let start = self.position;
        self.ensure_has_bytes(len)?;
        self.position += len;
        Ok(&self.buffer[start..self.position])
    }

    fn read_type(&mut self) -> Result<Type> {
        Type::from_i7(self.read_var_i7()?)
    }

    fn read_external_kind(&mut self) -> Result<ExternalKind> {
        ExternalKind::from_u8(self.read_u8()?)
    }

    fn read_func_type(&mut self) -> Result<FuncType> {
        let form = self.read_type()?;
        let params_len = self.read_var_u32()? as usize;
        let mut params: Vec<Type> = Vec::with_capacity(params_len);
        for _ in 0..params_len {
            params.push(self.read_type()?);
        }
        let returns_len = self.read_var_u32()? as usize;
        let mut returns: Vec<Type> = Vec::with_capacity(returns_len);
        for _ in 0..returns_len {
            returns.push(self.read_type()?);
        }
        Ok(FuncType {
            form: form,
            params: params,
            returns: returns,
        })
    }

    fn read_resizable_limits(&mut self) -> Result<ResizableLimits> {
        let flags = self.read_var_u32()?;
        let initial = self.read_var_u32()?;
        let maximum: Option<u32>;
        if (flags & 0x1) != 0 {
            maximum = Some(self.read_var_u32()?);
        } else {
            maximum = None;
        }
        Ok(ResizableLimits {
            flags: flags,
            initial: initial,
            maximum: maximum,
        })
    }

    fn read_table_type(&mut self) -> Result<TableType> {
        Ok(TableType {
            element_type: self.read_type()?,
            limits: self.read_resizable_limits()?,
        })
    }

    fn read_memory_type(&mut self) -> Result<MemoryType> {
        Ok(MemoryType { limits: self.read_resizable_limits()? })
    }

    fn read_global_type(&mut self) -> Result<GlobalType> {
        Ok(GlobalType {
            content_type: self.read_type()?,
            mutability: self.read_var_u1()?,
        })
    }

    fn read_memory_immediate(&mut self) -> Result<MemoryImmediate> {
        Ok(MemoryImmediate {
            flags: self.read_var_u32()?,
            offset: self.read_var_u32()?,
        })
    }

    fn read_header(&mut self) -> Result<()> {
        let magic_number = self.read_u32()?;
        if magic_number != WASM_MAGIC_NUMBER {
            return Err("Bad magic number");
        }
        let version = self.read_u32()?;
        if version != WASM_SUPPORTED_VERSION && version != WASM_EXPERIMENTAL_VERSION {
            return Err("Bad version number");
        }
        self.state = ParserState::BeginWasm {
            magic_number: magic_number,
            version: version,
        };
        Ok(())
    }

    fn read_section_code(&mut self, id: u32) -> Result<SectionCode<'a>> {
        assert!(SectionCode::is_known_section_code(id));
        let code;
        if SectionCode::is_custom_section_code(id) {
            let name = self.read_string()?;
            let kind = if is_name(name, "name") {
                CustomSectionKind::Name
            } else if is_name(name, "sourceMappingURL") {
                CustomSectionKind::SourceMappingURL
            } else if is_name_prefix(name, "reloc.") {
                CustomSectionKind::Reloc
            } else if is_name(name, "linking") {
                CustomSectionKind::Linking
            } else {
                CustomSectionKind::Unknown
            };
            code = SectionCode::Custom {
                name: name,
                kind: kind,
            };
        } else {
            code = SectionCode::from_u32(id)?;
        }
        Ok(code)
    }

    fn read_section_header(&mut self) -> Result<()> {
        let id = self.read_var_u7()?;
        if !SectionCode::is_known_section_code(id) {
            return Err("Unknown section code");
        }
        let payload_len = self.read_var_u32()? as usize;
        let payload_end = self.position + payload_len;
        self.state = ParserState::BeginSection(self.read_section_code(id)?);
        self.section_range = Some((self.position, payload_end));
        Ok(())
    }

    fn read_type_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        self.state = ParserState::TypeSectionEnty(self.read_func_type()?);
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_import_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        let module = self.read_string()?;
        let field = self.read_string()?;
        let kind = self.read_external_kind()?;
        let ty: ImportSectionEntryType;
        match kind {
            ExternalKind::Function => ty = ImportSectionEntryType::Function(self.read_var_u32()?),
            ExternalKind::Table => ty = ImportSectionEntryType::Table(self.read_table_type()?),
            ExternalKind::Memory => ty = ImportSectionEntryType::Memory(self.read_memory_type()?),
            ExternalKind::Global => ty = ImportSectionEntryType::Global(self.read_global_type()?),
        }

        self.state = ParserState::ImportSectionEntry {
            module: module,
            field: field,
            ty: ty,
        };
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_function_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        self.state = ParserState::FunctionSectionEnty(self.read_var_u32()?);
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_memory_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        self.state = ParserState::MemorySectionEntry(self.read_memory_type()?);
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_global_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        self.state = ParserState::BeginGlobalSectionEntry(self.read_global_type()?);
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_init_expression_body(&mut self, cont: InitExpressionContinuation) {
        self.state = ParserState::BeginInitExpressionBody;
        self.init_expr_continuation = Some(cont);
    }

    fn read_br_table(&mut self) -> Result<BrTable> {
        let targets_len = self.read_var_u32()? as usize;
        let mut targets_table = Vec::with_capacity(targets_len);
        for _ in 0..targets_len {
            targets_table.push(self.read_var_u32()?);
        }
        let default_target = self.read_var_u32()?;
        Ok(BrTable {
            targets_table: targets_table,
            default_target: default_target,
        })
    }

    fn read_operator(&mut self) -> Result<Operator> {
        let code = self.read_u8()?;
        Ok(match code {
            0x00 => Operator::Unreachable,
            0x01 => Operator::Nop,
            0x02 => Operator::Block(self.read_type()?),
            0x03 => Operator::Loop(self.read_type()?),
            0x04 => Operator::If(self.read_type()?),
            0x05 => Operator::Else,
            0x0b => Operator::End,
            0x0c => Operator::Br(self.read_var_u32()?),
            0x0d => Operator::BrIf(self.read_var_u32()?),
            0x0e => Operator::BrTable(self.read_br_table()?),
            0x0f => Operator::Return,
            0x10 => Operator::Call(self.read_var_u32()?),
            0x11 => Operator::CallIndirect { index: self.read_var_u32()?, table_index: self.read_var_u1()?, },
            0x1a => Operator::Drop,
            0x1b => Operator::Select,
            0x20 => Operator::GetLocal(self.read_var_u32()?),
            0x21 => Operator::SetLocal(self.read_var_u32()?),
            0x22 => Operator::TeeLocal(self.read_var_u32()?),
            0x23 => Operator::GetGlobal(self.read_var_u32()?),
            0x24 => Operator::SetGlobal(self.read_var_u32()?),
            0x28 => Operator::I32Load(self.read_memory_immediate()?),
            0x29 => Operator::I64Load(self.read_memory_immediate()?),
            0x2a => Operator::F32Load(self.read_memory_immediate()?),
            0x2b => Operator::F64Load(self.read_memory_immediate()?),
            0x2c => Operator::I32Load8S(self.read_memory_immediate()?),
            0x2d => Operator::I32Load8U(self.read_memory_immediate()?),
            0x2e => Operator::I32Load16S(self.read_memory_immediate()?),
            0x2f => Operator::I32Load16U(self.read_memory_immediate()?),
            0x30 => Operator::I64Load8S(self.read_memory_immediate()?),
            0x31 => Operator::I64Load8U(self.read_memory_immediate()?),
            0x32 => Operator::I64Load16S(self.read_memory_immediate()?),
            0x33 => Operator::I64Load16U(self.read_memory_immediate()?),
            0x34 => Operator::I64Load32S(self.read_memory_immediate()?),
            0x35 => Operator::I64Load32U(self.read_memory_immediate()?),
            0x36 => Operator::I32Store(self.read_memory_immediate()?),
            0x37 => Operator::I64Store(self.read_memory_immediate()?),
            0x38 => Operator::F32Store(self.read_memory_immediate()?),
            0x39 => Operator::F64Store(self.read_memory_immediate()?),
            0x3a => Operator::I32Store8(self.read_memory_immediate()?),
            0x3b => Operator::I32Store16(self.read_memory_immediate()?),
            0x3c => Operator::I64Store8(self.read_memory_immediate()?),
            0x3d => Operator::I64Store16(self.read_memory_immediate()?),
            0x3e => Operator::I64Store32(self.read_memory_immediate()?),
            0x3f => Operator::CurrentMemory(self.read_var_u1()?),
            0x40 => Operator::GrowMemory(self.read_var_u1()?),
            0x41 => Operator::I32Const(self.read_var_i32()?),
            0x42 => Operator::I64Const(self.read_var_i64()?),
            0x43 => Operator::F32Const(self.read_f32()?),
            0x44 => Operator::F64Const(self.read_f64()?),
            0x45 => Operator::I32Eqz,
            0x46 => Operator::I32Eq,
            0x47 => Operator::I32Ne,
            0x48 => Operator::I32LtS,
            0x49 => Operator::I32LtU,
            0x4a => Operator::I32GtS,
            0x4b => Operator::I32GtU,
            0x4c => Operator::I32LeS,
            0x4d => Operator::I32LeU,
            0x4e => Operator::I32GeS,
            0x4f => Operator::I32GeU,
            0x50 => Operator::I64Eqz,
            0x51 => Operator::I64Eq,
            0x52 => Operator::I64Ne,
            0x53 => Operator::I64LtS,
            0x54 => Operator::I64LtU,
            0x55 => Operator::I64GtS,
            0x56 => Operator::I64GtU,
            0x57 => Operator::I64LeS,
            0x58 => Operator::I64LeU,
            0x59 => Operator::I64GeS,
            0x5a => Operator::I64GeU,
            0x5b => Operator::F32Eq,
            0x5c => Operator::F32Ne,
            0x5d => Operator::F32Lt,
            0x5e => Operator::F32Gt,
            0x5f => Operator::F32Le,
            0x60 => Operator::F32Ge,
            0x61 => Operator::F64Eq,
            0x62 => Operator::F64Ne,
            0x63 => Operator::F64Lt,
            0x64 => Operator::F64Gt,
            0x65 => Operator::F64Le,
            0x66 => Operator::F64Ge,
            0x67 => Operator::I32Clz,
            0x68 => Operator::I32Ctz,
            0x69 => Operator::I32Popcnt,
            0x6a => Operator::I32Add,
            0x6b => Operator::I32Sub,
            0x6c => Operator::I32Mul,
            0x6d => Operator::I32DivS,
            0x6e => Operator::I32DivU,
            0x6f => Operator::I32RemS,
            0x70 => Operator::I32RemU,
            0x71 => Operator::I32And,
            0x72 => Operator::I32Or,
            0x73 => Operator::I32Xor,
            0x74 => Operator::I32Shl,
            0x75 => Operator::I32ShrS,
            0x76 => Operator::I32ShrU,
            0x77 => Operator::I32Rotl,
            0x78 => Operator::I32Rotr,
            0x79 => Operator::I64Clz,
            0x7a => Operator::I64Ctz,
            0x7b => Operator::I64Popcnt,
            0x7c => Operator::I64Add,
            0x7d => Operator::I64Sub,
            0x7e => Operator::I64Mul,
            0x7f => Operator::I64DivS,
            0x80 => Operator::I64DivU,
            0x81 => Operator::I64RemS,
            0x82 => Operator::I64RemU,
            0x83 => Operator::I64And,
            0x84 => Operator::I64Or,
            0x85 => Operator::I64Xor,
            0x86 => Operator::I64Shl,
            0x87 => Operator::I64ShrS,
            0x88 => Operator::I64ShrU,
            0x89 => Operator::I64Rotl,
            0x8a => Operator::I64Rotr,
            0x8b => Operator::F32Abs,
            0x8c => Operator::F32Neg,
            0x8d => Operator::F32Ceil,
            0x8e => Operator::F32Floor,
            0x8f => Operator::F32Trunc,
            0x90 => Operator::F32Nearest,
            0x91 => Operator::F32Sqrt,
            0x92 => Operator::F32Add,
            0x93 => Operator::F32Sub,
            0x94 => Operator::F32Mul,
            0x95 => Operator::F32Div,
            0x96 => Operator::F32Min,
            0x97 => Operator::F32Max,
            0x98 => Operator::F32Copysign,
            0x99 => Operator::F64Abs,
            0x9a => Operator::F64Neg,
            0x9b => Operator::F64Ceil,
            0x9c => Operator::F64Floor,
            0x9d => Operator::F64Trunc,
            0x9e => Operator::F64Nearest,
            0x9f => Operator::F64Sqrt,
            0xa0 => Operator::F64Add,
            0xa1 => Operator::F64Sub,
            0xa2 => Operator::F64Mul,
            0xa3 => Operator::F64Div,
            0xa4 => Operator::F64Min,
            0xa5 => Operator::F64Max,
            0xa6 => Operator::F64Copysign,
            0xa7 => Operator::I32WrapI64,
            0xa8 => Operator::I32TruncSF32,
            0xa9 => Operator::I32TruncUF32,
            0xaa => Operator::I32TruncSF64,
            0xab => Operator::I32TruncUF64,
            0xac => Operator::I64ExtendSI32,
            0xad => Operator::I64ExtendUI32,
            0xae => Operator::I64TruncSF32,
            0xaf => Operator::I64TruncUF32,
            0xb0 => Operator::I64TruncSF64,
            0xb1 => Operator::I64TruncUF64,
            0xb2 => Operator::F32ConvertSI32,
            0xb3 => Operator::F32ConvertUI32,
            0xb4 => Operator::F32ConvertSI64,
            0xb5 => Operator::F32ConvertUI64,
            0xb6 => Operator::F32DemoteF64,
            0xb7 => Operator::F64ConvertSI32,
            0xb8 => Operator::F64ConvertUI32,
            0xb9 => Operator::F64ConvertSI64,
            0xba => Operator::F64ConvertUI64,
            0xbb => Operator::F64PromoteF32,
            0xbc => Operator::I32ReinterpretF32,
            0xbd => Operator::I64ReinterpretF64,
            0xbe => Operator::F32ReinterpretI32,
            0xbf => Operator::F64ReinterpretI64,
            _ => return Err("Unknown opcode"),
        })
    }

    fn read_init_expression_operator(&mut self) -> Result<()> {
        let op = self.read_operator()?;
        if let Operator::End = op {
            self.state = ParserState::EndInitExpressionBody;
            return Ok(());
        }
        self.state = ParserState::InitExpressionOperator(op);
        Ok(())
    }

    fn read_export_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        let field = self.read_string()?;
        let kind = self.read_external_kind()?;
        let index = self.read_var_u32()?;
        self.state = ParserState::ExportSectionEntry {
            field: field,
            kind: kind,
            index: index
        };
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_element_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        self.state = ParserState::BeginElementSectionEntry(self.read_var_u32()?);
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_element_entry_body(&mut self) -> Result<()> {
        let num_elements = self.read_var_u32()? as usize;
        let mut elements: Vec<u32> = Vec::with_capacity(num_elements);
        for _ in 0..num_elements {
            elements.push(self.read_var_u32()?);
        }
        self.state = ParserState::ElementSectionEntryBody(elements);
        Ok(())
    }

    fn read_function_body(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        let size = self.read_var_u32()? as usize;
        let body_end = self.position + size;
        let local_count = self.read_var_u32()? as usize;
        let mut locals: Vec<(u32, Type)> = Vec::with_capacity(local_count);
        for _ in 0..local_count {
            let count = self.read_var_u32()?;
            let ty = self.read_type()?;
            locals.push((count, ty));
        }
        self.state = ParserState::BeginFunctionBody(locals);
        self.function_range = Some((self.position, body_end));
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_code_operator(&mut self) -> Result<()> {
        let op = self.read_operator()?;
        if self.position >= self.function_range.unwrap().1 {
            self.state = ParserState::EndFunctionBody;
            self.function_range = None;
            return Ok(());
        }
        self.state = ParserState::CodeOperator(op);
        Ok(())
    }

    fn read_table_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        self.state = ParserState::TableSectionEntry(self.read_table_type()?);
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_data_entry(&mut self) -> Result<()>  {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        let index = self.read_var_u32()?;
        self.state = ParserState::BeginDataSectionEntry(index);
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_data_entry_body(&mut self) -> Result<()> {
        self.state = ParserState::DataSectionEntryBody(self.read_string()?);
        Ok(())
    }

    fn read_name_map(&mut self) -> Result<Vec<Naming<'a>>> {
        let count = self.read_var_u32()? as usize;
        let mut result = Vec::with_capacity(count);
        for _ in 0..count {
            let index = self.read_var_u32()?;
            let name = self.read_string()?;
            result.push(Naming {
                            index: index,
                            name: name,
                        });
        }
        Ok(result)
    }

    fn read_name_entry(&mut self) -> Result<()> {
        if self.position >= self.section_range.unwrap().1 {
            return self.position_to_section_end();
        }
        let ty = NameType::from_u7(self.read_var_u7()?)?;
        self.read_var_u32()?; // payload_len
        let entry = match ty {
            NameType::Module => NameEntry::Module(self.read_string()?),
            NameType::Function => NameEntry::Function(self.read_name_map()?),
            NameType::Local => {
                let funcs_len = self.read_var_u32()? as usize;
                let mut funcs: Vec<LocalName<'a>> = Vec::with_capacity(funcs_len);
                for _ in 0..funcs_len {
                    funcs.push(LocalName {
                                   index: self.read_var_u32()?,
                                   locals: self.read_name_map()?,
                               });
                }
                NameEntry::Local(funcs)
            }
        };
        self.state = ParserState::NameSectionEntry(entry);
        Ok(())
    }

    fn read_source_mapping(&mut self) -> Result<()> {
        self.state = ParserState::SourceMappingURL(self.read_string()?);
        Ok(())
    }

    // See https://github.com/WebAssembly/tool-conventions/blob/master/Linking.md
    fn read_reloc_header(&mut self) -> Result<()> {
        let section_id = self.read_var_u7()?;
        if !SectionCode::is_known_section_code(section_id) {
            return Err("Unknown section code");
        }
        let section_code = self.read_section_code(section_id)?;
        self.state = ParserState::RelocSectionHeader(section_code);
        Ok(())
    }

    fn read_reloc_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        let ty = RelocType::from_u7(self.read_var_u7()?)?;
        let offset = self.read_var_u32()?;
        let index = self.read_var_u32()?;
        let addend = match ty {
            RelocType::FunctionIndexLEB |
            RelocType::TableIndexSLEB |
            RelocType::TableIndexI32 |
            RelocType::TypeIndexLEB |
            RelocType::GlobalIndexLEB => None,
            RelocType::GlobalAddrLEB |
            RelocType::GlobalAddrSLEB |
            RelocType::GlobalAddrI32 => {
                Some(self.read_var_u32()?)
            }
        };
        self.state = ParserState::RelocSectionEntry(RelocEntry {
            ty: ty,
            offset: offset,
            index: index,
            addend: addend,
        });
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_linking_entry(&mut self) -> Result<()> {
        if self.section_entries_left == 0 {
            return self.position_to_section_end();
        }
        let ty = self.read_var_u32()?;
        let entry = match ty {
            1 => LinkingType::StackPointer(self.read_var_u32()?),
            _ => { return Err("Invalid linking type"); }
        };
        self.state = ParserState::LinkingSectionEntry(entry);
        self.section_entries_left -= 1;
        Ok(())
    }

    fn read_section_body(&mut self) -> Result<()> {
        match self.state {
            ParserState::BeginSection(SectionCode::Type) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_type_entry()?;
            }
            ParserState::BeginSection(SectionCode::Import) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_import_entry()?;
            }
            ParserState::BeginSection(SectionCode::Function) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_function_entry()?;
            }
            ParserState::BeginSection(SectionCode::Memory) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_memory_entry()?;
            }
            ParserState::BeginSection(SectionCode::Global) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_global_entry()?;
            }
            ParserState::BeginSection(SectionCode::Export) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_export_entry()?;
            }
            ParserState::BeginSection(SectionCode::Element) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_element_entry()?;
            }
            ParserState::BeginSection(SectionCode::Code) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_function_body()?;
            }
            ParserState::BeginSection(SectionCode::Table) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_table_entry()?;
            }
            ParserState::BeginSection(SectionCode::Data) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_data_entry()?;
            }
            ParserState::BeginSection(SectionCode::Start) => {
                self.state = ParserState::StartSectionEntry(self.read_var_u32()?);
            }
            ParserState::BeginSection(SectionCode::Custom { kind: CustomSectionKind::Name, .. }) => {
                self.read_name_entry()?;
            }
            ParserState::BeginSection(SectionCode::Custom { kind: CustomSectionKind::SourceMappingURL, .. }) => {
                self.read_source_mapping()?;
            }
            ParserState::BeginSection(SectionCode::Custom { kind: CustomSectionKind::Reloc, .. }) => {
                self.read_reloc_header()?;
            }
            ParserState::BeginSection(SectionCode::Custom { kind: CustomSectionKind::Linking, .. }) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_linking_entry()?;
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn position_to_section_end(&mut self) -> Result<()> {
        if self.section_range.unwrap().1 > self.end {
            return Err("Position past the section end");
        }
        self.position = self.section_range.unwrap().1;
        self.section_range = None;
        self.state = ParserState::EndSection;
        Ok(())
    }

    fn read_wrapped(&mut self) -> Result<()> {
        match self.state {
            ParserState::Initial => self.read_header()?,
            ParserState::EndWasm => {
                assert!(self.position < self.end);
                self.read_header()?;
            }
            ParserState::BeginWasm { .. } |
            ParserState::EndSection => {
                if self.position >= self.end {
                    self.state = ParserState::EndWasm;
                } else if let Some(WASM_MAGIC_NUMBER) = self.peek_u32() {
                    self.state = ParserState::EndWasm;
                } else {
                    self.read_section_header()?;
                }
            }
            ParserState::BeginSection(_) => self.read_section_body()?,
            ParserState::SkippingSection => self.position_to_section_end()?,
            ParserState::TypeSectionEnty(_) => self.read_type_entry()?,
            ParserState::ImportSectionEntry { .. } => self.read_import_entry()?,
            ParserState::FunctionSectionEnty(_) => self.read_function_entry()?,
            ParserState::MemorySectionEntry(_) => self.read_memory_entry()?,
            ParserState::TableSectionEntry(_) => self.read_table_entry()?,
            ParserState::ExportSectionEntry { .. } => self.read_export_entry()?,
            ParserState::BeginGlobalSectionEntry(_) => {
                self.read_init_expression_body(InitExpressionContinuation::GlobalSection)
            }
            ParserState::EndGlobalSectionEntry => self.read_global_entry()?,
            ParserState::BeginElementSectionEntry(_) => {
                self.read_init_expression_body(InitExpressionContinuation::ElementSection)
            }
            ParserState::BeginInitExpressionBody |
            ParserState::InitExpressionOperator(_) => self.read_init_expression_operator()?,
            ParserState::BeginDataSectionEntry(_) => {
                self.read_init_expression_body(InitExpressionContinuation::DataSection)
            }
            ParserState::EndInitExpressionBody => {
                match self.init_expr_continuation {
                    Some(InitExpressionContinuation::GlobalSection) => {
                        self.state = ParserState::EndGlobalSectionEntry
                    }
                    Some(InitExpressionContinuation::ElementSection) => {
                        self.read_element_entry_body()?
                    }
                    Some(InitExpressionContinuation::DataSection) => self.read_data_entry_body()?,
                    _ => unreachable!(),
                }
                self.init_expr_continuation = None;
            }
            ParserState::BeginFunctionBody(_) |
            ParserState::CodeOperator(_) => self.read_code_operator()?,
            ParserState::EndFunctionBody => self.read_function_body()?,
            ParserState::SkippingFunctionBody => {
                assert!(self.position <= self.function_range.unwrap().1);
                self.position = self.function_range.unwrap().1;
                self.state = ParserState::EndFunctionBody;
                self.function_range = None;
            }
            ParserState::DataSectionEntryBody(_) => {
                self.state = ParserState::EndDataSectionEntry;
            }
            ParserState::EndDataSectionEntry => self.read_data_entry()?,
            ParserState::ElementSectionEntryBody(_) => {
                self.state = ParserState::EndElementSectionEntry;
            }
            ParserState::EndElementSectionEntry => self.read_element_entry()?,
            ParserState::StartSectionEntry(_) => self.position_to_section_end()?,
            ParserState::NameSectionEntry(_) => self.read_name_entry()?,
            ParserState::SourceMappingURL(_) => self.position_to_section_end()?,
            ParserState::RelocSectionHeader(_) => {
                self.section_entries_left = self.read_var_u32()?;
                self.read_reloc_entry()?;
            },
            ParserState::RelocSectionEntry(_) => self.read_reloc_entry()?,
            ParserState::LinkingSectionEntry(_) => self.read_linking_entry()?,
            _ => panic!("Invalid reader state"),
        }
        Ok(())
    }

    pub fn read(&mut self) -> Option<&ParserState> {
        match self.state {
            ParserState::EndWasm if self.position >= self.end => return None,
            ParserState::Error(_) => panic!("Parser in error state"),
            _ => {
                let result = self.read_wrapped();
                if let Err(msg) = result {
                    self.state = ParserState::Error(msg);
                }
                return Some(&self.state);
            }
        }
    }

    pub fn skip_section(&mut self) {
        match self.state {
            ParserState::Initial |
            ParserState::EndWasm |
            ParserState::Error(_) |
            ParserState::BeginWasm { .. } |
            ParserState::EndSection => return,
            _ => self.state = ParserState::SkippingSection,
        }
    }

    pub fn skip_function_body(&mut self) {
        self.state = ParserState::SkippingFunctionBody;
    }
}
