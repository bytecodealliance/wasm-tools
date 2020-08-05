//! A crate to convert a WebAssembly binary to its textual representation in the
//! WebAssembly Text Format (WAT).
//!
//! This crate is intended for developer toolchains and debugging, supporting
//! human-readable versions of a wasm binary. This can also be useful when
//! developing wasm toolchain support in Rust for various purposes like testing
//! and debugging and such.

#![deny(missing_docs)]

use anyhow::{bail, Context, Result};
use std::collections::HashMap;
use std::fmt::Write;
use std::mem;
use std::path::Path;
use wasmparser::*;

const MAX_LOCALS: u32 = 50000;

/// Reads a WebAssembly `file` from the filesystem and then prints it into an
/// in-memory `String`.
pub fn print_file(file: impl AsRef<Path>) -> Result<String> {
    _parse_file(file.as_ref())
}

fn _parse_file(file: &Path) -> Result<String> {
    let contents = std::fs::read(file).context(format!("failed to read `{}`", file.display()))?;
    Printer::new().print(&contents)
}

/// Prints an in-memory `wasm` binary blob into an in-memory `String` which is
/// its textual representation.
pub fn print_bytes(wasm: impl AsRef<[u8]>) -> Result<String> {
    Printer::new().print(wasm.as_ref())
}

/// Context used for printing a WebAssembly binary.
///
/// This is largely only required if you'd like to register custom printers for
/// custom sections in a wasm binary.
#[derive(Default)]
pub struct Printer {
    printers: HashMap<String, Box<dyn FnMut(&mut Printer, usize, &[u8]) -> Result<()>>>,
    result: String,
    state: ModuleState,
    nesting: u32,
}

#[derive(Default)]
struct ModuleState {
    func: u32,
    module: u32,
    instance: u32,
    memory: u32,
    global: u32,
    table: u32,
    types: Vec<Option<FuncType>>,
    names: HashMap<u32, String>,
    local_names: HashMap<u32, HashMap<u32, String>>,
    module_name: Option<String>,
}

impl Printer {
    /// Creates a new `Printer` object that's ready to start printing wasm
    /// binaries to strings.
    pub fn new() -> Printer {
        Printer::default()
    }

    /// Registers a custom `printer` function to get invoked whenever a custom
    /// section of name `section` is seen.
    ///
    /// This can be used to register printers into a textual format for custom
    /// sections, such as by emitting annotations and/or other textual
    /// references (maybe coments!)
    ///
    /// By default all custom sections are ignored for the text format.
    ///
    /// The `printer` function provided takes three arguments:
    ///
    /// * A `&mut Printer`, or where to print results to
    /// * A `usize` offset which is the start of the offset for the custom
    ///   section
    /// * A byte slice which is the actual contents of the custom section.
    pub fn add_custom_section_printer(
        &mut self,
        section: &str,
        printer: impl FnMut(&mut Printer, usize, &[u8]) -> Result<()> + 'static,
    ) {
        self.printers.insert(section.to_string(), Box::new(printer));
    }

    /// Gets the output result of this `Printer`, or where all output is going.
    pub fn result_mut(&mut self) -> &mut String {
        &mut self.result
    }

    /// Prints a WebAssembly binary into a `String`
    ///
    /// This function takes an entire `wasm` binary blob and will print it to
    /// the WebAssembly Text Format and return the result as a `String`.
    pub fn print(&mut self, wasm: &[u8]) -> Result<String> {
        self.start_group("module");
        self.print_contents(0, wasm, "")?;
        self.end_group();
        Ok(mem::take(&mut self.result))
    }

    fn print_contents(&mut self, offset: u64, wasm: &[u8], module_ty: &str) -> Result<()> {
        // First up try to find the `name` subsection which we'll use to print
        // pretty names everywhere. Also look for the `code` section so we can
        // print out functions as soon as we hit the function section.
        let mut code = None;
        let mut module_code = None;
        let mut parser = Parser::new(offset);
        let prev = mem::take(&mut self.state);
        let mut bytes = wasm;
        loop {
            let payload = match parser.parse(bytes, true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    payload
                }
            };
            match payload {
                Payload::CodeSectionStart { size, range, .. } => {
                    let offset = offset as usize;
                    let section = match wasm.get(range.start - offset..range.end - offset) {
                        Some(slice) => slice,
                        None => bail!("invalid code section"),
                    };
                    code = Some(CodeSectionReader::new(section, range.start)?);
                    parser.skip_section();
                    bytes = &bytes[size as usize..];
                }
                Payload::ModuleCodeSectionStart { size, range, .. } => {
                    let offset = offset as usize;
                    let section = match wasm.get(range.start - offset..range.end - offset) {
                        Some(slice) => slice,
                        None => bail!("invalid module code section"),
                    };
                    module_code = Some(ModuleCodeSectionReader::new(section, range.start)?);
                    parser.skip_section();
                    bytes = &bytes[size as usize..];
                }
                Payload::CustomSection {
                    name: "name",
                    data_offset,
                    data,
                } => {
                    let reader = NameSectionReader::new(data, data_offset)?;
                    self.register_names(reader)?;
                }
                Payload::End => break,
                _ => {}
            }
        }

        // ... and here we go, time to print all the sections!
        if let Some(name) = &self.state.module_name {
            self.result.push_str(" $");
            self.result.push_str(name);
        }
        self.result.push_str(module_ty);
        let mut parser = Parser::new(offset);
        let mut bytes = wasm;
        loop {
            let payload = match parser.parse(bytes, true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    payload
                }
            };
            match payload {
                Payload::CustomSection {
                    name,
                    data,
                    data_offset,
                } => {
                    let mut printers = mem::replace(&mut self.printers, HashMap::new());
                    if let Some(printer) = printers.get_mut(name) {
                        printer(self, data_offset, data)?;
                    }
                    self.printers = printers;
                }
                Payload::TypeSection(s) => self.print_types(s)?,
                Payload::ImportSection(s) => self.print_imports(s)?,
                Payload::FunctionSection(reader) => {
                    if reader.get_count() == 0 {
                        continue;
                    }
                    let code = match code.take() {
                        Some(f) => f,
                        None => bail!("found function section without code section"),
                    };
                    self.print_code(code, reader)?;
                }
                Payload::TableSection(s) => self.print_tables(s)?,
                Payload::MemorySection(s) => self.print_memories(s)?,
                Payload::GlobalSection(s) => self.print_globals(s)?,
                Payload::ExportSection(s) => self.print_exports(s)?,
                Payload::StartSection { func, .. } => {
                    self.newline();
                    self.start_group("start ");
                    self.print_func_idx(func)?;
                    self.end_group();
                }
                Payload::ElementSection(s) => self.print_elems(s)?,
                Payload::CodeSectionStart { size, .. }
                | Payload::ModuleCodeSectionStart { size, .. } => {
                    // printed with the `Function` or `Module` section, so we
                    // skip these
                    bytes = &bytes[size as usize..];
                    parser.skip_section();
                }

                Payload::DataSection(s) => self.print_data(s)?,
                Payload::AliasSection(s) => self.print_aliases(s)?,
                Payload::ModuleSection(reader) => {
                    if reader.get_count() == 0 {
                        continue;
                    }
                    let module_code = match module_code.as_mut() {
                        Some(f) => f,
                        None => bail!("found function section without code section"),
                    };
                    self.print_module_code(module_code, reader)?;
                }
                Payload::InstanceSection(s) => self.print_instances(s)?,

                // not part of the text format
                Payload::Version { .. } | Payload::DataCountSection { .. } => {}
                // we skip these sections
                Payload::CodeSectionEntry(_) | Payload::ModuleCodeSectionEntry { .. } => {
                    unreachable!()
                }

                Payload::End => break,

                Payload::UnknownSection { id, .. } => bail!("found unknown section `{}`", id),
            }
        }
        self.state = prev;
        Ok(())
    }

    fn start_group(&mut self, name: &str) {
        self.result.push_str("(");
        self.result.push_str(name);
        self.nesting += 1;
    }

    fn end_group(&mut self) {
        self.result.push_str(")");
        self.nesting -= 1;
    }

    fn register_names(&mut self, names: NameSectionReader<'_>) -> Result<()> {
        for section in names {
            match section? {
                Name::Module(n) => self.state.module_name = Some(n.get_name()?.to_string()),
                Name::Function(n) => {
                    let mut map = n.get_map()?;
                    for _ in 0..map.get_count() {
                        let name = map.read()?;
                        self.state.names.insert(name.index, name.name.to_string());
                    }
                }
                Name::Local(n) => {
                    let mut reader = n.get_function_local_reader()?;
                    for _ in 0..reader.get_count() {
                        let local_name = reader.read()?;
                        let mut map = local_name.get_map()?;
                        let mut local_map = HashMap::new();
                        for _ in 0..map.get_count() {
                            let name = map.read()?;
                            local_map.insert(name.index, name.name.to_string());
                        }
                        self.state
                            .local_names
                            .insert(local_name.func_index, local_map);
                    }
                }
            }
        }
        Ok(())
    }

    fn print_types(&mut self, parser: TypeSectionReader<'_>) -> Result<()> {
        for ty in parser {
            self.newline();
            self.start_group("type");
            write!(self.result, " (;{};) ", self.state.types.len())?;
            let ty = match ty? {
                TypeDef::Func(ty) => {
                    self.start_group("func");
                    self.print_functype(&ty, None)?;
                    Some(ty)
                }
                TypeDef::Module(ty) => {
                    self.newline();
                    self.start_group("module");
                    for import in ty.imports.iter() {
                        self.print_import(import, false)?;
                    }
                    for export in ty.exports.iter() {
                        self.newline();
                        self.start_group("export ");
                        self.print_str(export.name)?;
                        self.result.push_str(" ");
                        self.print_import_ty(&export.ty, false)?;
                        self.end_group();
                    }
                    None
                }
                TypeDef::Instance(ty) => {
                    self.newline();
                    self.start_group("instance");
                    for export in ty.exports.iter() {
                        self.newline();
                        self.start_group("export ");
                        self.print_str(export.name)?;
                        self.result.push_str(" ");
                        self.print_import_ty(&export.ty, false)?;
                        self.end_group();
                    }
                    None
                }
            };
            self.end_group(); // inner type
            self.end_group(); // `type` itself
            self.state.types.push(ty);
        }
        Ok(())
    }

    fn print_functype_idx(&mut self, idx: u32, names_for: Option<u32>) -> Result<u32> {
        let ty = match self.state.types.get(idx as usize) {
            Some(Some(ty)) => ty.clone(),
            Some(None) => bail!("function type index `{}` is not a function", idx),
            None => bail!("function type index `{}` out of bounds", idx),
        };
        self.print_functype(&ty, names_for)
    }

    /// Returns the number of parameters, useful for local index calculations
    /// later.
    fn print_functype(&mut self, ty: &FuncType, names_for: Option<u32>) -> Result<u32> {
        let mut params = NamedLocalPrinter::new("param");
        // Note that named parameters must be alone in a `param` block, so
        // we need to be careful to terminate previous param blocks and open
        // a new one if that's the case with a named parameter.
        for (i, param) in ty.params.iter().enumerate() {
            let local_names = &self.state.local_names;
            let name = names_for
                .and_then(|n| local_names.get(&n))
                .and_then(|n| n.get(&(i as u32)));
            params.start_local(name, &mut self.result);
            self.print_valtype(*param)?;
            params.end_local(&mut self.result);
        }
        params.finish(&mut self.result);
        if ty.returns.len() > 0 {
            self.result.push_str(" (result");
            for result in ty.returns.iter() {
                self.result.push_str(" ");
                self.print_valtype(*result)?;
            }
            self.result.push_str(")");
        }
        Ok(ty.params.len() as u32)
    }

    fn print_valtype(&mut self, ty: Type) -> Result<()> {
        match ty {
            Type::I32 => self.result.push_str("i32"),
            Type::I64 => self.result.push_str("i64"),
            Type::F32 => self.result.push_str("f32"),
            Type::F64 => self.result.push_str("f64"),
            Type::V128 => self.result.push_str("v128"),
            Type::FuncRef => self.result.push_str("funcref"),
            Type::ExternRef => self.result.push_str("externref"),
            _ => bail!("unimplemented {:?}", ty),
        }
        Ok(())
    }

    fn print_reftype(&mut self, ty: Type) -> Result<()> {
        match ty {
            Type::FuncRef => self.result.push_str("func"),
            Type::ExternRef => self.result.push_str("extern"),
            _ => bail!("invalid reference type {:?}", ty),
        }
        Ok(())
    }

    fn print_imports(&mut self, parser: ImportSectionReader<'_>) -> Result<()> {
        for import in parser {
            let import = import?;
            self.print_import(&import, true)?;
            match import.ty {
                ImportSectionEntryType::Function(_) => self.state.func += 1,
                ImportSectionEntryType::Module(_) => self.state.module += 1,
                ImportSectionEntryType::Instance(_) => self.state.instance += 1,
                ImportSectionEntryType::Table(_) => self.state.table += 1,
                ImportSectionEntryType::Memory(_) => self.state.memory += 1,
                ImportSectionEntryType::Global(_) => self.state.global += 1,
            }
        }
        Ok(())
    }

    fn print_import(&mut self, import: &Import<'_>, index: bool) -> Result<()> {
        self.newline();
        self.start_group("import ");
        self.print_str(import.module)?;
        if let Some(field) = import.field {
            self.result.push_str(" ");
            self.print_str(field)?;
        }
        self.result.push_str(" ");
        self.print_import_ty(&import.ty, index)?;
        self.end_group();
        Ok(())
    }

    fn print_import_ty(&mut self, ty: &ImportSectionEntryType, index: bool) -> Result<()> {
        use ImportSectionEntryType::*;
        match ty {
            Function(f) => {
                self.start_group("func");
                if index {
                    match self.state.names.get(&self.state.func) {
                        Some(name) => write!(self.result, " ${}", name)?,
                        None => write!(self.result, " (;{};)", self.state.func)?,
                    }
                }
                write!(self.result, " (type {})", f)?;
            }
            Module(f) => {
                self.start_group("module ");
                if index {
                    write!(self.result, "(;{};) ", self.state.module)?;
                }
                write!(self.result, "(type {})", f)?;
            }
            Instance(f) => {
                self.start_group("instance ");
                if index {
                    write!(self.result, "(;{};) ", self.state.instance)?;
                }
                write!(self.result, "(type {})", f)?;
            }
            Table(f) => self.print_table_type(&f, index)?,
            Memory(f) => self.print_memory_type(&f, index)?,
            Global(f) => self.print_global_type(&f, index)?,
        }
        self.end_group();
        Ok(())
    }

    fn print_table_type(&mut self, ty: &TableType, index: bool) -> Result<()> {
        self.start_group("table ");
        if index {
            write!(self.result, "(;{};) ", self.state.table)?;
        }
        self.print_limits(&ty.limits)?;
        self.result.push_str(" ");
        self.print_valtype(ty.element_type)?;
        Ok(())
    }

    fn print_memory_type(&mut self, ty: &MemoryType, index: bool) -> Result<()> {
        self.start_group("memory ");
        if index {
            write!(self.result, "(;{};) ", self.state.memory)?;
        }
        match ty {
            MemoryType::M32 { limits, shared } => {
                self.print_limits(limits)?;
                if *shared {
                    self.result.push_str(" shared");
                }
            }
            MemoryType::M64 { limits } => {
                write!(self.result, "i64 {}", limits.initial)?;
                if let Some(max) = limits.maximum {
                    write!(self.result, " {}", max)?;
                }
            }
        }
        Ok(())
    }

    fn print_limits(&mut self, limits: &ResizableLimits) -> Result<()> {
        write!(self.result, "{}", limits.initial)?;
        if let Some(max) = limits.maximum {
            write!(self.result, " {}", max)?;
        }
        Ok(())
    }

    fn print_global_type(&mut self, ty: &GlobalType, index: bool) -> Result<()> {
        self.start_group("global ");
        if index {
            write!(self.result, "(;{};) ", self.state.global)?;
        }
        if ty.mutable {
            self.result.push_str("(mut ");
            self.print_valtype(ty.content_type)?;
            self.result.push_str(")");
        } else {
            self.print_valtype(ty.content_type)?;
        }
        Ok(())
    }

    fn print_tables(&mut self, parser: TableSectionReader<'_>) -> Result<()> {
        for table in parser {
            let table = table?;
            self.newline();
            self.print_table_type(&table, true)?;
            self.end_group();
            self.state.table += 1;
        }
        Ok(())
    }

    fn print_memories(&mut self, parser: MemorySectionReader<'_>) -> Result<()> {
        for memory in parser {
            let memory = memory?;
            self.newline();
            self.print_memory_type(&memory, true)?;
            self.end_group();
            self.state.memory += 1;
        }
        Ok(())
    }

    fn print_globals(&mut self, parser: GlobalSectionReader<'_>) -> Result<()> {
        for global in parser {
            let global = global?;
            self.newline();
            self.print_global_type(&global.ty, true)?;
            self.result.push_str(" ");
            self.print_init_expr(&global.init_expr)?;
            self.end_group();
            self.state.global += 1;
        }
        Ok(())
    }

    fn print_code(
        &mut self,
        code: CodeSectionReader<'_>,
        mut funcs: FunctionSectionReader<'_>,
    ) -> Result<()> {
        for body in code {
            let body = body?;
            let ty = funcs.read()?;
            self.newline();
            self.start_group("func ");
            match self.state.names.get(&self.state.func) {
                Some(name) => write!(self.result, "${}", name)?,
                None => write!(self.result, "(;{};)", self.state.func)?,
            }
            write!(self.result, " (type {})", ty)?;
            let params = self.print_functype_idx(ty, Some(self.state.func))?;

            let mut first = true;
            let mut local_idx = 0;
            let mut locals = NamedLocalPrinter::new("local");
            for local in body.get_locals_reader()? {
                let (cnt, ty) = local?;
                if MAX_LOCALS
                    .checked_sub(local_idx)
                    .and_then(|s| s.checked_sub(cnt))
                    .is_none()
                {
                    bail!("function exceeds the maximum number of locals that can be printed");
                }
                for _ in 0..cnt {
                    if first {
                        self.result.push_str("\n   ");
                        first = false;
                    }
                    let name = self
                        .state
                        .local_names
                        .get(&self.state.func)
                        .and_then(|m| m.get(&(params + local_idx)));
                    locals.start_local(name, &mut self.result);
                    self.print_valtype(ty)?;
                    locals.end_local(&mut self.result);
                    local_idx += 1;
                }
            }
            locals.finish(&mut self.result);

            let nesting_start = self.nesting;
            for operator in body.get_operators_reader()? {
                let operator = operator?;
                match operator {
                    Operator::Else => self.nesting -= 1,
                    Operator::End if self.nesting == nesting_start => continue,
                    Operator::End => self.nesting -= 1,
                    _ => {}
                }
                self.newline();
                self.print_operator(&operator, nesting_start)?;
            }
            self.end_group();

            self.state.func += 1;
        }
        Ok(())
    }

    fn newline(&mut self) {
        self.result.push_str("\n");
        for _ in 0..self.nesting {
            self.result.push_str("  ");
        }
    }

    fn print_operator(&mut self, op: &Operator<'_>, nesting_start: u32) -> Result<()> {
        use Operator::*;
        let cur_label = self.nesting - nesting_start + 1;
        let label = |relative: u32| match cur_label
            .checked_sub(relative)
            .and_then(|i| i.checked_sub(1))
        {
            Some(i) => format!("@{}", i),
            None => format!(" INVALID "),
        };
        match op {
            Nop => self.result.push_str("nop"),
            Unreachable => self.result.push_str("unreachable"),
            Block { ty } => {
                self.result.push_str("block");
                self.print_blockty(ty)?;
                write!(self.result, "  ;; label = @{}", cur_label)?;
                self.nesting += 1;
            }
            Loop { ty } => {
                self.result.push_str("loop");
                self.print_blockty(ty)?;
                write!(self.result, "  ;; label = @{}", cur_label)?;
                self.nesting += 1;
            }
            If { ty } => {
                self.result.push_str("if");
                self.print_blockty(ty)?;
                write!(self.result, "  ;; label = @{}", cur_label)?;
                self.nesting += 1;
            }
            Else => {
                self.result.push_str("else");
                self.nesting += 1;
            }
            End => {
                self.result.push_str("end");
            }
            Br { relative_depth } => {
                write!(
                    self.result,
                    "br {} (;{};)",
                    relative_depth,
                    label(*relative_depth),
                )?;
            }
            BrIf { relative_depth } => {
                write!(
                    self.result,
                    "br_if {} (;{};)",
                    relative_depth,
                    label(*relative_depth),
                )?;
            }
            BrTable { table } => {
                self.result.push_str("br_table");
                for item in table.targets() {
                    let (item, _is_default) = item?;
                    write!(self.result, " {} (;{};)", item, label(item))?;
                }
            }

            Return => self.result.push_str("return"),
            Call { function_index } => {
                self.result.push_str("call ");
                self.print_func_idx(*function_index)?;
            }
            CallIndirect { table_index, index } => {
                self.result.push_str("call_indirect");
                if *table_index != 0 {
                    write!(self.result, " {}", table_index)?;
                }
                write!(self.result, " (type {})", index)?;
            }
            ReturnCall { function_index } => {
                self.result.push_str("return_call ");
                self.print_func_idx(*function_index)?;
            }
            ReturnCallIndirect { table_index, index } => {
                self.result.push_str("return_call_indirect");
                if *table_index != 0 {
                    write!(self.result, " {}", table_index)?;
                }
                write!(self.result, " (type {})", index)?;
            }

            Drop => self.result.push_str("drop"),
            Select => self.result.push_str("select"),
            TypedSelect { ty } => {
                self.result.push_str("select (result ");
                self.print_valtype(*ty)?;
                self.result.push_str(")");
            }
            LocalGet { local_index } => {
                self.result.push_str("local.get ");
                self.print_local_idx(self.state.func, *local_index)?;
            }
            LocalSet { local_index } => {
                self.result.push_str("local.set ");
                self.print_local_idx(self.state.func, *local_index)?;
            }
            LocalTee { local_index } => {
                self.result.push_str("local.tee ");
                self.print_local_idx(self.state.func, *local_index)?;
            }

            GlobalGet { global_index } => {
                write!(self.result, "global.get {}", global_index)?;
            }
            GlobalSet { global_index } => {
                write!(self.result, "global.set {}", global_index)?;
            }

            I32Load { memarg } => self.mem_instr("i32.load", memarg, 4)?,
            I64Load { memarg } => self.mem_instr("i64.load", memarg, 8)?,
            F32Load { memarg } => self.mem_instr("f32.load", memarg, 4)?,
            F64Load { memarg } => self.mem_instr("f64.load", memarg, 8)?,
            I32Load8S { memarg } => self.mem_instr("i32.load8_s", memarg, 1)?,
            I32Load8U { memarg } => self.mem_instr("i32.load8_u", memarg, 1)?,
            I32Load16S { memarg } => self.mem_instr("i32.load16_s", memarg, 2)?,
            I32Load16U { memarg } => self.mem_instr("i32.load16_u", memarg, 2)?,
            I64Load8S { memarg } => self.mem_instr("i64.load8_s", memarg, 1)?,
            I64Load8U { memarg } => self.mem_instr("i64.load8_u", memarg, 1)?,
            I64Load16S { memarg } => self.mem_instr("i64.load16_s", memarg, 2)?,
            I64Load16U { memarg } => self.mem_instr("i64.load16_u", memarg, 2)?,
            I64Load32S { memarg } => self.mem_instr("i64.load32_s", memarg, 4)?,
            I64Load32U { memarg } => self.mem_instr("i64.load32_u", memarg, 4)?,

            I32Store { memarg } => self.mem_instr("i32.store", memarg, 4)?,
            I64Store { memarg } => self.mem_instr("i64.store", memarg, 8)?,
            F32Store { memarg } => self.mem_instr("f32.store", memarg, 4)?,
            F64Store { memarg } => self.mem_instr("f64.store", memarg, 8)?,
            I32Store8 { memarg } => self.mem_instr("i32.store8", memarg, 1)?,
            I32Store16 { memarg } => self.mem_instr("i32.store16", memarg, 2)?,
            I64Store8 { memarg } => self.mem_instr("i64.store8", memarg, 1)?,
            I64Store16 { memarg } => self.mem_instr("i64.store16", memarg, 2)?,
            I64Store32 { memarg } => self.mem_instr("i64.store32", memarg, 4)?,

            MemorySize { mem: 0, .. } => self.result.push_str("memory.size"),
            MemorySize { mem, .. } => write!(self.result, "memory.size {}", mem)?,
            MemoryGrow { mem: 0, .. } => self.result.push_str("memory.grow"),
            MemoryGrow { mem, .. } => write!(self.result, "memory.grow {}", mem)?,

            I32Const { value } => write!(self.result, "i32.const {}", value)?,
            I64Const { value } => write!(self.result, "i64.const {}", value)?,
            F32Const { value } => {
                self.result.push_str("f32.const ");
                self.print_f32(value.bits())?;
            }
            F64Const { value } => {
                self.result.push_str("f64.const ");
                self.print_f64(value.bits())?;
            }

            RefNull { ty } => {
                self.result.push_str("ref.null ");
                self.print_reftype(*ty)?;
            }
            RefIsNull => self.result.push_str("ref.is_null"),
            RefFunc { function_index } => {
                self.result.push_str("ref.func ");
                self.print_func_idx(*function_index)?;
            }

            I32Eqz => self.result.push_str("i32.eqz"),
            I32Eq => self.result.push_str("i32.eq"),
            I32Ne => self.result.push_str("i32.ne"),
            I32LtS => self.result.push_str("i32.lt_s"),
            I32LtU => self.result.push_str("i32.lt_u"),
            I32GtS => self.result.push_str("i32.gt_s"),
            I32GtU => self.result.push_str("i32.gt_u"),
            I32LeS => self.result.push_str("i32.le_s"),
            I32LeU => self.result.push_str("i32.le_u"),
            I32GeS => self.result.push_str("i32.ge_s"),
            I32GeU => self.result.push_str("i32.ge_u"),

            I64Eqz => self.result.push_str("i64.eqz"),
            I64Eq => self.result.push_str("i64.eq"),
            I64Ne => self.result.push_str("i64.ne"),
            I64LtS => self.result.push_str("i64.lt_s"),
            I64LtU => self.result.push_str("i64.lt_u"),
            I64GtS => self.result.push_str("i64.gt_s"),
            I64GtU => self.result.push_str("i64.gt_u"),
            I64LeS => self.result.push_str("i64.le_s"),
            I64LeU => self.result.push_str("i64.le_u"),
            I64GeS => self.result.push_str("i64.ge_s"),
            I64GeU => self.result.push_str("i64.ge_u"),

            F32Eq => self.result.push_str("f32.eq"),
            F32Ne => self.result.push_str("f32.ne"),
            F32Lt => self.result.push_str("f32.lt"),
            F32Gt => self.result.push_str("f32.gt"),
            F32Le => self.result.push_str("f32.le"),
            F32Ge => self.result.push_str("f32.ge"),

            F64Eq => self.result.push_str("f64.eq"),
            F64Ne => self.result.push_str("f64.ne"),
            F64Lt => self.result.push_str("f64.lt"),
            F64Gt => self.result.push_str("f64.gt"),
            F64Le => self.result.push_str("f64.le"),
            F64Ge => self.result.push_str("f64.ge"),

            I32Clz => self.result.push_str("i32.clz"),
            I32Ctz => self.result.push_str("i32.ctz"),
            I32Popcnt => self.result.push_str("i32.popcnt"),
            I32Add => self.result.push_str("i32.add"),
            I32Sub => self.result.push_str("i32.sub"),
            I32Mul => self.result.push_str("i32.mul"),
            I32DivS => self.result.push_str("i32.div_s"),
            I32DivU => self.result.push_str("i32.div_u"),
            I32RemS => self.result.push_str("i32.rem_s"),
            I32RemU => self.result.push_str("i32.rem_u"),
            I32And => self.result.push_str("i32.and"),
            I32Or => self.result.push_str("i32.or"),
            I32Xor => self.result.push_str("i32.xor"),
            I32Shl => self.result.push_str("i32.shl"),
            I32ShrS => self.result.push_str("i32.shr_s"),
            I32ShrU => self.result.push_str("i32.shr_u"),
            I32Rotl => self.result.push_str("i32.rotl"),
            I32Rotr => self.result.push_str("i32.rotr"),

            I64Clz => self.result.push_str("i64.clz"),
            I64Ctz => self.result.push_str("i64.ctz"),
            I64Popcnt => self.result.push_str("i64.popcnt"),
            I64Add => self.result.push_str("i64.add"),
            I64Sub => self.result.push_str("i64.sub"),
            I64Mul => self.result.push_str("i64.mul"),
            I64DivS => self.result.push_str("i64.div_s"),
            I64DivU => self.result.push_str("i64.div_u"),
            I64RemS => self.result.push_str("i64.rem_s"),
            I64RemU => self.result.push_str("i64.rem_u"),
            I64And => self.result.push_str("i64.and"),
            I64Or => self.result.push_str("i64.or"),
            I64Xor => self.result.push_str("i64.xor"),
            I64Shl => self.result.push_str("i64.shl"),
            I64ShrS => self.result.push_str("i64.shr_s"),
            I64ShrU => self.result.push_str("i64.shr_u"),
            I64Rotl => self.result.push_str("i64.rotl"),
            I64Rotr => self.result.push_str("i64.rotr"),

            F32Abs => self.result.push_str("f32.abs"),
            F32Neg => self.result.push_str("f32.neg"),
            F32Ceil => self.result.push_str("f32.ceil"),
            F32Floor => self.result.push_str("f32.floor"),
            F32Trunc => self.result.push_str("f32.trunc"),
            F32Nearest => self.result.push_str("f32.nearest"),
            F32Sqrt => self.result.push_str("f32.sqrt"),
            F32Add => self.result.push_str("f32.add"),
            F32Sub => self.result.push_str("f32.sub"),
            F32Mul => self.result.push_str("f32.mul"),
            F32Div => self.result.push_str("f32.div"),
            F32Min => self.result.push_str("f32.min"),
            F32Max => self.result.push_str("f32.max"),
            F32Copysign => self.result.push_str("f32.copysign"),

            F64Abs => self.result.push_str("f64.abs"),
            F64Neg => self.result.push_str("f64.neg"),
            F64Ceil => self.result.push_str("f64.ceil"),
            F64Floor => self.result.push_str("f64.floor"),
            F64Trunc => self.result.push_str("f64.trunc"),
            F64Nearest => self.result.push_str("f64.nearest"),
            F64Sqrt => self.result.push_str("f64.sqrt"),
            F64Add => self.result.push_str("f64.add"),
            F64Sub => self.result.push_str("f64.sub"),
            F64Mul => self.result.push_str("f64.mul"),
            F64Div => self.result.push_str("f64.div"),
            F64Min => self.result.push_str("f64.min"),
            F64Max => self.result.push_str("f64.max"),
            F64Copysign => self.result.push_str("f64.copysign"),

            I32WrapI64 => self.result.push_str("i32.wrap_i64"),
            I32TruncF32S => self.result.push_str("i32.trunc_f32_s"),
            I32TruncF32U => self.result.push_str("i32.trunc_f32_u"),
            I32TruncF64S => self.result.push_str("i32.trunc_f64_s"),
            I32TruncF64U => self.result.push_str("i32.trunc_f64_u"),
            I64ExtendI32S => self.result.push_str("i64.extend_i32_s"),
            I64ExtendI32U => self.result.push_str("i64.extend_i32_u"),
            I64TruncF32S => self.result.push_str("i64.trunc_f32_s"),
            I64TruncF32U => self.result.push_str("i64.trunc_f32_u"),
            I64TruncF64S => self.result.push_str("i64.trunc_f64_s"),
            I64TruncF64U => self.result.push_str("i64.trunc_f64_u"),

            F32ConvertI32S => self.result.push_str("f32.convert_i32_s"),
            F32ConvertI32U => self.result.push_str("f32.convert_i32_u"),
            F32ConvertI64S => self.result.push_str("f32.convert_i64_s"),
            F32ConvertI64U => self.result.push_str("f32.convert_i64_u"),
            F32DemoteF64 => self.result.push_str("f32.demote_f64"),
            F64ConvertI32S => self.result.push_str("f64.convert_i32_s"),
            F64ConvertI32U => self.result.push_str("f64.convert_i32_u"),
            F64ConvertI64S => self.result.push_str("f64.convert_i64_s"),
            F64ConvertI64U => self.result.push_str("f64.convert_i64_u"),
            F64PromoteF32 => self.result.push_str("f64.promote_f32"),

            I32ReinterpretF32 => self.result.push_str("i32.reinterpret_f32"),
            I64ReinterpretF64 => self.result.push_str("i64.reinterpret_f64"),
            F32ReinterpretI32 => self.result.push_str("f32.reinterpret_i32"),
            F64ReinterpretI64 => self.result.push_str("f64.reinterpret_i64"),

            I32Extend8S => self.result.push_str("i32.extend8_s"),
            I32Extend16S => self.result.push_str("i32.extend16_s"),
            I64Extend8S => self.result.push_str("i64.extend8_s"),
            I64Extend16S => self.result.push_str("i64.extend16_s"),
            I64Extend32S => self.result.push_str("i64.extend32_s"),

            I32TruncSatF32S => self.result.push_str("i32.trunc_sat_f32_s"),
            I32TruncSatF32U => self.result.push_str("i32.trunc_sat_f32_u"),
            I32TruncSatF64S => self.result.push_str("i32.trunc_sat_f64_s"),
            I32TruncSatF64U => self.result.push_str("i32.trunc_sat_f64_u"),
            I64TruncSatF32S => self.result.push_str("i64.trunc_sat_f32_s"),
            I64TruncSatF32U => self.result.push_str("i64.trunc_sat_f32_u"),
            I64TruncSatF64S => self.result.push_str("i64.trunc_sat_f64_s"),
            I64TruncSatF64U => self.result.push_str("i64.trunc_sat_f64_u"),

            MemoryInit { segment, mem: 0 } => write!(self.result, "memory.init {}", segment)?,
            MemoryInit { segment, mem } => write!(self.result, "memory.init {} {}", segment, mem)?,
            DataDrop { segment } => write!(self.result, "data.drop {}", segment)?,
            MemoryCopy { src: 0, dst: 0 } => self.result.push_str("memory.copy"),
            MemoryCopy { src, dst } => write!(self.result, "memory.copy {} {}", dst, src)?,
            MemoryFill { mem: 0 } => self.result.push_str("memory.fill"),
            MemoryFill { mem } => write!(self.result, "memory.fill {}", mem)?,

            TableInit { table, segment } => {
                if *table == 0 {
                    write!(self.result, "table.init {}", segment)?
                } else {
                    write!(self.result, "table.init {} {}", table, segment)?
                }
            }
            ElemDrop { segment } => write!(self.result, "elem.drop {}", segment)?,
            TableCopy {
                dst_table,
                src_table,
            } => {
                if *dst_table == *src_table && *src_table == 0 {
                    self.result.push_str("table.copy");
                } else {
                    write!(self.result, "table.copy {} {}", dst_table, src_table)?
                }
            }
            TableGet { table } => write!(self.result, "table.get {}", table)?,
            TableSet { table } => write!(self.result, "table.set {}", table)?,
            TableGrow { table } => write!(self.result, "table.grow {}", table)?,
            TableSize { table } => write!(self.result, "table.size {}", table)?,
            TableFill { table } => write!(self.result, "table.fill {}", table)?,

            MemoryAtomicNotify { memarg } => self.mem_instr("memory.atomic.notify", memarg, 4)?,
            MemoryAtomicWait32 { memarg } => self.mem_instr("memory.atomic.wait32", memarg, 4)?,
            MemoryAtomicWait64 { memarg } => self.mem_instr("memory.atomic.wait64", memarg, 8)?,
            AtomicFence { flags: _ } => self.result.push_str("atomic.fence"),

            I32AtomicLoad { memarg } => self.mem_instr("i32.atomic.load", memarg, 4)?,
            I64AtomicLoad { memarg } => self.mem_instr("i64.atomic.load", memarg, 8)?,
            I32AtomicLoad8U { memarg } => self.mem_instr("i32.atomic.load8_u", memarg, 1)?,
            I32AtomicLoad16U { memarg } => self.mem_instr("i32.atomic.load16_u", memarg, 2)?,
            I64AtomicLoad8U { memarg } => self.mem_instr("i64.atomic.load8_u", memarg, 1)?,
            I64AtomicLoad16U { memarg } => self.mem_instr("i64.atomic.load16_u", memarg, 2)?,
            I64AtomicLoad32U { memarg } => self.mem_instr("i64.atomic.load32_u", memarg, 4)?,

            I32AtomicStore { memarg } => self.mem_instr("i32.atomic.store", memarg, 4)?,
            I64AtomicStore { memarg } => self.mem_instr("i64.atomic.store", memarg, 8)?,
            I32AtomicStore8 { memarg } => self.mem_instr("i32.atomic.store8", memarg, 1)?,
            I32AtomicStore16 { memarg } => self.mem_instr("i32.atomic.store16", memarg, 2)?,
            I64AtomicStore8 { memarg } => self.mem_instr("i64.atomic.store8", memarg, 1)?,
            I64AtomicStore16 { memarg } => self.mem_instr("i64.atomic.store16", memarg, 2)?,
            I64AtomicStore32 { memarg } => self.mem_instr("i64.atomic.store32", memarg, 4)?,

            I32AtomicRmwAdd { memarg } => self.mem_instr("i32.atomic.rmw.add", memarg, 4)?,
            I64AtomicRmwAdd { memarg } => self.mem_instr("i64.atomic.rmw.add", memarg, 8)?,
            I32AtomicRmw8AddU { memarg } => self.mem_instr("i32.atomic.rmw8.add_u", memarg, 1)?,
            I32AtomicRmw16AddU { memarg } => self.mem_instr("i32.atomic.rmw16.add_u", memarg, 2)?,
            I64AtomicRmw8AddU { memarg } => self.mem_instr("i64.atomic.rmw8.add_u", memarg, 1)?,
            I64AtomicRmw16AddU { memarg } => self.mem_instr("i64.atomic.rmw16.add_u", memarg, 2)?,
            I64AtomicRmw32AddU { memarg } => self.mem_instr("i64.atomic.rmw32.add_u", memarg, 4)?,

            I32AtomicRmwSub { memarg } => self.mem_instr("i32.atomic.rmw.sub", memarg, 4)?,
            I64AtomicRmwSub { memarg } => self.mem_instr("i64.atomic.rmw.sub", memarg, 8)?,
            I32AtomicRmw8SubU { memarg } => self.mem_instr("i32.atomic.rmw8.sub_u", memarg, 1)?,
            I32AtomicRmw16SubU { memarg } => self.mem_instr("i32.atomic.rmw16.sub_u", memarg, 2)?,
            I64AtomicRmw8SubU { memarg } => self.mem_instr("i64.atomic.rmw8.sub_u", memarg, 1)?,
            I64AtomicRmw16SubU { memarg } => self.mem_instr("i64.atomic.rmw16.sub_u", memarg, 2)?,
            I64AtomicRmw32SubU { memarg } => self.mem_instr("i64.atomic.rmw32.sub_u", memarg, 4)?,

            I32AtomicRmwAnd { memarg } => self.mem_instr("i32.atomic.rmw.and", memarg, 4)?,
            I64AtomicRmwAnd { memarg } => self.mem_instr("i64.atomic.rmw.and", memarg, 8)?,
            I32AtomicRmw8AndU { memarg } => self.mem_instr("i32.atomic.rmw8.and_u", memarg, 1)?,
            I32AtomicRmw16AndU { memarg } => self.mem_instr("i32.atomic.rmw16.and_u", memarg, 2)?,
            I64AtomicRmw8AndU { memarg } => self.mem_instr("i64.atomic.rmw8.and_u", memarg, 1)?,
            I64AtomicRmw16AndU { memarg } => self.mem_instr("i64.atomic.rmw16.and_u", memarg, 2)?,
            I64AtomicRmw32AndU { memarg } => self.mem_instr("i64.atomic.rmw32.and_u", memarg, 4)?,

            I32AtomicRmwOr { memarg } => self.mem_instr("i32.atomic.rmw.or", memarg, 4)?,
            I64AtomicRmwOr { memarg } => self.mem_instr("i64.atomic.rmw.or", memarg, 8)?,
            I32AtomicRmw8OrU { memarg } => self.mem_instr("i32.atomic.rmw8.or_u", memarg, 1)?,
            I32AtomicRmw16OrU { memarg } => self.mem_instr("i32.atomic.rmw16.or_u", memarg, 2)?,
            I64AtomicRmw8OrU { memarg } => self.mem_instr("i64.atomic.rmw8.or_u", memarg, 1)?,
            I64AtomicRmw16OrU { memarg } => self.mem_instr("i64.atomic.rmw16.or_u", memarg, 2)?,
            I64AtomicRmw32OrU { memarg } => self.mem_instr("i64.atomic.rmw32.or_u", memarg, 4)?,

            I32AtomicRmwXor { memarg } => self.mem_instr("i32.atomic.rmw.xor", memarg, 4)?,
            I64AtomicRmwXor { memarg } => self.mem_instr("i64.atomic.rmw.xor", memarg, 8)?,
            I32AtomicRmw8XorU { memarg } => self.mem_instr("i32.atomic.rmw8.xor_u", memarg, 1)?,
            I32AtomicRmw16XorU { memarg } => self.mem_instr("i32.atomic.rmw16.xor_u", memarg, 2)?,
            I64AtomicRmw8XorU { memarg } => self.mem_instr("i64.atomic.rmw8.xor_u", memarg, 1)?,
            I64AtomicRmw16XorU { memarg } => self.mem_instr("i64.atomic.rmw16.xor_u", memarg, 2)?,
            I64AtomicRmw32XorU { memarg } => self.mem_instr("i64.atomic.rmw32.xor_u", memarg, 4)?,

            I32AtomicRmwXchg { memarg } => self.mem_instr("i32.atomic.rmw.xchg", memarg, 4)?,
            I64AtomicRmwXchg { memarg } => self.mem_instr("i64.atomic.rmw.xchg", memarg, 8)?,
            I32AtomicRmw8XchgU { memarg } => self.mem_instr("i32.atomic.rmw8.xchg_u", memarg, 1)?,
            I32AtomicRmw16XchgU { memarg } => {
                self.mem_instr("i32.atomic.rmw16.xchg_u", memarg, 2)?
            }
            I64AtomicRmw8XchgU { memarg } => self.mem_instr("i64.atomic.rmw8.xchg_u", memarg, 1)?,
            I64AtomicRmw16XchgU { memarg } => {
                self.mem_instr("i64.atomic.rmw16.xchg_u", memarg, 2)?
            }
            I64AtomicRmw32XchgU { memarg } => {
                self.mem_instr("i64.atomic.rmw32.xchg_u", memarg, 4)?
            }

            I32AtomicRmwCmpxchg { memarg } => {
                self.mem_instr("i32.atomic.rmw.cmpxchg", memarg, 4)?
            }
            I64AtomicRmwCmpxchg { memarg } => {
                self.mem_instr("i64.atomic.rmw.cmpxchg", memarg, 8)?
            }
            I32AtomicRmw8CmpxchgU { memarg } => {
                self.mem_instr("i32.atomic.rmw8.cmpxchg_u", memarg, 1)?
            }
            I32AtomicRmw16CmpxchgU { memarg } => {
                self.mem_instr("i32.atomic.rmw16.cmpxchg_u", memarg, 2)?
            }
            I64AtomicRmw8CmpxchgU { memarg } => {
                self.mem_instr("i64.atomic.rmw8.cmpxchg_u", memarg, 1)?
            }
            I64AtomicRmw16CmpxchgU { memarg } => {
                self.mem_instr("i64.atomic.rmw16.cmpxchg_u", memarg, 2)?
            }
            I64AtomicRmw32CmpxchgU { memarg } => {
                self.mem_instr("i64.atomic.rmw32.cmpxchg_u", memarg, 4)?
            }

            V128Load { memarg } => self.mem_instr("v128.load", memarg, 16)?,
            V128Store { memarg } => self.mem_instr("v128.store", memarg, 16)?,
            V128Const { value } => {
                write!(self.result, "v128.const i32x4")?;
                for chunk in value.bytes().chunks(4) {
                    write!(
                        self.result,
                        " 0x{:02x}{:02x}{:02x}{:02x}",
                        chunk[3], chunk[2], chunk[1], chunk[0]
                    )?;
                }
            }

            I8x16Splat => self.result.push_str("i8x16.splat"),
            I8x16ExtractLaneS { lane } => write!(self.result, "i8x16.extract_lane_s {}", lane)?,
            I8x16ExtractLaneU { lane } => write!(self.result, "i8x16.extract_lane_u {}", lane)?,
            I8x16ReplaceLane { lane } => write!(self.result, "i8x16.replace_lane {}", lane)?,
            I16x8Splat => self.result.push_str("i16x8.splat"),
            I16x8ExtractLaneS { lane } => write!(self.result, "i16x8.extract_lane_s {}", lane)?,
            I16x8ExtractLaneU { lane } => write!(self.result, "i16x8.extract_lane_u {}", lane)?,
            I16x8ReplaceLane { lane } => write!(self.result, "i16x8.replace_lane {}", lane)?,
            I32x4Splat => self.result.push_str("i32x4.splat"),
            I32x4ExtractLane { lane } => write!(self.result, "i32x4.extract_lane {}", lane)?,
            I32x4ReplaceLane { lane } => write!(self.result, "i32x4.replace_lane {}", lane)?,
            I64x2Splat => self.result.push_str("i64x2.splat"),
            I64x2ExtractLane { lane } => write!(self.result, "i64x2.extract_lane {}", lane)?,
            I64x2ReplaceLane { lane } => write!(self.result, "i64x2.replace_lane {}", lane)?,
            F32x4Splat => self.result.push_str("f32x4.splat"),
            F32x4ExtractLane { lane } => write!(self.result, "f32x4.extract_lane {}", lane)?,
            F32x4ReplaceLane { lane } => write!(self.result, "f32x4.replace_lane {}", lane)?,
            F64x2Splat => self.result.push_str("f64x2.splat"),
            F64x2ExtractLane { lane } => write!(self.result, "f64x2.extract_lane {}", lane)?,
            F64x2ReplaceLane { lane } => write!(self.result, "f64x2.replace_lane {}", lane)?,

            I8x16Eq => self.result.push_str("i8x16.eq"),
            I8x16Ne => self.result.push_str("i8x16.ne"),
            I8x16LtS => self.result.push_str("i8x16.lt_s"),
            I8x16LtU => self.result.push_str("i8x16.lt_u"),
            I8x16GtS => self.result.push_str("i8x16.gt_s"),
            I8x16GtU => self.result.push_str("i8x16.gt_u"),
            I8x16LeS => self.result.push_str("i8x16.le_s"),
            I8x16LeU => self.result.push_str("i8x16.le_u"),
            I8x16GeS => self.result.push_str("i8x16.ge_s"),
            I8x16GeU => self.result.push_str("i8x16.ge_u"),

            I16x8Eq => self.result.push_str("i16x8.eq"),
            I16x8Ne => self.result.push_str("i16x8.ne"),
            I16x8LtS => self.result.push_str("i16x8.lt_s"),
            I16x8LtU => self.result.push_str("i16x8.lt_u"),
            I16x8GtS => self.result.push_str("i16x8.gt_s"),
            I16x8GtU => self.result.push_str("i16x8.gt_u"),
            I16x8LeS => self.result.push_str("i16x8.le_s"),
            I16x8LeU => self.result.push_str("i16x8.le_u"),
            I16x8GeS => self.result.push_str("i16x8.ge_s"),
            I16x8GeU => self.result.push_str("i16x8.ge_u"),

            I32x4Eq => self.result.push_str("i32x4.eq"),
            I32x4Ne => self.result.push_str("i32x4.ne"),
            I32x4LtS => self.result.push_str("i32x4.lt_s"),
            I32x4LtU => self.result.push_str("i32x4.lt_u"),
            I32x4GtS => self.result.push_str("i32x4.gt_s"),
            I32x4GtU => self.result.push_str("i32x4.gt_u"),
            I32x4LeS => self.result.push_str("i32x4.le_s"),
            I32x4LeU => self.result.push_str("i32x4.le_u"),
            I32x4GeS => self.result.push_str("i32x4.ge_s"),
            I32x4GeU => self.result.push_str("i32x4.ge_u"),

            F32x4Eq => self.result.push_str("f32x4.eq"),
            F32x4Ne => self.result.push_str("f32x4.ne"),
            F32x4Lt => self.result.push_str("f32x4.lt"),
            F32x4Gt => self.result.push_str("f32x4.gt"),
            F32x4Le => self.result.push_str("f32x4.le"),
            F32x4Ge => self.result.push_str("f32x4.ge"),

            F64x2Eq => self.result.push_str("f64x2.eq"),
            F64x2Ne => self.result.push_str("f64x2.ne"),
            F64x2Lt => self.result.push_str("f64x2.lt"),
            F64x2Gt => self.result.push_str("f64x2.gt"),
            F64x2Le => self.result.push_str("f64x2.le"),
            F64x2Ge => self.result.push_str("f64x2.ge"),

            V128Not => self.result.push_str("v128.not"),
            V128And => self.result.push_str("v128.and"),
            V128AndNot => self.result.push_str("v128.andnot"),
            V128Or => self.result.push_str("v128.or"),
            V128Xor => self.result.push_str("v128.xor"),
            V128Bitselect => self.result.push_str("v128.bitselect"),

            I8x16Abs => self.result.push_str("i8x16.abs"),
            I8x16Neg => self.result.push_str("i8x16.neg"),
            I8x16AnyTrue => self.result.push_str("i8x16.any_true"),
            I8x16AllTrue => self.result.push_str("i8x16.all_true"),
            I8x16Bitmask => self.result.push_str("i8x16.bitmask"),
            I8x16Shl => self.result.push_str("i8x16.shl"),
            I8x16ShrU => self.result.push_str("i8x16.shr_u"),
            I8x16ShrS => self.result.push_str("i8x16.shr_s"),
            I8x16Add => self.result.push_str("i8x16.add"),
            I8x16AddSaturateS => self.result.push_str("i8x16.add_saturate_s"),
            I8x16AddSaturateU => self.result.push_str("i8x16.add_saturate_u"),
            I8x16Sub => self.result.push_str("i8x16.sub"),
            I8x16SubSaturateS => self.result.push_str("i8x16.sub_saturate_s"),
            I8x16SubSaturateU => self.result.push_str("i8x16.sub_saturate_u"),

            I16x8Abs => self.result.push_str("i16x8.abs"),
            I16x8Neg => self.result.push_str("i16x8.neg"),
            I16x8AnyTrue => self.result.push_str("i16x8.any_true"),
            I16x8AllTrue => self.result.push_str("i16x8.all_true"),
            I16x8Bitmask => self.result.push_str("i16x8.bitmask"),
            I16x8Shl => self.result.push_str("i16x8.shl"),
            I16x8ShrU => self.result.push_str("i16x8.shr_u"),
            I16x8ShrS => self.result.push_str("i16x8.shr_s"),
            I16x8Add => self.result.push_str("i16x8.add"),
            I16x8AddSaturateS => self.result.push_str("i16x8.add_saturate_s"),
            I16x8AddSaturateU => self.result.push_str("i16x8.add_saturate_u"),
            I16x8Sub => self.result.push_str("i16x8.sub"),
            I16x8SubSaturateS => self.result.push_str("i16x8.sub_saturate_s"),
            I16x8SubSaturateU => self.result.push_str("i16x8.sub_saturate_u"),
            I16x8Mul => self.result.push_str("i16x8.mul"),

            I32x4Abs => self.result.push_str("i32x4.abs"),
            I32x4Neg => self.result.push_str("i32x4.neg"),
            I32x4AnyTrue => self.result.push_str("i32x4.any_true"),
            I32x4AllTrue => self.result.push_str("i32x4.all_true"),
            I32x4Bitmask => self.result.push_str("i32x4.bitmask"),
            I32x4Shl => self.result.push_str("i32x4.shl"),
            I32x4ShrU => self.result.push_str("i32x4.shr_u"),
            I32x4ShrS => self.result.push_str("i32x4.shr_s"),
            I32x4Add => self.result.push_str("i32x4.add"),
            I32x4Sub => self.result.push_str("i32x4.sub"),
            I32x4Mul => self.result.push_str("i32x4.mul"),

            I64x2Neg => self.result.push_str("i64x2.neg"),
            I64x2Shl => self.result.push_str("i64x2.shl"),
            I64x2ShrU => self.result.push_str("i64x2.shr_u"),
            I64x2ShrS => self.result.push_str("i64x2.shr_s"),
            I64x2Add => self.result.push_str("i64x2.add"),
            I64x2Sub => self.result.push_str("i64x2.sub"),
            I64x2Mul => self.result.push_str("i64x2.mul"),

            F32x4Abs => self.result.push_str("f32x4.abs"),
            F32x4Neg => self.result.push_str("f32x4.neg"),
            F32x4Sqrt => self.result.push_str("f32x4.sqrt"),
            F32x4Add => self.result.push_str("f32x4.add"),
            F32x4Sub => self.result.push_str("f32x4.sub"),
            F32x4Div => self.result.push_str("f32x4.div"),
            F32x4Mul => self.result.push_str("f32x4.mul"),
            F32x4Min => self.result.push_str("f32x4.min"),
            F32x4Max => self.result.push_str("f32x4.max"),

            F64x2Abs => self.result.push_str("f64x2.abs"),
            F64x2Neg => self.result.push_str("f64x2.neg"),
            F64x2Sqrt => self.result.push_str("f64x2.sqrt"),
            F64x2Add => self.result.push_str("f64x2.add"),
            F64x2Sub => self.result.push_str("f64x2.sub"),
            F64x2Div => self.result.push_str("f64x2.div"),
            F64x2Mul => self.result.push_str("f64x2.mul"),
            F64x2Min => self.result.push_str("f64x2.min"),
            F64x2Max => self.result.push_str("f64x2.max"),

            I32x4TruncSatF32x4S => self.result.push_str("i32x4.trunc_sat_f32x4_s"),
            I32x4TruncSatF32x4U => self.result.push_str("i32x4.trunc_sat_f32x4_u"),
            F32x4ConvertI32x4S => self.result.push_str("f32x4.convert_i32x4_s"),
            F32x4ConvertI32x4U => self.result.push_str("f32x4.convert_i32x4_u"),

            V8x16Swizzle => self.result.push_str("v8x16.swizzle"),
            V8x16Shuffle { lanes } => {
                self.result.push_str("v8x16.shuffle");
                for lane in lanes {
                    write!(self.result, " {}", lane)?;
                }
            }
            V8x16LoadSplat { memarg } => self.mem_instr("v8x16.load_splat", memarg, 1)?,
            V16x8LoadSplat { memarg } => self.mem_instr("v16x8.load_splat", memarg, 2)?,
            V32x4LoadSplat { memarg } => self.mem_instr("v32x4.load_splat", memarg, 4)?,
            V64x2LoadSplat { memarg } => self.mem_instr("v64x2.load_splat", memarg, 8)?,

            I8x16NarrowI16x8S => self.result.push_str("i8x16.narrow_i16x8_s"),
            I8x16NarrowI16x8U => self.result.push_str("i8x16.narrow_i16x8_u"),
            I16x8NarrowI32x4S => self.result.push_str("i16x8.narrow_i32x4_s"),
            I16x8NarrowI32x4U => self.result.push_str("i16x8.narrow_i32x4_u"),

            I16x8WidenLowI8x16S => self.result.push_str("i16x8.widen_low_i8x16_s"),
            I16x8WidenHighI8x16S => self.result.push_str("i16x8.widen_high_i8x16_s"),
            I16x8WidenLowI8x16U => self.result.push_str("i16x8.widen_low_i8x16_u"),
            I16x8WidenHighI8x16U => self.result.push_str("i16x8.widen_high_i8x16_u"),
            I32x4WidenLowI16x8S => self.result.push_str("i32x4.widen_low_i16x8_s"),
            I32x4WidenHighI16x8S => self.result.push_str("i32x4.widen_high_i16x8_s"),
            I32x4WidenLowI16x8U => self.result.push_str("i32x4.widen_low_i16x8_u"),
            I32x4WidenHighI16x8U => self.result.push_str("i32x4.widen_high_i16x8_u"),

            I16x8Load8x8S { memarg } => self.mem_instr("i16x8.load8x8_s", memarg, 8)?,
            I16x8Load8x8U { memarg } => self.mem_instr("i16x8.load8x8_u", memarg, 8)?,
            I32x4Load16x4S { memarg } => self.mem_instr("i32x4.load16x4_s", memarg, 8)?,
            I32x4Load16x4U { memarg } => self.mem_instr("i32x4.load16x4_u", memarg, 8)?,
            I64x2Load32x2S { memarg } => self.mem_instr("i64x2.load32x2_s", memarg, 8)?,
            I64x2Load32x2U { memarg } => self.mem_instr("i64x2.load32x2_u", memarg, 8)?,

            I8x16RoundingAverageU => self.result.push_str("i8x16.avgr_u"),
            I16x8RoundingAverageU => self.result.push_str("i16x8.avgr_u"),

            I8x16MinS => self.result.push_str("i8x16.min_s"),
            I8x16MinU => self.result.push_str("i8x16.min_u"),
            I8x16MaxS => self.result.push_str("i8x16.max_s"),
            I8x16MaxU => self.result.push_str("i8x16.max_u"),
            I16x8MinS => self.result.push_str("i16x8.min_s"),
            I16x8MinU => self.result.push_str("i16x8.min_u"),
            I16x8MaxS => self.result.push_str("i16x8.max_s"),
            I16x8MaxU => self.result.push_str("i16x8.max_u"),
            I32x4MinS => self.result.push_str("i32x4.min_s"),
            I32x4MinU => self.result.push_str("i32x4.min_u"),
            I32x4MaxS => self.result.push_str("i32x4.max_s"),
            I32x4MaxU => self.result.push_str("i32x4.max_u"),
        }
        Ok(())
    }

    fn mem_instr(
        &mut self,
        name: &str,
        memarg: &MemoryImmediate,
        default_align: u32,
    ) -> Result<()> {
        self.result.push_str(name);
        if memarg.memory != 0 {
            write!(self.result, " {}", memarg.memory)?;
        }
        if memarg.offset != 0 {
            write!(self.result, " offset={}", memarg.offset)?;
        }
        let align = 1 << memarg.align;
        if default_align != align {
            write!(self.result, " align={}", align)?;
        }
        Ok(())
    }

    fn print_blockty(&mut self, ty: &TypeOrFuncType) -> Result<()> {
        match ty {
            TypeOrFuncType::Type(Type::EmptyBlockType) => Ok(()),
            TypeOrFuncType::Type(t) => {
                self.result.push_str(" (result ");
                self.print_valtype(*t)?;
                self.result.push_str(")");
                Ok(())
            }
            TypeOrFuncType::FuncType(idx) => {
                self.print_functype_idx(*idx, None)?;
                Ok(())
            }
        }
    }

    fn print_exports(&mut self, data: ExportSectionReader) -> Result<()> {
        for export in data {
            let export = export?;
            self.newline();
            self.start_group("export ");
            self.print_str(export.field)?;
            self.result.push_str(" ");
            self.start_group("");
            match export.kind {
                ExternalKind::Function => {
                    self.result.push_str("func ");
                    self.print_func_idx(export.index)?;
                }
                ExternalKind::Table => write!(self.result, "table {}", export.index)?,
                ExternalKind::Global => write!(self.result, "global {}", export.index)?,
                ExternalKind::Memory => write!(self.result, "memory {}", export.index)?,
                ExternalKind::Module => write!(self.result, "module {}", export.index)?,
                ExternalKind::Instance => write!(self.result, "instance {}", export.index)?,
                ExternalKind::Type => write!(self.result, "type {}", export.index)?,
            }
            self.end_group(); // field
            self.end_group(); // export
        }
        return Ok(());
    }

    fn print_external(&mut self, kind: ExternalKind, index: u32) -> Result<()> {
        self.result.push_str("(");
        match kind {
            ExternalKind::Function => {
                self.result.push_str("func ");
                self.print_func_idx(index)?;
            }
            ExternalKind::Table => write!(self.result, "table {}", index)?,
            ExternalKind::Global => write!(self.result, "global {}", index)?,
            ExternalKind::Memory => write!(self.result, "memory {}", index)?,
            ExternalKind::Module => write!(self.result, "module {}", index)?,
            ExternalKind::Instance => write!(self.result, "instance {}", index)?,
            ExternalKind::Type => write!(self.result, "type {}", index)?,
        }
        self.result.push_str(")");
        Ok(())
    }

    /// Prints a function index specified, using the identifier for the function
    /// from the `name` section if it was present.
    ///
    /// This will either print `$foo` or `idx` as a raw integer.
    pub fn print_func_idx(&mut self, idx: u32) -> Result<()> {
        match self.state.names.get(&idx) {
            Some(name) => write!(self.result, "${}", name)?,
            None => write!(self.result, "{}", idx)?,
        }
        Ok(())
    }

    fn print_local_idx(&mut self, func: u32, idx: u32) -> Result<()> {
        match self.state.local_names.get(&func).and_then(|f| f.get(&idx)) {
            Some(name) => write!(self.result, "${}", name)?,
            None => write!(self.result, "{}", idx)?,
        }
        Ok(())
    }

    fn print_elems(&mut self, data: ElementSectionReader) -> Result<()> {
        for (i, elem) in data.into_iter().enumerate() {
            let mut elem = elem?;
            write!(self.result, "\n  (elem (;{};)", i)?;
            match &mut elem.kind {
                ElementKind::Passive => {}
                ElementKind::Declared => write!(self.result, " declare")?,
                ElementKind::Active {
                    table_index,
                    init_expr,
                } => {
                    if *table_index != 0 {
                        write!(self.result, " (table {})", table_index)?;
                    }
                    self.result.push_str(" ");
                    self.print_init_expr(&init_expr)?;
                }
            }
            let mut items_reader = elem.items.get_items_reader()?;
            self.result.push_str(" ");
            if items_reader.uses_exprs() {
                self.print_valtype(elem.ty)?;
            } else {
                self.print_reftype(elem.ty)?;
            }
            for _ in 0..items_reader.get_count() {
                match items_reader.read()? {
                    ElementItem::Null(ty) => {
                        self.result.push_str(" (ref.null ");
                        self.print_reftype(ty)?;
                        self.result.push_str(")");
                    }
                    ElementItem::Func(idx) if items_reader.uses_exprs() => {
                        self.result.push_str(" (ref.func ");
                        self.print_func_idx(idx)?;
                        self.result.push_str(")");
                    }
                    ElementItem::Func(idx) => {
                        self.result.push_str(" ");
                        self.print_func_idx(idx)?;
                    }
                }
            }
            self.result.push_str(")");
        }
        Ok(())
    }

    fn print_data(&mut self, data: DataSectionReader) -> Result<()> {
        for (i, data) in data.into_iter().enumerate() {
            let data = data?;
            write!(self.result, "\n  (data (;{};) ", i)?;
            match &data.kind {
                DataKind::Passive => {}
                DataKind::Active {
                    memory_index,
                    init_expr,
                } => {
                    if *memory_index != 0 {
                        write!(self.result, " (memory {}) ", memory_index)?;
                    }
                    self.print_init_expr(&init_expr)?;
                    self.result.push_str(" ");
                }
            }
            self.print_bytes(data.data)?;
            self.result.push_str(")");
        }
        Ok(())
    }

    fn print_instances(&mut self, instances: InstanceSectionReader) -> Result<()> {
        for (i, instance) in instances.into_iter().enumerate() {
            let instance = instance?;
            self.newline();
            self.start_group("instance");
            write!(self.result, " (;{};)", i)?;
            self.newline();
            self.start_group("instantiate");
            write!(self.result, " {}", instance.module())?;
            for export in instance.args()? {
                let (kind, index) = export?;
                self.newline();
                self.print_external(kind, index)?;
            }
            self.end_group(); // instantiate
            self.end_group(); // instance
        }
        Ok(())
    }

    fn print_aliases(&mut self, aliases: AliasSectionReader) -> Result<()> {
        for alias in aliases {
            let alias = alias?;
            self.newline();
            self.start_group("alias");
            match alias.kind {
                ExternalKind::Function => {
                    match self.state.names.get(&self.state.func) {
                        Some(name) => write!(self.result, " ${}", name)?,
                        None => write!(self.result, " (;{};)", self.state.func)?,
                    }
                    self.state.func += 1;
                }
                ExternalKind::Table => {
                    write!(self.result, " (;{};)", self.state.table)?;
                    self.state.table += 1;
                }
                ExternalKind::Memory => {
                    write!(self.result, " (;{};)", self.state.memory)?;
                    self.state.memory += 1;
                }
                ExternalKind::Global => {
                    write!(self.result, " (;{};)", self.state.global)?;
                    self.state.global += 1;
                }
                ExternalKind::Instance => {
                    write!(self.result, " (;{};)", self.state.instance)?;
                    self.state.instance += 1;
                }
                ExternalKind::Module => {
                    write!(self.result, " (;{};)", self.state.module)?;
                    self.state.module += 1;
                }
                ExternalKind::Type => self.state.types.push(None),
            }
            self.result.push_str(" ");
            match alias.instance {
                AliasedInstance::Parent => self.result.push_str("parent"),
                AliasedInstance::Child(i) => {
                    self.start_group("instance");
                    write!(self.result, " {}", i)?;
                    self.end_group();
                }
            }
            self.result.push_str(" ");
            match alias.kind {
                ExternalKind::Function => self.start_group("func"),
                ExternalKind::Table => self.start_group("table"),
                ExternalKind::Memory => self.start_group("memory"),
                ExternalKind::Global => self.start_group("global"),
                ExternalKind::Instance => self.start_group("instance"),
                ExternalKind::Module => self.start_group("module"),
                ExternalKind::Type => self.start_group("type"),
            }
            write!(self.result, " {}", alias.index)?;
            self.end_group();
            self.end_group();
        }
        Ok(())
    }

    fn print_module_code(
        &mut self,
        module_code: &mut ModuleCodeSectionReader<'_>,
        tys: ModuleSectionReader<'_>,
    ) -> Result<()> {
        for ty in tys {
            let ty = ty?;
            let module = module_code.read()?;
            let (offset, wasm) = module.raw_bytes();
            self.newline();
            self.start_group("module");
            self.print_contents(
                offset as u64,
                wasm,
                &format!(" (;{};) (type {})", self.state.module, ty),
            )?;
            self.end_group();
            self.state.module += 1;
        }
        Ok(())
    }

    fn print_init_expr(&mut self, expr: &InitExpr) -> Result<()> {
        self.start_group("");
        for (i, op) in expr.get_operators_reader().into_iter().enumerate() {
            match op? {
                Operator::End => {}
                other => {
                    if i > 0 {
                        self.result.push_str(" ");
                    }
                    self.print_operator(&other, self.nesting)?
                }
            }
        }
        self.end_group();
        Ok(())
    }

    fn print_str(&mut self, name: &str) -> Result<()> {
        let mut bytes = [0; 4];
        self.result.push_str("\"");
        for c in name.chars() {
            let v = c as u32;
            if v >= 0x20 && v < 0x7f && c != '"' && c != '\\' && v < 0xff {
                self.result.push(c);
            } else {
                for byte in c.encode_utf8(&mut bytes).as_bytes() {
                    self.hex_byte(*byte);
                }
            }
        }
        self.result.push_str("\"");
        Ok(())
    }

    fn print_bytes(&mut self, bytes: &[u8]) -> Result<()> {
        self.result.push_str("\"");
        for byte in bytes {
            if *byte >= 0x20 && *byte < 0x7f && *byte != b'"' && *byte != b'\\' {
                self.result.push(*byte as char);
            } else {
                self.hex_byte(*byte);
            }
        }
        self.result.push_str("\"");
        return Ok(());
    }

    fn hex_byte(&mut self, byte: u8) {
        fn to_hex(b: u8) -> char {
            if b < 10 {
                (b'0' + b) as char
            } else {
                (b'a' + b - 10) as char
            }
        }
        self.result.push('\\');
        self.result.push(to_hex((byte >> 4) & 0xf));
        self.result.push(to_hex(byte & 0xf));
    }
}

struct NamedLocalPrinter {
    group_name: &'static str,
    in_group: bool,
    end_group_after_local: bool,
}

impl NamedLocalPrinter {
    fn new(group_name: &'static str) -> NamedLocalPrinter {
        NamedLocalPrinter {
            group_name,
            in_group: false,
            end_group_after_local: false,
        }
    }

    fn start_local(&mut self, name: Option<&String>, dst: &mut String) {
        // Named locals must be in their own group, so if we have a name we need
        // to terminate the previous group.
        if name.is_some() && self.in_group {
            dst.push_str(")");
            self.in_group = false;
        }

        // Next we either need a separator if we're already in a group or we
        // need to open a group for our new local.
        dst.push_str(" ");
        if !self.in_group {
            dst.push_str("(");
            dst.push_str(self.group_name);
            dst.push_str(" ");
            self.in_group = true;
        }

        // Print the optional name if given...
        if let Some(name) = name {
            dst.push_str("$");
            dst.push_str(name);
            dst.push_str(" ");
        }
        self.end_group_after_local = name.is_some();
    }

    fn end_local(&mut self, dst: &mut String) {
        if self.end_group_after_local {
            dst.push_str(")");
            self.end_group_after_local = false;
            self.in_group = false;
        }
    }
    fn finish(self, dst: &mut String) {
        if self.in_group {
            dst.push_str(")");
        }
    }
}

macro_rules! print_float {
    ($name:ident $float:ident $uint:ident $sint:ident $exp_bits:tt) => {
        fn $name(&mut self, mut bits: $uint) -> Result<()> {
            // Calculate a few constants
            let int_width = mem::size_of::<$uint>() * 8;
            let exp_width = $exp_bits;
            let mantissa_width = int_width - 1 - exp_width;
            let bias = (1 << (exp_width - 1)) - 1;
            let max_exp = (1 as $sint) << (exp_width - 1);
            let min_exp = -max_exp + 1;

            // Handle `NaN` and infinity specially
            let f = $float::from_bits(bits);
            if bits >> (int_width - 1) != 0 {
                bits ^= 1 << (int_width - 1);
                self.result.push_str("-");
            }
            if f.is_infinite() {
                write!(self.result, "inf (;={};)", f)?;
                return Ok(());
            }
            if f.is_nan() {
                let payload = bits & ((1 << mantissa_width) - 1);
                if payload == 1 << (mantissa_width - 1) {
                    write!(self.result, "nan (;={};)", f)?;
                } else {
                    write!(self.result, "nan:{:#x} (;={};)", payload, f)?;
                }
                return Ok(());
            }

            // Figure out our exponent, but keep in mine that it's in an
            // integer width that may not be supported. As a result we do a few
            // tricks here:
            //
            // * Make the MSB the top bit of the exponent, then shift the
            //   exponent to the bottom. This means we now have a signed
            //   integer in `$sint` width representing the whole exponent.
            // * Do the arithmetic for the exponent (subtract)
            // * Next we only care about the lowest `$exp_bits` bits of the
            //   result, but we do care about the sign. Use sign-carrying of
            //   the signed integer shifts to shift it left then shift it back.
            //
            // Overall this should do basic arithmetic for `$exp_bits` bit
            // numbers and get the result back as a signed integer with `$sint`
            // bits in `exponent` representing the same decimal value.
            let mut exponent = (((bits << 1) as $sint) >> (mantissa_width + 1)).wrapping_sub(bias);
            exponent = (exponent << (int_width - exp_width)) >> (int_width - exp_width);
            let mut fraction = bits & ((1 << mantissa_width) - 1);
            self.result.push_str("0x");
            if bits == 0 {
                self.result.push_str("0p+0");
            } else {
                self.result.push_str("1");
                if fraction > 0 {
                    fraction = fraction << (int_width - mantissa_width);

                    // Apparently the subnormal is handled here. I don't know
                    // what a subnormal is. If someone else does, please let me
                    // know!
                    if exponent == min_exp {
                        let leading = fraction.leading_zeros();
                        if (leading as usize) < int_width - 1 {
                            fraction <<= leading + 1;
                        } else {
                            fraction = 0;
                        }
                        exponent -= leading as $sint;
                    }

                    self.result.push_str(".");
                    while fraction > 0 {
                        write!(self.result, "{:x}", fraction >> (int_width - 4))?;
                        fraction <<= 4;
                    }
                }
                write!(self.result, "p{:+}", exponent)?;
            }
            write!(self.result, " (;={};)", f)?;
            Ok(())
        }
    };
}

impl Printer {
    print_float!(print_f32 f32 u32 i32 8);
    print_float!(print_f64 f64 u64 i64 11);
}
