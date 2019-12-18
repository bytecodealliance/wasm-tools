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
    func: u32,
    memory: u32,
    global: u32,
    table: u32,
    nesting: u32,
    functypes: Vec<FuncType>,
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
        // Reset internal state which will get configured throughout printing.
        self.func = 0;
        self.memory = 0;
        self.global = 0;
        self.table = 0;
        self.functypes.truncate(0);
        self.local_names.drain();
        self.module_name = None;

        // First up try to find the `name` subsection which we'll use to print
        // pretty names everywhere. Also look for the `code` section so we can
        // print out functions as soon as we hit the function section.
        let mut parser = wasmparser::ModuleReader::new(wasm)?;
        let mut code = None;
        while !parser.eof() {
            let section = parser.read()?;
            match section.code {
                SectionCode::Code => code = Some(section.get_code_section_reader()?),
                SectionCode::Custom { name: "name", .. } => {
                    self.register_names(section.get_name_section_reader()?)?;
                }
                _ => {}
            }
        }

        // ... and here we go, time to print all the sections!
        self.result.push_str("(module");
        if let Some(name) = &self.module_name {
            self.result.push_str(" $");
            self.result.push_str(name);
        }
        let mut parser = wasmparser::ModuleReader::new(wasm)?;
        let mut printers = mem::replace(&mut self.printers, HashMap::new());
        while !parser.eof() {
            let section = parser.read()?;
            match section.code {
                SectionCode::Custom { name, .. } => {
                    let range = section.get_binary_reader().range();
                    if let Some(printer) = printers.get_mut(name) {
                        printer(self, range.start, &wasm[range.start..range.end])?;
                    }
                }
                SectionCode::Type => {
                    self.print_types(section.get_type_section_reader()?)?;
                }
                SectionCode::Import => {
                    self.print_imports(section.get_import_section_reader()?)?;
                }
                SectionCode::Function => {
                    let reader = section.get_function_section_reader()?;
                    if reader.get_count() == 0 {
                        continue;
                    }
                    let code = match code.take() {
                        Some(f) => f,
                        None => bail!("found function section without code section"),
                    };
                    self.print_code(code, reader)?;
                }
                SectionCode::Table => {
                    self.print_tables(section.get_table_section_reader()?)?;
                }
                SectionCode::Memory => {
                    self.print_memories(section.get_memory_section_reader()?)?;
                }
                SectionCode::Global => {
                    self.print_globals(section.get_global_section_reader()?)?;
                }
                SectionCode::Export => {
                    self.print_exports(section.get_export_section_reader()?)?;
                }
                SectionCode::Start => {
                    self.result.push_str("\n  (start ");
                    // FIXME(WebAssembly/wabt#1226): we should print the pretty
                    // name here.
                    // self.print_func_idx(section.get_start_section_content()?)?;
                    write!(self.result, "{}", section.get_start_section_content()?)?;
                    self.result.push_str(")");
                }
                SectionCode::Element => {
                    self.print_elems(section.get_element_section_reader()?)?;
                }
                SectionCode::Code => {
                    // printed above with the `Function` section
                }
                SectionCode::Data => {
                    self.print_data(section.get_data_section_reader()?)?;
                }
                SectionCode::DataCount => {
                    // not part of the text format
                }
            }
        }
        self.result.push_str(")");
        self.printers = printers;
        Ok(mem::replace(&mut self.result, String::new()))
    }

    fn register_names(&mut self, names: NameSectionReader<'_>) -> Result<()> {
        for section in names {
            match section? {
                Name::Module(n) => self.module_name = Some(n.get_name()?.to_string()),
                Name::Function(n) => {
                    let mut map = n.get_map()?;
                    for _ in 0..map.get_count() {
                        let name = map.read()?;
                        self.names.insert(name.index, name.name.to_string());
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
                        self.local_names.insert(local_name.func_index, local_map);
                    }
                }
            }
        }
        Ok(())
    }

    fn print_types(&mut self, parser: TypeSectionReader<'_>) -> Result<()> {
        for (i, ty) in parser.into_iter().enumerate() {
            let ty = ty?;
            write!(self.result, "\n  (type (;{};) (func", i)?;
            self.print_functype(&ty, None)?;
            self.result.push_str("))");
            self.functypes.push(ty);
        }
        Ok(())
    }

    fn print_functype_idx(&mut self, idx: u32, names_for: Option<u32>) -> Result<u32> {
        let ty = match self.functypes.get(idx as usize) {
            Some(ty) => ty.clone(),
            None => bail!("function type index `{}` out of bounds", idx),
        };
        self.print_functype(&ty, names_for)
    }

    /// Returns the number of parameters, useful for local index calculations
    /// later.
    fn print_functype(&mut self, ty: &FuncType, names_for: Option<u32>) -> Result<u32> {
        if ty.params.len() > 0 {
            self.result.push_str(" (param");
            // When local variables are named `wasm2wat` seems to have odd
            // behavior where it'll print everything in one `(param)` block
            // until something is named. All further parameters go into a
            // separate `(param)` block. I'm not entirely certain why or if it's
            // really even valid syntax, but for now we mirror `wasm2wat`.
            let mut restart = false;
            for (i, param) in ty.params.iter().enumerate() {
                if restart {
                    self.result.push_str(") (param ");
                    restart = false;
                } else {
                    self.result.push_str(" ");
                }
                let local_names = &self.local_names;
                let name = names_for
                    .and_then(|n| local_names.get(&n))
                    .and_then(|n| n.get(&(i as u32)));
                if let Some(name) = name {
                    self.result.push_str("$");
                    self.result.push_str(name);
                    self.result.push_str(" ");
                    restart = true;
                }
                self.print_valtype(*param)?;
            }
            self.result.push_str(")");
        }
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
            Type::AnyFunc => self.result.push_str("funcref"),
            Type::AnyRef => self.result.push_str("anyref"),
            _ => bail!("unimplemented {:?}", ty),
        }
        Ok(())
    }

    fn print_imports(&mut self, parser: ImportSectionReader<'_>) -> Result<()> {
        use ImportSectionEntryType::*;
        for import in parser {
            let import = import?;
            self.result.push_str("\n  (import ");
            self.print_str(import.module)?;
            self.result.push_str(" ");
            self.print_str(import.field)?;
            self.result.push_str(" (");
            match import.ty {
                Function(f) => {
                    self.result.push_str("func ");
                    match self.names.get(&self.func) {
                        Some(name) => write!(self.result, "${}", name)?,
                        None => write!(self.result, "(;{};)", self.func)?,
                    }
                    write!(self.result, " (type {})", f)?;
                    self.func += 1;
                }
                Table(f) => self.print_table_type(&f)?,
                Memory(f) => self.print_memory_type(&f)?,
                Global(f) => self.print_global_type(&f)?,
            }
            self.result.push_str("))");
        }
        Ok(())
    }

    fn print_table_type(&mut self, ty: &TableType) -> Result<()> {
        write!(self.result, "table (;{};) ", self.table)?;
        self.print_limits(&ty.limits)?;
        self.result.push_str(" ");
        self.print_valtype(ty.element_type)?;
        self.table += 1;
        Ok(())
    }

    fn print_memory_type(&mut self, ty: &MemoryType) -> Result<()> {
        write!(self.result, "memory (;{};) ", self.memory)?;
        self.print_limits(&ty.limits)?;
        if ty.shared {
            self.result.push_str(" shared");
        }
        self.memory += 1;
        Ok(())
    }

    fn print_limits(&mut self, limits: &ResizableLimits) -> Result<()> {
        write!(self.result, "{}", limits.initial)?;
        if let Some(max) = limits.maximum {
            write!(self.result, " {}", max)?;
        }
        Ok(())
    }

    fn print_global_type(&mut self, ty: &GlobalType) -> Result<()> {
        write!(self.result, "global (;{};) ", self.global)?;
        if ty.mutable {
            self.result.push_str("(mut ");
            self.print_valtype(ty.content_type)?;
            self.result.push_str(")");
        } else {
            self.print_valtype(ty.content_type)?;
        }
        self.global += 1;
        Ok(())
    }

    fn print_tables(&mut self, parser: TableSectionReader<'_>) -> Result<()> {
        for table in parser {
            let table = table?;
            self.result.push_str("\n  (");
            self.print_table_type(&table)?;
            self.result.push_str(")");
        }
        Ok(())
    }

    fn print_memories(&mut self, parser: MemorySectionReader<'_>) -> Result<()> {
        for memory in parser {
            let memory = memory?;
            self.result.push_str("\n  (");
            self.print_memory_type(&memory)?;
            self.result.push_str(")");
        }
        Ok(())
    }

    fn print_globals(&mut self, parser: GlobalSectionReader<'_>) -> Result<()> {
        for global in parser {
            let global = global?;
            self.result.push_str("\n  (");
            self.print_global_type(&global.ty)?;
            self.result.push_str(" ");
            self.print_init_expr(&global.init_expr)?;
            self.result.push_str(")");
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
            self.result.push_str("\n  (func ");
            match self.names.get(&self.func) {
                Some(name) => write!(self.result, "${}", name)?,
                None => write!(self.result, "(;{};)", self.func)?,
            }
            write!(self.result, " (type {})", ty)?;
            let params = self.print_functype_idx(ty, Some(self.func))?;

            let mut first = true;
            let mut idx = params;
            let mut restart = false;
            for local in body.get_locals_reader()? {
                let (cnt, ty) = local?;
                if first {
                    self.result.push_str("\n    (local");
                    first = false;
                }
                for _ in 0..cnt {
                    // See comments in `print_functype` for why `restart` is
                    // here.
                    if restart {
                        self.result.push_str(") (local ");
                        restart = false;
                    } else {
                        self.result.push_str(" ");
                    }
                    if let Some(name) = self.local_names.get(&self.func).and_then(|m| m.get(&idx)) {
                        self.result.push_str("$");
                        self.result.push_str(name);
                        self.result.push_str(" ");
                        restart = true;
                    }
                    self.print_valtype(ty)?;
                    idx += 1;
                }
            }
            if !first {
                self.result.push_str(")");
            }

            self.nesting = 1;
            for operator in body.get_operators_reader()? {
                let operator = operator?;
                match operator {
                    Operator::Else => self.nesting -= 1,
                    Operator::End if self.nesting == 1 => continue,
                    Operator::End => self.nesting -= 1,
                    _ => {}
                }
                self.result.push_str("\n  ");
                for _ in 0..self.nesting {
                    self.result.push_str("  ");
                }
                self.print_operator(&operator)?;
            }
            self.result.push_str(")");

            self.func += 1;
        }
        Ok(())
    }

    fn print_operator(&mut self, op: &Operator<'_>) -> Result<()> {
        use Operator::*;
        let nesting = self.nesting;
        let label =
            |relative: u32| match nesting.checked_sub(relative).and_then(|s| s.checked_sub(1)) {
                Some(i) => format!("@{}", i),
                None => format!(" INVALID "),
            };
        match op {
            Nop => self.result.push_str("nop"),
            Unreachable => self.result.push_str("unreachable"),
            Block { ty } => {
                self.result.push_str("block");
                self.print_blockty(ty)?;
                write!(self.result, "  ;; label = @{}", self.nesting)?;
                self.nesting += 1;
            }
            Loop { ty } => {
                self.result.push_str("loop");
                self.print_blockty(ty)?;
                write!(self.result, "  ;; label = @{}", self.nesting)?;
                self.nesting += 1;
            }
            If { ty } => {
                self.result.push_str("if");
                self.print_blockty(ty)?;
                write!(self.result, "  ;; label = @{}", self.nesting)?;
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
                let (table, default) = table.read_table()?;
                self.result.push_str("br_table");
                for item in table.iter().cloned().chain(Some(default)) {
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

            Drop => self.result.push_str("drop"),
            Select => self.result.push_str("select"),
            LocalGet { local_index } => {
                self.result.push_str("local.get ");
                self.print_local_idx(self.func, *local_index)?;
            }
            LocalSet { local_index } => {
                self.result.push_str("local.set ");
                self.print_local_idx(self.func, *local_index)?;
            }
            LocalTee { local_index } => {
                self.result.push_str("local.tee ");
                self.print_local_idx(self.func, *local_index)?;
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

            MemorySize { .. } => self.result.push_str("memory.size"),
            MemoryGrow { .. } => self.result.push_str("memory.grow"),

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

            RefNull => self.result.push_str("ref.null"),
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

            MemoryInit { segment } => write!(self.result, "memory.init {}", segment)?,
            DataDrop { segment } => write!(self.result, "data.drop {}", segment)?,
            MemoryCopy => self.result.push_str("memory.copy"),
            MemoryFill => self.result.push_str("memory.fill"),

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

            AtomicNotify { memarg } => self.mem_instr("atomic.notify", memarg, 4)?,
            I32AtomicWait { memarg } => self.mem_instr("i32.atomic.wait", memarg, 4)?,
            I64AtomicWait { memarg } => self.mem_instr("i64.atomic.wait", memarg, 8)?,
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

            I8x16Neg => self.result.push_str("i8x16.neg"),
            I8x16AnyTrue => self.result.push_str("i8x16.any_true"),
            I8x16AllTrue => self.result.push_str("i8x16.all_true"),
            I8x16Shl => self.result.push_str("i8x16.shl"),
            I8x16ShrU => self.result.push_str("i8x16.shr_u"),
            I8x16ShrS => self.result.push_str("i8x16.shr_s"),
            I8x16Add => self.result.push_str("i8x16.add"),
            I8x16AddSaturateS => self.result.push_str("i8x16.add_saturate_s"),
            I8x16AddSaturateU => self.result.push_str("i8x16.add_saturate_u"),
            I8x16Sub => self.result.push_str("i8x16.sub"),
            I8x16SubSaturateS => self.result.push_str("i8x16.sub_saturate_s"),
            I8x16SubSaturateU => self.result.push_str("i8x16.sub_saturate_u"),
            I8x16Mul => self.result.push_str("i8x16.mul"),

            I16x8Neg => self.result.push_str("i16x8.neg"),
            I16x8AnyTrue => self.result.push_str("i16x8.any_true"),
            I16x8AllTrue => self.result.push_str("i16x8.all_true"),
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

            I32x4Neg => self.result.push_str("i32x4.neg"),
            I32x4AnyTrue => self.result.push_str("i32x4.any_true"),
            I32x4AllTrue => self.result.push_str("i32x4.all_true"),
            I32x4Shl => self.result.push_str("i32x4.shl"),
            I32x4ShrU => self.result.push_str("i32x4.shr_u"),
            I32x4ShrS => self.result.push_str("i32x4.shr_s"),
            I32x4Add => self.result.push_str("i32x4.add"),
            I32x4Sub => self.result.push_str("i32x4.sub"),
            I32x4Mul => self.result.push_str("i32x4.mul"),

            I64x2Neg => self.result.push_str("i64x2.neg"),
            I64x2AnyTrue => self.result.push_str("i64x2.any_true"),
            I64x2AllTrue => self.result.push_str("i64x2.all_true"),
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
            I64x2TruncSatF64x2S => self.result.push_str("i64x2.trunc_sat_f64x2_s"),
            I64x2TruncSatF64x2U => self.result.push_str("i64x2.trunc_sat_f64x2_u"),
            F32x4ConvertI32x4S => self.result.push_str("f32x4.convert_i32x4_s"),
            F32x4ConvertI32x4U => self.result.push_str("f32x4.convert_i32x4_u"),
            F64x2ConvertI64x2S => self.result.push_str("f64x2.convert_i64x2_s"),
            F64x2ConvertI64x2U => self.result.push_str("f64x2.convert_i64x2_u"),

            V8x16Swizzle => self.result.push_str("v8x16.swizzle"),
            V8x16Shuffle { lanes } => {
                self.result.push_str("v8x16.shuffle");
                // FIXME(WebAssembly/wabt#1227): the double space and trailing
                // space seem like bugs in wabt
                for lane in lanes {
                    write!(self.result, "  {}", lane)?;
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

            I16x8Load8x8S { memarg } => self.mem_instr("i16x8.load8x8_s", memarg, 1)?,
            I16x8Load8x8U { memarg } => self.mem_instr("i16x8.load8x8_u", memarg, 1)?,
            I32x4Load16x4S { memarg } => self.mem_instr("i32x4.load16x4_s", memarg, 2)?,
            I32x4Load16x4U { memarg } => self.mem_instr("i32x4.load16x4_u", memarg, 2)?,
            I64x2Load32x2S { memarg } => self.mem_instr("i64x2.load32x2_s", memarg, 4)?,
            I64x2Load32x2U { memarg } => self.mem_instr("i64x2.load32x2_u", memarg, 4)?,

            I8x16RoundingAverageU => self.result.push_str("i8x16.avgr_u"),
            I16x8RoundingAverageU => self.result.push_str("i16x8.avgr_u"),
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
        if memarg.offset != 0 {
            write!(self.result, " offset={}", memarg.offset)?;
        }
        let align = 1 << (memarg.flags & 0x1f);
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
            self.result.push_str("\n  (export ");
            self.print_str(export.field)?;
            self.result.push_str(" (");
            match export.kind {
                ExternalKind::Function => {
                    self.result.push_str("func ");
                    self.print_func_idx(export.index)?;
                }
                ExternalKind::Table => write!(self.result, "table {}", export.index)?,
                ExternalKind::Global => write!(self.result, "global {}", export.index)?,
                ExternalKind::Memory => write!(self.result, "memory {}", export.index)?,
            }
            self.result.push_str("))");
        }
        return Ok(());
    }

    /// Prints a function index specified, using the identifier for the function
    /// from the `name` section if it was present.
    ///
    /// This will either print `$foo` or `idx` as a raw integer.
    pub fn print_func_idx(&mut self, idx: u32) -> Result<()> {
        match self.names.get(&idx) {
            Some(name) => write!(self.result, "${}", name)?,
            None => write!(self.result, "{}", idx)?,
        }
        Ok(())
    }

    fn print_local_idx(&mut self, func: u32, idx: u32) -> Result<()> {
        match self.local_names.get(&func).and_then(|f| f.get(&idx)) {
            Some(name) => write!(self.result, "${}", name)?,
            None => write!(self.result, "{}", idx)?,
        }
        Ok(())
    }

    fn print_elems(&mut self, data: ElementSectionReader) -> Result<()> {
        for (i, elem) in data.into_iter().enumerate() {
            let mut elem = elem?;
            write!(self.result, "\n  (elem (;{};) ", i)?;
            match &mut elem.kind {
                ElementKind::Passive { ty, items } => {
                    self.print_valtype(*ty)?;
                    let mut items = items.get_items_reader()?;
                    for _ in 0..items.get_count() {
                        match items.read()? {
                            PassiveElementItem::Func(idx) => {
                                self.result.push_str(" (ref.func ");
                                self.print_func_idx(idx)?;
                                self.result.push_str(")");
                            }
                            PassiveElementItem::Null => {
                                self.result.push_str(" (ref.null)");
                            }
                        }
                    }
                }
                ElementKind::Active {
                    table_index,
                    init_expr,
                    items,
                } => {
                    if *table_index != 0 {
                        write!(self.result, "{} ", table_index)?;
                    }
                    self.print_init_expr(&init_expr)?;
                    for item in items.get_items_reader()? {
                        self.result.push_str(" ");
                        self.print_func_idx(item?)?;
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
                        bail!("don't know how to print active nonzero memory data");
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

    fn print_init_expr(&mut self, expr: &InitExpr) -> Result<()> {
        self.result.push_str("(");
        self.nesting = 1;
        for op in expr.get_operators_reader() {
            match op? {
                Operator::End => {}
                other => self.print_operator(&other)?,
            }
        }
        self.result.push_str(")");
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

macro_rules! print_float {
    ($name:ident $float:ident $uint:ident $sint:ident $exp_bits:tt) => (
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
                return Ok(())
            }
            if f.is_nan() {
                let payload = bits & ((1 << mantissa_width) - 1);
                if payload == 1 << (mantissa_width - 1) {
                    write!(self.result, "nan (;={};)", f)?;
                } else {
                    write!(self.result, "nan:{:#x} (;={};)", payload, f)?;
                }
                return Ok(())
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
    )
}

impl Printer {
    print_float!(print_f32 f32 u32 i32 8);
    print_float!(print_f64 f64 u64 i64 11);
}
