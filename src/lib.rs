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
    printers: HashMap<String, Box<dyn FnMut(&mut Printer, &[u8]) -> Result<()>>>,
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
    pub fn add_custom_section_printer(
        &mut self,
        section: &str,
        printer: impl FnMut(&mut Printer, &[u8]) -> Result<()> + 'static,
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
                        printer(self, &wasm[range.start..range.end])?;
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
                    // TODO: this seems to be a bug in `wabt`, it probably
                    // should be printing the name of the function rather than
                    // the index here.
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

            I32Load { memarg } => self.print_mem_instr("i32.load", memarg, 4)?,
            I64Load { memarg } => self.print_mem_instr("i64.load", memarg, 8)?,
            F32Load { memarg } => self.print_mem_instr("f32.load", memarg, 4)?,
            F64Load { memarg } => self.print_mem_instr("f64.load", memarg, 8)?,
            I32Load8S { memarg } => self.print_mem_instr("i32.load8_s", memarg, 1)?,
            I32Load8U { memarg } => self.print_mem_instr("i32.load8_u", memarg, 1)?,
            I32Load16S { memarg } => self.print_mem_instr("i32.load16_s", memarg, 2)?,
            I32Load16U { memarg } => self.print_mem_instr("i32.load16_u", memarg, 2)?,
            I64Load8S { memarg } => self.print_mem_instr("i64.load8_s", memarg, 1)?,
            I64Load8U { memarg } => self.print_mem_instr("i64.load8_u", memarg, 1)?,
            I64Load16S { memarg } => self.print_mem_instr("i64.load16_s", memarg, 2)?,
            I64Load16U { memarg } => self.print_mem_instr("i64.load16_u", memarg, 2)?,
            I64Load32S { memarg } => self.print_mem_instr("i64.load32_s", memarg, 4)?,
            I64Load32U { memarg } => self.print_mem_instr("i64.load32_u", memarg, 4)?,

            I32Store { memarg } => self.print_mem_instr("i32.store", memarg, 4)?,
            I64Store { memarg } => self.print_mem_instr("i64.store", memarg, 8)?,
            F32Store { memarg } => self.print_mem_instr("f32.store", memarg, 4)?,
            F64Store { memarg } => self.print_mem_instr("f64.store", memarg, 8)?,
            I32Store8 { memarg } => self.print_mem_instr("i32.store8", memarg, 1)?,
            I32Store16 { memarg } => self.print_mem_instr("i32.store16", memarg, 2)?,
            I64Store8 { memarg } => self.print_mem_instr("i64.store8", memarg, 1)?,
            I64Store16 { memarg } => self.print_mem_instr("i64.store16", memarg, 2)?,
            I64Store32 { memarg } => self.print_mem_instr("i64.store32", memarg, 4)?,

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
            RefFunc { index } => {
                self.result.push_str("ref.func ");
                self.print_func_idx(*index)?;
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

            TableInit { segment } => write!(self.result, "table.init {}", segment)?,
            ElemDrop { segment } => write!(self.result, "elem.drop {}", segment)?,
            TableCopy => self.result.push_str("table.copy"),
            TableGet { table } => write!(self.result, "table.get {}", table)?,
            TableSet { table } => write!(self.result, "table.set {}", table)?,
            TableGrow { table } => write!(self.result, "table.grow {}", table)?,
            TableSize { table } => write!(self.result, "table.size {}", table)?,

            _ => {}
        }
        Ok(())
    }

    fn print_mem_instr(
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
                    for _ in 0..items.get_count() {
                        match items.get_next_func_idx()? {
                            Some(idx) => {
                                self.result.push_str(" (ref.func ");
                                self.print_func_idx(idx)?;
                                self.result.push_str(")");
                            }
                            None => {
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
                        bail!("don't know how to print active nonzero table elem");
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
