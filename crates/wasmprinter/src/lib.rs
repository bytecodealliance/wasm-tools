//! A crate to convert a WebAssembly binary to its textual representation in the
//! WebAssembly Text Format (WAT).
//!
//! This crate is intended for developer toolchains and debugging, supporting
//! human-readable versions of a wasm binary. This can also be useful when
//! developing wasm toolchain support in Rust for various purposes like testing
//! and debugging and such.

#![deny(missing_docs)]

use anyhow::{bail, Context, Result};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write};
use std::mem;
use std::path::Path;
use wasmparser::*;

const MAX_LOCALS: u32 = 50000;
const MAX_NESTING_TO_PRINT: u32 = 50;
const MAX_WASM_FUNCTIONS: u32 = 1_000_000;

mod operator;

/// Reads a WebAssembly `file` from the filesystem and then prints it into an
/// in-memory `String`.
pub fn print_file(file: impl AsRef<Path>) -> Result<String> {
    let file = file.as_ref();
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
    print_offsets: bool,
    printers: HashMap<String, Box<dyn FnMut(&mut Printer, usize, &[u8]) -> Result<()>>>,
    result: String,
    nesting: u32,
    line: usize,
    group_lines: Vec<usize>,
}

#[derive(Default)]
struct CoreState {
    types: Vec<Option<FuncType>>,
    funcs: u32,
    memories: u32,
    tags: u32,
    globals: u32,
    tables: u32,
    labels: u32,
    modules: u32,
    instances: u32,
    func_names: HashMap<u32, Naming>,
    local_names: HashMap<(u32, u32), Naming>,
    label_names: HashMap<(u32, u32), Naming>,
    type_names: HashMap<u32, Naming>,
    table_names: HashMap<u32, Naming>,
    memory_names: HashMap<u32, Naming>,
    global_names: HashMap<u32, Naming>,
    element_names: HashMap<u32, Naming>,
    data_names: HashMap<u32, Naming>,
    module_names: HashMap<u32, Naming>,
    instance_names: HashMap<u32, Naming>,
}

#[derive(Default)]
struct ComponentState {
    types: u32,
    funcs: u32,
    instances: u32,
    components: u32,
    values: u32,
    type_names: HashMap<u32, Naming>,
    func_names: HashMap<u32, Naming>,
    component_names: HashMap<u32, Naming>,
    instance_names: HashMap<u32, Naming>,
    value_names: HashMap<u32, Naming>,
}

struct State {
    encoding: Encoding,
    name: Option<Naming>,
    core: CoreState,
    component: ComponentState,
}

impl State {
    fn new(encoding: Encoding) -> Self {
        Self {
            encoding,
            name: None,
            core: CoreState::default(),
            component: ComponentState::default(),
        }
    }
}

struct Naming {
    identifier: Option<String>,
    name: String,
}

impl Printer {
    /// Creates a new `Printer` object that's ready to start printing wasm
    /// binaries to strings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Whether or not to print binary offsets of each item as comments in the
    /// text format whenever a newline is printed.
    pub fn print_offsets(&mut self, print: bool) {
        self.print_offsets = print;
    }

    /// Registers a custom `printer` function to get invoked whenever a custom
    /// section of name `section` is seen.
    ///
    /// This can be used to register printers into a textual format for custom
    /// sections, such as by emitting annotations and/or other textual
    /// references (maybe comments!)
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
        self.print_contents(wasm)?;
        Ok(mem::take(&mut self.result))
    }

    fn read_names_and_code<'a>(
        &mut self,
        mut bytes: &'a [u8],
        mut parser: Parser,
        state: &mut State,
        code: &mut Vec<FunctionBody<'a>>,
    ) -> Result<()> {
        loop {
            let payload = match parser.parse(bytes, true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    payload
                }
            };

            match payload {
                Payload::FunctionSection(s) => {
                    if s.count() > MAX_WASM_FUNCTIONS {
                        bail!(
                            "module contains {} functions which exceeds the limit of {}",
                            s.count(),
                            MAX_WASM_FUNCTIONS
                        );
                    }
                    code.reserve(s.count() as usize);
                }
                Payload::CodeSectionEntry(f) => {
                    code.push(f);
                }
                Payload::ModuleSection { range, .. } | Payload::ComponentSection { range, .. } => {
                    let offset = range.end - range.start;
                    if offset > bytes.len() {
                        bail!("invalid module or component section range");
                    }
                    bytes = &bytes[offset..];
                }

                // Ignore any error associated with the name sections.
                Payload::CustomSection(c) if c.name() == "name" => {
                    let reader = NameSectionReader::new(c.data(), c.data_offset());
                    drop(self.register_names(state, reader));
                }
                Payload::CustomSection(c) if c.name() == "component-name" => {
                    let reader = ComponentNameSectionReader::new(c.data(), c.data_offset());
                    drop(self.register_component_names(state, reader));
                }

                Payload::End(_) => break,
                _ => {}
            }
        }

        Ok(())
    }

    fn ensure_module(states: &[State]) -> Result<()> {
        if !matches!(states.last().unwrap().encoding, Encoding::Module) {
            bail!("a module section was encountered when parsing a component");
        }

        Ok(())
    }

    fn ensure_component(states: &[State]) -> Result<()> {
        if !matches!(states.last().unwrap().encoding, Encoding::Component) {
            bail!("a component section was encountered when parsing a module");
        }

        Ok(())
    }

    fn print_contents(&mut self, mut bytes: &[u8]) -> Result<()> {
        let mut expected = None;
        let mut states: Vec<State> = Vec::new();
        let mut parser = Parser::new(0);
        let mut parsers = Vec::new();
        let mut code = Vec::new();
        let mut code_printed = false;

        loop {
            let payload = match parser.parse(bytes, true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    payload
                }
            };
            match payload {
                Payload::Version { encoding, .. } => {
                    if let Some(e) = expected {
                        if encoding != e {
                            bail!("incorrect encoding for nested module or component");
                        }
                        expected = None;
                    }

                    assert!(states.last().map(|s| s.encoding) != Some(Encoding::Module));

                    match encoding {
                        Encoding::Module => {
                            states.push(State::new(Encoding::Module));
                            if states.len() > 1 {
                                self.start_group("core module");
                            } else {
                                self.start_group("module");
                            }

                            if states.len() > 1 {
                                let parent = &states[states.len() - 2];
                                self.result.push(' ');
                                self.print_name(&parent.core.module_names, parent.core.modules)?;
                            }
                        }
                        Encoding::Component => {
                            states.push(State::new(Encoding::Component));
                            self.start_group("component");

                            if states.len() > 1 {
                                let parent = &states[states.len() - 2];
                                self.result.push(' ');
                                self.print_name(
                                    &parent.component.component_names,
                                    parent.component.components,
                                )?;
                            }
                        }
                    }

                    let len = states.len();
                    let state = states.last_mut().unwrap();

                    // First up try to find the `name` subsection which we'll use to print
                    // pretty names everywhere. Also look for the `code` section so we can
                    // print out functions as soon as we hit the function section.
                    code.clear();
                    code_printed = false;
                    self.read_names_and_code(bytes, parser.clone(), state, &mut code)?;

                    if len == 1 {
                        if let Some(name) = state.name.as_ref() {
                            self.result.push(' ');
                            name.write(&mut self.result);
                        }
                    }
                }
                Payload::CustomSection(c) => {
                    let mut printers = mem::take(&mut self.printers);
                    if let Some(printer) = printers.get_mut(c.name()) {
                        printer(self, c.data_offset(), c.data())?;
                    }
                    self.printers = printers;
                }
                Payload::TypeSection(s) => self.print_types(states.last_mut().unwrap(), s)?,
                Payload::ImportSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_imports(states.last_mut().unwrap(), s)?
                }
                Payload::FunctionSection(reader) => {
                    Self::ensure_module(&states)?;
                    if mem::replace(&mut code_printed, true) {
                        bail!("function section appeared twice in module");
                    }
                    if reader.count() == 0 {
                        continue;
                    }
                    self.print_code(states.last_mut().unwrap(), &code, reader)?;
                }
                Payload::TableSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_tables(states.last_mut().unwrap(), s)?
                }
                Payload::MemorySection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_memories(states.last_mut().unwrap(), s)?
                }
                Payload::TagSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_tags(states.last_mut().unwrap(), s)?
                }
                Payload::GlobalSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_globals(states.last_mut().unwrap(), s)?
                }
                Payload::ExportSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_exports(states.last().unwrap(), s)?
                }
                Payload::StartSection { func, range } => {
                    Self::ensure_module(&states)?;
                    self.newline(range.start);
                    self.start_group("start ");
                    self.print_idx(&states.last().unwrap().core.func_names, func)?;
                    self.end_group();
                }
                Payload::ElementSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_elems(states.last_mut().unwrap(), s)?;
                }
                // printed with the `Function` section, so we
                // skip this section
                Payload::CodeSectionStart { size, .. } => {
                    Self::ensure_module(&states)?;
                    bytes = &bytes[size as usize..];
                    parser.skip_section();
                }
                Payload::CodeSectionEntry(_) => unreachable!(),
                Payload::DataCountSection { .. } => {
                    Self::ensure_module(&states)?;
                    // not part of the text format
                }
                Payload::DataSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_data(states.last_mut().unwrap(), s)?;
                }

                Payload::ModuleSection {
                    parser: inner,
                    range,
                } => {
                    Self::ensure_component(&states)?;
                    expected = Some(Encoding::Module);
                    parsers.push(parser);
                    parser = inner;
                    self.newline(range.start);
                }
                Payload::InstanceSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_instances(states.last_mut().unwrap(), s)?;
                }
                Payload::CoreTypeSection(s) => self.print_core_types(&mut states, s)?,
                Payload::ComponentSection {
                    parser: inner,
                    range,
                } => {
                    Self::ensure_component(&states)?;
                    expected = Some(Encoding::Component);
                    parsers.push(parser);
                    parser = inner;
                    self.newline(range.start);
                }
                Payload::ComponentInstanceSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_instances(states.last_mut().unwrap(), s)?;
                }
                Payload::ComponentAliasSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_aliases(&mut states, s)?;
                }
                Payload::ComponentTypeSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_types(&mut states, s)?;
                }
                Payload::ComponentCanonicalSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_canonical_functions(states.last_mut().unwrap(), s)?;
                }
                Payload::ComponentStartSection { start, range } => {
                    Self::ensure_component(&states)?;
                    self.print_component_start(states.last_mut().unwrap(), range.start, start)?;
                }
                Payload::ComponentImportSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_imports(states.last_mut().unwrap(), s)?;
                }
                Payload::ComponentExportSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_exports(states.last_mut().unwrap(), s)?;
                }

                Payload::End(_) => {
                    self.end_group(); // close the `module` or `component` group

                    let state = states.pop().unwrap();
                    if let Some(parent) = states.last_mut() {
                        match state.encoding {
                            Encoding::Module => {
                                parent.core.modules += 1;
                            }
                            Encoding::Component => {
                                parent.component.components += 1;
                            }
                        }
                        parser = parsers.pop().unwrap();
                    } else {
                        break;
                    }
                }

                Payload::UnknownSection { id, .. } => bail!("found unknown section `{}`", id),
            }
        }

        Ok(())
    }

    fn start_group(&mut self, name: &str) {
        self.result.push('(');
        self.result.push_str(name);
        self.nesting += 1;
        self.group_lines.push(self.line);
    }

    fn end_group(&mut self) {
        self.nesting -= 1;
        if let Some(line) = self.group_lines.pop() {
            if line != self.line {
                self.newline_unknown_pos();
            }
        }
        self.result.push(')');
    }

    fn register_names(&mut self, state: &mut State, names: NameSectionReader<'_>) -> Result<()> {
        fn indirect_name_map(
            into: &mut HashMap<(u32, u32), Naming>,
            names: IndirectNameMap<'_>,
            name: &str,
        ) -> Result<()> {
            for indirect in names {
                let indirect = indirect?;
                let mut used = match name {
                    // labels can be shadowed, so maintaining the used names is not useful.
                    "label" => None,
                    "local" => Some(HashSet::new()),
                    _ => unimplemented!("{name} is an unknown type of indirect names"),
                };
                for naming in indirect.names {
                    let naming = naming?;
                    into.insert(
                        (indirect.index, naming.index),
                        Naming::new(naming.name, naming.index, name, used.as_mut()),
                    );
                }
            }
            Ok(())
        }

        for section in names {
            match section? {
                Name::Module { name, .. } => {
                    let name = Naming::new(name, 0, "module", None);
                    state.name = Some(name);
                }
                Name::Function(n) => name_map(&mut state.core.func_names, n, "func")?,
                Name::Local(n) => indirect_name_map(&mut state.core.local_names, n, "local")?,
                Name::Label(n) => indirect_name_map(&mut state.core.label_names, n, "label")?,
                Name::Type(n) => name_map(&mut state.core.type_names, n, "type")?,
                Name::Table(n) => name_map(&mut state.core.table_names, n, "table")?,
                Name::Memory(n) => name_map(&mut state.core.memory_names, n, "memory")?,
                Name::Global(n) => name_map(&mut state.core.global_names, n, "global")?,
                Name::Element(n) => name_map(&mut state.core.element_names, n, "elem")?,
                Name::Data(n) => name_map(&mut state.core.data_names, n, "data")?,
                Name::Unknown { .. } => (),
            }
        }
        Ok(())
    }

    fn register_component_names(
        &mut self,
        state: &mut State,
        names: ComponentNameSectionReader<'_>,
    ) -> Result<()> {
        for section in names {
            match section? {
                ComponentName::Component { name, .. } => {
                    let name = Naming::new(name, 0, "component", None);
                    state.name = Some(name);
                }
                ComponentName::CoreFuncs(n) => {
                    name_map(&mut state.core.func_names, n, "core-func")?
                }
                ComponentName::CoreTypes(n) => {
                    name_map(&mut state.core.type_names, n, "core-type")?
                }
                ComponentName::CoreTables(n) => {
                    name_map(&mut state.core.table_names, n, "core-table")?
                }
                ComponentName::CoreMemories(n) => {
                    name_map(&mut state.core.memory_names, n, "core-memory")?
                }
                ComponentName::CoreGlobals(n) => {
                    name_map(&mut state.core.global_names, n, "core-global")?
                }
                ComponentName::CoreModules(n) => {
                    name_map(&mut state.core.module_names, n, "core-module")?
                }
                ComponentName::CoreInstances(n) => {
                    name_map(&mut state.core.instance_names, n, "core-instance")?
                }
                ComponentName::Types(n) => name_map(&mut state.component.type_names, n, "type")?,
                ComponentName::Instances(n) => {
                    name_map(&mut state.component.instance_names, n, "instance")?
                }
                ComponentName::Components(n) => {
                    name_map(&mut state.component.component_names, n, "component")?
                }
                ComponentName::Funcs(n) => name_map(&mut state.component.func_names, n, "func")?,
                ComponentName::Values(n) => name_map(&mut state.component.value_names, n, "value")?,
                ComponentName::Unknown { .. } => (),
            }
        }
        Ok(())
    }

    fn print_core_type(&mut self, states: &mut Vec<State>, ty: wasmparser::CoreType) -> Result<()> {
        self.start_group("core type ");
        self.print_name(
            &states.last().unwrap().core.type_names,
            states.last().unwrap().core.types.len() as u32,
        )?;
        let ty = match ty {
            wasmparser::CoreType::Func(ty) => {
                self.result.push(' ');
                self.start_group("func");
                self.print_func_type(states.last().unwrap(), &ty, None)?;
                self.end_group();
                Some(ty)
            }
            wasmparser::CoreType::Module(decls) => {
                self.print_module_type(states, decls.into_vec())?;
                None
            }
        };
        self.end_group(); // `core type` itself

        states.last_mut().unwrap().core.types.push(ty);
        Ok(())
    }

    fn print_type(&mut self, state: &mut State, ty: wasmparser::Type) -> Result<()> {
        self.start_group("type ");
        self.print_name(&state.core.type_names, state.core.types.len() as u32)?;
        self.result.push(' ');
        let ty = match ty {
            wasmparser::Type::Func(ty) => {
                self.start_group("func");
                self.print_func_type(state, &ty, None)?;
                self.end_group();
                ty
            }
        };
        self.end_group(); // `type` itself
        state.core.types.push(Some(ty));
        Ok(())
    }

    fn print_core_types(
        &mut self,
        states: &mut Vec<State>,
        parser: CoreTypeSectionReader<'_>,
    ) -> Result<()> {
        for ty in parser.into_iter_with_offsets() {
            let (offset, ty) = ty?;
            self.newline(offset);
            self.print_core_type(states, ty)?;
        }

        Ok(())
    }

    fn print_types(&mut self, state: &mut State, parser: TypeSectionReader<'_>) -> Result<()> {
        for ty in parser.into_iter_with_offsets() {
            let (offset, ty) = ty?;
            self.newline(offset);
            self.print_type(state, ty)?;
        }

        Ok(())
    }

    fn print_core_functype_idx(
        &mut self,
        state: &State,
        idx: u32,
        names_for: Option<u32>,
    ) -> Result<Option<u32>> {
        self.print_type_ref(state, idx, true, None)?;

        match state.core.types.get(idx as usize) {
            Some(Some(ty)) => self.print_func_type(state, ty, names_for).map(Some),
            Some(None) | None => Ok(None),
        }
    }

    /// Returns the number of parameters, useful for local index calculations
    /// later.
    fn print_func_type(
        &mut self,
        state: &State,
        ty: &FuncType,
        names_for: Option<u32>,
    ) -> Result<u32> {
        if !ty.params().is_empty() {
            self.result.push(' ');
        }

        let mut params = NamedLocalPrinter::new("param");
        // Note that named parameters must be alone in a `param` block, so
        // we need to be careful to terminate previous param blocks and open
        // a new one if that's the case with a named parameter.
        for (i, param) in ty.params().iter().enumerate() {
            let name = names_for.and_then(|n| state.core.local_names.get(&(n, i as u32)));
            params.start_local(name, &mut self.result);
            self.print_valtype(*param)?;
            params.end_local(&mut self.result);
        }
        params.finish(&mut self.result);
        if !ty.results().is_empty() {
            self.result.push_str(" (result");
            for result in ty.results().iter() {
                self.result.push(' ');
                self.print_valtype(*result)?;
            }
            self.result.push(')');
        }
        Ok(ty.params().len() as u32)
    }

    fn print_valtype(&mut self, ty: ValType) -> Result<()> {
        match ty {
            ValType::I32 => self.result.push_str("i32"),
            ValType::I64 => self.result.push_str("i64"),
            ValType::F32 => self.result.push_str("f32"),
            ValType::F64 => self.result.push_str("f64"),
            ValType::V128 => self.result.push_str("v128"),
            ValType::FuncRef => self.result.push_str("funcref"),
            ValType::ExternRef => self.result.push_str("externref"),
        }
        Ok(())
    }

    fn print_reftype(&mut self, ty: ValType) -> Result<()> {
        match ty {
            ValType::FuncRef => self.result.push_str("func"),
            ValType::ExternRef => self.result.push_str("extern"),
            _ => bail!("invalid reference type {:?}", ty),
        }
        Ok(())
    }

    fn print_imports(&mut self, state: &mut State, parser: ImportSectionReader<'_>) -> Result<()> {
        for import in parser.into_iter_with_offsets() {
            let (offset, import) = import?;
            self.newline(offset);
            self.print_import(state, &import, true)?;
            match import.ty {
                TypeRef::Func(_) => state.core.funcs += 1,
                TypeRef::Table(_) => state.core.tables += 1,
                TypeRef::Memory(_) => state.core.memories += 1,
                TypeRef::Tag(_) => state.core.tags += 1,
                TypeRef::Global(_) => state.core.globals += 1,
            }
        }
        Ok(())
    }

    fn print_import(&mut self, state: &State, import: &Import<'_>, index: bool) -> Result<()> {
        self.start_group("import ");
        self.print_str(import.module)?;
        self.result.push(' ');
        self.print_str(import.name)?;
        self.result.push(' ');
        self.print_import_ty(state, &import.ty, index)?;
        self.end_group();
        Ok(())
    }

    fn print_import_ty(&mut self, state: &State, ty: &TypeRef, index: bool) -> Result<()> {
        match ty {
            TypeRef::Func(f) => {
                self.start_group("func ");
                if index {
                    self.print_name(&state.core.func_names, state.core.funcs)?;
                    self.result.push(' ');
                }
                self.print_type_ref(state, *f, true, None)?;
            }
            TypeRef::Table(f) => self.print_table_type(state, f, index)?,
            TypeRef::Memory(f) => self.print_memory_type(state, f, index)?,
            TypeRef::Tag(f) => self.print_tag_type(state, f, index)?,
            TypeRef::Global(f) => self.print_global_type(state, f, index)?,
        }
        self.end_group();
        Ok(())
    }

    fn print_table_type(&mut self, state: &State, ty: &TableType, index: bool) -> Result<()> {
        self.start_group("table ");
        if index {
            self.print_name(&state.core.table_names, state.core.tables)?;
            self.result.push(' ');
        }
        self.print_limits(ty.initial, ty.maximum)?;
        self.result.push(' ');
        self.print_valtype(ty.element_type)?;
        Ok(())
    }

    fn print_memory_type(&mut self, state: &State, ty: &MemoryType, index: bool) -> Result<()> {
        self.start_group("memory ");
        if index {
            self.print_name(&state.core.memory_names, state.core.memories)?;
            self.result.push(' ');
        }
        if ty.memory64 {
            self.result.push_str("i64 ");
        }
        self.print_limits(ty.initial, ty.maximum)?;
        if ty.shared {
            self.result.push_str(" shared");
        }
        Ok(())
    }

    fn print_tag_type(&mut self, state: &State, ty: &TagType, index: bool) -> Result<()> {
        self.start_group("tag ");
        if index {
            write!(self.result, "(;{};) ", state.core.tags)?;
        }
        self.print_core_functype_idx(state, ty.func_type_idx, None)?;
        Ok(())
    }

    fn print_limits<T>(&mut self, initial: T, maximum: Option<T>) -> Result<()>
    where
        T: fmt::Display,
    {
        write!(self.result, "{}", initial)?;
        if let Some(max) = maximum {
            write!(self.result, " {}", max)?;
        }
        Ok(())
    }

    fn print_global_type(&mut self, state: &State, ty: &GlobalType, index: bool) -> Result<()> {
        self.start_group("global ");
        if index {
            self.print_name(&state.core.global_names, state.core.globals)?;
            self.result.push(' ');
        }
        if ty.mutable {
            self.result.push_str("(mut ");
            self.print_valtype(ty.content_type)?;
            self.result.push(')');
        } else {
            self.print_valtype(ty.content_type)?;
        }
        Ok(())
    }

    fn print_tables(&mut self, state: &mut State, parser: TableSectionReader<'_>) -> Result<()> {
        for table in parser.into_iter_with_offsets() {
            let (offset, table) = table?;
            self.newline(offset);
            self.print_table_type(state, &table, true)?;
            self.end_group();
            state.core.tables += 1;
        }
        Ok(())
    }

    fn print_memories(&mut self, state: &mut State, parser: MemorySectionReader<'_>) -> Result<()> {
        for memory in parser.into_iter_with_offsets() {
            let (offset, memory) = memory?;
            self.newline(offset);
            self.print_memory_type(state, &memory, true)?;
            self.end_group();
            state.core.memories += 1;
        }
        Ok(())
    }

    fn print_tags(&mut self, state: &mut State, parser: TagSectionReader<'_>) -> Result<()> {
        for tag in parser.into_iter_with_offsets() {
            let (offset, tag) = tag?;
            self.newline(offset);
            self.print_tag_type(state, &tag, true)?;
            self.end_group();
            state.core.tags += 1;
        }
        Ok(())
    }

    fn print_globals(&mut self, state: &mut State, parser: GlobalSectionReader<'_>) -> Result<()> {
        for global in parser.into_iter_with_offsets() {
            let (offset, global) = global?;
            self.newline(offset);
            self.print_global_type(state, &global.ty, true)?;
            self.result.push(' ');
            self.print_const_expr(state, &global.init_expr)?;
            self.end_group();
            state.core.globals += 1;
        }
        Ok(())
    }

    fn print_code(
        &mut self,
        state: &mut State,
        code: &[FunctionBody<'_>],
        funcs: FunctionSectionReader<'_>,
    ) -> Result<()> {
        if funcs.count() != code.len() as u32 {
            bail!("mismatch in function and code section counts");
        }
        for (body, ty) in code.iter().zip(funcs) {
            let mut body = body.get_binary_reader();
            let offset = body.original_position();
            let ty = ty?;
            self.newline(offset);
            self.start_group("func ");
            let func_idx = state.core.funcs;
            self.print_name(&state.core.func_names, func_idx)?;
            self.result.push(' ');
            let params = self
                .print_core_functype_idx(state, ty, Some(func_idx))?
                .unwrap_or(0);

            let mut first = true;
            let mut local_idx = 0;
            let mut locals = NamedLocalPrinter::new("local");
            for _ in 0..body.read_var_u32()? {
                let offset = body.original_position();
                let cnt = body.read_var_u32()?;
                let ty = body.read()?;
                if MAX_LOCALS
                    .checked_sub(local_idx)
                    .and_then(|s| s.checked_sub(cnt))
                    .is_none()
                {
                    bail!("function exceeds the maximum number of locals that can be printed");
                }
                for _ in 0..cnt {
                    if first {
                        self.newline(offset);
                        first = false;
                    }
                    let name = state.core.local_names.get(&(func_idx, params + local_idx));
                    locals.start_local(name, &mut self.result);
                    self.print_valtype(ty)?;
                    locals.end_local(&mut self.result);
                    local_idx += 1;
                }
            }
            locals.finish(&mut self.result);

            state.core.labels = 0;
            let nesting_start = self.nesting;
            body.allow_memarg64(true);

            let mut buf = String::new();
            let mut op_printer = operator::PrintOperator::new(self, state);
            while !body.eof() {
                // TODO
                let offset = body.original_position();
                mem::swap(&mut buf, &mut op_printer.printer.result);
                let op_kind = body.visit_operator(&mut op_printer)??;
                mem::swap(&mut buf, &mut op_printer.printer.result);

                match op_kind {
                    // The final `end` in a reader is not printed, it's implied
                    // in the text format.
                    operator::OpKind::End if body.eof() => break,

                    // When we start a block we newline to the current
                    // indentation, then we increase the indentation so further
                    // instructions are tabbed over.
                    operator::OpKind::BlockStart => {
                        op_printer.printer.newline(offset);
                        op_printer.printer.nesting += 1;
                    }

                    // `else`/`catch` are special in that it's printed at
                    // the previous indentation, but it doesn't actually change
                    // our nesting level.
                    operator::OpKind::BlockMid => {
                        op_printer.printer.nesting -= 1;
                        op_printer.printer.newline(offset);
                        op_printer.printer.nesting += 1;
                    }

                    // Exiting a block prints `end` at the previous indentation
                    // level. `delegate` also ends a block like `end` for `try`.
                    operator::OpKind::End | operator::OpKind::Delegate
                        if op_printer.printer.nesting > nesting_start =>
                    {
                        op_printer.printer.nesting -= 1;
                        op_printer.printer.newline(offset);
                    }

                    // .. otherwise everything else just has a normal newline
                    // out in front.
                    _ => op_printer.printer.newline(offset),
                }
                op_printer.printer.result.push_str(&buf);
                buf.truncate(0);
            }

            // If this was an invalid function body then the nesting may not
            // have reset back to normal. Fix that up here and forcibly insert
            // a newline as well in case the last instruction was something
            // like an `if` which has a comment after it which could interfere
            // with the closing paren printed for the func.
            if self.nesting != nesting_start {
                self.nesting = nesting_start;
                self.newline(body.original_position());
            }

            self.end_group();

            state.core.funcs += 1;
        }
        Ok(())
    }

    fn newline(&mut self, offset: usize) {
        self.print_newline(Some(offset))
    }

    fn newline_unknown_pos(&mut self) {
        self.print_newline(None)
    }

    fn print_newline(&mut self, offset: Option<usize>) {
        self.result.push('\n');
        if self.print_offsets {
            match offset {
                Some(offset) => write!(self.result, "(;@{offset:<6x};)").unwrap(),
                None => self.result.push_str("           "),
            }
        }
        self.line += 1;

        // Clamp the maximum nesting size that we print at something somewhat
        // reasonable to avoid generating hundreds of megabytes of whitespace
        // for small-ish modules that have deep-ish nesting.
        for _ in 0..self.nesting.min(MAX_NESTING_TO_PRINT) {
            self.result.push_str("  ");
        }
    }

    fn print_exports(&mut self, state: &State, data: ExportSectionReader) -> Result<()> {
        for export in data.into_iter_with_offsets() {
            let (offset, export) = export?;
            self.newline(offset);
            self.print_export(state, &export)?;
        }
        Ok(())
    }

    fn print_export(&mut self, state: &State, export: &Export) -> Result<()> {
        self.start_group("export ");
        self.print_str(export.name)?;
        self.result.push(' ');
        self.print_external_kind(state, export.kind, export.index)?;
        self.end_group(); // export
        Ok(())
    }

    fn print_external_kind(&mut self, state: &State, kind: ExternalKind, index: u32) -> Result<()> {
        self.result.push('(');
        match kind {
            ExternalKind::Func => {
                self.result.push_str("func ");
                self.print_idx(&state.core.func_names, index)?;
            }
            ExternalKind::Table => {
                self.result.push_str("table ");
                self.print_idx(&state.core.table_names, index)?;
            }
            ExternalKind::Global => {
                self.result.push_str("global ");
                self.print_idx(&state.core.global_names, index)?;
            }
            ExternalKind::Memory => {
                self.result.push_str("memory ");
                self.print_idx(&state.core.memory_names, index)?;
            }
            ExternalKind::Tag => write!(self.result, "tag {}", index)?,
        }
        self.result.push(')');
        Ok(())
    }

    fn print_type_ref(
        &mut self,
        state: &State,
        idx: u32,
        core: bool,
        bounds: Option<TypeBounds>,
    ) -> Result<()> {
        self.result.push_str("(type ");
        let closing = match bounds {
            Some(TypeBounds::Eq) => {
                self.result.push_str("(eq ");
                ")"
            }
            None => "",
        };
        self.print_idx(
            if core {
                &state.core.type_names
            } else {
                &state.component.type_names
            },
            idx,
        )?;
        self.result.push_str(closing);
        self.result.push(')');
        Ok(())
    }

    fn print_idx(&mut self, names: &HashMap<u32, Naming>, idx: u32) -> Result<()> {
        match names.get(&idx) {
            Some(name) => write!(self.result, "${}", name.identifier())?,
            None => write!(self.result, "{}", idx)?,
        }
        Ok(())
    }

    fn print_local_idx(&mut self, state: &State, func: u32, idx: u32) -> Result<()> {
        match state.core.local_names.get(&(func, idx)) {
            Some(name) => write!(self.result, "${}", name.identifier())?,
            None => write!(self.result, "{}", idx)?,
        }
        Ok(())
    }

    fn print_name(&mut self, names: &HashMap<u32, Naming>, cur_idx: u32) -> Result<()> {
        if let Some(name) = names.get(&cur_idx) {
            name.write(&mut self.result);
            self.result.push(' ');
        }
        write!(self.result, "(;{};)", cur_idx)?;
        Ok(())
    }

    fn print_elems(&mut self, state: &mut State, data: ElementSectionReader) -> Result<()> {
        for (i, elem) in data.into_iter_with_offsets().enumerate() {
            let (offset, mut elem) = elem?;
            self.newline(offset);
            self.start_group("elem ");
            self.print_name(&state.core.element_names, i as u32)?;
            match &mut elem.kind {
                ElementKind::Passive => {}
                ElementKind::Declared => write!(self.result, " declare")?,
                ElementKind::Active {
                    table_index,
                    offset_expr,
                } => {
                    if *table_index != 0 {
                        self.result.push_str(" (table ");
                        self.print_idx(&state.core.table_names, *table_index)?;
                        self.result.push(')');
                    }
                    self.result.push(' ');
                    self.print_const_expr_sugar(state, offset_expr, "offset")?;
                }
            }
            self.result.push(' ');
            match elem.items {
                ElementItems::Functions(reader) => {
                    self.print_reftype(elem.ty)?;
                    for idx in reader {
                        self.result.push(' ');
                        self.print_idx(&state.core.func_names, idx?)?
                    }
                }
                ElementItems::Expressions(reader) => {
                    self.print_valtype(elem.ty)?;
                    for expr in reader {
                        self.result.push(' ');
                        self.print_const_expr_sugar(state, &expr?, "item")?
                    }
                }
            }
            self.end_group();
        }
        Ok(())
    }

    fn print_data(&mut self, state: &mut State, data: DataSectionReader) -> Result<()> {
        for (i, data) in data.into_iter_with_offsets().enumerate() {
            let (offset, data) = data?;
            self.newline(offset);
            self.start_group("data ");
            self.print_name(&state.core.data_names, i as u32)?;
            self.result.push(' ');
            match &data.kind {
                DataKind::Passive => {}
                DataKind::Active {
                    memory_index,
                    offset_expr,
                } => {
                    if *memory_index != 0 {
                        self.result.push_str("(memory ");
                        self.print_idx(&state.core.memory_names, *memory_index)?;
                        self.result.push_str(") ");
                    }
                    self.print_const_expr_sugar(state, offset_expr, "offset")?;
                    self.result.push(' ');
                }
            }
            self.print_bytes(data.data)?;
            self.end_group();
        }
        Ok(())
    }

    /// Prints the operators of `expr` space-separated, taking into account that
    /// if there's only one operator in `expr` then instead of `(explicit ...)`
    /// the printing can be `(...)`.
    fn print_const_expr_sugar(
        &mut self,
        state: &mut State,
        expr: &ConstExpr,
        explicit: &str,
    ) -> Result<()> {
        self.start_group("");
        let mut prev = mem::take(&mut self.result);
        let mut reader = expr.get_operators_reader();
        let mut op_printer = operator::PrintOperator::new(self, state);
        let mut first_op = None;
        for i in 0.. {
            if reader.eof() {
                break;
            }
            match reader.visit_operator(&mut op_printer)?? {
                operator::OpKind::End if reader.eof() => {}

                _ if i == 0 => first_op = Some(mem::take(&mut op_printer.printer.result)),

                _ => {
                    if i == 1 {
                        prev.push_str(explicit);
                        prev.push(' ');
                        prev.push_str(&first_op.take().unwrap());
                    }
                    prev.push(' ');
                    prev.push_str(&op_printer.printer.result);
                }
            }

            op_printer.printer.result.truncate(0);
        }

        // If `first_op` is still set here then it means we don't need to print
        // an expression with `explicit` as the leading token, instead we can
        // print the single operator.
        if let Some(op) = first_op {
            prev.push_str(&op);
        }
        self.result = prev;
        self.end_group();
        Ok(())
    }

    /// Prints the operators of `expr` space-separated.
    fn print_const_expr(&mut self, state: &mut State, expr: &ConstExpr) -> Result<()> {
        let mut reader = expr.get_operators_reader();
        let mut first = true;

        let mut result = mem::take(&mut self.result);
        let mut op_printer = operator::PrintOperator::new(self, state);
        while !reader.eof() {
            if first {
                first = false;
            } else {
                op_printer.printer.result.push(' ');
            }
            match reader.visit_operator(&mut op_printer)?? {
                operator::OpKind::End if reader.eof() => {}
                _ => {
                    result.push_str(&op_printer.printer.result);
                    op_printer.printer.result.truncate(0);
                }
            }
        }
        self.result = result;
        Ok(())
    }

    fn print_primitive_val_type(&mut self, ty: &PrimitiveValType) {
        match ty {
            PrimitiveValType::Bool => self.result.push_str("bool"),
            PrimitiveValType::S8 => self.result.push_str("s8"),
            PrimitiveValType::U8 => self.result.push_str("u8"),
            PrimitiveValType::S16 => self.result.push_str("s16"),
            PrimitiveValType::U16 => self.result.push_str("u16"),
            PrimitiveValType::S32 => self.result.push_str("s32"),
            PrimitiveValType::U32 => self.result.push_str("u32"),
            PrimitiveValType::S64 => self.result.push_str("s64"),
            PrimitiveValType::U64 => self.result.push_str("u64"),
            PrimitiveValType::Float32 => self.result.push_str("float32"),
            PrimitiveValType::Float64 => self.result.push_str("float64"),
            PrimitiveValType::Char => self.result.push_str("char"),
            PrimitiveValType::String => self.result.push_str("string"),
        }
    }

    fn print_record_type(
        &mut self,
        state: &State,
        fields: &[(&str, ComponentValType)],
    ) -> Result<()> {
        self.start_group("record");
        for (name, ty) in fields.iter() {
            self.result.push(' ');
            self.start_group("field ");
            self.print_str(name)?;
            self.result.push(' ');
            self.print_component_val_type(state, ty)?;
            self.end_group()
        }
        self.end_group();
        Ok(())
    }

    fn print_variant_type(&mut self, state: &State, cases: &[VariantCase]) -> Result<()> {
        self.start_group("variant");
        for case in cases {
            self.result.push(' ');
            self.start_group("case ");
            self.print_str(case.name)?;

            if let Some(ty) = case.ty {
                self.result.push(' ');
                self.print_component_val_type(state, &ty)?;
            }

            if let Some(refines) = case.refines {
                self.result.push(' ');
                self.start_group("refines ");
                write!(&mut self.result, "{}", refines)?;
                self.end_group();
            }
            self.end_group()
        }
        self.end_group();

        Ok(())
    }

    fn print_list_type(&mut self, state: &State, element_ty: &ComponentValType) -> Result<()> {
        self.start_group("list ");
        self.print_component_val_type(state, element_ty)?;
        self.end_group();
        Ok(())
    }

    fn print_tuple_or_union_type(
        &mut self,
        ty: &str,
        state: &State,
        tys: &[ComponentValType],
    ) -> Result<()> {
        self.start_group(ty);
        for ty in tys {
            self.result.push(' ');
            self.print_component_val_type(state, ty)?;
        }
        self.end_group();
        Ok(())
    }

    fn print_flag_or_enum_type(&mut self, ty: &str, names: &[&str]) -> Result<()> {
        self.start_group(ty);
        for name in names {
            self.result.push(' ');
            self.print_str(name)?;
        }
        self.end_group();
        Ok(())
    }

    fn print_option_type(&mut self, state: &State, inner: &ComponentValType) -> Result<()> {
        self.start_group("option ");
        self.print_component_val_type(state, inner)?;
        self.end_group();
        Ok(())
    }

    fn print_result_type(
        &mut self,
        state: &State,
        ok: Option<ComponentValType>,
        err: Option<ComponentValType>,
    ) -> Result<()> {
        self.start_group("result");

        if let Some(ok) = ok {
            self.result.push(' ');
            self.print_component_val_type(state, &ok)?;
        }

        if let Some(err) = err {
            self.result.push(' ');
            self.start_group("error ");
            self.print_component_val_type(state, &err)?;
            self.end_group();
        }

        self.end_group();

        Ok(())
    }

    fn print_defined_type(&mut self, state: &State, ty: &ComponentDefinedType) -> Result<()> {
        match ty {
            ComponentDefinedType::Primitive(ty) => self.print_primitive_val_type(ty),
            ComponentDefinedType::Record(fields) => self.print_record_type(state, fields)?,
            ComponentDefinedType::Variant(cases) => self.print_variant_type(state, cases)?,
            ComponentDefinedType::List(ty) => self.print_list_type(state, ty)?,
            ComponentDefinedType::Tuple(tys) => {
                self.print_tuple_or_union_type("tuple", state, tys)?
            }
            ComponentDefinedType::Flags(names) => self.print_flag_or_enum_type("flags", names)?,
            ComponentDefinedType::Enum(cases) => self.print_flag_or_enum_type("enum", cases)?,
            ComponentDefinedType::Union(tys) => {
                self.print_tuple_or_union_type("union", state, tys)?
            }
            ComponentDefinedType::Option(ty) => self.print_option_type(state, ty)?,
            ComponentDefinedType::Result { ok, err } => self.print_result_type(state, *ok, *err)?,
        }

        Ok(())
    }

    fn print_component_val_type(&mut self, state: &State, ty: &ComponentValType) -> Result<()> {
        match ty {
            ComponentValType::Primitive(ty) => self.print_primitive_val_type(ty),
            ComponentValType::Type(idx) => {
                self.print_idx(&state.component.type_names, *idx)?;
            }
        }

        Ok(())
    }

    fn print_module_type(
        &mut self,
        states: &mut Vec<State>,
        decls: Vec<ModuleTypeDeclaration>,
    ) -> Result<()> {
        states.push(State::new(Encoding::Module));
        self.newline_unknown_pos();
        self.start_group("module");
        for decl in decls {
            self.newline_unknown_pos();
            match decl {
                ModuleTypeDeclaration::Type(ty) => {
                    self.print_type(states.last_mut().unwrap(), ty)?
                }
                ModuleTypeDeclaration::OuterAlias { kind, count, index } => {
                    self.print_outer_alias(states, kind, count, index)?;
                }
                ModuleTypeDeclaration::Export { name, ty } => {
                    self.start_group("export ");
                    self.print_str(name)?;
                    self.result.push(' ');
                    self.print_import_ty(states.last_mut().unwrap(), &ty, false)?;
                    self.end_group();
                }
                ModuleTypeDeclaration::Import(import) => {
                    self.print_import(states.last_mut().unwrap(), &import, false)?;
                }
            }
        }
        self.end_group();
        states.pop();
        Ok(())
    }

    fn print_component_type<'a>(
        &mut self,
        states: &mut Vec<State>,
        decls: Vec<ComponentTypeDeclaration<'a>>,
    ) -> Result<()> {
        states.push(State::new(Encoding::Component));
        self.newline_unknown_pos();
        self.start_group("component");
        for decl in decls {
            self.newline_unknown_pos();
            match decl {
                ComponentTypeDeclaration::CoreType(ty) => self.print_core_type(states, ty)?,
                ComponentTypeDeclaration::Type(ty) => self.print_component_type_def(states, ty)?,
                ComponentTypeDeclaration::Alias(alias) => {
                    self.print_component_alias(states, alias)?;
                }
                ComponentTypeDeclaration::Export { name, url, ty } => {
                    self.start_group("export ");
                    self.print_component_kind_name(states.last_mut().unwrap(), ty.kind())?;
                    self.print_str(name)?;
                    if !url.is_empty() {
                        self.result.push(' ');
                        self.print_str(url)?;
                    }
                    self.result.push(' ');
                    self.print_component_import_ty(states.last().unwrap(), &ty, false)?;
                    self.end_group();
                }
                ComponentTypeDeclaration::Import(import) => {
                    self.print_component_import(states.last_mut().unwrap(), &import, false)?
                }
            }
        }
        self.end_group();
        states.pop().unwrap();
        Ok(())
    }

    fn print_instance_type<'a>(
        &mut self,
        states: &mut Vec<State>,
        decls: Vec<InstanceTypeDeclaration<'a>>,
    ) -> Result<()> {
        states.push(State::new(Encoding::Component));
        self.newline_unknown_pos();
        self.start_group("instance");
        for decl in decls {
            self.newline_unknown_pos();
            match decl {
                InstanceTypeDeclaration::CoreType(ty) => self.print_core_type(states, ty)?,
                InstanceTypeDeclaration::Type(ty) => self.print_component_type_def(states, ty)?,
                InstanceTypeDeclaration::Alias(alias) => {
                    self.print_component_alias(states, alias)?;
                }
                InstanceTypeDeclaration::Export { name, url, ty } => {
                    self.start_group("export ");
                    self.print_component_kind_name(states.last_mut().unwrap(), ty.kind())?;
                    self.print_str(name)?;
                    if !url.is_empty() {
                        self.result.push(' ');
                        self.print_str(url)?;
                    }
                    self.result.push(' ');
                    self.print_component_import_ty(states.last().unwrap(), &ty, false)?;
                    self.end_group();
                }
            }
        }
        self.end_group();
        states.pop().unwrap();
        Ok(())
    }

    fn outer_state(states: &[State], count: u32) -> Result<&State> {
        let count = count as usize;
        if count >= states.len() {
            bail!("invalid outer alias count of {}", count);
        }

        Ok(&states[states.len() - count - 1])
    }

    fn print_outer_alias(
        &mut self,
        states: &mut [State],
        kind: OuterAliasKind,
        count: u32,
        index: u32,
    ) -> Result<()> {
        let state = states.last().unwrap();
        let outer = Self::outer_state(states, count)?;
        self.start_group("alias outer ");
        if let Some(name) = outer.name.as_ref() {
            name.write(&mut self.result);
        } else {
            self.result.push_str(count.to_string().as_str());
        }
        self.result.push(' ');
        match kind {
            OuterAliasKind::Type => {
                self.print_idx(&outer.core.type_names, index)?;
                self.result.push(' ');
                self.start_group("type ");
                self.print_name(&state.core.type_names, state.core.types.len() as u32)?;
            }
        }
        self.end_group(); // kind
        self.end_group(); // alias

        let state = states.last_mut().unwrap();
        match kind {
            OuterAliasKind::Type => state.core.types.push(None),
        }

        Ok(())
    }

    fn print_component_func_type(&mut self, state: &State, ty: &ComponentFuncType) -> Result<()> {
        self.start_group("func");
        for (name, ty) in ty.params.iter() {
            self.result.push(' ');
            self.start_group("param ");
            self.print_str(name)?;
            self.result.push(' ');
            self.print_component_val_type(state, ty)?;
            self.end_group()
        }

        for (name, ty) in ty.results.iter() {
            self.result.push(' ');
            self.start_group("result ");
            if let Some(name) = name {
                self.print_str(name)?;
                self.result.push(' ');
            }
            self.print_component_val_type(state, ty)?;
            self.end_group()
        }

        self.end_group();

        Ok(())
    }

    fn print_component_type_def<'a>(
        &mut self,
        states: &mut Vec<State>,
        ty: ComponentType<'a>,
    ) -> Result<()> {
        self.start_group("type ");
        {
            let state = states.last_mut().unwrap();
            self.print_name(&state.component.type_names, state.component.types)?;
        }
        match ty {
            ComponentType::Defined(ty) => {
                self.result.push(' ');
                self.print_defined_type(states.last_mut().unwrap(), &ty)?;
            }
            ComponentType::Func(ty) => {
                self.result.push(' ');
                self.print_component_func_type(states.last_mut().unwrap(), &ty)?;
            }
            ComponentType::Component(decls) => {
                self.print_component_type(states, decls.into_vec())?;
            }
            ComponentType::Instance(decls) => {
                self.print_instance_type(states, decls.into_vec())?;
            }
        }
        self.end_group();

        states.last_mut().unwrap().component.types += 1;

        Ok(())
    }

    fn print_component_types<'a>(
        &mut self,
        states: &mut Vec<State>,
        parser: ComponentTypeSectionReader<'a>,
    ) -> Result<()> {
        for ty in parser.into_iter_with_offsets() {
            let (offset, ty) = ty?;
            self.newline(offset);
            self.print_component_type_def(states, ty)?;
        }

        Ok(())
    }

    fn print_component_imports(
        &mut self,
        state: &mut State,
        parser: ComponentImportSectionReader,
    ) -> Result<()> {
        for import in parser.into_iter_with_offsets() {
            let (offset, import) = import?;
            self.newline(offset);
            self.print_component_import(state, &import, true)?;
            match import.ty {
                ComponentTypeRef::Module(_) => {
                    state.core.modules += 1;
                }
                ComponentTypeRef::Func(_) => {
                    state.component.funcs += 1;
                }
                ComponentTypeRef::Value(_) => {
                    state.component.values += 1;
                }
                ComponentTypeRef::Type(..) => {
                    state.component.types += 1;
                }
                ComponentTypeRef::Instance(_) => {
                    state.component.instances += 1;
                }
                ComponentTypeRef::Component(_) => {
                    state.component.components += 1;
                }
            }
        }

        Ok(())
    }

    fn print_component_import(
        &mut self,
        state: &mut State,
        import: &ComponentImport,
        index: bool,
    ) -> Result<()> {
        self.start_group("import ");
        self.print_str(import.name)?;
        if !import.url.is_empty() {
            self.result.push(' ');
            self.print_str(import.url)?;
        }
        self.result.push(' ');
        self.print_component_import_ty(state, &import.ty, index)?;
        self.end_group();
        Ok(())
    }

    fn print_component_import_ty(
        &mut self,
        state: &State,
        ty: &ComponentTypeRef,
        index: bool,
    ) -> Result<()> {
        match ty {
            ComponentTypeRef::Module(idx) => {
                self.start_group("core module ");
                if index {
                    self.print_name(&state.core.module_names, state.core.modules as u32)?;
                    self.result.push(' ');
                }
                self.print_type_ref(state, *idx, true, None)?;
                self.end_group();
            }
            ComponentTypeRef::Func(idx) => {
                self.start_group("func ");
                if index {
                    self.print_name(&state.component.func_names, state.component.funcs)?;
                    self.result.push(' ');
                }
                self.print_type_ref(state, *idx, false, None)?;
                self.end_group();
            }
            ComponentTypeRef::Value(ty) => {
                self.start_group("value ");
                if index {
                    self.print_name(&state.component.value_names, state.component.values)?;
                    self.result.push(' ');
                }
                match ty {
                    ComponentValType::Primitive(ty) => self.print_primitive_val_type(ty),
                    ComponentValType::Type(idx) => {
                        self.print_type_ref(state, *idx, false, None)?;
                    }
                }
                self.end_group();
            }
            ComponentTypeRef::Type(bounds, idx) => {
                self.print_type_ref(state, *idx, false, Some(*bounds))?;
            }
            ComponentTypeRef::Instance(idx) => {
                self.start_group("instance ");
                if index {
                    self.print_name(&state.component.instance_names, state.component.instances)?;
                    self.result.push(' ');
                }
                self.print_type_ref(state, *idx, false, None)?;
                self.end_group();
            }
            ComponentTypeRef::Component(idx) => {
                self.start_group("component ");
                if index {
                    self.print_name(&state.component.component_names, state.component.components)?;
                    self.result.push(' ');
                }
                self.print_type_ref(state, *idx, false, None)?;
                self.end_group();
            }
        }
        Ok(())
    }

    fn print_component_exports(
        &mut self,
        state: &mut State,
        parser: ComponentExportSectionReader,
    ) -> Result<()> {
        for export in parser.into_iter_with_offsets() {
            let (offset, export) = export?;
            self.newline(offset);
            self.print_component_export(state, &export, true)?;
        }
        Ok(())
    }

    fn print_component_export(
        &mut self,
        state: &mut State,
        export: &ComponentExport,
        named: bool,
    ) -> Result<()> {
        self.start_group("export ");
        if named {
            self.print_component_kind_name(state, export.kind)?;
        }
        self.print_str(export.name)?;
        if !export.url.is_empty() {
            self.result.push(' ');
            self.print_str(export.url)?;
        }
        self.result.push(' ');
        self.print_component_external_kind(state, export.kind, export.index)?;
        self.end_group();
        Ok(())
    }

    fn print_component_kind_name(
        &mut self,
        state: &mut State,
        kind: ComponentExternalKind,
    ) -> Result<()> {
        match kind {
            ComponentExternalKind::Func => {
                self.print_name(&state.component.func_names, state.component.funcs)?;
                state.component.funcs += 1;
            }
            ComponentExternalKind::Module => {
                self.print_name(&state.core.module_names, state.core.modules)?;
                state.core.modules += 1;
            }
            ComponentExternalKind::Value => {
                self.print_name(&state.component.value_names, state.component.values)?;
                state.component.values += 1;
            }
            ComponentExternalKind::Type => {
                self.print_name(&state.component.type_names, state.component.types)?;
                state.component.types += 1;
            }
            ComponentExternalKind::Instance => {
                self.print_name(&state.component.instance_names, state.component.instances)?;
                state.component.instances += 1;
            }
            ComponentExternalKind::Component => {
                self.print_name(&state.component.component_names, state.component.components)?;
                state.component.components += 1;
            }
        }
        self.result.push(' ');
        Ok(())
    }

    fn print_component_external_kind(
        &mut self,
        state: &State,
        kind: ComponentExternalKind,
        index: u32,
    ) -> Result<()> {
        match kind {
            ComponentExternalKind::Module => {
                self.start_group("core module ");
                self.print_idx(&state.core.module_names, index)?;
            }
            ComponentExternalKind::Component => {
                self.start_group("component ");
                self.print_idx(&state.component.component_names, index)?;
            }
            ComponentExternalKind::Instance => {
                self.start_group("instance ");
                self.print_idx(&state.component.instance_names, index)?;
            }
            ComponentExternalKind::Func => {
                self.start_group("func ");
                self.print_idx(&state.component.func_names, index)?;
            }
            ComponentExternalKind::Value => {
                self.start_group("value ");
                self.print_idx(&state.component.value_names, index)?;
            }
            ComponentExternalKind::Type => {
                self.start_group("type ");
                self.print_idx(&state.component.type_names, index)?;
            }
        }
        self.end_group();
        Ok(())
    }

    fn print_canonical_options(
        &mut self,
        state: &State,
        options: &[CanonicalOption],
    ) -> Result<()> {
        for option in options {
            self.result.push(' ');
            match option {
                CanonicalOption::UTF8 => self.result.push_str("string-encoding=utf8"),
                CanonicalOption::UTF16 => self.result.push_str("string-encoding=utf16"),
                CanonicalOption::CompactUTF16 => {
                    self.result.push_str("string-encoding=latin1+utf16")
                }
                CanonicalOption::Memory(idx) => {
                    self.start_group("memory ");
                    self.print_idx(&state.core.memory_names, *idx)?;
                    self.end_group();
                }
                CanonicalOption::Realloc(idx) => {
                    self.start_group("realloc ");
                    self.print_idx(&state.core.func_names, *idx)?;
                    self.end_group();
                }
                CanonicalOption::PostReturn(idx) => {
                    self.start_group("post-return ");
                    self.print_idx(&state.core.func_names, *idx)?;
                    self.end_group();
                }
            }
        }
        Ok(())
    }

    fn print_canonical_functions(
        &mut self,
        state: &mut State,
        parser: ComponentCanonicalSectionReader,
    ) -> Result<()> {
        for func in parser.into_iter_with_offsets() {
            let (offset, func) = func?;
            self.newline(offset);
            match func {
                CanonicalFunction::Lift {
                    core_func_index,
                    type_index,
                    options,
                } => {
                    self.start_group("func ");
                    self.print_name(&state.component.func_names, state.component.funcs)?;
                    self.result.push(' ');
                    self.start_group("type ");
                    self.print_idx(&state.component.type_names, type_index)?;
                    self.end_group();
                    self.result.push(' ');
                    self.start_group("canon lift ");
                    self.start_group("core func ");
                    self.print_idx(&state.core.func_names, core_func_index)?;
                    self.end_group();
                    self.print_canonical_options(state, &options)?;
                    self.end_group();
                    self.end_group();
                    state.component.funcs += 1;
                }
                CanonicalFunction::Lower {
                    func_index,
                    options,
                } => {
                    self.start_group("core func ");
                    self.print_name(&state.core.func_names, state.core.funcs)?;
                    self.result.push(' ');
                    self.start_group("canon lower ");
                    self.start_group("func ");
                    self.print_idx(&state.component.func_names, func_index)?;
                    self.end_group();
                    self.print_canonical_options(state, &options)?;
                    self.end_group();
                    self.end_group();
                    state.core.funcs += 1;
                }
            }
        }

        Ok(())
    }

    fn print_instances(&mut self, state: &mut State, parser: InstanceSectionReader) -> Result<()> {
        for instance in parser.into_iter_with_offsets() {
            let (offset, instance) = instance?;
            self.newline(offset);
            self.start_group("core instance ");
            self.print_name(&state.core.instance_names, state.core.instances)?;
            match instance {
                Instance::Instantiate { module_index, args } => {
                    self.result.push(' ');
                    self.start_group("instantiate ");
                    self.print_idx(&state.core.module_names, module_index)?;
                    for arg in args.iter() {
                        self.newline(offset);
                        self.print_instantiation_arg(state, arg)?;
                    }
                    self.end_group();
                    state.core.instances += 1;
                }
                Instance::FromExports(exports) => {
                    for export in exports.iter() {
                        self.newline(offset);
                        self.print_export(state, export)?;
                    }
                    state.core.instances += 1;
                }
            }
            self.end_group();
        }
        Ok(())
    }

    fn print_component_instances(
        &mut self,
        state: &mut State,
        parser: ComponentInstanceSectionReader,
    ) -> Result<()> {
        for instance in parser.into_iter_with_offsets() {
            let (offset, instance) = instance?;
            self.newline(offset);
            self.start_group("instance ");
            self.print_name(&state.component.instance_names, state.component.instances)?;
            state.component.instances += 1;
            match instance {
                ComponentInstance::Instantiate {
                    component_index,
                    args,
                } => {
                    self.result.push(' ');
                    self.start_group("instantiate ");
                    self.print_idx(&state.component.component_names, component_index)?;
                    for arg in args.iter() {
                        self.newline(offset);
                        self.print_component_instantiation_arg(state, arg)?;
                    }
                    self.end_group();
                }
                ComponentInstance::FromExports(exports) => {
                    for export in exports.iter() {
                        self.newline(offset);
                        self.print_component_export(state, export, false)?;
                    }
                }
            }
            self.end_group();
        }
        Ok(())
    }

    fn print_instantiation_arg(&mut self, state: &State, arg: &InstantiationArg) -> Result<()> {
        self.start_group("with ");
        self.print_str(arg.name)?;
        self.result.push(' ');
        match arg.kind {
            InstantiationArgKind::Instance => {
                self.start_group("instance ");
                self.print_idx(&state.core.instance_names, arg.index)?;
                self.end_group();
            }
        }
        self.end_group();
        Ok(())
    }

    fn print_component_instantiation_arg(
        &mut self,
        state: &State,
        arg: &ComponentInstantiationArg,
    ) -> Result<()> {
        self.start_group("with ");
        self.print_str(arg.name)?;
        self.result.push(' ');
        self.print_component_external_kind(state, arg.kind, arg.index)?;
        self.end_group();
        Ok(())
    }

    fn print_component_start(
        &mut self,
        state: &mut State,
        pos: usize,
        start: ComponentStartFunction,
    ) -> Result<()> {
        self.newline(pos);
        self.start_group("start ");
        self.print_idx(&state.component.func_names, start.func_index)?;

        for arg in start.arguments.iter() {
            self.result.push(' ');
            self.start_group("value ");
            self.print_idx(&state.component.value_names, *arg)?;
            self.end_group();
        }

        for _ in 0..start.results {
            self.result.push(' ');
            self.start_group("result ");
            self.start_group("value ");
            self.print_name(&state.component.value_names, state.component.values)?;
            self.end_group();
            self.end_group();
            state.component.values += 1;
        }

        self.end_group(); // start

        Ok(())
    }

    fn print_component_aliases(
        &mut self,
        states: &mut [State],
        parser: ComponentAliasSectionReader,
    ) -> Result<()> {
        for alias in parser.into_iter_with_offsets() {
            let (offset, alias) = alias?;
            self.newline(offset);
            self.print_component_alias(states, alias)?;
        }
        Ok(())
    }

    fn print_component_alias(
        &mut self,
        states: &mut [State],
        alias: ComponentAlias<'_>,
    ) -> Result<()> {
        match alias {
            ComponentAlias::InstanceExport {
                kind,
                instance_index,
                name,
            } => {
                let state = states.last_mut().unwrap();
                self.start_group("alias export ");
                self.print_idx(&state.component.instance_names, instance_index)?;
                self.result.push(' ');
                self.print_str(name)?;
                self.result.push(' ');
                match kind {
                    ComponentExternalKind::Module => {
                        self.start_group("core module ");
                        self.print_name(&state.core.module_names, state.core.modules)?;
                        self.end_group();
                        state.core.modules += 1;
                    }
                    ComponentExternalKind::Component => {
                        self.start_group("component ");
                        self.print_name(
                            &state.component.component_names,
                            state.component.components,
                        )?;
                        self.end_group();
                        state.component.components += 1;
                    }
                    ComponentExternalKind::Instance => {
                        self.start_group("instance ");
                        self.print_name(
                            &state.component.instance_names,
                            state.component.instances,
                        )?;
                        self.end_group();
                        state.component.instances += 1;
                    }
                    ComponentExternalKind::Func => {
                        self.start_group("func ");
                        self.print_name(&state.component.func_names, state.component.funcs)?;
                        self.end_group();
                        state.component.funcs += 1;
                    }
                    ComponentExternalKind::Value => {
                        self.start_group("value ");
                        self.print_name(&state.component.value_names, state.component.values)?;
                        self.end_group();
                        state.component.values += 1;
                    }
                    ComponentExternalKind::Type => {
                        self.start_group("type ");
                        self.print_name(&state.component.type_names, state.component.types)?;
                        self.end_group();
                        state.component.types += 1;
                    }
                }

                self.end_group(); // alias export
            }
            ComponentAlias::CoreInstanceExport {
                instance_index,
                kind,
                name,
            } => {
                let state = states.last_mut().unwrap();
                self.start_group("alias core export ");
                self.print_idx(&state.core.instance_names, instance_index)?;
                self.result.push(' ');
                self.print_str(name)?;
                self.result.push(' ');
                match kind {
                    ExternalKind::Func => {
                        self.start_group("core func ");
                        self.print_name(&state.core.func_names, state.core.funcs)?;
                        self.end_group();
                        state.core.funcs += 1;
                    }
                    ExternalKind::Table => {
                        self.start_group("core table ");
                        self.print_name(&state.core.table_names, state.core.tables)?;
                        self.end_group();
                        state.core.tables += 1;
                    }
                    ExternalKind::Memory => {
                        self.start_group("core memory ");
                        self.print_name(&state.core.memory_names, state.core.memories)?;
                        self.end_group();
                        state.core.memories += 1;
                    }
                    ExternalKind::Global => {
                        self.start_group("core global ");
                        self.print_name(&state.core.global_names, state.core.globals)?;
                        self.end_group();
                        state.core.globals += 1;
                    }
                    ExternalKind::Tag => {
                        self.start_group("core tag ");
                        write!(self.result, "(;{};)", state.core.tags)?;
                        self.end_group();
                        state.core.tags += 1;
                    }
                }
                self.end_group(); // alias export
            }

            ComponentAlias::Outer { kind, count, index } => {
                let state = states.last().unwrap();
                let outer = Self::outer_state(states, count)?;
                self.start_group("alias outer ");
                if let Some(name) = outer.name.as_ref() {
                    name.write(&mut self.result);
                } else {
                    self.result.push_str(count.to_string().as_str());
                }
                self.result.push(' ');
                match kind {
                    ComponentOuterAliasKind::CoreModule => {
                        self.print_idx(&outer.core.module_names, index)?;
                        self.result.push(' ');
                        self.start_group("core module ");
                        self.print_name(&state.core.module_names, state.core.modules)?;
                    }
                    ComponentOuterAliasKind::CoreType => {
                        self.print_idx(&outer.core.type_names, index)?;
                        self.result.push(' ');
                        self.start_group("core type ");
                        self.print_name(&state.core.type_names, state.core.types.len() as u32)?;
                    }
                    ComponentOuterAliasKind::Type => {
                        self.print_idx(&outer.component.type_names, index)?;
                        self.result.push(' ');
                        self.start_group("type ");
                        self.print_name(&state.component.type_names, state.component.types)?;
                    }
                    ComponentOuterAliasKind::Component => {
                        self.print_idx(&outer.component.component_names, index)?;
                        self.result.push(' ');
                        self.start_group("component ");
                        self.print_name(
                            &state.component.component_names,
                            state.component.components,
                        )?;
                    }
                }
                self.end_group(); // kind
                self.end_group(); // alias

                let state = states.last_mut().unwrap();
                match kind {
                    ComponentOuterAliasKind::CoreModule => state.core.modules += 1,
                    ComponentOuterAliasKind::CoreType => state.core.types.push(None),
                    ComponentOuterAliasKind::Type => state.component.types += 1,
                    ComponentOuterAliasKind::Component => state.component.components += 1,
                }
            }
        }
        Ok(())
    }

    fn print_str(&mut self, name: &str) -> Result<()> {
        let mut bytes = [0; 4];
        self.result.push('"');
        for c in name.chars() {
            let v = c as u32;
            if (0x20..0x7f).contains(&v) && c != '"' && c != '\\' && v < 0xff {
                self.result.push(c);
            } else {
                for byte in c.encode_utf8(&mut bytes).as_bytes() {
                    self.hex_byte(*byte);
                }
            }
        }
        self.result.push('"');
        Ok(())
    }

    fn print_bytes(&mut self, bytes: &[u8]) -> Result<()> {
        self.result.push('"');
        for byte in bytes {
            if *byte >= 0x20 && *byte < 0x7f && *byte != b'"' && *byte != b'\\' {
                self.result.push(*byte as char);
            } else {
                self.hex_byte(*byte);
            }
        }
        self.result.push('"');
        Ok(())
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
    first: bool,
}

impl NamedLocalPrinter {
    fn new(group_name: &'static str) -> NamedLocalPrinter {
        NamedLocalPrinter {
            group_name,
            in_group: false,
            end_group_after_local: false,
            first: true,
        }
    }

    fn start_local(&mut self, name: Option<&Naming>, dst: &mut String) {
        // Named locals must be in their own group, so if we have a name we need
        // to terminate the previous group.
        if name.is_some() && self.in_group {
            dst.push(')');
            self.in_group = false;
        }

        if self.first {
            self.first = false;
        } else {
            dst.push(' ');
        }

        // Next we either need a separator if we're already in a group or we
        // need to open a group for our new local.
        if !self.in_group {
            dst.push('(');
            dst.push_str(self.group_name);
            dst.push(' ');
            self.in_group = true;
        }

        // Print the optional name if given...
        if let Some(name) = name {
            name.write(dst);
            dst.push(' ');
        }
        self.end_group_after_local = name.is_some();
    }

    fn end_local(&mut self, dst: &mut String) {
        if self.end_group_after_local {
            dst.push(')');
            self.end_group_after_local = false;
            self.in_group = false;
        }
    }
    fn finish(self, dst: &mut String) {
        if self.in_group {
            dst.push(')');
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
                self.result.push('-');
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
                self.result.push('1');
                if fraction > 0 {
                    fraction <<= (int_width - mantissa_width);

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

                    self.result.push('.');
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

impl Naming {
    fn new<'a>(
        name: &'a str,
        index: u32,
        group: &str,
        used: Option<&mut HashSet<&'a str>>,
    ) -> Naming {
        let mut identifier = None;

        // If the `name` provided can't be used as the raw identifier for the
        // item that it's describing then a synthetic name must be made. The
        // rules here which generate a name are:
        //
        // * Empty identifiers are not allowed
        // * Identifiers have a fixed set of valid characters
        // * For wasmprinter's purposes we "reserve" identifiers with the `#`
        //   prefix, which is in theory rare to encounter in practice.
        // * If the name has already been used for some other item and cannot
        //   be reused (e.g. because shadowing in this context is not possible).
        //
        // If any of these conditions match then we generate a unique identifier
        // based on `name` but not it exactly. By factoring in the `group`,
        // `index`, and `name` we get a guaranteed unique identifier (due to the
        // leading `#` prefix that we reserve and factoring in of the item
        // index) while preserving human readability at least somewhat (the
        // valid identifier characters of `name` still appear in the returned
        // name).
        if name.is_empty()
            || name.chars().any(|c| !is_idchar(c))
            || name.starts_with('#')
            || used.map(|set| !set.insert(name)).unwrap_or(false)
        {
            let mut id = String::new();
            id.push('#');
            id.push_str(group);
            write!(id, "{}", index).unwrap();
            id.push('<');
            id.extend(name.chars().map(|c| if is_idchar(c) { c } else { '_' }));
            id.push('>');
            identifier = Some(id);
        }
        return Naming {
            identifier,
            name: name.to_string(),
        };

        // See https://webassembly.github.io/spec/core/text/values.html#text-id
        fn is_idchar(c: char) -> bool {
            matches!(
                c,
                '0'..='9'
                | 'a'..='z'
                | 'A'..='Z'
                | '!'
                | '#'
                | '$'
                | '%'
                | '&'
                | '\''
                | '*'
                | '+'
                | '-'
                | '.'
                | '/'
                | ':'
                | '<'
                | '='
                | '>'
                | '?'
                | '@'
                | '\\'
                | '^'
                | '_'
                | '`'
                | '|'
                | '~'
            )
        }
    }

    fn identifier(&self) -> &str {
        match &self.identifier {
            Some(s) => s,
            None => &self.name,
        }
    }

    fn write(&self, dst: &mut String) {
        match &self.identifier {
            Some(alternate) => {
                assert!(*alternate != self.name);
                dst.push('$');
                dst.push_str(alternate);
                dst.push_str(" (@name \"");
                // https://webassembly.github.io/spec/core/text/values.html#text-string
                for c in self.name.chars() {
                    match c {
                        '\t' => dst.push_str("\\t"),
                        '\n' => dst.push_str("\\n"),
                        '\r' => dst.push_str("\\r"),
                        '"' => dst.push_str("\\\""),
                        '\'' => dst.push_str("\\'"),
                        '\\' => dst.push_str("\\\\"),
                        c if (c as u32) < 0x20 || c as u32 == 0x7f => {
                            dst.push_str("\\u{");
                            write!(dst, "{:x}", c as u32).unwrap();
                            dst.push('}');
                        }
                        other => dst.push(other),
                    }
                }
                dst.push_str("\")");
            }
            None => {
                dst.push('$');
                dst.push_str(&self.name);
            }
        }
    }
}

fn name_map(into: &mut HashMap<u32, Naming>, names: NameMap<'_>, name: &str) -> Result<()> {
    let mut used = HashSet::new();
    for naming in names {
        let naming = naming?;
        into.insert(
            naming.index,
            Naming::new(naming.name, naming.index, name, Some(&mut used)),
        );
    }
    Ok(())
}
