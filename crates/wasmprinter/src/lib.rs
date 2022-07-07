//! A crate to convert a WebAssembly binary to its textual representation in the
//! WebAssembly Text Format (WAT).
//!
//! This crate is intended for developer toolchains and debugging, supporting
//! human-readable versions of a wasm binary. This can also be useful when
//! developing wasm toolchain support in Rust for various purposes like testing
//! and debugging and such.

#![deny(missing_docs)]

use anyhow::{anyhow, bail, Context, Result};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write};
use std::mem;
use std::path::Path;
use wasmparser::*;

const MAX_LOCALS: u32 = 50000;
const MAX_NESTING_TO_PRINT: u32 = 50;
const MAX_WASM_FUNCTIONS: u32 = 1_000_000;

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
    printers: HashMap<String, Box<dyn FnMut(&mut Printer, usize, &[u8]) -> Result<()>>>,
    result: String,
    nesting: u32,
    line: usize,
    group_lines: Vec<usize>,
}

enum Type<'a> {
    // A core function type.
    // The value is `None` for lowered component function types as they're not
    // needed currently.
    Func(Option<FuncType>),
    Module,
    // Stores a map between export name and index into the global type list.
    // The exports are a subset of those exports relevant to the printer's state.
    Component(HashMap<String, usize>),
    // Stores a map between export name and index into the global type list.
    // The exports are a subset of those exports relevant to the printer's state.
    Instance(HashMap<String, usize>),
    ComponentFunc(ComponentFuncType<'a>),
    // A component defined type
    Defined,
}

#[derive(Default)]
struct CoreState {
    types: Vec<usize>, // Stores indexes into global type list.
    funcs: Vec<usize>, // Stores indexes into global type list (function type).
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
    types: Vec<usize>,      // Stores indexes into global type list.
    funcs: Vec<usize>,      // Stores indexes into global type list (component function type).
    instances: Vec<usize>,  // Stores indexes into global type list (instance or component type).
    components: Vec<usize>, // Stores indexes into global type list (component type).
    values: u32,
    exports: HashMap<String, usize>,
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

    fn add_component_export(&mut self, name: &str, ty: usize) -> Result<()> {
        if self
            .component
            .exports
            .insert(name.to_string(), ty)
            .is_some()
        {
            bail!(
                "duplicate export name `{}` in export or type definition",
                name
            );
        }

        Ok(())
    }

    fn core_ty(&self, index: u32) -> Result<usize> {
        self.core
            .types
            .get(index as usize)
            .copied()
            .ok_or_else(|| anyhow!("core type index {} out of bounds", index))
    }

    fn ty(&self, index: u32) -> Result<usize> {
        self.component
            .types
            .get(index as usize)
            .copied()
            .ok_or_else(|| anyhow!("type index {} out of bounds", index))
    }

    fn func(&self, index: u32) -> Result<usize> {
        self.component
            .funcs
            .get(index as usize)
            .copied()
            .ok_or_else(|| anyhow!("function index {} out of bounds", index))
    }

    fn instance(&self, index: u32) -> Result<usize> {
        self.component
            .instances
            .get(index as usize)
            .copied()
            .ok_or_else(|| anyhow!("instance index {} out of bounds", index))
    }

    fn component(&self, index: u32) -> Result<usize> {
        self.component
            .components
            .get(index as usize)
            .copied()
            .ok_or_else(|| anyhow!("component index {} out of bounds", index))
    }

    fn component_instance_export(
        &self,
        types: &[Type],
        instance_index: u32,
        name: &str,
    ) -> Result<usize> {
        match &types[self.instance(instance_index)?] {
            Type::Component(exports) | Type::Instance(exports) => {
                exports.get(name).copied().ok_or_else(|| {
                    anyhow!(
                        "no export named `{}` for instance with index {}",
                        name,
                        instance_index
                    )
                })
            }
            _ => unreachable!(),
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
                    if s.get_count() > MAX_WASM_FUNCTIONS {
                        bail!(
                            "module contains {} functions which exceeds the limit of {}",
                            s.get_count(),
                            MAX_WASM_FUNCTIONS
                        );
                    }
                    code.reserve(s.get_count() as usize);
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
                Payload::CustomSection(c) if c.name() == "name" => {
                    let reader = NameSectionReader::new(c.data(), c.data_offset())?;

                    // Ignore any error associated with the name section.
                    drop(self.register_names(state, reader));
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
        let mut types = Vec::new();
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
                                    parent.component.components.len() as u32,
                                )?;
                            }
                        }
                    }

                    let state = states.last_mut().unwrap();

                    // First up try to find the `name` subsection which we'll use to print
                    // pretty names everywhere. Also look for the `code` section so we can
                    // print out functions as soon as we hit the function section.
                    code.clear();
                    code_printed = false;
                    self.read_names_and_code(bytes, parser.clone(), state, &mut code)?;

                    if let Some(name) = state.name.as_ref() {
                        self.result.push(' ');
                        name.write(&mut self.result);
                    }
                }
                Payload::CustomSection(c) => {
                    let mut printers = mem::take(&mut self.printers);
                    if let Some(printer) = printers.get_mut(c.name()) {
                        printer(self, c.data_offset(), c.data())?;
                    }
                    self.printers = printers;
                }
                Payload::TypeSection(s) => {
                    self.print_types(states.last_mut().unwrap(), &mut types, s)?
                }
                Payload::ImportSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_imports(states.last_mut().unwrap(), &types, s)?
                }
                Payload::FunctionSection(reader) => {
                    Self::ensure_module(&states)?;
                    if mem::replace(&mut code_printed, true) {
                        bail!("function section appeared twice in module");
                    }
                    if reader.get_count() == 0 {
                        continue;
                    }
                    self.print_code(states.last_mut().unwrap(), &types, &code, reader)?;
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
                    self.print_tags(states.last_mut().unwrap(), &types, s)?
                }
                Payload::GlobalSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_globals(states.last_mut().unwrap(), &types, s)?
                }
                Payload::ExportSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_exports(states.last().unwrap(), s)?
                }
                Payload::StartSection { func, .. } => {
                    Self::ensure_module(&states)?;
                    self.newline();
                    self.start_group("start ");
                    self.print_idx(&states.last().unwrap().core.func_names, func)?;
                    self.end_group();
                }
                Payload::ElementSection(s) => {
                    Self::ensure_module(&states)?;
                    self.print_elems(states.last_mut().unwrap(), &types, s)?;
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
                    self.print_data(states.last_mut().unwrap(), &types, s)?;
                }

                Payload::ModuleSection { parser: inner, .. } => {
                    Self::ensure_component(&states)?;
                    expected = Some(Encoding::Module);
                    parsers.push(parser);
                    parser = inner;
                    self.newline();
                }
                Payload::InstanceSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_instances(states.last_mut().unwrap(), s)?;
                }
                Payload::AliasSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_aliases(&mut states, &mut types, s)?;
                }
                Payload::CoreTypeSection(s) => self.print_core_types(&mut states, &mut types, s)?,
                Payload::ComponentSection { parser: inner, .. } => {
                    Self::ensure_component(&states)?;
                    expected = Some(Encoding::Component);
                    parsers.push(parser);
                    parser = inner;
                    self.newline();
                }
                Payload::ComponentInstanceSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_instances(states.last_mut().unwrap(), &mut types, s)?;
                }
                Payload::ComponentAliasSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_aliases(&mut states, &types, s)?;
                }
                Payload::ComponentTypeSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_types(&mut states, &mut types, s)?;
                }
                Payload::ComponentCanonicalSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_canonical_functions(states.last_mut().unwrap(), &mut types, s)?;
                }
                Payload::ComponentStartSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_start(states.last_mut().unwrap(), &types, s)?;
                }
                Payload::ComponentImportSection(s) => {
                    Self::ensure_component(&states)?;
                    self.print_component_imports(states.last_mut().unwrap(), &types, s)?;
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
                                parent.component.components.push(types.len());
                                types.push(Type::Component(state.component.exports));
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
                self.newline();
            }
        }
        self.result.push(')');
    }

    fn register_names(&mut self, state: &mut State, names: NameSectionReader<'_>) -> Result<()> {
        fn name_map(into: &mut HashMap<u32, Naming>, names: NameMap<'_>, name: &str) -> Result<()> {
            let mut used = HashSet::new();
            let mut map = names.get_map()?;
            for _ in 0..map.get_count() {
                let naming = map.read()?;
                into.insert(
                    naming.index,
                    Naming::new(naming.name, naming.index, name, &mut used),
                );
            }
            Ok(())
        }

        fn indirect_name_map(
            into: &mut HashMap<(u32, u32), Naming>,
            names: IndirectNameMap<'_>,
            name: &str,
        ) -> Result<()> {
            let mut outer_map = names.get_indirect_map()?;
            for _ in 0..outer_map.get_indirect_count() {
                let mut used = HashSet::new();
                let outer = outer_map.read()?;
                let mut inner_map = outer.get_map()?;
                for _ in 0..inner_map.get_count() {
                    let inner = inner_map.read()?;
                    into.insert(
                        (outer.indirect_index, inner.index),
                        Naming::new(inner.name, inner.index, name, &mut used),
                    );
                }
            }
            Ok(())
        }

        for section in names {
            match section? {
                Name::Module(n) => {
                    let name = Naming::new(n.get_name()?, 0, "module", &mut HashSet::new());
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

    fn print_core_type(
        &mut self,
        states: &mut Vec<State>,
        types: &mut Vec<Type>,
        ty: wasmparser::CoreType,
    ) -> Result<()> {
        self.start_group("core type ");
        self.print_name(
            &states.last().unwrap().core.type_names,
            states.last().unwrap().core.types.len() as u32,
        )?;
        self.result.push(' ');
        let ty = match ty {
            wasmparser::CoreType::Func(ty) => {
                self.start_group("func");
                self.print_func_type(states.last().unwrap(), &ty, None)?;
                self.end_group();
                Type::Func(Some(ty))
            }
            wasmparser::CoreType::Module(decls) => {
                self.print_module_type(states, types, decls.into_vec())?;
                Type::Module
            }
        };
        self.end_group(); // `core type` itself

        states.last_mut().unwrap().core.types.push(types.len());
        types.push(ty);
        Ok(())
    }

    fn print_type(
        &mut self,
        state: &mut State,
        types: &mut Vec<Type>,
        ty: wasmparser::Type,
    ) -> Result<()> {
        self.start_group("type ");
        self.print_name(&state.core.type_names, state.core.types.len() as u32)?;
        self.result.push(' ');
        let ty = match ty {
            wasmparser::Type::Func(ty) => {
                self.start_group("func");
                self.print_func_type(state, &ty, None)?;
                self.end_group();
                Type::Func(Some(ty))
            }
        };
        self.end_group(); // `type` itself

        state.core.types.push(types.len());
        types.push(ty);
        Ok(())
    }

    fn print_core_types(
        &mut self,
        states: &mut Vec<State>,
        types: &mut Vec<Type>,
        parser: CoreTypeSectionReader<'_>,
    ) -> Result<()> {
        for ty in parser {
            self.newline();
            self.print_core_type(states, types, ty?)?;
        }

        Ok(())
    }

    fn print_types(
        &mut self,
        state: &mut State,
        types: &mut Vec<Type>,
        parser: TypeSectionReader<'_>,
    ) -> Result<()> {
        for ty in parser {
            self.newline();
            self.print_type(state, types, ty?)?;
        }

        Ok(())
    }

    fn print_core_functype_idx(
        &mut self,
        state: &State,
        types: &[Type],
        idx: u32,
        always_print_type: bool,
        names_for: Option<u32>,
    ) -> Result<Option<u32>> {
        if always_print_type {
            self.print_type_ref(state, idx, true, None)?;
        }

        match &types[state.core_ty(idx)?] {
            Type::Func(Some(ty)) => self.print_func_type(state, ty, names_for).map(Some),
            _ => unreachable!(),
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
        if ty.params.len() > 0 {
            self.result.push(' ');
        }

        let mut params = NamedLocalPrinter::new("param");
        // Note that named parameters must be alone in a `param` block, so
        // we need to be careful to terminate previous param blocks and open
        // a new one if that's the case with a named parameter.
        for (i, param) in ty.params.iter().enumerate() {
            let name = names_for.and_then(|n| state.core.local_names.get(&(n, i as u32)));
            params.start_local(name, &mut self.result);
            self.print_valtype(*param)?;
            params.end_local(&mut self.result);
        }
        params.finish(&mut self.result);
        if ty.returns.len() > 0 {
            self.result.push_str(" (result");
            for result in ty.returns.iter() {
                self.result.push(' ');
                self.print_valtype(*result)?;
            }
            self.result.push(')');
        }
        Ok(ty.params.len() as u32)
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

    fn print_imports(
        &mut self,
        state: &mut State,
        types: &[Type],
        parser: ImportSectionReader<'_>,
    ) -> Result<()> {
        for import in parser {
            let import = import?;
            self.newline();
            self.print_import(state, types, &import, true)?;
            match import.ty {
                TypeRef::Func(index) => {
                    let ty = state.core_ty(index)?;
                    match &types[ty] {
                        Type::Func(_) => {
                            state.core.funcs.push(ty);
                        }
                        _ => bail!("index is not a function type"),
                    }
                }
                TypeRef::Table(_) => state.core.tables += 1,
                TypeRef::Memory(_) => state.core.memories += 1,
                TypeRef::Tag(_) => state.core.tags += 1,
                TypeRef::Global(_) => state.core.globals += 1,
            }
        }
        Ok(())
    }

    fn print_import(
        &mut self,
        state: &State,
        types: &[Type],
        import: &Import<'_>,
        index: bool,
    ) -> Result<()> {
        self.start_group("import ");
        self.print_str(import.module)?;
        self.result.push(' ');
        self.print_str(import.name)?;
        self.result.push(' ');
        self.print_import_ty(state, types, &import.ty, index)?;
        self.end_group();
        Ok(())
    }

    fn print_import_ty(
        &mut self,
        state: &State,
        types: &[Type],
        ty: &TypeRef,
        index: bool,
    ) -> Result<()> {
        match ty {
            TypeRef::Func(f) => {
                self.start_group("func");
                if index {
                    self.result.push(' ');
                    self.print_name(&state.core.func_names, state.core.funcs.len() as u32)?;
                }
                self.print_type_ref(state, *f, true, None)?;
            }
            TypeRef::Table(f) => self.print_table_type(state, f, index)?,
            TypeRef::Memory(f) => self.print_memory_type(state, f, index)?,
            TypeRef::Tag(f) => self.print_tag_type(state, types, f, index)?,
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

    fn print_tag_type(
        &mut self,
        state: &State,
        types: &[Type],
        ty: &TagType,
        index: bool,
    ) -> Result<()> {
        self.start_group("tag ");
        if index {
            write!(self.result, "(;{};)", state.core.tags)?;
        }
        self.print_core_functype_idx(state, types, ty.func_type_idx, true, None)?;
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
        for table in parser {
            let table = table?;
            self.newline();
            self.print_table_type(state, &table, true)?;
            self.end_group();
            state.core.tables += 1;
        }
        Ok(())
    }

    fn print_memories(&mut self, state: &mut State, parser: MemorySectionReader<'_>) -> Result<()> {
        for memory in parser {
            let memory = memory?;
            self.newline();
            self.print_memory_type(state, &memory, true)?;
            self.end_group();
            state.core.memories += 1;
        }
        Ok(())
    }

    fn print_tags(
        &mut self,
        state: &mut State,
        types: &[Type],
        parser: TagSectionReader<'_>,
    ) -> Result<()> {
        for tag in parser {
            let tag = tag?;
            self.newline();
            self.print_tag_type(state, types, &tag, true)?;
            self.end_group();
            state.core.tags += 1;
        }
        Ok(())
    }

    fn print_globals(
        &mut self,
        state: &mut State,
        types: &[Type],
        parser: GlobalSectionReader<'_>,
    ) -> Result<()> {
        for global in parser {
            let global = global?;
            self.newline();
            self.print_global_type(state, &global.ty, true)?;
            self.result.push(' ');
            self.print_init_expr(state, types, &global.init_expr)?;
            self.end_group();
            state.core.globals += 1;
        }
        Ok(())
    }

    fn print_code(
        &mut self,
        state: &mut State,
        types: &[Type],
        code: &[FunctionBody<'_>],
        mut funcs: FunctionSectionReader<'_>,
    ) -> Result<()> {
        if funcs.get_count() != code.len() as u32 {
            bail!("mismatch in function and code section counts");
        }
        for body in code {
            let ty = funcs.read()?;
            self.newline();
            self.start_group("func ");
            let func_idx = state.core.funcs.len() as u32;
            self.print_name(&state.core.func_names, func_idx)?;
            let params = self
                .print_core_functype_idx(state, types, ty, true, Some(func_idx))?
                .unwrap_or(0);

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
                        self.newline();
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
            let mut reader = body.get_operators_reader()?;
            reader.allow_memarg64(true);
            while !reader.eof() {
                let operator = reader.read()?;
                match operator {
                    // The final `end` in a reader is not printed, it's implied
                    // in the text format.
                    Operator::End if reader.eof() => break,

                    // When we start a block we newline to the current
                    // indentation, then we increase the indentation so further
                    // instructions are tabbed over.
                    Operator::If { .. }
                    | Operator::Block { .. }
                    | Operator::Loop { .. }
                    | Operator::Try { .. } => {
                        self.newline();
                        self.nesting += 1;
                    }

                    // `else`/`catch` are special in that it's printed at
                    // the previous indentation, but it doesn't actually change
                    // our nesting level.
                    Operator::Else | Operator::Catch { .. } | Operator::CatchAll => {
                        self.nesting -= 1;
                        self.newline();
                        self.nesting += 1;
                    }

                    // Exiting a block prints `end` at the previous indentation
                    // level. `delegate` also ends a block like `end` for `try`.
                    Operator::End | Operator::Delegate { .. } if self.nesting > nesting_start => {
                        self.nesting -= 1;
                        self.newline();
                    }

                    // .. otherwise everything else just has a normal newline
                    // out in front.
                    _ => self.newline(),
                }
                self.print_operator(state, types, &operator, nesting_start)?;
            }

            // If this was an invalid function body then the nesting may not
            // have reset back to normal. Fix that up here and forcibly insert
            // a newline as well in case the last instruction was something
            // like an `if` which has a comment after it which could interfere
            // with the closing paren printed for the func.
            if self.nesting != nesting_start {
                self.nesting = nesting_start;
                self.newline();
            }

            self.end_group();

            state.core.funcs.push(state.core_ty(ty)?);
        }
        Ok(())
    }

    fn newline(&mut self) {
        self.result.push('\n');
        self.line += 1;

        // Clamp the maximum nesting size that we print at something somewhat
        // reasonable to avoid generating hundreds of megabytes of whitespace
        // for small-ish modules that have deep-ish nesting.
        for _ in 0..self.nesting.min(MAX_NESTING_TO_PRINT) {
            self.result.push_str("  ");
        }
    }

    fn print_operator(
        &mut self,
        state: &mut State,
        types: &[Type],
        op: &Operator<'_>,
        nesting_start: u32,
    ) -> Result<()> {
        use Operator::*;
        let cur_depth = self.nesting - nesting_start;
        let label = |relative: u32| match cur_depth.checked_sub(relative) {
            Some(i) => format!("@{}", i),
            None => " INVALID ".to_string(),
        };
        match op {
            Nop => self.result.push_str("nop"),
            Unreachable => self.result.push_str("unreachable"),
            Block { ty } => {
                self.result.push_str("block");
                self.print_blockty(state, types, ty, cur_depth)?;
            }
            Loop { ty } => {
                self.result.push_str("loop");
                self.print_blockty(state, types, ty, cur_depth)?;
            }
            If { ty } => {
                self.result.push_str("if");
                self.print_blockty(state, types, ty, cur_depth)?;
            }
            Else => self.result.push_str("else"),
            Try { ty } => {
                self.result.push_str("try");
                self.print_blockty(state, types, ty, cur_depth)?;
            }
            Catch { index } => {
                write!(self.result, "catch {}", index)?;
            }
            Throw { index } => {
                write!(self.result, "throw {}", index)?;
            }
            Rethrow { relative_depth } => {
                write!(
                    self.result,
                    "rethrow {} (;{};)",
                    relative_depth,
                    label(*relative_depth)
                )?;
            }
            End => self.result.push_str("end"),
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
                for item in table.targets().chain(Some(Ok(table.default()))) {
                    let item = item?;
                    write!(self.result, " {} (;{};)", item, label(item))?;
                }
            }

            Return => self.result.push_str("return"),
            Call { function_index } => {
                self.result.push_str("call ");
                self.print_idx(&state.core.func_names, *function_index)?;
            }
            CallIndirect {
                table_index,
                index,
                table_byte: _,
            } => {
                self.result.push_str("call_indirect");
                if *table_index != 0 {
                    self.result.push(' ');
                    self.print_idx(&state.core.table_names, *table_index)?;
                }
                self.print_type_ref(state, *index, true, None)?;
            }
            ReturnCall { function_index } => {
                self.result.push_str("return_call ");
                self.print_idx(&state.core.func_names, *function_index)?;
            }
            ReturnCallIndirect { table_index, index } => {
                self.result.push_str("return_call_indirect");
                if *table_index != 0 {
                    self.result.push(' ');
                    self.print_idx(&state.core.table_names, *table_index)?;
                }
                self.print_type_ref(state, *index, true, None)?;
            }

            Delegate { relative_depth } => {
                write!(self.result, "delegate {}", relative_depth)?;
            }
            CatchAll => self.result.push_str("catch_all"),

            Drop => self.result.push_str("drop"),
            Select => self.result.push_str("select"),
            TypedSelect { ty } => {
                self.result.push_str("select (result ");
                self.print_valtype(*ty)?;
                self.result.push(')');
            }
            LocalGet { local_index } => {
                self.result.push_str("local.get ");
                self.print_local_idx(state, state.core.funcs.len() as u32, *local_index)?;
            }
            LocalSet { local_index } => {
                self.result.push_str("local.set ");
                self.print_local_idx(state, state.core.funcs.len() as u32, *local_index)?;
            }
            LocalTee { local_index } => {
                self.result.push_str("local.tee ");
                self.print_local_idx(state, state.core.funcs.len() as u32, *local_index)?;
            }

            GlobalGet { global_index } => {
                self.result.push_str("global.get ");
                self.print_idx(&state.core.global_names, *global_index)?;
            }
            GlobalSet { global_index } => {
                self.result.push_str("global.set ");
                self.print_idx(&state.core.global_names, *global_index)?;
            }

            I32Load { memarg } => self.mem_instr(state, "i32.load", memarg, 4)?,
            I64Load { memarg } => self.mem_instr(state, "i64.load", memarg, 8)?,
            F32Load { memarg } => self.mem_instr(state, "f32.load", memarg, 4)?,
            F64Load { memarg } => self.mem_instr(state, "f64.load", memarg, 8)?,
            I32Load8S { memarg } => self.mem_instr(state, "i32.load8_s", memarg, 1)?,
            I32Load8U { memarg } => self.mem_instr(state, "i32.load8_u", memarg, 1)?,
            I32Load16S { memarg } => self.mem_instr(state, "i32.load16_s", memarg, 2)?,
            I32Load16U { memarg } => self.mem_instr(state, "i32.load16_u", memarg, 2)?,
            I64Load8S { memarg } => self.mem_instr(state, "i64.load8_s", memarg, 1)?,
            I64Load8U { memarg } => self.mem_instr(state, "i64.load8_u", memarg, 1)?,
            I64Load16S { memarg } => self.mem_instr(state, "i64.load16_s", memarg, 2)?,
            I64Load16U { memarg } => self.mem_instr(state, "i64.load16_u", memarg, 2)?,
            I64Load32S { memarg } => self.mem_instr(state, "i64.load32_s", memarg, 4)?,
            I64Load32U { memarg } => self.mem_instr(state, "i64.load32_u", memarg, 4)?,

            I32Store { memarg } => self.mem_instr(state, "i32.store", memarg, 4)?,
            I64Store { memarg } => self.mem_instr(state, "i64.store", memarg, 8)?,
            F32Store { memarg } => self.mem_instr(state, "f32.store", memarg, 4)?,
            F64Store { memarg } => self.mem_instr(state, "f64.store", memarg, 8)?,
            I32Store8 { memarg } => self.mem_instr(state, "i32.store8", memarg, 1)?,
            I32Store16 { memarg } => self.mem_instr(state, "i32.store16", memarg, 2)?,
            I64Store8 { memarg } => self.mem_instr(state, "i64.store8", memarg, 1)?,
            I64Store16 { memarg } => self.mem_instr(state, "i64.store16", memarg, 2)?,
            I64Store32 { memarg } => self.mem_instr(state, "i64.store32", memarg, 4)?,

            MemorySize { mem: 0, .. } => self.result.push_str("memory.size"),
            MemorySize { mem, .. } => {
                self.result.push_str("memory.size ");
                self.print_idx(&state.core.memory_names, *mem)?;
            }
            MemoryGrow { mem: 0, .. } => self.result.push_str("memory.grow"),
            MemoryGrow { mem, .. } => {
                self.result.push_str("memory.grow ");
                self.print_idx(&state.core.memory_names, *mem)?;
            }

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
                self.print_idx(&state.core.func_names, *function_index)?;
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

            MemoryInit { segment, mem } => {
                self.result.push_str("memory.init ");
                if *mem != 0 {
                    self.print_idx(&state.core.memory_names, *mem)?;
                    self.result.push(' ');
                }
                self.print_idx(&state.core.data_names, *segment)?;
            }
            DataDrop { segment } => {
                self.result.push_str("data.drop ");
                self.print_idx(&state.core.data_names, *segment)?;
            }
            MemoryCopy { src: 0, dst: 0 } => self.result.push_str("memory.copy"),
            MemoryCopy { src, dst } => {
                self.result.push_str("memory.copy ");
                self.print_idx(&state.core.memory_names, *dst)?;
                self.result.push(' ');
                self.print_idx(&state.core.memory_names, *src)?;
            }
            MemoryFill { mem: 0 } => self.result.push_str("memory.fill"),
            MemoryFill { mem } => {
                self.result.push_str("memory.fill ");
                self.print_idx(&state.core.memory_names, *mem)?;
            }

            TableInit { table, segment } => {
                self.result.push_str("table.init ");
                if *table != 0 {
                    self.print_idx(&state.core.table_names, *table)?;
                    self.result.push(' ');
                }
                self.print_idx(&state.core.element_names, *segment)?;
            }
            ElemDrop { segment } => {
                self.result.push_str("elem.drop ");
                self.print_idx(&state.core.element_names, *segment)?;
            }
            TableCopy {
                dst_table,
                src_table,
            } => {
                self.result.push_str("table.copy");
                if *dst_table != *src_table || *src_table != 0 {
                    self.result.push(' ');
                    self.print_idx(&state.core.table_names, *dst_table)?;
                    self.result.push(' ');
                    self.print_idx(&state.core.table_names, *src_table)?;
                }
            }
            TableGet { table } => {
                self.result.push_str("table.get ");
                self.print_idx(&state.core.table_names, *table)?;
            }
            TableSet { table } => {
                self.result.push_str("table.set ");
                self.print_idx(&state.core.table_names, *table)?;
            }
            TableGrow { table } => {
                self.result.push_str("table.grow ");
                self.print_idx(&state.core.table_names, *table)?;
            }
            TableSize { table } => {
                self.result.push_str("table.size ");
                self.print_idx(&state.core.table_names, *table)?;
            }
            TableFill { table } => {
                self.result.push_str("table.fill ");
                self.print_idx(&state.core.table_names, *table)?;
            }

            MemoryAtomicNotify { memarg } => {
                self.mem_instr(state, "memory.atomic.notify", memarg, 4)?
            }
            MemoryAtomicWait32 { memarg } => {
                self.mem_instr(state, "memory.atomic.wait32", memarg, 4)?
            }
            MemoryAtomicWait64 { memarg } => {
                self.mem_instr(state, "memory.atomic.wait64", memarg, 8)?
            }
            AtomicFence { flags: _ } => self.result.push_str("atomic.fence"),

            I32AtomicLoad { memarg } => self.mem_instr(state, "i32.atomic.load", memarg, 4)?,
            I64AtomicLoad { memarg } => self.mem_instr(state, "i64.atomic.load", memarg, 8)?,
            I32AtomicLoad8U { memarg } => self.mem_instr(state, "i32.atomic.load8_u", memarg, 1)?,
            I32AtomicLoad16U { memarg } => {
                self.mem_instr(state, "i32.atomic.load16_u", memarg, 2)?
            }
            I64AtomicLoad8U { memarg } => self.mem_instr(state, "i64.atomic.load8_u", memarg, 1)?,
            I64AtomicLoad16U { memarg } => {
                self.mem_instr(state, "i64.atomic.load16_u", memarg, 2)?
            }
            I64AtomicLoad32U { memarg } => {
                self.mem_instr(state, "i64.atomic.load32_u", memarg, 4)?
            }

            I32AtomicStore { memarg } => self.mem_instr(state, "i32.atomic.store", memarg, 4)?,
            I64AtomicStore { memarg } => self.mem_instr(state, "i64.atomic.store", memarg, 8)?,
            I32AtomicStore8 { memarg } => self.mem_instr(state, "i32.atomic.store8", memarg, 1)?,
            I32AtomicStore16 { memarg } => {
                self.mem_instr(state, "i32.atomic.store16", memarg, 2)?
            }
            I64AtomicStore8 { memarg } => self.mem_instr(state, "i64.atomic.store8", memarg, 1)?,
            I64AtomicStore16 { memarg } => {
                self.mem_instr(state, "i64.atomic.store16", memarg, 2)?
            }
            I64AtomicStore32 { memarg } => {
                self.mem_instr(state, "i64.atomic.store32", memarg, 4)?
            }

            I32AtomicRmwAdd { memarg } => self.mem_instr(state, "i32.atomic.rmw.add", memarg, 4)?,
            I64AtomicRmwAdd { memarg } => self.mem_instr(state, "i64.atomic.rmw.add", memarg, 8)?,
            I32AtomicRmw8AddU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw8.add_u", memarg, 1)?
            }
            I32AtomicRmw16AddU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw16.add_u", memarg, 2)?
            }
            I64AtomicRmw8AddU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw8.add_u", memarg, 1)?
            }
            I64AtomicRmw16AddU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw16.add_u", memarg, 2)?
            }
            I64AtomicRmw32AddU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw32.add_u", memarg, 4)?
            }

            I32AtomicRmwSub { memarg } => self.mem_instr(state, "i32.atomic.rmw.sub", memarg, 4)?,
            I64AtomicRmwSub { memarg } => self.mem_instr(state, "i64.atomic.rmw.sub", memarg, 8)?,
            I32AtomicRmw8SubU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw8.sub_u", memarg, 1)?
            }
            I32AtomicRmw16SubU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw16.sub_u", memarg, 2)?
            }
            I64AtomicRmw8SubU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw8.sub_u", memarg, 1)?
            }
            I64AtomicRmw16SubU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw16.sub_u", memarg, 2)?
            }
            I64AtomicRmw32SubU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw32.sub_u", memarg, 4)?
            }

            I32AtomicRmwAnd { memarg } => self.mem_instr(state, "i32.atomic.rmw.and", memarg, 4)?,
            I64AtomicRmwAnd { memarg } => self.mem_instr(state, "i64.atomic.rmw.and", memarg, 8)?,
            I32AtomicRmw8AndU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw8.and_u", memarg, 1)?
            }
            I32AtomicRmw16AndU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw16.and_u", memarg, 2)?
            }
            I64AtomicRmw8AndU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw8.and_u", memarg, 1)?
            }
            I64AtomicRmw16AndU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw16.and_u", memarg, 2)?
            }
            I64AtomicRmw32AndU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw32.and_u", memarg, 4)?
            }

            I32AtomicRmwOr { memarg } => self.mem_instr(state, "i32.atomic.rmw.or", memarg, 4)?,
            I64AtomicRmwOr { memarg } => self.mem_instr(state, "i64.atomic.rmw.or", memarg, 8)?,
            I32AtomicRmw8OrU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw8.or_u", memarg, 1)?
            }
            I32AtomicRmw16OrU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw16.or_u", memarg, 2)?
            }
            I64AtomicRmw8OrU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw8.or_u", memarg, 1)?
            }
            I64AtomicRmw16OrU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw16.or_u", memarg, 2)?
            }
            I64AtomicRmw32OrU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw32.or_u", memarg, 4)?
            }

            I32AtomicRmwXor { memarg } => self.mem_instr(state, "i32.atomic.rmw.xor", memarg, 4)?,
            I64AtomicRmwXor { memarg } => self.mem_instr(state, "i64.atomic.rmw.xor", memarg, 8)?,
            I32AtomicRmw8XorU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw8.xor_u", memarg, 1)?
            }
            I32AtomicRmw16XorU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw16.xor_u", memarg, 2)?
            }
            I64AtomicRmw8XorU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw8.xor_u", memarg, 1)?
            }
            I64AtomicRmw16XorU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw16.xor_u", memarg, 2)?
            }
            I64AtomicRmw32XorU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw32.xor_u", memarg, 4)?
            }

            I32AtomicRmwXchg { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw.xchg", memarg, 4)?
            }
            I64AtomicRmwXchg { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw.xchg", memarg, 8)?
            }
            I32AtomicRmw8XchgU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw8.xchg_u", memarg, 1)?
            }
            I32AtomicRmw16XchgU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw16.xchg_u", memarg, 2)?
            }
            I64AtomicRmw8XchgU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw8.xchg_u", memarg, 1)?
            }
            I64AtomicRmw16XchgU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw16.xchg_u", memarg, 2)?
            }
            I64AtomicRmw32XchgU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw32.xchg_u", memarg, 4)?
            }

            I32AtomicRmwCmpxchg { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw.cmpxchg", memarg, 4)?
            }
            I64AtomicRmwCmpxchg { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw.cmpxchg", memarg, 8)?
            }
            I32AtomicRmw8CmpxchgU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw8.cmpxchg_u", memarg, 1)?
            }
            I32AtomicRmw16CmpxchgU { memarg } => {
                self.mem_instr(state, "i32.atomic.rmw16.cmpxchg_u", memarg, 2)?
            }
            I64AtomicRmw8CmpxchgU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw8.cmpxchg_u", memarg, 1)?
            }
            I64AtomicRmw16CmpxchgU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw16.cmpxchg_u", memarg, 2)?
            }
            I64AtomicRmw32CmpxchgU { memarg } => {
                self.mem_instr(state, "i64.atomic.rmw32.cmpxchg_u", memarg, 4)?
            }

            V128Load { memarg } => self.mem_instr(state, "v128.load", memarg, 16)?,
            V128Store { memarg } => self.mem_instr(state, "v128.store", memarg, 16)?,
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

            I64x2Eq => self.result.push_str("i64x2.eq"),
            I64x2Ne => self.result.push_str("i64x2.ne"),
            I64x2LtS => self.result.push_str("i64x2.lt_s"),
            I64x2GtS => self.result.push_str("i64x2.gt_s"),
            I64x2LeS => self.result.push_str("i64x2.le_s"),
            I64x2GeS => self.result.push_str("i64x2.ge_s"),

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
            V128AnyTrue => self.result.push_str("v128.any_true"),

            I8x16Abs => self.result.push_str("i8x16.abs"),
            I8x16Neg => self.result.push_str("i8x16.neg"),
            I8x16AllTrue => self.result.push_str("i8x16.all_true"),
            I8x16Bitmask => self.result.push_str("i8x16.bitmask"),
            I8x16Shl => self.result.push_str("i8x16.shl"),
            I8x16ShrU => self.result.push_str("i8x16.shr_u"),
            I8x16ShrS => self.result.push_str("i8x16.shr_s"),
            I8x16Add => self.result.push_str("i8x16.add"),
            I8x16AddSatS => self.result.push_str("i8x16.add_sat_s"),
            I8x16AddSatU => self.result.push_str("i8x16.add_sat_u"),
            I8x16Sub => self.result.push_str("i8x16.sub"),
            I8x16SubSatS => self.result.push_str("i8x16.sub_sat_s"),
            I8x16SubSatU => self.result.push_str("i8x16.sub_sat_u"),

            I16x8Abs => self.result.push_str("i16x8.abs"),
            I16x8Neg => self.result.push_str("i16x8.neg"),
            I16x8AllTrue => self.result.push_str("i16x8.all_true"),
            I16x8Bitmask => self.result.push_str("i16x8.bitmask"),
            I16x8Shl => self.result.push_str("i16x8.shl"),
            I16x8ShrU => self.result.push_str("i16x8.shr_u"),
            I16x8ShrS => self.result.push_str("i16x8.shr_s"),
            I16x8Add => self.result.push_str("i16x8.add"),
            I16x8AddSatS => self.result.push_str("i16x8.add_sat_s"),
            I16x8AddSatU => self.result.push_str("i16x8.add_sat_u"),
            I16x8Sub => self.result.push_str("i16x8.sub"),
            I16x8SubSatS => self.result.push_str("i16x8.sub_sat_s"),
            I16x8SubSatU => self.result.push_str("i16x8.sub_sat_u"),
            I16x8Mul => self.result.push_str("i16x8.mul"),

            I32x4Abs => self.result.push_str("i32x4.abs"),
            I32x4Neg => self.result.push_str("i32x4.neg"),
            I32x4AllTrue => self.result.push_str("i32x4.all_true"),
            I32x4Bitmask => self.result.push_str("i32x4.bitmask"),
            I32x4Shl => self.result.push_str("i32x4.shl"),
            I32x4ShrU => self.result.push_str("i32x4.shr_u"),
            I32x4ShrS => self.result.push_str("i32x4.shr_s"),
            I32x4Add => self.result.push_str("i32x4.add"),
            I32x4Sub => self.result.push_str("i32x4.sub"),
            I32x4Mul => self.result.push_str("i32x4.mul"),

            I64x2Abs => self.result.push_str("i64x2.abs"),
            I64x2Neg => self.result.push_str("i64x2.neg"),
            I64x2AllTrue => self.result.push_str("i64x2.all_true"),
            I64x2Bitmask => self.result.push_str("i64x2.bitmask"),
            I64x2Shl => self.result.push_str("i64x2.shl"),
            I64x2ShrU => self.result.push_str("i64x2.shr_u"),
            I64x2ShrS => self.result.push_str("i64x2.shr_s"),
            I64x2Add => self.result.push_str("i64x2.add"),
            I64x2Sub => self.result.push_str("i64x2.sub"),
            I64x2Mul => self.result.push_str("i64x2.mul"),

            F32x4Ceil => self.result.push_str("f32x4.ceil"),
            F32x4Floor => self.result.push_str("f32x4.floor"),
            F32x4Trunc => self.result.push_str("f32x4.trunc"),
            F32x4Nearest => self.result.push_str("f32x4.nearest"),
            F64x2Ceil => self.result.push_str("f64x2.ceil"),
            F64x2Floor => self.result.push_str("f64x2.floor"),
            F64x2Trunc => self.result.push_str("f64x2.trunc"),
            F64x2Nearest => self.result.push_str("f64x2.nearest"),
            F32x4Abs => self.result.push_str("f32x4.abs"),
            F32x4Neg => self.result.push_str("f32x4.neg"),
            F32x4Sqrt => self.result.push_str("f32x4.sqrt"),
            F32x4Add => self.result.push_str("f32x4.add"),
            F32x4Sub => self.result.push_str("f32x4.sub"),
            F32x4Div => self.result.push_str("f32x4.div"),
            F32x4Mul => self.result.push_str("f32x4.mul"),
            F32x4Min => self.result.push_str("f32x4.min"),
            F32x4Max => self.result.push_str("f32x4.max"),
            F32x4PMin => self.result.push_str("f32x4.pmin"),
            F32x4PMax => self.result.push_str("f32x4.pmax"),

            F64x2Abs => self.result.push_str("f64x2.abs"),
            F64x2Neg => self.result.push_str("f64x2.neg"),
            F64x2Sqrt => self.result.push_str("f64x2.sqrt"),
            F64x2Add => self.result.push_str("f64x2.add"),
            F64x2Sub => self.result.push_str("f64x2.sub"),
            F64x2Div => self.result.push_str("f64x2.div"),
            F64x2Mul => self.result.push_str("f64x2.mul"),
            F64x2Min => self.result.push_str("f64x2.min"),
            F64x2Max => self.result.push_str("f64x2.max"),
            F64x2PMin => self.result.push_str("f64x2.pmin"),
            F64x2PMax => self.result.push_str("f64x2.pmax"),

            I32x4TruncSatF32x4S => self.result.push_str("i32x4.trunc_sat_f32x4_s"),
            I32x4TruncSatF32x4U => self.result.push_str("i32x4.trunc_sat_f32x4_u"),
            F32x4ConvertI32x4S => self.result.push_str("f32x4.convert_i32x4_s"),
            F32x4ConvertI32x4U => self.result.push_str("f32x4.convert_i32x4_u"),

            I8x16Swizzle => self.result.push_str("i8x16.swizzle"),
            I8x16Shuffle { lanes } => {
                self.result.push_str("i8x16.shuffle");
                for lane in lanes {
                    write!(self.result, " {}", lane)?;
                }
            }
            V128Load8Splat { memarg } => self.mem_instr(state, "v128.load8_splat", memarg, 1)?,
            V128Load16Splat { memarg } => self.mem_instr(state, "v128.load16_splat", memarg, 2)?,
            V128Load32Splat { memarg } => self.mem_instr(state, "v128.load32_splat", memarg, 4)?,
            V128Load64Splat { memarg } => self.mem_instr(state, "v128.load64_splat", memarg, 8)?,

            V128Load32Zero { memarg } => self.mem_instr(state, "v128.load32_zero", memarg, 4)?,
            V128Load64Zero { memarg } => self.mem_instr(state, "v128.load64_zero", memarg, 8)?,

            I8x16NarrowI16x8S => self.result.push_str("i8x16.narrow_i16x8_s"),
            I8x16NarrowI16x8U => self.result.push_str("i8x16.narrow_i16x8_u"),
            I16x8NarrowI32x4S => self.result.push_str("i16x8.narrow_i32x4_s"),
            I16x8NarrowI32x4U => self.result.push_str("i16x8.narrow_i32x4_u"),

            I16x8ExtendLowI8x16S => self.result.push_str("i16x8.extend_low_i8x16_s"),
            I16x8ExtendHighI8x16S => self.result.push_str("i16x8.extend_high_i8x16_s"),
            I16x8ExtendLowI8x16U => self.result.push_str("i16x8.extend_low_i8x16_u"),
            I16x8ExtendHighI8x16U => self.result.push_str("i16x8.extend_high_i8x16_u"),
            I32x4ExtendLowI16x8S => self.result.push_str("i32x4.extend_low_i16x8_s"),
            I32x4ExtendHighI16x8S => self.result.push_str("i32x4.extend_high_i16x8_s"),
            I32x4ExtendLowI16x8U => self.result.push_str("i32x4.extend_low_i16x8_u"),
            I32x4ExtendHighI16x8U => self.result.push_str("i32x4.extend_high_i16x8_u"),
            I64x2ExtendLowI32x4S => self.result.push_str("i64x2.extend_low_i32x4_s"),
            I64x2ExtendHighI32x4S => self.result.push_str("i64x2.extend_high_i32x4_s"),
            I64x2ExtendLowI32x4U => self.result.push_str("i64x2.extend_low_i32x4_u"),
            I64x2ExtendHighI32x4U => self.result.push_str("i64x2.extend_high_i32x4_u"),

            I16x8ExtMulLowI8x16S => self.result.push_str("i16x8.extmul_low_i8x16_s"),
            I16x8ExtMulHighI8x16S => self.result.push_str("i16x8.extmul_high_i8x16_s"),
            I16x8ExtMulLowI8x16U => self.result.push_str("i16x8.extmul_low_i8x16_u"),
            I16x8ExtMulHighI8x16U => self.result.push_str("i16x8.extmul_high_i8x16_u"),
            I32x4ExtMulLowI16x8S => self.result.push_str("i32x4.extmul_low_i16x8_s"),
            I32x4ExtMulHighI16x8S => self.result.push_str("i32x4.extmul_high_i16x8_s"),
            I32x4ExtMulLowI16x8U => self.result.push_str("i32x4.extmul_low_i16x8_u"),
            I32x4ExtMulHighI16x8U => self.result.push_str("i32x4.extmul_high_i16x8_u"),
            I64x2ExtMulLowI32x4S => self.result.push_str("i64x2.extmul_low_i32x4_s"),
            I64x2ExtMulHighI32x4S => self.result.push_str("i64x2.extmul_high_i32x4_s"),
            I64x2ExtMulLowI32x4U => self.result.push_str("i64x2.extmul_low_i32x4_u"),
            I64x2ExtMulHighI32x4U => self.result.push_str("i64x2.extmul_high_i32x4_u"),

            I16x8Q15MulrSatS => self.result.push_str("i16x8.q15mulr_sat_s"),

            V128Load8x8S { memarg } => self.mem_instr(state, "v128.load8x8_s", memarg, 8)?,
            V128Load8x8U { memarg } => self.mem_instr(state, "v128.load8x8_u", memarg, 8)?,
            V128Load16x4S { memarg } => self.mem_instr(state, "v128.load16x4_s", memarg, 8)?,
            V128Load16x4U { memarg } => self.mem_instr(state, "v128.load16x4_u", memarg, 8)?,
            V128Load32x2S { memarg } => self.mem_instr(state, "v128.load32x2_s", memarg, 8)?,
            V128Load32x2U { memarg } => self.mem_instr(state, "v128.load32x2_u", memarg, 8)?,

            V128Load8Lane { memarg, lane } => {
                self.mem_instr(state, "v128.load8_lane", memarg, 1)?;
                write!(self.result, " {}", lane)?;
            }
            V128Load16Lane { memarg, lane } => {
                self.mem_instr(state, "v128.load16_lane", memarg, 2)?;
                write!(self.result, " {}", lane)?;
            }
            V128Load32Lane { memarg, lane } => {
                self.mem_instr(state, "v128.load32_lane", memarg, 4)?;
                write!(self.result, " {}", lane)?;
            }
            V128Load64Lane { memarg, lane } => {
                self.mem_instr(state, "v128.load64_lane", memarg, 8)?;
                write!(self.result, " {}", lane)?;
            }

            V128Store8Lane { memarg, lane } => {
                self.mem_instr(state, "v128.store8_lane", memarg, 1)?;
                write!(self.result, " {}", lane)?;
            }
            V128Store16Lane { memarg, lane } => {
                self.mem_instr(state, "v128.store16_lane", memarg, 2)?;
                write!(self.result, " {}", lane)?;
            }
            V128Store32Lane { memarg, lane } => {
                self.mem_instr(state, "v128.store32_lane", memarg, 4)?;
                write!(self.result, " {}", lane)?;
            }
            V128Store64Lane { memarg, lane } => {
                self.mem_instr(state, "v128.store64_lane", memarg, 8)?;
                write!(self.result, " {}", lane)?;
            }

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
            I32x4DotI16x8S => self.result.push_str("i32x4.dot_i16x8_s"),

            F32x4DemoteF64x2Zero => self.result.push_str("f32x4.demote_f64x2_zero"),
            F64x2PromoteLowF32x4 => self.result.push_str("f64x2.promote_low_f32x4"),
            F64x2ConvertLowI32x4S => self.result.push_str("f64x2.convert_low_i32x4_s"),
            F64x2ConvertLowI32x4U => self.result.push_str("f64x2.convert_low_i32x4_u"),
            I32x4TruncSatF64x2SZero => self.result.push_str("i32x4.trunc_sat_f64x2_s_zero"),
            I32x4TruncSatF64x2UZero => self.result.push_str("i32x4.trunc_sat_f64x2_u_zero"),

            I8x16Popcnt => self.result.push_str("i8x16.popcnt"),

            I16x8ExtAddPairwiseI8x16S => self.result.push_str("i16x8.extadd_pairwise_i8x16_s"),
            I16x8ExtAddPairwiseI8x16U => self.result.push_str("i16x8.extadd_pairwise_i8x16_u"),
            I32x4ExtAddPairwiseI16x8S => self.result.push_str("i32x4.extadd_pairwise_i16x8_s"),
            I32x4ExtAddPairwiseI16x8U => self.result.push_str("i32x4.extadd_pairwise_i16x8_u"),

            I8x16RelaxedSwizzle => self.result.push_str("i8x16.relaxed_swizzle"),
            I32x4RelaxedTruncSatF32x4S => self.result.push_str("i32x4.relaxed_trunc_f32x4_s"),
            I32x4RelaxedTruncSatF32x4U => self.result.push_str("i32x4.relaxed_trunc_f32x4_u"),
            I32x4RelaxedTruncSatF64x2SZero => {
                self.result.push_str("i32x4.relaxed_trunc_f64x2_s_zero")
            }
            I32x4RelaxedTruncSatF64x2UZero => {
                self.result.push_str("i32x4.relaxed_trunc_f64x2_u_zero")
            }
            F32x4Fma => self.result.push_str("f32x4.fma"),
            F32x4Fms => self.result.push_str("f32x4.fms"),
            F64x2Fma => self.result.push_str("f64x2.fma"),
            F64x2Fms => self.result.push_str("f64x2.fms"),
            I8x16LaneSelect => self.result.push_str("i8x16.laneselect"),
            I16x8LaneSelect => self.result.push_str("i16x8.laneselect"),
            I32x4LaneSelect => self.result.push_str("i32x4.laneselect"),
            I64x2LaneSelect => self.result.push_str("i64x2.laneselect"),
            F32x4RelaxedMin => self.result.push_str("f32x4.relaxed_min"),
            F32x4RelaxedMax => self.result.push_str("f32x4.relaxed_max"),
            F64x2RelaxedMin => self.result.push_str("f64x2.relaxed_min"),
            F64x2RelaxedMax => self.result.push_str("f64x2.relaxed_max"),
        }
        Ok(())
    }

    fn mem_instr(
        &mut self,
        state: &State,
        name: &str,
        memarg: &MemoryImmediate,
        default_align: u32,
    ) -> Result<()> {
        self.result.push_str(name);
        if memarg.memory != 0 {
            self.result.push(' ');
            self.print_idx(&state.core.memory_names, memarg.memory)?;
        }
        if memarg.offset != 0 {
            write!(self.result, " offset={}", memarg.offset)?;
        }
        if memarg.align >= 32 {
            bail!("alignment in memarg too large");
        }
        let align = 1 << memarg.align;
        if default_align != align {
            write!(self.result, " align={}", align)?;
        }
        Ok(())
    }

    fn print_blockty(
        &mut self,
        state: &mut State,
        types: &[Type],
        ty: &BlockType,
        cur_depth: u32,
    ) -> Result<()> {
        if let Some(name) = state
            .core
            .label_names
            .get(&(state.core.funcs.len() as u32, state.core.labels))
        {
            self.result.push(' ');
            name.write(&mut self.result);
        }
        match ty {
            BlockType::Empty => {}
            BlockType::Type(t) => {
                self.result.push_str(" (result ");
                self.print_valtype(*t)?;
                self.result.push(')');
            }
            BlockType::FuncType(idx) => {
                self.print_core_functype_idx(state, types, *idx, false, None)?;
            }
        }
        write!(self.result, "  ;; label = @{}", cur_depth)?;
        state.core.labels += 1;
        Ok(())
    }

    fn print_exports(&mut self, state: &State, data: ExportSectionReader) -> Result<()> {
        for export in data {
            self.newline();
            self.print_export(state, &export?)?;
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
        self.result.push_str(" (type ");
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

    fn print_elems(
        &mut self,
        state: &mut State,
        types: &[Type],
        data: ElementSectionReader,
    ) -> Result<()> {
        for (i, elem) in data.into_iter().enumerate() {
            let mut elem = elem?;
            self.newline();
            self.start_group("elem ");
            self.print_name(&state.core.element_names, i as u32)?;
            match &mut elem.kind {
                ElementKind::Passive => {}
                ElementKind::Declared => write!(self.result, " declare")?,
                ElementKind::Active {
                    table_index,
                    init_expr,
                } => {
                    if *table_index != 0 {
                        self.result.push_str(" (table ");
                        self.print_idx(&state.core.table_names, *table_index)?;
                        self.result.push(')');
                    }
                    self.result.push(' ');
                    self.print_init_expr_sugar(state, types, init_expr, "offset")?;
                }
            }
            let mut items_reader = elem.items.get_items_reader()?;
            self.result.push(' ');
            if items_reader.uses_exprs() {
                self.print_valtype(elem.ty)?;
            } else {
                self.print_reftype(elem.ty)?;
            }
            for _ in 0..items_reader.get_count() {
                self.result.push(' ');
                match items_reader.read()? {
                    ElementItem::Expr(expr) => {
                        self.print_init_expr_sugar(state, types, &expr, "item")?
                    }
                    ElementItem::Func(idx) => self.print_idx(&state.core.func_names, idx)?,
                }
            }
            self.end_group();
        }
        Ok(())
    }

    fn print_data(
        &mut self,
        state: &mut State,
        types: &[Type],
        data: DataSectionReader,
    ) -> Result<()> {
        for (i, data) in data.into_iter().enumerate() {
            let data = data?;
            self.newline();
            self.start_group("data ");
            self.print_name(&state.core.data_names, i as u32)?;
            self.result.push(' ');
            match &data.kind {
                DataKind::Passive => {}
                DataKind::Active {
                    memory_index,
                    init_expr,
                } => {
                    if *memory_index != 0 {
                        self.result.push_str("(memory ");
                        self.print_idx(&state.core.memory_names, *memory_index)?;
                        self.result.push_str(") ");
                    }
                    self.print_init_expr_sugar(state, types, init_expr, "offset")?;
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
    fn print_init_expr_sugar(
        &mut self,
        state: &mut State,
        types: &[Type],
        expr: &InitExpr,
        explicit: &str,
    ) -> Result<()> {
        self.start_group("");
        let mut first_op = None;
        for (i, op) in expr.get_operators_reader().into_iter().enumerate() {
            match op? {
                Operator::End => {}

                // Save the first operator to get printed later.
                other if i == 0 => first_op = Some(other),

                // If this is the second operator (i == 1) then we push our
                // un-sugared form with `explicit` as the starting delimiter and
                // then the operators are printed.
                other => {
                    if i == 1 {
                        self.result.push_str(explicit);
                        self.result.push(' ');
                        self.print_operator(state, types, &first_op.take().unwrap(), self.nesting)?;
                    }
                    self.result.push(' ');
                    self.print_operator(state, types, &other, self.nesting)?;
                }
            }
        }

        // If `first_op` is still set here then it means we don't need to print
        // an expression with `explicit` as the leading token, instead we can
        // print the single operator.
        if let Some(op) = first_op {
            self.print_operator(state, types, &op, self.nesting)?;
        }
        self.end_group();
        Ok(())
    }

    /// Prints the operators of `expr` space-separated.
    fn print_init_expr(
        &mut self,
        state: &mut State,
        types: &[Type],
        expr: &InitExpr,
    ) -> Result<()> {
        for (i, op) in expr.get_operators_reader().into_iter().enumerate() {
            match op? {
                Operator::End => {}
                other => {
                    if i > 0 {
                        self.result.push(' ');
                    }
                    self.print_operator(state, types, &other, self.nesting)?
                }
            }
        }
        Ok(())
    }

    fn print_primitive_val_type(&mut self, ty: &PrimitiveValType) {
        match ty {
            PrimitiveValType::Unit => self.result.push_str("unit"),
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
            self.result.push(' ');
            self.print_component_val_type(state, &case.ty)?;

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

    fn print_expected_type(
        &mut self,
        state: &State,
        ok: &ComponentValType,
        error: &ComponentValType,
    ) -> Result<()> {
        self.start_group("expected ");
        self.print_component_val_type(state, ok)?;
        self.result.push(' ');
        self.print_component_val_type(state, error)?;
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
            ComponentDefinedType::Expected { ok, error } => {
                self.print_expected_type(state, ok, error)?
            }
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
        types: &mut Vec<Type>,
        decls: Vec<ModuleTypeDeclaration>,
    ) -> Result<()> {
        states.push(State::new(Encoding::Module));
        self.newline();
        self.start_group("module");
        for decl in decls {
            self.newline();
            match decl {
                ModuleTypeDeclaration::Type(ty) => {
                    self.print_type(states.last_mut().unwrap(), types, ty)?
                }
                ModuleTypeDeclaration::Alias(alias) => {
                    self.print_alias(states, types, alias, false)?
                }
                ModuleTypeDeclaration::Export { name, ty } => {
                    self.start_group("export ");
                    self.print_str(name)?;
                    self.result.push(' ');
                    self.print_import_ty(states.last_mut().unwrap(), types, &ty, false)?;
                    self.end_group();
                }
                ModuleTypeDeclaration::Import(import) => {
                    self.print_import(states.last_mut().unwrap(), types, &import, false)?;
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
        types: &mut Vec<Type<'a>>,
        decls: Vec<ComponentTypeDeclaration<'a>>,
    ) -> Result<HashMap<String, usize>> {
        states.push(State::new(Encoding::Component));
        self.newline();
        self.start_group("component");
        for decl in decls {
            self.newline();
            match decl {
                ComponentTypeDeclaration::CoreType(ty) => {
                    self.print_core_type(states, types, ty)?
                }
                ComponentTypeDeclaration::Type(ty) => {
                    self.print_component_type_def(states, types, ty)?
                }
                ComponentTypeDeclaration::Alias(alias) => match alias {
                    ComponentAlias::InstanceExport { .. } => {
                        bail!("component types cannot alias instance exports")
                    }
                    ComponentAlias::Outer { kind, count, index } => {
                        self.print_component_outer_alias(states, kind, count, index)?
                    }
                },
                ComponentTypeDeclaration::Export { name, ty } => {
                    self.start_group("export ");
                    self.print_str(name)?;
                    self.result.push(' ');
                    self.print_component_import_ty(states.last().unwrap(), &ty, false)?;
                    self.end_group();
                    match ty {
                        ComponentTypeRef::Func(idx)
                        | ComponentTypeRef::Instance(idx)
                        | ComponentTypeRef::Component(idx) => {
                            let state = states.last_mut().unwrap();
                            let idx = state.ty(idx)?;
                            state.add_component_export(name, idx)?;
                        }
                        _ => {}
                    }
                }
                ComponentTypeDeclaration::Import(import) => {
                    self.print_component_import(states.last_mut().unwrap(), &import, false)?
                }
            }
        }
        self.end_group();
        Ok(states.pop().unwrap().component.exports)
    }

    fn print_instance_type<'a>(
        &mut self,
        states: &mut Vec<State>,
        types: &mut Vec<Type<'a>>,
        decls: Vec<InstanceTypeDeclaration<'a>>,
    ) -> Result<HashMap<String, usize>> {
        states.push(State::new(Encoding::Component));
        self.newline();
        self.start_group("instance");
        for decl in decls {
            self.newline();
            match decl {
                InstanceTypeDeclaration::CoreType(ty) => self.print_core_type(states, types, ty)?,
                InstanceTypeDeclaration::Type(ty) => {
                    self.print_component_type_def(states, types, ty)?
                }
                InstanceTypeDeclaration::Alias(alias) => match alias {
                    ComponentAlias::InstanceExport { .. } => {
                        bail!("instance types cannot alias instance exports")
                    }
                    ComponentAlias::Outer { kind, count, index } => {
                        self.print_component_outer_alias(states, kind, count, index)?
                    }
                },
                InstanceTypeDeclaration::Export { name, ty } => {
                    self.start_group("export ");
                    self.print_str(name)?;
                    self.result.push(' ');
                    self.print_component_import_ty(states.last().unwrap(), &ty, false)?;
                    self.end_group();
                    match ty {
                        ComponentTypeRef::Func(idx)
                        | ComponentTypeRef::Instance(idx)
                        | ComponentTypeRef::Component(idx) => {
                            let state = states.last_mut().unwrap();
                            let idx = state.ty(idx)?;
                            state.add_component_export(name, idx)?;
                        }
                        _ => {}
                    }
                }
            }
        }
        self.end_group();
        Ok(states.pop().unwrap().component.exports)
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
        core_prefix: bool,
    ) -> Result<()> {
        let index = {
            let state = states.last().unwrap();
            let outer = Self::outer_state(states, count)?;
            self.start_group(if core_prefix {
                "core alias outer "
            } else {
                "alias outer "
            });
            if let Some(name) = outer.name.as_ref() {
                name.write(&mut self.result);
            } else {
                self.result.push_str(count.to_string().as_str());
            }
            self.result.push(' ');
            let index = match kind {
                OuterAliasKind::Type => {
                    self.print_idx(&outer.core.type_names, index)?;
                    self.result.push(' ');
                    self.start_group("type ");
                    self.print_name(&state.core.type_names, state.core.types.len() as u32)?;
                    Some(outer.core_ty(index)?)
                }
            };
            self.end_group(); // kind
            self.end_group(); // alias
            index
        };

        let state = states.last_mut().unwrap();
        match kind {
            OuterAliasKind::Type => state.core.types.push(index.unwrap()),
        }

        Ok(())
    }

    fn print_component_outer_alias(
        &mut self,
        states: &mut [State],
        kind: ComponentOuterAliasKind,
        count: u32,
        index: u32,
    ) -> Result<()> {
        let index = {
            let state = states.last().unwrap();
            let outer = Self::outer_state(states, count)?;
            self.start_group("alias outer ");
            if let Some(name) = outer.name.as_ref() {
                name.write(&mut self.result);
            } else {
                self.result.push_str(count.to_string().as_str());
            }
            self.result.push(' ');
            let index = match kind {
                ComponentOuterAliasKind::CoreModule => {
                    self.print_idx(&outer.core.module_names, index)?;
                    self.result.push(' ');
                    self.start_group("core module ");
                    self.print_name(&state.core.module_names, state.core.modules)?;
                    None
                }
                ComponentOuterAliasKind::CoreType => {
                    self.print_idx(&outer.core.type_names, index)?;
                    self.result.push(' ');
                    self.start_group("core type ");
                    self.print_name(&state.core.type_names, state.core.types.len() as u32)?;
                    Some(outer.core_ty(index)?)
                }
                ComponentOuterAliasKind::Type => {
                    self.print_idx(&outer.component.type_names, index)?;
                    self.result.push(' ');
                    self.start_group("type ");
                    self.print_name(
                        &state.component.type_names,
                        state.component.types.len() as u32,
                    )?;
                    Some(outer.ty(index)?)
                }
                ComponentOuterAliasKind::Component => {
                    self.print_idx(&outer.component.component_names, index)?;
                    self.result.push(' ');
                    self.start_group("component ");
                    self.print_name(
                        &state.component.component_names,
                        state.component.components.len() as u32,
                    )?;
                    Some(outer.component(index)?)
                }
            };
            self.end_group(); // kind
            self.end_group(); // alias
            index
        };

        let state = states.last_mut().unwrap();
        match kind {
            ComponentOuterAliasKind::CoreModule => state.core.modules += 1,
            ComponentOuterAliasKind::CoreType => state.core.types.push(index.unwrap()),
            ComponentOuterAliasKind::Type => state.component.types.push(index.unwrap()),
            ComponentOuterAliasKind::Component => state.component.components.push(index.unwrap()),
        }

        Ok(())
    }

    fn print_component_func_type(&mut self, state: &State, ty: &ComponentFuncType) -> Result<()> {
        self.start_group("func");
        for (name, ty) in ty.params.iter() {
            self.result.push(' ');
            self.start_group("param ");
            if let Some(name) = name {
                self.print_str(name)?;
                self.result.push(' ');
            }
            self.print_component_val_type(state, ty)?;
            self.end_group()
        }

        if !matches!(
            ty.result,
            ComponentValType::Primitive(PrimitiveValType::Unit)
        ) {
            self.result.push(' ');
            self.start_group("result ");
            self.print_component_val_type(state, &ty.result)?;
            self.end_group();
        }

        self.end_group();

        Ok(())
    }

    fn print_component_type_def<'a>(
        &mut self,
        states: &mut Vec<State>,
        types: &mut Vec<Type<'a>>,
        ty: ComponentType<'a>,
    ) -> Result<()> {
        self.start_group("type ");
        {
            let state = states.last_mut().unwrap();
            self.print_name(
                &state.component.type_names,
                state.component.types.len() as u32,
            )?;
        }
        self.result.push(' ');
        let ty = match ty {
            ComponentType::Defined(ty) => {
                self.print_defined_type(states.last_mut().unwrap(), &ty)?;
                Type::Defined
            }
            ComponentType::Func(ty) => {
                self.print_component_func_type(states.last_mut().unwrap(), &ty)?;
                Type::ComponentFunc(ty)
            }
            ComponentType::Component(decls) => {
                Type::Component(self.print_component_type(states, types, decls.into_vec())?)
            }
            ComponentType::Instance(decls) => {
                Type::Instance(self.print_instance_type(states, types, decls.into_vec())?)
            }
        };
        self.end_group();

        states.last_mut().unwrap().component.types.push(types.len());
        types.push(ty);

        Ok(())
    }

    fn print_component_types<'a>(
        &mut self,
        states: &mut Vec<State>,
        types: &mut Vec<Type<'a>>,
        parser: ComponentTypeSectionReader<'a>,
    ) -> Result<()> {
        for ty in parser {
            let ty = ty?;
            self.newline();
            self.print_component_type_def(states, types, ty)?;
        }

        Ok(())
    }

    fn print_component_imports(
        &mut self,
        state: &mut State,
        types: &[Type],
        parser: ComponentImportSectionReader,
    ) -> Result<()> {
        for import in parser {
            let import = import?;
            self.newline();
            self.print_component_import(state, &import, true)?;
            match import.ty {
                ComponentTypeRef::Module(_) => {
                    state.core.modules += 1;
                }
                ComponentTypeRef::Func(idx) => {
                    let ty = state.ty(idx)?;
                    match &types[ty] {
                        Type::ComponentFunc(_) => {
                            state.component.funcs.push(ty);
                        }
                        _ => bail!("index is not a function type"),
                    }
                }
                ComponentTypeRef::Value(_) => {
                    state.component.values += 1;
                }
                ComponentTypeRef::Type(TypeBounds::Eq, idx) => {
                    state.component.types.push(state.ty(idx)?);
                }
                ComponentTypeRef::Instance(idx) => {
                    let ty = state.ty(idx)?;
                    match &types[ty] {
                        Type::Instance(_) => {
                            state.component.instances.push(ty);
                        }
                        _ => bail!("index is not an instance type"),
                    }
                }
                ComponentTypeRef::Component(idx) => {
                    let ty = state.ty(idx)?;
                    match &types[ty] {
                        Type::Component(_) => {
                            state.component.components.push(ty);
                        }
                        _ => bail!("index is not an instance type"),
                    }
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
                self.start_group("core module");
                if index {
                    self.result.push(' ');
                    self.print_name(&state.core.module_names, state.core.modules as u32)?;
                }
                self.print_type_ref(state, *idx, true, None)?;
                self.end_group();
            }
            ComponentTypeRef::Func(idx) => {
                self.start_group("func");
                if index {
                    self.result.push(' ');
                    self.print_name(
                        &state.component.func_names,
                        state.component.funcs.len() as u32,
                    )?;
                }
                self.print_type_ref(state, *idx, false, None)?;
                self.end_group();
            }
            ComponentTypeRef::Value(ty) => {
                self.start_group("value ");
                if index {
                    self.print_name(&state.component.value_names, state.component.values as u32)?;
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
                self.start_group("instance");
                if index {
                    self.result.push(' ');
                    self.print_name(
                        &state.component.instance_names,
                        state.component.instances.len() as u32,
                    )?;
                }
                self.print_type_ref(state, *idx, false, None)?;
                self.end_group();
            }
            ComponentTypeRef::Component(idx) => {
                self.start_group("component");
                if index {
                    self.result.push(' ');
                    self.print_name(
                        &state.component.component_names,
                        state.component.components.len() as u32,
                    )?;
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
        for export in parser {
            let export = export?;
            self.newline();
            self.print_component_export(state, &export)?;

            match export.kind {
                ComponentExternalKind::Component => {
                    state.add_component_export(export.name, state.component(export.index)?)?;
                }
                ComponentExternalKind::Instance => {
                    state.add_component_export(export.name, state.instance(export.index)?)?;
                }
                ComponentExternalKind::Func => {
                    state.add_component_export(export.name, state.func(export.index)?)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn print_component_export(
        &mut self,
        state: &mut State,
        export: &ComponentExport,
    ) -> Result<()> {
        self.start_group("export ");
        self.print_str(export.name)?;
        self.result.push(' ');
        self.print_component_external_kind(state, export.kind, export.index)?;
        self.end_group();
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
        types: &mut Vec<Type>,
        parser: ComponentCanonicalSectionReader,
    ) -> Result<()> {
        for func in parser {
            self.newline();
            match func? {
                CanonicalFunction::Lift {
                    core_func_index,
                    type_index,
                    options,
                } => {
                    self.start_group("func ");
                    self.print_name(
                        &state.component.func_names,
                        state.component.funcs.len() as u32,
                    )?;
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
                    state.component.funcs.push(state.ty(type_index)?);
                }
                CanonicalFunction::Lower {
                    func_index,
                    options,
                } => {
                    self.start_group("core func ");
                    self.print_name(&state.core.func_names, state.core.funcs.len() as u32)?;
                    self.result.push(' ');
                    self.start_group("canon lower ");
                    self.start_group("func ");
                    self.print_idx(&state.component.func_names, func_index)?;
                    self.end_group();
                    self.print_canonical_options(state, &options)?;
                    self.end_group();
                    self.end_group();

                    // We don't actually care about core func types in components, so don't
                    // bother lowering the component function type.
                    state.core.funcs.push(types.len());
                    types.push(Type::Func(None));
                }
            }
        }

        Ok(())
    }

    fn print_instances(&mut self, state: &mut State, parser: InstanceSectionReader) -> Result<()> {
        for instance in parser {
            self.newline();
            self.start_group("core instance ");
            self.print_name(&state.core.instance_names, state.core.instances)?;
            self.result.push(' ');
            match instance? {
                Instance::Instantiate { module_index, args } => {
                    self.start_group("instantiate ");
                    self.print_idx(&state.core.module_names, module_index)?;
                    for arg in args.iter() {
                        self.newline();
                        self.print_instantiation_arg(state, arg)?;
                    }
                    self.end_group();
                    state.core.instances += 1;
                }
                Instance::FromExports(exports) => {
                    for export in exports.iter() {
                        self.newline();
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
        types: &mut Vec<Type>,
        parser: ComponentInstanceSectionReader,
    ) -> Result<()> {
        for instance in parser {
            self.newline();
            self.start_group("instance ");
            self.print_name(
                &state.component.instance_names,
                state.component.instances.len() as u32,
            )?;
            match instance? {
                ComponentInstance::Instantiate {
                    component_index,
                    args,
                } => {
                    self.result.push(' ');
                    self.start_group("instantiate ");
                    self.print_idx(&state.component.component_names, component_index)?;
                    for arg in args.iter() {
                        self.newline();
                        self.print_component_instantiation_arg(state, arg)?;
                    }
                    self.end_group();
                    state
                        .component
                        .instances
                        .push(state.component(component_index)?);
                }
                ComponentInstance::FromExports(exports) => {
                    let mut type_exports = HashMap::new();
                    for export in exports.iter() {
                        self.newline();
                        self.print_component_export(state, export)?;

                        let existing = match export.kind {
                            ComponentExternalKind::Component => type_exports
                                .insert(export.name.to_string(), state.component(export.index)?),
                            ComponentExternalKind::Instance => type_exports
                                .insert(export.name.to_string(), state.instance(export.index)?),
                            ComponentExternalKind::Func => type_exports
                                .insert(export.name.to_string(), state.func(export.index)?),
                            _ => continue,
                        };

                        if existing.is_some() {
                            bail!("duplicate export named `{}` for instance", export.name);
                        }
                    }

                    state.component.instances.push(types.len());
                    types.push(Type::Instance(type_exports));
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
        types: &[Type],
        mut parser: ComponentStartSectionReader,
    ) -> Result<()> {
        let start = parser.read()?;

        let ty = match state.component.funcs.get(start.func_index as usize) {
            Some(type_index) => match &types[*type_index] {
                Type::ComponentFunc(ty) => ty,
                _ => bail!(
                    "start function index {} is not a component function",
                    start.func_index
                ),
            },
            None => {
                bail!("start function {} out of bounds", start.func_index);
            }
        };

        self.newline();
        self.start_group("start ");
        self.print_idx(&state.component.func_names, start.func_index)?;

        for arg in start.arguments.iter() {
            self.result.push(' ');
            self.start_group("value ");
            self.print_idx(&state.component.value_names, *arg)?;
            self.end_group();
        }

        if !matches!(
            ty.result,
            ComponentValType::Primitive(PrimitiveValType::Unit)
        ) {
            self.result.push(' ');
            self.start_group("result ");
            self.print_name(&state.component.value_names, state.component.values)?;
            self.end_group();
            state.component.values += 1;
        }

        self.end_group(); // start

        Ok(())
    }

    fn print_alias(
        &mut self,
        states: &mut [State],
        types: &mut Vec<Type>,
        alias: Alias,
        core_prefix: bool,
    ) -> Result<()> {
        match alias {
            Alias::InstanceExport {
                instance_index,
                kind,
                name,
            } => {
                let state = states.last_mut().unwrap();
                self.start_group(if core_prefix {
                    "core alias export "
                } else {
                    "alias export "
                });
                self.print_idx(&state.core.instance_names, instance_index)?;
                self.result.push(' ');
                self.print_str(name)?;
                self.result.push(' ');
                match kind {
                    ExternalKind::Func => {
                        self.start_group("func ");
                        self.print_name(&state.core.func_names, state.core.funcs.len() as u32)?;
                        self.end_group();
                        state.core.funcs.push(types.len());
                        types.push(Type::Func(None));
                    }
                    ExternalKind::Table => {
                        self.start_group("table ");
                        self.print_name(&state.core.table_names, state.core.tables)?;
                        self.end_group();
                        state.core.tables += 1;
                    }
                    ExternalKind::Memory => {
                        self.start_group("memory ");
                        self.print_name(&state.core.memory_names, state.core.memories)?;
                        self.end_group();
                        state.core.memories += 1;
                    }
                    ExternalKind::Global => {
                        self.start_group("global ");
                        self.print_name(&state.core.global_names, state.core.globals)?;
                        self.end_group();
                        state.core.globals += 1;
                    }
                    ExternalKind::Tag => {
                        self.start_group("tag ");
                        write!(self.result, "(;{};)", state.core.tags)?;
                        self.end_group();
                        state.core.tags += 1;
                    }
                }
                self.end_group(); // alias export
            }
            Alias::Outer { kind, count, index } => {
                self.print_outer_alias(states, kind, count, index, core_prefix)?;
            }
        }
        Ok(())
    }

    fn print_aliases(
        &mut self,
        states: &mut Vec<State>,
        types: &mut Vec<Type>,
        parser: AliasSectionReader,
    ) -> Result<()> {
        for alias in parser {
            self.newline();
            self.print_alias(states, types, alias?, true)?;
        }
        Ok(())
    }

    fn print_component_aliases(
        &mut self,
        states: &mut [State],
        types: &[Type],
        parser: ComponentAliasSectionReader,
    ) -> Result<()> {
        for alias in parser {
            self.newline();
            match alias? {
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
                                state.component.components.len() as u32,
                            )?;
                            self.end_group();

                            let ty =
                                state.component_instance_export(types, instance_index, name)?;
                            match types[ty] {
                                Type::Component(_) => {
                                    state.component.components.push(ty);
                                }
                                _ => {
                                    bail!(
                                        "export `{}` from component instance {} is not a component",
                                        name,
                                        instance_index
                                    );
                                }
                            }
                        }
                        ComponentExternalKind::Instance => {
                            self.start_group("instance ");
                            self.print_name(
                                &state.component.instance_names,
                                state.component.instances.len() as u32,
                            )?;
                            self.end_group();

                            let ty =
                                state.component_instance_export(types, instance_index, name)?;
                            match types[ty] {
                                Type::Instance(_) => {
                                    state.component.instances.push(ty);
                                }
                                _ => {
                                    bail!(
                                        "export `{}` from component instance {} is not an instance",
                                        name,
                                        instance_index
                                    );
                                }
                            }
                        }
                        ComponentExternalKind::Func => {
                            self.start_group("func ");
                            self.print_name(
                                &state.component.func_names,
                                state.component.funcs.len() as u32,
                            )?;
                            self.end_group();

                            let ty =
                                state.component_instance_export(types, instance_index, name)?;
                            match types[ty] {
                                Type::ComponentFunc(_) => {
                                    state.component.funcs.push(ty);
                                }
                                _ => {
                                    bail!(
                                        "export `{}` from component instance {} is not a function",
                                        name,
                                        instance_index
                                    );
                                }
                            }
                        }
                        ComponentExternalKind::Value => {
                            self.start_group("value ");
                            self.print_name(&state.component.value_names, state.component.values)?;
                            self.end_group();
                            state.component.values += 1;
                        }
                        ComponentExternalKind::Type => {
                            bail!("component export alias for types is not supported")
                        }
                    }

                    self.end_group(); // alias export
                }
                ComponentAlias::Outer { kind, count, index } => {
                    self.print_component_outer_alias(states, kind, count, index)?
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
    fn new<'a>(name: &'a str, index: u32, group: &str, used: &mut HashSet<&'a str>) -> Naming {
        let mut identifier = None;

        // If the `name` provided can't be used as the raw identifier for the
        // item that it's describing then a synthetic name must be made. The
        // rules here which generate a name are:
        //
        // * Empty identifiers are not allowed
        // * Identifiers have a fixed set of valid characters
        // * For wasmprinter's purposes we "reserve" identifiers with the `#`
        //   prefix, which is in theory rare to encounter in practice.
        // * If the name has already been used for some other item then it can't
        //   be reused.
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
            || !used.insert(name)
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
