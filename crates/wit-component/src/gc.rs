use self::bitvec::BitVec;
use anyhow::{bail, Result};
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;
use std::mem;
use wasm_encoder::{Encode, EntityType};
use wasmparser::*;

/// This function will reduce the input core `wasm` module to only the set of
/// exports `required`.
///
/// This internally performs a "gc" pass after removing exports to ensure that
/// the resulting module imports the minimal set of functions necessary.
pub fn run(wasm: &[u8], required: &IndexMap<&str, FuncType>) -> Result<Vec<u8>> {
    assert!(!required.is_empty());

    let mut module = Module::default();
    module.parse(wasm)?;

    // Make sure that all required names are present in the module, and then
    // remove all names that are not required.
    for (name, _ty) in required {
        if !module.exports.contains_key(name) {
            bail!("adapter module does not have export `{name}`")
        }
    }
    let mut not_required = IndexSet::new();
    for name in module.exports.keys().copied() {
        // Explicitly keep `cabi_realloc` if it's there in case an interface
        // needs it for a lowering.
        if !required.contains_key(name) && name != "cabi_realloc" {
            not_required.insert(name);
        }
    }
    for name in not_required {
        module.exports.remove(name);
    }
    assert!(!module.exports.is_empty());
    module.liveness()?;
    module.encode()
}

// Representation of a wasm module which is used to GC a module to its minimal
// set of required items necessary to implement the `exports`
//
// Note that this is not a complete representation of a wasm module since it
// doesn't represent everything such as data and element segments. This is only
// used for adapter modules which otherwise have these restrictions and makes
// this gc pass a bit easier to write.
#[derive(Default)]
struct Module<'a> {
    // Definitions found when parsing a module
    types: Vec<wasmparser::Type>,
    tables: Vec<Table<'a>>,
    globals: Vec<Global<'a>>,
    memories: Vec<Memory<'a>>,
    funcs: Vec<Func<'a>>,
    exports: IndexMap<&'a str, Export<'a>>,
    func_names: HashMap<u32, &'a str>,
    global_names: HashMap<u32, &'a str>,

    // Known-live sets of indices after the `liveness` pass has run.
    live_types: BitVec,
    live_tables: BitVec,
    live_globals: BitVec,
    live_memories: BitVec,
    live_funcs: BitVec,

    // Helper data structure used during the `liveness` path to avoid recursion.
    // When calculating the liveness of an item this `worklist` is pushed to and
    // then processed until it's empty. An item pushed onto this list represents
    // a new index that has been discovered to be live and the function is wehat
    // walks the item's definition to find other items that it references.
    worklist: Vec<(u32, fn(&mut Module<'a>, u32) -> Result<()>)>,
}

struct Table<'a> {
    def: Definition<'a, ()>,
    ty: TableType,
}

struct Memory<'a> {
    def: Definition<'a, ()>,
    ty: MemoryType,
}

struct Global<'a> {
    def: Definition<'a, ConstExpr<'a>>,
    ty: GlobalType,
}

struct Func<'a> {
    def: Definition<'a, FunctionBody<'a>>,
    ty: u32,
}

enum Definition<'a, T> {
    Import(&'a str, &'a str),
    Local(T),
}

impl<'a> Module<'a> {
    fn parse(&mut self, wasm: &'a [u8]) -> Result<()> {
        let mut next_code_index = 0;
        let mut validator = Validator::new();
        for payload in Parser::new(0).parse_all(wasm) {
            let payload = payload?;
            validator.payload(&payload)?;
            match payload {
                Payload::Version { encoding, .. } => {
                    if encoding != Encoding::Module {
                        bail!("adapter must be a core wasm module, not a component");
                    }
                }
                Payload::End(_) => {}
                Payload::TypeSection(s) => {
                    for ty in s {
                        self.types.push(ty?);
                    }
                }
                Payload::ImportSection(s) => {
                    for i in s {
                        let i = i?;
                        match i.ty {
                            TypeRef::Func(ty) => self.funcs.push(Func {
                                def: Definition::Import(i.module, i.name),
                                ty,
                            }),
                            TypeRef::Table(ty) => self.tables.push(Table {
                                def: Definition::Import(i.module, i.name),
                                ty,
                            }),
                            TypeRef::Global(ty) => self.globals.push(Global {
                                def: Definition::Import(i.module, i.name),
                                ty,
                            }),
                            TypeRef::Memory(ty) => self.memories.push(Memory {
                                def: Definition::Import(i.module, i.name),
                                ty,
                            }),
                            TypeRef::Tag(_) => bail!("unsupported `tag` type"),
                        }
                    }
                }
                Payload::TableSection(s) => {
                    for ty in s {
                        let ty = ty?;
                        self.tables.push(Table {
                            def: Definition::Local(()),
                            ty,
                        });
                    }
                }
                Payload::MemorySection(s) => {
                    for ty in s {
                        let ty = ty?;
                        self.memories.push(Memory {
                            def: Definition::Local(()),
                            ty,
                        });
                    }
                }
                Payload::GlobalSection(s) => {
                    for g in s {
                        let g = g?;
                        self.globals.push(Global {
                            def: Definition::Local(g.init_expr),
                            ty: g.ty,
                        });
                    }
                }

                Payload::ExportSection(s) => {
                    for e in s {
                        let e = e?;
                        self.exports.insert(e.name, e);
                    }
                }

                Payload::FunctionSection(s) => {
                    next_code_index = self.funcs.len();
                    for ty in s {
                        let ty = ty?;
                        self.funcs.push(Func {
                            // Specify a dummy definition to get filled in later
                            // when parsing the code section.
                            def: Definition::Local(FunctionBody::new(0, &[])),
                            ty,
                        });
                    }
                }

                Payload::CodeSectionStart { .. } => {}
                Payload::CodeSectionEntry(body) => {
                    self.funcs[next_code_index].def = Definition::Local(body);
                    next_code_index += 1;
                }

                // Ignore all custom sections except for the `name` section
                // which we parse, but ignore errors within.
                Payload::CustomSection(s) => {
                    if s.name() == "name" {
                        drop(self.parse_name_section(&s));
                    }
                }

                // sections that shouldn't appear in the specially-crafted core wasm
                // adapter self we're processing
                Payload::DataCountSection { .. }
                | Payload::ElementSection(_)
                | Payload::DataSection(_)
                | Payload::StartSection { .. }
                | Payload::TagSection(_)
                | Payload::UnknownSection { .. } => {
                    bail!("unsupported section found in adapter module")
                }

                // component-model related things that shouldn't show up
                Payload::ModuleSection { .. }
                | Payload::ComponentSection { .. }
                | Payload::InstanceSection(_)
                | Payload::ComponentInstanceSection(_)
                | Payload::ComponentAliasSection(_)
                | Payload::ComponentCanonicalSection(_)
                | Payload::ComponentStartSection(_)
                | Payload::ComponentImportSection(_)
                | Payload::CoreTypeSection(_)
                | Payload::ComponentExportSection(_)
                | Payload::ComponentTypeSection(_) => {
                    bail!("component section found in adapter module")
                }
            }
        }

        Ok(())
    }

    fn parse_name_section(&mut self, section: &CustomSectionReader<'a>) -> Result<()> {
        let section = NameSectionReader::new(section.data(), section.data_offset())?;
        for s in section {
            match s? {
                Name::Function(map) => {
                    let mut map = map.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        self.func_names.insert(naming.index, naming.name);
                    }
                }
                Name::Global(map) => {
                    let mut map = map.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        self.global_names.insert(naming.index, naming.name);
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Iteratively calculates the set of live items within this module
    /// considering all exports as the root of live functions.
    fn liveness(&mut self) -> Result<()> {
        let exports = mem::take(&mut self.exports);
        for (_, e) in exports.iter() {
            match e.kind {
                ExternalKind::Func => self.func(e.index),
                ExternalKind::Global => self.global(e.index),
                ExternalKind::Table => self.table(e.index),
                ExternalKind::Memory => self.memory(e.index),
                ExternalKind::Tag => bail!("unsupported exported tag"),
            }
        }
        self.exports = exports;

        while let Some((idx, func)) = self.worklist.pop() {
            func(self, idx)?;
        }
        Ok(())
    }

    fn func(&mut self, func: u32) {
        if !self.live_funcs.insert(func) {
            return;
        }
        self.worklist.push((func, |me, func| {
            let func = &me.funcs[func as usize];
            me.live_types.insert(func.ty);
            let mut body = match &func.def {
                Definition::Import(..) => return Ok(()),
                Definition::Local(e) => e.get_binary_reader(),
            };
            let local_count = body.read_var_u32()?;
            for _ in 0..local_count {
                body.read_var_u32()?;
                body.read_val_type()?;
            }
            me.operators(body)
        }));
    }

    fn global(&mut self, global: u32) {
        if !self.live_globals.insert(global) {
            return;
        }
        self.worklist.push((global, |me, global| {
            let init = match &me.globals[global as usize].def {
                Definition::Import(..) => return Ok(()),
                Definition::Local(e) => e,
            };
            me.operators(init.get_binary_reader())
        }));
    }

    fn table(&mut self, table: u32) {
        self.live_tables.insert(table);
    }

    fn memory(&mut self, memory: u32) {
        self.live_memories.insert(memory);
    }

    fn blockty(&mut self, ty: BlockType) {
        if let BlockType::FuncType(ty) = ty {
            self.live_types.insert(ty);
        }
    }

    fn operators(&mut self, mut reader: BinaryReader<'a>) -> Result<()> {
        while !reader.eof() {
            reader.visit_operator(self)?;
        }
        Ok(())
    }

    fn live_types(&self) -> impl Iterator<Item = (u32, &wasmparser::Type)> + '_ {
        live_iter(&self.live_types, self.types.iter())
    }

    fn live_funcs(&self) -> impl Iterator<Item = (u32, &Func<'a>)> + '_ {
        live_iter(&self.live_funcs, self.funcs.iter())
    }

    fn live_memories(&self) -> impl Iterator<Item = (u32, &Memory<'a>)> + '_ {
        live_iter(&self.live_memories, self.memories.iter())
    }

    fn live_globals(&self) -> impl Iterator<Item = (u32, &Global<'a>)> + '_ {
        live_iter(&self.live_globals, self.globals.iter())
    }

    fn live_tables(&self) -> impl Iterator<Item = (u32, &Table<'a>)> + '_ {
        live_iter(&self.live_tables, self.tables.iter())
    }

    /// Encodes this `Module` to a new wasm module which is gc'd and only
    /// contains the items that are live as calculated by the `liveness` pass.
    fn encode(&mut self) -> Result<Vec<u8>> {
        // Data structure used to track the mapping of old index to new index
        // for all live items.
        let mut map = Encoder::default();

        // Sections that will be assembled into the final module at the end of
        // this function.
        let mut types = wasm_encoder::TypeSection::new();
        let mut imports = wasm_encoder::ImportSection::new();
        let mut funcs = wasm_encoder::FunctionSection::new();
        let mut tables = wasm_encoder::TableSection::new();
        let mut memories = wasm_encoder::MemorySection::new();
        let mut globals = wasm_encoder::GlobalSection::new();
        let mut code = wasm_encoder::CodeSection::new();

        let mut empty_type = None;
        for (i, ty) in self.live_types() {
            map.types.push(i);
            match ty {
                Type::Func(ty) => {
                    types.function(
                        ty.params().iter().copied().map(valty),
                        ty.results().iter().copied().map(valty),
                    );

                    // Keep track of the "empty type" to see if we can reuse an
                    // existing one or one needs to be injected if a `start`
                    // function is calculated at the end.
                    if ty.params().len() == 0 && ty.results().len() == 0 {
                        empty_type = Some(map.types.remap(i));
                    }
                }
            }
        }

        let mut num_memories = 0;
        for (i, mem) in self.live_memories() {
            map.memories.push(i);
            let ty = wasm_encoder::MemoryType {
                minimum: mem.ty.initial,
                maximum: mem.ty.maximum,
                shared: mem.ty.shared,
                memory64: mem.ty.memory64,
            };
            match &mem.def {
                Definition::Import(m, n) => {
                    imports.import(m, n, ty);
                }
                Definition::Local(()) => {
                    memories.memory(ty);
                }
            }
            num_memories += 1;
        }

        for (i, table) in self.live_tables() {
            map.tables.push(i);
            let ty = wasm_encoder::TableType {
                minimum: table.ty.initial,
                maximum: table.ty.maximum,
                element_type: valty(table.ty.element_type),
            };
            match &table.def {
                Definition::Import(m, n) => {
                    imports.import(m, n, ty);
                }
                Definition::Local(()) => {
                    tables.table(ty);
                }
            }
        }

        for (i, global) in self.live_globals() {
            map.globals.push(i);
            let ty = wasm_encoder::GlobalType {
                mutable: global.ty.mutable,
                val_type: valty(global.ty.content_type),
            };
            match &global.def {
                Definition::Import(m, n) => {
                    imports.import(m, n, ty);
                }
                Definition::Local(init) => {
                    let mut bytes = map.operators(init.get_binary_reader())?;
                    assert_eq!(bytes.pop(), Some(0xb));
                    globals.global(ty, &wasm_encoder::ConstExpr::raw(bytes));
                }
            }
        }

        // For functions first assign a new index to all functions and then
        // afterwards actually map the body of all functions so the `map` of all
        // index mappings is fully populated before instructions are mapped.
        let mut num_funcs = 0;
        for (i, func) in self.live_funcs() {
            map.funcs.push(i);
            let ty = map.types.remap(func.ty);
            match &func.def {
                Definition::Import(m, n) => {
                    imports.import(m, n, EntityType::Function(ty));
                }
                Definition::Local(_) => {
                    funcs.function(ty);
                }
            }
            num_funcs += 1;
        }

        for (_, func) in self.live_funcs() {
            let mut body = match &func.def {
                Definition::Import(..) => continue,
                Definition::Local(body) => body.get_binary_reader(),
            };
            let mut locals = Vec::new();
            for _ in 0..body.read_var_u32()? {
                let cnt = body.read_var_u32()?;
                let ty = body.read_val_type()?;
                locals.push((cnt, valty(ty)));
            }
            let mut func = wasm_encoder::Function::new(locals);
            let bytes = map.operators(body)?;
            func.raw(bytes);
            code.function(&func);
        }

        // Inject a start function to initialize the stack pointer which will be
        // local to this module. This only happens if a memory is preserved and
        // a stack pointer global is found.
        let mut start = None;
        let sp = self.find_stack_pointer()?;
        if let Some(sp) = sp {
            if num_memories > 0 {
                use wasm_encoder::Instruction::*;

                // If there are any memories or any mutable globals there must be
                // precisely one of each as otherwise we don't know how to filter
                // down to the right one.
                if num_memories != 1 {
                    bail!("adapter modules don't support multi-memory");
                }

                let sp = map.globals.remap(sp);

                // Generate a function type for this start function, adding a new
                // function type to the module if necessary.
                let empty_type = empty_type.unwrap_or_else(|| {
                    types.function([], []);
                    types.len() - 1
                });
                funcs.function(empty_type);

                let mut func = wasm_encoder::Function::new([(1, wasm_encoder::ValType::I32)]);
                // Grow the memory by 1 page to allocate ourselves some stack space.
                func.instruction(&I32Const(1));
                func.instruction(&MemoryGrow(0));
                func.instruction(&LocalTee(0));

                // Test if the return value of the growth was -1 and trap if so
                // since we don't have a stack page.
                func.instruction(&I32Const(-1));
                func.instruction(&I32Eq);
                func.instruction(&If(wasm_encoder::BlockType::Empty));
                func.instruction(&Unreachable);
                func.instruction(&End);

                // Set our stack pointer to the top of the page we were given, which
                // is the page index times the page size plus the size of a page.
                func.instruction(&LocalGet(0));
                func.instruction(&I32Const(1));
                func.instruction(&I32Add);
                func.instruction(&I32Const(16));
                func.instruction(&I32Shl);
                func.instruction(&GlobalSet(sp));
                func.instruction(&End);
                code.function(&func);

                start = Some(wasm_encoder::StartSection {
                    function_index: num_funcs,
                });
            }
        }

        // Sanity-check the shape of the module since some parts won't work if
        // this fails. Note that during parsing we've already validated there
        // are no data segments or element segments.

        // Shouldn't have any tables if there are no element segments since
        // otherwise there's no meaning to a defined or imported table.
        if self.live_tables().count() != 0 {
            bail!("tables should not be present in the final adapter module");
        }

        // multi-memory should not be enabled and if any memory it should be
        // imported.
        if self.live_memories().count() > 1 {
            bail!("the adapter module should not use multi-memory");
        }
        if !memories.is_empty() {
            bail!("locally-defined memories are not allowed define a local memory");
        }

        let mut ret = wasm_encoder::Module::default();
        if !types.is_empty() {
            ret.section(&types);
        }
        if !imports.is_empty() {
            ret.section(&imports);
        }
        if !funcs.is_empty() {
            ret.section(&funcs);
        }
        if !tables.is_empty() {
            ret.section(&tables);
        }
        if !memories.is_empty() {
            ret.section(&memories);
        }
        if !globals.is_empty() {
            ret.section(&globals);
        }

        if !self.exports.is_empty() {
            let mut exports = wasm_encoder::ExportSection::new();
            for (_, export) in self.exports.iter() {
                let (kind, index) = match export.kind {
                    ExternalKind::Func => (
                        wasm_encoder::ExportKind::Func,
                        map.funcs.remap(export.index),
                    ),
                    ExternalKind::Table => (
                        wasm_encoder::ExportKind::Table,
                        map.tables.remap(export.index),
                    ),
                    ExternalKind::Memory => (
                        wasm_encoder::ExportKind::Memory,
                        map.memories.remap(export.index),
                    ),
                    ExternalKind::Global => (
                        wasm_encoder::ExportKind::Global,
                        map.globals.remap(export.index),
                    ),
                    kind => bail!("unsupported export kind {kind:?}"),
                };
                exports.export(export.name, kind, index);
            }
            ret.section(&exports);
        }

        if let Some(start) = &start {
            ret.section(start);
        }

        if !code.is_empty() {
            ret.section(&code);
        }

        // Append a custom `name` section using the names of the functions that
        // were found prior to the GC pass in the original module.
        let mut func_names = Vec::new();
        let mut global_names = Vec::new();
        for (i, _func) in self.live_funcs() {
            let name = match self.func_names.get(&i) {
                Some(name) => name,
                None => continue,
            };
            func_names.push((map.funcs.remap(i), *name));
        }
        if start.is_some() {
            func_names.push((num_funcs, "initialize_stack_pointer"));
        }
        for (i, _global) in self.live_globals() {
            let name = match self.global_names.get(&i) {
                Some(name) => name,
                None => continue,
            };
            global_names.push((map.globals.remap(i), *name));
        }
        let mut section = Vec::new();
        let mut encode_subsection = |code: u8, names: &[(u32, &str)]| {
            if names.is_empty() {
                return;
            }
            let mut subsection = Vec::new();
            names.len().encode(&mut subsection);
            for (i, name) in names {
                i.encode(&mut subsection);
                name.encode(&mut subsection);
            }
            section.push(code);
            subsection.encode(&mut section);
        };
        encode_subsection(0x01, &func_names);
        encode_subsection(0x07, &global_names);
        if !section.is_empty() {
            ret.section(&wasm_encoder::CustomSection {
                name: "name",
                data: &section,
            });
        }

        Ok(ret.finish())
    }

    fn find_stack_pointer(&self) -> Result<Option<u32>> {
        let mutable_i32_globals = self
            .live_globals()
            .filter(|(_, g)| g.ty.mutable && g.ty.content_type == ValType::I32)
            .collect::<Vec<_>>();

        // If there are no 32-bit mutable globals then there's definitely no
        // stack pointer in this module
        if mutable_i32_globals.is_empty() {
            return Ok(None);
        }

        // If there are some mutable 32-bit globals then there's currently no
        // great way of determining which is the stack pointer. For now a hack
        // is used where we use the name section to find the name that LLVM
        // injects. This hopefully can be improved in the future.
        let stack_pointers = mutable_i32_globals
            .iter()
            .filter_map(|(i, _)| {
                let name = *self.global_names.get(&i)?;
                if name == "__stack_pointer" {
                    Some(*i)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        match stack_pointers.len() {
            0 => Ok(None),
            1 => Ok(Some(stack_pointers[0])),
            n => bail!("found {n} globals that look like the stack pointer"),
        }
    }
}

// This helper macro is used to define a visitor of all instructions with
// special handling for all payloads of instructions to mark any referenced
// items live.
//
// Currently item identification happesn through the field name of the payload.
// While not exactly the most robust solution this should work well enough for
// now.
macro_rules! define_visit {
    ($(@$p:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
        $(
            fn $visit(&mut self, _offset: usize $(, $($arg: $argty),*)?)  {
                $(
                    $(
                        define_visit!(mark_live self $arg $arg);
                    )*
                )?
            }
        )*
    };

    (mark_live $self:ident $arg:ident type_index) => {$self.live_types.insert($arg);};
    (mark_live $self:ident $arg:ident src_table) => {$self.table($arg);};
    (mark_live $self:ident $arg:ident dst_table) => {$self.table($arg);};
    (mark_live $self:ident $arg:ident table_index) => {$self.table($arg);};
    (mark_live $self:ident $arg:ident table) => {$self.table($arg);};
    (mark_live $self:ident $arg:ident table_index) => {$self.table($arg);};
    (mark_live $self:ident $arg:ident global_index) => {$self.global($arg);};
    (mark_live $self:ident $arg:ident function_index) => {$self.func($arg);};
    (mark_live $self:ident $arg:ident mem) => {$self.memory($arg);};
    (mark_live $self:ident $arg:ident src_mem) => {$self.memory($arg);};
    (mark_live $self:ident $arg:ident dst_mem) => {$self.memory($arg);};
    (mark_live $self:ident $arg:ident memarg) => {$self.memory($arg.memory);};
    (mark_live $self:ident $arg:ident blockty) => {$self.blockty($arg);};
    (mark_live $self:ident $arg:ident lane) => {};
    (mark_live $self:ident $arg:ident lanes) => {};
    (mark_live $self:ident $arg:ident flags) => {};
    (mark_live $self:ident $arg:ident value) => {};
    (mark_live $self:ident $arg:ident mem_byte) => {};
    (mark_live $self:ident $arg:ident table_byte) => {};
    (mark_live $self:ident $arg:ident local_index) => {};
    (mark_live $self:ident $arg:ident relative_depth) => {};
    (mark_live $self:ident $arg:ident tag_index) => {};
    (mark_live $self:ident $arg:ident targets) => {};
    (mark_live $self:ident $arg:ident ty) => {};
    (mark_live $self:ident $arg:ident data_index) => {};
    (mark_live $self:ident $arg:ident elem_index) => {};
}

impl<'a> VisitOperator<'a> for Module<'a> {
    type Output = ();

    wasmparser::for_each_operator!(define_visit);
}

/// Helper function to filter `iter` based on the `live` set, yielding an
/// iterator over the index of the item that's live as well as the item itself.
fn live_iter<'a, T>(
    live: &'a BitVec,
    iter: impl Iterator<Item = T> + 'a,
) -> impl Iterator<Item = (u32, T)> + 'a {
    iter.enumerate().filter_map(|(i, t)| {
        let i = i as u32;
        if live.contains(i) {
            Some((i, t))
        } else {
            None
        }
    })
}

#[derive(Default)]
struct Encoder {
    types: Remap,
    funcs: Remap,
    memories: Remap,
    globals: Remap,
    tables: Remap,
    buf: Vec<u8>,
}

impl Encoder {
    fn operators(&mut self, mut reader: BinaryReader<'_>) -> Result<Vec<u8>> {
        assert!(self.buf.is_empty());
        while !reader.eof() {
            reader.visit_operator(self)?;
        }
        Ok(mem::take(&mut self.buf))
    }

    fn memarg(&self, ty: MemArg) -> wasm_encoder::MemArg {
        wasm_encoder::MemArg {
            offset: ty.offset,
            align: ty.align.into(),
            memory_index: self.memories.remap(ty.memory),
        }
    }

    fn blockty(&self, ty: BlockType) -> wasm_encoder::BlockType {
        match ty {
            BlockType::Empty => wasm_encoder::BlockType::Empty,
            BlockType::Type(ty) => wasm_encoder::BlockType::Result(valty(ty)),
            BlockType::FuncType(ty) => wasm_encoder::BlockType::FunctionType(self.types.remap(ty)),
        }
    }
}

// This is a helper macro to translate all `wasmparser` instructions to
// `wasm-encoder` instructions without having to list out every single
// instruction itself.
//
// The general goal of this macro is to have O(unique instruction payload)
// number of cases while also simultaneously adapting between the styles of
// wasmparser and wasm-encoder.
macro_rules! define_encode {
    ($(@$p:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
        $(
            fn $visit(&mut self, _offset: usize $(, $($arg: $argty),*)?)  {
                #[allow(unused_imports)]
                use wasm_encoder::Instruction::*;
                $(
                    $(
                        let $arg = define_encode!(map self $arg $arg);
                    )*
                )?
                let insn = define_encode!(mk $op $($($arg)*)?);
                insn.encode(&mut self.buf);
            }
        )*
    };

    // No-payload instructions are named the same in wasmparser as they are in
    // wasm-encoder
    (mk $op:ident) => ($op);

    // Instructions which need "special care" to map from wasmparser to
    // wasm-encoder
    (mk BrTable $arg:ident) => ({
        BrTable($arg.0, $arg.1)
    });
    (mk CallIndirect $ty:ident $table:ident $table_byte:ident) => ({
        drop($table_byte);
        CallIndirect { ty: $ty, table: $table }
    });
    (mk ReturnCallIndirect $ty:ident $table:ident) => (
        ReturnCallIndirect { ty: $ty, table: $table }
    );
    (mk MemorySize $mem:ident $mem_byte:ident) => ({
        drop($mem_byte);
        MemorySize($mem)
    });
    (mk MemoryGrow $mem:ident $mem_byte:ident) => ({
        drop($mem_byte);
        MemoryGrow($mem)
    });
    (mk I32Const $v:ident) => (I32Const($v));
    (mk I64Const $v:ident) => (I64Const($v));
    (mk F32Const $v:ident) => (F32Const(f32::from_bits($v.bits())));
    (mk F64Const $v:ident) => (F64Const(f64::from_bits($v.bits())));
    (mk V128Const $v:ident) => (V128Const($v.i128()));

    // Catch-all for the translation of one payload argument which is typically
    // represented as a tuple-enum in wasm-encoder.
    (mk $op:ident $arg:ident) => ($op($arg));

    // Catch-all of everything else where the wasmparser fields are simply
    // translated to wasm-encoder fields.
    (mk $op:ident $($arg:ident)*) => ($op { $($arg),* });

    // Individual cases of mapping one argument type to another, similar to the
    // `define_visit` macro above.
    (map $self:ident $arg:ident memarg) => {$self.memarg($arg)};
    (map $self:ident $arg:ident blockty) => {$self.blockty($arg)};
    (map $self:ident $arg:ident tag_index) => {$arg};
    (map $self:ident $arg:ident relative_depth) => {$arg};
    (map $self:ident $arg:ident function_index) => {$self.funcs.remap($arg)};
    (map $self:ident $arg:ident global_index) => {$self.globals.remap($arg)};
    (map $self:ident $arg:ident mem) => {$self.memories.remap($arg)};
    (map $self:ident $arg:ident src_mem) => {$self.memories.remap($arg)};
    (map $self:ident $arg:ident dst_mem) => {$self.memories.remap($arg)};
    (map $self:ident $arg:ident table) => {$self.tables.remap($arg)};
    (map $self:ident $arg:ident table_index) => {$self.tables.remap($arg)};
    (map $self:ident $arg:ident src_table) => {$self.tables.remap($arg)};
    (map $self:ident $arg:ident dst_table) => {$self.tables.remap($arg)};
    (map $self:ident $arg:ident type_index) => {$self.types.remap($arg)};
    (map $self:ident $arg:ident ty) => {valty($arg)};
    (map $self:ident $arg:ident local_index) => {$arg};
    (map $self:ident $arg:ident lane) => {$arg};
    (map $self:ident $arg:ident lanes) => {$arg};
    (map $self:ident $arg:ident elem_index) => {$arg};
    (map $self:ident $arg:ident data_index) => {$arg};
    (map $self:ident $arg:ident table_byte) => {$arg};
    (map $self:ident $arg:ident mem_byte) => {$arg};
    (map $self:ident $arg:ident value) => {$arg};
    (map $self:ident $arg:ident targets) => ((
        $arg.targets().map(|i| i.unwrap()).collect::<Vec<_>>().into(),
        $arg.default(),
    ));
}

impl<'a> VisitOperator<'a> for Encoder {
    type Output = ();

    wasmparser::for_each_operator!(define_encode);
}

fn valty(ty: wasmparser::ValType) -> wasm_encoder::ValType {
    match ty {
        wasmparser::ValType::I32 => wasm_encoder::ValType::I32,
        wasmparser::ValType::I64 => wasm_encoder::ValType::I64,
        wasmparser::ValType::F32 => wasm_encoder::ValType::F32,
        wasmparser::ValType::F64 => wasm_encoder::ValType::F64,
        wasmparser::ValType::V128 => wasm_encoder::ValType::V128,
        wasmparser::ValType::FuncRef => wasm_encoder::ValType::FuncRef,
        wasmparser::ValType::ExternRef => wasm_encoder::ValType::ExternRef,
    }
}

// Minimal definition of a bit vector necessary for the liveness calculations
// above.
mod bitvec {
    use std::mem;

    type T = u64;

    #[derive(Default)]
    pub struct BitVec {
        bits: Vec<T>,
    }

    impl BitVec {
        /// Inserts `idx` into this bit vector, returning whether it was not
        /// previously present.
        pub fn insert(&mut self, idx: u32) -> bool {
            let (idx, bit) = idx_bit(idx);
            match self.bits.get_mut(idx) {
                Some(bits) => {
                    if *bits & bit != 0 {
                        return false;
                    }
                    *bits |= bit;
                }
                None => {
                    self.bits.resize(idx + 1, 0);
                    self.bits[idx] = bit;
                }
            }
            true
        }

        /// Returns whether this bit vector contains the specified `idx`th bit.
        pub fn contains(&self, idx: u32) -> bool {
            let (idx, bit) = idx_bit(idx);
            match self.bits.get(idx) {
                Some(bits) => (*bits & bit) != 0,
                None => false,
            }
        }
    }

    fn idx_bit(idx: u32) -> (usize, T) {
        let idx = idx as usize;
        let size = mem::size_of::<T>() * 8;
        let index = idx / size;
        let bit = 1 << (idx % size);
        (index, bit)
    }
}

/// Small data structure used to track index mappings from an old index space to
/// a new.
#[derive(Default)]
struct Remap {
    /// Map, indexed by the old index set, to the new index set.
    ///
    /// Placeholders of `u32::MAX` means that the old index is not present in
    /// the new index space.
    map: Vec<u32>,
    /// The next available index in the new index space.
    next: u32,
}

impl Remap {
    /// Appends a new live "old index" into this remapping structure.
    ///
    /// This will assign a new index for the old index provided. This method
    /// must be called in increasing order of old indexes.
    fn push(&mut self, old: u32) {
        self.map.resize(old as usize, u32::MAX);
        self.map.push(self.next);
        self.next += 1;
    }

    /// Returns the new index corresponding to an old index.
    ///
    /// Panics if the `old` index was not added via `push` above.
    fn remap(&self, old: u32) -> u32 {
        let ret = self.map[old as usize];
        assert!(ret != u32::MAX);
        return ret;
    }
}
