//! Support for "pseudo-dynamic", shared-everything linking of Wasm modules into a component.
//!
//! This implements [shared-everything
//! linking](https://github.com/WebAssembly/component-model/blob/main/design/mvp/examples/SharedEverythingDynamicLinking.md),
//! taking as input one or more [dynamic
//! library](https://github.com/WebAssembly/tool-conventions/blob/main/DynamicLinking.md) modules and producing a
//! component whose type is the union of any `component-type*` custom sections found in the input modules.
//!
//! The entry point into this process is `Linker::encode`, which analyzes and topologically sorts the input
//! modules, then synthesizes two additional modules:
//!
//! - `main` AKA `env`: hosts the component's single memory and function table and exports any functions needed to
//! break dependency cycles discovered in the input modules. Those functions use `call.indirect` to invoke the real
//! functions, references to which are placed in the table by the `init` module.
//!
//! - `init`: populates the function table as described above, initializes global variables per the dynamic linking
//! tool convention, and calls any static constructors and/or link-time fixup functions
//!
//! `Linker` also supports synthesizing `dlopen`/`dlsym` lookup tables which allow symbols to be resolved at
//! runtime.  Note that this is not true dynamic linking, since all the code is baked into the component ahead of
//! time -- we simply allow runtime resolution of already-resident definitions.  This is sufficient to support
//! dynamic language FFI features such as Python native extensions, provided the required libraries are linked
//! ahead-of-time.

use {
    crate::encoding::{ComponentEncoder, Instance, Item, LibraryInfo, MainOrAdapter},
    anyhow::{Context, Result, anyhow, bail},
    indexmap::{IndexMap, IndexSet, map::Entry},
    metadata::{Export, ExportKey, FunctionType, GlobalType, Metadata, Type, ValueType},
    std::{
        collections::{BTreeMap, HashMap, HashSet},
        fmt::Debug,
        hash::Hash,
        iter,
    },
    wasm_encoder::{
        CodeSection, ConstExpr, DataSection, ElementSection, Elements, EntityType, ExportKind,
        ExportSection, Function, FunctionSection, GlobalSection, ImportSection, MemArg,
        MemorySection, MemoryType, Module, RawCustomSection, RefType, StartSection, TableSection,
        TableType, TypeSection, ValType,
    },
    wasmparser::SymbolFlags,
};

mod metadata;

const PAGE_SIZE_BYTES: u32 = 65536;
// This matches the default stack size LLVM produces:
pub const DEFAULT_STACK_SIZE_BYTES: u32 = 16 * PAGE_SIZE_BYTES;
const HEAP_ALIGNMENT_BYTES: u32 = 16;
const STUB_LIBRARY_NAME: &str = "wit-component:stubs";
const CABI_REALLOC: &str = "cabi_realloc";

static EMPTY_FUNCTION_TYPE: FunctionType = FunctionType {
    parameters: Vec::new(),
    results: Vec::new(),
};

/// Symbols to re-export from the `env` module regardless of whether any
/// libraries import them, since
/// `EncodingState::create_export_task_initialization_wrappers` needs to be able
/// to call them.
static ENV_REEXPORTS: &[&str] = &[metadata::INIT_TASK, metadata::INIT_ASYNC_TASK];

enum Address<'a> {
    Function(u32),
    Global(&'a str),
}

/// Represents a `dlopen`/`dlsym` lookup table enabling runtime symbol resolution
///
/// The top level of this table is a sorted list of library names and offsets, each pointing to a sorted list of
/// symbol names and offsets.  See ../dl/src/lib.rs for how this is used at runtime.
struct DlOpenables<'a> {
    /// Offset into the main module's table where function references will be stored
    table_base: u32,

    /// Offset into the main module's memory where the lookup table will be stored
    memory_base: u32,

    /// The lookup table itself
    buffer: Vec<u8>,

    /// Linear memory addresses where global variable addresses will live
    ///
    /// The init module will fill in the correct values at instantiation time.
    global_addresses: Vec<(&'a str, &'a str, u32)>,

    /// Number of function references to be stored in the main module's table
    function_count: u32,

    /// Linear memory address where the root of the lookup table will reside
    ///
    /// This can be different from `memory_base` depending on how the tree of libraries and symbols is laid out in
    /// memory.
    libraries_address: u32,
}

impl<'a> DlOpenables<'a> {
    /// Construct a lookup table containing all "dlopen-able" libraries and their symbols using the specified table
    /// and memory offsets.
    fn new(table_base: u32, memory_base: u32, metadata: &'a [Metadata<'a>]) -> Self {
        let mut function_count = 0;
        let mut buffer = Vec::new();
        let mut global_addresses = Vec::new();
        let mut libraries = metadata
            .iter()
            .filter(|metadata| metadata.dl_openable)
            .map(|metadata| {
                let name_address = memory_base + u32::try_from(buffer.len()).unwrap();
                write_bytes_padded(&mut buffer, metadata.name.as_bytes());

                let mut symbols = metadata
                    .exports
                    .iter()
                    .filter_map(|export| {
                        let name_address = memory_base + u32::try_from(buffer.len()).unwrap();
                        write_bytes_padded(&mut buffer, export.key.name.as_bytes());

                        let address = match &export.key.ty {
                            Type::Function(_) => Address::Function(
                                table_base + get_and_increment(&mut function_count),
                            ),
                            Type::Global(_) => Address::Global(export.key.name),
                            Type::Tag(_) => return None,
                        };

                        Some((export.key.name, name_address, address))
                    })
                    .collect::<Vec<_>>();

                symbols.sort_by_key(|(name, ..)| *name);

                let start = buffer.len();
                for (name, name_address, address) in symbols {
                    write_u32(&mut buffer, u32::try_from(name.len()).unwrap());
                    write_u32(&mut buffer, name_address);
                    match address {
                        Address::Function(address) => write_u32(&mut buffer, address),
                        Address::Global(name) => {
                            global_addresses.push((
                                metadata.name,
                                name,
                                memory_base + u32::try_from(buffer.len()).unwrap(),
                            ));

                            write_u32(&mut buffer, 0);
                        }
                    }
                }

                (
                    metadata.name,
                    name_address,
                    metadata.exports.len(),
                    memory_base + u32::try_from(start).unwrap(),
                )
            })
            .collect::<Vec<_>>();

        libraries.sort_by_key(|(name, ..)| *name);

        let start = buffer.len();
        for (name, name_address, count, symbols) in &libraries {
            write_u32(&mut buffer, u32::try_from(name.len()).unwrap());
            write_u32(&mut buffer, *name_address);
            write_u32(&mut buffer, u32::try_from(*count).unwrap());
            write_u32(&mut buffer, *symbols);
        }

        let libraries_address = memory_base + u32::try_from(buffer.len()).unwrap();
        write_u32(&mut buffer, u32::try_from(libraries.len()).unwrap());
        write_u32(&mut buffer, memory_base + u32::try_from(start).unwrap());

        Self {
            table_base,
            memory_base,
            buffer,
            global_addresses,
            function_count,
            libraries_address,
        }
    }
}

/// The layout of the whole program's thread-local storage bookkeeping.
///
/// This generates a C structure that matches this layout:
///
/// ```c
/// struct {
///     size_t num_libraries;
///     void **library_info;
///     void **main_thread_tls_base;
/// } __wasm_program_tls_info;
/// ```
///
/// where the `main_thread_tls_base` array is placed first, then the
/// `library_info` array, then this structure itself.
#[derive(Default)]
struct TlsLayout {
    /// Address of the `main_thread_tls_base` array.
    ///
    /// This is left zero-initialized; no data segment covers it.
    main_thread_tls_base: u32,

    /// Address of the `library_info` array.
    library_info: u32,

    /// Address of the `__wasm_program_tls_info` struct itself.
    program_info: u32,

    /// For each library, the slot it uses in the array of TLS base pointers, or
    /// `None` if it has no thread-local storage of its own.
    slots: Vec<Option<u32>>,

    /// Static contents of the `__wasm_program_tls_info`.
    buffer: Vec<u8>,
}

impl TlsLayout {
    /// Reserve linear memory and table space for the layout described above,
    /// advancing `memory_offset` and `table_offset` past what's used.
    ///
    /// Nothing is reserved for a program which doesn't use thread-local storage
    /// at all, and the `__wasm_program_tls_info` half is skipped unless some
    /// library actually asks for it, which is only the case when the program
    /// might spawn a thread.
    fn new(metadata: &[Metadata], memory_offset: &mut u32) -> Self {
        let needs_tls_base = metadata
            .iter()
            .any(|m| m.needs_get_tls_base || m.needs_set_tls_base);
        let needs_program_info = metadata.iter().any(|m| m.needs_program_tls_info);
        if !needs_tls_base && !needs_program_info {
            return Self::default();
        }

        // Filter out libraries that don't have TLS.
        let libraries = metadata
            .iter()
            .enumerate()
            .filter(|(_, metadata)| metadata.has_library_tls_info)
            .map(|(index, _)| index)
            .collect::<Vec<_>>();

        let mut slots = vec![None; metadata.len()];
        for (slot, index) in libraries.iter().enumerate() {
            slots[*index] = Some(u32::try_from(slot).unwrap());
        }
        let count = u32::try_from(libraries.len()).unwrap();

        // Allocate space for `main_thread_tls_base`
        *memory_offset = align(*memory_offset, 4);
        let main_thread_tls_base = *memory_offset;
        *memory_offset += count * 4;

        let mut library_info = 0;
        let mut program_info = 0;
        let mut buffer = Vec::new();
        if needs_program_info {
            library_info = *memory_offset;
            *memory_offset += count * 4;
            program_info = *memory_offset;
            *memory_offset += 12;

            write_u32(&mut buffer, count);
            write_u32(&mut buffer, library_info);
            write_u32(&mut buffer, main_thread_tls_base);
        }

        Self {
            main_thread_tls_base,
            library_info,
            program_info,
            slots,
            buffer,
        }
    }

    /// The slot library `index` uses in the array of TLS base pointers.
    fn slot(&self, index: usize) -> Option<u32> {
        self.slots.get(index).copied().flatten()
    }
}

fn write_u32(buffer: &mut Vec<u8>, value: u32) {
    buffer.extend(value.to_le_bytes());
}

fn write_bytes_padded(buffer: &mut Vec<u8>, bytes: &[u8]) {
    buffer.extend(bytes);

    let len = u32::try_from(bytes.len()).unwrap();
    for _ in len..align(len, 4) {
        buffer.push(0);
    }
}

fn align(a: u32, b: u32) -> u32 {
    assert!(b.is_power_of_two());
    (a + (b - 1)) & !(b - 1)
}

fn get_and_increment(n: &mut u32) -> u32 {
    let v = *n;
    *n += 1;
    v
}

fn const_u32(a: u32) -> ConstExpr {
    ConstExpr::i32_const(a as i32)
}

/// Helper trait for determining the size of a set or map
trait Length {
    fn len(&self) -> usize;
}

impl<T> Length for HashSet<T> {
    fn len(&self) -> usize {
        HashSet::len(self)
    }
}

impl<K, V> Length for HashMap<K, V> {
    fn len(&self) -> usize {
        HashMap::len(self)
    }
}

impl<T> Length for IndexSet<T> {
    fn len(&self) -> usize {
        IndexSet::len(self)
    }
}

impl<K, V> Length for IndexMap<K, V> {
    fn len(&self) -> usize {
        IndexMap::len(self)
    }
}

/// Extension trait for collecting into a set or map and asserting that there were no duplicate entries in the
/// source iterator.
trait CollectUnique: Iterator + Sized {
    fn collect_unique<T: FromIterator<Self::Item> + Length>(self) -> T {
        let tmp = self.collect::<Vec<_>>();
        let len = tmp.len();
        let result = tmp.into_iter().collect::<T>();
        assert!(
            result.len() == len,
            "one or more duplicate items detected when collecting into set or map"
        );
        result
    }
}

impl<T: Iterator> CollectUnique for T {}

/// Extension trait for inserting into a map and asserting that an entry did not already exist for the key
trait InsertUnique {
    type Key;
    type Value;

    fn insert_unique(&mut self, k: Self::Key, v: Self::Value);
}

impl<K: Hash + Eq + PartialEq + Debug, V: Debug> InsertUnique for HashMap<K, V> {
    type Key = K;
    type Value = V;

    fn insert_unique(&mut self, k: Self::Key, v: Self::Value) {
        if let Some(old_v) = self.get(&k) {
            panic!(
                "duplicate item inserted into map for key {k:?} (old value: {old_v:?}; new value: {v:?})"
            );
        }
        self.insert(k, v);
    }
}

/// Synthesize the "main" module for the component, responsible for exporting functions which break cyclic
/// dependencies, as well as hosting the memory and function table.
fn make_env_module<'a>(
    metadata: &'a [Metadata<'a>],
    env_exports: &[EnvExport<'_>],
    cabi_realloc_exporter: Option<&str>,
    stack_size_bytes: u32,
) -> (Vec<u8>, DlOpenables<'a>, TlsLayout, u32) {
    // TODO: deduplicate types
    let mut types = TypeSection::new();
    let mut imports = ImportSection::new();
    let mut import_map = IndexMap::new();
    let mut function_count = 0;
    let mut global_offset = 0;
    let mut wasi_start = None;

    for metadata in metadata {
        for import in &metadata.imports {
            if let Entry::Vacant(entry) = import_map.entry(import) {
                imports.import(
                    import.module,
                    import.name,
                    match &import.ty {
                        Type::Function(ty) => {
                            let index = get_and_increment(&mut function_count);
                            entry.insert(index);
                            types.ty().function(
                                ty.parameters.iter().copied().map(ValType::from),
                                ty.results.iter().copied().map(ValType::from),
                            );
                            EntityType::Function(index)
                        }
                        Type::Global(ty) => {
                            entry.insert(get_and_increment(&mut global_offset));
                            EntityType::Global(wasm_encoder::GlobalType {
                                val_type: ty.ty.into(),
                                mutable: ty.mutable,
                                shared: ty.shared,
                            })
                        }
                        Type::Tag(_) => continue,
                    },
                );
            }
        }

        if metadata.has_wasi_start {
            if wasi_start.is_some() {
                panic!("multiple libraries export {}", metadata::START);
            }
            let index = get_and_increment(&mut function_count);

            types.ty().function(vec![], vec![]);
            imports.import(metadata.name, metadata::START, EntityType::Function(index));

            wasi_start = Some(index);
        }
    }

    let mut memory_offset = stack_size_bytes;

    // Table offset 0 is reserved for the null function pointer.
    // This convention follows wasm-ld's table layout:
    // https://github.com/llvm/llvm-project/blob/913622d012f72edb5ac3a501cef8639d0ebe471b/lld/wasm/Driver.cpp#L581-L584
    let mut table_offset = 1;
    let mut globals = GlobalSection::new();
    let mut exports = ExportSection::new();

    if let Some(exporter) = cabi_realloc_exporter {
        let index = get_and_increment(&mut function_count);
        types.ty().function([ValType::I32; 4], [ValType::I32]);
        imports.import(exporter, CABI_REALLOC, EntityType::Function(index));
        exports.export(CABI_REALLOC, ExportKind::Func, index);
    }

    // If tls base shims are being generated, and something might spawn a
    // thread, then the shims generated will need access to `context.get 1`.
    let indirect_tls_base = metadata
        .iter()
        .any(|m| m.needs_get_tls_base || m.needs_set_tls_base)
        && metadata.iter().any(|m| m.uses_thread_new_indirect);
    let tls_context_get = if indirect_tls_base {
        let index = get_and_increment(&mut function_count);
        types.ty().function([], [ValType::I32]);
        imports.import(
            metadata::ROOT,
            metadata::CONTEXT_GET_1,
            EntityType::Function(index),
        );
        Some(index)
    } else {
        None
    };

    let mut add_global_export = |name: &str, value, mutable| {
        let index = globals.len();
        globals.global(
            wasm_encoder::GlobalType {
                val_type: ValType::I32,
                mutable,
                shared: false,
            },
            &const_u32(value),
        );
        exports.export(name, ExportKind::Global, index);
    };

    let dl_openables = DlOpenables::new(table_offset, memory_offset, metadata);

    if metadata.iter().any(|m| m.needs_libdl_libraries) {
        add_global_export(
            metadata::LIBDL_LIBRARIES,
            dl_openables.libraries_address,
            true,
        );
    }

    table_offset += dl_openables.function_count;
    memory_offset += u32::try_from(dl_openables.buffer.len()).unwrap();

    let tls = TlsLayout::new(metadata, &mut memory_offset);

    if metadata.iter().any(|m| m.needs_program_tls_info) {
        add_global_export(metadata::PROGRAM_TLS_INFO, tls.program_info, true);
    }

    let memory_size = {
        if metadata.iter().any(|m| m.needs_stack_pointer) {
            add_global_export(metadata::STACK_POINTER, stack_size_bytes, true);
        }
        if metadata.iter().any(|m| m.needs_init_stack_pointer) {
            add_global_export(metadata::INIT_STACK_POINTER, stack_size_bytes, false);
        }

        // Binaryen's Asyncify transform for shared everything linking requires these globals
        // to be provided from env module
        let has_asyncified_module = metadata.iter().any(|m| m.is_asyncified);
        if has_asyncified_module {
            add_global_export(metadata::ASYNCIFY_STATE, 0, true);
            add_global_export(metadata::ASYNCIFY_DATA, 0, true);
        }

        // The libc.so in WASI-SDK 28+ requires these:
        if metadata.iter().any(|m| m.needs_stack_high) {
            add_global_export(metadata::STACK_HIGH, stack_size_bytes, true);
        }
        if metadata.iter().any(|m| m.needs_stack_low) {
            add_global_export(metadata::STACK_LOW, 0, true);
        }

        for metadata in metadata {
            memory_offset = align(memory_offset, 1 << metadata.mem_info.memory_alignment);
            table_offset = align(table_offset, 1 << metadata.mem_info.table_alignment);

            add_global_export(
                &format!("{}:memory_base", metadata.name),
                memory_offset,
                false,
            );
            add_global_export(
                &format!("{}:table_base", metadata.name),
                table_offset,
                false,
            );

            memory_offset += metadata.mem_info.memory_size;
            table_offset += metadata.mem_info.table_size;

            for import in &metadata.memory_address_imports {
                // Note that we initialize this to zero and let the init module compute the real value at
                // instantiation time.
                add_global_export(&format!("{}:{import}", metadata.name), 0, true);
            }
        }

        {
            let offsets = env_exports
                .iter()
                .enumerate()
                .map(|(offset, EnvExport { name, exporter, .. })| {
                    (
                        *name,
                        (
                            table_offset + u32::try_from(offset).unwrap(),
                            metadata[*exporter].name == STUB_LIBRARY_NAME,
                        ),
                    )
                })
                .collect_unique::<HashMap<_, _>>();

            for metadata in metadata {
                for import in &metadata.table_address_imports {
                    let &(offset, is_stub) = offsets.get(import).unwrap();
                    if is_stub
                        && metadata
                            .env_imports
                            .iter()
                            .any(|e| e.0 == *import && e.1.1.contains(SymbolFlags::BINDING_WEAK))
                    {
                        add_global_export(&format!("{}:{import}", metadata.name), 0, true);
                    } else {
                        add_global_export(&format!("{}:{import}", metadata.name), offset, true);
                    }
                }
            }
        }

        memory_offset = align(memory_offset, HEAP_ALIGNMENT_BYTES);
        if metadata.iter().any(|m| m.needs_heap_base) {
            add_global_export(metadata::HEAP_BASE, memory_offset, true);
        }

        let heap_end = align(memory_offset, PAGE_SIZE_BYTES);
        if metadata.iter().any(|m| m.needs_heap_end) {
            add_global_export(metadata::HEAP_END, heap_end, true);
        }
        heap_end / PAGE_SIZE_BYTES
    };

    let indirection_table_base = table_offset;

    let mut functions = FunctionSection::new();
    let mut code = CodeSection::new();
    for export in env_exports {
        let index = get_and_increment(&mut function_count);
        types.ty().function(
            export.ty.parameters.iter().copied().map(ValType::from),
            export.ty.results.iter().copied().map(ValType::from),
        );
        functions.function(u32::try_from(index).unwrap());
        let mut function = Function::new([]);
        for local in 0..export.ty.parameters.len() {
            function
                .instructions()
                .local_get(u32::try_from(local).unwrap());
        }
        function
            .instructions()
            .i32_const(i32::try_from(table_offset).unwrap())
            .call_indirect(0, u32::try_from(index).unwrap())
            .end();
        code.function(&function);
        exports.export(export.name, ExportKind::Func, index);

        table_offset += 1;
    }

    // Define a distinct `__wasm_{get,set}_tls_base` pair for each library that
    // needs one. Each pair reads and writes that library's slot of the array of
    // pointers described by `TlsLayout`.
    for (index, metadata) in metadata.iter().enumerate() {
        // A library with no thread-local storage of its own has no slot in the
        // array. If it still imports these then synthesize functions that trap
        // since they shouldn't ever be called.
        let Some(slot) = tls.slot(index) else {
            for (needed, name, params, results) in [
                (
                    metadata.needs_get_tls_base,
                    metadata::GET_TLS_BASE,
                    &[][..],
                    &[ValType::I32][..],
                ),
                (
                    metadata.needs_set_tls_base,
                    metadata::SET_TLS_BASE,
                    &[ValType::I32][..],
                    &[][..],
                ),
            ] {
                if !needed {
                    continue;
                }
                let func = get_and_increment(&mut function_count);
                types
                    .ty()
                    .function(params.iter().copied(), results.iter().copied());
                functions.function(func);
                let mut function = Function::new([]);
                function.instructions().unreachable().end();
                code.function(&function);
                exports.export(&format!("{}:{name}", metadata.name), ExportKind::Func, func);
            }
            continue;
        };

        let mem_arg = MemArg {
            offset: u64::from(slot * 4),
            align: 2,
            memory_index: 0,
        };

        if metadata.needs_get_tls_base {
            let func = get_and_increment(&mut function_count);
            types.ty().function([], [ValType::I32]);
            functions.function(func);
            let mut function = Function::new([]);
            // With coop threads the base pointer is in `context.get 1`. Without
            // coop threads the base pointer is `main_thread_tls_base` itself.
            match tls_context_get {
                Some(get) => {
                    function.instructions().call(get);
                }
                None => {
                    function
                        .instructions()
                        .i32_const(i32::try_from(tls.main_thread_tls_base).unwrap());
                }
            }
            function.instructions().i32_load(mem_arg).end();
            code.function(&function);
            exports.export(
                &format!("{}:{}", metadata.name, metadata::GET_TLS_BASE),
                ExportKind::Func,
                func,
            );
        }

        if metadata.needs_set_tls_base {
            let func = get_and_increment(&mut function_count);
            types.ty().function([ValType::I32], []);
            functions.function(func);
            let mut function = Function::new_with_locals_types(if tls_context_get.is_some() {
                vec![ValType::I32]
            } else {
                vec![]
            });
            // With coop threads this intrinsic conditionally initializes
            // `main_thread_tls_base` based on `context.get 1`. Otherwise it
            // writes through to it if it's set.
            //
            // Without coop threads this is updating `main_thread_tls_base`.
            match tls_context_get {
                Some(get) => {
                    function
                        .instructions()
                        .call(get)
                        .local_tee(1)
                        .i32_eqz()
                        .if_(wasm_encoder::BlockType::Empty)
                        .i32_const(i32::try_from(tls.main_thread_tls_base).unwrap())
                        .local_get(0)
                        .i32_store(mem_arg)
                        .else_()
                        .local_get(1)
                        .local_get(0)
                        .i32_store(mem_arg)
                        .end()
                        .end();
                }
                None => {
                    function
                        .instructions()
                        .i32_const(i32::try_from(tls.main_thread_tls_base).unwrap())
                        .local_get(0)
                        .i32_store(mem_arg)
                        .end();
                }
            }
            code.function(&function);
            exports.export(
                &format!("{}:{}", metadata.name, metadata::SET_TLS_BASE),
                ExportKind::Func,
                func,
            );
        }
    }

    for (import, offset) in import_map {
        exports.export(
            &format!("{}:{}", import.module, import.name),
            ExportKind::from(&import.ty),
            offset,
        );
    }
    if let Some(index) = wasi_start {
        exports.export(metadata::START, ExportKind::Func, index);
    }

    let mut module = Module::new();

    module.section(&types);
    module.section(&imports);
    module.section(&functions);

    {
        let mut tables = TableSection::new();
        tables.table(TableType {
            element_type: RefType::FUNCREF,
            minimum: table_offset.into(),
            maximum: None,
            table64: false,
            shared: false,
        });
        exports.export(metadata::INDIRECT_FUNCTION_TABLE, ExportKind::Table, 0);
        module.section(&tables);
    }

    {
        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: u64::from(memory_size),
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        exports.export(metadata::MEMORY, ExportKind::Memory, 0);
        module.section(&memories);
    }

    module.section(&globals);
    module.section(&exports);
    module.section(&code);
    module.section(&RawCustomSection(
        &crate::base_producers().raw_custom_section(),
    ));

    let module = module.finish();
    wasmparser::validate(&module).unwrap();

    (module, dl_openables, tls, indirection_table_base)
}

/// Synthesize the "init" module, responsible for initializing global variables per the dynamic linking tool
/// convention and calling any static constructors and/or link-time fixup functions.
///
/// This module also contains the data segment for the `dlopen`/`dlsym` lookup table.
fn make_init_module(
    metadata: &[Metadata],
    exporters: &IndexMap<&ExportKey, (&str, &Export)>,
    env_exports: &[EnvExport<'_>],
    dl_openables: DlOpenables,
    tls: TlsLayout,
    indirection_table_base: u32,
) -> Result<Vec<u8>> {
    let mut module = Module::new();

    // TODO: deduplicate types
    let mut types = TypeSection::new();
    types.ty().function([], []);
    let thunk_ty = 0;
    types.ty().function([ValType::I32], []);
    let mut type_offset = 2;

    for metadata in metadata {
        if metadata.dl_openable {
            for export in &metadata.exports {
                if let Type::Function(ty) = &export.key.ty {
                    types.ty().function(
                        ty.parameters.iter().copied().map(ValType::from),
                        ty.results.iter().copied().map(ValType::from),
                    );
                }
            }
        }
    }
    for export in env_exports {
        types.ty().function(
            export.ty.parameters.iter().copied().map(ValType::from),
            export.ty.results.iter().copied().map(ValType::from),
        );
    }
    module.section(&types);

    let mut imports = ImportSection::new();
    imports.import(
        metadata::ENV,
        metadata::MEMORY,
        MemoryType {
            minimum: 0,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        },
    );
    imports.import(
        metadata::ENV,
        metadata::INDIRECT_FUNCTION_TABLE,
        TableType {
            element_type: RefType::FUNCREF,
            minimum: 0,
            maximum: None,
            table64: false,
            shared: false,
        },
    );

    let mut global_count = 0;
    let mut global_map = HashMap::new();
    let mut add_global_import = |imports: &mut ImportSection, module: &str, name: &str, mutable| {
        *global_map
            .entry((module.to_owned(), name.to_owned()))
            .or_insert_with(|| {
                imports.import(
                    module,
                    name,
                    wasm_encoder::GlobalType {
                        val_type: ValType::I32,
                        mutable,
                        shared: false,
                    },
                );
                get_and_increment(&mut global_count)
            })
    };

    let mut function_count = 0;
    let mut function_map = HashMap::new();
    let mut add_function_import = |imports: &mut ImportSection, module: &str, name: &str, ty| {
        *function_map
            .entry((module.to_owned(), name.to_owned()))
            .or_insert_with(|| {
                imports.import(module, name, EntityType::Function(ty));
                get_and_increment(&mut function_count)
            })
    };

    let mut start = Function::new([]);

    let mut names = HashMap::new();
    for (index, metadata) in metadata.iter().enumerate() {
        names.insert_unique(index, metadata.name);
    }

    for (exporter, export, address) in dl_openables.global_addresses.iter() {
        let memory_base = add_global_import(
            &mut imports,
            metadata::ENV,
            &format!("{exporter}:memory_base"),
            false,
        );
        let export = add_global_import(&mut imports, exporter, export, false);
        start
            .instructions()
            .i32_const(i32::try_from(*address).unwrap())
            .global_get(memory_base)
            .global_get(export)
            .i32_add()
            .i32_store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            });
    }

    for metadata in metadata {
        for import in &metadata.memory_address_imports {
            let (exporter, _) = find_offset_exporter(import, exporters)?;

            let memory_base = add_global_import(
                &mut imports,
                metadata::ENV,
                &format!("{exporter}:memory_base"),
                false,
            );
            let offset = add_global_import(&mut imports, exporter, import, false);
            let address = add_global_import(
                &mut imports,
                metadata::ENV,
                &format!("{}:{import}", metadata.name),
                true,
            );
            start
                .instructions()
                .global_get(memory_base)
                .global_get(offset)
                .i32_add()
                .global_set(address);
        }
    }

    for metadata in metadata {
        if metadata.has_data_relocs {
            let func = add_function_import(
                &mut imports,
                metadata.name,
                metadata::APPLY_DATA_RELOCS,
                thunk_ty,
            );
            start.instructions().call(func);
        }
    }

    let mut init_task_exporter = exporters
        .get(&ExportKey {
            name: metadata::INIT_TASK,
            ty: Type::Function(EMPTY_FUNCTION_TYPE.clone()),
        })
        .map(|(name, _)| name);

    for metadata in metadata {
        if metadata.has_ctors && metadata.has_initialize {
            bail!(
                "library {} exports both `{}` and `{}`; \
                 expected at most one of the two",
                metadata.name,
                metadata::CALL_CTORS,
                metadata::INITIALIZE
            );
        }

        // Before calling either `__wasm_call_ctors` or `_initialize`, we need
        // to call `__wasm_init_task` if present to set up the shadow stack when
        // using the cooperative multithreading ABI.
        if let (Some(exporter), true) = (
            init_task_exporter,
            metadata.has_ctors || metadata.has_initialize,
        ) {
            // We only need to call it at most once, so we set
            // `init_task_exporter` to `None` to avoid calling it again:
            init_task_exporter = None;

            let func = add_function_import(&mut imports, exporter, metadata::INIT_TASK, thunk_ty);
            start.instructions().call(func);
        }

        if metadata.has_ctors {
            let func =
                add_function_import(&mut imports, metadata.name, metadata::CALL_CTORS, thunk_ty);
            start.instructions().call(func);
        }

        if metadata.has_initialize {
            let func =
                add_function_import(&mut imports, metadata.name, metadata::INITIALIZE, thunk_ty);
            start.instructions().call(func);
        }
    }

    let mut dl_openable_functions = Vec::new();
    for metadata in metadata {
        if metadata.dl_openable {
            for export in &metadata.exports {
                if let Type::Function(_) = &export.key.ty {
                    dl_openable_functions.push(add_function_import(
                        &mut imports,
                        metadata.name,
                        export.key.name,
                        get_and_increment(&mut type_offset),
                    ));
                }
            }
        }
    }

    let indirections = env_exports
        .iter()
        .map(|EnvExport { name, exporter, .. }| {
            add_function_import(
                &mut imports,
                names[exporter],
                name,
                get_and_increment(&mut type_offset),
            )
        })
        .collect::<Vec<_>>();

    // For all libraries that export TLS information the initialization function
    // here will setup the in-memory `__wasm_program_tls_info` structure by
    // storing the pointers to each `__wasm_library_tls_info` structure in a
    // sequence.
    if tls.library_info != 0 {
        for (lib_index, tls_index) in tls
            .slots
            .iter()
            .enumerate()
            .filter_map(|(i, slot)| slot.map(|j| (i, j)))
        {
            let metadata = &metadata[lib_index];
            let memory_base = add_global_import(
                &mut imports,
                metadata::ENV,
                &format!("{}:memory_base", metadata.name),
                false,
            );
            let i = add_global_import(
                &mut imports,
                metadata.name,
                metadata::LIBRARY_TLS_INFO,
                false,
            );
            start
                .instructions()
                .i32_const((tls.library_info + tls_index * 4).cast_signed())
                .global_get(i)
                .global_get(memory_base)
                .i32_add()
                .i32_store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                });
        }
    }

    module.section(&imports);

    {
        let mut functions = FunctionSection::new();
        functions.function(thunk_ty);
        module.section(&functions);
    }

    module.section(&StartSection {
        function_index: function_count,
    });

    {
        let mut elements = ElementSection::new();
        elements.active(
            None,
            &const_u32(dl_openables.table_base),
            Elements::Functions(dl_openable_functions.into()),
        );
        elements.active(
            None,
            &const_u32(indirection_table_base),
            Elements::Functions(indirections.into()),
        );
        module.section(&elements);
    }

    {
        let mut code = CodeSection::new();
        start.instructions().end();
        code.function(&start);
        module.section(&code);
    }

    let mut data = DataSection::new();
    data.active(0, &const_u32(dl_openables.memory_base), dl_openables.buffer);
    if !tls.buffer.is_empty() {
        data.active(0, &const_u32(tls.program_info), tls.buffer);
    }
    module.section(&data);

    module.section(&RawCustomSection(
        &crate::base_producers().raw_custom_section(),
    ));

    let module = module.finish();
    wasmparser::validate(&module)?;

    Ok(module)
}

/// Find the library which exports the specified function or global address.
fn find_offset_exporter<'a>(
    name: &str,
    exporters: &IndexMap<&ExportKey, (&'a str, &'a Export<'a>)>,
) -> Result<(&'a str, &'a Export<'a>)> {
    let export = ExportKey {
        name,
        ty: Type::Global(GlobalType {
            ty: ValueType::I32,
            mutable: false,
            shared: false,
        }),
    };

    exporters
        .get(&export)
        .copied()
        .ok_or_else(|| anyhow!("unable to find {export:?} in any library"))
}

/// Find the library which exports the specified function.
fn find_function_exporter<'a>(
    name: &str,
    ty: &FunctionType,
    exporters: &IndexMap<&ExportKey, (&'a str, &'a Export<'a>)>,
) -> Result<(&'a str, &'a Export<'a>)> {
    let export = ExportKey {
        name,
        ty: Type::Function(ty.clone()),
    };

    exporters
        .get(&export)
        .copied()
        .ok_or_else(|| anyhow!("unable to find {export:?} in any library"))
}

/// Find the library which exports the specified tag.
fn find_tag_exporter<'a>(
    name: &str,
    ty: &FunctionType,
    exporters: &IndexMap<&ExportKey, (&'a str, &'a Export<'a>)>,
) -> Result<(&'a str, &'a Export<'a>)> {
    let export = ExportKey {
        name,
        ty: Type::Tag(ty.clone()),
    };

    exporters
        .get(&export)
        .copied()
        .ok_or_else(|| anyhow!("unable to find {export:?} in any library"))
}

/// Analyze the specified library metadata, producing a symbol-to-library-name map of exports.
fn resolve_exporters<'a>(
    metadata: &'a [Metadata<'a>],
) -> Result<IndexMap<&'a ExportKey<'a>, Vec<(&'a str, &'a Export<'a>)>>> {
    let mut exporters = IndexMap::<_, Vec<_>>::new();
    for metadata in metadata {
        for export in &metadata.exports {
            exporters
                .entry(&export.key)
                .or_default()
                .push((metadata.name, export));
        }
    }
    Ok(exporters)
}

/// Match up all imported symbols to their corresponding exports, reporting any missing or duplicate symbols.
fn resolve_symbols<'a>(
    metadata: &'a [Metadata<'a>],
    exporters: &'a IndexMap<&'a ExportKey<'a>, Vec<(&'a str, &'a Export<'a>)>>,
) -> (
    IndexMap<&'a ExportKey<'a>, (&'a str, &'a Export<'a>)>,
    Vec<(&'a str, Export<'a>)>,
    Vec<(&'a str, &'a ExportKey<'a>, &'a [(&'a str, &'a Export<'a>)])>,
) {
    let function_exporters = exporters
        .iter()
        .filter_map(|(export, exporters)| match &export.ty {
            Type::Function(_) => Some((export.name, (export, exporters))),
            Type::Global(_) | Type::Tag(_) => None,
        })
        .collect_unique::<IndexMap<_, _>>();

    let mut resolved = IndexMap::new();
    let mut missing = Vec::new();
    let mut duplicates = Vec::new();

    let mut triage = |metadata: &'a Metadata, export: Export<'a>| {
        if let Some((key, value)) = exporters.get_key_value(&export.key) {
            // Note that we do not use `insert_unique` here since multiple libraries may import the same
            // symbol, in which case we may redundantly insert the same value.
            match value.as_slice() {
                [] => unreachable!(),
                [exporter] => {
                    resolved.insert(*key, *exporter);
                }
                [exporter, ..] => {
                    resolved.insert(*key, *exporter);
                    duplicates.push((metadata.name, *key, value.as_slice()));
                }
            }
        } else {
            missing.push((metadata.name, export));
        }
    };

    for metadata in metadata {
        for (name, (ty, flags)) in &metadata.env_imports {
            triage(
                metadata,
                Export {
                    key: ExportKey {
                        name,
                        ty: Type::Function(ty.clone()),
                    },
                    flags: *flags,
                },
            );
        }

        for name in &metadata.memory_address_imports {
            triage(
                metadata,
                Export {
                    key: ExportKey {
                        name,
                        ty: Type::Global(GlobalType {
                            ty: ValueType::I32,
                            mutable: false,
                            shared: false,
                        }),
                    },
                    flags: SymbolFlags::empty(),
                },
            );
        }

        for (name, ty) in &metadata.tag_imports {
            triage(
                metadata,
                Export {
                    key: ExportKey {
                        name,
                        ty: Type::Tag(ty.clone()),
                    },
                    flags: SymbolFlags::empty(),
                },
            );
        }
    }

    for metadata in metadata {
        for name in &metadata.table_address_imports {
            if let Some((key, value)) = function_exporters.get(name) {
                // Note that we do not use `insert_unique` here since multiple libraries may import the same
                // symbol, in which case we may redundantly insert the same value.
                match value.as_slice() {
                    [] => unreachable!(),
                    [exporter] => {
                        resolved.insert(key, *exporter);
                    }
                    [exporter, ..] => {
                        resolved.insert(key, *exporter);
                        duplicates.push((metadata.name, *key, value.as_slice()));
                    }
                }
            } else if metadata.env_imports.iter().any(|(n, _)| n == name) {
                // GOT entry for a function which is imported from the env module, but not exported by any library,
                // already handled above.
            } else {
                missing.push((
                    metadata.name,
                    Export {
                        key: ExportKey {
                            name,
                            ty: Type::Function(FunctionType {
                                parameters: Vec::new(),
                                results: Vec::new(),
                            }),
                        },
                        flags: SymbolFlags::empty(),
                    },
                ));
            }
        }
    }

    // Even if no library imports these symbols, we re-export them from the
    // `env` module so that
    // `EncodingState::create_export_task_initialization_wrappers` can find
    // and use them:
    for &name in ENV_REEXPORTS {
        let export = Export {
            key: ExportKey {
                name,
                ty: Type::Function(EMPTY_FUNCTION_TYPE.clone()),
            },
            flags: SymbolFlags::empty(),
        };

        if let Some((key, value)) = exporters.get_key_value(&export.key) {
            // Note that we do not use `insert_unique` here since multiple
            // libraries may import the same symbol, in which case we may
            // redundantly insert the same value.
            match value.as_slice() {
                [] => unreachable!(),
                [exporter] | [exporter, ..] => {
                    resolved.insert(*key, *exporter);
                }
            }
        }
    }

    (resolved, missing, duplicates)
}

/// Recursively add a library (represented by its offset) and its dependency to the specified set, maintaining
/// topological order (modulo cycles).
fn topo_add(
    sorted: &mut IndexSet<usize>,
    dependencies: &IndexMap<usize, IndexSet<usize>>,
    element: usize,
) {
    let empty = &IndexSet::new();
    let deps = dependencies.get(&element).unwrap_or(empty);

    // First, add any dependencies which do not depend on `element`
    for &dep in deps {
        if !(sorted.contains(&dep) || dependencies.get(&dep).unwrap_or(empty).contains(&element)) {
            topo_add(sorted, dependencies, dep);
        }
    }

    // Next, add the element
    sorted.insert(element);

    // Finally, add any dependencies which depend on `element`
    for &dep in deps {
        if !sorted.contains(&dep) && dependencies.get(&dep).unwrap_or(empty).contains(&element) {
            topo_add(sorted, dependencies, dep);
        }
    }
}

/// Topologically sort a set of libraries (represented by their offsets) according to their dependencies, modulo
/// cycles.
fn topo_sort(count: usize, dependencies: &IndexMap<usize, IndexSet<usize>>) -> Result<Vec<usize>> {
    let mut sorted = IndexSet::new();
    for index in 0..count {
        topo_add(&mut sorted, &dependencies, index);
    }

    Ok(sorted.into_iter().collect())
}

/// Analyze the specified library metadata, producing a map of transitive dependencies, where each library is
/// represented by its offset in the original metadata slice.
fn find_dependencies(
    metadata: &[Metadata],
    exporters: &IndexMap<&ExportKey, (&str, &Export)>,
) -> Result<IndexMap<usize, IndexSet<usize>>> {
    // First, generate a map of direct dependencies (i.e. depender to dependees)
    let mut dependencies = IndexMap::<_, IndexSet<_>>::new();
    let mut indexes = HashMap::new();
    for (index, metadata) in metadata.iter().enumerate() {
        indexes.insert_unique(metadata.name, index);
        for &needed in &metadata.needed_libs {
            dependencies
                .entry(metadata.name)
                .or_default()
                .insert(needed);
        }
        for (import_name, (ty, _)) in &metadata.env_imports {
            dependencies
                .entry(metadata.name)
                .or_default()
                .insert(find_function_exporter(import_name, ty, exporters)?.0);
        }
    }

    // Next, convert the map from names to offsets
    let mut dependencies = dependencies
        .into_iter()
        .map(|(k, v)| {
            (
                indexes[k],
                v.into_iter()
                    .map(|v| indexes[v])
                    .collect_unique::<IndexSet<_>>(),
            )
        })
        .collect_unique::<IndexMap<_, _>>();

    // Finally, add all transitive dependencies to the map in a fixpoint loop, exiting when no new dependencies are
    // discovered.
    let empty = &IndexSet::new();

    loop {
        let mut new = IndexMap::<_, IndexSet<_>>::new();
        for (index, exporters) in &dependencies {
            for exporter in exporters {
                for exporter in dependencies.get(exporter).unwrap_or(empty) {
                    if !exporters.contains(exporter) {
                        new.entry(*index).or_default().insert(*exporter);
                    }
                }
            }
        }

        if new.is_empty() {
            break Ok(dependencies);
        } else {
            for (index, exporters) in new {
                dependencies.entry(index).or_default().extend(exporters);
            }
        }
    }
}

struct EnvExports<'a> {
    exports: Vec<EnvExport<'a>>,
    reexport_cabi_realloc: bool,
}

struct EnvExport<'a> {
    name: &'a str,
    ty: &'a FunctionType,
    exporter: usize,
}

/// Analyze the specified metadata and generate what needs to be exported from
/// the main (aka "env") module.
///
/// This includes a list of functions which should be re-exported as a
/// `call.indirect`-based function including the offset of the library
/// containing the original export.
///
/// Additionally this includes any tags necessary that are shared amongst
/// modules.
fn env_exports<'a>(
    metadata: &'a [Metadata<'a>],
    exporters: &'a IndexMap<&'a ExportKey, (&'a str, &Export)>,
    topo_sorted: &[usize],
) -> Result<EnvExports<'a>> {
    let function_exporters = exporters
        .iter()
        .filter_map(|(export, exporter)| {
            if let Type::Function(ty) = &export.ty {
                Some((export.name, (ty, *exporter)))
            } else {
                None
            }
        })
        .collect_unique::<HashMap<_, _>>();

    let indexes = metadata
        .iter()
        .enumerate()
        .map(|(index, metadata)| (metadata.name, index))
        .collect_unique::<HashMap<_, _>>();

    let mut result = Vec::new();
    let mut exported = HashSet::new();
    let mut seen = HashSet::new();

    for &index in topo_sorted {
        let metadata = &metadata[index];

        for name in &metadata.table_address_imports {
            if !exported.contains(name) {
                let (ty, (exporter, _)) = function_exporters
                    .get(name)
                    .ok_or_else(|| anyhow!("unable to find {name:?} in any library"))?;

                result.push(EnvExport {
                    name: *name,
                    ty: *ty,
                    exporter: indexes[exporter],
                });
                exported.insert(*name);
            }
        }

        for (import_name, (ty, _)) in &metadata.env_imports {
            if !exported.contains(import_name) {
                let exporter = indexes[find_function_exporter(import_name, ty, exporters)
                    .unwrap()
                    .0];
                if !seen.contains(&exporter) {
                    result.push(EnvExport {
                        name: *import_name,
                        ty,
                        exporter,
                    });
                    exported.insert(*import_name);
                }
            }
        }

        seen.insert(index);
    }

    // Even if no library imports these symbols, we re-export them from the
    // `env` module so that
    // `EncodingState::create_export_task_initialization_wrappers` can find
    // and use them:
    for &name in ENV_REEXPORTS {
        if !exported.contains(name) {
            if let Some(exporter) = exporters.get(&ExportKey {
                name,
                ty: Type::Function(EMPTY_FUNCTION_TYPE.clone()),
            }) {
                result.push(EnvExport {
                    name,
                    ty: &EMPTY_FUNCTION_TYPE,
                    exporter: indexes[exporter.0],
                });
                exported.insert(name);
            }
        }
    }

    let reexport_cabi_realloc = exported.contains(CABI_REALLOC);

    Ok(EnvExports {
        exports: result,
        reexport_cabi_realloc,
    })
}

/// Synthesize a module which contains trapping stub exports for the specified functions.
fn make_stubs_module(missing: &[(&str, Export)]) -> Vec<u8> {
    let mut types = TypeSection::new();
    let mut exports = ExportSection::new();
    let mut functions = FunctionSection::new();
    let mut code = CodeSection::new();
    for (offset, (_, export)) in missing.iter().enumerate() {
        let offset = u32::try_from(offset).unwrap();

        let Export {
            key:
                ExportKey {
                    name,
                    ty: Type::Function(ty),
                },
            ..
        } = export
        else {
            unreachable!();
        };

        types.ty().function(
            ty.parameters.iter().copied().map(ValType::from),
            ty.results.iter().copied().map(ValType::from),
        );
        functions.function(offset);
        let mut function = Function::new([]);
        function.instructions().unreachable().end();
        code.function(&function);
        exports.export(name, ExportKind::Func, offset);
    }

    let mut module = Module::new();

    module.section(&types);
    module.section(&functions);
    module.section(&exports);
    module.section(&code);
    module.section(&RawCustomSection(
        &crate::base_producers().raw_custom_section(),
    ));

    let module = module.finish();
    wasmparser::validate(&module).unwrap();

    module
}

/// Determine which of the specified libraries are transitively reachable at runtime, i.e. reachable from a
/// component export or via `dlopen`.
fn find_reachable<'a>(
    metadata: &'a [Metadata<'a>],
    dependencies: &IndexMap<usize, IndexSet<usize>>,
) -> IndexSet<&'a str> {
    let reachable = metadata
        .iter()
        .enumerate()
        .filter_map(|(index, metadata)| {
            if metadata.has_component_exports || metadata.dl_openable || metadata.has_wasi_start {
                Some(index)
            } else {
                None
            }
        })
        .collect_unique::<IndexSet<_>>();

    let empty = &IndexSet::new();

    reachable
        .iter()
        .chain(
            reachable
                .iter()
                .flat_map(|index| dependencies.get(index).unwrap_or(empty)),
        )
        .map(|&index| metadata[index].name)
        .collect()
}

/// Builder type for composing dynamic library modules into a component
#[derive(Default)]
pub struct Linker {
    /// The `(name, module, dl_openable)` triple representing the libraries to be composed
    ///
    /// The order of this list determines priority in cases where more than one library exports the same symbol.
    libraries: Vec<(String, Vec<u8>, bool)>,

    /// The set of adapters to use when generating the component
    adapters: Vec<(String, Vec<u8>)>,

    /// Whether to validate the resulting component prior to returning it
    validate: bool,

    /// Whether to generate trapping stubs for any unresolved imports
    stub_missing_functions: bool,

    /// Whether to use a built-in implementation of `dlopen`/`dlsym`.
    use_built_in_libdl: bool,

    /// Whether to generate debug `name` sections.
    debug_names: bool,

    /// Size of stack (in bytes) to allocate in the synthesized main module
    ///
    /// If `None`, use `DEFAULT_STACK_SIZE_BYTES`.
    stack_size: Option<u32>,

    /// This affects how when to WIT worlds are merged together, for example
    /// from two different libraries, whether their imports are unified when the
    /// semver version ranges for interface allow it.
    merge_imports_based_on_semver: Option<bool>,
}

impl Linker {
    /// Add a dynamic library module to this linker.
    ///
    /// If `dl_openable` is true, all of the library's exports will be added to the `dlopen`/`dlsym` lookup table
    /// for runtime resolution.
    pub fn library(mut self, name: &str, module: &[u8], dl_openable: bool) -> Result<Self> {
        self.libraries
            .push((name.to_owned(), module.to_vec(), dl_openable));

        Ok(self)
    }

    /// Add an adapter to this linker.
    ///
    /// See [crate::encoding::ComponentEncoder::adapter] for details.
    pub fn adapter(mut self, name: &str, module: &[u8]) -> Result<Self> {
        self.adapters.push((name.to_owned(), module.to_vec()));

        Ok(self)
    }

    /// Specify whether to validate the resulting component prior to returning it
    pub fn validate(mut self, validate: bool) -> Self {
        self.validate = validate;
        self
    }

    /// Specify size of stack to allocate in the synthesized main module
    pub fn stack_size(mut self, stack_size: u32) -> Self {
        self.stack_size = Some(stack_size);
        self
    }

    /// Specify whether to generate trapping stubs for any unresolved imports
    pub fn stub_missing_functions(mut self, stub_missing_functions: bool) -> Self {
        self.stub_missing_functions = stub_missing_functions;
        self
    }

    /// Specify whether to use a built-in implementation of `dlopen`/`dlsym`.
    pub fn use_built_in_libdl(mut self, use_built_in_libdl: bool) -> Self {
        self.use_built_in_libdl = use_built_in_libdl;
        self
    }

    /// Whether or not to generate debug name sections.
    pub fn debug_names(mut self, enable: bool) -> Self {
        self.debug_names = enable;
        self
    }

    /// This affects how when to WIT worlds are merged together, for example
    /// from two different libraries, whether their imports are unified when the
    /// semver version ranges for interface allow it.
    ///
    /// This is enabled by default.
    pub fn merge_imports_based_on_semver(mut self, merge: bool) -> Self {
        self.merge_imports_based_on_semver = Some(merge);
        self
    }

    /// Encode the component and return the bytes
    pub fn encode(mut self) -> Result<Vec<u8>> {
        if self.use_built_in_libdl {
            self.use_built_in_libdl = false;
            self = self.library("libdl.so", include_bytes!("../libdl.so"), false)?;
        }

        let adapter_names = self
            .adapters
            .iter()
            .map(|(name, _)| name.as_str())
            .collect_unique::<HashSet<_>>();

        if adapter_names.len() != self.adapters.len() {
            bail!("duplicate adapter name");
        }

        let metadata = self
            .libraries
            .iter()
            .map(|(name, module, dl_openable)| {
                Metadata::try_new(name, *dl_openable, module, &adapter_names)
                    .with_context(|| format!("failed to extract linking metadata from {name}"))
            })
            .collect::<Result<Vec<_>>>()?;

        {
            let names = self
                .libraries
                .iter()
                .map(|(name, ..)| name.as_str())
                .collect_unique::<HashSet<_>>();

            let missing = metadata
                .iter()
                .filter_map(|metadata| {
                    let missing = metadata
                        .needed_libs
                        .iter()
                        .copied()
                        .filter(|name| !names.contains(*name))
                        .collect::<Vec<_>>();

                    if missing.is_empty() {
                        None
                    } else {
                        Some((metadata.name, missing))
                    }
                })
                .collect::<Vec<_>>();

            if !missing.is_empty() {
                bail!(
                    "missing libraries:\n{}",
                    missing
                        .iter()
                        .map(|(needed_by, missing)| format!(
                            "\t{needed_by} needs {}",
                            missing.join(", ")
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                );
            }
        }

        let exporters = resolve_exporters(&metadata)?;

        let cabi_realloc_exporter = exporters
            .get(&ExportKey {
                name: "cabi_realloc",
                ty: Type::Function(FunctionType {
                    parameters: vec![ValueType::I32; 4],
                    results: vec![ValueType::I32],
                }),
            })
            .map(|exporters| exporters.first().unwrap().0);

        let (exporters, missing, _) = resolve_symbols(&metadata, &exporters);

        if !missing.is_empty() {
            if missing
                .iter()
                .all(|(_, export)| matches!(&export.key.ty, Type::Function(_)))
                && (self.stub_missing_functions
                    || missing
                        .iter()
                        .all(|(_, export)| export.flags.contains(SymbolFlags::BINDING_WEAK)))
            {
                self.stub_missing_functions = false;
                self.libraries
                    .push((STUB_LIBRARY_NAME.into(), make_stubs_module(&missing), false));
                return self.encode();
            } else {
                bail!(
                    "unresolved symbol(s):\n{}",
                    missing
                        .iter()
                        .filter(|(_, export)| !export.flags.contains(SymbolFlags::BINDING_WEAK))
                        .map(|(importer, export)| { format!("\t{importer} needs {}", export.key) })
                        .collect::<Vec<_>>()
                        .join("\n")
                );
            }
        }

        let dependencies = find_dependencies(&metadata, &exporters)?;

        {
            let reachable = find_reachable(&metadata, &dependencies);
            let unreachable = self
                .libraries
                .iter()
                .filter_map(|(name, ..)| (!reachable.contains(name.as_str())).then(|| name.clone()))
                .collect_unique::<HashSet<_>>();

            if !unreachable.is_empty() {
                self.libraries
                    .retain(|(name, ..)| !unreachable.contains(name));
                return self.encode();
            }
        }

        let topo_sorted = topo_sort(metadata.len(), &dependencies)?;

        let EnvExports {
            exports: env_exports,
            reexport_cabi_realloc,
        } = env_exports(&metadata, &exporters, &topo_sorted)?;

        let (env_module, dl_openables, tls, table_base) = make_env_module(
            &metadata,
            &env_exports,
            if reexport_cabi_realloc {
                // If "env" module already reexports "cabi_realloc", we don't need to
                // reexport it again.
                None
            } else {
                cabi_realloc_exporter
            },
            self.stack_size.unwrap_or(DEFAULT_STACK_SIZE_BYTES),
        );

        let mut encoder = ComponentEncoder::default()
            .validate(self.validate)
            .debug_names(self.debug_names);
        if let Some(merge) = self.merge_imports_based_on_semver {
            encoder = encoder.merge_imports_based_on_semver(merge);
        };
        encoder = encoder.module(&env_module)?;

        for (name, module) in &self.adapters {
            encoder = encoder.adapter(name, module)?;
        }

        let default_env_items = [
            Item {
                alias: metadata::MEMORY.into(),
                kind: ExportKind::Memory,
                which: MainOrAdapter::Main,
                name: metadata::MEMORY.into(),
            },
            Item {
                alias: metadata::INDIRECT_FUNCTION_TABLE.into(),
                kind: ExportKind::Table,
                which: MainOrAdapter::Main,
                name: metadata::INDIRECT_FUNCTION_TABLE.into(),
            },
            Item {
                alias: metadata::STACK_POINTER.into(),
                kind: ExportKind::Global,
                which: MainOrAdapter::Main,
                name: metadata::STACK_POINTER.into(),
            },
            Item {
                alias: metadata::INIT_STACK_POINTER.into(),
                kind: ExportKind::Global,
                which: MainOrAdapter::Main,
                name: metadata::INIT_STACK_POINTER.into(),
            },
        ];

        let mut seen = HashSet::new();
        for index in topo_sorted {
            let (name, module, _) = &self.libraries[index];
            let metadata = &metadata[index];

            let env_items = default_env_items
                .iter()
                .cloned()
                .chain([
                    Item {
                        alias: metadata::MEMORY_BASE.into(),
                        kind: ExportKind::Global,
                        which: MainOrAdapter::Main,
                        name: format!("{name}:memory_base"),
                    },
                    Item {
                        alias: metadata::TABLE_BASE.into(),
                        kind: ExportKind::Global,
                        which: MainOrAdapter::Main,
                        name: format!("{name}:table_base"),
                    },
                ])
                .chain(
                    [
                        (metadata.needs_get_tls_base, metadata::GET_TLS_BASE),
                        (metadata.needs_set_tls_base, metadata::SET_TLS_BASE),
                    ]
                    .into_iter()
                    .filter(|(needed, _)| *needed)
                    .map(|(_, intrinsic)| Item {
                        alias: intrinsic.into(),
                        kind: ExportKind::Func,
                        which: MainOrAdapter::Main,
                        name: format!("{name}:{intrinsic}"),
                    }),
                )
                .chain(metadata.env_imports.iter().map(|(name, (ty, _))| {
                    let (exporter, _) = find_function_exporter(name, ty, &exporters).unwrap();

                    Item {
                        alias: (*name).into(),
                        kind: ExportKind::Func,
                        which: if seen.contains(exporter) {
                            MainOrAdapter::Adapter(exporter.to_owned())
                        } else {
                            MainOrAdapter::Main
                        },
                        name: (*name).into(),
                    }
                }))
                .chain(
                    metadata
                        .tag_imports
                        .iter()
                        .map(|(name, ty)| {
                            let (exporter, _) = find_tag_exporter(name, ty, &exporters).unwrap();

                            Ok(Item {
                                alias: (*name).into(),
                                kind: ExportKind::Tag,
                                which: if seen.contains(exporter) {
                                    MainOrAdapter::Adapter(exporter.to_owned())
                                } else {
                                    // As of this writing, LLVM-produced shared
                                    // libraries which use C++ exceptions import
                                    // a `cpp_exception` tag which is defined in
                                    // `libunwind.so`.  Presumably
                                    // `libunwind.so` will not import anything
                                    // circularly from such shared libraries, so
                                    // this case shouldn't be hit in practice
                                    // unless we're dealing with some other,
                                    // non-LLVM toolchain that does weird
                                    // circular things with imports and
                                    // exception tags.
                                    bail!(
                                        "circular dependency prevents direct tag import from `{}`",
                                        exporter.to_owned()
                                    )
                                },
                                name: (*name).into(),
                            })
                        })
                        .collect::<Result<Vec<_>>>()?,
                )
                .chain(if metadata.is_asyncified {
                    vec![
                        Item {
                            alias: metadata::ASYNCIFY_STATE.into(),
                            kind: ExportKind::Global,
                            which: MainOrAdapter::Main,
                            name: metadata::ASYNCIFY_STATE.into(),
                        },
                        Item {
                            alias: metadata::ASYNCIFY_DATA.into(),
                            kind: ExportKind::Global,
                            which: MainOrAdapter::Main,
                            name: metadata::ASYNCIFY_DATA.into(),
                        },
                    ]
                } else {
                    vec![]
                })
                .collect();

            let global_item = |address_name: &str| Item {
                alias: address_name.into(),
                kind: ExportKind::Global,
                which: MainOrAdapter::Main,
                name: format!("{name}:{address_name}"),
            };

            let mem_items = metadata
                .memory_address_imports
                .iter()
                .copied()
                .map(global_item)
                .chain(
                    [
                        metadata::HEAP_BASE,
                        metadata::HEAP_END,
                        metadata::STACK_HIGH,
                        metadata::STACK_LOW,
                        metadata::LIBDL_LIBRARIES,
                        metadata::PROGRAM_TLS_INFO,
                    ]
                    .into_iter()
                    .map(|name| Item {
                        alias: name.into(),
                        kind: ExportKind::Global,
                        which: MainOrAdapter::Main,
                        name: name.into(),
                    }),
                )
                .collect();

            let func_items = metadata
                .table_address_imports
                .iter()
                .copied()
                .map(global_item)
                .collect();

            let mut import_items = BTreeMap::<_, Vec<_>>::new();
            for import in &metadata.imports {
                import_items.entry(import.module).or_default().push(Item {
                    alias: import.name.into(),
                    kind: ExportKind::from(&import.ty),
                    which: MainOrAdapter::Main,
                    name: format!("{}:{}", import.module, import.name),
                });
            }

            encoder = encoder.library(
                name,
                module,
                LibraryInfo {
                    instantiate_after_shims: false,
                    arguments: [
                        (metadata::GOT_MEM.into(), Instance::Items(mem_items)),
                        (metadata::GOT_FUNC.into(), Instance::Items(func_items)),
                        (metadata::ENV.into(), Instance::Items(env_items)),
                    ]
                    .into_iter()
                    .chain(
                        import_items
                            .into_iter()
                            .map(|(k, v)| (k.into(), Instance::Items(v))),
                    )
                    .collect(),
                },
            )?;

            seen.insert(name.as_str());
        }

        encoder
            .library(
                "__init",
                &make_init_module(
                    &metadata,
                    &exporters,
                    &env_exports,
                    dl_openables,
                    tls,
                    table_base,
                )?,
                LibraryInfo {
                    instantiate_after_shims: true,
                    arguments: iter::once((
                        metadata::ENV.into(),
                        Instance::MainOrAdapter(MainOrAdapter::Main),
                    ))
                    .chain(self.libraries.iter().map(|(name, ..)| {
                        (
                            name.clone(),
                            Instance::MainOrAdapter(MainOrAdapter::Adapter(name.clone())),
                        )
                    }))
                    .collect(),
                },
            )?
            .encode()
    }
}
