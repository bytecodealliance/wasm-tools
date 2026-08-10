//! Support for parsing and analyzing [dynamic
//! library](https://github.com/WebAssembly/tool-conventions/blob/main/DynamicLinking.md) modules.

use {
    anyhow::{Context, Error, Result, bail},
    std::{
        collections::{BTreeSet, HashMap, HashSet},
        fmt,
    },
    wasmparser::{
        Dylink0Subsection, ExternalKind, FuncType, KnownCustom, MemInfo, Operator, Parser, Payload,
        RefType, SymbolFlags, TableType, TagKind, TagType, TypeRef, ValType,
    },
};

pub const ENV: &str = "env";
pub const GOT_MEM: &str = "GOT.mem";
pub const GOT_FUNC: &str = "GOT.func";
pub const MEMORY: &str = "memory";
pub const MEMORY_BASE: &str = "__memory_base";
pub const TABLE_BASE: &str = "__table_base";
pub const STACK_POINTER: &str = "__stack_pointer";
pub const INIT_STACK_POINTER: &str = "__init_stack_pointer";
pub const ASYNCIFY_DATA: &str = "__asyncify_data";
pub const ASYNCIFY_STATE: &str = "__asyncify_state";
pub const INDIRECT_FUNCTION_TABLE: &str = "__indirect_function_table";
pub const HEAP_BASE: &str = "__heap_base";
pub const HEAP_END: &str = "__heap_end";
pub const STACK_HIGH: &str = "__stack_high";
pub const STACK_LOW: &str = "__stack_low";
pub const APPLY_DATA_RELOCS: &str = "__wasm_apply_data_relocs";
pub const CALL_CTORS: &str = "__wasm_call_ctors";
pub const INITIALIZE: &str = "_initialize";
pub const START: &str = "_start";
pub const LIBDL_LIBRARIES: &str = "__wasm_libdl_libraries";
pub const INIT_TASK: &str = "__wasm_init_task";
pub const INIT_ASYNC_TASK: &str = "__wasm_init_async_task";
pub const ROOT: &str = "$root";
pub const THREAD_NEW_INDIRECT: &str = "[thread-new-indirect-v0]";
pub const CONTEXT_GET_1: &str = "[context-get-1]";
pub const GET_STACK_POINTER: &str = "__wasm_get_stack_pointer";
pub const SET_STACK_POINTER: &str = "__wasm_set_stack_pointer";
pub const GET_TLS_BASE: &str = "__wasm_get_tls_base";
pub const SET_TLS_BASE: &str = "__wasm_set_tls_base";
pub const TLS_SIZE: &str = "__tls_size";
pub const TLS_ALIGN: &str = "__tls_align";
pub const INIT_TLS: &str = "__wasm_init_tls";
pub const PROGRAM_TLS_INFO: &str = "__wasm_program_tls_info";

/// Represents a core Wasm value type (not including V128 or reference types, which are not yet supported)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

impl TryFrom<ValType> for ValueType {
    type Error = Error;

    fn try_from(value: ValType) -> Result<Self> {
        Ok(match value {
            ValType::I32 => Self::I32,
            ValType::I64 => Self::I64,
            ValType::F32 => Self::F32,
            ValType::F64 => Self::F64,
            _ => bail!("{value:?} not yet supported"),
        })
    }
}

impl From<ValueType> for wasm_encoder::ValType {
    fn from(value: ValueType) -> Self {
        match value {
            ValueType::I32 => Self::I32,
            ValueType::I64 => Self::I64,
            ValueType::F32 => Self::F32,
            ValueType::F64 => Self::F64,
        }
    }
}

/// Represents a core Wasm function type
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionType {
    pub parameters: Vec<ValueType>,
    pub results: Vec<ValueType>,
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} -> {:?}", self.parameters, self.results)
    }
}

impl TryFrom<&FuncType> for FunctionType {
    type Error = Error;

    fn try_from(value: &FuncType) -> Result<Self> {
        Ok(Self {
            parameters: value
                .params()
                .iter()
                .map(|&v| ValueType::try_from(v))
                .collect::<Result<_>>()?,
            results: value
                .results()
                .iter()
                .map(|&v| ValueType::try_from(v))
                .collect::<Result<_>>()?,
        })
    }
}

/// Represents a core Wasm global variable type
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GlobalType {
    pub ty: ValueType,
    pub mutable: bool,
    pub shared: bool,
}

impl fmt::Display for GlobalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.mutable {
            write!(f, "mut ")?;
        }
        write!(f, "{:?}", self.ty)
    }
}

/// Represents a core Wasm export or import type
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Function(FunctionType),
    Global(GlobalType),
    Tag(FunctionType),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Function(ty) => write!(f, "function {ty}"),
            Self::Global(ty) => write!(f, "global {ty}"),
            Self::Tag(ty) => write!(f, "tag {ty}"),
        }
    }
}

impl From<&Type> for wasm_encoder::ExportKind {
    fn from(value: &Type) -> Self {
        match value {
            Type::Function(_) => wasm_encoder::ExportKind::Func,
            Type::Global(_) => wasm_encoder::ExportKind::Global,
            Type::Tag(_) => wasm_encoder::ExportKind::Tag,
        }
    }
}

/// Represents a core Wasm import
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Import<'a> {
    pub module: &'a str,
    pub name: &'a str,
    pub ty: Type,
    pub flags: SymbolFlags,
}

/// Represents a core Wasm export
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExportKey<'a> {
    pub name: &'a str,
    pub ty: Type,
}

impl<'a> fmt::Display for ExportKey<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.ty)
    }
}

/// Represents a core Wasm export, including dylink.0 flags
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Export<'a> {
    pub key: ExportKey<'a>,
    pub flags: SymbolFlags,
}

/// Metadata extracted from a dynamic library module
#[derive(Debug)]
pub struct Metadata<'a> {
    /// The name of the module
    ///
    /// This is currently not part of the file itself and must be provided separately, but the plan is to add
    /// something like a `WASM_DYLINK_SO_NAME` field to the dynamic linking tool convention so we can parse it
    /// along with everything else.
    pub name: &'a str,

    /// Whether this module should be resolvable via `dlopen`
    pub dl_openable: bool,

    /// The `WASM_DYLINK_MEM_INFO` value (or all zeros if not found)
    pub mem_info: MemInfo,

    /// The `WASM_DYLINK_NEEDED` values, if any
    pub needed_libs: Vec<&'a str>,

    /// The `WASM_DYLINK_RUNTIME_PATH` values, if any
    pub runtime_path: Vec<&'a str>,

    /// Whether this module exports `__wasm_apply_data_relocs`
    pub has_data_relocs: bool,

    /// Whether this module exports `__wasm_call_ctors`
    pub has_ctors: bool,

    /// Whether this module exports `_initialize`
    pub has_initialize: bool,

    /// Whether this module exports `_start`
    pub has_wasi_start: bool,

    /// Whether this module imports `__wasm_libdl_libraries`
    pub needs_libdl_libraries: bool,

    /// Whether this module exports `__wasm_init_task`
    pub has_init_task: bool,

    /// Whether this module includes any `component-type*` custom sections which include exports
    pub has_component_exports: bool,

    /// Whether this module imports `__asyncify_state` or `__asyncify_data`, indicating that it is
    /// asyncified with `--pass-arg=asyncify-relocatable` option.
    pub is_asyncified: bool,

    /// Whether this module imports `__stack_pointer`
    pub needs_stack_pointer: bool,

    /// Whether this module imports `__init_stack_pointer`
    pub needs_init_stack_pointer: bool,

    /// Whether this module imports `__heap_base`
    pub needs_heap_base: bool,

    /// Whether this module imports `__heap_end`
    pub needs_heap_end: bool,

    /// Whether this module imports `__stack_high`
    pub needs_stack_high: bool,

    /// Whether this module imports `__stack_low`
    pub needs_stack_low: bool,

    /// Whether this module imports `env::__wasm_get_tls_base`
    pub needs_get_tls_base: bool,

    /// Whether this module imports `env::__wasm_set_tls_base`
    pub needs_set_tls_base: bool,

    /// Whether this module imports the address of `__wasm_program_tls_info`
    pub needs_program_tls_info: bool,

    /// Whether this module imports `$root::[thread-new-indirect-v0]`, meaning
    /// the program may spawn threads and is thus using cooperative threading.
    pub uses_thread_new_indirect: bool,

    /// Whether this module has thread-local storage of its own which needs
    /// allocating and initializing on a freshly spawned thread.
    ///
    /// This requires the module to export all of `__tls_size`, `__tls_align`,
    /// and `__wasm_init_tls`, and to have a non-zero `tls_size`.
    pub has_tls_info: bool,

    /// The size of this module's thread-local storage, i.e. the value of the
    /// `__tls_size` global that `wasm-ld` synthesized for it.
    ///
    /// Zero if the module has no thread-local storage at all.
    pub tls_size: u32,

    /// The alignment of this module's thread-local storage, i.e. the value of
    /// the `__tls_align` global. At least one whenever `has_tls_info` is set.
    pub tls_align: u32,

    /// The functions imported from the `env` module, if any
    pub env_imports: BTreeSet<(&'a str, (FunctionType, SymbolFlags))>,

    /// The memory addresses imported from `GOT.mem`, if any
    pub memory_address_imports: BTreeSet<&'a str>,

    /// The table addresses imported from `GOT.func`, if any
    pub table_address_imports: BTreeSet<&'a str>,

    /// Imported exception tags
    pub tag_imports: BTreeSet<(&'a str, FunctionType)>,

    /// The symbols exported by this module, if any
    pub exports: BTreeSet<Export<'a>>,

    /// The symbols imported by this module (and not accounted for in the above fields), if any
    pub imports: BTreeSet<Import<'a>>,
}

impl<'a> Metadata<'a> {
    /// Parse the specified module and extract its metadata.
    pub fn try_new(
        name: &'a str,
        dl_openable: bool,
        module: &'a [u8],
        adapter_names: &HashSet<&str>,
    ) -> Result<Self> {
        let bindgen = crate::metadata::decode(module)?.1;
        let has_component_exports = !bindgen.resolve.worlds[bindgen.world].exports.is_empty();

        let mut result = Self {
            name,
            dl_openable,
            mem_info: MemInfo {
                memory_size: 0,
                memory_alignment: 1,
                table_size: 0,
                table_alignment: 1,
            },
            needed_libs: Vec::new(),
            runtime_path: Vec::new(),
            has_data_relocs: false,
            has_ctors: false,
            has_initialize: false,
            has_wasi_start: false,
            needs_libdl_libraries: false,
            has_init_task: false,
            has_component_exports,
            is_asyncified: false,
            needs_stack_pointer: false,
            needs_init_stack_pointer: false,
            needs_heap_base: false,
            needs_heap_end: false,
            needs_stack_high: false,
            needs_stack_low: false,
            needs_get_tls_base: false,
            needs_set_tls_base: false,
            needs_program_tls_info: false,
            uses_thread_new_indirect: false,
            has_tls_info: false,
            tls_size: 0,
            tls_align: 0,
            env_imports: BTreeSet::new(),
            memory_address_imports: BTreeSet::new(),
            table_address_imports: BTreeSet::new(),
            exports: BTreeSet::new(),
            imports: BTreeSet::new(),
            tag_imports: BTreeSet::new(),
        };
        let mut types = Vec::new();
        let mut function_types = Vec::new();
        let mut global_types = Vec::new();
        let mut global_values = Vec::new();
        let mut tls_size = None;
        let mut tls_align = None;
        let mut has_init_tls = false;
        let mut tag_types = Vec::new();
        let mut import_info = HashMap::new();
        let mut export_info = HashMap::new();

        for payload in Parser::new(0).parse_all(module) {
            match payload? {
                Payload::CustomSection(section) => {
                    if let KnownCustom::Dylink0(reader) = section.as_known() {
                        for subsection in reader {
                            match subsection.context("failed to parse `dylink.0` subsection")? {
                                Dylink0Subsection::MemInfo(info) => result.mem_info = info,
                                Dylink0Subsection::Needed(needed) => {
                                    result.needed_libs = needed.clone()
                                }
                                Dylink0Subsection::ExportInfo(info) => {
                                    export_info
                                        .extend(info.iter().map(|info| (info.name, info.flags)));
                                }
                                Dylink0Subsection::ImportInfo(info) => {
                                    import_info.extend(
                                        info.iter()
                                            .map(|info| ((info.module, info.field), info.flags)),
                                    );
                                }
                                Dylink0Subsection::RuntimePath(runtime_path) => {
                                    result.runtime_path.extend(runtime_path.iter());
                                }
                                Dylink0Subsection::Unknown { ty, .. } => {
                                    bail!("unrecognized `dylink.0` subsection: {ty}")
                                }
                            }
                        }
                    }
                }

                Payload::TypeSection(reader) => {
                    types = reader
                        .into_iter_err_on_gc_types()
                        .collect::<Result<Vec<_>, _>>()?;
                }

                Payload::ImportSection(reader) => {
                    for import in reader.into_imports() {
                        let import = import?;

                        match import.ty {
                            TypeRef::Func(ty) => function_types.push(usize::try_from(ty).unwrap()),
                            TypeRef::Global(ty) => {
                                global_types.push(ty);
                                global_values.push(None);
                            }
                            TypeRef::Tag(ty) => tag_types.push(ty),
                            _ => (),
                        }

                        let type_error = || {
                            bail!(
                                "unexpected type for {}:{}: {:?}",
                                import.module,
                                import.name,
                                import.ty
                            )
                        };

                        match (import.module, import.name) {
                            (self::ENV, self::MEMORY) => {
                                if !matches!(import.ty, TypeRef::Memory(_)) {
                                    return type_error();
                                }
                            }
                            (self::ENV, self::ASYNCIFY_DATA | self::ASYNCIFY_STATE) => {
                                result.is_asyncified = true;
                                if !matches!(
                                    import.ty,
                                    TypeRef::Global(wasmparser::GlobalType {
                                        content_type: ValType::I32,
                                        ..
                                    })
                                ) {
                                    return type_error();
                                }
                            }
                            (
                                self::ENV,
                                self::MEMORY_BASE
                                | self::TABLE_BASE
                                | self::STACK_POINTER
                                | self::INIT_STACK_POINTER,
                            ) => {
                                if matches!(
                                    import.ty,
                                    TypeRef::Global(wasmparser::GlobalType {
                                        content_type: ValType::I32,
                                        ..
                                    })
                                ) {
                                    match import.name {
                                        self::STACK_POINTER => result.needs_stack_pointer = true,
                                        self::INIT_STACK_POINTER => {
                                            result.needs_init_stack_pointer = true
                                        }
                                        _ => {}
                                    }
                                } else {
                                    return type_error();
                                }
                            }
                            (self::ENV, self::INDIRECT_FUNCTION_TABLE) => {
                                if let TypeRef::Table(TableType {
                                    element_type,
                                    maximum: None,
                                    ..
                                }) = import.ty
                                {
                                    if element_type != RefType::FUNCREF {
                                        return type_error();
                                    }
                                } else {
                                    return type_error();
                                }
                            }
                            (
                                self::ENV,
                                name @ (self::GET_STACK_POINTER
                                | self::SET_STACK_POINTER
                                | self::GET_TLS_BASE
                                | self::SET_TLS_BASE),
                            ) => {
                                if !matches!(import.ty, TypeRef::Func(_)) {
                                    return type_error();
                                }
                                match name {
                                    self::GET_TLS_BASE => result.needs_get_tls_base = true,
                                    self::SET_TLS_BASE => result.needs_set_tls_base = true,
                                    _ => {}
                                }
                            }
                            (self::ENV, name) => match import.ty {
                                TypeRef::Func(ty) => {
                                    result.env_imports.insert((
                                        name,
                                        (
                                            FunctionType::try_from(
                                                &types[usize::try_from(ty).unwrap()],
                                            )?,
                                            import_info
                                                .get(&(self::ENV, name))
                                                .copied()
                                                .unwrap_or_default(),
                                        ),
                                    ));
                                }
                                TypeRef::Tag(TagType {
                                    kind: TagKind::Exception,
                                    func_type_idx,
                                }) => {
                                    result.tag_imports.insert((
                                        name,
                                        FunctionType::try_from(
                                            &types[usize::try_from(func_type_idx).unwrap()],
                                        )?,
                                    ));
                                }
                                _ => return type_error(),
                            },
                            (self::GOT_MEM, name) => {
                                if let TypeRef::Global(wasmparser::GlobalType {
                                    content_type: ValType::I32,
                                    ..
                                }) = import.ty
                                {
                                    match name {
                                        self::HEAP_BASE => result.needs_heap_base = true,
                                        self::HEAP_END => result.needs_heap_end = true,
                                        self::STACK_HIGH => result.needs_stack_high = true,
                                        self::STACK_LOW => result.needs_stack_low = true,
                                        self::LIBDL_LIBRARIES => {
                                            result.needs_libdl_libraries = true;
                                        }
                                        self::PROGRAM_TLS_INFO => {
                                            result.needs_program_tls_info = true;
                                        }

                                        _ => {
                                            result.memory_address_imports.insert(name);
                                        }
                                    }
                                } else {
                                    return type_error();
                                }
                            }
                            (self::GOT_FUNC, name) => {
                                if let TypeRef::Global(wasmparser::GlobalType {
                                    content_type: ValType::I32,
                                    ..
                                }) = import.ty
                                {
                                    result.table_address_imports.insert(name);
                                } else {
                                    return type_error();
                                }
                            }
                            (self::ROOT, self::THREAD_NEW_INDIRECT) => {
                                result.uses_thread_new_indirect = true;
                            }
                            (module, name) if adapter_names.contains(module) => {
                                let ty = match import.ty {
                                    TypeRef::Global(wasmparser::GlobalType {
                                        content_type,
                                        mutable,
                                        shared,
                                    }) => Type::Global(GlobalType {
                                        ty: content_type.try_into()?,
                                        mutable,
                                        shared,
                                    }),
                                    TypeRef::Func(ty) => Type::Function(FunctionType::try_from(
                                        &types[usize::try_from(ty).unwrap()],
                                    )?),
                                    ty => {
                                        bail!("unsupported import kind for {module}.{name}: {ty:?}",)
                                    }
                                };
                                let flags = import_info
                                    .get(&(module, name))
                                    .copied()
                                    .unwrap_or_default();
                                result.imports.insert(Import {
                                    module,
                                    name,
                                    ty,
                                    flags,
                                });
                            }
                            _ => {
                                if !matches!(import.ty, TypeRef::Func(_) | TypeRef::Global(_)) {
                                    return type_error();
                                }
                            }
                        }
                    }
                }

                Payload::FunctionSection(reader) => {
                    for function in reader {
                        function_types.push(usize::try_from(function?).unwrap());
                    }
                }

                Payload::GlobalSection(reader) => {
                    for global in reader {
                        let global = global?;
                        global_types.push(global.ty);
                        let mut ops = global.init_expr.get_operators_reader();
                        global_values.push(match (ops.read(), ops.read()) {
                            (Ok(Operator::I32Const { value }), Ok(Operator::End)) => {
                                Some(value as u32)
                            }
                            _ => None,
                        });
                    }
                }

                Payload::TagSection(reader) => {
                    for tag in reader {
                        tag_types.push(tag?);
                    }
                }

                Payload::ExportSection(reader) => {
                    for export in reader {
                        let export = export?;

                        let global_value = || {
                            global_values
                                .get(usize::try_from(export.index).unwrap())
                                .copied()
                                .flatten()
                        };
                        match export.name {
                            self::APPLY_DATA_RELOCS => result.has_data_relocs = true,
                            self::CALL_CTORS => result.has_ctors = true,
                            self::INITIALIZE => result.has_initialize = true,
                            self::START => result.has_wasi_start = true,
                            self::TLS_SIZE => tls_size = global_value(),
                            self::TLS_ALIGN => tls_align = global_value(),
                            self::INIT_TLS => has_init_tls = true,
                            _ => {
                                if export.name == self::INIT_TASK {
                                    result.has_init_task = true;
                                }
                                let ty = match export.kind {
                                    ExternalKind::Func => Type::Function(FunctionType::try_from(
                                        &types[function_types
                                            [usize::try_from(export.index).unwrap()]],
                                    )?),
                                    ExternalKind::Global => {
                                        let ty =
                                            global_types[usize::try_from(export.index).unwrap()];
                                        Type::Global(GlobalType {
                                            ty: ValueType::try_from(ty.content_type)?,
                                            mutable: ty.mutable,
                                            shared: ty.shared,
                                        })
                                    }
                                    ExternalKind::Tag => Type::Tag(FunctionType::try_from(
                                        &types[usize::try_from(
                                            tag_types[usize::try_from(export.index).unwrap()]
                                                .func_type_idx,
                                        )
                                        .unwrap()],
                                    )?),
                                    kind => {
                                        bail!(
                                            "unsupported export kind for {}: {kind:?}",
                                            export.name
                                        )
                                    }
                                };
                                let flags =
                                    export_info.get(&export.name).copied().unwrap_or_default();
                                result.exports.insert(Export {
                                    key: ExportKey {
                                        name: export.name,
                                        ty,
                                    },
                                    flags,
                                });
                            }
                        }
                    }
                }

                _ => {}
            }
        }

        // A module only participates in whole-program TLS setup if the linker
        // driver arranged for these to be exported and it actually has some
        // thread-local storage. Anything else (a library built without those
        // exports, or the synthesized stubs module) is left out of
        // `__wasm_program_tls_info`: it never reads a TLS base.
        result.tls_size = tls_size.unwrap_or(0);
        result.tls_align = tls_align.unwrap_or(0);
        result.has_tls_info = result.tls_size > 0;
        if result.has_tls_info {
            if !has_init_tls {
                bail!(
                    "{} has {} bytes of thread-local storage but does not export `{}`",
                    result.name,
                    result.tls_size,
                    self::INIT_TLS,
                );
            }
            if result.tls_align == 0 {
                bail!(
                    "{} has {} bytes of thread-local storage but no alignment",
                    result.name,
                    result.tls_size
                );
            }
        }

        Ok(result)
    }
}
