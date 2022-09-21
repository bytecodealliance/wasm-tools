#![allow(
    dead_code,
    unused_imports,
    dead_code,
    unused_variables,
    unreachable_code
)]
#[cfg(feature = "cli")]
pub mod cli;

use anyhow::{bail, Result};
use indexmap::IndexMap;
use wasmparser::{
    types::{
        ComponentDefinedType, ComponentEntityType, ComponentFuncType, ComponentValType, Type, Types,
    },
    Chunk, ComponentExport, ComponentExternalKind, ComponentImport, ComponentType,
    ComponentTypeRef, Encoding, FuncType, Parser, Payload, PrimitiveValType, ValType, Validator,
    WasmFeatures,
};

pub fn lift(_bytes: &[u8]) -> Result<Vec<u8>> {
    todo!()
}

struct Component<'a> {
    types: Types,
    imports: Vec<ComponentImport<'a>>,
    exports: Vec<ComponentExport<'a>>,
}

impl<'a> Component<'a> {
    pub fn parse(mut bytes: &'a [u8]) -> Result<Component<'a>> {
        let mut parser = Parser::new(0);
        let mut parsers = Vec::new();
        let mut validator = Validator::new_with_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });
        let mut imports = Vec::new();
        let mut exports = Vec::new();

        loop {
            match parser.parse(bytes, true)? {
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    match payload {
                        Payload::Version {
                            num,
                            encoding,
                            range,
                        } => {
                            if parsers.is_empty() && encoding != Encoding::Component {
                                bail!("file is not a WebAssembly component");
                            }
                            validator.version(num, encoding, &range)?;
                        }
                        Payload::TypeSection(s) => {
                            validator.type_section(&s)?;
                        }
                        Payload::ImportSection(s) => {
                            validator.import_section(&s)?;
                        }
                        Payload::FunctionSection(s) => {
                            validator.function_section(&s)?;
                        }
                        Payload::TableSection(s) => {
                            validator.table_section(&s)?;
                        }
                        Payload::MemorySection(s) => {
                            validator.memory_section(&s)?;
                        }
                        Payload::TagSection(s) => {
                            validator.tag_section(&s)?;
                        }
                        Payload::GlobalSection(s) => {
                            validator.global_section(&s)?;
                        }
                        Payload::ExportSection(s) => {
                            validator.export_section(&s)?;
                        }
                        Payload::StartSection { func, range } => {
                            validator.start_section(func, &range)?;
                        }
                        Payload::ElementSection(s) => {
                            validator.element_section(&s)?;
                        }
                        Payload::DataCountSection { count, range, .. } => {
                            validator.data_count_section(count, &range)?;
                        }
                        Payload::DataSection(s) => {
                            validator.data_section(&s)?;
                        }
                        Payload::CodeSectionStart { count, range, .. } => {
                            validator.code_section_start(count, &range)?;
                        }
                        Payload::CodeSectionEntry(s) => {
                            validator.code_section_entry(&s)?;
                        }
                        Payload::ModuleSection {
                            parser: inner,
                            range,
                        } => {
                            validator.module_section(&range)?;
                            parsers.push(parser);
                            parser = inner;
                        }
                        Payload::InstanceSection(s) => {
                            validator.instance_section(&s)?;
                        }
                        Payload::CoreTypeSection(s) => {
                            validator.core_type_section(&s)?;
                        }
                        Payload::ComponentSection {
                            parser: inner,
                            range,
                        } => {
                            // FIXME: we cant lower recursive components, so is this even a sensible
                            // thing to permit?
                            validator.component_section(&range)?;
                            parsers.push(parser);
                            parser = inner;
                        }
                        Payload::ComponentInstanceSection(s) => {
                            validator.component_instance_section(&s)?;
                        }
                        Payload::ComponentAliasSection(s) => {
                            validator.component_alias_section(&s)?;
                        }
                        Payload::ComponentTypeSection(s) => {
                            validator.component_type_section(&s)?;
                        }
                        Payload::ComponentCanonicalSection(s) => {
                            validator.component_canonical_section(&s)?;
                        }
                        Payload::ComponentStartSection(s) => {
                            validator.component_start_section(&s)?;
                        }
                        Payload::ComponentImportSection(mut s) => {
                            validator.component_import_section(&s)?;
                            if parsers.is_empty() {
                                for _ in 0..s.get_count() {
                                    let import = s.read().expect("import");
                                    imports.push(import);
                                }
                            }
                        }
                        Payload::ComponentExportSection(mut s) => {
                            validator.component_export_section(&s)?;
                            if parsers.is_empty() {
                                for _ in 0..s.get_count() {
                                    let export = s.read().expect("export");
                                    exports.push(export);
                                }
                            }
                        }
                        Payload::CustomSection { .. } => {
                            // skip!
                        }
                        Payload::UnknownSection { id, range, .. } => {
                            validator.unknown_section(id, &range)?;
                        }
                        Payload::End(offset) => {
                            let types = validator.end(offset)?;
                            match parsers.pop() {
                                Some(parent) => parser = parent,
                                None => {
                                    return Ok(Self {
                                        types,
                                        exports,
                                        imports,
                                    })
                                }
                            }
                        }
                    }
                }
                Chunk::NeedMoreData(_) => bail!("incomplete module"),
            }
        }
    }

    fn start_params(&self) -> impl Iterator<Item = (&'a str, ComponentValType)> + '_ {
        self.imports.iter().filter_map(|i| {
            match self
                .types
                .component_entity_type_from_import(&i)
                .expect("component import")
            {
                ComponentEntityType::Value(v) => Some((i.name, v)),
                _ => None,
            }
        })
    }

    fn import_funcs(&self) -> impl Iterator<Item = (&'a str, &'_ ComponentFuncType)> + '_ {
        self.imports.iter().filter_map(|i| {
            match self
                .types
                .component_entity_type_from_import(&i)
                .expect("component import")
            {
                ComponentEntityType::Func(f) => Some((
                    i.name,
                    match self.types.type_from_id(f).expect("type from id") {
                        Type::ComponentFunc(ft) => ft,
                        _ => unreachable!("entity type was Func, but didnt find a ComponentFunc?"),
                    },
                )),
                _ => None,
            }
        })
    }

    fn start_returns(&self) -> impl Iterator<Item = (&'a str, ComponentValType)> + '_ {
        self.exports.iter().filter_map(|i| match i.kind {
            ComponentExternalKind::Value => Some((
                i.name,
                self.types.value_at(i.index).expect("component value"),
            )),
            _ => None,
        })
    }

    fn export_funcs(&self) -> impl Iterator<Item = (&'a str, &'_ ComponentFuncType)> + '_ {
        self.exports.iter().filter_map(|e| {
            match self
                .types
                .component_entity_type_from_export(e)
                .expect("component export")
            {
                ComponentEntityType::Func(f) => Some((
                    e.name,
                    match self.types.type_from_id(f).expect("type from id") {
                        Type::ComponentFunc(ft) => ft,
                        _ => {
                            unreachable!("entity type was a Func, but didnt find a ComponentFunc?")
                        }
                    },
                )),
                _ => None,
            }
        })
    }

    fn despecialize_val_type(&self, val_type: &ComponentValType) -> ComponentDespecializedType {
        match val_type {
            ComponentValType::Primitive(prim) => ComponentDespecializedType::Primitive(*prim),
            ComponentValType::Type(ty) => {
                match self.types.type_from_id(*ty).expect("type id in val type") {
                    Type::Defined(def) => match def {
                        ComponentDefinedType::Primitive(prim) => {
                            ComponentDespecializedType::Primitive(*prim)
                        }
                        ComponentDefinedType::Record(record) => ComponentDespecializedType::Record(
                            record
                                .fields
                                .iter()
                                .map(|(name, valtype)| {
                                    (name.clone(), self.despecialize_val_type(valtype))
                                })
                                .collect(),
                        ),
                        ComponentDefinedType::Variant(variant) => {
                            ComponentDespecializedType::Variant(
                                variant
                                    .cases
                                    .iter()
                                    .map(|(name, variant_case)| {
                                        (
                                            name.clone(),
                                            variant_case
                                                .ty
                                                .as_ref()
                                                .map(|t| self.despecialize_val_type(t)),
                                        )
                                    })
                                    .collect(),
                            )
                        }
                        ComponentDefinedType::List(valtype) => ComponentDespecializedType::List(
                            Box::new(self.despecialize_val_type(valtype)),
                        ),
                        ComponentDefinedType::Tuple(tuple) => ComponentDespecializedType::Record(
                            tuple
                                .types
                                .iter()
                                .enumerate()
                                .map(|(ix, valtype)| {
                                    (format!("{}", ix), self.despecialize_val_type(valtype))
                                })
                                .collect(),
                        ),
                        ComponentDefinedType::Flags(flags) => ComponentDespecializedType::Record(
                            flags
                                .iter()
                                .map(|name| {
                                    (
                                        name.clone(),
                                        ComponentDespecializedType::Primitive(
                                            PrimitiveValType::Bool,
                                        ),
                                    )
                                })
                                .collect(),
                        ),
                        ComponentDefinedType::Union(cases) => ComponentDespecializedType::Variant(
                            cases
                                .types
                                .iter()
                                .enumerate()
                                .map(|(ix, valtype)| {
                                    (format!("{}", ix), Some(self.despecialize_val_type(valtype)))
                                })
                                .collect(),
                        ),
                        ComponentDefinedType::Enum(labels) => ComponentDespecializedType::Variant(
                            labels.iter().map(|label| (label.clone(), None)).collect(),
                        ),
                        ComponentDefinedType::Option(t) => ComponentDespecializedType::Variant(
                            [
                                ("none".to_owned(), None),
                                ("some".to_owned(), Some(self.despecialize_val_type(t))),
                            ]
                            .into_iter()
                            .collect(),
                        ),
                        ComponentDefinedType::Result { ok, err } => {
                            ComponentDespecializedType::Variant(
                                [
                                    (
                                        "ok".to_owned(),
                                        ok.as_ref().map(|t| self.despecialize_val_type(t)),
                                    ),
                                    (
                                        "err".to_owned(),
                                        err.as_ref().map(|t| self.despecialize_val_type(t)),
                                    ),
                                ]
                                .into_iter()
                                .collect(),
                            )
                        }
                    },
                    _ => unreachable!("val type can only contain prim and defined type"),
                }
            }
        }
    }

    fn flatten_func_type(&self, ft: &ComponentFuncType, context: FlatteningContext) -> FuncType {
        // FIXME idk the values for these
        const MAX_FLAT_PARAMS: usize = 123;
        const MAX_FLAT_RESULTS: usize = 123;

        let mut flat_params = Vec::new();
        for (_param_name, ty) in ft.params.iter() {
            flat_params.append(&mut self.despecialize_val_type(ty).flatten());
        }
        if flat_params.len() > MAX_FLAT_PARAMS {
            flat_params = vec![ValType::I32];
        }

        let mut flat_results = Vec::new();
        for (_result_name, ty) in ft.results.iter() {
            flat_results.append(&mut self.despecialize_val_type(ty).flatten());
        }
        if flat_results.len() > MAX_FLAT_RESULTS {
            match context {
                FlatteningContext::Lift => flat_results = vec![ValType::I32],
                FlatteningContext::Lower => {
                    // FIXME does this mean the params length could exceed MAX_FLAT_PARAMS? is that
                    // ok in the spec or not allowed?
                    flat_params.push(ValType::I32);
                    flat_results = Vec::new();
                }
            }
        }

        FuncType::new(flat_params, flat_results)
    }

    /// This takes slices of params and results, rather than a ComponentFuncType, because
    /// I can't construct a ComponentFuncType to use for the start function
    pub fn mangle_funcname(
        &self,
        name: &str,
        func_params: &[(Option<String>, ComponentValType)],
        func_results: &[(Option<String>, ComponentValType)],
    ) -> String {
        todo!()
    }
}

pub enum ComponentDespecializedType {
    Primitive(PrimitiveValType),
    Record(IndexMap<String, ComponentDespecializedType>),
    Variant(IndexMap<String, Option<ComponentDespecializedType>>),
    List(Box<ComponentDespecializedType>),
}

impl ComponentDespecializedType {
    pub fn contains_dynamic_allocation(&self) -> bool {
        use ComponentDespecializedType::*;
        match self {
            Primitive(PrimitiveValType::String) => true,
            Primitive(_) => false,
            Record(fields) => fields.iter().any(|(_, t)| t.contains_dynamic_allocation()),
            Variant(cases) => cases.iter().any(|(_, t)| {
                t.as_ref()
                    .map(|t| t.contains_dynamic_allocation())
                    .unwrap_or(false)
            }),
            List(t) => t.contains_dynamic_allocation(),
        }
    }

    pub fn flatten(&self) -> Vec<ValType> {
        use ComponentDespecializedType::*;

        match self {
            Primitive(PrimitiveValType::Bool) => vec![ValType::I32],
            Primitive(PrimitiveValType::U8 | PrimitiveValType::U16 | PrimitiveValType::U32) => {
                vec![ValType::I32]
            }
            Primitive(PrimitiveValType::S8 | PrimitiveValType::S16 | PrimitiveValType::S32) => {
                vec![ValType::I32]
            }
            Primitive(PrimitiveValType::U64 | PrimitiveValType::S64) => {
                vec![ValType::I64]
            }
            Primitive(PrimitiveValType::Float32) => vec![ValType::F32],
            Primitive(PrimitiveValType::Float64) => vec![ValType::F64],
            Primitive(PrimitiveValType::Char) => vec![ValType::I32],
            Primitive(PrimitiveValType::String) => vec![ValType::I32, ValType::I32],
            List(_) => vec![ValType::I32, ValType::I32],
            Record(fields) => {
                let mut flat = Vec::new();
                for (_name, ty) in fields {
                    flat.append(&mut ty.flatten());
                }
                flat
            }
            Variant(cases) => {
                let mut flat = Vec::new();
                for (_name, maybe_ty) in cases {
                    if let Some(ty) = maybe_ty {
                        for (ix, ft) in ty.flatten().iter().enumerate() {
                            if ix < flat.len() {
                                flat[ix] = join(flat[ix], *ft);
                            } else {
                                flat.push(*ft)
                            }
                        }
                    }
                }

                let mut with_discriminant = vec![ValType::I32];
                with_discriminant.append(&mut flat);
                with_discriminant
            }
        }
    }
}

fn join(a: ValType, b: ValType) -> ValType {
    if a == ValType::V128
        || a == ValType::FuncRef
        || a == ValType::ExternRef
        || b == ValType::V128
        || b == ValType::FuncRef
        || b == ValType::ExternRef
    {
        // FIXME is this true? what does the spec say about these types
        unreachable!("V128, FuncRef, and ExternRef shouldnt be supported??");
    }
    match (a, b) {
        _ if a == b => a,
        (ValType::I32, ValType::F32) => ValType::I32,
        (ValType::F32, ValType::I32) => ValType::I32,
        (ValType::I64, ValType::F64) => ValType::I64,
        (ValType::F64, ValType::I64) => ValType::I64,
        _ => ValType::I64,
    }
}

pub struct ModuleType {
    pub imports: IndexMap<(String, String), EntityType>,
    pub exports: IndexMap<String, EntityType>,
}

pub enum EntityType {
    Func(FuncType),
    Memory,
}

// aka canonical_module_type
pub fn lower(bytes: &[u8]) -> Result<ModuleType> {
    // FIXME
    const CABI_VERSION: &str = "idk";

    let ct = Component::parse(bytes)?;

    let mut imports = IndexMap::new();
    for (name, func_type) in ct.import_funcs() {
        let flat_ft = ct.flatten_func_type(func_type, FlatteningContext::Lower);
        imports.insert(
            (
                "".to_owned(),
                ct.mangle_funcname(name, &func_type.params, &func_type.results),
            ),
            EntityType::Func(flat_ft),
        );
    }

    let mut exports = IndexMap::new();
    exports.insert("cabi_memory".to_owned(), EntityType::Memory);
    exports.insert(
        "cabi_realloc".to_owned(),
        EntityType::Func(FuncType::new(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32],
        )),
    );

    Ok(ModuleType { imports, exports })

    // input: ComponentType
    // output: ModuleType
    //
    // start_params, import_funcs = mangle_instances(ct.imports)
    // start_results, export_funcs = mangle_instances(ct.exports)
    //
    // imports = []
    // for name,ft in import_funcs:
    //   flat_ft = flatten_functype(ft, 'lower')
    //   imports.append(CoreImportDecl('', mangle_funcname(name, ft), flat_ft))
    //
    // exports = []
    // exports.append(CoreExportDecl('cabi_memory', CoreMemoryType(initial=0, maximum=None)))
    // exports.append(CoreExportDecl('cabi_realloc', CoreFuncType(['i32','i32','i32','i32'],['i32'])))
    //
    // start_ft = FuncType(start_params, start_results)
    // start_name = mangle_funcname('cabi_start{cabi=' + CABI_VERSION + '}', start_ft)
    // exports.append(CoreExportDecl(start_name, flatten_functype(start_ft, 'lift')))
    //
    // for name,ft in export_funcs:
    //   flat_ft = flatten_functype(ft, 'lift')
    //   exports.append(CoreExportDecl(mangle_funcname(name, ft), flat_ft))
    //   if any(contains_dynamic_allocation(t) for t in ft.results):
    //     exports.append(CoreExportDecl('cabi_post_' + name, CoreFuncType(flat_ft.results, [])))
    //  return ModuleType(imports, exports)
    //
    //  def contains_dynamic_allication(t)
    //    match despecialize(t):
    //      case String(): return True
    //      case List(_): return True
    //      case Record(fields): return any(contains_dynamic_allocation(f.t) for f in fields)
    //      case Variant(cases): return any(contains_dynamic_allocation(c.t) for c in cases)
    //      case _: return False,
}

enum FlatteningContext {
    Lift,
    Lower,
}
