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
    ComponentTypeRef, Encoding, Parser, Payload, PrimitiveValType, ValType, Validator,
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
        todo!()
    }
}

// aka canonical_module_type
pub fn lower(bytes: &[u8]) -> Result<Vec<u8>> {
    let ct = Component::parse(bytes)?;

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
    todo!()
}

enum FlatteningContext {
    Lift,
    Lower,
}
fn flatten_functype(ft: () /* component function type */, context: FlatteningContext) -> () /* core wasm function type */
{
    todo!();
    // flat_params = flatten_types(ft.param_types())
    // if len(flat_params) > MAX_FLAT_PARAMS:
    //   flat_params = ['i32']
    // flat_results = flatten_types(ft.result_types())
    // if len(flat_results) > MAX_FLAT_RESULTS:
    //   match context:
    //     case 'lift':
    //       flat_results = ['i32']
    //     case 'lower':
    //       flat_params += ['i32']
    //       flat_results = []
    //  return CoreFuncType(flat_params, flat_results)
    //
    //  def flatten_types(ts):
    //    return [ ft for t in ts for ft in flatten_type(t) ]
}

fn flatten_type(t: () /* component type */) -> Vec<()> /* set of core types */ {
    todo!();
    // match despecialize(t):
    //  case Bool() : return ['i32']
    //  case U8() | U16() | U32() | S8() | S16() | S32(): return ['i32']
    //  case S64() | U64: return ['i64']
    //  case Float32() : return ['f32']
    //  case Float64() : return ['64']
    //  case Char() : return ['i32']
    //  case String() | List(_): return ['i32', 'i32']
    //  case Record(fields): return flatten_record(fields)
    //  case Variant(cases): return flatten_variant(cases)
    //  case Flags(labels): return ['i32'] * num_i32_flags(labels)
    //  _: unreachable()
    //
    //
    //  def flatten_record(fields):
    //    flat = []
    //    for f in fields:
    //      flat += flatten_type(f.t)
    //    return flat
    //
    //  def flatten_variant(cases):
    //    flat = []
    //    for c in cases:
    //      if c.t is not None:
    //        for (i,ft) in enumerate(flatten_type(c.t)):
    //          if i < len(flat):
    //            flat[i] = join(flat[i], ft)
    //          else:
    //            flat.append(ft)
    //    return flatten_type(discriminant_type(cases)) + flat
    //
    //  def join(a, b):
    //    if a == b: return a
    //    if (a == 'i32' and b == 'f32') or (a == 'f32' and b == 'i32'): return 'i32'
    //    return 'i64'
    //
}
