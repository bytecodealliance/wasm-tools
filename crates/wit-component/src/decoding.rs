use anyhow::{anyhow, bail, Result};
use indexmap::IndexMap;
use std::hash::{Hash, Hasher};
use wasmparser::{
    types::{self, KebabString},
    ComponentExport, ComponentImport, ComponentTypeRef, Parser, Payload, PrimitiveValType,
    ValidPayload, Validator, WasmFeatures,
};
use wit_parser::*;

/// Represents information about a decoded WebAssembly component.
struct ComponentInfo<'a> {
    /// Wasmparser-defined type information learned after a component is fully
    /// validated.
    types: types::Types,
    /// Map of imports and what type they're importing.
    imports: IndexMap<&'a str, ComponentImport<'a>>,
    /// Map of exports and what they're exporting.
    exports: IndexMap<&'a str, ComponentExport<'a>>,
}

impl<'a> ComponentInfo<'a> {
    /// Creates a new component info by parsing the given WebAssembly component bytes.
    fn new(bytes: &'a [u8]) -> Result<Self> {
        let mut validator = Validator::new_with_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });
        let mut exports = IndexMap::new();
        let mut imports = IndexMap::new();
        let mut depth = 1;
        let mut types = None;

        for payload in Parser::new(0).parse_all(bytes) {
            let payload = payload?;
            match validator.payload(&payload)? {
                ValidPayload::Ok => {}
                ValidPayload::Parser(_) => depth += 1,
                ValidPayload::End(t) => {
                    depth -= 1;
                    if depth == 0 {
                        types = Some(t);
                    }
                }
                ValidPayload::Func(..) => {}
            }

            match payload {
                Payload::ComponentImportSection(s) if depth == 1 => {
                    for import in s {
                        let import = import?;
                        let prev = imports.insert(import.name, import);
                        assert!(prev.is_none());
                    }
                }
                Payload::ComponentExportSection(s) if depth == 1 => {
                    for export in s {
                        let export = export?;
                        let prev = exports.insert(export.name, export);
                        assert!(prev.is_none());
                    }
                }
                _ => {}
            }
        }
        Ok(Self {
            types: types.unwrap(),
            imports,
            exports,
        })
    }
}

/// Decode the world described by the given component bytes.
///
/// This function takes a binary component as input and will infer the
/// `World` representation of its imports and exports. The binary component at
/// this time is either a "types only" component produced by `wit-component` or
/// an actual output of `wit-component`.
///
/// The returned world represents the description of imports and exports
/// from the component.
///
/// This can fail if the input component is invalid or otherwise isn't of the
/// expected shape. At this time not all component shapes are supported here.
pub fn decode_world(name: &str, bytes: &[u8]) -> Result<(Document, WorldId)> {
    let info = ComponentInfo::new(bytes)?;
    let mut imports = IndexMap::new();
    let mut exports = IndexMap::new();
    let mut ret = Document::default();
    let mut decoder = InterfaceDecoder::new(&info, &mut ret);

    for (name, import) in info.imports.iter() {
        // Imports right now are only supported if they're an import of an
        // instance. The instance is expected to export only functions and types
        // where types are named types used in functions.
        let ty = match import.ty {
            ComponentTypeRef::Instance(i) => match info.types.type_at(i, false).unwrap() {
                types::Type::ComponentInstance(i) => i,
                _ => unreachable!(),
            },
            _ => bail!("unsupported non-instance import `{name}`"),
        };
        let id = decoder.decode(
            Some(*name),
            Some(import.url),
            ty.exports(info.types.as_ref())
                .map(|(n, _, ty)| (n.as_str(), ty)),
        )?;
        imports.insert(name.to_string(), id);
    }

    let mut default = IndexMap::new();
    for (name, export) in info.exports.iter() {
        // Get a `ComponentEntityType` which describes the type of the item
        // being exported here. If a type itself is being exported then "peel"
        // it to feign an actual entity being exported here to handle both
        // type-only and normal components produced by `wit-component`.
        let mut ty = info
            .types
            .component_entity_type_from_export(export)
            .unwrap();
        if let types::ComponentEntityType::Type(id) = ty {
            match info.types.type_from_id(id).unwrap() {
                types::Type::ComponentInstance(_) => ty = types::ComponentEntityType::Instance(id),
                types::Type::ComponentFunc(_) => ty = types::ComponentEntityType::Func(id),
                _ => {}
            }
        }

        match ty {
            // If an instance is being exported then that means this is an
            // interface being exported, so decode the interface here and
            // register an export.
            types::ComponentEntityType::Instance(ty) => {
                let ty = info
                    .types
                    .type_from_id(ty)
                    .unwrap()
                    .as_component_instance_type()
                    .unwrap();
                let id = decoder.decode(
                    Some(*name),
                    Some(export.url),
                    ty.exports(info.types.as_ref())
                        .map(|(n, _, t)| (n.as_str(), t)),
                )?;
                exports.insert(name.to_string(), id);
            }

            // Otherwise assume everything else is part of the "default" export.
            ty => {
                default.insert(*name, ty);
            }
        }
    }

    let default = if default.is_empty() {
        None
    } else {
        Some(decoder.decode(None, None, default.iter().map(|(n, t)| (*n, *t)))?)
    };
    let world = ret.worlds.alloc(World {
        name: name.to_string(),
        docs: Default::default(),
        imports,
        exports,
        default,
    });
    Ok((ret, world))
}

/// Represents an interface decoder for WebAssembly components.
struct InterfaceDecoder<'a, 'doc> {
    info: &'a ComponentInfo<'a>,
    doc: &'doc mut Document,

    /// Names learned prior about each type id, if it was exported.
    name_map: IndexMap<types::TypeId, &'a str>,

    /// A map from a type id to what it's been translated to.
    type_map: IndexMap<types::TypeId, Type>,

    /// A second map, similar to `type_map`, which is keyed off a pointer hash
    /// instead of `TypeId`.
    ///
    /// The purpose of this is to detect when a type is aliased as there will
    /// be two unique `TypeId` structures pointing at the same `types::Type`
    /// structure, so the second layer of map here ensures that types are
    /// only defined once and the second `TypeId` referring to a type will end
    /// up as an alias and/or import.
    type_src_map: IndexMap<PtrHash<'a, types::Type>, Type>,
}

impl<'a, 'doc> InterfaceDecoder<'a, 'doc> {
    /// Creates a new interface decoder for the given component information.
    fn new(info: &'a ComponentInfo<'a>, doc: &'doc mut Document) -> Self {
        Self {
            info,
            doc,
            name_map: IndexMap::new(),
            type_map: IndexMap::new(),
            type_src_map: IndexMap::new(),
        }
    }

    /// Consumes the decoder and returns the interface representation assuming
    /// that the interface is made of the specified exports.
    pub fn decode(
        &mut self,
        name: Option<&str>,
        url: Option<&str>,
        exports: impl ExactSizeIterator<Item = (&'a str, types::ComponentEntityType)> + Clone,
    ) -> Result<InterfaceId> {
        let mut interface = Interface::default();
        if let Some(name) = name {
            interface.name = name.to_string();
        }
        if let Some(url) = url {
            interface.url = Some(url.to_string());
        }
        // Populate names in the name map first
        let mut types = Vec::new();
        let mut funcs = Vec::new();
        for (name, ty) in exports {
            let id = match ty {
                types::ComponentEntityType::Type(id) => {
                    types.push(id);
                    id
                }
                types::ComponentEntityType::Func(ty) => {
                    funcs.push((name, ty));
                    continue;
                }
                _ => bail!("expected function or type export"),
            };

            let prev = self.name_map.insert(id, name);
            assert!(prev.is_none());
        }

        // Process all types first which should show show up in topological
        // order of the types as defined in the original component. This will
        // ensure that type aliases are resolved correctly for now.
        for id in types {
            assert!(matches!(
                self.info.types.type_from_id(id).unwrap(),
                types::Type::Defined(_)
            ));
            let ty = self.decode_type(&types::ComponentValType::Type(id))?;
            match ty {
                Type::Id(id) => {
                    interface.types.push(id);
                }
                _ => unreachable!(),
            }
        }

        // Afterwards process all functions which should mostly use the types
        // previously decoded.
        for (name, ty) in funcs {
            match self.info.types.type_from_id(ty).unwrap() {
                types::Type::ComponentFunc(ty) => {
                    let func = self.function(name, ty)?;
                    interface.functions.push(func);
                }
                _ => unimplemented!(),
            }
        }

        // Reset the `name_map` for the next interface, but notably persist the
        // `type_map` which is required to get `use` of types across interfaces
        // working since in the component it will be an alias to a type defined
        // in a previous interface.
        self.name_map.clear();

        Ok(self.doc.interfaces.alloc(interface))
    }

    fn decode_params(&mut self, ps: &[(KebabString, types::ComponentValType)]) -> Result<Params> {
        ps.iter()
            .map(|(n, t)| Ok((n.to_string(), self.decode_type(t)?)))
            .collect::<Result<_>>()
    }

    fn decode_results(
        &mut self,
        ps: &[(Option<KebabString>, types::ComponentValType)],
    ) -> Result<Results> {
        let results: Vec<(Option<String>, Type)> = ps
            .iter()
            .map(|(n, t)| Ok((n.as_ref().map(KebabString::to_string), self.decode_type(t)?)))
            .collect::<Result<_>>()?;

        // Results must be either
        // - A single anonymous type
        // - Any number of named types
        match results.len() {
            1 => {
                // We either have a single anonymous type or a single
                // named type. Either is valid.
                let (name, ty) = results.into_iter().next().unwrap();
                match name {
                    Some(name) => Ok(Results::Named(vec![(name, ty)])),
                    None => Ok(Results::Anon(ty)),
                }
            }
            _ => {
                // Otherwise, all types must be named; unwrap the names.
                Ok(Results::Named(
                    results.into_iter().map(|(n, t)| (n.unwrap(), t)).collect(),
                ))
            }
        }
    }

    fn function(&mut self, func_name: &str, ty: &types::ComponentFuncType) -> Result<Function> {
        let params = self.decode_params(&ty.params)?;
        let results = self.decode_results(&ty.results)?;

        Ok(Function {
            docs: Docs::default(),
            name: func_name.to_string(),
            kind: FunctionKind::Freestanding,
            params,
            results,
        })
    }

    fn decode_type(&mut self, ty: &types::ComponentValType) -> Result<Type> {
        Ok(match ty {
            types::ComponentValType::Primitive(ty) => self.decode_primitive(*ty)?,
            types::ComponentValType::Type(id) => {
                // If this precise `TypeId` has already been decoded, then
                // return that same result.
                if let Some(ty) = self.type_map.get(id) {
                    return Ok(*ty);
                }

                let name = self.name_map.get(id).map(ToString::to_string);
                let ty = self.info.types.type_from_id(*id).unwrap();
                let key = PtrHash(ty);
                let ty = match self.type_src_map.get(&key) {
                    // If this `TypeId` points to a type which has previously
                    // been defined then a second `TypeId` pointing at it is
                    // indicative of an alias. Inject the alias here.
                    Some(prev) => {
                        let id = self.doc.types.alloc(TypeDef {
                            docs: Default::default(),
                            kind: TypeDefKind::Type(*prev),
                            name,
                            interface: Some(self.doc.interfaces.next_id()),
                        });
                        Type::Id(id)
                    }

                    // ... or this `TypeId`'s source definition has never been
                    // seen before, so declare the full type.
                    None => {
                        let ty = self.decode_defined_type(name, ty)?;
                        let prev = self.type_src_map.insert(key, ty);
                        assert!(prev.is_none());
                        ty
                    }
                };

                // Record the result of translation with the `TypeId` we have.
                let prev = self.type_map.insert(*id, ty);
                assert!(prev.is_none());
                ty
            }
        })
    }

    fn decode_defined_type(&mut self, name: Option<String>, ty: &'a types::Type) -> Result<Type> {
        match ty {
            types::Type::Defined(ty) => match ty {
                types::ComponentDefinedType::Primitive(ty) => self.decode_named_primitive(name, ty),
                types::ComponentDefinedType::Record(r) => self.decode_record(name, r.fields.iter()),
                types::ComponentDefinedType::Variant(v) => {
                    self.decode_variant(name, v.cases.iter())
                }
                types::ComponentDefinedType::List(ty) => {
                    let inner = self.decode_type(ty)?;
                    Ok(Type::Id(self.alloc_type(name, TypeDefKind::List(inner))))
                }
                types::ComponentDefinedType::Tuple(t) => self.decode_tuple(name, &t.types),
                types::ComponentDefinedType::Flags(names) => self.decode_flags(name, names.iter()),
                types::ComponentDefinedType::Enum(names) => self.decode_enum(name, names.iter()),
                types::ComponentDefinedType::Union(u) => self.decode_union(name, &u.types),
                types::ComponentDefinedType::Option(ty) => self.decode_option(name, ty),
                types::ComponentDefinedType::Result { ok, err } => {
                    self.decode_result(name, ok.as_ref(), err.as_ref())
                }
            },
            _ => unreachable!(),
        }
    }

    fn decode_optional_type(
        &mut self,
        ty: Option<&types::ComponentValType>,
    ) -> Result<Option<Type>> {
        match ty {
            Some(ty) => self.decode_type(ty).map(Some),
            None => Ok(None),
        }
    }

    fn decode_named_primitive(
        &mut self,
        name: Option<String>,
        ty: &PrimitiveValType,
    ) -> Result<Type> {
        let mut ty = self.decode_primitive(*ty)?;
        if let Some(name) = name {
            ty = Type::Id(self.alloc_type(Some(name), TypeDefKind::Type(ty)));
        }

        Ok(ty)
    }

    fn decode_primitive(&mut self, ty: PrimitiveValType) -> Result<Type> {
        Ok(match ty {
            PrimitiveValType::Bool => Type::Bool,
            PrimitiveValType::S8 => Type::S8,
            PrimitiveValType::U8 => Type::U8,
            PrimitiveValType::S16 => Type::S16,
            PrimitiveValType::U16 => Type::U16,
            PrimitiveValType::S32 => Type::S32,
            PrimitiveValType::U32 => Type::U32,
            PrimitiveValType::S64 => Type::S64,
            PrimitiveValType::U64 => Type::U64,
            PrimitiveValType::Float32 => Type::Float32,
            PrimitiveValType::Float64 => Type::Float64,
            PrimitiveValType::Char => Type::Char,
            PrimitiveValType::String => Type::String,
        })
    }

    fn decode_record(
        &mut self,
        record_name: Option<String>,
        fields: impl ExactSizeIterator<Item = (&'a KebabString, &'a types::ComponentValType)>,
    ) -> Result<Type> {
        let record_name =
            record_name.ok_or_else(|| anyhow!("interface has an unnamed record type"))?;

        let record = Record {
            fields: fields
                .map(|(name, ty)| {
                    Ok(Field {
                        docs: Docs::default(),
                        name: name.to_string(),
                        ty: self.decode_type(ty)?,
                    })
                })
                .collect::<Result<_>>()?,
        };

        Ok(Type::Id(self.alloc_type(
            Some(record_name),
            TypeDefKind::Record(record),
        )))
    }

    fn decode_variant(
        &mut self,
        variant_name: Option<String>,
        cases: impl ExactSizeIterator<Item = (&'a KebabString, &'a types::VariantCase)>,
    ) -> Result<Type> {
        let variant_name =
            variant_name.ok_or_else(|| anyhow!("interface has an unnamed variant type"))?;

        let variant = Variant {
            cases: cases
                .map(|(name, case)| {
                    Ok(Case {
                        docs: Docs::default(),
                        name: name.to_string(),
                        ty: self.decode_optional_type(case.ty.as_ref())?,
                    })
                })
                .collect::<Result<_>>()?,
        };

        Ok(Type::Id(self.alloc_type(
            Some(variant_name),
            TypeDefKind::Variant(variant),
        )))
    }

    fn decode_tuple(
        &mut self,
        name: Option<String>,
        tys: &[types::ComponentValType],
    ) -> Result<Type> {
        let tuple = Tuple {
            types: tys
                .iter()
                .map(|ty| self.decode_type(ty))
                .collect::<Result<_>>()?,
        };

        Ok(Type::Id(self.alloc_type(name, TypeDefKind::Tuple(tuple))))
    }

    fn decode_flags(
        &mut self,
        flags_name: Option<String>,
        names: impl ExactSizeIterator<Item = &'a KebabString>,
    ) -> Result<Type> {
        let flags_name =
            flags_name.ok_or_else(|| anyhow!("interface has an unnamed flags type"))?;

        let flags = Flags {
            flags: names
                .map(|name| {
                    Ok(Flag {
                        docs: Docs::default(),
                        name: name.to_string(),
                    })
                })
                .collect::<Result<_>>()?,
        };

        Ok(Type::Id(
            self.alloc_type(Some(flags_name), TypeDefKind::Flags(flags)),
        ))
    }

    fn decode_enum(
        &mut self,
        enum_name: Option<String>,
        names: impl ExactSizeIterator<Item = &'a KebabString>,
    ) -> Result<Type> {
        let enum_name = enum_name.ok_or_else(|| anyhow!("interface has an unnamed enum type"))?;
        let enum_ = Enum {
            cases: names
                .map(|name| {
                    Ok(EnumCase {
                        docs: Docs::default(),
                        name: name.to_string(),
                    })
                })
                .collect::<Result<_>>()?,
        };

        Ok(Type::Id(
            self.alloc_type(Some(enum_name), TypeDefKind::Enum(enum_)),
        ))
    }

    fn decode_union(
        &mut self,
        name: Option<String>,
        tys: &[types::ComponentValType],
    ) -> Result<Type> {
        let union = Union {
            cases: tys
                .iter()
                .map(|ty| {
                    Ok(UnionCase {
                        docs: Docs::default(),
                        ty: self.decode_type(ty)?,
                    })
                })
                .collect::<Result<_>>()?,
        };

        Ok(Type::Id(self.alloc_type(name, TypeDefKind::Union(union))))
    }

    fn decode_option(
        &mut self,
        name: Option<String>,
        payload: &types::ComponentValType,
    ) -> Result<Type> {
        let payload = self.decode_type(payload)?;
        Ok(Type::Id(
            self.alloc_type(name, TypeDefKind::Option(payload)),
        ))
    }

    fn decode_result(
        &mut self,
        name: Option<String>,
        ok: Option<&types::ComponentValType>,
        err: Option<&types::ComponentValType>,
    ) -> Result<Type> {
        let ok = self.decode_optional_type(ok)?;
        let err = self.decode_optional_type(err)?;
        Ok(Type::Id(self.alloc_type(
            name,
            TypeDefKind::Result(Result_ { ok, err }),
        )))
    }

    fn alloc_type(&mut self, name: Option<String>, kind: TypeDefKind) -> TypeId {
        self.doc.types.alloc(TypeDef {
            docs: Docs::default(),
            kind,
            name,
            interface: Some(self.doc.interfaces.next_id()),
        })
    }
}

struct PtrHash<'a, T>(&'a T);

impl<T> PartialEq for PtrHash<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T> Eq for PtrHash<'_, T> {}

impl<T> Hash for PtrHash<'_, T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        std::ptr::hash(self.0, hasher)
    }
}
