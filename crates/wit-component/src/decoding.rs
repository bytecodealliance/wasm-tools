use anyhow::{anyhow, bail, Context, Result};
use indexmap::IndexMap;
use wasmparser::{
    types, Chunk, ComponentExternalKind, Encoding, Parser, Payload, PrimitiveValType, Validator,
    WasmFeatures,
};
use wit_parser::*;

/// Represents information about a decoded WebAssembly component.
pub struct ComponentInfo<'a> {
    /// The types defined in the component.
    pub types: types::Types,
    /// The exported types in the component.
    pub exported_types: Vec<(&'a str, u32)>,
    /// The exported functions in the component.
    pub exported_functions: Vec<(&'a str, u32)>,
}

impl<'a> ComponentInfo<'a> {
    /// Creates a new component info by parsing the given WebAssembly component bytes.
    pub fn new(mut bytes: &'a [u8]) -> Result<Self> {
        let mut parser = Parser::new(0);
        let mut parsers = Vec::new();
        let mut validator = Validator::new_with_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });
        let mut exported_types = Vec::new();
        let mut exported_functions = Vec::new();

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
                        Payload::DataCountSection { count, range } => {
                            validator.data_count_section(count, &range)?;
                        }
                        Payload::DataSection(s) => {
                            validator.data_section(&s)?;
                        }
                        Payload::CodeSectionStart { count, range, .. } => {
                            validator.code_section_start(count, &range)?;
                        }
                        Payload::CodeSectionEntry(f) => {
                            validator.code_section_entry(&f)?;
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
                        Payload::AliasSection(s) => {
                            validator.alias_section(&s)?;
                        }
                        Payload::CoreTypeSection(s) => {
                            validator.core_type_section(&s)?;
                        }
                        Payload::ComponentSection {
                            parser: inner,
                            range,
                        } => {
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
                        Payload::ComponentImportSection(s) => {
                            validator.component_import_section(&s)?;
                        }
                        Payload::ComponentExportSection(s) => {
                            validator.component_export_section(&s)?;

                            if parsers.is_empty() {
                                for export in s {
                                    let export = export?;
                                    match export.kind {
                                        ComponentExternalKind::Func => {
                                            exported_functions.push((export.name, export.index));
                                        }
                                        ComponentExternalKind::Type => {
                                            exported_types.push((export.name, export.index));
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                        Payload::CustomSection { .. } => {
                            // Skip custom sections
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
                                        exported_types,
                                        exported_functions,
                                    });
                                }
                            }
                        }
                    }
                }
                Chunk::NeedMoreData(_) => unreachable!(),
            }
        }
    }
}

/// Represents an interface decoder for WebAssembly components.
pub struct InterfaceDecoder<'a> {
    info: &'a ComponentInfo<'a>,
    interface: Interface,
    type_map: IndexMap<types::TypeId, Type>,
    name_map: IndexMap<types::TypeId, &'a str>,
}

impl<'a> InterfaceDecoder<'a> {
    /// Creates a new interface decoder for the given component information.
    pub fn new(info: &'a ComponentInfo<'a>) -> Self {
        Self {
            info,
            interface: Interface::default(),
            name_map: IndexMap::new(),
            type_map: IndexMap::new(),
        }
    }

    /// Consumes the decoder and returns the interface representation.
    pub fn decode(mut self) -> Result<Interface> {
        // Populate names in the name map first
        for (name, index) in &self.info.exported_types {
            if let types::Type::Defined(_) = self.info.types.type_at(*index, false).unwrap() {
                self.name_map.insert(
                    self.info.types.id_from_type_index(*index, false).unwrap(),
                    name,
                );
            }
        }

        for (name, index) in &self.info.exported_types {
            let ty = match self.info.types.type_at(*index, false).unwrap() {
                types::Type::ComponentFunc(ty) => ty,
                _ => continue,
            };

            self.add_function(name, ty)?;
        }

        for (name, index) in &self.info.exported_functions {
            let ty = self.info.types.component_function_at(*index).unwrap();
            self.add_function(name, ty)?;
        }

        Ok(self.interface)
    }

    fn add_function(&mut self, func_name: &str, ty: &types::ComponentFuncType) -> Result<()> {
        validate_id(func_name)
            .with_context(|| format!("function name `{}` is not a valid identifier", func_name))?;

        let mut params = Vec::new();
        for (name, ty) in ty.params.iter() {
            let name = name
                .as_ref()
                .ok_or_else(|| anyhow!("function `{}` has a parameter without a name", func_name))?
                .clone();

            validate_id(&name).with_context(|| {
                format!(
                    "function `{}` has a parameter `{}` that is not a valid identifier",
                    func_name, name
                )
            })?;

            params.push((name, self.decode_type(ty)?));
        }

        let result = self.decode_type(&ty.result)?;

        self.interface.functions.push(Function {
            is_async: false,
            docs: Docs::default(),
            name: func_name.to_string(),
            kind: FunctionKind::Freestanding,
            params,
            result,
        });

        Ok(())
    }

    fn decode_type(&mut self, ty: &types::ComponentValType) -> Result<Type> {
        Ok(match ty {
            types::ComponentValType::Primitive(ty) => self.decode_primitive(*ty)?,
            types::ComponentValType::Type(id) => {
                if let Some(ty) = self.type_map.get(id) {
                    return Ok(*ty);
                }

                let name = self.name_map.get(id).map(ToString::to_string);

                if let Some(name) = name.as_deref() {
                    validate_id(name).with_context(|| {
                        format!("type name `{}` is not a valid identifier", name)
                    })?;
                }

                let ty = match &self.info.types.type_from_id(*id).unwrap() {
                    types::Type::Defined(ty) => match ty {
                        types::ComponentDefinedType::Primitive(ty) => {
                            self.decode_named_primitive(name, ty)?
                        }
                        types::ComponentDefinedType::Record(r) => {
                            self.decode_record(name, r.fields.iter())?
                        }
                        types::ComponentDefinedType::Variant(v) => {
                            self.decode_variant(name, v.cases.iter())?
                        }
                        types::ComponentDefinedType::List(ty) => {
                            let inner = self.decode_type(ty)?;
                            Type::Id(self.alloc_type(name, TypeDefKind::List(inner)))
                        }
                        types::ComponentDefinedType::Tuple(t) => {
                            self.decode_tuple(name, &t.types)?
                        }
                        types::ComponentDefinedType::Flags(names) => {
                            self.decode_flags(name, names.iter())?
                        }
                        types::ComponentDefinedType::Enum(names) => {
                            self.decode_enum(name, names.iter())?
                        }
                        types::ComponentDefinedType::Union(u) => {
                            self.decode_union(name, &u.types)?
                        }
                        types::ComponentDefinedType::Option(ty) => self.decode_option(name, ty)?,
                        types::ComponentDefinedType::Expected(ok, err) => {
                            self.decode_expected(name, ok, err)?
                        }
                    },
                    _ => unreachable!(),
                };

                self.type_map.insert(*id, ty);
                ty
            }
        })
    }

    fn decode_named_primitive(
        &mut self,
        name: Option<String>,
        ty: &PrimitiveValType,
    ) -> Result<Type> {
        let mut ty = self.decode_primitive(*ty)?;
        if let Some(name) = name {
            validate_id(&name)
                .with_context(|| format!("type name `{}` is not a valid identifier", name))?;

            ty = Type::Id(self.alloc_type(Some(name), TypeDefKind::Type(ty)));
        }

        Ok(ty)
    }

    fn decode_primitive(&mut self, ty: PrimitiveValType) -> Result<Type> {
        Ok(match ty {
            PrimitiveValType::Unit => Type::Unit,
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
        fields: impl ExactSizeIterator<Item = (&'a String, &'a types::ComponentValType)>,
    ) -> Result<Type> {
        let record_name =
            record_name.ok_or_else(|| anyhow!("interface has an unnamed record type"))?;

        let record = Record {
            fields: fields
                .map(|(name, ty)| {
                    validate_id(name).with_context(|| {
                        format!(
                            "record `{}` has a field `{}` that is not a valid identifier",
                            record_name, name
                        )
                    })?;

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
        cases: impl ExactSizeIterator<Item = (&'a String, &'a types::VariantCase)>,
    ) -> Result<Type> {
        let variant_name =
            variant_name.ok_or_else(|| anyhow!("interface has an unnamed variant type"))?;

        let variant = Variant {
            cases: cases
                .map(|(name, case)| {
                    validate_id(name).with_context(|| {
                        format!(
                            "variant `{}` has a case `{}` that is not a valid identifier",
                            variant_name, name
                        )
                    })?;

                    Ok(Case {
                        docs: Docs::default(),
                        name: name.to_string(),
                        ty: self.decode_type(&case.ty)?,
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
        names: impl ExactSizeIterator<Item = &'a String>,
    ) -> Result<Type> {
        let flags_name =
            flags_name.ok_or_else(|| anyhow!("interface has an unnamed flags type"))?;

        let flags = Flags {
            flags: names
                .map(|name| {
                    validate_id(name).with_context(|| {
                        format!(
                            "flags `{}` has a flag named `{}` that is not a valid identifier",
                            flags_name, name
                        )
                    })?;

                    Ok(Flag {
                        docs: Docs::default(),
                        name: name.clone(),
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
        names: impl ExactSizeIterator<Item = &'a String>,
    ) -> Result<Type> {
        let enum_name = enum_name.ok_or_else(|| anyhow!("interface has an unnamed enum type"))?;
        let enum_ = Enum {
            cases: names
                .map(|name| {
                    validate_id(name).with_context(|| {
                        format!(
                            "enum `{}` has a value `{}` that is not a valid identifier",
                            enum_name, name
                        )
                    })?;

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

    fn decode_expected(
        &mut self,
        name: Option<String>,
        ok: &types::ComponentValType,
        err: &types::ComponentValType,
    ) -> Result<Type> {
        let ok = self.decode_type(ok)?;
        let err = self.decode_type(err)?;
        Ok(Type::Id(self.alloc_type(
            name,
            TypeDefKind::Expected(Expected { ok, err }),
        )))
    }

    fn alloc_type(&mut self, name: Option<String>, kind: TypeDefKind) -> TypeId {
        self.interface.types.alloc(TypeDef {
            docs: Docs::default(),
            kind,
            name,
            foreign_module: None,
        })
    }
}
