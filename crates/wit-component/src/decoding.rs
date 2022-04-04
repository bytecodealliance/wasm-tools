use anyhow::{anyhow, bail, Context, Result};
use std::collections::HashMap;
use wasmparser::{
    types, Chunk, ComponentExportKind, Encoding, Parser, Payload, PrimitiveInterfaceType,
    Validator, WasmFeatures,
};
use wit_parser::{
    abi::Abi, validate_id, Case, Docs, Field, Function, FunctionKind, Int, Interface, Record,
    RecordKind, Type, TypeDef, TypeDefKind, TypeId, Variant,
};

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

                        Payload::ComponentTypeSection(s) => {
                            validator.component_type_section(&s)?;
                        }
                        Payload::ComponentImportSection(s) => {
                            validator.component_import_section(&s)?;
                        }
                        Payload::ComponentFunctionSection(s) => {
                            validator.component_function_section(&s)?;
                        }
                        Payload::ModuleSection {
                            parser: inner,
                            range,
                        } => {
                            validator.module_section(&range)?;
                            parsers.push(parser);
                            parser = inner;
                        }
                        Payload::ComponentSection {
                            parser: inner,
                            range,
                        } => {
                            validator.component_section(&range)?;
                            parsers.push(parser);
                            parser = inner;
                        }
                        Payload::InstanceSection(s) => {
                            validator.instance_section(&s)?;
                        }
                        Payload::ComponentExportSection(s) => {
                            validator.component_export_section(&s)?;

                            if parsers.is_empty() {
                                for export in s {
                                    let export = export?;
                                    match export.kind {
                                        ComponentExportKind::Function(index) => {
                                            exported_functions.push((export.name, index));
                                        }
                                        ComponentExportKind::Type(index) => {
                                            exported_types.push((export.name, index));
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                        Payload::ComponentStartSection(s) => {
                            validator.component_start_section(&s)?;
                        }
                        Payload::AliasSection(s) => {
                            validator.alias_section(&s)?;
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
    type_map: HashMap<types::TypeId, Type>,
    name_map: HashMap<types::TypeId, &'a str>,
    bool_ty: Option<TypeId>,
    string_ty: Option<TypeId>,
}

impl<'a> InterfaceDecoder<'a> {
    /// Creates a new interface decoder for the given component information.
    pub fn new(info: &'a ComponentInfo<'a>) -> Self {
        Self {
            info,
            interface: Interface::default(),
            name_map: HashMap::new(),
            type_map: HashMap::new(),
            bool_ty: None,
            string_ty: None,
        }
    }

    /// Consumes the decoder and returns the interface representation.
    pub fn decode(mut self) -> Result<Interface> {
        // Populate names in the name map first
        for (name, index) in &self.info.exported_types {
            if let types::TypeDef::Interface(_) = self.info.types.type_at(*index).unwrap() {
                self.name_map
                    .insert(self.info.types.id_from_type_index(*index).unwrap(), name);
            }
        }

        for (name, index) in &self.info.exported_types {
            let ty = match self.info.types.type_at(*index).unwrap() {
                types::TypeDef::ComponentFunc(ty) => ty,
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

        let mut results = Vec::new();
        if let types::InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) = &ty.result {
        } else {
            results.push(("".to_string(), self.decode_type(&ty.result)?));
        }

        self.interface.functions.push(Function {
            abi: Abi::Canonical,
            is_async: false,
            docs: Docs::default(),
            name: func_name.to_string(),
            kind: FunctionKind::Freestanding,
            params,
            results,
        });

        Ok(())
    }

    fn decode_type(&mut self, ty: &types::InterfaceTypeRef) -> Result<Type> {
        Ok(match ty {
            types::InterfaceTypeRef::Primitive(ty) => self.decode_primitive(*ty)?,
            types::InterfaceTypeRef::Type(id) => {
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
                    types::TypeDef::Interface(ty) => match ty {
                        types::InterfaceType::Primitive(ty) => {
                            self.decode_named_primitive(name, ty)?
                        }
                        types::InterfaceType::Record(fields) => {
                            self.decode_record(name, fields.iter())?
                        }
                        types::InterfaceType::Variant(cases) => {
                            self.decode_variant(name, cases.iter())?
                        }
                        types::InterfaceType::List(ty) => {
                            let inner = self.decode_type(ty)?;
                            Type::Id(self.alloc_type(name, TypeDefKind::List(inner)))
                        }
                        types::InterfaceType::Tuple(tys) => self.decode_tuple(name, tys)?,
                        types::InterfaceType::Flags(names) => {
                            self.decode_flags(name, names.iter())?
                        }
                        types::InterfaceType::Enum(names) => {
                            self.decode_enum(name, names.iter())?
                        }
                        types::InterfaceType::Union(tys) => self.decode_union(name, tys)?,
                        types::InterfaceType::Option(ty) => self.decode_option(name, ty)?,
                        types::InterfaceType::Expected(ok, err) => {
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
        ty: &PrimitiveInterfaceType,
    ) -> Result<Type> {
        let mut ty = self.decode_primitive(*ty)?;
        if let Some(name) = name {
            validate_id(&name)
                .with_context(|| format!("type name `{}` is not a valid identifier", name))?;

            ty = Type::Id(self.alloc_type(Some(name), TypeDefKind::Type(ty)));
        }

        Ok(ty)
    }

    fn decode_primitive(&mut self, ty: PrimitiveInterfaceType) -> Result<Type> {
        Ok(match ty {
            PrimitiveInterfaceType::Unit => bail!("unexpected unit type in interface function"),
            PrimitiveInterfaceType::Bool => self.bool(),
            PrimitiveInterfaceType::S8 => Type::S8,
            PrimitiveInterfaceType::U8 => Type::U8,
            PrimitiveInterfaceType::S16 => Type::S16,
            PrimitiveInterfaceType::U16 => Type::U16,
            PrimitiveInterfaceType::S32 => Type::S32,
            PrimitiveInterfaceType::U32 => Type::U32,
            PrimitiveInterfaceType::S64 => Type::S64,
            PrimitiveInterfaceType::U64 => Type::U64,
            PrimitiveInterfaceType::Float32 => Type::F32,
            PrimitiveInterfaceType::Float64 => Type::F64,
            PrimitiveInterfaceType::Char => Type::Char,
            PrimitiveInterfaceType::String => self.string(),
        })
    }

    fn decode_record(
        &mut self,
        record_name: Option<String>,
        fields: impl ExactSizeIterator<Item = (&'a String, &'a types::InterfaceTypeRef)>,
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
            kind: RecordKind::Other,
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

        let cases_len = cases.len();

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
                        ty: match case.ty {
                            types::InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) => {
                                None
                            }
                            _ => Some(self.decode_type(&case.ty)?),
                        },
                    })
                })
                .collect::<Result<_>>()?,
            tag: Variant::infer_tag(cases_len),
        };

        Ok(Type::Id(self.alloc_type(
            Some(variant_name),
            TypeDefKind::Variant(variant),
        )))
    }

    fn decode_tuple(
        &mut self,
        name: Option<String>,
        tys: &[types::InterfaceTypeRef],
    ) -> Result<Type> {
        let record = Record {
            fields: tys
                .iter()
                .enumerate()
                .map(|(i, ty)| {
                    Ok(Field {
                        docs: Docs::default(),
                        name: i.to_string(),
                        ty: self.decode_type(ty)?,
                    })
                })
                .collect::<Result<_>>()?,
            kind: RecordKind::Tuple,
        };

        Ok(Type::Id(self.alloc_type(name, TypeDefKind::Record(record))))
    }

    fn decode_flags(
        &mut self,
        flags_name: Option<String>,
        names: impl ExactSizeIterator<Item = &'a String>,
    ) -> Result<Type> {
        let flags_name =
            flags_name.ok_or_else(|| anyhow!("interface has an unnamed flags type"))?;

        let record = Record {
            fields: names
                .map(|name| {
                    validate_id(name).with_context(|| {
                        format!(
                            "flags `{}` has a flag named `{}` that is not a valid identifier",
                            flags_name, name
                        )
                    })?;

                    Ok(Field {
                        docs: Docs::default(),
                        name: name.clone(),
                        ty: self.decode_primitive(PrimitiveInterfaceType::Bool)?,
                    })
                })
                .collect::<Result<_>>()?,
            kind: RecordKind::Flags(None),
        };

        Ok(Type::Id(
            self.alloc_type(Some(flags_name), TypeDefKind::Record(record)),
        ))
    }

    fn decode_enum(
        &mut self,
        enum_name: Option<String>,
        names: impl ExactSizeIterator<Item = &'a String>,
    ) -> Result<Type> {
        let enum_name = enum_name.ok_or_else(|| anyhow!("interface has an unnamed enum type"))?;

        let names_len = names.len();

        let variant = Variant {
            cases: names
                .map(|name| {
                    validate_id(name).with_context(|| {
                        format!(
                            "enum `{}` has a value `{}` that is not a valid identifier",
                            enum_name, name
                        )
                    })?;

                    Ok(Case {
                        docs: Docs::default(),
                        name: name.to_string(),
                        ty: None,
                    })
                })
                .collect::<Result<_>>()?,
            tag: Variant::infer_tag(names_len),
        };

        Ok(Type::Id(self.alloc_type(
            Some(enum_name),
            TypeDefKind::Variant(variant),
        )))
    }

    fn decode_union(
        &mut self,
        name: Option<String>,
        tys: &[types::InterfaceTypeRef],
    ) -> Result<Type> {
        let variant = Variant {
            cases: tys
                .iter()
                .enumerate()
                .map(|(i, ty)| {
                    Ok(Case {
                        docs: Docs::default(),
                        name: i.to_string(),
                        ty: Some(self.decode_type(ty)?),
                    })
                })
                .collect::<Result<_>>()?,
            tag: Variant::infer_tag(tys.len()),
        };

        Ok(Type::Id(
            self.alloc_type(name, TypeDefKind::Variant(variant)),
        ))
    }

    fn decode_option(
        &mut self,
        name: Option<String>,
        ty: &types::InterfaceTypeRef,
    ) -> Result<Type> {
        let variant = Variant {
            cases: vec![
                Case {
                    docs: Docs::default(),
                    name: "none".to_string(),
                    ty: None,
                },
                Case {
                    docs: Docs::default(),
                    name: "some".to_string(),
                    ty: Some(self.decode_type(ty)?),
                },
            ],
            tag: Variant::infer_tag(2),
        };

        Ok(Type::Id(
            self.alloc_type(name, TypeDefKind::Variant(variant)),
        ))
    }

    fn decode_expected(
        &mut self,
        name: Option<String>,
        ok: &types::InterfaceTypeRef,
        error: &types::InterfaceTypeRef,
    ) -> Result<Type> {
        let variant = Variant {
            cases: vec![
                Case {
                    docs: Docs::default(),
                    name: "ok".to_string(),
                    ty: match ok {
                        types::InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) => None,
                        _ => Some(self.decode_type(ok)?),
                    },
                },
                Case {
                    docs: Docs::default(),
                    name: "err".to_string(),
                    ty: match error {
                        types::InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) => None,
                        _ => Some(self.decode_type(error)?),
                    },
                },
            ],
            tag: Variant::infer_tag(2),
        };

        Ok(Type::Id(
            self.alloc_type(name, TypeDefKind::Variant(variant)),
        ))
    }

    fn alloc_type(&mut self, name: Option<String>, kind: TypeDefKind) -> TypeId {
        self.interface.types.alloc(TypeDef {
            docs: Docs::default(),
            kind,
            name,
            foreign_module: None,
        })
    }

    fn bool(&mut self) -> Type {
        if self.bool_ty.is_none() {
            self.bool_ty = Some(self.alloc_type(
                None,
                TypeDefKind::Variant(Variant {
                    cases: vec![
                        Case {
                            docs: Docs::default(),
                            name: "false".to_string(),
                            ty: None,
                        },
                        Case {
                            docs: Docs::default(),
                            name: "true".to_string(),
                            ty: None,
                        },
                    ],
                    tag: Int::U8,
                }),
            ));
        }
        Type::Id(self.bool_ty.unwrap())
    }

    fn string(&mut self) -> Type {
        if self.string_ty.is_none() {
            self.string_ty = Some(self.alloc_type(None, TypeDefKind::List(Type::Char)));
        }
        Type::Id(self.string_ty.unwrap())
    }
}
