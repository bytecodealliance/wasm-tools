use anyhow::{anyhow, bail, Context, Result};
use std::collections::HashMap;
use wasmparser::{
    Chunk, ComponentExport, ComponentExportKind, ComponentFuncType, ComponentImport,
    ComponentTypeDef, Encoding, InstanceType, InterfaceType, InterfaceTypeRef, Parser, Payload,
    PrimitiveInterfaceType, Validator, WasmFeatures,
};
use wit_parser::{
    abi::Abi, validate_id, Case, Docs, Field, Function, FunctionKind, Int, Interface, Record,
    RecordKind, Type, TypeDef, TypeDefKind, TypeId, Variant,
};

/// Represents information about a decoded WebAssembly component.
#[derive(Default)]
pub struct ComponentInfo<'a> {
    /// The types defined in the component.
    pub types: Vec<ComponentTypeDef<'a>>,
    /// The instance types imported by the component.
    pub imports: Vec<ComponentImport<'a>>,
    /// The instance types exported by the component.
    pub exports: Vec<ComponentExport<'a>>,
}

impl<'a> ComponentInfo<'a> {
    /// Creates a new component info by parsing the given WebAssembly component bytes.
    pub fn new(mut bytes: &'a [u8]) -> Result<Self> {
        let mut parser = Parser::new(0);
        let mut validator = Validator::new();
        let mut types = Vec::new();
        let mut imports = Vec::new();
        let mut exports = Vec::new();

        validator.wasm_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });

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
                            if encoding != Encoding::Component {
                                bail!("file is not a WebAssembly component");
                            }

                            validator.version(num, encoding, &range)?;
                        }

                        Payload::TypeSection(_) => unreachable!(),
                        Payload::ImportSection(_) => unreachable!(),
                        Payload::FunctionSection(_) => unreachable!(),
                        Payload::TableSection(_) => unreachable!(),
                        Payload::MemorySection(_) => unreachable!(),
                        Payload::TagSection(_) => unreachable!(),
                        Payload::GlobalSection(_) => unreachable!(),
                        Payload::ExportSection(_) => unreachable!(),
                        Payload::StartSection { .. } => unreachable!(),
                        Payload::ElementSection(_) => unreachable!(),
                        Payload::DataCountSection { .. } => unreachable!(),
                        Payload::DataSection(_) => unreachable!(),
                        Payload::CodeSectionStart { .. } => unreachable!(),
                        Payload::CodeSectionEntry(_) => unreachable!(),

                        Payload::ComponentTypeSection(s) => {
                            validator.component_type_section(&s)?;

                            types.reserve(s.get_count() as usize);
                            for ty in s {
                                types.push(ty?);
                            }
                        }
                        Payload::ComponentImportSection(s) => {
                            validator.component_import_section(&s)?;

                            for import in s {
                                let import = import?;
                                if let ComponentTypeDef::Instance(_) = &types[import.ty as usize] {
                                    imports.push(import);
                                }
                            }
                        }
                        Payload::ComponentFunctionSection(s) => {
                            validator.component_function_section(&s)?;
                        }
                        Payload::ModuleSection { range, .. } => {
                            validator.module_section(&range)?;
                        }
                        Payload::ComponentSection { range, .. } => {
                            validator.component_section(&range)?;
                        }
                        Payload::InstanceSection(s) => {
                            validator.instance_section(&s)?;
                        }
                        Payload::ComponentExportSection(s) => {
                            validator.component_export_section(&s)?;

                            for export in s {
                                let export = export?;
                                match export.kind {
                                    ComponentExportKind::Type(index) => {
                                        if let ComponentTypeDef::Instance(_) =
                                            &types[index as usize]
                                        {
                                            exports.push(export);
                                        }
                                    }
                                    ComponentExportKind::Instance(_) => {
                                        todo!()
                                    }
                                    _ => {}
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
                            validator.end(offset)?;
                            break;
                        }
                    }
                }
                Chunk::NeedMoreData(_) => unreachable!(),
            }
        }

        Ok(Self {
            types,
            imports,
            exports,
        })
    }
}

/// Represents an interface decoder for WebAssembly components.
pub struct InterfaceDecoder<'a> {
    info: &'a ComponentInfo<'a>,
    name_map: HashMap<u32, &'a str>,
    exported_funcs: Vec<(&'a str, u32)>,
    interface: Interface,
    type_map: HashMap<u32, Type>,
    bool_ty: Option<TypeId>,
    string_ty: Option<TypeId>,
}

impl<'a> InterfaceDecoder<'a> {
    /// Creates a new interface decoder for the given type information,
    /// interface name, and instance type index.
    pub fn new(info: &'a ComponentInfo<'a>, name: &str, ty: u32) -> Result<Self> {
        let defs = match info
            .types
            .get(ty as usize)
            .ok_or_else(|| anyhow!("instance type index {} out of bounds", ty))?
        {
            ComponentTypeDef::Instance(defs) => defs,
            _ => bail!("type index {} is not an instance type", ty),
        };

        let mut index_map = HashMap::new();
        let mut type_count = 0;
        let mut name_map = HashMap::new();
        let mut exported_funcs = Vec::new();

        for ty in defs.iter() {
            match ty {
                InstanceType::Type(_) => {
                    bail!("unexpected local type in instance type definition");
                }
                InstanceType::OuterType { count, index } => {
                    if *count != 1 {
                        bail!("unexpected count for outer type alias in instance type definition");
                    }
                    index_map.insert(type_count, *index);
                    type_count += 1;
                }
                InstanceType::Export { name, ty } => {
                    let index = index_map[ty];
                    let ty = &info.types[index as usize];
                    match ty {
                        ComponentTypeDef::Interface(_) => {
                            name_map.insert(index, *name);
                        }
                        ComponentTypeDef::Function(_) => {
                            exported_funcs.push((*name, index));
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(Self {
            info,
            name_map,
            exported_funcs,
            interface: Interface {
                name: name.to_string(),
                ..Default::default()
            },
            type_map: HashMap::new(),
            bool_ty: None,
            string_ty: None,
        })
    }

    /// Consumes the decoder and returns the interface representation.
    pub fn finish(mut self) -> Result<Interface> {
        let funcs = std::mem::take(&mut self.exported_funcs);
        for (name, index) in funcs {
            let ty = match &self.info.types[index as usize] {
                ComponentTypeDef::Function(ty) => ty,
                _ => unreachable!(),
            };

            self.add_function(name, ty)?;
        }

        Ok(self.interface)
    }

    fn add_function(&mut self, name: &str, ty: &ComponentFuncType) -> Result<()> {
        validate_id(name)
            .with_context(|| format!("function name `{}` is not a valid identifier", name))?;

        let mut params = Vec::new();
        for (name, ty) in ty.params.iter() {
            params.push((name.unwrap().to_string(), self.decode_type(ty)?));
        }

        let mut results = Vec::new();
        if let InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) = &ty.result {
        } else {
            results.push(("".to_string(), self.decode_type(&ty.result)?));
        }

        self.interface.functions.push(Function {
            abi: Abi::Canonical,
            is_async: false,
            docs: Docs::default(),
            name: name.to_string(),
            kind: FunctionKind::Freestanding,
            params,
            results,
        });

        Ok(())
    }

    fn decode_type(&mut self, ty: &InterfaceTypeRef) -> Result<Type> {
        Ok(match ty {
            InterfaceTypeRef::Primitive(ty) => self.decode_primitive(*ty)?,
            InterfaceTypeRef::Type(index) => {
                if let Some(ty) = self.type_map.get(index) {
                    return Ok(*ty);
                }

                let name = self.name_map.get(index).map(ToString::to_string);

                if let Some(name) = name.as_deref() {
                    validate_id(name).with_context(|| {
                        format!("type name `{}` is not a valid identifier", name)
                    })?;
                }

                let ty = match &self.info.types[*index as usize] {
                    ComponentTypeDef::Interface(ty) => match ty {
                        InterfaceType::Primitive(ty) => self.decode_named_primitive(name, ty)?,
                        InterfaceType::Record(fields) => self.decode_record(name, fields)?,
                        InterfaceType::Variant(cases) => self.decode_variant(name, cases)?,
                        InterfaceType::List(ty) => {
                            let inner = self.decode_type(ty)?;
                            Type::Id(self.alloc_type(name, TypeDefKind::List(inner)))
                        }
                        InterfaceType::Tuple(tys) => self.decode_tuple(name, tys)?,
                        InterfaceType::Flags(names) => self.decode_flags(name, names)?,
                        InterfaceType::Enum(names) => self.decode_enum(name, names)?,
                        InterfaceType::Union(tys) => self.decode_union(name, tys)?,
                        InterfaceType::Option(ty) => self.decode_option(name, ty)?,
                        InterfaceType::Expected { ok, error } => {
                            self.decode_expected(name, ok, error)?
                        }
                    },
                    _ => unreachable!(),
                };

                self.type_map.insert(*index, ty);
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
        name: Option<String>,
        fields: &[(&str, InterfaceTypeRef)],
    ) -> Result<Type> {
        if name.is_none() {
            bail!("interface has an unnamed record type");
        }

        let record = Record {
            fields: fields
                .iter()
                .map(|(name, ty)| {
                    Ok(Field {
                        docs: Docs::default(),
                        name: name.to_string(),
                        ty: self.decode_type(ty)?,
                    })
                })
                .collect::<Result<_>>()?,
            kind: RecordKind::Other,
        };

        Ok(Type::Id(self.alloc_type(name, TypeDefKind::Record(record))))
    }

    fn decode_variant(
        &mut self,
        name: Option<String>,
        cases: &[wasmparser::VariantCase],
    ) -> Result<Type> {
        if name.is_none() {
            bail!("interface has an unnamed variant type");
        }

        let variant = Variant {
            cases: cases
                .iter()
                .map(|case| {
                    Ok(Case {
                        docs: Docs::default(),
                        name: case.name.to_string(),
                        ty: match case.ty {
                            InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) => None,
                            _ => Some(self.decode_type(&case.ty)?),
                        },
                    })
                })
                .collect::<Result<_>>()?,
            tag: Variant::infer_tag(cases.len()),
        };

        Ok(Type::Id(
            self.alloc_type(name, TypeDefKind::Variant(variant)),
        ))
    }

    fn decode_tuple(&mut self, name: Option<String>, tys: &[InterfaceTypeRef]) -> Result<Type> {
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

    fn decode_flags(&mut self, name: Option<String>, names: &[&str]) -> Result<Type> {
        let record = Record {
            fields: names
                .iter()
                .map(|name| {
                    Ok(Field {
                        docs: Docs::default(),
                        name: name.to_string(),
                        ty: self.decode_primitive(PrimitiveInterfaceType::Bool)?,
                    })
                })
                .collect::<Result<_>>()?,
            kind: RecordKind::Flags(None),
        };

        Ok(Type::Id(self.alloc_type(name, TypeDefKind::Record(record))))
    }

    fn decode_enum(&mut self, name: Option<String>, names: &[&str]) -> Result<Type> {
        let variant = Variant {
            cases: names
                .iter()
                .map(|name| {
                    Ok(Case {
                        docs: Docs::default(),
                        name: name.to_string(),
                        ty: None,
                    })
                })
                .collect::<Result<_>>()?,
            tag: Variant::infer_tag(names.len()),
        };

        Ok(Type::Id(
            self.alloc_type(name, TypeDefKind::Variant(variant)),
        ))
    }

    fn decode_union(&mut self, name: Option<String>, tys: &[InterfaceTypeRef]) -> Result<Type> {
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

    fn decode_option(&mut self, name: Option<String>, ty: &InterfaceTypeRef) -> Result<Type> {
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
        ok: &InterfaceTypeRef,
        error: &InterfaceTypeRef,
    ) -> Result<Type> {
        let variant = Variant {
            cases: vec![
                Case {
                    docs: Docs::default(),
                    name: "ok".to_string(),
                    ty: match ok {
                        InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) => None,
                        _ => Some(self.decode_type(ok)?),
                    },
                },
                Case {
                    docs: Docs::default(),
                    name: "err".to_string(),
                    ty: match error {
                        InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit) => None,
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
