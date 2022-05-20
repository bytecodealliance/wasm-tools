use super::*;

impl Component {
    /// Encode this Wasm component into bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        self.encoded().finish()
    }

    fn encoded(&self) -> wasm_encoder::Component {
        let mut component = wasm_encoder::Component::new();
        for section in &self.sections {
            section.encode(&mut component);
        }
        component
    }
}

impl Section {
    fn encode(&self, component: &mut wasm_encoder::Component) {
        match self {
            Section::Custom(sec) => sec.encode(component),
            Section::Type(sec) => sec.encode(component),
            Section::Import(sec) => sec.encode(component),
            Section::Func(sec) => sec.encode(component),
            Section::Core(module) => {
                let bytes = module.to_bytes();
                component.section(&wasm_encoder::RawSection {
                    id: wasm_encoder::ComponentSectionId::Module as u8,
                    data: &bytes,
                });
            }
            Section::Component(comp) => {
                let bytes = comp.to_bytes();
                component.section(&wasm_encoder::RawSection {
                    id: wasm_encoder::ComponentSectionId::Component as u8,
                    data: &bytes,
                });
            }
            Section::Instance(_) => todo!(),
            Section::Export(_) => todo!(),
            Section::Start(_) => todo!(),
            Section::Alias(_) => todo!(),
        }
    }
}

impl CustomSection {
    fn encode(&self, component: &mut wasm_encoder::Component) {
        component.section(&wasm_encoder::CustomSection {
            name: &self.name,
            data: &self.data,
        });
    }
}

impl TypeSection {
    fn encode(&self, component: &mut wasm_encoder::Component) {
        let mut sec = wasm_encoder::ComponentTypeSection::new();
        for ty in &self.types {
            ty.encode(sec.ty());
        }
        component.section(&sec);
    }
}

impl ImportSection {
    fn encode(&self, component: &mut wasm_encoder::Component) {
        let mut sec = wasm_encoder::ComponentImportSection::new();
        for imp in &self.imports {
            sec.import(&imp.name, imp.ty.index);
        }
        component.section(&sec);
    }
}

impl FuncSection {
    fn encode(&self, component: &mut wasm_encoder::Component) {
        let mut sec = wasm_encoder::ComponentFunctionSection::new();
        for func in &self.funcs {
            match func {
                Func::CanonLift {
                    func_ty,
                    options,
                    core_func,
                } => {
                    let options = translate_canon_opt(options);
                    sec.lift(*func_ty, *core_func, options);
                }
                Func::CanonLower {
                    options,
                    inter_func,
                } => {
                    let options = translate_canon_opt(options);
                    sec.lower(*inter_func, options);
                }
            }
        }
        component.section(&sec);
    }
}

impl Type {
    fn encode(&self, enc: wasm_encoder::TypeEncoder<'_>) {
        match self {
            Type::Module(mod_ty) => {
                let mut enc_mod_ty = wasm_encoder::ModuleType::new();
                for def in &mod_ty.defs {
                    match def {
                        ModuleTypeDef::TypeDef(crate::core::Type::Func(func_ty)) => {
                            enc_mod_ty.function(
                                func_ty.params.iter().copied(),
                                func_ty.results.iter().copied(),
                            );
                        }
                        ModuleTypeDef::Import(imp) => {
                            enc_mod_ty.import(
                                &imp.module,
                                &imp.field,
                                crate::core::encode::translate_entity_type(&imp.entity_type),
                            );
                        }
                        ModuleTypeDef::Export(name, ty) => {
                            enc_mod_ty.export(name, crate::core::encode::translate_entity_type(ty));
                        }
                    }
                }
                enc.module(&enc_mod_ty);
            }
            Type::Component(comp_ty) => {
                let mut enc_comp_ty = wasm_encoder::ComponentType::new();
                for def in &comp_ty.defs {
                    match def {
                        ComponentTypeDef::Import(imp) => {
                            enc_comp_ty.import(&imp.name, imp.ty.index);
                        }
                        ComponentTypeDef::Type(ty) => {
                            ty.encode(enc_comp_ty.ty());
                        }
                        ComponentTypeDef::Export { name, ty } => {
                            enc_comp_ty.export(name, ty.index);
                        }
                        ComponentTypeDef::Alias(Alias::Outer {
                            count,
                            i,
                            kind: OuterAliasKind::Type,
                        }) => {
                            enc_comp_ty.alias_outer_type(*count, *i);
                        }
                        ComponentTypeDef::Alias(_) => unreachable!(),
                    }
                }
                enc.component(&enc_comp_ty);
            }
            Type::Instance(inst_ty) => {
                let mut enc_inst_ty = wasm_encoder::InstanceType::new();
                for def in &inst_ty.defs {
                    match def {
                        InstanceTypeDef::Type(ty) => {
                            ty.encode(enc_inst_ty.ty());
                        }
                        InstanceTypeDef::Export { name, ty } => {
                            enc_inst_ty.export(name, ty.index);
                        }
                        InstanceTypeDef::Alias(Alias::Outer {
                            count,
                            i,
                            kind: OuterAliasKind::Type,
                        }) => {
                            enc_inst_ty.alias_outer_type(*count, *i);
                        }
                        InstanceTypeDef::Alias(_) => unreachable!(),
                    }
                }
                enc.instance(&enc_inst_ty);
            }
            Type::Func(func_ty) => {
                enc.function(
                    func_ty
                        .params
                        .iter()
                        .map(|p| translate_optional_named_type(p)),
                    func_ty.result,
                );
            }
            Type::Value(val_ty) => {
                enc.value(val_ty.0);
            }
            Type::Interface(ty) => {
                ty.encode(enc.interface_type());
            }
        }
    }
}

impl InterfaceType {
    fn encode(&self, enc: wasm_encoder::InterfaceTypeEncoder<'_>) {
        match self {
            InterfaceType::Primitive(ty) => enc.primitive(*ty),
            InterfaceType::Record(ty) => {
                enc.record(ty.fields.iter().map(|f| translate_named_type(f)));
            }
            InterfaceType::Variant(ty) => {
                enc.variant(
                    ty.cases
                        .iter()
                        .map(|(ty, default_to)| (ty.name.as_str(), ty.ty, default_to.clone())),
                );
            }
            InterfaceType::List(ty) => {
                enc.list(ty.elem_ty);
            }
            InterfaceType::Tuple(ty) => {
                enc.tuple(ty.fields.iter().copied());
            }
            InterfaceType::Flags(ty) => {
                enc.flags(ty.fields.iter().map(|f| f.as_str()));
            }
            InterfaceType::Enum(ty) => {
                enc.enum_type(ty.variants.iter().map(|v| v.as_str()));
            }
            InterfaceType::Union(ty) => {
                enc.union(ty.variants.iter().copied());
            }
            InterfaceType::Option(ty) => {
                enc.option(ty.inner_ty);
            }
            InterfaceType::Expected(ty) => {
                enc.expected(ty.ok_ty, ty.err_ty);
            }
        }
    }
}

fn translate_named_type(ty: &NamedType) -> (&str, InterfaceTypeRef) {
    (&ty.name, ty.ty)
}

fn translate_optional_named_type(ty: &OptionalNamedType) -> (Option<&str>, InterfaceTypeRef) {
    (ty.name.as_deref(), ty.ty)
}

fn translate_canon_opt(options: &[CanonOpt]) -> Vec<wasm_encoder::CanonicalOption> {
    options
        .iter()
        .map(|o| match o {
            CanonOpt::StringUtf8 => wasm_encoder::CanonicalOption::UTF8,
            CanonOpt::StringUtf16 => wasm_encoder::CanonicalOption::UTF16,
            CanonOpt::StringLatin1Utf16 => wasm_encoder::CanonicalOption::CompactUTF16,
            CanonOpt::Into { instance } => wasm_encoder::CanonicalOption::Into(*instance),
        })
        .collect()
}
