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
            Section::Import(_) => todo!(),
            Section::Func(_) => todo!(),
            Section::Core(_) => todo!(),
            Section::Component(_) => todo!(),
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
                                &imp.0,
                                &imp.1,
                                crate::core::encode::translate_entity_type(&imp.2),
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
                            enc_comp_ty.import(&imp.name, imp.ty);
                        }
                        ComponentTypeDef::Type(ty) => {
                            ty.encode(enc_comp_ty.ty());
                        }
                        ComponentTypeDef::Export { name, ty } => {
                            enc_comp_ty.export(name, *ty);
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
                            enc_inst_ty.export(name, *ty);
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
            }
            Type::Func(func_ty) => {
                enc.function(
                    func_ty.params.iter().map(|p| translate_named_type(p)),
                    translate_inter_type(func_ty.result),
                );
            }
            Type::Value(val_ty) => {
                enc.value(translate_inter_type(val_ty.0));
            }
            Type::Compound(comp_ty) => {
                comp_ty.encode(enc.compound());
            }
        }
    }
}

impl CompoundType {
    fn encode(&self, enc: wasm_encoder::CompoundTypeEncoder<'_>) {
        match self {
            CompoundType::Record(ty) => {
                enc.record(ty.fields.iter().map(|f| translate_named_type(f)));
            }
            CompoundType::Variant(ty) => {
                enc.variant(ty.cases.iter().map(|f| translate_named_type(f)), ty.default);
            }
            CompoundType::List(ty) => {
                enc.list(translate_inter_type(ty.elem_ty));
            }
            CompoundType::Tuple(ty) => {
                enc.tuple(ty.fields.iter().map(|ty| translate_inter_type(*ty)));
            }
            CompoundType::Flags(ty) => {
                enc.flags(ty.fields.iter().map(|f| f.as_str()));
            }
            CompoundType::Enum(ty) => {
                enc.enum_type(ty.variants.iter().map(|v| v.as_str()));
            }
            CompoundType::Union(ty) => {
                enc.union(ty.variants.iter().map(|ty| translate_inter_type(*ty)));
            }
            CompoundType::Optional(ty) => {
                enc.optional(translate_inter_type(ty.inner_ty));
            }
            CompoundType::Expected(ty) => {
                enc.expected(
                    translate_inter_type(ty.ok_ty),
                    translate_inter_type(ty.err_ty),
                );
            }
        }
    }
}

fn translate_named_type(ty: &NamedType) -> (&str, wasm_encoder::InterfaceType) {
    (&ty.name, translate_inter_type(ty.ty))
}

fn translate_inter_type(ty: InterType) -> wasm_encoder::InterfaceType {
    match ty {
        InterType::Compound(i) => wasm_encoder::InterfaceType::Compound(i),
        InterType::Unit => wasm_encoder::InterfaceType::Unit,
        InterType::Bool => wasm_encoder::InterfaceType::Bool,
        InterType::S8 => wasm_encoder::InterfaceType::S8,
        InterType::U8 => wasm_encoder::InterfaceType::U8,
        InterType::S16 => wasm_encoder::InterfaceType::S16,
        InterType::U16 => wasm_encoder::InterfaceType::U16,
        InterType::S32 => wasm_encoder::InterfaceType::S32,
        InterType::U32 => wasm_encoder::InterfaceType::U32,
        InterType::S64 => wasm_encoder::InterfaceType::S64,
        InterType::U64 => wasm_encoder::InterfaceType::U64,
        InterType::F32 => wasm_encoder::InterfaceType::F32,
        InterType::F64 => wasm_encoder::InterfaceType::F64,
        InterType::Char => wasm_encoder::InterfaceType::Char,
        InterType::String => wasm_encoder::InterfaceType::String,
    }
}
