use std::collections::HashMap;

use anyhow::{bail, Result};
use wasm_encoder::{
    Alias, Component, ComponentExternName,
    ComponentOuterAliasKind, ComponentTypeRef, ComponentTypeSection, ComponentValType, FuncType,
    ImplementationImport, ImportMetadata, InstanceType, PrimitiveValType,
};
use wasmparser::{
    Chunk, ComponentImportSectionReader, ComponentType, ComponentTypeSectionReader, Encoding,
    Parser, Payload, SubType, TypeBounds, VariantCase, ComponentAliasSectionReader, ComponentAlias,
};

pub struct Lock {}

pub struct State {
    _encoding: Encoding,
    _name: Option<Naming>,
    _core: CoreState,
    _component: ComponentState,
}

#[derive(Default)]
struct ComponentState {
    _types: u32,
    _funcs: u32,
    _instances: u32,
    _components: u32,
    _values: u32,
    _type_names: HashMap<u32, Naming>,
    _func_names: HashMap<u32, Naming>,
    _component_names: HashMap<u32, Naming>,
    _instance_names: HashMap<u32, Naming>,
    _value_names: HashMap<u32, Naming>,
}

#[derive(Default)]
struct CoreState {
    _types: Vec<Option<SubType>>,
    _funcs: u32,
    _memories: u32,
    _tags: u32,
    _globals: u32,
    _tables: u32,
    _labels: u32,
    _modules: u32,
    _instances: u32,
    _func_names: HashMap<u32, Naming>,
    _local_names: HashMap<(u32, u32), Naming>,
    _label_names: HashMap<(u32, u32), Naming>,
    _type_names: HashMap<u32, Naming>,
    _table_names: HashMap<u32, Naming>,
    _memory_names: HashMap<u32, Naming>,
    _global_names: HashMap<u32, Naming>,
    _element_names: HashMap<u32, Naming>,
    _data_names: HashMap<u32, Naming>,
    _module_names: HashMap<u32, Naming>,
    _instance_names: HashMap<u32, Naming>,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    Implementation,
    Interface,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub name: String,
    pub kind: ImportKind,
    pub aliases: Vec<String>
}

impl Import {
    pub fn new(name: String, kind: ImportKind) -> Self {
        Self { name, kind, aliases: Vec::new() }
    }
}

#[derive(Debug)]
struct Naming {
    _identifier: Option<String>,
    _name: String,
}

impl Lock {
    pub fn new() -> Self {
        Self {}
    }
    pub fn parse_type_imports(
        &mut self,
        _states: &mut Vec<State>,
        parser: ComponentTypeSectionReader,
        component: &mut Component,
    ) -> Result<()> {
        let mut type_section = ComponentTypeSection::new();
        for ty in parser.into_iter_with_offsets() {
            let (_offset, ty) = ty?;
            match ty {
                ComponentType::Instance(decls) => {
                    let mut instance = InstanceType::new();
                    for decl in decls.into_vec() {
                        match decl {
                            wasmparser::InstanceTypeDeclaration::CoreType(_) => todo!(),
                            wasmparser::InstanceTypeDeclaration::Type(instance_ty) => {
                                match instance_ty {
                                    ComponentType::Defined(def) => {
                                        match def {
                                            wasmparser::ComponentDefinedType::Primitive(prim) => {
                                                match prim {
                                                    wasmparser::PrimitiveValType::Bool => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Bool),
                                                    wasmparser::PrimitiveValType::S8 => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S8),
                                                    wasmparser::PrimitiveValType::U8 => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U8),
                                                    wasmparser::PrimitiveValType::S16 => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S16),
                                                    wasmparser::PrimitiveValType::U16 => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U16),
                                                    wasmparser::PrimitiveValType::S32 => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S32),
                                                    wasmparser::PrimitiveValType::U32 => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U32),
                                                    wasmparser::PrimitiveValType::S64 => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S64),
                                                    wasmparser::PrimitiveValType::U64 => instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U64),
                                                    wasmparser::PrimitiveValType::Float32 => {
                                                        todo!()
                                                    }
                                                    wasmparser::PrimitiveValType::Float64 => {
                                                        todo!()
                                                    }
                                                    wasmparser::PrimitiveValType::Char => todo!(),
                                                    wasmparser::PrimitiveValType::String => todo!(),
                                                }
                                            }
                                            wasmparser::ComponentDefinedType::Record(items) => {
                                                let mut fields = Vec::new();
                                                for (key, val) in items.iter() {
                                                    match val {
                                                wasmparser::ComponentValType::Primitive(prim) => {
                                                  match prim {
                                                    wasmparser::PrimitiveValType::Bool => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::Bool))),
                                                    wasmparser::PrimitiveValType::S8 => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::S8))),
                                                    wasmparser::PrimitiveValType::U8 => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::U8))),
                                                    wasmparser::PrimitiveValType::S16 => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::S16))),
                                                    wasmparser::PrimitiveValType::U16 => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::U16))),
                                                    wasmparser::PrimitiveValType::S32 => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::S32))),
                                                    wasmparser::PrimitiveValType::U32 => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::U32))),
                                                    wasmparser::PrimitiveValType::S64 => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::S64))),
                                                    wasmparser::PrimitiveValType::U64 => fields.push((*key, ComponentValType::Primitive(PrimitiveValType::U64))),
                                                    wasmparser::PrimitiveValType::Float32 => todo!(),
                                                    wasmparser::PrimitiveValType::Float64 => todo!(),
                                                    wasmparser::PrimitiveValType::Char => todo!(),
                                                    wasmparser::PrimitiveValType::String => todo!(),
                                                  }
                                                },
                                                wasmparser::ComponentValType::Type(index) => fields.push((*key, ComponentValType::Type(*index))),
                                            }
                                                }
                                                instance.ty().defined_type().record(fields);
                                            }
                                            wasmparser::ComponentDefinedType::Variant(_) => todo!(),
                                            wasmparser::ComponentDefinedType::List(ty) => {
                                                match ty {
                                                    wasmparser::ComponentValType::Primitive(
                                                        prim,
                                                    ) => match prim {
                                                        wasmparser::PrimitiveValType::Bool => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Bool)
                                                        }
                                                        wasmparser::PrimitiveValType::S8 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S8)
                                                        }
                                                        wasmparser::PrimitiveValType::U8 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U8)
                                                        }
                                                        wasmparser::PrimitiveValType::S16 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S16)
                                                        }
                                                        wasmparser::PrimitiveValType::U16 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U16)
                                                        }
                                                        wasmparser::PrimitiveValType::S32 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S32)
                                                        }
                                                        wasmparser::PrimitiveValType::U32 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U32)
                                                        }
                                                        wasmparser::PrimitiveValType::S64 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S64)
                                                        }
                                                        wasmparser::PrimitiveValType::U64 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U64)
                                                        }
                                                        wasmparser::PrimitiveValType::Float32 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Float32)
                                                        }
                                                        wasmparser::PrimitiveValType::Float64 => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Float64)
                                                        }
                                                        wasmparser::PrimitiveValType::Char => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Char)
                                                        }
                                                        wasmparser::PrimitiveValType::String => {
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::String)
                                                        }
                                                    },
                                                    wasmparser::ComponentValType::Type(index) => {
                                                        instance
                                                            .ty()
                                                            .defined_type()
                                                            .list(ComponentValType::Type(index));
                                                    }
                                                }
                                            }
                                            wasmparser::ComponentDefinedType::Tuple(tys) => {
                                                let mut tuple = Vec::new();
                                                for item in tys.iter() {
                                                    match item {
                                                wasmparser::ComponentValType::Primitive(prim) => {
                                                  match prim {
                                                    wasmparser::PrimitiveValType::Bool => tuple.push(ComponentValType::Primitive(PrimitiveValType::Bool)),
                                                    wasmparser::PrimitiveValType::S8 => tuple.push(ComponentValType::Primitive(PrimitiveValType::S8)),
                                                    wasmparser::PrimitiveValType::U8 => tuple.push(ComponentValType::Primitive(PrimitiveValType::U8)),
                                                    wasmparser::PrimitiveValType::S16 => tuple.push(ComponentValType::Primitive(PrimitiveValType::S16)),
                                                    wasmparser::PrimitiveValType::U16 => tuple.push(ComponentValType::Primitive(PrimitiveValType::U16)),
                                                    wasmparser::PrimitiveValType::S32 => tuple.push(ComponentValType::Primitive(PrimitiveValType::S32)),
                                                    wasmparser::PrimitiveValType::U32 => tuple.push(ComponentValType::Primitive(PrimitiveValType::U32)),
                                                    wasmparser::PrimitiveValType::S64 => tuple.push(ComponentValType::Primitive(PrimitiveValType::S64)),
                                                    wasmparser::PrimitiveValType::U64 => tuple.push(ComponentValType::Primitive(PrimitiveValType::U64)),
                                                    wasmparser::PrimitiveValType::Float32 => tuple.push(ComponentValType::Primitive(PrimitiveValType::Float32)),
                                                    wasmparser::PrimitiveValType::Float64 => tuple.push(ComponentValType::Primitive(PrimitiveValType::Float64)),
                                                    wasmparser::PrimitiveValType::Char => tuple.push(ComponentValType::Primitive(PrimitiveValType::Char)),
                                                    wasmparser::PrimitiveValType::String => tuple.push(ComponentValType::Primitive(PrimitiveValType::String)),
                                                  }
                                                },
                                                wasmparser::ComponentValType::Type(index) => tuple.push(ComponentValType::Type(*index)),
                                            }
                                                }
                                                instance.ty().defined_type().tuple(tuple);
                                            }
                                            wasmparser::ComponentDefinedType::Flags(flags) => {
                                                let mut names = Vec::new();
                                                for flag in flags.iter() {
                                                    names.push(*flag);
                                                }
                                                instance.ty().defined_type().flags(names);
                                            }
                                            wasmparser::ComponentDefinedType::Enum(kinds) => {
                                                let mut variants = Vec::new();
                                                for kind in kinds.iter() {
                                                    variants.push(*kind);
                                                }
                                                instance.ty().defined_type().enum_type(variants);
                                            }
                                            wasmparser::ComponentDefinedType::Union(_) => todo!(),
                                            wasmparser::ComponentDefinedType::Option(_) => todo!(),
                                            wasmparser::ComponentDefinedType::Result {
                                                ok,
                                                err,
                                            } => {
                                                if let Some(ok_val_type) = ok {
                                                    let ok_val = match ok_val_type {
                                              wasmparser::ComponentValType::Primitive(prim) => {
                                                match prim {
                                                  wasmparser::PrimitiveValType::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
                                                  wasmparser::PrimitiveValType::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
                                                  wasmparser::PrimitiveValType::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
                                                  wasmparser::PrimitiveValType::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
                                                  wasmparser::PrimitiveValType::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
                                                  wasmparser::PrimitiveValType::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
                                                  wasmparser::PrimitiveValType::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
                                                  wasmparser::PrimitiveValType::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
                                                  wasmparser::PrimitiveValType::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
                                                  wasmparser::PrimitiveValType::Float32 => todo!(),
                                                  wasmparser::PrimitiveValType::Float64 => todo!(),
                                                  wasmparser::PrimitiveValType::Char => todo!(),
                                                  wasmparser::PrimitiveValType::String => todo!(),
                                                }
                                              },
                                              wasmparser::ComponentValType::Type(index) => {
                                                ComponentValType::Type(index)
                                              }
                                            };
                                                    if let Some(err_val_type) = err {
                                                        let err_val = match err_val_type {
                                                wasmparser::ComponentValType::Primitive(prim) => {
                                                  match prim {
                                                    wasmparser::PrimitiveValType::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
                                                    wasmparser::PrimitiveValType::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
                                                    wasmparser::PrimitiveValType::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
                                                    wasmparser::PrimitiveValType::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
                                                    wasmparser::PrimitiveValType::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
                                                    wasmparser::PrimitiveValType::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
                                                    wasmparser::PrimitiveValType::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
                                                    wasmparser::PrimitiveValType::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
                                                    wasmparser::PrimitiveValType::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
                                                    wasmparser::PrimitiveValType::Float32 => ComponentValType::Primitive(PrimitiveValType::Float32),
                                                    wasmparser::PrimitiveValType::Float64 => ComponentValType::Primitive(PrimitiveValType::Float64),
                                                    wasmparser::PrimitiveValType::Char => ComponentValType::Primitive(PrimitiveValType::Char),
                                                    wasmparser::PrimitiveValType::String => ComponentValType::Primitive(PrimitiveValType::String),
                                                  }
                                                },
                                                wasmparser::ComponentValType::Type(index) => {
                                                  ComponentValType::Type(index)
                                                }
                                              };
                                                        instance
                                                            .ty()
                                                            .defined_type()
                                                            .result(Some(ok_val), Some(err_val));
                                                    }
                                                }
                                            }
                                            wasmparser::ComponentDefinedType::Own(_) => todo!(),
                                            wasmparser::ComponentDefinedType::Borrow(_) => todo!(),
                                        };
                                    }
                                    ComponentType::Func(func) => {
                                        let params =
                                            func.params.iter().map(|(name, ty)| match ty {
                                                wasmparser::ComponentValType::Primitive(prim) => {
                                                    match prim {
                                                        wasmparser::PrimitiveValType::Bool => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::Bool,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::S8 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S8,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::U8 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::U8,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::S16 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S16,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::U16 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::U16,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::S32 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S32,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::U32 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::U32,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::S64 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S64,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::U64 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::U64,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::Float32 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::Float32,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::Float64 => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::Float64,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::Char => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::Char,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::String => (
                                                            name.to_owned(),
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::String,
                                                            ),
                                                        ),
                                                    }
                                                }
                                                wasmparser::ComponentValType::Type(index) => (
                                                    name.to_owned(),
                                                    ComponentValType::Type(*index),
                                                ),
                                            });
                                        match func.results {
                                            wasmparser::ComponentFuncResult::Unnamed(ty) => {
                                                let result = match ty {
                                                    wasmparser::ComponentValType::Primitive(
                                                        prim,
                                                    ) => match prim {
                                                        wasmparser::PrimitiveValType::Bool => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::Bool,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::S8 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S8,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::U8 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S8,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::S16 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S16,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::U16 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S16,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::S32 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S32,
                                                            )
                                                        }

                                                        wasmparser::PrimitiveValType::U32 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::U32,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::S64 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::S64,
                                                            )
                                                        }

                                                        wasmparser::PrimitiveValType::U64 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::U64,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::Float32 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::Float32,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::Float64 => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::Float64,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::Char => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::Char,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::String => {
                                                            ComponentValType::Primitive(
                                                                PrimitiveValType::String,
                                                            )
                                                        }
                                                    },
                                                    wasmparser::ComponentValType::Type(index) => {
                                                        ComponentValType::Type(index)
                                                    }
                                                };
                                                instance
                                                    .ty()
                                                    .function()
                                                    .params(params)
                                                    .result(result);
                                            }
                                            wasmparser::ComponentFuncResult::Named(ty) => {
                                                let mut results = Vec::new();
                                                for (name, val) in ty.iter() {
                                                    match val {
                                                    wasmparser::ComponentValType::Primitive(prim) => {
                                                      match prim {
                                                        wasmparser::PrimitiveValType::Bool => results.push((*name, ComponentValType::Primitive(PrimitiveValType::Bool))),
                                                        wasmparser::PrimitiveValType::S8 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::S8))),
                                                        wasmparser::PrimitiveValType::U8 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::U8))),
                                                        wasmparser::PrimitiveValType::S16 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::S16))),
                                                        wasmparser::PrimitiveValType::U16 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::U16))),
                                                        wasmparser::PrimitiveValType::S32 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::S32))),
                                                        wasmparser::PrimitiveValType::U32 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::U32))),
                                                        wasmparser::PrimitiveValType::S64 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::S64))),
                                                        wasmparser::PrimitiveValType::U64 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::U64))),
                                                        wasmparser::PrimitiveValType::Float32 => todo!(),
                                                        wasmparser::PrimitiveValType::Float64 => todo!(),
                                                        wasmparser::PrimitiveValType::Char => todo!(),
                                                        wasmparser::PrimitiveValType::String => todo!(),
                                                    }
                                                    },
                                                    wasmparser::ComponentValType::Type(index) => results.push((*name, ComponentValType::Type(*index))),
                                                }
                                                }
                                                instance
                                                    .ty()
                                                    .function()
                                                    .params(params)
                                                    .results(results);
                                            }
                                        };
                                    }
                                    ComponentType::Component(_) => {
                                      dbg!("COMPONENT");
                                    }
                                    ComponentType::Instance(_) => {
                                      dbg!("INSTANCE");
                                    },
                                    ComponentType::Resource { rep, dtor } => {
                                      dbg!("RESOURCE");
                                    },
                                }
                            }
                            wasmparser::InstanceTypeDeclaration::Alias(ty) => {
                                let alias = match ty {
                                    wasmparser::ComponentAlias::InstanceExport {
                                        kind,
                                        instance_index,
                                        name,
                                    } => todo!(),
                                    wasmparser::ComponentAlias::CoreInstanceExport {
                                        kind,
                                        instance_index,
                                        name,
                                    } => todo!(),
                                    wasmparser::ComponentAlias::Outer { kind, count, index } => {
                                        Alias::Outer {
                                            kind: match kind {
                                                wasmparser::ComponentOuterAliasKind::CoreModule => {
                                                    ComponentOuterAliasKind::CoreModule
                                                }
                                                wasmparser::ComponentOuterAliasKind::CoreType => {
                                                    ComponentOuterAliasKind::CoreType
                                                }
                                                wasmparser::ComponentOuterAliasKind::Type => {
                                                    ComponentOuterAliasKind::Type
                                                }
                                                wasmparser::ComponentOuterAliasKind::Component => {
                                                    ComponentOuterAliasKind::Component
                                                }
                                            },
                                            count,
                                            index,
                                        }
                                    }
                                };
                                instance.alias(alias);
                            }
                            wasmparser::InstanceTypeDeclaration::Export { name, ty } => {
                                let export_name = match name {
                                    wasmparser::ComponentExternName::Kebab(name) => {
                                        ComponentExternName::Kebab(name)
                                    }
                                    wasmparser::ComponentExternName::Interface(name) => {
                                        ComponentExternName::Interface(name)
                                    }
                                    wasmparser::ComponentExternName::Implementation(name) => {
                                        ComponentExternName::Implementation(match name {
                                            wasmparser::ImplementationImport::Url(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                    range,
                                                },
                                            ) => ImplementationImport::Url(ImportMetadata {
                                                name,
                                                location,
                                                integrity: Some(integrity),
                                                range: Some(range),
                                            }),
                                            wasmparser::ImplementationImport::Relative(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                    range,
                                                },
                                            ) => ImplementationImport::Url(ImportMetadata {
                                                name,
                                                location,
                                                integrity: Some(integrity),
                                                range: Some(range),
                                            }),
                                            wasmparser::ImplementationImport::Locked(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                    range,
                                                },
                                            ) => ImplementationImport::Url(ImportMetadata {
                                                name,
                                                location,
                                                integrity: Some(integrity),
                                                range: Some(range),
                                            }),
                                            wasmparser::ImplementationImport::Unlocked(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                    range,
                                                },
                                            ) => ImplementationImport::Url(ImportMetadata {
                                                name,
                                                location,
                                                integrity: Some(integrity),
                                                range: Some(range),
                                            }),
                                        })
                                    }
                                };
                                let export_type = match ty {
                                    wasmparser::ComponentTypeRef::Module(i) => {
                                        ComponentTypeRef::Module(i)
                                    }
                                    wasmparser::ComponentTypeRef::Func(i) => {
                                        ComponentTypeRef::Func(i)
                                    }
                                    wasmparser::ComponentTypeRef::Value(ty) => match ty {
                                        wasmparser::ComponentValType::Primitive(prim) => match prim
                                        {
                                            wasmparser::PrimitiveValType::Bool => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::Bool,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::S8 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::S8,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::U8 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::U8,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::S16 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::S16,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::U16 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::U16,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::S32 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::S32,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::U32 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::U32,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::S64 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::S64,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::U64 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::U64,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::Float32 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::Float32,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::Float64 => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::Float64,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::Char => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::Char,
                                                    ),
                                                )
                                            }
                                            wasmparser::PrimitiveValType::String => {
                                                ComponentTypeRef::Value(
                                                    ComponentValType::Primitive(
                                                        PrimitiveValType::String,
                                                    ),
                                                )
                                            }
                                        },
                                        wasmparser::ComponentValType::Type(i) => {
                                            wasm_encoder::ComponentTypeRef::Type(
                                                wasm_encoder::TypeBounds::Eq(i),
                                            )
                                        }
                                    },
                                    wasmparser::ComponentTypeRef::Type(bounds) => match bounds {
                                        TypeBounds::Eq(i) => {
                                            ComponentTypeRef::Type(wasm_encoder::TypeBounds::Eq(i))
                                        }
                                        TypeBounds::SubResource => ComponentTypeRef::Type(
                                            wasm_encoder::TypeBounds::SubResource,
                                        ),
                                    },
                                    wasmparser::ComponentTypeRef::Instance(i) => {
                                        ComponentTypeRef::Instance(i)
                                    }
                                    wasmparser::ComponentTypeRef::Component(i) => {
                                        ComponentTypeRef::Component(i)
                                    }
                                };
                                dbg!(&export_name, &export_type);
                                instance.export(export_name, export_type);
                            }
                        }
                    }
                    type_section.instance(&instance);
                }
                ComponentType::Defined(def) => {
                  match def {
                    wasmparser::ComponentDefinedType::Primitive(prim) => match prim {
                        wasmparser::PrimitiveValType::Bool => type_section.defined_type().primitive(PrimitiveValType::Bool),
                        wasmparser::PrimitiveValType::S8 => type_section.defined_type().primitive(PrimitiveValType::S8),
                        wasmparser::PrimitiveValType::U8 => type_section.defined_type().primitive(PrimitiveValType::U8),
                        wasmparser::PrimitiveValType::S16 => type_section.defined_type().primitive(PrimitiveValType::S16),
                        wasmparser::PrimitiveValType::U16 => type_section.defined_type().primitive(PrimitiveValType::U16),
                        wasmparser::PrimitiveValType::S32 => type_section.defined_type().primitive(PrimitiveValType::S32),
                        wasmparser::PrimitiveValType::U32 => type_section.defined_type().primitive(PrimitiveValType::U32),
                        wasmparser::PrimitiveValType::S64 => type_section.defined_type().primitive(PrimitiveValType::S64),
                        wasmparser::PrimitiveValType::U64 => type_section.defined_type().primitive(PrimitiveValType::U64),
                        wasmparser::PrimitiveValType::Float32 => type_section.defined_type().primitive(PrimitiveValType::Float32),
                        wasmparser::PrimitiveValType::Float64 => type_section.defined_type().primitive(PrimitiveValType::Float64),
                        wasmparser::PrimitiveValType::Char => type_section.defined_type().primitive(PrimitiveValType::Char),
                        wasmparser::PrimitiveValType::String => type_section.defined_type().primitive(PrimitiveValType::String),
                    },
                    wasmparser::ComponentDefinedType::Record(rec) => {
                      let record = rec.iter().map(|(name, ty)| {
                        match ty {
                            wasmparser::ComponentValType::Primitive(prim) => match prim {
                              wasmparser::PrimitiveValType::Bool => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::Bool,
                                ),
                            ),
                            wasmparser::PrimitiveValType::S8 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::S8,
                                ),
                            ),
                            wasmparser::PrimitiveValType::U8 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::U8,
                                ),
                            ),
                            wasmparser::PrimitiveValType::S16 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::S16,
                                ),
                            ),
                            wasmparser::PrimitiveValType::U16 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::U16,
                                ),
                            ),
                            wasmparser::PrimitiveValType::S32 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::S32,
                                ),
                            ),
                            wasmparser::PrimitiveValType::U32 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::U32,
                                ),
                            ),
                            wasmparser::PrimitiveValType::S64 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::S64,
                                ),
                            ),
                            wasmparser::PrimitiveValType::U64 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::U64,
                                ),
                            ),
                            wasmparser::PrimitiveValType::Float32 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::Float32,
                                ),
                            ),
                            wasmparser::PrimitiveValType::Float64 => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::Float64,
                                ),
                            ),
                            wasmparser::PrimitiveValType::Char => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::Char,
                                ),
                            ),
                            wasmparser::PrimitiveValType::String => (
                                name.to_owned(),
                                ComponentValType::Primitive(
                                    PrimitiveValType::String,
                                ),
                            ),
                            },
                            wasmparser::ComponentValType::Type(_) => todo!(),
                        }
                      });
                      type_section.defined_type().record(record);
                    },
                    wasmparser::ComponentDefinedType::Variant(variant) => {
                      let cases = variant.iter().map(|VariantCase { name, ty, refines}| {
                        if let Some(ty) = ty {
                          return match ty {
                            wasmparser::ComponentValType::Primitive(prim) => match prim {
                                wasmparser::PrimitiveValType::Bool => (*name, Some(ComponentValType::Primitive(PrimitiveValType::Bool)), *refines),
                                wasmparser::PrimitiveValType::S8 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::S8)), *refines),
                                wasmparser::PrimitiveValType::U8 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::U8)), *refines),
                                wasmparser::PrimitiveValType::S16 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::S16)), *refines),
                                wasmparser::PrimitiveValType::U16 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::U16)), *refines),
                                wasmparser::PrimitiveValType::S32 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::S32)), *refines),
                                wasmparser::PrimitiveValType::U32 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::U32)), *refines),
                                wasmparser::PrimitiveValType::S64 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::S64)), *refines),
                                wasmparser::PrimitiveValType::U64 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::U64)), *refines),
                                wasmparser::PrimitiveValType::Float32 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::Float32)), *refines),
                                wasmparser::PrimitiveValType::Float64 => (*name, Some(ComponentValType::Primitive(PrimitiveValType::Float64)), *refines),
                                wasmparser::PrimitiveValType::Char => (*name, Some(ComponentValType::Primitive(PrimitiveValType::Char)), *refines),
                                wasmparser::PrimitiveValType::String => (*name, Some(ComponentValType::Primitive(PrimitiveValType::String)), *refines),
                            },
                            wasmparser::ComponentValType::Type(i) => (*name, Some(ComponentValType::Type(*i)), *refines),
                          }
                        } 
                        (*name, None, *refines)
                      });
                      type_section.defined_type().variant(cases)
                    },
                    wasmparser::ComponentDefinedType::List(ty) => {
                      let ty = match ty {
                        wasmparser::ComponentValType::Primitive(prim) => match prim {
                            wasmparser::PrimitiveValType::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
                            wasmparser::PrimitiveValType::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
                            wasmparser::PrimitiveValType::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
                            wasmparser::PrimitiveValType::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
                            wasmparser::PrimitiveValType::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
                            wasmparser::PrimitiveValType::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
                            wasmparser::PrimitiveValType::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
                            wasmparser::PrimitiveValType::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
                            wasmparser::PrimitiveValType::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
                            wasmparser::PrimitiveValType::Float32 => ComponentValType::Primitive(PrimitiveValType::Float32),
                            wasmparser::PrimitiveValType::Float64 => ComponentValType::Primitive(PrimitiveValType::Float64),
                            wasmparser::PrimitiveValType::Char => ComponentValType::Primitive(PrimitiveValType::Char),
                            wasmparser::PrimitiveValType::String => ComponentValType::Primitive(PrimitiveValType::String),
                        },
                        wasmparser::ComponentValType::Type(i) => ComponentValType::Type(i),
                      };
                      type_section.defined_type().list(ty);
                    },
                    wasmparser::ComponentDefinedType::Tuple(tuple) => {
                      let types = tuple.iter().map(|ty| match ty {
                        wasmparser::ComponentValType::Primitive(prim) => match prim {
                          wasmparser::PrimitiveValType::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
                            wasmparser::PrimitiveValType::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
                            wasmparser::PrimitiveValType::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
                            wasmparser::PrimitiveValType::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
                            wasmparser::PrimitiveValType::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
                            wasmparser::PrimitiveValType::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
                            wasmparser::PrimitiveValType::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
                            wasmparser::PrimitiveValType::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
                            wasmparser::PrimitiveValType::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
                            wasmparser::PrimitiveValType::Float32 => ComponentValType::Primitive(PrimitiveValType::Float32),
                            wasmparser::PrimitiveValType::Float64 => ComponentValType::Primitive(PrimitiveValType::Float64),
                            wasmparser::PrimitiveValType::Char => ComponentValType::Primitive(PrimitiveValType::Char),
                            wasmparser::PrimitiveValType::String => ComponentValType::Primitive(PrimitiveValType::String),
                        },
                        wasmparser::ComponentValType::Type(i) => ComponentValType::Type(*i),
                      });
                      type_section.defined_type().tuple(types);
                    },
                    wasmparser::ComponentDefinedType::Flags(flags) => {
                      type_section.defined_type().flags(flags.iter().map(|f| *f));
                    },
                    wasmparser::ComponentDefinedType::Enum(tags) => {
                      type_section.defined_type().enum_type(tags.iter().map(|f| *f));

                    },
                    wasmparser::ComponentDefinedType::Union(_) => todo!(),
                    wasmparser::ComponentDefinedType::Option(_) => todo!(),
                    wasmparser::ComponentDefinedType::Result { ok, err } => {
                      if let Some(ok) = ok {
                        let ok_type = match ok {
                          wasmparser::ComponentValType::Primitive(prim) => match prim {
                            wasmparser::PrimitiveValType::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
                            wasmparser::PrimitiveValType::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
                            wasmparser::PrimitiveValType::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
                            wasmparser::PrimitiveValType::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
                            wasmparser::PrimitiveValType::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
                            wasmparser::PrimitiveValType::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
                            wasmparser::PrimitiveValType::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
                            wasmparser::PrimitiveValType::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
                            wasmparser::PrimitiveValType::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
                            wasmparser::PrimitiveValType::Float32 => ComponentValType::Primitive(PrimitiveValType::Float32),
                            wasmparser::PrimitiveValType::Float64 => ComponentValType::Primitive(PrimitiveValType::Float64),
                            wasmparser::PrimitiveValType::Char => ComponentValType::Primitive(PrimitiveValType::Char),
                            wasmparser::PrimitiveValType::String => ComponentValType::Primitive(PrimitiveValType::String),
                        },
                          wasmparser::ComponentValType::Type(i) => ComponentValType::Type(i),
                        };
                        if let Some(err) = err {
                          let err_type = match err {
                            wasmparser::ComponentValType::Primitive(prim) => match prim {
                              wasmparser::PrimitiveValType::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
                              wasmparser::PrimitiveValType::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
                              wasmparser::PrimitiveValType::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
                              wasmparser::PrimitiveValType::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
                              wasmparser::PrimitiveValType::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
                              wasmparser::PrimitiveValType::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
                              wasmparser::PrimitiveValType::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
                              wasmparser::PrimitiveValType::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
                              wasmparser::PrimitiveValType::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
                              wasmparser::PrimitiveValType::Float32 => ComponentValType::Primitive(PrimitiveValType::Float32),
                              wasmparser::PrimitiveValType::Float64 => ComponentValType::Primitive(PrimitiveValType::Float64),
                              wasmparser::PrimitiveValType::Char => ComponentValType::Primitive(PrimitiveValType::Char),
                              wasmparser::PrimitiveValType::String => ComponentValType::Primitive(PrimitiveValType::String),
                          },
                            wasmparser::ComponentValType::Type(i) => ComponentValType::Type(i),
                          };
                          type_section.defined_type().result(Some(ok_type), Some(err_type));
                        }
                          type_section.defined_type().result(Some(ok_type), None);
                      }
                          type_section.defined_type().result(None, None);
                    },
                    wasmparser::ComponentDefinedType::Own(_) => todo!(),
                    wasmparser::ComponentDefinedType::Borrow(_) => todo!(),
                }
                },
                ComponentType::Func(f) => {
                    let mut func = type_section.function();
                    let params = f.params.iter().map(|(name, ty)| match ty {
                        wasmparser::ComponentValType::Primitive(prim) => match prim {
                            wasmparser::PrimitiveValType::Bool => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::Bool),
                            ),
                            wasmparser::PrimitiveValType::S8 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::S8),
                            ),
                            wasmparser::PrimitiveValType::U8 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::U8),
                            ),
                            wasmparser::PrimitiveValType::S16 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::S16),
                            ),
                            wasmparser::PrimitiveValType::U16 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::U16),
                            ),
                            wasmparser::PrimitiveValType::S32 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::S32),
                            ),
                            wasmparser::PrimitiveValType::U32 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::U32),
                            ),
                            wasmparser::PrimitiveValType::S64 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::S64),
                            ),
                            wasmparser::PrimitiveValType::U64 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::U64),
                            ),
                            wasmparser::PrimitiveValType::Float32 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::Float32),
                            ),
                            wasmparser::PrimitiveValType::Float64 => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::Float64),
                            ),
                            wasmparser::PrimitiveValType::Char => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::Char),
                            ),
                            wasmparser::PrimitiveValType::String => (
                                name.to_owned(),
                                ComponentValType::Primitive(PrimitiveValType::String),
                            ),
                        },
                        wasmparser::ComponentValType::Type(index) => {
                            (name.to_owned(), ComponentValType::Type(*index))
                        }
                    });
                    func.params(params);
                    // let func = ComponentFuncTypeEncoder::new(f);
                    //   // let mut func = FuncType::new(f.params, f.results);
                    match f.results {
                        wasmparser::ComponentFuncResult::Unnamed(ty) => {
                            let result = match ty {
                                wasmparser::ComponentValType::Primitive(prim) => match prim {
                                    wasmparser::PrimitiveValType::Bool => {
                                        todo!()
                                    }
                                    wasmparser::PrimitiveValType::S8 => todo!(),
                                    wasmparser::PrimitiveValType::U8 => todo!(),
                                    wasmparser::PrimitiveValType::S16 => {
                                        todo!()
                                    }
                                    wasmparser::PrimitiveValType::U16 => {
                                        todo!()
                                    }
                                    wasmparser::PrimitiveValType::S32 => {
                                        ComponentValType::Primitive(PrimitiveValType::S32)
                                    }
                                    wasmparser::PrimitiveValType::U32 => {
                                        ComponentValType::Primitive(PrimitiveValType::U32)
                                    }
                                    wasmparser::PrimitiveValType::S64 => {
                                        todo!()
                                    }
                                    wasmparser::PrimitiveValType::U64 => {
                                        todo!()
                                    }
                                    wasmparser::PrimitiveValType::Float32 => {
                                        todo!()
                                    }
                                    wasmparser::PrimitiveValType::Float64 => {
                                        todo!()
                                    }
                                    wasmparser::PrimitiveValType::Char => {
                                        todo!()
                                    }
                                    wasmparser::PrimitiveValType::String => {
                                        todo!()
                                    }
                                },
                                wasmparser::ComponentValType::Type(index) => {
                                    ComponentValType::Type(index)
                                }
                            };
                            func.result(result);
                        }
                        wasmparser::ComponentFuncResult::Named(ty) => {
                            let mut results = Vec::new();
                            for (name, val) in ty.iter() {
                                match val {
                                    wasmparser::ComponentValType::Primitive(prim) => match prim {
                                        wasmparser::PrimitiveValType::Bool => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::Bool),
                                        )),
                                        wasmparser::PrimitiveValType::S8 => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::S8),
                                        )),
                                        wasmparser::PrimitiveValType::U8 => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::U8),
                                        )),
                                        wasmparser::PrimitiveValType::S16 => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::S16),
                                        )),
                                        wasmparser::PrimitiveValType::U16 => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::U16),
                                        )),
                                        wasmparser::PrimitiveValType::S32 => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::S32),
                                        )),
                                        wasmparser::PrimitiveValType::U32 => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::U32),
                                        )),
                                        wasmparser::PrimitiveValType::S64 => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::S64),
                                        )),
                                        wasmparser::PrimitiveValType::U64 => results.push((
                                            *name,
                                            ComponentValType::Primitive(PrimitiveValType::U64),
                                        )),
                                        wasmparser::PrimitiveValType::Float32 => todo!(),
                                        wasmparser::PrimitiveValType::Float64 => todo!(),
                                        wasmparser::PrimitiveValType::Char => todo!(),
                                        wasmparser::PrimitiveValType::String => todo!(),
                                    },
                                    wasmparser::ComponentValType::Type(index) => {
                                        results.push((*name, ComponentValType::Type(*index)))
                                    }
                                }
                            }
                            func.results(results);
                        }
                    };
                }
                ComponentType::Component(_) => todo!(),
                ComponentType::Resource { rep, dtor } => todo!(),
            }

            component.section(&type_section);
        }

        Ok(())
    }

    pub fn parse_imports<'b>(
        &'b mut self,
        _states: &mut Vec<State>,
        section: &ComponentImportSectionReader,
        packages: &'b mut Vec<Import>,
    ) -> Result<&mut Vec<Import>> {
        let clone = section.clone();
        for (i, import) in clone.into_iter_with_offsets().enumerate() {
            let (_, imp) = import.unwrap().clone();
            // dbg!(&imp);
            match imp.name {
                wasmparser::ComponentExternName::Kebab(_) => {
                }
                wasmparser::ComponentExternName::Interface(name) => {
                    packages.push(Import::new(name.to_string(), ImportKind::Interface));
                }
                wasmparser::ComponentExternName::Implementation(name) => {
                    packages.push(Import::new(
                        name.as_str().to_string(),
                        ImportKind::Implementation,
                    ));
                }
            }
        }
        Ok(packages)
    }

    // pub fn parse_alias<'b>(&self, _states: &mut Vec<State>, section: &ComponentAliasSectionReader, aliases: &'b mut Vec<ComponentAlias>) -> Result<&mut Vec<ComponentAlias>> {
    //   let clone = section.clone();
    //   for (i, alias) in clone.into_iter_with_offsets().enumerate() {
    //     let (j, al) = alias.unwrap();
    //       aliases.push(al);
    //   }
    //   Ok(aliases)
    // }

    pub fn parse<'b>(
        &'b mut self,
        mut bytes: &[u8],
        mut component: &mut Component,
        packages: &'b mut Vec<Import>,
        // aliases: &'b mut Vec<ComponentAlias>
    ) -> Result<(&mut Vec<Import>
      // , &mut Vec<ComponentAlias>
    )> {
        // let mut aliases = Vec::<ComponentAlias>::new();
        let mut parser = Parser::new(0);
        let mut _consumed = 0;
        let mut states: Vec<State> = Vec::new();
        loop {
            let payload = match parser.parse(bytes, true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    payload
                }
            };
            match payload {
                Payload::ComponentTypeSection(s) => {
                    self.parse_type_imports(&mut states, s, &mut component)?;
                }
                Payload::ComponentImportSection(s) => {
                    self.parse_imports(&mut states, &s, packages)?;
                }
                // Payload::ComponentAliasSection(s) => {
                //     self.parse_alias(&mut states, &s, &mut aliases)?;
                // }
                Payload::CodeSectionStart {
                    count: _,
                    range: _,
                    size: _,
                } => {
                    parser.skip_section();
                }
                Payload::ModuleSection { parser, range } => {
                    let offset = range.end - range.start;
                    if offset > bytes.len() {
                        bail!("invalid module or component section range");
                    }
                    bytes = &bytes[offset..];
                }
                Payload::ComponentSection { parser, range } => {
                    let offset = range.end - range.start;
                    if offset > bytes.len() {
                        bail!("invalid module or component section range");
                    }
                    bytes = &bytes[offset..];
                }
                Payload::End(_) => {
                    break;
                }
                _ => {}
            }
        }
        Ok(
          // (
            packages
          // , aliases
        )
      // )
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
