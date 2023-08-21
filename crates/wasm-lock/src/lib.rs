use std::collections::{HashMap, HashSet};

use anyhow::{bail, Result};
use wasm_encoder::{
    Alias, ComponentAliasSection, ComponentExternName, ComponentImportSection,
    ComponentOuterAliasKind, ComponentTypeRef, ComponentTypeSection, ComponentValType,
    ImplementationImport, ImportMetadata, InstanceType, PrimitiveValType,
};
use wasmparser::{
    Chunk, ComponentAlias, ComponentAliasSectionReader, ComponentImportSectionReader,
    ComponentType, ComponentTypeSectionReader, Parser, Payload, TypeBounds, VariantCase,
};

pub struct Lock {}

#[derive(Debug, Clone)]
pub struct Dependency {
    pub index: usize,
    pub instance: Option<usize>,
    pub instantiation_args: Vec<String>,
}

impl Dependency {
    pub fn new(index: usize) -> Self {
        Self {
            index,
            instance: None,
            instantiation_args: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeEntityType {
    Component,
    Instance,
}

#[derive(Debug, Clone)]
pub enum Entity {
    Type((String, TypeEntityType)),
    Alias(ComponentAliasSection),
    Import((Import, ComponentImportSection)),
}

#[derive(Clone, Debug)]
pub struct Graph {
    /// index of last type in previously parsed component
    pub last_type: usize,
    /// number of components
    pub num_components: usize,
    /// number of instances
    pub num_instances: usize,
    /// number of interfaces
    pub num_interfaces: usize,
    /// number of aliases
    pub num_aliases: usize,
    /// number of types
    pub type_count: usize,
    /// list of type decls
    pub type_decls: HashMap<usize, ComponentTypeDecl>,
    /// map of indicies for interface types
    pub interface_type_indices: HashMap<String, usize>,
    /// components accessable by component name
    pub components: HashMap<String, Dependency>,
    /// components not to be added as entities since they already have been
    pub imported: HashSet<String>,
    /// list of interfaces
    pub interfaces: HashMap<String, usize>,
    /// components accessable by component index
    pub indices: HashMap<usize, Import>,
    /// list of entities to be added to locked component
    pub entities: Vec<Entity>,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            last_type: 0,
            num_components: 0,
            num_instances: 0,
            num_interfaces: 0,
            num_aliases: 0,
            type_count: 0,
            type_decls: HashMap::new(),
            interface_type_indices: HashMap::new(),
            components: HashMap::new(),
            imported: HashSet::new(),
            interfaces: HashMap::new(),
            indices: HashMap::new(),
            entities: Vec::new(),
        }
    }

    pub fn insert_component(&mut self, package: Import, val: Dependency) {
        if let std::collections::hash_map::Entry::Vacant(e) =
            self.components.entry(package.name.clone())
        {
            self.indices.insert(self.num_components, package);
            self.num_components += 1;
            e.insert(val);
        }
    }

    pub fn insert_instance(&mut self, key: String, val: usize) {
        self.num_instances += 1;
        self.components.get_mut(&key).map(|comp| {
            comp.instance = Some(val);
        });
    }
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    Implementation,
    Interface,
    Kebab,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub name: String,
    pub kind: ImportKind,
    pub aliases: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ComponentTypeDecl {
    decls: Vec<InstanceTypeDecl>,
    exports: Vec<String>,
    type_count: usize
}

impl ComponentTypeDecl {
    pub fn encode_component(&self) -> wasm_encoder::ComponentType {
        let mut component_type = wasm_encoder::ComponentType::new();
        for decl in &self.decls {
            match decl {
                InstanceTypeDecl::CoreType => todo!(),
                InstanceTypeDecl::Type(type_decl) => match type_decl {
                    TypeDecl::Defined(def) => match def {
                        DefinedDecl::Primitive(prim) => match prim {
                            PrimitiveDecl::Bool => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::Bool),
                            PrimitiveDecl::S8 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::S8),
                            PrimitiveDecl::U8 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::U8),
                            PrimitiveDecl::S16 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::S16),
                            PrimitiveDecl::U16 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::U16),
                            PrimitiveDecl::S32 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::S32),
                            PrimitiveDecl::U32 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::U32),
                            PrimitiveDecl::S64 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::S64),
                            PrimitiveDecl::U64 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::U64),
                            PrimitiveDecl::Float32 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::Float32),
                            PrimitiveDecl::Float64 => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::Float64),
                            PrimitiveDecl::Char => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::Char),
                            PrimitiveDecl::String => component_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::String),
                        },
                        DefinedDecl::Record(rec) => {
                            let mut fields = Vec::new();
                            for (key, val) in rec.fields.clone() {
                                match val {
                                    ValType::Primitive(prim) => match prim {
                                        PrimitiveDecl::Bool => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::Bool),
                                        )),
                                        PrimitiveDecl::S8 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::S8),
                                        )),
                                        PrimitiveDecl::U8 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::U8),
                                        )),
                                        PrimitiveDecl::S16 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::S16),
                                        )),
                                        PrimitiveDecl::U16 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::U16),
                                        )),
                                        PrimitiveDecl::S32 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::S32),
                                        )),
                                        PrimitiveDecl::U32 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::U32),
                                        )),
                                        PrimitiveDecl::S64 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::S64),
                                        )),
                                        PrimitiveDecl::U64 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::U64),
                                        )),
                                        PrimitiveDecl::Float32 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::Float32),
                                        )),
                                        PrimitiveDecl::Float64 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::Float64),
                                        )),
                                        PrimitiveDecl::Char => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::Char),
                                        )),
                                        PrimitiveDecl::String => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::String),
                                        )),
                                    },
                                    ValType::Type(i) => {
                                        fields.push((key, ComponentValType::Type(i)))
                                    }
                                }
                            }
                            component_type
                                .ty()
                                .defined_type()
                                .record(fields.iter().map(|(k, v)| (k.as_str(), *v)));
                        }
                        DefinedDecl::Variant(_) => todo!(),
                        DefinedDecl::List(ty) => match ty {
                            ValType::Primitive(prim) => match prim {
                                PrimitiveDecl::Bool => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::Bool)),
                                PrimitiveDecl::S8 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::S8)),
                                PrimitiveDecl::U8 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::U8)),
                                PrimitiveDecl::S16 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::S16)),
                                PrimitiveDecl::U16 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::U16)),
                                PrimitiveDecl::S32 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::S32)),
                                PrimitiveDecl::U32 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::U32)),
                                PrimitiveDecl::S64 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::S64)),
                                PrimitiveDecl::U64 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::U64)),
                                PrimitiveDecl::Float32 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::Float32)),
                                PrimitiveDecl::Float64 => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::Float64)),
                                PrimitiveDecl::Char => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::Char)),
                                PrimitiveDecl::String => component_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::String)),
                            },
                            ValType::Type(i) => component_type
                                .ty()
                                .defined_type()
                                .list(ComponentValType::Type(*i)),
                        },
                        DefinedDecl::Tuple(types) => {
                            let mut tys = Vec::new();
                            for ty in types {
                                match ty {
                                    ValType::Primitive(prim) => match prim {
                                        PrimitiveDecl::Bool => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::Bool),
                                        ),
                                        PrimitiveDecl::S8 => tys.push(ComponentValType::Primitive(
                                            PrimitiveValType::S8,
                                        )),
                                        PrimitiveDecl::U8 => tys.push(ComponentValType::Primitive(
                                            PrimitiveValType::U8,
                                        )),
                                        PrimitiveDecl::S16 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::S16),
                                        ),
                                        PrimitiveDecl::U16 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::U16),
                                        ),
                                        PrimitiveDecl::S32 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::S32),
                                        ),
                                        PrimitiveDecl::U32 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::U32),
                                        ),
                                        PrimitiveDecl::S64 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::S64),
                                        ),
                                        PrimitiveDecl::U64 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::U64),
                                        ),
                                        PrimitiveDecl::Float32 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::Float32),
                                        ),
                                        PrimitiveDecl::Float64 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::Float64),
                                        ),
                                        PrimitiveDecl::Char => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::Char),
                                        ),
                                        PrimitiveDecl::String => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::String),
                                        ),
                                    },
                                    ValType::Type(i) => tys.push(ComponentValType::Type(*i)),
                                }
                            }
                            component_type.ty().defined_type().tuple(tys);
                        }
                        DefinedDecl::Flags(names) => {
                            component_type
                                .ty()
                                .defined_type()
                                .flags(names.iter().map(|s| s.as_str()));
                        }
                        DefinedDecl::Enum(tags) => {
                            component_type
                                .ty()
                                .defined_type()
                                .enum_type(tags.iter().map(|s| s.as_str()));
                        }
                        DefinedDecl::Union(_) => todo!(),
                        DefinedDecl::Option(val) => match val {
                            ValType::Primitive(prim) => match prim {
                                PrimitiveDecl::Bool => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::Bool)),
                                PrimitiveDecl::S8 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::S8)),
                                PrimitiveDecl::U8 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::U8)),
                                PrimitiveDecl::S16 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::S16)),
                                PrimitiveDecl::U16 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::U16)),
                                PrimitiveDecl::S32 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::S32)),
                                PrimitiveDecl::U32 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::U32)),
                                PrimitiveDecl::S64 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::S64)),
                                PrimitiveDecl::U64 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::U64)),
                                PrimitiveDecl::Float32 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::Float32)),
                                PrimitiveDecl::Float64 => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::Float64)),
                                PrimitiveDecl::Char => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::Char)),
                                PrimitiveDecl::String => component_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::String)),
                            },
                            ValType::Type(i) => component_type
                                .ty()
                                .defined_type()
                                .option(ComponentValType::Type(*i)),
                        },
                        DefinedDecl::Result((ok_type, err_type)) => {
                            if let Some(ok_ty) = ok_type {
                                let ok_ty = match ok_ty {
                                    ValType::Primitive(prim) => match prim {
                                        PrimitiveDecl::Bool => {
                                            ComponentValType::Primitive(PrimitiveValType::Bool)
                                        }
                                        PrimitiveDecl::S8 => {
                                            ComponentValType::Primitive(PrimitiveValType::S8)
                                        }
                                        PrimitiveDecl::U8 => {
                                            ComponentValType::Primitive(PrimitiveValType::U8)
                                        }
                                        PrimitiveDecl::S16 => {
                                            ComponentValType::Primitive(PrimitiveValType::S16)
                                        }
                                        PrimitiveDecl::U16 => {
                                            ComponentValType::Primitive(PrimitiveValType::U16)
                                        }
                                        PrimitiveDecl::S32 => {
                                            ComponentValType::Primitive(PrimitiveValType::S32)
                                        }
                                        PrimitiveDecl::U32 => {
                                            ComponentValType::Primitive(PrimitiveValType::U32)
                                        }
                                        PrimitiveDecl::S64 => {
                                            ComponentValType::Primitive(PrimitiveValType::S64)
                                        }
                                        PrimitiveDecl::U64 => {
                                            ComponentValType::Primitive(PrimitiveValType::U64)
                                        }
                                        PrimitiveDecl::Float32 => {
                                            ComponentValType::Primitive(PrimitiveValType::Float32)
                                        }
                                        PrimitiveDecl::Float64 => {
                                            ComponentValType::Primitive(PrimitiveValType::Float64)
                                        }
                                        PrimitiveDecl::Char => {
                                            ComponentValType::Primitive(PrimitiveValType::Char)
                                        }
                                        PrimitiveDecl::String => {
                                            ComponentValType::Primitive(PrimitiveValType::String)
                                        }
                                    },
                                    ValType::Type(i) => ComponentValType::Type(*i),
                                };
                                if let Some(err_ty) = err_type {
                                    let err_ty = match err_ty {
                                        ValType::Primitive(prim) => match prim {
                                            PrimitiveDecl::Bool => {
                                                ComponentValType::Primitive(PrimitiveValType::Bool)
                                            }
                                            PrimitiveDecl::S8 => {
                                                ComponentValType::Primitive(PrimitiveValType::S8)
                                            }
                                            PrimitiveDecl::U8 => {
                                                ComponentValType::Primitive(PrimitiveValType::U8)
                                            }
                                            PrimitiveDecl::S16 => {
                                                ComponentValType::Primitive(PrimitiveValType::S16)
                                            }
                                            PrimitiveDecl::U16 => {
                                                ComponentValType::Primitive(PrimitiveValType::U16)
                                            }
                                            PrimitiveDecl::S32 => {
                                                ComponentValType::Primitive(PrimitiveValType::S32)
                                            }
                                            PrimitiveDecl::U32 => {
                                                ComponentValType::Primitive(PrimitiveValType::U32)
                                            }
                                            PrimitiveDecl::S64 => {
                                                ComponentValType::Primitive(PrimitiveValType::S64)
                                            }
                                            PrimitiveDecl::U64 => {
                                                ComponentValType::Primitive(PrimitiveValType::U64)
                                            }
                                            PrimitiveDecl::Float32 => ComponentValType::Primitive(
                                                PrimitiveValType::Float32,
                                            ),
                                            PrimitiveDecl::Float64 => ComponentValType::Primitive(
                                                PrimitiveValType::Float64,
                                            ),
                                            PrimitiveDecl::Char => {
                                                ComponentValType::Primitive(PrimitiveValType::Char)
                                            }
                                            PrimitiveDecl::String => ComponentValType::Primitive(
                                                PrimitiveValType::String,
                                            ),
                                        },
                                        ValType::Type(i) => ComponentValType::Type(*i),
                                    };
                                    component_type
                                        .ty()
                                        .defined_type()
                                        .result(Some(ok_ty), Some(err_ty));
                                } else {
                                    component_type.ty().defined_type().result(Some(ok_ty), None);
                                }
                            } else {
                                component_type.ty().defined_type().result(None, None);
                            }
                        }
                    },
                    TypeDecl::Func(func) => {
                        let params: Vec<(&str, ComponentValType)> = func
                            .params
                            .iter()
                            .map(|(name, val)| match val {
                                ValType::Primitive(prim) => match prim {
                                    PrimitiveDecl::Bool => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::Bool),
                                    ),
                                    PrimitiveDecl::S8 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::S8),
                                    ),
                                    PrimitiveDecl::U8 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::U8),
                                    ),
                                    PrimitiveDecl::S16 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::S16),
                                    ),
                                    PrimitiveDecl::U16 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::U16),
                                    ),
                                    PrimitiveDecl::S32 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::S32),
                                    ),
                                    PrimitiveDecl::U32 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::U32),
                                    ),
                                    PrimitiveDecl::S64 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::S64),
                                    ),
                                    PrimitiveDecl::U64 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::U64),
                                    ),
                                    PrimitiveDecl::Float32 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::Float32),
                                    ),
                                    PrimitiveDecl::Float64 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::Float64),
                                    ),
                                    PrimitiveDecl::Char => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::Char),
                                    ),
                                    PrimitiveDecl::String => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::String),
                                    ),
                                },
                                ValType::Type(i) => (name.as_str(), ComponentValType::Type(*i)),
                            })
                            .collect();
                        match &func.results {
                            FuncResult::Unnamed(result) => match result {
                                ValType::Primitive(prim) => match prim {
                                    PrimitiveDecl::Bool => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::Bool),
                                        )
                                    }
                                    PrimitiveDecl::S8 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::S8),
                                        )
                                    }
                                    PrimitiveDecl::U8 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::U8),
                                        )
                                    }
                                    PrimitiveDecl::S16 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::S16),
                                        )
                                    }
                                    PrimitiveDecl::U16 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::U16),
                                        )
                                    }
                                    PrimitiveDecl::S32 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::S32),
                                        )
                                    }
                                    PrimitiveDecl::U32 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::U32),
                                        )
                                    }
                                    PrimitiveDecl::S64 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::S64),
                                        )
                                    }
                                    PrimitiveDecl::U64 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::U64),
                                        )
                                    }
                                    PrimitiveDecl::Float32 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::Float32),
                                        )
                                    }
                                    PrimitiveDecl::Float64 => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::Float64),
                                        )
                                    }
                                    PrimitiveDecl::Char => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::Char),
                                        )
                                    }
                                    PrimitiveDecl::String => {
                                        component_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::String),
                                        )
                                    }
                                },
                                ValType::Type(i) => component_type
                                    .ty()
                                    .function()
                                    .params(params)
                                    .result(ComponentValType::Type(*i)),
                            },
                            FuncResult::Named(results) => {
                                let results: Vec<(&str, ComponentValType)> = results
                                    .iter()
                                    .map(|(name, result)| match result {
                                        ValType::Primitive(prim) => match prim {
                                            PrimitiveDecl::Bool => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::Bool),
                                            ),
                                            PrimitiveDecl::S8 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::S8),
                                            ),
                                            PrimitiveDecl::U8 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::U8),
                                            ),
                                            PrimitiveDecl::S16 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::S16),
                                            ),
                                            PrimitiveDecl::U16 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::U16),
                                            ),
                                            PrimitiveDecl::S32 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::S32),
                                            ),
                                            PrimitiveDecl::U32 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::U32),
                                            ),
                                            PrimitiveDecl::S64 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::S64),
                                            ),
                                            PrimitiveDecl::U64 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::U64),
                                            ),
                                            PrimitiveDecl::Float32 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(
                                                    PrimitiveValType::Float32,
                                                ),
                                            ),
                                            PrimitiveDecl::Float64 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(
                                                    PrimitiveValType::Float64,
                                                ),
                                            ),
                                            PrimitiveDecl::Char => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::Char),
                                            ),
                                            PrimitiveDecl::String => (
                                                name.as_str(),
                                                ComponentValType::Primitive(
                                                    PrimitiveValType::String,
                                                ),
                                            ),
                                        },
                                        ValType::Type(i) => {
                                            (name.as_str(), ComponentValType::Type(*i))
                                        }
                                    })
                                    .collect();
                                component_type
                                    .ty()
                                    .function()
                                    .params(params)
                                    .results(results)
                            }
                        };
                    }
                },
                InstanceTypeDecl::Alias(alias) => {
                    match alias {
                        AliasDecl::InstanceExport => todo!(),
                        AliasDecl::CoreInstanceExport => todo!(),
                        AliasDecl::Outer(OuterDecl { kind, count, index }) => match kind {
                            OuterAliasKindDecl::CoreModule => component_type.alias(Alias::Outer {
                                kind: ComponentOuterAliasKind::CoreModule,
                                count: *count,
                                index: *index,
                            }),
                            OuterAliasKindDecl::CoreType => component_type.alias(Alias::Outer {
                                kind: ComponentOuterAliasKind::CoreType,
                                count: *count,
                                index: *index,
                            }),
                            OuterAliasKindDecl::Type => component_type.alias(Alias::Outer {
                                kind: ComponentOuterAliasKind::Type,
                                count: *count,
                                index: *index,
                            }),
                            OuterAliasKindDecl::Component => component_type.alias(Alias::Outer {
                                kind: ComponentOuterAliasKind::Component,
                                count: *count,
                                index: *index,
                            }),
                        },
                    };
                }
                InstanceTypeDecl::Export(export) => {
                    let export_name = match &export.name {
                        ExternNameDecl::Kebab(name) => ComponentExternName::Kebab(&name),
                        ExternNameDecl::Interface(name) => ComponentExternName::Interface(&name),
                        ExternNameDecl::Implementation(imp) => match imp {
                            ImplementationDecl::Url(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(ImplementationImport::Url(
                                ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                },
                            )),
                            ImplementationDecl::Relative(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(
                                ImplementationImport::Relative(ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                }),
                            ),
                            ImplementationDecl::Naked(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(ImplementationImport::Naked(
                                ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                },
                            )),
                            ImplementationDecl::Unlocked(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(
                                ImplementationImport::Unlocked(ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                }),
                            ),
                            ImplementationDecl::Locked(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(
                                ImplementationImport::Locked(ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                }),
                            ),
                        },
                    };
                    match &export.ty {
                        TypeRefDecl::Module(i) => {
                            component_type.export(export_name, ComponentTypeRef::Module(*i))
                        }
                        TypeRefDecl::Func(i) => {
                            component_type.export(export_name, ComponentTypeRef::Func(*i))
                        }
                        TypeRefDecl::Val(val) => match val {
                            ValType::Primitive(prim) => match prim {
                                PrimitiveDecl::Bool => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::Bool,
                                    )),
                                ),
                                PrimitiveDecl::S8 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::S8,
                                    )),
                                ),
                                PrimitiveDecl::U8 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::U8,
                                    )),
                                ),
                                PrimitiveDecl::S16 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::S16,
                                    )),
                                ),
                                PrimitiveDecl::U16 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::U16,
                                    )),
                                ),
                                PrimitiveDecl::S32 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::S32,
                                    )),
                                ),
                                PrimitiveDecl::U32 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::U32,
                                    )),
                                ),
                                PrimitiveDecl::S64 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::S64,
                                    )),
                                ),
                                PrimitiveDecl::U64 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::U64,
                                    )),
                                ),
                                PrimitiveDecl::Float32 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::Float32,
                                    )),
                                ),
                                PrimitiveDecl::Float64 => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::Float64,
                                    )),
                                ),
                                PrimitiveDecl::Char => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::Char,
                                    )),
                                ),
                                PrimitiveDecl::String => component_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::String,
                                    )),
                                ),
                            },
                            ValType::Type(i) => component_type.export(
                                export_name,
                                ComponentTypeRef::Value(ComponentValType::Type(*i)),
                            ),
                        },
                        TypeRefDecl::Type(bounds) => match bounds {
                            TypeBoundsDecl::Eq(i) => component_type.export(
                                export_name,
                                ComponentTypeRef::Type(wasm_encoder::TypeBounds::Eq(*i)),
                            ),
                            TypeBoundsDecl::SubResource => component_type.export(
                                export_name,
                                ComponentTypeRef::Type(wasm_encoder::TypeBounds::SubResource),
                            ),
                        },
                        TypeRefDecl::Instance(i) => {
                            component_type.export(export_name, ComponentTypeRef::Instance(*i))
                        }
                        TypeRefDecl::Component(i) => {
                            component_type.export(export_name, ComponentTypeRef::Component(*i))
                        }
                    };
                }
            }
        }
        component_type
    }

    pub fn encode_instance(&self) -> InstanceType {
        let mut instance_type = InstanceType::new();
        for decl in &self.decls {
            match decl {
                InstanceTypeDecl::CoreType => todo!(),
                InstanceTypeDecl::Type(type_decl) => match type_decl {
                    TypeDecl::Defined(def) => match def {
                        DefinedDecl::Primitive(prim) => match prim {
                            PrimitiveDecl::Bool => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::Bool),
                            PrimitiveDecl::S8 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::S8),
                            PrimitiveDecl::U8 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::U8),
                            PrimitiveDecl::S16 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::S16),
                            PrimitiveDecl::U16 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::U16),
                            PrimitiveDecl::S32 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::S32),
                            PrimitiveDecl::U32 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::U32),
                            PrimitiveDecl::S64 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::S64),
                            PrimitiveDecl::U64 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::U64),
                            PrimitiveDecl::Float32 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::Float32),
                            PrimitiveDecl::Float64 => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::Float64),
                            PrimitiveDecl::Char => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::Char),
                            PrimitiveDecl::String => instance_type
                                .ty()
                                .defined_type()
                                .primitive(PrimitiveValType::String),
                        },
                        DefinedDecl::Record(rec) => {
                            let mut fields = Vec::new();
                            for (key, val) in rec.fields.clone() {
                                match val {
                                    ValType::Primitive(prim) => match prim {
                                        PrimitiveDecl::Bool => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::Bool),
                                        )),
                                        PrimitiveDecl::S8 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::S8),
                                        )),
                                        PrimitiveDecl::U8 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::U8),
                                        )),
                                        PrimitiveDecl::S16 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::S16),
                                        )),
                                        PrimitiveDecl::U16 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::U16),
                                        )),
                                        PrimitiveDecl::S32 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::S32),
                                        )),
                                        PrimitiveDecl::U32 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::U32),
                                        )),
                                        PrimitiveDecl::S64 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::S64),
                                        )),
                                        PrimitiveDecl::U64 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::U64),
                                        )),
                                        PrimitiveDecl::Float32 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::Float32),
                                        )),
                                        PrimitiveDecl::Float64 => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::Float64),
                                        )),
                                        PrimitiveDecl::Char => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::Char),
                                        )),
                                        PrimitiveDecl::String => fields.push((
                                            key,
                                            ComponentValType::Primitive(PrimitiveValType::String),
                                        )),
                                    },
                                    ValType::Type(i) => {
                                        fields.push((key, ComponentValType::Type(i)))
                                    }
                                }
                            }
                            instance_type
                                .ty()
                                .defined_type()
                                .record(fields.iter().map(|(k, v)| (k.as_str(), *v)));
                        }
                        DefinedDecl::Variant(_) => todo!(),
                        DefinedDecl::List(ty) => match ty {
                            ValType::Primitive(prim) => match prim {
                                PrimitiveDecl::Bool => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::Bool)),
                                PrimitiveDecl::S8 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::S8)),
                                PrimitiveDecl::U8 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::U8)),
                                PrimitiveDecl::S16 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::S16)),
                                PrimitiveDecl::U16 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::U16)),
                                PrimitiveDecl::S32 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::S32)),
                                PrimitiveDecl::U32 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::U32)),
                                PrimitiveDecl::S64 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::S64)),
                                PrimitiveDecl::U64 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::U64)),
                                PrimitiveDecl::Float32 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::Float32)),
                                PrimitiveDecl::Float64 => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::Float64)),
                                PrimitiveDecl::Char => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::Char)),
                                PrimitiveDecl::String => instance_type
                                    .ty()
                                    .defined_type()
                                    .list(ComponentValType::Primitive(PrimitiveValType::String)),
                            },
                            ValType::Type(i) => instance_type
                                .ty()
                                .defined_type()
                                .list(ComponentValType::Type(*i)),
                        },
                        DefinedDecl::Tuple(types) => {
                            let mut tys = Vec::new();
                            for ty in types {
                                match ty {
                                    ValType::Primitive(prim) => match prim {
                                        PrimitiveDecl::Bool => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::Bool),
                                        ),
                                        PrimitiveDecl::S8 => tys.push(ComponentValType::Primitive(
                                            PrimitiveValType::S8,
                                        )),
                                        PrimitiveDecl::U8 => tys.push(ComponentValType::Primitive(
                                            PrimitiveValType::U8,
                                        )),
                                        PrimitiveDecl::S16 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::S16),
                                        ),
                                        PrimitiveDecl::U16 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::U16),
                                        ),
                                        PrimitiveDecl::S32 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::S32),
                                        ),
                                        PrimitiveDecl::U32 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::U32),
                                        ),
                                        PrimitiveDecl::S64 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::S64),
                                        ),
                                        PrimitiveDecl::U64 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::U64),
                                        ),
                                        PrimitiveDecl::Float32 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::Float32),
                                        ),
                                        PrimitiveDecl::Float64 => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::Float64),
                                        ),
                                        PrimitiveDecl::Char => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::Char),
                                        ),
                                        PrimitiveDecl::String => tys.push(
                                            ComponentValType::Primitive(PrimitiveValType::String),
                                        ),
                                    },
                                    ValType::Type(i) => tys.push(ComponentValType::Type(*i)),
                                }
                            }
                            instance_type.ty().defined_type().tuple(tys);
                        }
                        DefinedDecl::Flags(names) => {
                            instance_type
                                .ty()
                                .defined_type()
                                .flags(names.iter().map(|s| s.as_str()));
                        }
                        DefinedDecl::Enum(tags) => {
                            instance_type
                                .ty()
                                .defined_type()
                                .enum_type(tags.iter().map(|s| s.as_str()));
                        }
                        DefinedDecl::Union(_) => todo!(),
                        DefinedDecl::Option(val) => match val {
                            ValType::Primitive(prim) => match prim {
                                PrimitiveDecl::Bool => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::Bool)),
                                PrimitiveDecl::S8 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::S8)),
                                PrimitiveDecl::U8 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::U8)),
                                PrimitiveDecl::S16 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::S16)),
                                PrimitiveDecl::U16 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::U16)),
                                PrimitiveDecl::S32 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::S32)),
                                PrimitiveDecl::U32 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::U32)),
                                PrimitiveDecl::S64 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::S64)),
                                PrimitiveDecl::U64 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::U64)),
                                PrimitiveDecl::Float32 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::Float32)),
                                PrimitiveDecl::Float64 => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::Float64)),
                                PrimitiveDecl::Char => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::Char)),
                                PrimitiveDecl::String => instance_type
                                    .ty()
                                    .defined_type()
                                    .option(ComponentValType::Primitive(PrimitiveValType::String)),
                            },
                            ValType::Type(i) => instance_type
                                .ty()
                                .defined_type()
                                .option(ComponentValType::Type(*i)),
                        },
                        DefinedDecl::Result((ok_type, err_type)) => {
                            if let Some(ok_ty) = ok_type {
                                let ok_ty = match ok_ty {
                                    ValType::Primitive(prim) => match prim {
                                        PrimitiveDecl::Bool => {
                                            ComponentValType::Primitive(PrimitiveValType::Bool)
                                        }
                                        PrimitiveDecl::S8 => {
                                            ComponentValType::Primitive(PrimitiveValType::S8)
                                        }
                                        PrimitiveDecl::U8 => {
                                            ComponentValType::Primitive(PrimitiveValType::U8)
                                        }
                                        PrimitiveDecl::S16 => {
                                            ComponentValType::Primitive(PrimitiveValType::S16)
                                        }
                                        PrimitiveDecl::U16 => {
                                            ComponentValType::Primitive(PrimitiveValType::U16)
                                        }
                                        PrimitiveDecl::S32 => {
                                            ComponentValType::Primitive(PrimitiveValType::S32)
                                        }
                                        PrimitiveDecl::U32 => {
                                            ComponentValType::Primitive(PrimitiveValType::U32)
                                        }
                                        PrimitiveDecl::S64 => {
                                            ComponentValType::Primitive(PrimitiveValType::S64)
                                        }
                                        PrimitiveDecl::U64 => {
                                            ComponentValType::Primitive(PrimitiveValType::U64)
                                        }
                                        PrimitiveDecl::Float32 => {
                                            ComponentValType::Primitive(PrimitiveValType::Float32)
                                        }
                                        PrimitiveDecl::Float64 => {
                                            ComponentValType::Primitive(PrimitiveValType::Float64)
                                        }
                                        PrimitiveDecl::Char => {
                                            ComponentValType::Primitive(PrimitiveValType::Char)
                                        }
                                        PrimitiveDecl::String => {
                                            ComponentValType::Primitive(PrimitiveValType::String)
                                        }
                                    },
                                    ValType::Type(i) => ComponentValType::Type(*i),
                                };
                                if let Some(err_ty) = err_type {
                                    let err_ty = match err_ty {
                                        ValType::Primitive(prim) => match prim {
                                            PrimitiveDecl::Bool => {
                                                ComponentValType::Primitive(PrimitiveValType::Bool)
                                            }
                                            PrimitiveDecl::S8 => {
                                                ComponentValType::Primitive(PrimitiveValType::S8)
                                            }
                                            PrimitiveDecl::U8 => {
                                                ComponentValType::Primitive(PrimitiveValType::U8)
                                            }
                                            PrimitiveDecl::S16 => {
                                                ComponentValType::Primitive(PrimitiveValType::S16)
                                            }
                                            PrimitiveDecl::U16 => {
                                                ComponentValType::Primitive(PrimitiveValType::U16)
                                            }
                                            PrimitiveDecl::S32 => {
                                                ComponentValType::Primitive(PrimitiveValType::S32)
                                            }
                                            PrimitiveDecl::U32 => {
                                                ComponentValType::Primitive(PrimitiveValType::U32)
                                            }
                                            PrimitiveDecl::S64 => {
                                                ComponentValType::Primitive(PrimitiveValType::S64)
                                            }
                                            PrimitiveDecl::U64 => {
                                                ComponentValType::Primitive(PrimitiveValType::U64)
                                            }
                                            PrimitiveDecl::Float32 => ComponentValType::Primitive(
                                                PrimitiveValType::Float32,
                                            ),
                                            PrimitiveDecl::Float64 => ComponentValType::Primitive(
                                                PrimitiveValType::Float64,
                                            ),
                                            PrimitiveDecl::Char => {
                                                ComponentValType::Primitive(PrimitiveValType::Char)
                                            }
                                            PrimitiveDecl::String => ComponentValType::Primitive(
                                                PrimitiveValType::String,
                                            ),
                                        },
                                        ValType::Type(i) => ComponentValType::Type(*i),
                                    };
                                    instance_type
                                        .ty()
                                        .defined_type()
                                        .result(Some(ok_ty), Some(err_ty));
                                } else {
                                    instance_type.ty().defined_type().result(Some(ok_ty), None);
                                }
                            } else {
                                instance_type.ty().defined_type().result(None, None);
                            }
                        }
                    },
                    TypeDecl::Func(func) => {
                        let params: Vec<(&str, ComponentValType)> = func
                            .params
                            .iter()
                            .map(|(name, val)| match val {
                                ValType::Primitive(prim) => match prim {
                                    PrimitiveDecl::Bool => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::Bool),
                                    ),
                                    PrimitiveDecl::S8 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::S8),
                                    ),
                                    PrimitiveDecl::U8 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::U8),
                                    ),
                                    PrimitiveDecl::S16 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::S16),
                                    ),
                                    PrimitiveDecl::U16 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::U16),
                                    ),
                                    PrimitiveDecl::S32 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::S32),
                                    ),
                                    PrimitiveDecl::U32 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::U32),
                                    ),
                                    PrimitiveDecl::S64 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::S64),
                                    ),
                                    PrimitiveDecl::U64 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::U64),
                                    ),
                                    PrimitiveDecl::Float32 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::Float32),
                                    ),
                                    PrimitiveDecl::Float64 => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::Float64),
                                    ),
                                    PrimitiveDecl::Char => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::Char),
                                    ),
                                    PrimitiveDecl::String => (
                                        name.as_str(),
                                        ComponentValType::Primitive(PrimitiveValType::String),
                                    ),
                                },
                                ValType::Type(i) => (name.as_str(), ComponentValType::Type(*i)),
                            })
                            .collect();
                        match &func.results {
                            FuncResult::Unnamed(result) => match result {
                                ValType::Primitive(prim) => match prim {
                                    PrimitiveDecl::Bool => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::Bool),
                                        )
                                    }
                                    PrimitiveDecl::S8 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::S8),
                                        )
                                    }
                                    PrimitiveDecl::U8 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::U8),
                                        )
                                    }
                                    PrimitiveDecl::S16 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::S16),
                                        )
                                    }
                                    PrimitiveDecl::U16 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::U16),
                                        )
                                    }
                                    PrimitiveDecl::S32 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::S32),
                                        )
                                    }
                                    PrimitiveDecl::U32 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::U32),
                                        )
                                    }
                                    PrimitiveDecl::S64 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::S64),
                                        )
                                    }
                                    PrimitiveDecl::U64 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::U64),
                                        )
                                    }
                                    PrimitiveDecl::Float32 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::Float32),
                                        )
                                    }
                                    PrimitiveDecl::Float64 => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::Float64),
                                        )
                                    }
                                    PrimitiveDecl::Char => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::Char),
                                        )
                                    }
                                    PrimitiveDecl::String => {
                                        instance_type.ty().function().params(params).result(
                                            ComponentValType::Primitive(PrimitiveValType::String),
                                        )
                                    }
                                },
                                ValType::Type(i) => instance_type
                                    .ty()
                                    .function()
                                    .params(params)
                                    .result(ComponentValType::Type(*i)),
                            },
                            FuncResult::Named(results) => {
                                let results: Vec<(&str, ComponentValType)> = results
                                    .iter()
                                    .map(|(name, result)| match result {
                                        ValType::Primitive(prim) => match prim {
                                            PrimitiveDecl::Bool => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::Bool),
                                            ),
                                            PrimitiveDecl::S8 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::S8),
                                            ),
                                            PrimitiveDecl::U8 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::U8),
                                            ),
                                            PrimitiveDecl::S16 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::S16),
                                            ),
                                            PrimitiveDecl::U16 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::U16),
                                            ),
                                            PrimitiveDecl::S32 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::S32),
                                            ),
                                            PrimitiveDecl::U32 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::U32),
                                            ),
                                            PrimitiveDecl::S64 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::S64),
                                            ),
                                            PrimitiveDecl::U64 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::U64),
                                            ),
                                            PrimitiveDecl::Float32 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(
                                                    PrimitiveValType::Float32,
                                                ),
                                            ),
                                            PrimitiveDecl::Float64 => (
                                                name.as_str(),
                                                ComponentValType::Primitive(
                                                    PrimitiveValType::Float64,
                                                ),
                                            ),
                                            PrimitiveDecl::Char => (
                                                name.as_str(),
                                                ComponentValType::Primitive(PrimitiveValType::Char),
                                            ),
                                            PrimitiveDecl::String => (
                                                name.as_str(),
                                                ComponentValType::Primitive(
                                                    PrimitiveValType::String,
                                                ),
                                            ),
                                        },
                                        ValType::Type(i) => {
                                            (name.as_str(), ComponentValType::Type(*i))
                                        }
                                    })
                                    .collect();
                                instance_type
                                    .ty()
                                    .function()
                                    .params(params)
                                    .results(results)
                            }
                        };
                    }
                },
                InstanceTypeDecl::Alias(alias) => {
                    match alias {
                        AliasDecl::InstanceExport => todo!(),
                        AliasDecl::CoreInstanceExport => todo!(),
                        AliasDecl::Outer(OuterDecl { kind, count, index }) => match kind {
                            OuterAliasKindDecl::CoreModule => instance_type.alias(Alias::Outer {
                                kind: ComponentOuterAliasKind::CoreModule,
                                count: *count,
                                index: *index,
                            }),
                            OuterAliasKindDecl::CoreType => instance_type.alias(Alias::Outer {
                                kind: ComponentOuterAliasKind::CoreType,
                                count: *count,
                                index: *index,
                            }),
                            OuterAliasKindDecl::Type => instance_type.alias(Alias::Outer {
                                kind: ComponentOuterAliasKind::Type,
                                count: *count,
                                index: *index,
                            }),
                            OuterAliasKindDecl::Component => instance_type.alias(Alias::Outer {
                                kind: ComponentOuterAliasKind::Component,
                                count: *count,
                                index: *index,
                            }),
                        },
                    };
                }
                InstanceTypeDecl::Export(export) => {
                    let export_name = match &export.name {
                        ExternNameDecl::Kebab(name) => ComponentExternName::Kebab(&name),
                        ExternNameDecl::Interface(name) => ComponentExternName::Interface(&name),
                        ExternNameDecl::Implementation(imp) => match imp {
                            ImplementationDecl::Url(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(ImplementationImport::Url(
                                ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                },
                            )),
                            ImplementationDecl::Relative(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(
                                ImplementationImport::Relative(ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                }),
                            ),
                            ImplementationDecl::Naked(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(ImplementationImport::Naked(
                                ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                },
                            )),
                            ImplementationDecl::Unlocked(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(
                                ImplementationImport::Unlocked(ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                }),
                            ),
                            ImplementationDecl::Locked(Metadata {
                                name,
                                location,
                                integrity,
                            }) => ComponentExternName::Implementation(
                                ImplementationImport::Locked(ImportMetadata {
                                    name,
                                    location,
                                    integrity: integrity.as_ref().map(|i| i.as_str()),
                                }),
                            ),
                        },
                    };
                    match &export.ty {
                        TypeRefDecl::Module(i) => {
                            instance_type.export(export_name, ComponentTypeRef::Module(*i))
                        }
                        TypeRefDecl::Func(i) => {
                            instance_type.export(export_name, ComponentTypeRef::Func(*i))
                        }
                        TypeRefDecl::Val(val) => match val {
                            ValType::Primitive(prim) => match prim {
                                PrimitiveDecl::Bool => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::Bool,
                                    )),
                                ),
                                PrimitiveDecl::S8 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::S8,
                                    )),
                                ),
                                PrimitiveDecl::U8 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::U8,
                                    )),
                                ),
                                PrimitiveDecl::S16 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::S16,
                                    )),
                                ),
                                PrimitiveDecl::U16 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::U16,
                                    )),
                                ),
                                PrimitiveDecl::S32 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::S32,
                                    )),
                                ),
                                PrimitiveDecl::U32 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::U32,
                                    )),
                                ),
                                PrimitiveDecl::S64 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::S64,
                                    )),
                                ),
                                PrimitiveDecl::U64 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::U64,
                                    )),
                                ),
                                PrimitiveDecl::Float32 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::Float32,
                                    )),
                                ),
                                PrimitiveDecl::Float64 => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::Float64,
                                    )),
                                ),
                                PrimitiveDecl::Char => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::Char,
                                    )),
                                ),
                                PrimitiveDecl::String => instance_type.export(
                                    export_name,
                                    ComponentTypeRef::Value(ComponentValType::Primitive(
                                        PrimitiveValType::String,
                                    )),
                                ),
                            },
                            ValType::Type(i) => instance_type.export(
                                export_name,
                                ComponentTypeRef::Value(ComponentValType::Type(*i)),
                            ),
                        },
                        TypeRefDecl::Type(bounds) => match bounds {
                            TypeBoundsDecl::Eq(i) => instance_type.export(
                                export_name,
                                ComponentTypeRef::Type(wasm_encoder::TypeBounds::Eq(*i)),
                            ),
                            TypeBoundsDecl::SubResource => instance_type.export(
                                export_name,
                                ComponentTypeRef::Type(wasm_encoder::TypeBounds::SubResource),
                            ),
                        },
                        TypeRefDecl::Instance(i) => {
                            instance_type.export(export_name, ComponentTypeRef::Instance(*i))
                        }
                        TypeRefDecl::Component(i) => {
                            instance_type.export(export_name, ComponentTypeRef::Component(*i))
                        }
                    };
                }
            }
        }
        instance_type
    }
}

#[derive(Debug, Clone)]
pub enum InstanceDecl {
    CoreType, //TODO
    Type,
    Alias,
    Export,
}

#[derive(Debug, Clone)]
pub enum InstanceTypeDecl {
    CoreType, // TODO
    Type(TypeDecl),
    Alias(AliasDecl),
    Export(ExportDecl),
}

#[derive(Debug, Clone)]
pub enum TypeDecl {
    Defined(DefinedDecl),
    Func(FuncDecl),
}

#[derive(Debug, Clone)]
pub enum DefinedDecl {
    Primitive(PrimitiveDecl),
    Record(Record),
    Variant(Variant),
    List(ValType),
    Tuple(Vec<ValType>),
    Flags(Vec<String>),
    Enum(Vec<String>),
    Union(Union),
    Option(ValType),
    Result((Option<ValType>, Option<ValType>)),
}

#[derive(Debug, Clone)]
pub enum ValType {
    Primitive(PrimitiveDecl),
    Type(u32),
}

#[derive(Debug, Clone)]
pub enum PrimitiveDecl {
    Bool,
    S8,
    U8,
    S16,
    U16,
    S32,
    U32,
    S64,
    U64,
    Float32,
    Float64,
    Char,
    String,
}

#[derive(Debug, Clone)]
pub struct Record {
    fields: Vec<(String, ValType)>,
}

#[derive(Debug, Clone)]
pub struct Variant {} // TODO
#[derive(Debug, Clone)]
pub struct Union {} // TODO

#[derive(Debug, Clone)]
pub struct FuncDecl {
    params: Vec<(String, ValType)>,
    results: FuncResult,
}

#[derive(Debug, Clone)]
pub enum FuncResult {
    Unnamed(ValType),
    Named(Vec<(String, ValType)>),
}

#[derive(Debug, Clone)]
pub enum AliasDecl {
    InstanceExport,     // TODO
    CoreInstanceExport, // TODO
    Outer(OuterDecl),
}

#[derive(Debug, Clone)]
pub struct OuterDecl {
    kind: OuterAliasKindDecl,
    count: u32,
    index: u32,
}

#[derive(Debug, Clone)]
pub enum OuterAliasKindDecl {
    CoreModule,
    CoreType,
    Type,
    Component,
}

#[derive(Debug, Clone)]
pub struct ExportDecl {
    name: ExternNameDecl,
    ty: TypeRefDecl,
}

#[derive(Debug, Clone)]
pub enum ExternNameDecl {
    Kebab(String),
    Interface(String),
    Implementation(ImplementationDecl),
}

#[derive(Debug, Clone)]
pub enum ImplementationDecl {
    Url(Metadata),
    Relative(Metadata),
    Naked(Metadata),
    Locked(Metadata),
    Unlocked(Metadata),
}

#[derive(Debug, Clone)]
pub struct Metadata {
    name: String,
    location: String,
    integrity: Option<String>,
}

#[derive(Debug, Clone)]
pub enum TypeRefDecl {
    Module(u32),
    Func(u32),
    Val(ValType),
    Type(TypeBoundsDecl),
    Instance(u32),
    Component(u32),
}

#[derive(Debug, Clone)]
pub enum TypeBoundsDecl {
    Eq(u32),
    SubResource,
}

impl Import {
    pub fn new(name: String, kind: ImportKind) -> Self {
        Self {
            name,
            kind,
            aliases: Vec::new(),
        }
    }
}

impl Lock {
    pub fn new() -> Self {
        Self {
        //   graph: Graph::new()
        }
    }

    pub fn parse_import_type(
        &self,
        parser: &ComponentTypeSectionReader,
        graph: &mut Graph,
    ) -> Result<()> {
        let cloned = parser.clone();
        let mut type_section = ComponentTypeSection::new();
        for ty in cloned.into_iter_with_offsets() {
            let mut instance_decls = Vec::<InstanceTypeDecl>::new();
            let mut exports = Vec::<String>::new();
            let mut instance_type_count = 0;
            let (_offset, ty) = ty?;
            match ty {
                ComponentType::Instance(decls) => {
                    for decl in decls.into_vec() {
                        match decl {
                            wasmparser::InstanceTypeDeclaration::CoreType(_) => todo!(),
                            wasmparser::InstanceTypeDeclaration::Type(instance_ty) => {
                                match instance_ty {
                                    ComponentType::Defined(def) => {
                                        match def {
                                            wasmparser::ComponentDefinedType::Primitive(prim) => {
                                                match prim {
                                                    wasmparser::PrimitiveValType::Bool => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::Bool,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::S8 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::S8,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::U8 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::U8,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::S16 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::S16,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::U16 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::U16,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::S32 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::S32,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::U32 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::U32,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::S64 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::S64,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::U64 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::U64,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::Float32 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::Float32,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::Float64 => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::Float64,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::Char => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::Char,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    wasmparser::PrimitiveValType::String => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Primitive(
                                                                        PrimitiveDecl::String,
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                }
                                            }
                                            wasmparser::ComponentDefinedType::Record(items) => {
                                                let mut fields = Vec::new();
                                                let mut record = Record { fields: Vec::new() };
                                                for (key, val) in items.iter() {
                                                    match val {
                                                    wasmparser::ComponentValType::Primitive(prim) => {
                                                      match prim {
                                                        wasmparser::PrimitiveValType::Bool => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::Bool)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::Bool)));
                                                        },
                                                        wasmparser::PrimitiveValType::S8 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::S8)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::S8)));
                                                        },
                                                        wasmparser::PrimitiveValType::U8 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::U8)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::U8)));
                                                        },
                                                        wasmparser::PrimitiveValType::S16 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::S16)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::S16)));
                                                        },
                                                        wasmparser::PrimitiveValType::U16 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::U16)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::U16)));
                                                        },
                                                        wasmparser::PrimitiveValType::S32 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::S32)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::S32)));
                                                        },
                                                        wasmparser::PrimitiveValType::U32 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::U32)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::U32)));
                                                        },
                                                        wasmparser::PrimitiveValType::S64 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::S64)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::S64)));
                                                        },
                                                        wasmparser::PrimitiveValType::U64 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::U64)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::U64)));
                                                        },
                                                        wasmparser::PrimitiveValType::Float32 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::Float32)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::Float32)));
                                                        },
                                                        wasmparser::PrimitiveValType::Float64 => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::Float64)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::Float64)));
                                                        },
                                                        wasmparser::PrimitiveValType::Char => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::Char)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::Char)));
                                                        },
                                                        wasmparser::PrimitiveValType::String => {
                                                          fields.push((*key, ComponentValType::Primitive(PrimitiveValType::String)));
                                                          record.fields.push((key.to_string(), ValType::Primitive(PrimitiveDecl::String)));
                                                        },
                                                      }
                                                    },
                                                    wasmparser::ComponentValType::Type(index) => {
                                                      fields.push((*key, ComponentValType::Type(*index)));
                                                      record.fields.push((key.to_string(), ValType::Type(*index)));
                                                    },
                                                }
                                                }
                                                instance_decls.push(InstanceTypeDecl::Type(
                                                    TypeDecl::Defined(DefinedDecl::Record(record)),
                                                ));
                                            }
                                            wasmparser::ComponentDefinedType::Variant(_) => todo!(),
                                            wasmparser::ComponentDefinedType::List(ty) => {
                                                match ty {
                                                    wasmparser::ComponentValType::Primitive(
                                                        prim,
                                                    ) => match prim {
                                                        wasmparser::PrimitiveValType::Bool => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::Bool,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::S8 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::S8,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::U8 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::U8,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::S16 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::S16,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::U16 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::U16,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::S32 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::S32,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::U32 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::U32,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::S64 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::S64,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::U64 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::U64,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::Float32 => {
                                                            instance_decls
                                                                .push(InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::List(
                                                                        ValType::Primitive(
                                                                            PrimitiveDecl::Float32,
                                                                        ),
                                                                    ),
                                                                ),
                                                            ));
                                                        }
                                                        wasmparser::PrimitiveValType::Float64 => {
                                                            instance_decls
                                                                .push(InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::List(
                                                                        ValType::Primitive(
                                                                            PrimitiveDecl::Float64,
                                                                        ),
                                                                    ),
                                                                ),
                                                            ));
                                                        }
                                                        wasmparser::PrimitiveValType::Char => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::List(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::Char,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::String => {
                                                            instance_decls
                                                                .push(InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::List(
                                                                        ValType::Primitive(
                                                                            PrimitiveDecl::String,
                                                                        ),
                                                                    ),
                                                                ),
                                                            ));
                                                        }
                                                    },
                                                    wasmparser::ComponentValType::Type(index) => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::List(
                                                                        ValType::Type(index),
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                }
                                            }
                                            wasmparser::ComponentDefinedType::Tuple(tys) => {
                                                let mut tuple = Vec::new();
                                                let mut tuple_decl = Vec::new();
                                                for item in tys.iter() {
                                                    match item {
                                                      wasmparser::ComponentValType::Primitive(prim) => {
                                                        match prim {
                                                          wasmparser::PrimitiveValType::Bool => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::Bool));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::Bool));
                                                          },
                                                          wasmparser::PrimitiveValType::S8 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::S8));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::S8));
                                                          },
                                                          wasmparser::PrimitiveValType::U8 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::U8));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::U8));
                                                          },
                                                          wasmparser::PrimitiveValType::S16 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::S16));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::S16));
                                                          },
                                                          wasmparser::PrimitiveValType::U16 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::U16));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::U16));
                                                          },
                                                          wasmparser::PrimitiveValType::S32 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::S32));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::S32));
                                                          },
                                                          wasmparser::PrimitiveValType::U32 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::U32));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::U32));
                                                          },
                                                          wasmparser::PrimitiveValType::S64 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::S64));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::S64));
                                                          },
                                                          wasmparser::PrimitiveValType::U64 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::U64));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::U64));
                                                          },
                                                          wasmparser::PrimitiveValType::Float32 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::Float32));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::Float32));
                                                          },
                                                          wasmparser::PrimitiveValType::Float64 => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::Float64));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::Float64));
                                                          },
                                                          wasmparser::PrimitiveValType::Char => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::Char));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::Char));
                                                          },
                                                          wasmparser::PrimitiveValType::String => {
                                                            tuple.push(ComponentValType::Primitive(PrimitiveValType::String));
                                                            tuple_decl.push(ValType::Primitive(PrimitiveDecl::String));
                                                          },
                                                        }
                                                      },
                                                      wasmparser::ComponentValType::Type(index) => {
                                                        tuple.push(ComponentValType::Type(*index));
                                                        tuple_decl.push(ValType::Type(*index));
                                                      }
                                                    }
                                                }
                                                instance_decls.push(InstanceTypeDecl::Type(
                                                    TypeDecl::Defined(DefinedDecl::Tuple(
                                                        tuple_decl,
                                                    )),
                                                ));
                                            }
                                            wasmparser::ComponentDefinedType::Flags(flags) => {
                                                let mut names = Vec::new();
                                                for flag in flags.iter() {
                                                    names.push(*flag);
                                                }
                                                instance_decls.push(InstanceTypeDecl::Type(
                                                    TypeDecl::Defined(DefinedDecl::Flags(
                                                        names
                                                            .iter()
                                                            .map(|s| s.to_string())
                                                            .collect(),
                                                    )),
                                                ));
                                            }
                                            wasmparser::ComponentDefinedType::Enum(kinds) => {
                                                let mut variants = Vec::new();
                                                for kind in kinds.iter() {
                                                    variants.push(*kind);
                                                }
                                                instance_decls.push(InstanceTypeDecl::Type(
                                                    TypeDecl::Defined(DefinedDecl::Enum(
                                                        variants
                                                            .iter()
                                                            .map(|s| s.to_string())
                                                            .collect(),
                                                    )),
                                                ));
                                            }
                                            wasmparser::ComponentDefinedType::Union(_) => todo!(),
                                            wasmparser::ComponentDefinedType::Option(val) => {
                                                match val {
                                                    wasmparser::ComponentValType::Primitive(
                                                        prim,
                                                    ) => match prim {
                                                        wasmparser::PrimitiveValType::Bool => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::Bool,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::S8 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::S8,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::U8 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::U8,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::S16 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::S16,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::U16 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::U16,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::S32 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::S32,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::U32 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::U32,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::S64 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::S64,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::U64 => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::U64,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::Float32 => {
                                                            instance_decls
                                                                .push(InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Option(
                                                                        ValType::Primitive(
                                                                            PrimitiveDecl::Float32,
                                                                        ),
                                                                    ),
                                                                ),
                                                            ));
                                                        }
                                                        wasmparser::PrimitiveValType::Float64 => {
                                                            instance_decls
                                                                .push(InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Option(
                                                                        ValType::Primitive(
                                                                            PrimitiveDecl::Float64,
                                                                        ),
                                                                    ),
                                                                ),
                                                            ));
                                                        }
                                                        wasmparser::PrimitiveValType::Char => {
                                                            instance_decls.push(
                                                                InstanceTypeDecl::Type(
                                                                    TypeDecl::Defined(
                                                                        DefinedDecl::Option(
                                                                            ValType::Primitive(
                                                                                PrimitiveDecl::Char,
                                                                            ),
                                                                        ),
                                                                    ),
                                                                ),
                                                            );
                                                        }
                                                        wasmparser::PrimitiveValType::String => {
                                                            instance_decls
                                                                .push(InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Option(
                                                                        ValType::Primitive(
                                                                            PrimitiveDecl::String,
                                                                        ),
                                                                    ),
                                                                ),
                                                            ));
                                                        }
                                                    },
                                                    wasmparser::ComponentValType::Type(i) => {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Option(
                                                                        ValType::Type(i),
                                                                    ),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                }
                                            }
                                            wasmparser::ComponentDefinedType::Result {
                                                ok,
                                                err,
                                            } => {
                                                if let Some(ok_val_type) = ok {
                                                    let ok_decl = match ok_val_type {
                                                      wasmparser::ComponentValType::Primitive(prim) => {
                                                        match prim {
                                                          wasmparser::PrimitiveValType::Bool => ValType::Primitive(PrimitiveDecl::Bool),
                                                          wasmparser::PrimitiveValType::S8 => ValType::Primitive(PrimitiveDecl::S8),
                                                          wasmparser::PrimitiveValType::U8 => ValType::Primitive(PrimitiveDecl::U8),
                                                          wasmparser::PrimitiveValType::S16 => ValType::Primitive(PrimitiveDecl::S16),
                                                          wasmparser::PrimitiveValType::U16 => ValType::Primitive(PrimitiveDecl::U16),
                                                          wasmparser::PrimitiveValType::S32 => ValType::Primitive(PrimitiveDecl::S32),
                                                          wasmparser::PrimitiveValType::U32 => ValType::Primitive(PrimitiveDecl::U32),
                                                          wasmparser::PrimitiveValType::S64 => ValType::Primitive(PrimitiveDecl::S64),
                                                          wasmparser::PrimitiveValType::U64 => ValType::Primitive(PrimitiveDecl::U64),
                                                          wasmparser::PrimitiveValType::Float32 => ValType::Primitive(PrimitiveDecl::Float32),
                                                          wasmparser::PrimitiveValType::Float64 => ValType::Primitive(PrimitiveDecl::Float64),
                                                          wasmparser::PrimitiveValType::Char => ValType::Primitive(PrimitiveDecl::Char),
                                                          wasmparser::PrimitiveValType::String => ValType::Primitive(PrimitiveDecl::String),
                                                        }
                                                      },
                                                      wasmparser::ComponentValType::Type(index) => {
                                                        ValType::Type(index)
                                                      }
                                                    };
                                                    if let Some(err_val_type) = err {
                                                        let err_decl = match err_val_type {
                                                          wasmparser::ComponentValType::Primitive(prim) => {
                                                            match prim {
                                                              wasmparser::PrimitiveValType::Bool => ValType::Primitive(PrimitiveDecl::Bool),
                                                              wasmparser::PrimitiveValType::S8 => ValType::Primitive(PrimitiveDecl::S8),
                                                              wasmparser::PrimitiveValType::U8 => ValType::Primitive(PrimitiveDecl::U8),
                                                              wasmparser::PrimitiveValType::S16 => ValType::Primitive(PrimitiveDecl::S16),
                                                              wasmparser::PrimitiveValType::U16 => ValType::Primitive(PrimitiveDecl::U16),
                                                              wasmparser::PrimitiveValType::S32 => ValType::Primitive(PrimitiveDecl::S32),
                                                              wasmparser::PrimitiveValType::U32 => ValType::Primitive(PrimitiveDecl::U32),
                                                              wasmparser::PrimitiveValType::S64 => ValType::Primitive(PrimitiveDecl::S64),
                                                              wasmparser::PrimitiveValType::U64 => ValType::Primitive(PrimitiveDecl::U64),
                                                              wasmparser::PrimitiveValType::Float32 => ValType::Primitive(PrimitiveDecl::Float32),
                                                              wasmparser::PrimitiveValType::Float64 => ValType::Primitive(PrimitiveDecl::Float64),
                                                              wasmparser::PrimitiveValType::Char => ValType::Primitive(PrimitiveDecl::Char),
                                                              wasmparser::PrimitiveValType::String => ValType::Primitive(PrimitiveDecl::String),
                                                            }
                                                          },
                                                          wasmparser::ComponentValType::Type(index) => {
                                                            ValType::Type(index)
                                                          }
                                                        };
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Result((
                                                                        Some(ok_decl),
                                                                        Some(err_decl),
                                                                    )),
                                                                ),
                                                            ),
                                                        );
                                                    } else {
                                                        instance_decls.push(
                                                            InstanceTypeDecl::Type(
                                                                TypeDecl::Defined(
                                                                    DefinedDecl::Result((
                                                                        Some(ok_decl),
                                                                        None,
                                                                    )),
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                } else {
                                                    instance_decls.push(InstanceTypeDecl::Type(
                                                        TypeDecl::Defined(DefinedDecl::Result((
                                                            None, None,
                                                        ))),
                                                    ));
                                                }
                                            }
                                            wasmparser::ComponentDefinedType::Own(_) => todo!(),
                                            wasmparser::ComponentDefinedType::Borrow(_) => todo!(),
                                        };
                                    }
                                    ComponentType::Func(func) => {
                                        let params_decl = func
                                            .params
                                            .iter()
                                            .map(|(name, ty)| match ty {
                                                wasmparser::ComponentValType::Primitive(prim) => {
                                                    match prim {
                                                        wasmparser::PrimitiveValType::Bool => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::Bool),
                                                        ),
                                                        wasmparser::PrimitiveValType::S8 => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::S8),
                                                        ),
                                                        wasmparser::PrimitiveValType::U8 => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::U8),
                                                        ),
                                                        wasmparser::PrimitiveValType::S16 => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::S16),
                                                        ),
                                                        wasmparser::PrimitiveValType::U16 => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::U16),
                                                        ),
                                                        wasmparser::PrimitiveValType::S32 => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::S32),
                                                        ),
                                                        wasmparser::PrimitiveValType::U32 => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::U32),
                                                        ),
                                                        wasmparser::PrimitiveValType::S64 => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::S64),
                                                        ),
                                                        wasmparser::PrimitiveValType::U64 => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::U64),
                                                        ),
                                                        wasmparser::PrimitiveValType::Float32 => (
                                                            name.to_string(),
                                                            ValType::Primitive(
                                                                PrimitiveDecl::Float32,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::Float64 => (
                                                            name.to_string(),
                                                            ValType::Primitive(
                                                                PrimitiveDecl::Float64,
                                                            ),
                                                        ),
                                                        wasmparser::PrimitiveValType::Char => (
                                                            name.to_string(),
                                                            ValType::Primitive(PrimitiveDecl::Char),
                                                        ),
                                                        wasmparser::PrimitiveValType::String => (
                                                            name.to_string(),
                                                            ValType::Primitive(
                                                                PrimitiveDecl::String,
                                                            ),
                                                        ),
                                                    }
                                                }
                                                wasmparser::ComponentValType::Type(i) => {
                                                    (name.to_string(), ValType::Type(*i))
                                                }
                                            })
                                            .collect();
                                        match func.results {
                                            wasmparser::ComponentFuncResult::Unnamed(ty) => {
                                                let result_decl = match ty {
                                                    wasmparser::ComponentValType::Primitive(
                                                        prim,
                                                    ) => match prim {
                                                        wasmparser::PrimitiveValType::Bool => {
                                                            ValType::Primitive(PrimitiveDecl::Bool)
                                                        }
                                                        wasmparser::PrimitiveValType::S8 => {
                                                            ValType::Primitive(PrimitiveDecl::S8)
                                                        }
                                                        wasmparser::PrimitiveValType::U8 => {
                                                            ValType::Primitive(PrimitiveDecl::U8)
                                                        }
                                                        wasmparser::PrimitiveValType::S16 => {
                                                            ValType::Primitive(PrimitiveDecl::S16)
                                                        }
                                                        wasmparser::PrimitiveValType::U16 => {
                                                            ValType::Primitive(PrimitiveDecl::U16)
                                                        }
                                                        wasmparser::PrimitiveValType::S32 => {
                                                            ValType::Primitive(PrimitiveDecl::S32)
                                                        }
                                                        wasmparser::PrimitiveValType::U32 => {
                                                            ValType::Primitive(PrimitiveDecl::U32)
                                                        }
                                                        wasmparser::PrimitiveValType::S64 => {
                                                            ValType::Primitive(PrimitiveDecl::S64)
                                                        }
                                                        wasmparser::PrimitiveValType::U64 => {
                                                            ValType::Primitive(PrimitiveDecl::U64)
                                                        }
                                                        wasmparser::PrimitiveValType::Float32 => {
                                                            ValType::Primitive(
                                                                PrimitiveDecl::Float32,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::Float64 => {
                                                            ValType::Primitive(
                                                                PrimitiveDecl::Float64,
                                                            )
                                                        }
                                                        wasmparser::PrimitiveValType::Char => {
                                                            ValType::Primitive(PrimitiveDecl::Char)
                                                        }
                                                        wasmparser::PrimitiveValType::String => {
                                                            ValType::Primitive(
                                                                PrimitiveDecl::String,
                                                            )
                                                        }
                                                    },
                                                    wasmparser::ComponentValType::Type(i) => {
                                                        ValType::Type(i)
                                                    }
                                                };
                                                instance_decls.push(InstanceTypeDecl::Type(
                                                    TypeDecl::Func(FuncDecl {
                                                        params: params_decl,
                                                        results: FuncResult::Unnamed(result_decl),
                                                    }),
                                                ));
                                            }
                                            wasmparser::ComponentFuncResult::Named(ty) => {
                                                let mut results = Vec::new();
                                                let mut results_decl = Vec::new();
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
                                                          wasmparser::PrimitiveValType::Float32 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::Float32))),
                                                          wasmparser::PrimitiveValType::Float64 => results.push((*name, ComponentValType::Primitive(PrimitiveValType::Float64))),
                                                          wasmparser::PrimitiveValType::Char => results.push((*name, ComponentValType::Primitive(PrimitiveValType::Char))),
                                                          wasmparser::PrimitiveValType::String => results.push((*name, ComponentValType::Primitive(PrimitiveValType::String))),
                                                      }
                                                      },
                                                      wasmparser::ComponentValType::Type(index) => results.push((*name, ComponentValType::Type(*index))),
                                                    }
                                                }
                                                for (name, val) in ty.iter() {
                                                    match val {
                                                    wasmparser::ComponentValType::Primitive(prim) => {
                                                      match prim {
                                                        wasmparser::PrimitiveValType::Bool => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::Bool))),
                                                        wasmparser::PrimitiveValType::S8 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::S8))),
                                                        wasmparser::PrimitiveValType::U8 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::U8))),
                                                        wasmparser::PrimitiveValType::S16 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::S16))),
                                                        wasmparser::PrimitiveValType::U16 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::U16))),
                                                        wasmparser::PrimitiveValType::S32 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::S32))),
                                                        wasmparser::PrimitiveValType::U32 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::U32))),
                                                        wasmparser::PrimitiveValType::S64 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::S64))),
                                                        wasmparser::PrimitiveValType::U64 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::U64))),
                                                        wasmparser::PrimitiveValType::Float32 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::Float32))),
                                                        wasmparser::PrimitiveValType::Float64 => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::Float64))),
                                                        wasmparser::PrimitiveValType::Char => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::Char))),
                                                        wasmparser::PrimitiveValType::String => results_decl.push((name.to_string(), ValType::Primitive(PrimitiveDecl::String))),
                                                    }
                                                    },
                                                    wasmparser::ComponentValType::Type(index) => results_decl.push((name.to_string(), ValType::Type(*index))),
                                                  }
                                                }
                                                instance_decls.push(InstanceTypeDecl::Type(
                                                    TypeDecl::Func(FuncDecl {
                                                        params: params_decl,
                                                        results: FuncResult::Named(results_decl),
                                                    }),
                                                ));
                                            }
                                        };
                                    }
                                    ComponentType::Component(_) => {}
                                    ComponentType::Instance(_) => {}
                                    ComponentType::Resource { .. } => {}
                                }
                              instance_type_count += 1;
                            }
                            wasmparser::InstanceTypeDeclaration::Alias(ty) => {
                                let alias_decl = match ty {
                                    wasmparser::ComponentAlias::InstanceExport { .. } => todo!(),
                                    wasmparser::ComponentAlias::CoreInstanceExport { .. } => {
                                        todo!()
                                    }
                                    wasmparser::ComponentAlias::Outer { kind, count, index } => {
                                        OuterDecl {
                                            kind: match kind {
                                                wasmparser::ComponentOuterAliasKind::CoreModule => {
                                                    OuterAliasKindDecl::CoreModule
                                                }
                                                wasmparser::ComponentOuterAliasKind::CoreType => {
                                                    OuterAliasKindDecl::CoreType
                                                }
                                                wasmparser::ComponentOuterAliasKind::Type => {
                                                    OuterAliasKindDecl::Type
                                                }
                                                wasmparser::ComponentOuterAliasKind::Component => {
                                                    OuterAliasKindDecl::Component
                                                }
                                            },
                                            count,
                                            index: index as u32,
                                        }
                                    }
                                };
                                instance_decls
                                    .push(InstanceTypeDecl::Alias(AliasDecl::Outer(alias_decl)));
                            }
                            wasmparser::InstanceTypeDeclaration::Export { name, ty } => {
                                let export_name_decl = match name {
                                    wasmparser::ComponentExternName::Kebab(name) => {
                                        exports.push(name.to_string());
                                        ExternNameDecl::Kebab(name.to_string())
                                    }
                                    wasmparser::ComponentExternName::Interface(name) => {
                                        exports.push(name.to_string());
                                        ExternNameDecl::Interface(name.to_string())
                                    }
                                    wasmparser::ComponentExternName::Implementation(imp) => {
                                        match imp {
                                            wasmparser::ImplementationImport::Url(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                },
                                            ) => {
                                                exports.push(name.to_string());
                                                ExternNameDecl::Implementation(
                                                    ImplementationDecl::Url(Metadata {
                                                        name: name.to_string(),
                                                        location: location.to_string(),
                                                        integrity: Some(integrity.to_string()),
                                                    }),
                                                )
                                            }
                                            wasmparser::ImplementationImport::Relative(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                },
                                            ) => {
                                                exports.push(name.to_string());
                                                ExternNameDecl::Implementation(
                                                    ImplementationDecl::Relative(Metadata {
                                                        name: name.to_string(),
                                                        location: location.to_string(),
                                                        integrity: Some(integrity.to_string()),
                                                    }),
                                                )
                                            }
                                            wasmparser::ImplementationImport::Naked(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                },
                                            ) => {
                                                exports.push(name.to_string());
                                                ExternNameDecl::Implementation(
                                                    ImplementationDecl::Naked(Metadata {
                                                        name: name.to_string(),
                                                        location: location.to_string(),
                                                        integrity: Some(integrity.to_string()),
                                                    }),
                                                )
                                            }
                                            wasmparser::ImplementationImport::Locked(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                },
                                            ) => {
                                                exports.push(name.to_string());
                                                ExternNameDecl::Implementation(
                                                    ImplementationDecl::Locked(Metadata {
                                                        name: name.to_string(),
                                                        location: location.to_string(),
                                                        integrity: Some(integrity.to_string()),
                                                    }),
                                                )
                                            }
                                            wasmparser::ImplementationImport::Unlocked(
                                                wasmparser::ImportMetadata {
                                                    name,
                                                    location,
                                                    integrity,
                                                },
                                            ) => {
                                                exports.push(name.to_string());
                                                ExternNameDecl::Implementation(
                                                    ImplementationDecl::Unlocked(Metadata {
                                                        name: name.to_string(),
                                                        location: location.to_string(),
                                                        integrity: Some(integrity.to_string()),
                                                    }),
                                                )
                                            }
                                        }
                                    }
                                };
                                let export_type_decl = match ty {
                                    wasmparser::ComponentTypeRef::Module(i) => {
                                        TypeRefDecl::Module(i)
                                    }
                                    wasmparser::ComponentTypeRef::Func(i) => TypeRefDecl::Func(i),
                                    wasmparser::ComponentTypeRef::Value(ty) => match ty {
                                        wasmparser::ComponentValType::Primitive(prim) => match prim
                                        {
                                            wasmparser::PrimitiveValType::Bool => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::Bool),
                                            ),
                                            wasmparser::PrimitiveValType::S8 => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::S8),
                                            ),
                                            wasmparser::PrimitiveValType::U8 => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::U8),
                                            ),
                                            wasmparser::PrimitiveValType::S16 => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::S16),
                                            ),
                                            wasmparser::PrimitiveValType::U16 => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::U16),
                                            ),
                                            wasmparser::PrimitiveValType::S32 => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::S32),
                                            ),
                                            wasmparser::PrimitiveValType::U32 => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::U32),
                                            ),
                                            wasmparser::PrimitiveValType::S64 => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::S64),
                                            ),
                                            wasmparser::PrimitiveValType::U64 => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::U64),
                                            ),
                                            wasmparser::PrimitiveValType::Float32 => {
                                                TypeRefDecl::Val(ValType::Primitive(
                                                    PrimitiveDecl::Float32,
                                                ))
                                            }
                                            wasmparser::PrimitiveValType::Float64 => {
                                                TypeRefDecl::Val(ValType::Primitive(
                                                    PrimitiveDecl::Float64,
                                                ))
                                            }
                                            wasmparser::PrimitiveValType::Char => TypeRefDecl::Val(
                                                ValType::Primitive(PrimitiveDecl::Char),
                                            ),
                                            wasmparser::PrimitiveValType::String => {
                                                TypeRefDecl::Val(ValType::Primitive(
                                                    PrimitiveDecl::String,
                                                ))
                                            }
                                        },
                                        wasmparser::ComponentValType::Type(i) => {
                                            TypeRefDecl::Val(ValType::Type(i))
                                        }
                                    },
                                    wasmparser::ComponentTypeRef::Type(bounds) =>{
                                      instance_type_count += 1;
                                      match bounds {
                                          TypeBounds::Eq(i) => {
                                              TypeRefDecl::Type(TypeBoundsDecl::Eq(i))
                                          }
                                          TypeBounds::SubResource => {
                                              TypeRefDecl::Type(TypeBoundsDecl::SubResource)
                                          }
                                      }
                                    },
                                    wasmparser::ComponentTypeRef::Instance(i) => {
                                        TypeRefDecl::Instance(i)
                                    }
                                    wasmparser::ComponentTypeRef::Component(i) => {
                                        TypeRefDecl::Component(i)
                                    }
                                };
                                instance_decls.push(InstanceTypeDecl::Export(ExportDecl {
                                    name: export_name_decl,
                                    ty: export_type_decl,
                                }))
                            }
                        }
                    }
                    graph.type_decls.insert(
                        graph.type_count,
                        ComponentTypeDecl {
                            decls: instance_decls.clone(),
                            exports,
                            type_count: instance_type_count
                        },
                    );
                    graph.type_count += 1;
                }
                ComponentType::Defined(def) => {
                    match def {
                        wasmparser::ComponentDefinedType::Primitive(prim) => match prim {
                            wasmparser::PrimitiveValType::Bool => type_section
                                .defined_type()
                                .primitive(PrimitiveValType::Bool),
                            wasmparser::PrimitiveValType::S8 => {
                                type_section.defined_type().primitive(PrimitiveValType::S8)
                            }
                            wasmparser::PrimitiveValType::U8 => {
                                type_section.defined_type().primitive(PrimitiveValType::U8)
                            }
                            wasmparser::PrimitiveValType::S16 => {
                                type_section.defined_type().primitive(PrimitiveValType::S16)
                            }
                            wasmparser::PrimitiveValType::U16 => {
                                type_section.defined_type().primitive(PrimitiveValType::U16)
                            }
                            wasmparser::PrimitiveValType::S32 => {
                                type_section.defined_type().primitive(PrimitiveValType::S32)
                            }
                            wasmparser::PrimitiveValType::U32 => {
                                type_section.defined_type().primitive(PrimitiveValType::U32)
                            }
                            wasmparser::PrimitiveValType::S64 => {
                                type_section.defined_type().primitive(PrimitiveValType::S64)
                            }
                            wasmparser::PrimitiveValType::U64 => {
                                type_section.defined_type().primitive(PrimitiveValType::U64)
                            }
                            wasmparser::PrimitiveValType::Float32 => type_section
                                .defined_type()
                                .primitive(PrimitiveValType::Float32),
                            wasmparser::PrimitiveValType::Float64 => type_section
                                .defined_type()
                                .primitive(PrimitiveValType::Float64),
                            wasmparser::PrimitiveValType::Char => type_section
                                .defined_type()
                                .primitive(PrimitiveValType::Char),
                            wasmparser::PrimitiveValType::String => type_section
                                .defined_type()
                                .primitive(PrimitiveValType::String),
                        },
                        wasmparser::ComponentDefinedType::Record(rec) => {
                            let record = rec.iter().map(|(name, ty)| match ty {
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
                                wasmparser::ComponentValType::Type(_) => todo!(),
                            });
                            type_section.defined_type().record(record);
                        }
                        wasmparser::ComponentDefinedType::Variant(variant) => {
                            let cases = variant.iter().map(|VariantCase { name, ty, refines }| {
                                if let Some(ty) = ty {
                                    return match ty {
                                        wasmparser::ComponentValType::Primitive(prim) => match prim
                                        {
                                            wasmparser::PrimitiveValType::Bool => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::Bool,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::S8 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::S8,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::U8 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::U8,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::S16 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::S16,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::U16 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::U16,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::S32 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::S32,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::U32 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::U32,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::S64 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::S64,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::U64 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::U64,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::Float32 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::Float32,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::Float64 => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::Float64,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::Char => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::Char,
                                                )),
                                                *refines,
                                            ),
                                            wasmparser::PrimitiveValType::String => (
                                                *name,
                                                Some(ComponentValType::Primitive(
                                                    PrimitiveValType::String,
                                                )),
                                                *refines,
                                            ),
                                        },
                                        wasmparser::ComponentValType::Type(i) => {
                                            (*name, Some(ComponentValType::Type(*i)), *refines)
                                        }
                                    };
                                }
                                (*name, None, *refines)
                            });
                            type_section.defined_type().variant(cases)
                        }
                        wasmparser::ComponentDefinedType::List(ty) => {
                            let ty = match ty {
                                wasmparser::ComponentValType::Primitive(prim) => match prim {
                                    wasmparser::PrimitiveValType::Bool => {
                                        ComponentValType::Primitive(PrimitiveValType::Bool)
                                    }
                                    wasmparser::PrimitiveValType::S8 => {
                                        ComponentValType::Primitive(PrimitiveValType::S8)
                                    }
                                    wasmparser::PrimitiveValType::U8 => {
                                        ComponentValType::Primitive(PrimitiveValType::U8)
                                    }
                                    wasmparser::PrimitiveValType::S16 => {
                                        ComponentValType::Primitive(PrimitiveValType::S16)
                                    }
                                    wasmparser::PrimitiveValType::U16 => {
                                        ComponentValType::Primitive(PrimitiveValType::U16)
                                    }
                                    wasmparser::PrimitiveValType::S32 => {
                                        ComponentValType::Primitive(PrimitiveValType::S32)
                                    }
                                    wasmparser::PrimitiveValType::U32 => {
                                        ComponentValType::Primitive(PrimitiveValType::U32)
                                    }
                                    wasmparser::PrimitiveValType::S64 => {
                                        ComponentValType::Primitive(PrimitiveValType::S64)
                                    }
                                    wasmparser::PrimitiveValType::U64 => {
                                        ComponentValType::Primitive(PrimitiveValType::U64)
                                    }
                                    wasmparser::PrimitiveValType::Float32 => {
                                        ComponentValType::Primitive(PrimitiveValType::Float32)
                                    }
                                    wasmparser::PrimitiveValType::Float64 => {
                                        ComponentValType::Primitive(PrimitiveValType::Float64)
                                    }
                                    wasmparser::PrimitiveValType::Char => {
                                        ComponentValType::Primitive(PrimitiveValType::Char)
                                    }
                                    wasmparser::PrimitiveValType::String => {
                                        ComponentValType::Primitive(PrimitiveValType::String)
                                    }
                                },
                                wasmparser::ComponentValType::Type(i) => ComponentValType::Type(i),
                            };
                            type_section.defined_type().list(ty);
                        }
                        wasmparser::ComponentDefinedType::Tuple(tuple) => {
                            let types = tuple.iter().map(|ty| match ty {
                                wasmparser::ComponentValType::Primitive(prim) => match prim {
                                    wasmparser::PrimitiveValType::Bool => {
                                        ComponentValType::Primitive(PrimitiveValType::Bool)
                                    }
                                    wasmparser::PrimitiveValType::S8 => {
                                        ComponentValType::Primitive(PrimitiveValType::S8)
                                    }
                                    wasmparser::PrimitiveValType::U8 => {
                                        ComponentValType::Primitive(PrimitiveValType::U8)
                                    }
                                    wasmparser::PrimitiveValType::S16 => {
                                        ComponentValType::Primitive(PrimitiveValType::S16)
                                    }
                                    wasmparser::PrimitiveValType::U16 => {
                                        ComponentValType::Primitive(PrimitiveValType::U16)
                                    }
                                    wasmparser::PrimitiveValType::S32 => {
                                        ComponentValType::Primitive(PrimitiveValType::S32)
                                    }
                                    wasmparser::PrimitiveValType::U32 => {
                                        ComponentValType::Primitive(PrimitiveValType::U32)
                                    }
                                    wasmparser::PrimitiveValType::S64 => {
                                        ComponentValType::Primitive(PrimitiveValType::S64)
                                    }
                                    wasmparser::PrimitiveValType::U64 => {
                                        ComponentValType::Primitive(PrimitiveValType::U64)
                                    }
                                    wasmparser::PrimitiveValType::Float32 => {
                                        ComponentValType::Primitive(PrimitiveValType::Float32)
                                    }
                                    wasmparser::PrimitiveValType::Float64 => {
                                        ComponentValType::Primitive(PrimitiveValType::Float64)
                                    }
                                    wasmparser::PrimitiveValType::Char => {
                                        ComponentValType::Primitive(PrimitiveValType::Char)
                                    }
                                    wasmparser::PrimitiveValType::String => {
                                        ComponentValType::Primitive(PrimitiveValType::String)
                                    }
                                },
                                wasmparser::ComponentValType::Type(i) => ComponentValType::Type(*i),
                            });
                            type_section.defined_type().tuple(types);
                        }
                        wasmparser::ComponentDefinedType::Flags(flags) => {
                            type_section.defined_type().flags(flags.iter().map(|f| *f));
                        }
                        wasmparser::ComponentDefinedType::Enum(tags) => {
                            type_section
                                .defined_type()
                                .enum_type(tags.iter().map(|f| *f));
                        }
                        wasmparser::ComponentDefinedType::Union(_) => todo!(),
                        wasmparser::ComponentDefinedType::Option(_) => todo!(),
                        wasmparser::ComponentDefinedType::Result { ok, err } => {
                            if let Some(ok) = ok {
                                let ok_type = match ok {
                                    wasmparser::ComponentValType::Primitive(prim) => match prim {
                                        wasmparser::PrimitiveValType::Bool => {
                                            ComponentValType::Primitive(PrimitiveValType::Bool)
                                        }
                                        wasmparser::PrimitiveValType::S8 => {
                                            ComponentValType::Primitive(PrimitiveValType::S8)
                                        }
                                        wasmparser::PrimitiveValType::U8 => {
                                            ComponentValType::Primitive(PrimitiveValType::U8)
                                        }
                                        wasmparser::PrimitiveValType::S16 => {
                                            ComponentValType::Primitive(PrimitiveValType::S16)
                                        }
                                        wasmparser::PrimitiveValType::U16 => {
                                            ComponentValType::Primitive(PrimitiveValType::U16)
                                        }
                                        wasmparser::PrimitiveValType::S32 => {
                                            ComponentValType::Primitive(PrimitiveValType::S32)
                                        }
                                        wasmparser::PrimitiveValType::U32 => {
                                            ComponentValType::Primitive(PrimitiveValType::U32)
                                        }
                                        wasmparser::PrimitiveValType::S64 => {
                                            ComponentValType::Primitive(PrimitiveValType::S64)
                                        }
                                        wasmparser::PrimitiveValType::U64 => {
                                            ComponentValType::Primitive(PrimitiveValType::U64)
                                        }
                                        wasmparser::PrimitiveValType::Float32 => {
                                            ComponentValType::Primitive(PrimitiveValType::Float32)
                                        }
                                        wasmparser::PrimitiveValType::Float64 => {
                                            ComponentValType::Primitive(PrimitiveValType::Float64)
                                        }
                                        wasmparser::PrimitiveValType::Char => {
                                            ComponentValType::Primitive(PrimitiveValType::Char)
                                        }
                                        wasmparser::PrimitiveValType::String => {
                                            ComponentValType::Primitive(PrimitiveValType::String)
                                        }
                                    },
                                    wasmparser::ComponentValType::Type(i) => {
                                        ComponentValType::Type(i)
                                    }
                                };
                                if let Some(err) = err {
                                    let err_type = match err {
                                        wasmparser::ComponentValType::Primitive(prim) => match prim
                                        {
                                            wasmparser::PrimitiveValType::Bool => {
                                                ComponentValType::Primitive(PrimitiveValType::Bool)
                                            }
                                            wasmparser::PrimitiveValType::S8 => {
                                                ComponentValType::Primitive(PrimitiveValType::S8)
                                            }
                                            wasmparser::PrimitiveValType::U8 => {
                                                ComponentValType::Primitive(PrimitiveValType::U8)
                                            }
                                            wasmparser::PrimitiveValType::S16 => {
                                                ComponentValType::Primitive(PrimitiveValType::S16)
                                            }
                                            wasmparser::PrimitiveValType::U16 => {
                                                ComponentValType::Primitive(PrimitiveValType::U16)
                                            }
                                            wasmparser::PrimitiveValType::S32 => {
                                                ComponentValType::Primitive(PrimitiveValType::S32)
                                            }
                                            wasmparser::PrimitiveValType::U32 => {
                                                ComponentValType::Primitive(PrimitiveValType::U32)
                                            }
                                            wasmparser::PrimitiveValType::S64 => {
                                                ComponentValType::Primitive(PrimitiveValType::S64)
                                            }
                                            wasmparser::PrimitiveValType::U64 => {
                                                ComponentValType::Primitive(PrimitiveValType::U64)
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
                                                ComponentValType::Primitive(PrimitiveValType::Char)
                                            }
                                            wasmparser::PrimitiveValType::String => {
                                                ComponentValType::Primitive(
                                                    PrimitiveValType::String,
                                                )
                                            }
                                        },
                                        wasmparser::ComponentValType::Type(i) => {
                                            ComponentValType::Type(i)
                                        }
                                    };
                                    type_section
                                        .defined_type()
                                        .result(Some(ok_type), Some(err_type));
                                }
                                type_section.defined_type().result(Some(ok_type), None);
                            }
                            type_section.defined_type().result(None, None);
                        }
                        wasmparser::ComponentDefinedType::Own(_) => todo!(),
                        wasmparser::ComponentDefinedType::Borrow(_) => todo!(),
                    }
                    // graph.entities.push(Entity::Type(type_section.clone()));
                }
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
                ComponentType::Resource { .. } => todo!(),
            }
        }
        Ok(())
    }

    pub fn parse_import(
        &self,
        parser: &ComponentImportSectionReader,
        graph: &mut Graph,
    ) -> Result<()> {
        let component_index = graph.num_components - 1;
        let clone = parser.clone();
        for (i, import) in clone.into_iter_with_offsets().enumerate() {
            let mut final_import = Import::new("dummy".to_string(), ImportKind::Kebab);
            let mut imports = ComponentImportSection::new();
            let (_, imp) = import.unwrap().clone();
            match imp.name {
                wasmparser::ComponentExternName::Kebab(_) => {}
                wasmparser::ComponentExternName::Interface(name) => {
                    match imp.ty {
                        wasmparser::ComponentTypeRef::Module(i) => {
                            imports.import(name, ComponentTypeRef::Module(i));
                        }
                        wasmparser::ComponentTypeRef::Func(i) => {
                            imports.import(name, ComponentTypeRef::Func(i));
                        }
                        wasmparser::ComponentTypeRef::Value(ty) => match ty {
                            wasmparser::ComponentValType::Primitive(prim) => match prim {
                                wasmparser::PrimitiveValType::Bool => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::Bool,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::U8 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::U8,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::S8 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::S8,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::U16 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::U16,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::S16 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::S16,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::U32 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::U32,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::S32 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::S32,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::U64 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::U64,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::S64 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::S64,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::Float32 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::Float32,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::Float64 => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::Float64,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::Char => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::Char,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::String => {
                                    imports.import(
                                        name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::String,
                                        )),
                                    );
                                }
                            },
                            wasmparser::ComponentValType::Type(i) => {
                                imports.import(
                                    name,
                                    ComponentTypeRef::Type(wasm_encoder::TypeBounds::Eq(i)),
                                );
                            }
                        },
                        wasmparser::ComponentTypeRef::Type(bounds) => match bounds {
                            TypeBounds::Eq(i) => {
                                imports.import(
                                    name,
                                    ComponentTypeRef::Type(wasm_encoder::TypeBounds::Eq(i)),
                                );
                            }
                            TypeBounds::SubResource => {
                                imports.import(
                                    name,
                                    ComponentTypeRef::Type(wasm_encoder::TypeBounds::SubResource),
                                );
                            }
                        },
                        wasmparser::ComponentTypeRef::Instance(i) => {
                            if !graph.imported.contains(name) {
                                let ty = graph.type_decls.get(&(i as usize - &graph.num_aliases));
                                if let Some(ty) = ty {
                                    // let mut type_section = ComponentTypeSection::new();
                                    // type_section.ty().instance(&ty.encode_instance());
                                    graph.entities.push(Entity::Type((
                                        name.to_string(),
                                        TypeEntityType::Instance,
                                    )));
                                }
                                dbg!(name, i as usize - graph.num_aliases);
                                graph
                                    .interface_type_indices
                                    .insert(name.to_string(), i as usize - graph.num_aliases);
                                graph.imported.insert(name.to_string());
                                imports.import(name, ComponentTypeRef::Instance(i));
                            } else {
                                // dbg!(&graph.type_decls);
                                // if name == "wasi:filesystem/filesystem" || name == "wasi:io/streams" {
                                // dbg!(graph.last_type, i);
                                dbg!(graph.type_decls.len());
                                let cloned = graph.type_decls.clone();
                                // let new_decl = cloned.get(&(graph.last_type + i as usize));
                                let new_decl = cloned.get(&(graph.type_decls.len() - 1));
                                // dbg!(&new_decl);
                                if let Some(new) = new_decl {
                                    // match new {
                                    // ComponentTypeDecl::Instance(new_decls) => {
                                    let orig_index = graph.interface_type_indices.get(name);
                                    if let Some(i) = orig_index {
                                        let orig_type = graph.type_decls.get_mut(i);
                                        if let Some(orig) = orig_type {
                                            // match orig {
                                            // ComponentTypeDecl::Instance(orig_decls) => {
                                            let merge_index = orig.type_count - 1;
                                            for decl in &new.decls {
                                                match decl {
                                                InstanceTypeDecl::CoreType => todo!(),
                                                InstanceTypeDecl::Type(ty) => match ty {
                                                    TypeDecl::Defined(def) => match def {
                                                        DefinedDecl::Primitive(prim) => match prim {
                                                            PrimitiveDecl::Bool => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::Bool)))),
                                                            PrimitiveDecl::S8 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::S8)))),
                                                            PrimitiveDecl::U8 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::U8)))),
                                                            PrimitiveDecl::S16 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::S16)))),
                                                            PrimitiveDecl::U16 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::U16)))),
                                                            PrimitiveDecl::S32 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::S32)))),
                                                            PrimitiveDecl::U32 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::U32)))),
                                                            PrimitiveDecl::S64 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::S64)))),
                                                            PrimitiveDecl::U64 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::U64)))),
                                                            PrimitiveDecl::Float32 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::Float32)))),
                                                            PrimitiveDecl::Float64 => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::Float64)))),
                                                            PrimitiveDecl::Char => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::Char)))),
                                                            PrimitiveDecl::String => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Primitive(PrimitiveDecl::String)))),
                                                        },
                                                        DefinedDecl::Record(rec) => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Record(rec.clone())))),
                                                        DefinedDecl::Variant(val) => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Variant(val.clone())))),
                                                        DefinedDecl::List(val) => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::List(val.clone())))),
                                                        DefinedDecl::Tuple(vals) => {
                                                          let mut new_vals = Vec::<ValType>::new();
                                                          for val in vals {
                                                            match val {
                                                                ValType::Primitive(prim) => match prim {
                                                                    PrimitiveDecl::Bool => new_vals.push(ValType::Primitive(PrimitiveDecl::Bool)),
                                                                    PrimitiveDecl::S8 => new_vals.push(ValType::Primitive(PrimitiveDecl::S8)),
                                                                    PrimitiveDecl::U8 => new_vals.push(ValType::Primitive(PrimitiveDecl::U8)),
                                                                    PrimitiveDecl::S16 => new_vals.push(ValType::Primitive(PrimitiveDecl::S16)),
                                                                    PrimitiveDecl::U16 => new_vals.push(ValType::Primitive(PrimitiveDecl::U16)),
                                                                    PrimitiveDecl::S32 => new_vals.push(ValType::Primitive(PrimitiveDecl::S32)),
                                                                    PrimitiveDecl::U32 => new_vals.push(ValType::Primitive(PrimitiveDecl::U32)),
                                                                    PrimitiveDecl::S64 => new_vals.push(ValType::Primitive(PrimitiveDecl::S64)),
                                                                    PrimitiveDecl::U64 => new_vals.push(ValType::Primitive(PrimitiveDecl::U64)),
                                                                    PrimitiveDecl::Float32 => new_vals.push(ValType::Primitive(PrimitiveDecl::Float32)),
                                                                    PrimitiveDecl::Float64 => new_vals.push(ValType::Primitive(PrimitiveDecl::Float64)),
                                                                    PrimitiveDecl::Char => new_vals.push(ValType::Primitive(PrimitiveDecl::Char)),
                                                                    PrimitiveDecl::String => new_vals.push(ValType::Primitive(PrimitiveDecl::String)),
                                                                },
                                                                ValType::Type(i) => new_vals.push(ValType::Type(i + merge_index as u32))
                                                            }
                                                          }
                                                          orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Tuple(new_vals.clone()))))
                                                        },
                                                        DefinedDecl::Flags(vals) => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Flags(vals.clone())))),
                                                        DefinedDecl::Enum(vals) => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Enum(vals.clone())))),
                                                        DefinedDecl::Union(_) => {},
                                                        DefinedDecl::Option(vals) => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Option(vals.clone())))),
                                                        DefinedDecl::Result(vals) => orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Defined(DefinedDecl::Result(vals.clone())))),
                                                    },
                                                    TypeDecl::Func(func) => {
                                                      let mut new_params = Vec::new();
                                                      for (name, ty) in func.params.clone() {
                                                        let remapped_ty = match ty {
                                                            ValType::Primitive(prim) => ValType::Primitive(prim),
                                                            ValType::Type(i) => ValType::Type(i + merge_index as u32),
                                                        };
                                                        new_params.push((name, remapped_ty));
                                                      }
                                                      orig.decls.push(InstanceTypeDecl::Type(TypeDecl::Func(FuncDecl {params: new_params, results: func.results.clone()})));
                                                    },
                                                },
                                                InstanceTypeDecl::Alias(_) => {},
                                                InstanceTypeDecl::Export(export) =>  {
                                                  let mut already_exported = false;
                                                  let export_name = match &export.name {
                                                    ExternNameDecl::Kebab(name) => {
                                                      if orig.exports.contains(name) {
                                                        already_exported = true;
                                                      }
                                                      ExternNameDecl::Kebab(name.to_string())
                                                    },
                                                    ExternNameDecl::Interface(name) => {
                                                      if orig.exports.contains(name) {
                                                        already_exported = true;
                                                      }
                                                      ExternNameDecl::Interface(name.to_string())
                                                    },
                                                    ExternNameDecl::Implementation(imp) => match imp {
                                                        ImplementationDecl::Url(metadata) => {
                                                          if orig.exports.contains(&metadata.name) {
                                                            already_exported = true;
                                                          }
                                                          ExternNameDecl::Implementation(ImplementationDecl::Url(metadata.clone()))
                                                        },
                                                        ImplementationDecl::Relative(metadata) => {
                                                          if orig.exports.contains(&metadata.name) {
                                                            already_exported = true;
                                                          }
                                                          ExternNameDecl::Implementation(ImplementationDecl::Relative(metadata.clone()))
                                                        },
                                                        ImplementationDecl::Naked(metadata) => {
                                                          if orig.exports.contains(&metadata.name) {
                                                            already_exported = true;
                                                          }
                                                          ExternNameDecl::Implementation(ImplementationDecl::Naked(metadata.clone()))
                                                        },
                                                        ImplementationDecl::Locked(metadata) => {
                                                          if orig.exports.contains(&metadata.name) {
                                                            already_exported = true;
                                                          }
                                                          ExternNameDecl::Implementation(ImplementationDecl::Locked(metadata.clone()))
                                                        },
                                                        ImplementationDecl::Unlocked(metadata) => {
                                                          if orig.exports.contains(&metadata.name) {
                                                            already_exported = true;
                                                          }
                                                          ExternNameDecl::Implementation(ImplementationDecl::Unlocked(metadata.clone()))
                                                        },
                                                    },
                                                  };
                                                  if !already_exported {

                                                  match &export.ty {
                                                    TypeRefDecl::Module(i) => orig.decls.push(InstanceTypeDecl::Export(ExportDecl { name: export_name, ty: TypeRefDecl::Module(*i + merge_index as u32)})),
                                                    TypeRefDecl::Func(i) => orig.decls.push(InstanceTypeDecl::Export(ExportDecl { name: export_name, ty: TypeRefDecl::Func(*i + merge_index as u32 - 1)})),
                                                    TypeRefDecl::Val(val) => {
                                                      match val {
                                                        ValType::Primitive(prim) => match prim {
                                                          PrimitiveDecl::Bool => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::Bool))})),
                                                          PrimitiveDecl::S8 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::S8))})),
                                                          PrimitiveDecl::U8 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::U8))})),
                                                          PrimitiveDecl::S16 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::S16))})),
                                                          PrimitiveDecl::U16 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::U16))})),
                                                          PrimitiveDecl::S32 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::S32))})),
                                                          PrimitiveDecl::U32 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::U32))})),
                                                          PrimitiveDecl::S64 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::S64))})),
                                                          PrimitiveDecl::U64 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::U64))})),
                                                          PrimitiveDecl::Float32 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::Float32))})),
                                                          PrimitiveDecl::Float64 => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::Float64))})),
                                                          PrimitiveDecl::Char => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::Char))})),
                                                          PrimitiveDecl::String => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Primitive(PrimitiveDecl::String))})),
                                                        }
                                                        ValType::Type(i) => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Val(ValType::Type(*i))})),
                                                      }
                                                    },
                                                    TypeRefDecl::Type(bounds) => match bounds {
                                                      TypeBoundsDecl::Eq(i) => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Type(TypeBoundsDecl::Eq(*i))})),
                                                      TypeBoundsDecl::SubResource => orig.decls.push(InstanceTypeDecl::Export(ExportDecl {name: export_name, ty: TypeRefDecl::Type(TypeBoundsDecl::SubResource)})),
                                                    },
                                                    TypeRefDecl::Instance(i) => orig.decls.push(InstanceTypeDecl::Export(ExportDecl { name: export_name, ty: TypeRefDecl::Instance(*i)})),
                                                    TypeRefDecl::Component(i) => orig.decls.push(InstanceTypeDecl::Export(ExportDecl { name: export_name, ty: TypeRefDecl::Component(*i)})),
                                                }
                                                  }
                                                },
                                                }
                                            }
                                            // },
                                            // }
                                        }
                                    }
                                    // },
                                    // }
                                }
                                // dbg!(&new_decls);
                                // }

                                //     dbg!(&orig);
                                //         dbg!(graph.last_type);
                                //         dbg!(graph.type_decls.len());
                                //         // dbg!(new_decls,
                                //           //  &ty
                                //           // );
                                //         // dbg!("&new_decls");
                                //     if let Some(new_decls) = new_decls {
                                //         let mut ty = graph.type_decls.get_mut(orig);
                                //     }
                                // }
                            }
                        }
                        wasmparser::ComponentTypeRef::Component(i) => {
                            imports.import(name, ComponentTypeRef::Component(i));
                        }
                    }
                    graph.insert_instance(name.to_string(), graph.num_instances);
                    final_import = Import::new(name.to_string(), ImportKind::Interface);
                    graph
                        .interfaces
                        .insert(name.to_string(), graph.num_instances);
                    let cur = graph.indices.get(&component_index);
                    graph.num_interfaces += 1;
                    if let Some(cur) = cur {
                        let component = graph.components.get_mut(&cur.name);
                        if let Some(comp) = component {
                            comp.instantiation_args.push(name.to_string());
                        }
                    }
                    graph.entities.push(Entity::Import((final_import, imports)));
                }
                wasmparser::ComponentExternName::Implementation(name) => {
                    let mut already_imported = false;
                    let extern_name = match name {
                        wasmparser::ImplementationImport::Url(wasmparser::ImportMetadata {
                            name,
                            location,
                            integrity,
                        }) => ComponentExternName::Implementation(ImplementationImport::Locked(
                            ImportMetadata {
                                name,
                                location,
                                integrity: Some(integrity),
                            },
                        )),
                        wasmparser::ImplementationImport::Relative(
                            wasmparser::ImportMetadata {
                                name,
                                location,
                                integrity,
                            },
                        ) => ComponentExternName::Implementation(ImplementationImport::Locked(
                            ImportMetadata {
                                name,
                                location,
                                integrity: Some(integrity),
                            },
                        )),
                        wasmparser::ImplementationImport::Naked(wasmparser::ImportMetadata {
                            name,
                            location,
                            integrity,
                        }) => ComponentExternName::Implementation(ImplementationImport::Locked(
                            ImportMetadata {
                                name,
                                location,
                                integrity: Some(integrity),
                            },
                        )),
                        wasmparser::ImplementationImport::Locked(wasmparser::ImportMetadata {
                            name,
                            location,
                            integrity,
                        }) => ComponentExternName::Implementation(ImplementationImport::Locked(
                            ImportMetadata {
                                name,
                                location,
                                integrity: Some(integrity),
                            },
                        )),
                        wasmparser::ImplementationImport::Unlocked(
                            wasmparser::ImportMetadata {
                                name,
                                location,
                                integrity,
                            },
                        ) => {
                            if graph.imported.contains(name) {
                                already_imported = true;
                                // return Ok(());
                            }
                            // let exists = graph.components.get(name);
                            // if let Some(_) = exists {
                            //   return Ok(());
                            // }
                            final_import =
                                Import::new(name.to_string(), ImportKind::Implementation);
                            let cur = graph.indices.get(&component_index);
                            if let Some(cur) = cur {
                                let component = graph.components.get_mut(&cur.name);
                                if let Some(comp) = component {
                                    comp.instantiation_args.push(name.to_string());
                                }
                            }
                            graph.imported.insert(name.to_string());
                            ComponentExternName::Implementation(ImplementationImport::Locked(
                                ImportMetadata {
                                    name,
                                    location,
                                    integrity: Some(integrity),
                                },
                            ))
                        }
                    };
                    if already_imported {
                        return Ok(());
                    }
                    match imp.ty {
                        wasmparser::ComponentTypeRef::Module(i) => {
                            imports.import(extern_name, ComponentTypeRef::Module(i));
                        }
                        wasmparser::ComponentTypeRef::Func(i) => {
                            imports.import(extern_name, ComponentTypeRef::Func(i));
                        }
                        wasmparser::ComponentTypeRef::Value(ty) => match ty {
                            wasmparser::ComponentValType::Primitive(prim) => match prim {
                                wasmparser::PrimitiveValType::Bool => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::Bool,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::U8 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::U8,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::S8 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::S8,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::U16 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::U16,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::S16 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::S16,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::U32 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::U32,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::S32 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::S32,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::U64 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::U64,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::S64 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::S64,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::Float32 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::Float32,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::Float64 => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::Float64,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::Char => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::Char,
                                        )),
                                    );
                                }
                                wasmparser::PrimitiveValType::String => {
                                    imports.import(
                                        extern_name,
                                        ComponentTypeRef::Value(ComponentValType::Primitive(
                                            PrimitiveValType::String,
                                        )),
                                    );
                                }
                            },
                            wasmparser::ComponentValType::Type(i) => {
                                imports.import(
                                    extern_name,
                                    ComponentTypeRef::Type(wasm_encoder::TypeBounds::Eq(i)),
                                );
                            }
                        },
                        wasmparser::ComponentTypeRef::Type(bounds) => match bounds {
                            TypeBounds::Eq(i) => {
                                imports.import(
                                    extern_name,
                                    ComponentTypeRef::Type(wasm_encoder::TypeBounds::Eq(i)),
                                );
                            }
                            TypeBounds::SubResource => {
                                imports.import(
                                    extern_name,
                                    ComponentTypeRef::Type(wasm_encoder::TypeBounds::SubResource),
                                );
                            }
                        },
                        wasmparser::ComponentTypeRef::Instance(_) => {
                            let ty = graph.type_decls.get(&(i + 1));
                            if let Some(ty) = ty {
                                graph.entities.push(Entity::Type((
                                    name.as_str().to_string(),
                                    TypeEntityType::Component,
                                )));
                            }
                            imports
                                .import(extern_name, ComponentTypeRef::Component((i + 1) as u32));
                            graph.entities.push(Entity::Import((final_import, imports)));
                        }
                        wasmparser::ComponentTypeRef::Component(_) => {
                            let ty = graph.type_decls.get(&(i + 1));
                            if let Some(ty) = ty {
                                graph.entities.push(Entity::Type((
                                    name.as_str().to_string(),
                                    TypeEntityType::Component,
                                )));
                            }
                            imports.import(
                                extern_name,
                                ComponentTypeRef::Component((graph.type_count) as u32),
                            );
                            graph.entities.push(Entity::Import((final_import, imports)));
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn parse_alias(
        &self,
        parser: &ComponentAliasSectionReader,
        graph: &mut Graph,
    ) -> Result<()> {
        let clone = parser.clone();
        let mut alias_section = ComponentAliasSection::new();
        for (_, alias) in clone.into_iter_with_offsets().enumerate() {
            let (_, al) = alias.unwrap();
            match al {
                ComponentAlias::InstanceExport {
                    kind,
                    instance_index,
                    name,
                } => {
                    match kind {
                        wasmparser::ComponentExternalKind::Module => {
                            alias_section.alias(Alias::InstanceExport {
                                instance: instance_index,
                                kind: wasm_encoder::ComponentExportKind::Module,
                                name,
                            });
                        }
                        wasmparser::ComponentExternalKind::Func => {
                            let component_index = graph.num_components - 1;
                            let component = graph.indices.get(&component_index);
                            if let Some(item) = component {
                                let dep = graph.components.get(&item.name);
                                if let Some(dep) = dep {
                                    if instance_index < dep.instantiation_args.len() as u32 {
                                        let instance_name =
                                            &dep.instantiation_args[instance_index as usize];
                                        let instance = graph.components.get(instance_name);
                                        if let Some(inst) = instance {
                                            alias_section.alias(Alias::InstanceExport {
                                                // instance: instance_index + (graph.num_components + graph.num_instances) as u32,
                                                instance: (inst.index + graph.num_instances) as u32,
                                                kind: wasm_encoder::ComponentExportKind::Func,
                                                name,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                        wasmparser::ComponentExternalKind::Value => {
                            alias_section.alias(Alias::InstanceExport {
                                instance: instance_index
                                    + (graph.num_components + graph.num_instances) as u32,
                                kind: wasm_encoder::ComponentExportKind::Value,
                                name,
                            });
                        }
                        wasmparser::ComponentExternalKind::Type => {
                            match kind {
                                wasmparser::ComponentExternalKind::Module => todo!(),
                                wasmparser::ComponentExternalKind::Func => {
                                    alias_section.alias(Alias::InstanceExport {
                                        instance: instance_index,
                                        kind: wasm_encoder::ComponentExportKind::Func,
                                        name,
                                    })
                                }
                                wasmparser::ComponentExternalKind::Value => todo!(),
                                wasmparser::ComponentExternalKind::Type => {
                                    graph.num_aliases += 1;
                                    alias_section.alias(Alias::InstanceExport {
                                        instance: instance_index,
                                        kind: wasm_encoder::ComponentExportKind::Type,
                                        name,
                                    })
                                }
                                wasmparser::ComponentExternalKind::Instance => todo!(),
                                wasmparser::ComponentExternalKind::Component => todo!(),
                            };
                        }
                        wasmparser::ComponentExternalKind::Instance => {

                            // alias_section.alias(Alias::InstanceExport {
                            //     instance: instance_index,
                            //     kind: wasm_encoder::ComponentExportKind::Instance,
                            //     name,
                            // });
                        }
                        wasmparser::ComponentExternalKind::Component => {
                            alias_section.alias(Alias::InstanceExport {
                                instance: instance_index,
                                kind: wasm_encoder::ComponentExportKind::Component,
                                name,
                            });
                        }
                    }
                }
                ComponentAlias::CoreInstanceExport {
                    kind,
                    instance_index,
                    name,
                } => {}
                ComponentAlias::Outer { kind, count, index } => match kind {
                    wasmparser::ComponentOuterAliasKind::CoreModule => {
                        alias_section.alias(Alias::Outer {
                            kind: ComponentOuterAliasKind::CoreModule,
                            count,
                            index,
                        });
                    }
                    wasmparser::ComponentOuterAliasKind::CoreType => {
                        alias_section.alias(Alias::Outer {
                            kind: ComponentOuterAliasKind::CoreType,
                            count,
                            index,
                        });
                    }
                    wasmparser::ComponentOuterAliasKind::Type => {
                        alias_section.alias(Alias::Outer {
                            kind: ComponentOuterAliasKind::Type,
                            count,
                            index,
                        });
                    }
                    wasmparser::ComponentOuterAliasKind::Component => {
                        alias_section.alias(Alias::Outer {
                            kind: ComponentOuterAliasKind::Component,
                            count,
                            index,
                        });
                    }
                },
            }
        }

        graph.entities.push(Entity::Alias(alias_section));
        Ok(())
    }

    pub fn parse<'a: 'b, 'b>(&mut self, mut bytes: &[u8], mut graph: &mut Graph) -> Result<()> {
        let mut parser = Parser::new(0);
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
                    self.parse_import_type(&s, &mut graph)?;
                }
                Payload::ComponentImportSection(s) => {
                    self.parse_import(&s, &mut graph)?;
                }
                Payload::ComponentAliasSection(s) => {
                    self.parse_alias(&s, &mut graph)?;
                }
                Payload::CodeSectionStart {
                    count: _,
                    range: _,
                    size: _,
                } => {
                    parser.skip_section();
                }
                Payload::ModuleSection { range, .. } => {
                    let offset = range.end - range.start;
                    if offset > bytes.len() {
                        bail!("invalid module or component section range");
                    }
                    bytes = &bytes[offset..];
                }
                Payload::ComponentSection { range, .. } => {
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
        graph.last_type = graph.type_count;
        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
