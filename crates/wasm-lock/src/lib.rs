use std::collections::{HashMap, HashSet};

use anyhow::{bail, Result};
use wasm_encoder::{
    Alias, ComponentAliasSection, ComponentExternName, ComponentImportSection,
    ComponentOuterAliasKind, ComponentTypeRef, ComponentTypeSection, ComponentValType, ImplementationImport, ImportMetadata, InstanceType, PrimitiveValType,
};
use wasmparser::{
    Chunk, ComponentAlias, ComponentAliasSectionReader, ComponentImportSectionReader,
    ComponentType, ComponentTypeSectionReader, Parser, Payload, TypeBounds,
    VariantCase,
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
pub enum Entity {
    Type(ComponentTypeSection),
    Alias(ComponentAliasSection),
    Import((Import, ComponentImportSection)),
}

#[derive(Clone, Debug)]
pub struct Graph {
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
    /// list of types that can be multiple types in locked component
    pub undetermined_types: HashMap<usize, UndeterminedType>,
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
            num_components: 0,
            num_instances: 0,
            num_interfaces: 0,
            num_aliases: 0,
            type_count: 0,
            undetermined_types: HashMap::new(),
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

#[derive(Clone, Debug)]
pub struct UndeterminedType {
  pub component: wasm_encoder::ComponentType,
  pub instance: InstanceType
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
        Self {}
    }

    pub fn parse_import_type_new(
        &self,
        parser: &ComponentTypeSectionReader,
        graph: &mut Graph,
    ) -> Result<()> {
        let cloned = parser.clone();
        let mut type_section = ComponentTypeSection::new();
        for ty in cloned.into_iter_with_offsets() {
            let mut instance = InstanceType::new();
            let mut component = wasm_encoder::ComponentType::new();
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
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Bool);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Bool);
                                                    },
                                                    wasmparser::PrimitiveValType::S8 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S8);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S8);
                                                    },
                                                    wasmparser::PrimitiveValType::U8 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U8);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U8);
                                                    },
                                                    wasmparser::PrimitiveValType::S16 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S16);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S16);
                                                    },
                                                    wasmparser::PrimitiveValType::U16 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U16);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U16);
                                                    },
                                                    wasmparser::PrimitiveValType::S32 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S32);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S32);
                                                    },
                                                    wasmparser::PrimitiveValType::U32 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U32);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U32);
                                                    },
                                                    wasmparser::PrimitiveValType::S64 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S64);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::S64);
                                                    },
                                                    wasmparser::PrimitiveValType::U64 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U64);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::U64);
                                                    },
                                                    wasmparser::PrimitiveValType::Float32 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Float32);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Float32);
                                                    },
                                                    wasmparser::PrimitiveValType::Float64 => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Float64);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Float64);
                                                    },
                                                    wasmparser::PrimitiveValType::Char => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Char);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Char);
                                                    },
                                                    wasmparser::PrimitiveValType::String => {
                                                      component
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::String);
                                                      instance
                                                        .ty()
                                                        .defined_type()
                                                        .primitive(PrimitiveValType::Char);
                                                    },
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
                                                component.ty().defined_type().record(fields.clone());
                                                instance.ty().defined_type().record(fields);
                                            }
                                            wasmparser::ComponentDefinedType::Variant(_) => todo!(),
                                            wasmparser::ComponentDefinedType::List(ty) => {
                                                match ty {
                                                    wasmparser::ComponentValType::Primitive(
                                                        prim,
                                                    ) => match prim {
                                                        wasmparser::PrimitiveValType::Bool => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Bool);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Bool);
                                                        }
                                                        wasmparser::PrimitiveValType::S8 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S8);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S8);
                                                        }
                                                        wasmparser::PrimitiveValType::U8 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U8);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U8);
                                                        }
                                                        wasmparser::PrimitiveValType::S16 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S16);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S16);
                                                        }
                                                        wasmparser::PrimitiveValType::U16 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U16);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U16);
                                                        }
                                                        wasmparser::PrimitiveValType::S32 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S32);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S32);
                                                        }
                                                        wasmparser::PrimitiveValType::U32 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U32);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U32);
                                                        }
                                                        wasmparser::PrimitiveValType::S64 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S64);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::S64);
                                                        }
                                                        wasmparser::PrimitiveValType::U64 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U64);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::U64);
                                                        }
                                                        wasmparser::PrimitiveValType::Float32 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Float32);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Float32);
                                                        }
                                                        wasmparser::PrimitiveValType::Float64 => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Float64);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Float64);
                                                        }
                                                        wasmparser::PrimitiveValType::Char => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Char);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::Char);
                                                        }
                                                        wasmparser::PrimitiveValType::String => {
                                                            component
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::String);
                                                            instance
                                                                .ty()
                                                                .defined_type()
                                                                .list(PrimitiveValType::String);
                                                        }
                                                    },
                                                    wasmparser::ComponentValType::Type(index) => {
                                                        component
                                                            .ty()
                                                            .defined_type()
                                                            .list(ComponentValType::Type(index));
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
                                                component.ty().defined_type().tuple(tuple.clone());
                                                instance.ty().defined_type().tuple(tuple);
                                            }
                                            wasmparser::ComponentDefinedType::Flags(flags) => {
                                                let mut names = Vec::new();
                                                for flag in flags.iter() {
                                                    names.push(*flag);
                                                }
                                                component.ty().defined_type().flags(names.clone());
                                                instance.ty().defined_type().flags(names);
                                            }
                                            wasmparser::ComponentDefinedType::Enum(kinds) => {
                                                let mut variants = Vec::new();
                                                for kind in kinds.iter() {
                                                    variants.push(*kind);
                                                }
                                                component.ty().defined_type().enum_type(variants.clone());
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
                                                  component
                                                  .ty()
                                                  .defined_type()
                                                  .result(Some(ok_val), Some(err_val));
                                                  instance
                                                  .ty()
                                                  .defined_type()
                                                  .result(Some(ok_val), Some(err_val));
                                                } else {
                                                  component
                                                      .ty()
                                                      .defined_type()
                                                      .result(Some(ok_val), None);
                                                  instance
                                                      .ty()
                                                      .defined_type()
                                                      .result(Some(ok_val), None);

                                                }
                                              } else {
                                                component
                                                .ty()
                                                .defined_type()
                                                .result(None, None);
                                                instance
                                                    .ty()
                                                    .defined_type()
                                                    .result(None, None);
                                              }
                                            },
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
                                                component
                                                    .ty()
                                                    .function()
                                                    .params(params.clone())
                                                    .result(result);
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
                                                component
                                                    .ty()
                                                    .function()
                                                    .params(params.clone())
                                                    .results(results.clone());
                                                instance
                                                    .ty()
                                                    .function()
                                                    .params(params)
                                                    .results(results);
                                            }
                                        };
                                    }
                                    ComponentType::Component(_) => {
                                    }
                                    ComponentType::Instance(_) => {
                                    }
                                    ComponentType::Resource { .. } => {
                                    }
                                }
                            }
                            wasmparser::InstanceTypeDeclaration::Alias(ty) => {
                                let alias = match ty {
                                    wasmparser::ComponentAlias::InstanceExport {..} => todo!(),
                                    wasmparser::ComponentAlias::CoreInstanceExport {..} => todo!(),
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
                                            index: index as u32,
                                        }
                                    }
                                };
                                component.alias(alias);
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
                                component.export(export_name, export_type);
                                instance.export(export_name, export_type);
                            }
                        }
                    }
                    graph.undetermined_types.insert(graph.type_count, UndeterminedType {component: component.clone(), instance: instance.clone()});
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
                ComponentType::Resource {..} => todo!(),
            }
        }
        Ok(())
    }

    pub fn parse_import_new(
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
                            let ty = graph.undetermined_types.get(&(i as usize - &graph.num_aliases));
                            if let Some(ty) = ty {
                              let mut type_section = ComponentTypeSection::new();
                              type_section.ty().instance(&ty.instance);
                              graph.entities.push(Entity::Type(type_section));
                            }
                            imports.import(name, ComponentTypeRef::Instance(i));
                        }
                        wasmparser::ComponentTypeRef::Component(i) => {
                            imports.import(name, ComponentTypeRef::Component(i));
                        }
                    }
                    graph.insert_instance(name.to_string(), graph.num_instances);
                    final_import = Import::new(name.to_string(), ImportKind::Interface);
                    graph.interfaces.insert(name.to_string(), graph.num_instances);
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
                            range,
                        }) => ComponentExternName::Implementation(ImplementationImport::Locked(
                            ImportMetadata {
                                name,
                                location,
                                integrity: Some(integrity),
                                range: Some(range),
                            },
                        )),
                        wasmparser::ImplementationImport::Relative(
                            wasmparser::ImportMetadata {
                                name,
                                location,
                                integrity,
                                range,
                            },
                        ) => ComponentExternName::Implementation(ImplementationImport::Locked(
                            ImportMetadata {
                                name,
                                location,
                                integrity: Some(integrity),
                                range: Some(range),
                            },
                        )),
                        wasmparser::ImplementationImport::Locked(wasmparser::ImportMetadata {
                            name,
                            location,
                            integrity,
                            range,
                        }) => ComponentExternName::Implementation(ImplementationImport::Locked(
                            ImportMetadata {
                                name,
                                location,
                                integrity: Some(integrity),
                                range: Some(range),
                            },
                        )),
                        wasmparser::ImplementationImport::Unlocked(
                            wasmparser::ImportMetadata {
                                name,
                                location,
                                integrity,
                                range,
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
                                    range: Some(range),
                                },
                            ))
                        }
                    };
                    if already_imported {
                      return Ok(())
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
                            let ty = graph.undetermined_types.get(&(i + 1));
                            if let Some(ty) = ty {
                              let mut type_section = ComponentTypeSection::new();
                              type_section.ty().component(&ty.component);
                              graph.entities.push(Entity::Type(type_section));
                            }
                            imports.import(extern_name, ComponentTypeRef::Component((i + 1) as u32));
                            graph.entities.push(Entity::Import((final_import, imports)));
                        }
                        wasmparser::ComponentTypeRef::Component(_) => {
                            let ty = graph.undetermined_types.get(&(i + 1));
                            if let Some(ty) = ty {
                              let mut type_section = ComponentTypeSection::new();
                              type_section.ty().component(&ty.component);
                              graph.entities.push(Entity::Type(type_section));
                            }
                            imports.import(extern_name, ComponentTypeRef::Component((graph.type_count) as u32));
                            graph.entities.push(Entity::Import((final_import, imports)));
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn parse_alias_new(
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
                                  let instance_name = &dep.instantiation_args[instance_index as usize];
                                  let instance = graph.components.get(instance_name);
                                  if let Some(inst) = instance {
                                    alias_section.alias(Alias::InstanceExport {
                                        instance: inst.index as u32,
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
                                instance: instance_index,
                                kind: wasm_encoder::ComponentExportKind::Value,
                                name,
                            });
                        }
                        wasmparser::ComponentExternalKind::Type => {
                                  match kind {
                                    wasmparser::ComponentExternalKind::Module => todo!(),
                                    wasmparser::ComponentExternalKind::Func => alias_section.alias(Alias::InstanceExport {
                                      instance: instance_index,
                                      kind: wasm_encoder::ComponentExportKind::Func,
                                      name,
                                    }),
                                    wasmparser::ComponentExternalKind::Value => todo!(),
                                    wasmparser::ComponentExternalKind::Type => {
                                      graph.num_aliases += 1;
                                      alias_section.alias(Alias::InstanceExport {
                                      instance: instance_index,
                                      kind: wasm_encoder::ComponentExportKind::Type,
                                      name,
                                    })},
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

    pub fn parse_new(&mut self, mut bytes: &[u8], mut graph: &mut Graph) -> Result<()> {
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
                    self.parse_import_type_new(&s, &mut graph)?;
                }
                Payload::ComponentImportSection(s) => {
                    self.parse_import_new(&s, &mut graph)?;
                }
                Payload::ComponentAliasSection(s) => {
                    self.parse_alias_new(&s, &mut graph)?;
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
        Ok(())
    }


}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
