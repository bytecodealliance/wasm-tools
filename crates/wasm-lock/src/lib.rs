use std::collections::HashMap;

use anyhow::{bail, Result};
use wasm_encoder::{
    Alias,
    Component, ComponentTypeSection, ComponentValType, InstanceType, PrimitiveValType,
    ComponentImportSection, ComponentExternName, ImplementationImport, ImportMetadata, ComponentTypeRef, ComponentInstanceSection, ComponentExportKind, ComponentExportSection, ComponentOuterAliasKind,
};
use wasmparser::{
    Chunk, ComponentImportSectionReader, ComponentType,
    ComponentTypeSectionReader, Encoding, Parser, Payload, SubType
};

pub struct Lock {
}

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

#[derive(Debug)]
struct Naming {
    _identifier: Option<String>,
    _name: String,
}

impl Lock {
    pub fn new(
    ) -> Self {
        Self {
        }
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
                                      dbg!(&def);

                                      match def {
                                        wasmparser::ComponentDefinedType::Primitive(prim) => {
                                          match prim {
                                            wasmparser::PrimitiveValType::Bool => instance.ty().defined_type().primitive(PrimitiveValType::Bool),
                                            wasmparser::PrimitiveValType::S8 => instance.ty().defined_type().primitive(PrimitiveValType::S8),
                                            wasmparser::PrimitiveValType::U8 => instance.ty().defined_type().primitive(PrimitiveValType::U8),
                                            wasmparser::PrimitiveValType::S16 => instance.ty().defined_type().primitive(PrimitiveValType::S16),
                                            wasmparser::PrimitiveValType::U16 => instance.ty().defined_type().primitive(PrimitiveValType::U16),
                                            wasmparser::PrimitiveValType::S32 => instance.ty().defined_type().primitive(PrimitiveValType::S32),
                                            wasmparser::PrimitiveValType::U32 => instance.ty().defined_type().primitive(PrimitiveValType::U32),
                                            wasmparser::PrimitiveValType::S64 => instance.ty().defined_type().primitive(PrimitiveValType::S64),
                                            wasmparser::PrimitiveValType::U64 => instance.ty().defined_type().primitive(PrimitiveValType::U64),
                                            wasmparser::PrimitiveValType::Float32 => todo!(),
                                            wasmparser::PrimitiveValType::Float64 => todo!(),
                                            wasmparser::PrimitiveValType::Char => todo!(),
                                            wasmparser::PrimitiveValType::String => todo!(),
                                          }
                                        },
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
                                        },
                                        wasmparser::ComponentDefinedType::Variant(_) => todo!(),
                                        wasmparser::ComponentDefinedType::List(ty) => {
                                          match ty {
                                            wasmparser::ComponentValType::Primitive(prim) => {
                                              match prim {
                                                wasmparser::PrimitiveValType::Bool => instance.ty().defined_type().primitive(PrimitiveValType::Bool),
                                                wasmparser::PrimitiveValType::S8 => instance.ty().defined_type().primitive(PrimitiveValType::S8),
                                                wasmparser::PrimitiveValType::U8 => instance.ty().defined_type().primitive(PrimitiveValType::U8),
                                                wasmparser::PrimitiveValType::S16 => instance.ty().defined_type().primitive(PrimitiveValType::S16),
                                                wasmparser::PrimitiveValType::U16 => instance.ty().defined_type().primitive(PrimitiveValType::U16),
                                                wasmparser::PrimitiveValType::S32 => instance.ty().defined_type().primitive(PrimitiveValType::S32),
                                                wasmparser::PrimitiveValType::U32 => instance.ty().defined_type().primitive(PrimitiveValType::U32),
                                                wasmparser::PrimitiveValType::S64 => instance.ty().defined_type().primitive(PrimitiveValType::S64),
                                                wasmparser::PrimitiveValType::U64 => instance.ty().defined_type().primitive(PrimitiveValType::U64),
                                                wasmparser::PrimitiveValType::Float32 => todo!(),
                                                wasmparser::PrimitiveValType::Float64 => todo!(),
                                                wasmparser::PrimitiveValType::Char => todo!(),
                                                wasmparser::PrimitiveValType::String => todo!(),
                                              }
                                            },
                                            wasmparser::ComponentValType::Type(index) => {
                                              instance.ty().defined_type().list(ComponentValType::Type(index));
                                            },
                                        }
                                        },
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
                                        },
                                        wasmparser::ComponentDefinedType::Flags(flags) => {
                                          let mut names = Vec::new();
                                          for flag in flags.iter() {
                                            names.push(*flag);
                                          }
                                          instance.ty().defined_type().flags(names);
                                        },
                                        wasmparser::ComponentDefinedType::Enum(kinds) => {
                                          let mut variants = Vec::new();
                                          for kind in kinds.iter() {
                                            variants.push(*kind);
                                          }
                                          instance.ty().defined_type().enum_type(variants);
                                        },
                                        wasmparser::ComponentDefinedType::Union(_) => todo!(),
                                        wasmparser::ComponentDefinedType::Option(_) => todo!(),
                                        wasmparser::ComponentDefinedType::Result { ok, err } => {
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
                                              instance.ty().defined_type().result(Some(ok_val), Some(err_val));
                                            }
                                          }
                                        },
                                        wasmparser::ComponentDefinedType::Own(_) => todo!(),
                                        wasmparser::ComponentDefinedType::Borrow(_) => todo!(),
                                      };
                                    },
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
                                                wasmparser::ComponentValType::Type(index) => (name.to_owned(), ComponentValType::Type(*index)),
                                            });
                                        match func.results {
                                            wasmparser::ComponentFuncResult::Unnamed(ty) => {
                                                let result = match ty {
                                                    wasmparser::ComponentValType::Primitive(
                                                        prim,
                                                    ) => match prim {
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
                                                    wasmparser::ComponentValType::Type(index) => ComponentValType::Type(index)
                                                };
                                                instance.ty().function().params(params).result(result);
                                                type_section.instance(&instance);
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
                                              instance.ty().function().params(params).results(results);
                                              type_section.instance(&instance);
                                            }
                                        };
                                    }
                                    ComponentType::Component(_) => {}
                                    ComponentType::Instance(_) => todo!(),
                                    ComponentType::Resource { rep, dtor } => todo!(),
                                }
                            }
                            wasmparser::InstanceTypeDeclaration::Alias(ty) => {
                              let alias = match ty {
                                wasmparser::ComponentAlias::InstanceExport { kind, instance_index, name } => todo!(),
                                wasmparser::ComponentAlias::CoreInstanceExport { kind, instance_index, name } => todo!(),
                                wasmparser::ComponentAlias::Outer { kind, count, index } => Alias::Outer {
                                    kind: match kind {
                                        wasmparser::ComponentOuterAliasKind::CoreModule => ComponentOuterAliasKind::CoreModule,
                                        wasmparser::ComponentOuterAliasKind::CoreType => ComponentOuterAliasKind::CoreType,
                                        wasmparser::ComponentOuterAliasKind::Type => ComponentOuterAliasKind::Type,
                                        wasmparser::ComponentOuterAliasKind::Component => ComponentOuterAliasKind::Component,
                                    },
                                    count,
                                    index
                                  }
                                };
                              instance.alias(alias);
                            },
                            wasmparser::InstanceTypeDeclaration::Export { name, ty } => {}
                        }
                    }
                }
                _ => {} 
            }

        component.section(&type_section);            
        }

        Ok(())
    }

    pub fn parse_imports<'b>(
        &'b mut self,
        _states: &mut Vec<State>,
        section: &ComponentImportSectionReader,
        component: &mut Component,
        imports: &mut ComponentImportSection,
        instances: &mut ComponentInstanceSection,
        packages: &'b mut Vec<String>
    ) -> Result<&mut Vec<String>> {
        dbg!("PARSE IMPORTS");
        let mut args: Vec<(&str, ComponentExportKind, u32)> = Vec::new();
        let clone = section.clone();
        for (i, import) in clone.into_iter_with_offsets().enumerate() {
            let (_, imp) = import.unwrap().clone();
            dbg!(&imp);
            match imp.name {
                wasmparser::ComponentExternName::Kebab(_) => {},
                wasmparser::ComponentExternName::Interface(name) => {

                },
                wasmparser::ComponentExternName::Implementation(name) => {
                  packages.push(name.as_str().to_string());
                },
            }
            // if let wasmparser::ComponentExternName::Implementation(name) = imp.name {
            //   packages.push(name.as_str().to_string());
            //   // let import = ComponentExternName::Implementation(
            //   //   ImplementationImport::Locked(ImportMetadata {
            //   //     name: name.as_str(),
            //   //     location: "",
            //   //     integrity: Some("asldkjf"),
            //   //     range: Some("1.0.0")
            //   //   }));
            //   //   let ty = ComponentTypeRef::Component(0);
            //   // let temp_args: Vec<(&str, ComponentExportKind, u32)> = Vec::new();
            //   // instances.instantiate(i as u32 + 1, temp_args);
            //   // args.push((name.as_str(), ComponentExportKind::Instance, i as u32));
              
            //   // self.parse(_states, &section, component, imports);
            // }
        }
        // instances.instantiate(0, args);
        // component.section(imports);
        let num_instances = instances.len();
        let mut exports = ComponentExportSection::new();
        let export = ComponentExternName::Kebab("bundled");
        // exports.export(export, ComponentExportKind::Instance, num_instances - 1, None);
        // component.section(instances);
        // component.section(&exports);
        dbg!(&packages);
        Ok(packages)
    }

    pub fn parse<'b>(&'b mut self, mut bytes: &[u8], mut component: &mut Component, mut imports: &mut ComponentImportSection, mut instances: &mut ComponentInstanceSection, packages: &'b mut Vec<String>) -> Result<&mut Vec<String>> {
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
                    self.parse_imports(&mut states, &s, &mut component, &mut imports, &mut instances, packages)?;
                }
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
        Ok(packages)
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
