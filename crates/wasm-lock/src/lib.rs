use std::collections::HashMap;

use anyhow::{bail, Result};
use wasm_encoder::{
    Component, ComponentTypeSection, ComponentValType, InstanceType, PrimitiveValType,
    ComponentImportSection, ComponentExternName, ImplementationImport, ImportMetadata, ComponentTypeRef, ComponentInstanceSection, ComponentExportKind, ComponentExportSection,
};
use wasmparser::{
    Chunk, ComponentImportSectionReader, ComponentType,
    ComponentTypeSectionReader, Encoding, Parser, Payload, SubType
};

pub struct Lock<'a> {
    name: &'a str,
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

impl<'a> Lock<'a> {
    pub fn new() -> Self {
        Self { name: "foo" }
    }
    pub fn parse_type_imports(
        &mut self,
        _states: &mut Vec<State>,
        parser: ComponentTypeSectionReader<'a>,
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
                                    ComponentType::Defined(_) => todo!(),
                                    ComponentType::Func(func) => {
                                        let params =
                                            func.params.iter().map(|(name, ty)| match ty {
                                                wasmparser::ComponentValType::Primitive(prim) => {
                                                    match prim {
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
                                                    }
                                                }
                                                wasmparser::ComponentValType::Type(_) => todo!(),
                                            });
                                        let result = match func.results {
                                            wasmparser::ComponentFuncResult::Unnamed(ty) => {
                                                match ty {
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
                                                    wasmparser::ComponentValType::Type(_) => {
                                                        todo!()
                                                    }
                                                }
                                            }
                                            wasmparser::ComponentFuncResult::Named(_) => todo!(),
                                        };
                                        instance.ty().function().params(params).result(result);
                                        type_section.instance(&instance);
                                    }
                                    ComponentType::Component(_) => {}
                                    ComponentType::Instance(_) => todo!(),
                                    ComponentType::Resource { rep, dtor } => todo!(),
                                }
                            }
                            wasmparser::InstanceTypeDeclaration::Alias(_) => todo!(),
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

    pub fn parse_imports(
        &mut self,
        _states: &mut Vec<State>,
        parser: ComponentImportSectionReader<'a>,
        component: &mut Component,
        imports: &mut ComponentImportSection,
        instances: &mut ComponentInstanceSection
    ) -> Result<()> {
        let mut args: Vec<(&str, ComponentExportKind, u32)> = Vec::new();
        for (i, import) in parser.into_iter_with_offsets().enumerate() {
            let (_, imp) = import.unwrap().clone();
            if let wasmparser::ComponentExternName::Implementation(name) = imp.name {
              let import = ComponentExternName::Implementation(
                ImplementationImport::Locked(ImportMetadata {
                  name: name.as_str(),
                  location: "",
                  integrity: Some("asldkjf"),
                  range: Some("1.0.0")
                }));
                let ty = ComponentTypeRef::Component(0);

              imports.import(import, ty);
              let temp_args: Vec<(&str, ComponentExportKind, u32)> = Vec::new();
              instances.instantiate(i as u32 + 1, temp_args);
              args.push((name.as_str(), ComponentExportKind::Instance, i as u32));
            }
            }
        instances.instantiate(0, args);
        component.section(imports);
        let num_instances = instances.len();
        let mut exports = ComponentExportSection::new();
        let export = ComponentExternName::Kebab("bundled");
        exports.export(export, ComponentExportKind::Instance, num_instances - 1, None);
        component.section(instances);
        component.section(&exports);
        
        Ok(())
    }
    pub fn parse(&mut self, mut bytes: &'a [u8], mut component: &'a mut Component, mut imports: &mut ComponentImportSection, mut instances: &mut ComponentInstanceSection) -> Result<&'a mut Component> {
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
                    self.parse_imports(&mut states, s, &mut component, &mut imports, &mut instances)?;
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
        Ok(component)
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
