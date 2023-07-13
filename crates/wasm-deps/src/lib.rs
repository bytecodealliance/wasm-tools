use std::{collections::HashMap, marker::PhantomData};

use wasmparser::{Encoding, Parser, Payload, SubType, ComponentTypeSectionReader, ComponentType, ComponentTypeDeclaration, Chunk, ComponentExternName, ComponentImportSectionReader, ComponentImport};
use anyhow::{Result};

pub struct DepsParser<'a> {
  name: &'a str
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

impl <'a> DepsParser<'a> {
    pub fn new() -> Self {
      Self {
        name: "foo"
      }
    }
    pub fn parse_type_imports(
      &mut self,
      _states: &mut Vec<State>,
      parser: ComponentTypeSectionReader,
      deps: &mut Vec<String>
    ) -> Result<()>{
      for ty in parser.into_iter_with_offsets() {
          let (_offset, ty) = ty?;
          match ty {
            ComponentType::Component(decls) => {
              dbg!("COMPONENT TYPE");
              for decl in decls.into_vec() {
                match decl {
                  ComponentTypeDeclaration::Type(ty) => {
                    match ty {
                      ComponentType::Component(cdecls) => {
                        for cdecl in cdecls.into_vec() {
                          dbg!(&cdecl);
                          match cdecl {
                            ComponentTypeDeclaration::Import(import) => {
                              let name = import.name.as_str().to_owned();
                              deps.push(name);
                            }
                            _ => {
                              dbg!("OTHER");
                            }
                          }
                        }
                      }
                      ComponentType::Instance(idecls) => {
                        for idecl in idecls.into_vec() {
                          dbg!(idecl);
                        }
                      }
                      _ => {}
                    }
                  }
                  ComponentTypeDeclaration::Import(import) => {
                  }
                  _ => {}
                }
              }
            }
            _ => {}
          }
      }
      Ok(())
    }
    pub fn parse_imports(
      &mut self,
      _states: &mut Vec<State>,
      parser: ComponentImportSectionReader<'a>,
      deps: &mut Vec<ComponentImport<'a>>
    )
     -> Result<Vec<ComponentImport<'a>>>
      {
      for import in parser.into_iter_with_offsets() {
        let (_, imp) = import.unwrap().clone();
        deps.push(imp);
      }
      Ok(deps.to_vec())
    }
    pub fn parse(&mut self, mut bytes: &'a [u8]) -> Result<Vec<ComponentImport<'a>>> {
        let mut parser = Parser::new(0);
        let mut _consumed = 0;
        let mut states: Vec<State> = Vec::new();
        let mut deps = Vec::new();
        loop {
            let payload = match parser.parse(bytes, true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    payload
                }
            };
            // consumed += size;
            match payload {
                // Payload::ComponentTypeSection(s) => {
                //   self.parse_type_imports(&mut states, s, &mut deps);
                //   // dbg!(deps);
                // }
                Payload::ComponentImportSection(s) => {
                  let parsed = self.parse_imports(&mut states, s, &mut deps);
                }
                Payload::CodeSectionStart{count: _, range: _, size: _} => {
                  parser.skip_section();
                }
                Payload::End(_) => break,
                _ => {}
            }
        }
        Ok(deps)
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}