use anyhow::Result;
use std::{collections::HashMap, fs, path::Path};
use warg_client::{
    storage::{FileSystemContentStorage, FileSystemRegistryStorage, RegistryStorage},
    Client,
};
use warg_protocol::{package::ReleaseState, registry::PackageId};
use wasm_encoder::{
    Component, ComponentImportSection, ComponentSectionId, ComponentTypeRef, RawSection,
};
use wasmparser::{
    parser::State, Chunk, ComponentExternName, ComponentImportSectionReader, ImplementationImport,
    Parser, Payload,
};

pub const MAX_WASM_MODULE_SIZE: usize = 1024 * 1024 * 1024; //= 1 GiB
/// Bundles Dependencies
pub struct Bundler<'a> {
    client: &'a Client<FileSystemRegistryStorage, FileSystemContentStorage>, // state: State,
                                                                             // max_size: u64,
                                                                             // offset: u64,
                                                                             // encoding: Encoding
}

impl<'a> Bundler<'a> {
    pub fn new(client: &'a Client<FileSystemRegistryStorage, FileSystemContentStorage>) -> Self {
        Self {
            client, // state: State::Header,
                    // max_size: u64::MAX,
                    // offset,
                    // encoding: Encoding::Module
        }
    }

    pub async fn parse_imports(
        &mut self,
        _states: &mut Vec<State>,
        parser: ComponentImportSectionReader<'a>,
        component: &mut Component,
    ) -> Result<Vec<u8>> {
        let mut imports = ComponentImportSection::new();
        let mut interfaces = HashMap::new();
        for import in parser.into_iter_with_offsets() {
            let (_, imp) = import.unwrap().clone();
            match imp.name {
                ComponentExternName::Implementation(impl_import) => match impl_import {
                    ImplementationImport::Locked(metadata)
                    | ImplementationImport::Unlocked(metadata) => {
                        let mut full_name = metadata.name.split('/');
                        let name = full_name.next();
                        if let Some(name) = name {
                            let pkg_id = PackageId::new(name)?;
                            if let Some(info) = self.client.registry().load_package(&pkg_id).await?
                            {
                                let state = &info.state.releases().last().unwrap().state;
                                if let ReleaseState::Released { content } = state {
                                    let full_digest = content.to_string();
                                    let digest = full_digest.split(':').last().unwrap();
                                    let mut content_path = String::from("/Users/interpretations/Library/Caches/warg/content/sha256/");
                                    content_path.push_str(&digest);
                                    let path = Path::new(&content_path);
                                    let bytes = fs::read(path)?;
                                    component.section(&RawSection {
                                        id: ComponentSectionId::Component.into(),
                                        data: &bytes,
                                    });
                                }
                            }
                        }
                    }
                    _ => {}
                },
                ComponentExternName::Interface(name) => {
                    interfaces.insert(name.to_string(), name.to_string());
                }
                _ => {}
            }
        }
        for (key, _) in interfaces {
            let extern_name = wasm_encoder::ComponentExternName::Interface(&key);
            imports.import(extern_name, ComponentTypeRef::Instance(0));
        }
        component.section(&imports);
        Ok(Vec::new())
    }

    pub async fn parse(&mut self, mut bytes: &'a [u8]) -> Result<Component> {
        let constant = bytes.clone();
        let mut parser = Parser::new(0);
        let mut _consumed = 0;
        let mut states: Vec<State> = Vec::new();
        let mut component = Component::new();
        let mut foo = Vec::new();
        pub const HEADER: [u8; 8] = [
            // Magic
            0x00, 0x61, 0x73, 0x6D, // Version
            0x0d, 0x00, 0x01, 0x00,
        ];
        foo.extend_from_slice(&HEADER);
        loop {
            let payload = match parser.parse(bytes, true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    payload
                }
            };
            dbg!(&payload);
            match payload {
                Payload::ComponentImportSection(s) => {
                    self.parse_imports(&mut states, s, &mut component).await?;
                }
                Payload::ModuleSection { parser, range } => {
                    let offset = range.end - range.start;
                    component.section(&RawSection {
                        id: 1,
                        data: &constant[range],
                    });
                    // }
                    if offset > bytes.len() {
                        panic!();
                    }
                    bytes = &bytes[offset..];
                }
                Payload::End(_) => {
                    break;
                }
                _ => {
                    if let Some((id, range)) = payload.as_section() {
                        component.section(&RawSection {
                            id,
                            data: &constant[range],
                        });
                    }
                }
            }
        }
        Ok(component)
    }
}
