use crate::{
    module::{PrimitiveTypeInfo, TypeInfo},
    Result,
};
use std::collections::HashSet;
use std::convert::TryFrom;
use wasm_encoder::{RawSection, SectionId};
use wasmparser::{Chunk, Parser, Payload, SectionReader};

/// Provides module information for future usage during mutation
/// an instance of ModuleInfo could be user to determine which mutation could be applied
#[derive(Default, Clone)]
pub struct ModuleInfo<'a> {
    // The following fields are offsets inside the `raw_sections` field.
    // The main idea is to maintain the order of the sections in the input Wasm.
    pub exports: Option<usize>,
    pub export_names: HashSet<String>,

    pub types: Option<usize>,
    pub imports: Option<usize>,
    pub tables: Option<usize>,
    pub memories: Option<usize>,
    pub globals: Option<usize>,
    pub elements: Option<usize>,
    pub functions: Option<usize>,
    pub data: Option<usize>,
    pub code: Option<usize>,

    pub is_start_defined: bool,
    pub function_count: u32,
    pub exports_count: u32,
    pub imported_functions_count: u32,

    // types for inner functions
    pub types_map: Vec<TypeInfo>,

    // function idx to type idx
    pub function_map: Vec<u32>,
    pub global_types: Vec<PrimitiveTypeInfo>,

    // raw_sections
    pub raw_sections: Vec<RawSection<'a>>,
    pub input_wasm: &'a [u8],
}

impl<'a> ModuleInfo<'a> {
    /// Parse the given Wasm bytes and fill out a `ModuleInfo` AST for it.
    pub fn new(input_wasm: &[u8]) -> Result<ModuleInfo> {
        let mut parser = Parser::new(0);
        let mut info = ModuleInfo::default();
        let mut wasm = input_wasm;
        info.input_wasm = wasm;

        loop {
            let (payload, consumed) = match parser.parse(wasm, true)? {
                Chunk::NeedMoreData(hint) => {
                    panic!("Invalid Wasm module {:?}", hint);
                }
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            match payload {
                Payload::CodeSectionStart {
                    count,
                    range,
                    size: _,
                } => {
                    info.code = Some(info.raw_sections.len());
                    info.function_count = count;
                    info.section(SectionId::Code.into(), range, input_wasm);
                    parser.skip_section();
                    // update slice, bypass the section
                    wasm = &input_wasm[range.end..];

                    continue;
                }
                Payload::TypeSection(mut reader) => {
                    info.types = Some(info.raw_sections.len());
                    info.section(SectionId::Type.into(), reader.range(), input_wasm);

                    // Save function types
                    for _ in 0..reader.get_count() {
                        reader.read().map(|ty| {
                            let typeinfo = TypeInfo::try_from(ty).unwrap();
                            info.types_map.push(typeinfo);
                        })?;
                    }
                }
                Payload::ImportSection(mut reader) => {
                    info.imports = Some(info.raw_sections.len());
                    info.section(SectionId::Import.into(), reader.range(), input_wasm);

                    for _ in 0..reader.get_count() {
                        reader.read().map(|ty| {
                            match ty.ty {
                                wasmparser::ImportSectionEntryType::Function(ty) => {
                                    // Save imported functions
                                    info.function_map.push(ty);
                                    info.imported_functions_count += 1;
                                }
                                wasmparser::ImportSectionEntryType::Global(ty) => {
                                    let ty = PrimitiveTypeInfo::try_from(ty.content_type).unwrap();
                                    info.global_types.push(ty);
                                }
                                _ => {
                                    // Do nothing
                                }
                            }
                        })?;
                    }
                }
                Payload::FunctionSection(mut reader) => {
                    info.functions = Some(info.raw_sections.len());
                    info.section(SectionId::Function.into(), reader.range(), input_wasm);

                    for _ in 0..reader.get_count() {
                        reader.read().map(|ty| {
                            info.function_map.push(ty);
                        })?;
                    }
                }
                Payload::TableSection(reader) => {
                    info.tables = Some(info.raw_sections.len());
                    info.section(SectionId::Table.into(), reader.range(), input_wasm);
                }
                Payload::MemorySection(reader) => {
                    info.memories = Some(info.raw_sections.len());
                    info.section(SectionId::Memory.into(), reader.range(), input_wasm);
                }
                Payload::GlobalSection(mut reader) => {
                    info.globals = Some(info.raw_sections.len());
                    info.section(SectionId::Global.into(), reader.range(), input_wasm);

                    for _ in 0..reader.get_count() {
                        reader.read().map(|ty| {
                            // We only need the type of the global, not necesarily if is mutable or not
                            let ty = PrimitiveTypeInfo::try_from(ty.ty.content_type).unwrap();
                            info.global_types.push(ty);
                        })?;
                    }
                }
                Payload::ExportSection(mut reader) => {
                    info.exports = Some(info.raw_sections.len());
                    info.exports_count = reader.get_count();

                    for _ in 0..reader.get_count() {
                        let entry = reader.read()?;
                        info.export_names.insert(entry.field.into());
                    }

                    info.section(SectionId::Export.into(), reader.range(), input_wasm);
                }
                Payload::StartSection { func: _, range: _ } => {
                    info.is_start_defined = true;
                }
                Payload::ElementSection(reader) => {
                    info.elements = Some(info.raw_sections.len());
                    info.section(SectionId::Element.into(), reader.range(), input_wasm);
                }
                Payload::DataSection(reader) => {
                    info.data = Some(info.raw_sections.len());
                    info.section(SectionId::Data.into(), reader.range(), input_wasm);
                }
                Payload::CustomSection {
                    name: _,
                    data_offset: _,
                    data: _,
                    range,
                } => {
                    info.section(SectionId::Custom.into(), range, input_wasm);
                }
                Payload::AliasSection(reader) => {
                    info.section(SectionId::Alias.into(), reader.range(), input_wasm);
                }
                Payload::UnknownSection {
                    id,
                    contents: _,
                    range,
                } => {
                    info.section(id, range, input_wasm);
                }
                Payload::DataCountSection { count: _, range } => {
                    info.section(SectionId::DataCount.into(), range, input_wasm);
                }
                Payload::Version { .. } => {}
                Payload::End => {
                    break;
                }
                _ => todo!("{:?} not implemented", payload),
            }
            wasm = &wasm[consumed..];
        }

        Ok(info)
    }

    pub fn has_code(&self) -> bool {
        self.code != None
    }

    /// Registers a new raw_section in the ModuleInfo
    pub fn section(&mut self, id: u8, range: wasmparser::Range, full_wasm: &'a [u8]) {
        self.raw_sections.push(RawSection {
            id,
            data: &full_wasm[range.start..range.end],
        });
    }

    pub fn get_code_section(&self) -> RawSection {
        self.raw_sections[self.code.unwrap()]
    }

    pub fn get_exports_section(&self) -> &RawSection {
        &self.raw_sections[self.exports.unwrap()]
    }

    pub fn has_exports(&self) -> bool {
        self.exports != None
    }

    /// Returns the type information based on its index `types[index]`
    pub fn get_type_idx(&self, idx: usize) -> &TypeInfo {
        &self.types_map[idx]
    }

    /// Returns the function type based on the index of the function type
    /// `types[functions[idx]]`
    pub fn get_functype_idx(&self, idx: usize) -> &TypeInfo {
        let functpeindex = self.function_map[idx] as usize;
        &self.types_map[functpeindex]
    }

    pub fn replace_section(
        &self,
        i: usize,
        new_section: &impl wasm_encoder::Section,
    ) -> wasm_encoder::Module {
        let mut module = wasm_encoder::Module::new();
        self.raw_sections.iter().enumerate().for_each(|(j, s)| {
            if i == j {
                module.section(new_section);
            } else {
                module.section(s);
            }
        });
        module
    }
}
