use crate::{
    module::{PrimitiveTypeInfo, TypeInfo},
    Result,
};
use std::collections::HashSet;
use std::convert::TryFrom;
use std::ops::Range;
use wasm_encoder::{RawSection, SectionId};
use wasmparser::{Chunk, Parser, Payload};

/// Provides module information for future usage during mutation
/// an instance of ModuleInfo could be user to determine which mutation could be applied
#[derive(Default, Clone, Debug)]
pub struct ModuleInfo<'a> {
    // The following fields are offsets inside the `raw_sections` field.
    // The main idea is to maintain the order of the sections in the input Wasm.
    pub exports: Option<usize>,
    pub export_names: HashSet<String>,

    // Indices of various sections within `self.raw_sections`.
    pub types: Option<usize>,
    pub imports: Option<usize>,
    pub tables: Option<usize>,
    pub memories: Option<usize>,
    pub globals: Option<usize>,
    pub elements: Option<usize>,
    pub functions: Option<usize>,
    pub data_count: Option<usize>,
    pub data: Option<usize>,
    pub code: Option<usize>,
    pub start: Option<usize>,

    pub exports_count: u32,
    elements_count: u32,
    data_segments_count: u32,
    start_function: Option<u32>,
    memory_count: u32,
    table_count: u32,
    tag_count: u32,

    imported_functions_count: u32,
    imported_globals_count: u32,
    imported_memories_count: u32,
    imported_tables_count: u32,
    imported_tags_count: u32,

    // types for inner functions
    pub types_map: Vec<TypeInfo>,

    // function idx to type idx
    pub function_map: Vec<u32>,
    pub global_types: Vec<PrimitiveTypeInfo>,
    pub table_elem_types: Vec<PrimitiveTypeInfo>,
    pub memory_types: Vec<wasmparser::MemoryType>,

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
                    count: _,
                    range,
                    size: _,
                } => {
                    info.code = Some(info.raw_sections.len());
                    info.section(SectionId::Code.into(), range.clone(), input_wasm);
                    parser.skip_section();
                    // update slice, bypass the section
                    wasm = &input_wasm[range.end..];

                    continue;
                }
                Payload::TypeSection(reader) => {
                    info.types = Some(info.raw_sections.len());
                    info.section(SectionId::Type.into(), reader.range(), input_wasm);

                    // Save function types
                    for ty in reader.into_iter_err_on_gc_types() {
                        info.types_map.push(ty?.into());
                    }
                }
                Payload::ImportSection(reader) => {
                    info.imports = Some(info.raw_sections.len());
                    info.section(SectionId::Import.into(), reader.range(), input_wasm);

                    for ty in reader {
                        match ty?.ty {
                            wasmparser::TypeRef::Func(ty) => {
                                // Save imported functions
                                info.function_map.push(ty);
                                info.imported_functions_count += 1;
                            }
                            wasmparser::TypeRef::Global(ty) => {
                                let ty = PrimitiveTypeInfo::try_from(ty.content_type).unwrap();
                                info.global_types.push(ty);
                                info.imported_globals_count += 1;
                            }
                            wasmparser::TypeRef::Memory(ty) => {
                                info.memory_count += 1;
                                info.imported_memories_count += 1;
                                info.memory_types.push(ty);
                            }
                            wasmparser::TypeRef::Table(ty) => {
                                info.table_count += 1;
                                info.imported_tables_count += 1;
                                info.table_elem_types.push(ty.element_type.into());
                            }
                            wasmparser::TypeRef::Tag(_ty) => {
                                info.tag_count += 1;
                                info.imported_tags_count += 1;
                            }
                        }
                    }
                }
                Payload::FunctionSection(reader) => {
                    info.functions = Some(info.raw_sections.len());
                    info.section(SectionId::Function.into(), reader.range(), input_wasm);

                    for ty in reader {
                        info.function_map.push(ty?);
                    }
                }
                Payload::TableSection(reader) => {
                    info.tables = Some(info.raw_sections.len());
                    info.table_count += reader.count();
                    info.section(SectionId::Table.into(), reader.range(), input_wasm);

                    for table in reader {
                        let table = table?;
                        let ty = PrimitiveTypeInfo::try_from(table.ty.element_type).unwrap();
                        info.table_elem_types.push(ty);
                    }
                }
                Payload::MemorySection(reader) => {
                    info.memories = Some(info.raw_sections.len());
                    info.memory_count += reader.count();
                    info.section(SectionId::Memory.into(), reader.range(), input_wasm);

                    for ty in reader {
                        info.memory_types.push(ty?);
                    }
                }
                Payload::GlobalSection(reader) => {
                    info.globals = Some(info.raw_sections.len());
                    info.section(SectionId::Global.into(), reader.range(), input_wasm);

                    for ty in reader {
                        let ty = ty?;
                        // We only need the type of the global, not necessarily if is mutable or not
                        let ty = PrimitiveTypeInfo::try_from(ty.ty.content_type).unwrap();
                        info.global_types.push(ty);
                    }
                }
                Payload::ExportSection(reader) => {
                    info.exports = Some(info.raw_sections.len());
                    info.exports_count = reader.count();

                    for entry in reader.clone() {
                        info.export_names.insert(entry?.name.into());
                    }

                    info.section(SectionId::Export.into(), reader.range(), input_wasm);
                }
                Payload::StartSection { func, range } => {
                    info.start = Some(info.raw_sections.len());
                    info.start_function = Some(func);
                    info.section(SectionId::Start.into(), range, input_wasm);
                }
                Payload::ElementSection(reader) => {
                    info.elements = Some(info.raw_sections.len());
                    info.elements_count = reader.count();
                    info.section(SectionId::Element.into(), reader.range(), input_wasm);
                }
                Payload::DataSection(reader) => {
                    info.data = Some(info.raw_sections.len());
                    info.data_segments_count = reader.count();
                    info.section(SectionId::Data.into(), reader.range(), input_wasm);
                }
                Payload::CustomSection(c) => {
                    info.section(SectionId::Custom.into(), c.range(), input_wasm);
                }
                Payload::UnknownSection {
                    id,
                    contents: _,
                    range,
                } => {
                    info.section(id, range, input_wasm);
                }
                Payload::DataCountSection { count: _, range } => {
                    info.data_count = Some(info.raw_sections.len());
                    info.section(SectionId::DataCount.into(), range, input_wasm);
                }
                Payload::Version { .. } => {}
                Payload::End(_) => {
                    break;
                }
                _ => todo!("{:?} not implemented", payload),
            }
            wasm = &wasm[consumed..];
        }

        Ok(info)
    }

    pub fn has_nonempty_code(&self) -> bool {
        if let Some(section) = self.code {
            let section_data = self.raw_sections[section].data;
            wasmparser::CodeSectionReader::new(section_data, 0)
                .map(|r| r.count() != 0)
                .unwrap_or(false)
        } else {
            false
        }
    }

    pub fn has_code(&self) -> bool {
        self.code != None
    }

    /// Does this module have any custom sections?
    pub fn has_custom_section(&self) -> bool {
        self.raw_sections
            .iter()
            .any(|s| s.id == SectionId::Custom as u8)
    }

    /// Registers a new raw_section in the ModuleInfo
    pub fn section(&mut self, id: u8, range: Range<usize>, full_wasm: &'a [u8]) {
        self.raw_sections.push(RawSection {
            id,
            data: &full_wasm[range],
        });
    }

    pub fn get_type_section(&self) -> Option<RawSection<'a>> {
        let idx = self.types?;
        Some(self.raw_sections[idx])
    }

    pub fn get_code_section(&self) -> RawSection<'a> {
        self.raw_sections[self.code.unwrap()]
    }

    pub fn get_exports_section(&self) -> RawSection<'a> {
        self.raw_sections[self.exports.unwrap()]
    }

    pub fn get_data_section(&self) -> RawSection<'a> {
        self.raw_sections[self.data.unwrap()]
    }

    pub fn has_exports(&self) -> bool {
        self.exports != None
    }

    /// Returns the function type based on the index of the function type
    /// `types[functions[idx]]`
    pub fn get_functype_idx(&self, idx: u32) -> &TypeInfo {
        let functpeindex = self.function_map[idx as usize] as usize;
        &self.types_map[functpeindex]
    }

    /// Returns the number of globals used by the Wasm binary including imported
    /// glboals
    pub fn get_global_count(&self) -> usize {
        self.global_types.len()
    }

    /// Returns the global section bytes as a `RawSection` instance
    pub fn get_global_section(&self) -> RawSection {
        self.raw_sections[self.globals.unwrap()]
    }

    /// Insert a new section as the `i`th section in the Wasm module.
    pub fn insert_section(
        &self,
        i: usize,
        new_section: &impl wasm_encoder::Section,
    ) -> wasm_encoder::Module {
        log::trace!("inserting new section at {}", i);
        let mut module = wasm_encoder::Module::new();
        self.raw_sections.iter().enumerate().for_each(|(j, s)| {
            if i == j {
                module.section(new_section);
            }
            module.section(s);
        });
        if self.raw_sections.len() == i {
            module.section(new_section);
        }
        module
    }

    /// Move a section from index `src_idx` to `dest_idx` in the Wasm module
    pub fn move_section(&self, src_idx: usize, dest_idx: usize) -> wasm_encoder::Module {
        log::trace!(
            "moving section from index {} to index {}",
            src_idx,
            dest_idx
        );
        assert!(src_idx < self.raw_sections.len());
        assert!(dest_idx < self.raw_sections.len());
        assert_ne!(src_idx, dest_idx);
        let mut module = wasm_encoder::Module::new();
        self.raw_sections.iter().enumerate().for_each(|(i, s)| {
            if src_idx < dest_idx && i == dest_idx {
                module.section(&self.raw_sections[src_idx]);
            }
            if i != src_idx {
                module.section(s);
            }
            if dest_idx < src_idx && i == dest_idx {
                module.section(&self.raw_sections[src_idx]);
            }
        });
        module
    }

    /// Replace the `i`th section in this module with the given new section.
    pub fn replace_section(
        &self,
        i: usize,
        new_section: &impl wasm_encoder::Section,
    ) -> wasm_encoder::Module {
        log::trace!("replacing section {}", i);
        let mut module = wasm_encoder::Module::new();
        for (j, s) in self.raw_sections.iter().enumerate() {
            if i == j {
                module.section(new_section);
            } else {
                module.section(s);
            }
        }
        module
    }

    /// Replaces raw sections in the passed indexes and return a new module
    ///
    /// This method will be helpful to add more than one custom section. For
    /// example, some code mutations might need to add a few globals. This
    /// method can be used to write a new or custom global section before the
    /// code section.
    /// * `section_writer` this callback should write the custom section and
    ///   returns true if it was successful, if false is returned then the
    ///   default section will be written to the module
    pub fn replace_multiple_sections<P>(&self, mut section_writer: P) -> wasm_encoder::Module
    where
        P: FnMut(usize, u8, &mut wasm_encoder::Module) -> bool,
    {
        let mut module = wasm_encoder::Module::new();
        self.raw_sections.iter().enumerate().for_each(|(j, s)| {
            // Write if the section_writer did not write a custom section
            if !section_writer(j, s.id, &mut module) {
                module.section(s);
            }
        });
        module
    }

    pub fn num_functions(&self) -> u32 {
        self.function_map.len() as u32
    }

    pub fn num_local_functions(&self) -> u32 {
        self.num_functions() - self.num_imported_functions()
    }

    pub fn num_imported_functions(&self) -> u32 {
        self.imported_functions_count
    }

    pub fn num_tables(&self) -> u32 {
        self.table_count
    }

    pub fn num_imported_tables(&self) -> u32 {
        self.imported_tables_count
    }

    pub fn num_memories(&self) -> u32 {
        self.memory_count
    }

    pub fn num_imported_memories(&self) -> u32 {
        self.imported_memories_count
    }

    pub fn num_globals(&self) -> u32 {
        self.global_types.len() as u32
    }

    pub fn num_imported_globals(&self) -> u32 {
        self.imported_globals_count
    }

    pub fn num_local_globals(&self) -> u32 {
        self.global_types.len() as u32 - self.imported_globals_count
    }

    pub fn num_tags(&self) -> u32 {
        self.tag_count
    }

    pub fn num_imported_tags(&self) -> u32 {
        self.imported_tags_count
    }

    pub fn num_data(&self) -> u32 {
        self.data_segments_count
    }

    pub fn num_elements(&self) -> u32 {
        self.elements_count
    }

    pub fn num_types(&self) -> u32 {
        self.types_map.len() as u32
    }
}
