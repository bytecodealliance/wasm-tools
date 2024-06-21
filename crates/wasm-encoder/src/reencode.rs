#![allow(missing_docs)] // FIXME

#[derive(Debug)]
pub enum Error {
    /// There was a type reference that was canonicalized and no longer
    /// references an index into a module's types space, so we cannot encode it
    /// into a Wasm binary again.
    CanonicalizedHeapTypeReference,
    /// The const expression is invalid: not actually constant or something like
    /// that.
    InvalidConstExpr,
    /// There was an error when parsing.
    ParseError(wasmparser::BinaryReaderError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::ParseError(_e) => {
                write!(fmt, "There was an error when parsing")
            }
            Self::InvalidConstExpr => write!(fmt, "The const expression was invalid"),
            Self::CanonicalizedHeapTypeReference => write!(
                fmt,
                "There was a canonicalized heap type reference without type index information"
            ),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::ParseError(e) => Some(e),
            Self::InvalidConstExpr | Self::CanonicalizedHeapTypeReference => None,
        }
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub trait WasmParserToWasmEncoder {
    fn core_module<'a, 'b>(
        &mut self,
        module: &'a mut crate::Module,
        sections: impl Iterator<Item = Result<wasmparser::Payload<'b>, wasmparser::BinaryReaderError>>,
    ) -> Result<&'a crate::Module> {
        utils::core_module(self, module, sections)
    }

    fn component_primitive_val_type(
        &mut self,
        ty: wasmparser::PrimitiveValType,
    ) -> crate::component::PrimitiveValType {
        utils::component_primitive_val_type(self, ty)
    }

    fn memory_index(&mut self, memory: u32) -> u32 {
        utils::memory_index(self, memory)
    }

    fn mem_arg(&mut self, arg: wasmparser::MemArg) -> crate::MemArg {
        utils::mem_arg(self, arg)
    }

    fn ordering(&mut self, arg: wasmparser::Ordering) -> crate::Ordering {
        utils::ordering(self, arg)
    }

    fn function_index(&mut self, func: u32) -> u32 {
        utils::function_index(self, func)
    }

    fn tag_index(&mut self, tag: u32) -> u32 {
        utils::tag_index(self, tag)
    }

    fn label_index(&mut self, label: u32) -> u32 {
        utils::label_index(self, label)
    }

    fn catch(&mut self, arg: wasmparser::Catch) -> crate::Catch {
        utils::catch(self, arg)
    }

    fn custom_section<'a>(
        &mut self,
        section: wasmparser::CustomSectionReader<'a>,
    ) -> crate::CustomSection<'a> {
        utils::custom_section(self, section)
    }

    fn export_kind(&mut self, external_kind: wasmparser::ExternalKind) -> crate::ExportKind {
        utils::export_kind(self, external_kind)
    }

    fn memory_type(&mut self, memory_ty: wasmparser::MemoryType) -> crate::MemoryType {
        utils::memory_type(self, memory_ty)
    }

    fn tag_kind(&mut self, kind: wasmparser::TagKind) -> crate::TagKind {
        utils::tag_kind(self, kind)
    }

    fn type_index(&mut self, ty: u32) -> u32 {
        utils::type_index(self, ty)
    }

    fn tag_type(&mut self, tag_ty: wasmparser::TagType) -> crate::TagType {
        utils::tag_type(self, tag_ty)
    }

    fn abstract_heap_type(
        &mut self,
        value: wasmparser::AbstractHeapType,
    ) -> crate::AbstractHeapType {
        utils::abstract_heap_type(self, value)
    }

    fn parse_type_section<'a>(
        &mut self,
        types: &'a mut crate::TypeSection,
        section: wasmparser::TypeSectionReader<'_>,
    ) -> Result<&'a mut crate::TypeSection> {
        utils::parse_type_section(self, types, section)
    }

    fn parse_recursive_type_group<'a>(
        &mut self,
        types: &'a mut crate::TypeSection,
        rec_group: wasmparser::RecGroup,
    ) -> Result<&'a mut crate::TypeSection> {
        utils::parse_recursive_type_group(self, types, rec_group)
    }

    fn sub_type(&mut self, sub_ty: wasmparser::SubType) -> Result<crate::SubType> {
        utils::sub_type(self, sub_ty)
    }

    fn composite_type(
        &mut self,
        composite_ty: wasmparser::CompositeType,
    ) -> Result<crate::CompositeType> {
        utils::composite_type(self, composite_ty)
    }

    fn func_type(&mut self, func_ty: wasmparser::FuncType) -> Result<crate::FuncType> {
        utils::func_type(self, func_ty)
    }

    fn array_type(&mut self, array_ty: wasmparser::ArrayType) -> Result<crate::ArrayType> {
        utils::array_type(self, array_ty)
    }

    fn struct_type(&mut self, struct_ty: wasmparser::StructType) -> Result<crate::StructType> {
        utils::struct_type(self, struct_ty)
    }

    fn field_type(&mut self, field_ty: wasmparser::FieldType) -> Result<crate::FieldType> {
        utils::field_type(self, field_ty)
    }

    fn storage_type(&mut self, storage_ty: wasmparser::StorageType) -> Result<crate::StorageType> {
        utils::storage_type(self, storage_ty)
    }

    fn val_type(&mut self, val_ty: wasmparser::ValType) -> Result<crate::ValType> {
        utils::val_type(self, val_ty)
    }

    fn ref_type(&mut self, ref_type: wasmparser::RefType) -> Result<crate::RefType> {
        utils::ref_type(self, ref_type)
    }

    fn heap_type(&mut self, heap_type: wasmparser::HeapType) -> Result<crate::HeapType> {
        utils::heap_type(self, heap_type)
    }

    fn parse_table_section<'a>(
        &mut self,
        tables: &'a mut crate::TableSection,
        section: wasmparser::TableSectionReader<'_>,
    ) -> Result<&'a mut crate::TableSection> {
        utils::parse_table_section(self, tables, section)
    }

    fn parse_table<'a>(
        &mut self,
        tables: &'a mut crate::TableSection,
        table: wasmparser::Table<'_>,
    ) -> Result<&'a mut crate::TableSection> {
        utils::parse_table(self, tables, table)
    }

    fn table_type(&mut self, table_ty: wasmparser::TableType) -> Result<crate::TableType> {
        utils::table_type(self, table_ty)
    }

    fn parse_tag_section<'a>(
        &mut self,
        tags: &'a mut crate::TagSection,
        section: wasmparser::TagSectionReader<'_>,
    ) -> Result<&'a mut crate::TagSection> {
        utils::parse_tag_section(self, tags, section)
    }

    fn parse_export_section<'a>(
        &mut self,
        exports: &'a mut crate::ExportSection,
        section: wasmparser::ExportSectionReader<'_>,
    ) -> Result<&'a mut crate::ExportSection> {
        utils::parse_export_section(self, exports, section)
    }

    fn export_index(&mut self, export: u32) -> u32 {
        utils::export_index(self, export)
    }

    fn parse_export<'a>(
        &mut self,
        exports: &'a mut crate::ExportSection,
        export: wasmparser::Export<'_>,
    ) -> &'a mut crate::ExportSection {
        utils::parse_export(self, exports, export)
    }

    fn parse_global_section<'a>(
        &mut self,
        globals: &'a mut crate::GlobalSection,
        section: wasmparser::GlobalSectionReader<'_>,
    ) -> Result<&'a mut crate::GlobalSection> {
        utils::parse_global_section(self, globals, section)
    }

    fn parse_global<'a>(
        &mut self,
        globals: &'a mut crate::GlobalSection,
        global: wasmparser::Global<'_>,
    ) -> Result<&'a mut crate::GlobalSection> {
        utils::parse_global(self, globals, global)
    }

    fn global_type(&mut self, global_ty: wasmparser::GlobalType) -> Result<crate::GlobalType> {
        utils::global_type(self, global_ty)
    }

    fn entity_type(&mut self, type_ref: wasmparser::TypeRef) -> Result<crate::EntityType> {
        utils::entity_type(self, type_ref)
    }

    fn parse_import_section<'a>(
        &mut self,
        imports: &'a mut crate::ImportSection,
        section: wasmparser::ImportSectionReader<'_>,
    ) -> Result<&'a mut crate::ImportSection> {
        utils::parse_import_section(self, imports, section)
    }

    fn parse_import<'a>(
        &mut self,
        imports: &'a mut crate::ImportSection,
        import: wasmparser::Import<'_>,
    ) -> Result<&'a mut crate::ImportSection> {
        utils::parse_import(self, imports, import)
    }

    fn parse_memory_section<'a>(
        &mut self,
        memories: &'a mut crate::MemorySection,
        section: wasmparser::MemorySectionReader<'_>,
    ) -> Result<&'a mut crate::MemorySection> {
        utils::parse_memory_section(self, memories, section)
    }

    fn parse_function_section<'a>(
        &mut self,
        functions: &'a mut crate::FunctionSection,
        section: wasmparser::FunctionSectionReader<'_>,
    ) -> Result<&'a mut crate::FunctionSection> {
        utils::parse_function_section(self, functions, section)
    }

    fn parse_data_section<'a>(
        &mut self,
        data: &'a mut crate::DataSection,
        section: wasmparser::DataSectionReader<'_>,
    ) -> Result<&'a mut crate::DataSection> {
        utils::parse_data_section(self, data, section)
    }

    fn parse_data<'a>(
        &mut self,
        data: &'a mut crate::DataSection,
        datum: wasmparser::Data<'_>,
    ) -> Result<&'a mut crate::DataSection> {
        utils::parse_data(self, data, datum)
    }

    fn parse_element_section<'a>(
        &mut self,
        elements: &'a mut crate::ElementSection,
        section: wasmparser::ElementSectionReader<'_>,
    ) -> Result<&'a mut crate::ElementSection> {
        utils::parse_element_section(self, elements, section)
    }

    fn parse_element<'a>(
        &mut self,
        elements: &'a mut crate::ElementSection,
        element: wasmparser::Element<'_>,
    ) -> Result<&'a mut crate::ElementSection> {
        utils::parse_element(self, elements, element)
    }

    fn table_index(&mut self, table: u32) -> u32 {
        utils::table_index(self, table)
    }

    fn global_index(&mut self, global: u32) -> u32 {
        utils::global_index(self, global)
    }

    fn data_index(&mut self, data: u32) -> u32 {
        utils::data_index(self, data)
    }

    fn element_index(&mut self, element: u32) -> u32 {
        utils::element_index(self, element)
    }

    fn const_expr(&mut self, const_expr: wasmparser::ConstExpr) -> Result<crate::ConstExpr> {
        utils::const_expr(self, const_expr)
    }

    fn block_type(&mut self, arg: wasmparser::BlockType) -> Result<crate::BlockType> {
        utils::block_type(self, arg)
    }

    fn instruction<'a>(&mut self, arg: wasmparser::Operator<'a>) -> Result<crate::Instruction<'a>> {
        utils::instruction(self, arg)
    }

    fn parse_code_section<'a>(
        &mut self,
        code: &'a mut crate::CodeSection,
        section: wasmparser::CodeSectionReader<'_>,
    ) -> Result<&'a mut crate::CodeSection> {
        utils::parse_code_section(self, code, section)
    }

    fn parse_function_body<'a>(
        &mut self,
        code: &'a mut crate::CodeSection,
        func: wasmparser::FunctionBody<'_>,
    ) -> Result<&'a mut crate::CodeSection> {
        utils::parse_function_body(self, code, func)
    }

    fn new_function_with_parsed_locals(
        &mut self,
        func: &wasmparser::FunctionBody<'_>,
    ) -> Result<crate::Function> {
        utils::new_function_with_parsed_locals(self, func)
    }

    fn parse_instruction<'a>(
        &mut self,
        function: &'a mut crate::Function,
        reader: &mut wasmparser::OperatorsReader<'_>,
    ) -> Result<&'a mut crate::Function> {
        utils::parse_instruction(self, function, reader)
    }
}

#[derive(Debug)]
pub struct RoundtripReencoder;

impl WasmParserToWasmEncoder for RoundtripReencoder {}

pub mod utils {
    use super::{Error, Result, WasmParserToWasmEncoder};

    pub fn core_module<'a, 'b>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        module: &'a mut crate::Module,
        sections: impl Iterator<Item = Result<wasmparser::Payload<'b>, wasmparser::BinaryReaderError>>,
    ) -> Result<&'a crate::Module> {
        fn module_section_flush_codes(
            module: &mut crate::Module,
            section: &impl crate::Section,
            codes: &mut Option<crate::CodeSection>,
        ) {
            if let Some(codes) = codes.take() {
                module.section(&codes);
            }

            module.section(section);
        }

        let mut codes = None;

        for section in sections {
            let section = section.map_err(Error::ParseError)?;

            match section {
                wasmparser::Payload::Version { .. } => (),
                wasmparser::Payload::TypeSection(section) => {
                    let mut types = crate::TypeSection::new();
                    reencoder.parse_type_section(&mut types, section)?;
                    module_section_flush_codes(module, &types, &mut codes);
                }
                wasmparser::Payload::ImportSection(section) => {
                    let mut imports = crate::ImportSection::new();
                    reencoder.parse_import_section(&mut imports, section)?;
                    module_section_flush_codes(module, &imports, &mut codes);
                }
                wasmparser::Payload::FunctionSection(section) => {
                    let mut functions = crate::FunctionSection::new();
                    reencoder.parse_function_section(&mut functions, section)?;
                    module_section_flush_codes(module, &functions, &mut codes);
                }
                wasmparser::Payload::TableSection(section) => {
                    let mut tables = crate::TableSection::new();
                    reencoder.parse_table_section(&mut tables, section)?;
                    module_section_flush_codes(module, &tables, &mut codes);
                }
                wasmparser::Payload::MemorySection(section) => {
                    let mut memories = crate::MemorySection::new();
                    reencoder.parse_memory_section(&mut memories, section)?;
                    module_section_flush_codes(module, &memories, &mut codes);
                }
                wasmparser::Payload::TagSection(section) => {
                    let mut tags = crate::TagSection::new();
                    reencoder.parse_tag_section(&mut tags, section)?;
                    module_section_flush_codes(module, &tags, &mut codes);
                }
                wasmparser::Payload::GlobalSection(section) => {
                    let mut globals = crate::GlobalSection::new();
                    reencoder.parse_global_section(&mut globals, section)?;
                    module_section_flush_codes(module, &globals, &mut codes);
                }
                wasmparser::Payload::ExportSection(section) => {
                    let mut exports = crate::ExportSection::new();
                    reencoder.parse_export_section(&mut exports, section)?;
                    module_section_flush_codes(module, &exports, &mut codes);
                }
                wasmparser::Payload::StartSection { func, .. } => {
                    module_section_flush_codes(
                        module,
                        &crate::StartSection {
                            function_index: reencoder.function_index(func),
                        },
                        &mut codes,
                    );
                }
                wasmparser::Payload::ElementSection(section) => {
                    let mut elements = crate::ElementSection::new();
                    reencoder.parse_element_section(&mut elements, section)?;
                    module_section_flush_codes(module, &elements, &mut codes);
                }
                wasmparser::Payload::DataCountSection { .. } => (),
                wasmparser::Payload::DataSection(section) => {
                    let mut data = crate::DataSection::new();
                    reencoder.parse_data_section(&mut data, section)?;
                    module_section_flush_codes(module, &data, &mut codes);
                }
                wasmparser::Payload::CodeSectionStart { .. } => {
                    if let Some(codes) = codes.replace(crate::CodeSection::new()) {
                        module.section(&codes);
                    }
                }
                wasmparser::Payload::CodeSectionEntry(section) => {
                    let codes = codes.get_or_insert_with(crate::CodeSection::new);
                    reencoder.parse_function_body(codes, section)?;
                }
                wasmparser::Payload::ModuleSection { .. }
                | wasmparser::Payload::InstanceSection(_)
                | wasmparser::Payload::CoreTypeSection(_)
                | wasmparser::Payload::ComponentSection { .. }
                | wasmparser::Payload::ComponentInstanceSection(_)
                | wasmparser::Payload::ComponentAliasSection(_)
                | wasmparser::Payload::ComponentTypeSection(_)
                | wasmparser::Payload::ComponentCanonicalSection(_)
                | wasmparser::Payload::ComponentStartSection { .. }
                | wasmparser::Payload::ComponentImportSection(_)
                | wasmparser::Payload::ComponentExportSection(_) => {
                    unreachable!("wasm component model section is not allowed in core wasm module")
                }
                wasmparser::Payload::CustomSection(section) => {
                    module_section_flush_codes(
                        module,
                        &reencoder.custom_section(section),
                        &mut codes,
                    );
                }
                wasmparser::Payload::UnknownSection { id, .. } => {
                    unreachable!("unknown wasm section with id {id}")
                }
                wasmparser::Payload::End(_) => (),
            }
        }

        if let Some(codes) = codes {
            module.section(&codes);
        }

        Ok(module)
    }

    pub fn component_primitive_val_type(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        ty: wasmparser::PrimitiveValType,
    ) -> crate::component::PrimitiveValType {
        match ty {
            wasmparser::PrimitiveValType::Bool => crate::component::PrimitiveValType::Bool,
            wasmparser::PrimitiveValType::S8 => crate::component::PrimitiveValType::S8,
            wasmparser::PrimitiveValType::U8 => crate::component::PrimitiveValType::U8,
            wasmparser::PrimitiveValType::S16 => crate::component::PrimitiveValType::S16,
            wasmparser::PrimitiveValType::U16 => crate::component::PrimitiveValType::U16,
            wasmparser::PrimitiveValType::S32 => crate::component::PrimitiveValType::S32,
            wasmparser::PrimitiveValType::U32 => crate::component::PrimitiveValType::U32,
            wasmparser::PrimitiveValType::S64 => crate::component::PrimitiveValType::S64,
            wasmparser::PrimitiveValType::U64 => crate::component::PrimitiveValType::U64,
            wasmparser::PrimitiveValType::F32 => crate::component::PrimitiveValType::F32,
            wasmparser::PrimitiveValType::F64 => crate::component::PrimitiveValType::F64,
            wasmparser::PrimitiveValType::Char => crate::component::PrimitiveValType::Char,
            wasmparser::PrimitiveValType::String => crate::component::PrimitiveValType::String,
        }
    }

    pub fn memory_index(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        memory: u32,
    ) -> u32 {
        memory
    }

    pub fn mem_arg(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        arg: wasmparser::MemArg,
    ) -> crate::MemArg {
        crate::MemArg {
            offset: arg.offset,
            align: arg.align.into(),
            memory_index: reencoder.memory_index(arg.memory),
        }
    }

    pub fn ordering(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        arg: wasmparser::Ordering,
    ) -> crate::Ordering {
        match arg {
            wasmparser::Ordering::SeqCst => crate::Ordering::SeqCst,
            wasmparser::Ordering::AcqRel => crate::Ordering::AcqRel,
        }
    }

    pub fn function_index(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        func: u32,
    ) -> u32 {
        func
    }

    pub fn tag_index(_reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder), tag: u32) -> u32 {
        tag
    }

    pub fn label_index(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        label: u32,
    ) -> u32 {
        label
    }

    pub fn catch(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        arg: wasmparser::Catch,
    ) -> crate::Catch {
        match arg {
            wasmparser::Catch::One { tag, label } => crate::Catch::One {
                tag: reencoder.tag_index(tag),
                label: reencoder.label_index(label),
            },
            wasmparser::Catch::OneRef { tag, label } => crate::Catch::OneRef {
                tag: reencoder.tag_index(tag),
                label: reencoder.label_index(label),
            },
            wasmparser::Catch::All { label } => crate::Catch::All {
                label: reencoder.label_index(label),
            },
            wasmparser::Catch::AllRef { label } => crate::Catch::AllRef {
                label: reencoder.label_index(label),
            },
        }
    }

    pub fn custom_section<'a>(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        section: wasmparser::CustomSectionReader<'a>,
    ) -> crate::CustomSection<'a> {
        crate::CustomSection {
            data: section.data().into(),
            name: section.name().into(),
        }
    }

    pub fn export_kind(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        external_kind: wasmparser::ExternalKind,
    ) -> crate::ExportKind {
        match external_kind {
            wasmparser::ExternalKind::Func => crate::ExportKind::Func,
            wasmparser::ExternalKind::Table => crate::ExportKind::Table,
            wasmparser::ExternalKind::Memory => crate::ExportKind::Memory,
            wasmparser::ExternalKind::Global => crate::ExportKind::Global,
            wasmparser::ExternalKind::Tag => crate::ExportKind::Tag,
        }
    }

    pub fn memory_type(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        memory_ty: wasmparser::MemoryType,
    ) -> crate::MemoryType {
        crate::MemoryType {
            minimum: memory_ty.initial,
            maximum: memory_ty.maximum,
            memory64: memory_ty.memory64,
            shared: memory_ty.shared,
            page_size_log2: memory_ty.page_size_log2,
        }
    }

    pub fn tag_kind(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        kind: wasmparser::TagKind,
    ) -> crate::TagKind {
        match kind {
            wasmparser::TagKind::Exception => crate::TagKind::Exception,
        }
    }

    pub fn type_index(_reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder), ty: u32) -> u32 {
        ty
    }

    pub fn tag_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        tag_ty: wasmparser::TagType,
    ) -> crate::TagType {
        crate::TagType {
            kind: reencoder.tag_kind(tag_ty.kind),
            func_type_idx: reencoder.type_index(tag_ty.func_type_idx),
        }
    }

    pub fn abstract_heap_type(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        value: wasmparser::AbstractHeapType,
    ) -> crate::AbstractHeapType {
        use wasmparser::AbstractHeapType::*;
        match value {
            Func => crate::AbstractHeapType::Func,
            Extern => crate::AbstractHeapType::Extern,
            Any => crate::AbstractHeapType::Any,
            None => crate::AbstractHeapType::None,
            NoExtern => crate::AbstractHeapType::NoExtern,
            NoFunc => crate::AbstractHeapType::NoFunc,
            Eq => crate::AbstractHeapType::Eq,
            Struct => crate::AbstractHeapType::Struct,
            Array => crate::AbstractHeapType::Array,
            I31 => crate::AbstractHeapType::I31,
            Exn => crate::AbstractHeapType::Exn,
            NoExn => crate::AbstractHeapType::NoExn,
        }
    }

    pub fn parse_type_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        types: &'a mut crate::TypeSection,
        section: wasmparser::TypeSectionReader<'_>,
    ) -> Result<&'a mut crate::TypeSection> {
        for rec_group in section {
            reencoder.parse_recursive_type_group(types, rec_group.map_err(Error::ParseError)?)?;
        }
        Ok(types)
    }

    pub fn parse_recursive_type_group<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        types: &'a mut crate::TypeSection,
        rec_group: wasmparser::RecGroup,
    ) -> Result<&'a mut crate::TypeSection> {
        if rec_group.is_explicit_rec_group() {
            let subtypes = rec_group
                .into_types()
                .map(|t| reencoder.sub_type(t))
                .collect::<Result<Vec<_>, _>>()?;
            types.rec(subtypes);
        } else {
            let ty = rec_group.into_types().next().unwrap();
            types.subtype(&reencoder.sub_type(ty)?);
        }
        Ok(types)
    }

    pub fn sub_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        sub_ty: wasmparser::SubType,
    ) -> Result<crate::SubType> {
        Ok(crate::SubType {
            is_final: sub_ty.is_final,
            supertype_idx: sub_ty
                .supertype_idx
                .map(|i| {
                    i.as_module_index()
                        .map(|ty| reencoder.type_index(ty))
                        .ok_or(Error::CanonicalizedHeapTypeReference)
                })
                .transpose()?,
            composite_type: reencoder.composite_type(sub_ty.composite_type)?,
        })
    }

    pub fn composite_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        composite_ty: wasmparser::CompositeType,
    ) -> Result<crate::CompositeType> {
        Ok(match composite_ty {
            wasmparser::CompositeType::Func(f) => {
                crate::CompositeType::Func(reencoder.func_type(f)?)
            }
            wasmparser::CompositeType::Array(a) => {
                crate::CompositeType::Array(reencoder.array_type(a)?)
            }
            wasmparser::CompositeType::Struct(s) => {
                crate::CompositeType::Struct(reencoder.struct_type(s)?)
            }
        })
    }

    pub fn func_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        func_ty: wasmparser::FuncType,
    ) -> Result<crate::FuncType> {
        let mut buf = Vec::with_capacity(func_ty.params().len() + func_ty.results().len());
        for ty in func_ty.params().iter().chain(func_ty.results()).copied() {
            buf.push(reencoder.val_type(ty)?);
        }
        Ok(crate::FuncType::from_parts(
            buf.into(),
            func_ty.params().len(),
        ))
    }

    pub fn array_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        array_ty: wasmparser::ArrayType,
    ) -> Result<crate::ArrayType> {
        Ok(crate::ArrayType(reencoder.field_type(array_ty.0)?))
    }

    pub fn struct_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        struct_ty: wasmparser::StructType,
    ) -> Result<crate::StructType> {
        Ok(crate::StructType {
            fields: struct_ty
                .fields
                .iter()
                .map(|field_ty| reencoder.field_type(field_ty.clone()))
                .collect::<Result<_, _>>()?,
        })
    }

    pub fn field_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        field_ty: wasmparser::FieldType,
    ) -> Result<crate::FieldType> {
        Ok(crate::FieldType {
            element_type: reencoder.storage_type(field_ty.element_type)?,
            mutable: field_ty.mutable,
        })
    }

    pub fn storage_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        storage_ty: wasmparser::StorageType,
    ) -> Result<crate::StorageType> {
        Ok(match storage_ty {
            wasmparser::StorageType::I8 => crate::StorageType::I8,
            wasmparser::StorageType::I16 => crate::StorageType::I16,
            wasmparser::StorageType::Val(v) => crate::StorageType::Val(reencoder.val_type(v)?),
        })
    }

    pub fn val_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        val_ty: wasmparser::ValType,
    ) -> Result<crate::ValType> {
        Ok(match val_ty {
            wasmparser::ValType::I32 => crate::ValType::I32,
            wasmparser::ValType::I64 => crate::ValType::I64,
            wasmparser::ValType::F32 => crate::ValType::F32,
            wasmparser::ValType::F64 => crate::ValType::F64,
            wasmparser::ValType::V128 => crate::ValType::V128,
            wasmparser::ValType::Ref(r) => crate::ValType::Ref(reencoder.ref_type(r)?),
        })
    }

    pub fn ref_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        ref_type: wasmparser::RefType,
    ) -> Result<crate::RefType> {
        Ok(crate::RefType {
            nullable: ref_type.is_nullable(),
            heap_type: reencoder.heap_type(ref_type.heap_type())?,
        })
    }

    pub fn heap_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        heap_type: wasmparser::HeapType,
    ) -> Result<crate::HeapType> {
        Ok(match heap_type {
            wasmparser::HeapType::Concrete(i) => crate::HeapType::Concrete(
                i.as_module_index()
                    .map(|ty| reencoder.type_index(ty))
                    .ok_or(Error::CanonicalizedHeapTypeReference)?,
            ),
            wasmparser::HeapType::Abstract { shared, ty } => crate::HeapType::Abstract {
                shared,
                ty: reencoder.abstract_heap_type(ty),
            },
        })
    }

    pub fn parse_table_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        tables: &'a mut crate::TableSection,
        section: wasmparser::TableSectionReader<'_>,
    ) -> Result<&'a mut crate::TableSection> {
        for table in section {
            reencoder.parse_table(tables, table.map_err(Error::ParseError)?)?;
        }
        Ok(tables)
    }

    pub fn parse_table<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        tables: &'a mut crate::TableSection,
        table: wasmparser::Table<'_>,
    ) -> Result<&'a mut crate::TableSection> {
        let ty = reencoder.table_type(table.ty)?;
        match table.init {
            wasmparser::TableInit::RefNull => {
                tables.table(ty);
            }
            wasmparser::TableInit::Expr(e) => {
                tables.table_with_init(ty, &reencoder.const_expr(e)?);
            }
        }
        Ok(tables)
    }

    pub fn table_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        table_ty: wasmparser::TableType,
    ) -> Result<crate::TableType> {
        Ok(crate::TableType {
            element_type: reencoder.ref_type(table_ty.element_type)?,
            minimum: table_ty.initial,
            maximum: table_ty.maximum,
            table64: table_ty.table64,
        })
    }

    pub fn parse_tag_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        tags: &'a mut crate::TagSection,
        section: wasmparser::TagSectionReader<'_>,
    ) -> Result<&'a mut crate::TagSection> {
        for tag in section {
            let tag = tag.map_err(Error::ParseError)?;
            tags.tag(reencoder.tag_type(tag));
        }
        Ok(tags)
    }

    pub fn parse_export_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        exports: &'a mut crate::ExportSection,
        section: wasmparser::ExportSectionReader<'_>,
    ) -> Result<&'a mut crate::ExportSection> {
        for export in section {
            reencoder.parse_export(exports, export.map_err(Error::ParseError)?);
        }
        Ok(exports)
    }

    pub fn export_index(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        export: u32,
    ) -> u32 {
        export
    }

    pub fn parse_export<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        exports: &'a mut crate::ExportSection,
        export: wasmparser::Export<'_>,
    ) -> &'a mut crate::ExportSection {
        exports.export(
            export.name,
            reencoder.export_kind(export.kind),
            reencoder.export_index(export.index),
        )
    }

    pub fn parse_global_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        globals: &'a mut crate::GlobalSection,
        section: wasmparser::GlobalSectionReader<'_>,
    ) -> Result<&'a mut crate::GlobalSection> {
        for global in section {
            reencoder.parse_global(globals, global.map_err(Error::ParseError)?)?;
        }
        Ok(globals)
    }

    pub fn parse_global<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        globals: &'a mut crate::GlobalSection,
        global: wasmparser::Global<'_>,
    ) -> Result<&'a mut crate::GlobalSection> {
        globals.global(
            reencoder.global_type(global.ty)?,
            &reencoder.const_expr(global.init_expr)?,
        );
        Ok(globals)
    }

    pub fn global_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        global_ty: wasmparser::GlobalType,
    ) -> Result<crate::GlobalType> {
        Ok(crate::GlobalType {
            val_type: reencoder.val_type(global_ty.content_type)?,
            mutable: global_ty.mutable,
            shared: global_ty.shared,
        })
    }

    pub fn entity_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        type_ref: wasmparser::TypeRef,
    ) -> Result<crate::EntityType> {
        Ok(match type_ref {
            wasmparser::TypeRef::Func(i) => crate::EntityType::Function(reencoder.type_index(i)),
            wasmparser::TypeRef::Table(t) => crate::EntityType::Table(reencoder.table_type(t)?),
            wasmparser::TypeRef::Memory(m) => crate::EntityType::Memory(reencoder.memory_type(m)),
            wasmparser::TypeRef::Global(g) => crate::EntityType::Global(reencoder.global_type(g)?),
            wasmparser::TypeRef::Tag(t) => crate::EntityType::Tag(reencoder.tag_type(t)),
        })
    }

    pub fn parse_import_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        imports: &'a mut crate::ImportSection,
        section: wasmparser::ImportSectionReader<'_>,
    ) -> Result<&'a mut crate::ImportSection> {
        for import in section {
            reencoder.parse_import(imports, import.map_err(Error::ParseError)?)?;
        }
        Ok(imports)
    }

    pub fn parse_import<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        imports: &'a mut crate::ImportSection,
        import: wasmparser::Import<'_>,
    ) -> Result<&'a mut crate::ImportSection> {
        imports.import(
            import.module,
            import.name,
            reencoder.entity_type(import.ty)?,
        );
        Ok(imports)
    }

    pub fn parse_memory_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        memories: &'a mut crate::MemorySection,
        section: wasmparser::MemorySectionReader<'_>,
    ) -> Result<&'a mut crate::MemorySection> {
        for memory in section {
            let memory = memory.map_err(Error::ParseError)?;
            memories.memory(reencoder.memory_type(memory));
        }
        Ok(memories)
    }

    pub fn parse_function_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        functions: &'a mut crate::FunctionSection,
        section: wasmparser::FunctionSectionReader<'_>,
    ) -> Result<&'a mut crate::FunctionSection> {
        for func in section {
            functions.function(reencoder.type_index(func.map_err(Error::ParseError)?));
        }
        Ok(functions)
    }

    pub fn parse_data_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        data: &'a mut crate::DataSection,
        section: wasmparser::DataSectionReader<'_>,
    ) -> Result<&'a mut crate::DataSection> {
        for datum in section {
            reencoder.parse_data(data, datum.map_err(Error::ParseError)?)?;
        }
        Ok(data)
    }

    pub fn parse_data<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        data: &'a mut crate::DataSection,
        datum: wasmparser::Data<'_>,
    ) -> Result<&'a mut crate::DataSection> {
        match datum.kind {
            wasmparser::DataKind::Active {
                memory_index,
                offset_expr,
            } => Ok(data.active(
                reencoder.memory_index(memory_index),
                &reencoder.const_expr(offset_expr)?,
                datum.data.iter().copied(),
            )),
            wasmparser::DataKind::Passive => Ok(data.passive(datum.data.iter().copied())),
        }
    }

    pub fn parse_element_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        elements: &'a mut crate::ElementSection,
        section: wasmparser::ElementSectionReader<'_>,
    ) -> Result<&'a mut crate::ElementSection> {
        for element in section {
            reencoder.parse_element(elements, element.map_err(Error::ParseError)?)?;
        }
        Ok(elements)
    }

    pub fn parse_element<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        elements: &'a mut crate::ElementSection,
        element: wasmparser::Element<'_>,
    ) -> Result<&'a mut crate::ElementSection> {
        let mut funcs;
        let mut exprs;
        let elems = match element.items {
            wasmparser::ElementItems::Functions(f) => {
                funcs = Vec::new();
                for func in f {
                    funcs.push(reencoder.function_index(func.map_err(Error::ParseError)?));
                }
                crate::Elements::Functions(&funcs)
            }
            wasmparser::ElementItems::Expressions(ty, e) => {
                exprs = Vec::new();
                for expr in e {
                    exprs.push(reencoder.const_expr(expr.map_err(Error::ParseError)?)?);
                }
                crate::Elements::Expressions(reencoder.ref_type(ty)?, &exprs)
            }
        };
        match element.kind {
            wasmparser::ElementKind::Active {
                table_index,
                offset_expr,
            } => Ok(elements.active(
                table_index.map(|t| reencoder.table_index(t)),
                &reencoder.const_expr(offset_expr)?,
                elems,
            )),
            wasmparser::ElementKind::Passive => Ok(elements.passive(elems)),
            wasmparser::ElementKind::Declared => Ok(elements.declared(elems)),
        }
    }

    pub fn table_index(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        table: u32,
    ) -> u32 {
        table
    }

    pub fn global_index(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        global: u32,
    ) -> u32 {
        global
    }

    pub fn data_index(_reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder), data: u32) -> u32 {
        data
    }

    pub fn element_index(
        _reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        element: u32,
    ) -> u32 {
        element
    }

    pub fn const_expr(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        const_expr: wasmparser::ConstExpr,
    ) -> Result<crate::ConstExpr> {
        let mut ops = const_expr.get_operators_reader().into_iter();

        let result = match ops.next() {
            Some(Ok(wasmparser::Operator::I32Const { value })) => {
                crate::ConstExpr::i32_const(value)
            }
            Some(Ok(wasmparser::Operator::I64Const { value })) => {
                crate::ConstExpr::i64_const(value)
            }
            Some(Ok(wasmparser::Operator::F32Const { value })) => {
                crate::ConstExpr::f32_const(f32::from_bits(value.bits()))
            }
            Some(Ok(wasmparser::Operator::F64Const { value })) => {
                crate::ConstExpr::f64_const(f64::from_bits(value.bits()))
            }
            Some(Ok(wasmparser::Operator::V128Const { value })) => {
                crate::ConstExpr::v128_const(i128::from_le_bytes(*value.bytes()))
            }
            Some(Ok(wasmparser::Operator::RefNull { hty })) => {
                crate::ConstExpr::ref_null(reencoder.heap_type(hty)?)
            }
            Some(Ok(wasmparser::Operator::RefFunc { function_index })) => {
                crate::ConstExpr::ref_func(reencoder.function_index(function_index))
            }
            Some(Ok(wasmparser::Operator::GlobalGet { global_index })) => {
                crate::ConstExpr::global_get(reencoder.global_index(global_index))
            }

            // TODO: support the extended-const proposal.
            Some(Ok(_op)) => return Err(Error::InvalidConstExpr),

            Some(Err(e)) => return Err(Error::ParseError(e)),
            None => return Err(Error::InvalidConstExpr),
        };

        match (ops.next(), ops.next()) {
            (Some(Ok(wasmparser::Operator::End)), None) => Ok(result),
            _ => Err(Error::InvalidConstExpr),
        }
    }

    pub fn block_type(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        arg: wasmparser::BlockType,
    ) -> Result<crate::BlockType> {
        match arg {
            wasmparser::BlockType::Empty => Ok(crate::BlockType::Empty),
            wasmparser::BlockType::FuncType(n) => {
                Ok(crate::BlockType::FunctionType(reencoder.type_index(n)))
            }
            wasmparser::BlockType::Type(t) => Ok(crate::BlockType::Result(reencoder.val_type(t)?)),
        }
    }

    pub fn instruction<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        arg: wasmparser::Operator<'a>,
    ) -> Result<crate::Instruction<'a>> {
        use crate::Instruction;

        macro_rules! translate {
            ($( @$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
                Ok(match arg {
                    $(
                        wasmparser::Operator::$op $({ $($arg),* })? => {
                            $(
                                $(let $arg = translate!(map $arg $arg);)*
                            )?
                            translate!(build $op $($($arg)*)?)
                        }
                    )*
                })
            };

            // This case is used to map, based on the name of the field, from the
            // wasmparser payload type to the wasm-encoder payload type through
            // `Translator` as applicable.
            (map $arg:ident tag_index) => (reencoder.tag_index($arg));
            (map $arg:ident function_index) => (reencoder.function_index($arg));
            (map $arg:ident table) => (reencoder.table_index($arg));
            (map $arg:ident table_index) => (reencoder.table_index($arg));
            (map $arg:ident dst_table) => (reencoder.table_index($arg));
            (map $arg:ident src_table) => (reencoder.table_index($arg));
            (map $arg:ident type_index) => (reencoder.type_index($arg));
            (map $arg:ident array_type_index) => (reencoder.type_index($arg));
            (map $arg:ident array_type_index_dst) => (reencoder.type_index($arg));
            (map $arg:ident array_type_index_src) => (reencoder.type_index($arg));
            (map $arg:ident struct_type_index) => (reencoder.type_index($arg));
            (map $arg:ident global_index) => (reencoder.global_index($arg));
            (map $arg:ident mem) => (reencoder.memory_index($arg));
            (map $arg:ident src_mem) => (reencoder.memory_index($arg));
            (map $arg:ident dst_mem) => (reencoder.memory_index($arg));
            (map $arg:ident data_index) => (reencoder.data_index($arg));
            (map $arg:ident elem_index) => (reencoder.element_index($arg));
            (map $arg:ident array_data_index) => (reencoder.data_index($arg));
            (map $arg:ident array_elem_index) => (reencoder.element_index($arg));
            (map $arg:ident blockty) => (reencoder.block_type($arg)?);
            (map $arg:ident relative_depth) => ($arg);
            (map $arg:ident targets) => ((
                $arg
                    .targets()
                    .collect::<Result<Vec<_>, wasmparser::BinaryReaderError>>().map_err(Error::ParseError)?
                    .into(),
                $arg.default(),
            ));
            (map $arg:ident flags) => (());
            (map $arg:ident ty) => (reencoder.val_type($arg)?);
            (map $arg:ident hty) => (reencoder.heap_type($arg)?);
            (map $arg:ident from_ref_type) => (reencoder.ref_type($arg)?);
            (map $arg:ident to_ref_type) => (reencoder.ref_type($arg)?);
            (map $arg:ident memarg) => (reencoder.mem_arg($arg));
            (map $arg:ident ordering) => (reencoder.ordering($arg));
            (map $arg:ident local_index) => ($arg);
            (map $arg:ident value) => ($arg);
            (map $arg:ident lane) => ($arg);
            (map $arg:ident lanes) => ($arg);
            (map $arg:ident array_size) => ($arg);
            (map $arg:ident field_index) => ($arg);
            (map $arg:ident try_table) => ($arg);

            // This case takes the arguments of a wasmparser instruction and creates
            // a wasm-encoder instruction. There are a few special cases for where
            // the structure of a wasmparser instruction differs from that of
            // wasm-encoder.
            (build $op:ident) => (Instruction::$op);
            (build BrTable $arg:ident) => (Instruction::BrTable($arg.0, $arg.1));
            (build I32Const $arg:ident) => (Instruction::I32Const($arg));
            (build I64Const $arg:ident) => (Instruction::I64Const($arg));
            (build F32Const $arg:ident) => (Instruction::F32Const(f32::from_bits($arg.bits())));
            (build F64Const $arg:ident) => (Instruction::F64Const(f64::from_bits($arg.bits())));
            (build V128Const $arg:ident) => (Instruction::V128Const($arg.i128()));
            (build TryTable $table:ident) => (Instruction::TryTable(reencoder.block_type($table.ty)?, {
                $table.catches.into_iter().map(|c| reencoder.catch(c)).collect::<Vec<_>>().into()
            }));
            (build $op:ident $arg:ident) => (Instruction::$op($arg));
            (build $op:ident $($arg:ident)*) => (Instruction::$op { $($arg),* });
        }

        wasmparser::for_each_operator!(translate)
    }

    pub fn parse_code_section<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        code: &'a mut crate::CodeSection,
        section: wasmparser::CodeSectionReader<'_>,
    ) -> Result<&'a mut crate::CodeSection> {
        for func in section {
            reencoder.parse_function_body(code, func.map_err(Error::ParseError)?)?;
        }
        Ok(code)
    }

    pub fn parse_function_body<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        code: &'a mut crate::CodeSection,
        func: wasmparser::FunctionBody<'_>,
    ) -> Result<&'a mut crate::CodeSection> {
        let mut f = reencoder.new_function_with_parsed_locals(&func)?;
        let mut reader = func.get_operators_reader().map_err(Error::ParseError)?;
        while !reader.eof() {
            reencoder.parse_instruction(&mut f, &mut reader)?;
        }
        Ok(code.function(&f))
    }

    pub fn new_function_with_parsed_locals(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        func: &wasmparser::FunctionBody<'_>,
    ) -> Result<crate::Function> {
        let mut locals = Vec::new();
        for pair in func.get_locals_reader().map_err(Error::ParseError)? {
            let (cnt, ty) = pair.map_err(Error::ParseError)?;
            locals.push((cnt, reencoder.val_type(ty)?));
        }
        Ok(crate::Function::new(locals))
    }

    pub fn parse_instruction<'a>(
        reencoder: &mut (impl ?Sized + WasmParserToWasmEncoder),
        function: &'a mut crate::Function,
        reader: &mut wasmparser::OperatorsReader<'_>,
    ) -> Result<&'a mut crate::Function> {
        Ok(
            function
                .instruction(&reencoder.instruction(reader.read().map_err(Error::ParseError)?)?),
        )
    }
}
