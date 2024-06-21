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

impl From<wasmparser::BinaryReaderError> for Error {
    fn from(err: wasmparser::BinaryReaderError) -> Self {
        Self::ParseError(err)
    }
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

pub trait Reencode {
    fn core_module<'a, 'b>(
        &mut self,
        module: &mut crate::Module,
        sections: impl Iterator<Item = Result<wasmparser::Payload<'b>, wasmparser::BinaryReaderError>>,
    ) -> Result<()> {
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

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the types to the `types` section.
    fn parse_type_section(
        &mut self,
        types: &mut crate::TypeSection,
        section: wasmparser::TypeSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_type_section(self, types, section)
    }

    /// Parses a single [`wasmparser::RecGroup`] and adds it to the `types` section.
    fn parse_recursive_type_group(
        &mut self,
        types: &mut crate::TypeSection,
        rec_group: wasmparser::RecGroup,
    ) -> Result<()> {
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

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the tables to the `tables` section.
    fn parse_table_section(
        &mut self,
        tables: &mut crate::TableSection,
        section: wasmparser::TableSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_table_section(self, tables, section)
    }

    /// Parses a single [`wasmparser::Table`] and adds it to the `tables` section.
    fn parse_table(
        &mut self,
        tables: &mut crate::TableSection,
        table: wasmparser::Table<'_>,
    ) -> Result<()> {
        utils::parse_table(self, tables, table)
    }

    fn table_type(&mut self, table_ty: wasmparser::TableType) -> Result<crate::TableType> {
        utils::table_type(self, table_ty)
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the tags to the `tags` section.
    fn parse_tag_section(
        &mut self,
        tags: &mut crate::TagSection,
        section: wasmparser::TagSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_tag_section(self, tags, section)
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the exports to the `exports` section.
    fn parse_export_section(
        &mut self,
        exports: &mut crate::ExportSection,
        section: wasmparser::ExportSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_export_section(self, exports, section)
    }

    fn export_index(&mut self, export: u32) -> u32 {
        utils::export_index(self, export)
    }

    /// Parses the single [`wasmparser::Export`] provided and adds it to the
    /// `exports` section.
    fn parse_export(&mut self, exports: &mut crate::ExportSection, export: wasmparser::Export<'_>) {
        utils::parse_export(self, exports, export)
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the globals to the `globals` section.
    fn parse_global_section(
        &mut self,
        globals: &mut crate::GlobalSection,
        section: wasmparser::GlobalSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_global_section(self, globals, section)
    }

    /// Parses the single [`wasmparser::Global`] provided and adds it to the
    /// `globals` section.
    fn parse_global(
        &mut self,
        globals: &mut crate::GlobalSection,
        global: wasmparser::Global<'_>,
    ) -> Result<()> {
        utils::parse_global(self, globals, global)
    }

    fn global_type(&mut self, global_ty: wasmparser::GlobalType) -> Result<crate::GlobalType> {
        utils::global_type(self, global_ty)
    }

    fn entity_type(&mut self, type_ref: wasmparser::TypeRef) -> Result<crate::EntityType> {
        utils::entity_type(self, type_ref)
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the imports to the `import` section.
    fn parse_import_section(
        &mut self,
        imports: &mut crate::ImportSection,
        section: wasmparser::ImportSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_import_section(self, imports, section)
    }

    /// Parses the single [`wasmparser::Import`] provided and adds it to the
    /// `import` section.
    fn parse_import(
        &mut self,
        imports: &mut crate::ImportSection,
        import: wasmparser::Import<'_>,
    ) -> Result<()> {
        utils::parse_import(self, imports, import)
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the memories to the `memories` section.
    fn parse_memory_section(
        &mut self,
        memories: &mut crate::MemorySection,
        section: wasmparser::MemorySectionReader<'_>,
    ) -> Result<()> {
        utils::parse_memory_section(self, memories, section)
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the functions to the `functions` section.
    fn parse_function_section(
        &mut self,
        functions: &mut crate::FunctionSection,
        section: wasmparser::FunctionSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_function_section(self, functions, section)
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the data to the `data` section.
    fn parse_data_section(
        &mut self,
        data: &mut crate::DataSection,
        section: wasmparser::DataSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_data_section(self, data, section)
    }

    /// Parses a single [`wasmparser::Data`] and adds it to the `data` section.
    fn parse_data(
        &mut self,
        data: &mut crate::DataSection,
        datum: wasmparser::Data<'_>,
    ) -> Result<()> {
        utils::parse_data(self, data, datum)
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the elements to the `element` section.
    fn parse_element_section(
        &mut self,
        elements: &mut crate::ElementSection,
        section: wasmparser::ElementSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_element_section(self, elements, section)
    }

    /// Parses the single [`wasmparser::Element`] provided and adds it to the
    /// `element` section.
    fn parse_element(
        &mut self,
        elements: &mut crate::ElementSection,
        element: wasmparser::Element<'_>,
    ) -> Result<()> {
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

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the code to the `code` section.
    fn parse_code_section(
        &mut self,
        code: &mut crate::CodeSection,
        section: wasmparser::CodeSectionReader<'_>,
    ) -> Result<()> {
        utils::parse_code_section(self, code, section)
    }

    /// Parses a single [`wasmparser::FunctionBody`] and adds it to the `code` section.
    fn parse_function_body(
        &mut self,
        code: &mut crate::CodeSection,
        func: wasmparser::FunctionBody<'_>,
    ) -> Result<()> {
        utils::parse_function_body(self, code, func)
    }

    /// Create a new [`crate::Function`] by parsing the locals declarations from the
    /// provided [`wasmparser::FunctionBody`].
    fn new_function_with_parsed_locals(
        &mut self,
        func: &wasmparser::FunctionBody<'_>,
    ) -> Result<crate::Function> {
        utils::new_function_with_parsed_locals(self, func)
    }

    /// Parses a single instruction from `reader` and adds it to `function`.
    fn parse_instruction(
        &mut self,
        function: &mut crate::Function,
        reader: &mut wasmparser::OperatorsReader<'_>,
    ) -> Result<()> {
        utils::parse_instruction(self, function, reader)
    }
}

#[derive(Debug)]
pub struct RoundtripReencoder;

impl Reencode for RoundtripReencoder {}

pub mod utils {
    use super::{Error, Reencode, Result};

    pub fn core_module<'a, 'b>(
        reencoder: &mut (impl ?Sized + Reencode),
        module: &'a mut crate::Module,
        sections: impl Iterator<Item = Result<wasmparser::Payload<'b>, wasmparser::BinaryReaderError>>,
    ) -> Result<()> {
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
            let section = section?;

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
                wasmparser::Payload::DataCountSection { count, .. } => {
                    module_section_flush_codes(
                        module,
                        &crate::DataCountSection { count },
                        &mut codes,
                    );
                }
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

        Ok(())
    }

    pub fn component_primitive_val_type(
        _reencoder: &mut (impl ?Sized + Reencode),
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

    pub fn memory_index(_reencoder: &mut (impl ?Sized + Reencode), memory: u32) -> u32 {
        memory
    }

    pub fn mem_arg(
        reencoder: &mut (impl ?Sized + Reencode),
        arg: wasmparser::MemArg,
    ) -> crate::MemArg {
        crate::MemArg {
            offset: arg.offset,
            align: arg.align.into(),
            memory_index: reencoder.memory_index(arg.memory),
        }
    }

    pub fn ordering(
        _reencoder: &mut (impl ?Sized + Reencode),
        arg: wasmparser::Ordering,
    ) -> crate::Ordering {
        match arg {
            wasmparser::Ordering::SeqCst => crate::Ordering::SeqCst,
            wasmparser::Ordering::AcqRel => crate::Ordering::AcqRel,
        }
    }

    pub fn function_index(_reencoder: &mut (impl ?Sized + Reencode), func: u32) -> u32 {
        func
    }

    pub fn tag_index(_reencoder: &mut (impl ?Sized + Reencode), tag: u32) -> u32 {
        tag
    }

    pub fn label_index(_reencoder: &mut (impl ?Sized + Reencode), label: u32) -> u32 {
        label
    }

    pub fn catch(reencoder: &mut (impl ?Sized + Reencode), arg: wasmparser::Catch) -> crate::Catch {
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
        _reencoder: &mut (impl ?Sized + Reencode),
        section: wasmparser::CustomSectionReader<'a>,
    ) -> crate::CustomSection<'a> {
        crate::CustomSection {
            data: section.data().into(),
            name: section.name().into(),
        }
    }

    pub fn export_kind(
        _reencoder: &mut (impl ?Sized + Reencode),
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
        _reencoder: &mut (impl ?Sized + Reencode),
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
        _reencoder: &mut (impl ?Sized + Reencode),
        kind: wasmparser::TagKind,
    ) -> crate::TagKind {
        match kind {
            wasmparser::TagKind::Exception => crate::TagKind::Exception,
        }
    }

    pub fn type_index(_reencoder: &mut (impl ?Sized + Reencode), ty: u32) -> u32 {
        ty
    }

    pub fn tag_type(
        reencoder: &mut (impl ?Sized + Reencode),
        tag_ty: wasmparser::TagType,
    ) -> crate::TagType {
        crate::TagType {
            kind: reencoder.tag_kind(tag_ty.kind),
            func_type_idx: reencoder.type_index(tag_ty.func_type_idx),
        }
    }

    pub fn abstract_heap_type(
        _reencoder: &mut (impl ?Sized + Reencode),
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

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the types to the `types` section.
    pub fn parse_type_section(
        reencoder: &mut (impl ?Sized + Reencode),
        types: &mut crate::TypeSection,
        section: wasmparser::TypeSectionReader<'_>,
    ) -> Result<()> {
        for rec_group in section {
            reencoder.parse_recursive_type_group(types, rec_group?)?;
        }
        Ok(())
    }

    /// Parses a single [`wasmparser::RecGroup`] and adds it to the `types` section.
    pub fn parse_recursive_type_group(
        reencoder: &mut (impl ?Sized + Reencode),
        types: &mut crate::TypeSection,
        rec_group: wasmparser::RecGroup,
    ) -> Result<()> {
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
        Ok(())
    }

    pub fn sub_type(
        reencoder: &mut (impl ?Sized + Reencode),
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
        reencoder: &mut (impl ?Sized + Reencode),
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
        reencoder: &mut (impl ?Sized + Reencode),
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
        reencoder: &mut (impl ?Sized + Reencode),
        array_ty: wasmparser::ArrayType,
    ) -> Result<crate::ArrayType> {
        Ok(crate::ArrayType(reencoder.field_type(array_ty.0)?))
    }

    pub fn struct_type(
        reencoder: &mut (impl ?Sized + Reencode),
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
        reencoder: &mut (impl ?Sized + Reencode),
        field_ty: wasmparser::FieldType,
    ) -> Result<crate::FieldType> {
        Ok(crate::FieldType {
            element_type: reencoder.storage_type(field_ty.element_type)?,
            mutable: field_ty.mutable,
        })
    }

    pub fn storage_type(
        reencoder: &mut (impl ?Sized + Reencode),
        storage_ty: wasmparser::StorageType,
    ) -> Result<crate::StorageType> {
        Ok(match storage_ty {
            wasmparser::StorageType::I8 => crate::StorageType::I8,
            wasmparser::StorageType::I16 => crate::StorageType::I16,
            wasmparser::StorageType::Val(v) => crate::StorageType::Val(reencoder.val_type(v)?),
        })
    }

    pub fn val_type(
        reencoder: &mut (impl ?Sized + Reencode),
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
        reencoder: &mut (impl ?Sized + Reencode),
        ref_type: wasmparser::RefType,
    ) -> Result<crate::RefType> {
        Ok(crate::RefType {
            nullable: ref_type.is_nullable(),
            heap_type: reencoder.heap_type(ref_type.heap_type())?,
        })
    }

    pub fn heap_type(
        reencoder: &mut (impl ?Sized + Reencode),
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

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the tables to the `tables` section.
    pub fn parse_table_section(
        reencoder: &mut (impl ?Sized + Reencode),
        tables: &mut crate::TableSection,
        section: wasmparser::TableSectionReader<'_>,
    ) -> Result<()> {
        for table in section {
            reencoder.parse_table(tables, table?)?;
        }
        Ok(())
    }

    /// Parses a single [`wasmparser::Table`] and adds it to the `tables` section.
    pub fn parse_table(
        reencoder: &mut (impl ?Sized + Reencode),
        tables: &mut crate::TableSection,
        table: wasmparser::Table<'_>,
    ) -> Result<()> {
        let ty = reencoder.table_type(table.ty)?;
        match table.init {
            wasmparser::TableInit::RefNull => {
                tables.table(ty);
            }
            wasmparser::TableInit::Expr(e) => {
                tables.table_with_init(ty, &reencoder.const_expr(e)?);
            }
        }
        Ok(())
    }

    pub fn table_type(
        reencoder: &mut (impl ?Sized + Reencode),
        table_ty: wasmparser::TableType,
    ) -> Result<crate::TableType> {
        Ok(crate::TableType {
            element_type: reencoder.ref_type(table_ty.element_type)?,
            minimum: table_ty.initial,
            maximum: table_ty.maximum,
            table64: table_ty.table64,
        })
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the tags to the `tags` section.
    pub fn parse_tag_section(
        reencoder: &mut (impl ?Sized + Reencode),
        tags: &mut crate::TagSection,
        section: wasmparser::TagSectionReader<'_>,
    ) -> Result<()> {
        for tag in section {
            let tag = tag?;
            tags.tag(reencoder.tag_type(tag));
        }
        Ok(())
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the exports to the `exports` section.
    pub fn parse_export_section(
        reencoder: &mut (impl ?Sized + Reencode),
        exports: &mut crate::ExportSection,
        section: wasmparser::ExportSectionReader<'_>,
    ) -> Result<()> {
        for export in section {
            reencoder.parse_export(exports, export?);
        }
        Ok(())
    }

    pub fn export_index(_reencoder: &mut (impl ?Sized + Reencode), export: u32) -> u32 {
        export
    }

    /// Parses the single [`wasmparser::Export`] provided and adds it to the
    /// `exports` section.
    pub fn parse_export(
        reencoder: &mut (impl ?Sized + Reencode),
        exports: &mut crate::ExportSection,
        export: wasmparser::Export<'_>,
    ) {
        exports.export(
            export.name,
            reencoder.export_kind(export.kind),
            reencoder.export_index(export.index),
        );
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the globals to the `globals` section.
    pub fn parse_global_section(
        reencoder: &mut (impl ?Sized + Reencode),
        globals: &mut crate::GlobalSection,
        section: wasmparser::GlobalSectionReader<'_>,
    ) -> Result<()> {
        for global in section {
            reencoder.parse_global(globals, global?)?;
        }
        Ok(())
    }

    /// Parses the single [`wasmparser::Global`] provided and adds it to the
    /// `globals` section.
    pub fn parse_global(
        reencoder: &mut (impl ?Sized + Reencode),
        globals: &mut crate::GlobalSection,
        global: wasmparser::Global<'_>,
    ) -> Result<()> {
        globals.global(
            reencoder.global_type(global.ty)?,
            &reencoder.const_expr(global.init_expr)?,
        );
        Ok(())
    }

    pub fn global_type(
        reencoder: &mut (impl ?Sized + Reencode),
        global_ty: wasmparser::GlobalType,
    ) -> Result<crate::GlobalType> {
        Ok(crate::GlobalType {
            val_type: reencoder.val_type(global_ty.content_type)?,
            mutable: global_ty.mutable,
            shared: global_ty.shared,
        })
    }

    pub fn entity_type(
        reencoder: &mut (impl ?Sized + Reencode),
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

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the imports to the `import` section.
    pub fn parse_import_section(
        reencoder: &mut (impl ?Sized + Reencode),
        imports: &mut crate::ImportSection,
        section: wasmparser::ImportSectionReader<'_>,
    ) -> Result<()> {
        for import in section {
            reencoder.parse_import(imports, import?)?;
        }
        Ok(())
    }

    /// Parses the single [`wasmparser::Import`] provided and adds it to the
    /// `import` section.
    pub fn parse_import(
        reencoder: &mut (impl ?Sized + Reencode),
        imports: &mut crate::ImportSection,
        import: wasmparser::Import<'_>,
    ) -> Result<()> {
        imports.import(
            import.module,
            import.name,
            reencoder.entity_type(import.ty)?,
        );
        Ok(())
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the memories to the `memories` section.
    pub fn parse_memory_section(
        reencoder: &mut (impl ?Sized + Reencode),
        memories: &mut crate::MemorySection,
        section: wasmparser::MemorySectionReader<'_>,
    ) -> Result<()> {
        for memory in section {
            let memory = memory?;
            memories.memory(reencoder.memory_type(memory));
        }
        Ok(())
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the functions to the `functions` section.
    pub fn parse_function_section(
        reencoder: &mut (impl ?Sized + Reencode),
        functions: &mut crate::FunctionSection,
        section: wasmparser::FunctionSectionReader<'_>,
    ) -> Result<()> {
        for func in section {
            functions.function(reencoder.type_index(func?));
        }
        Ok(())
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the data to the `data` section.
    pub fn parse_data_section(
        reencoder: &mut (impl ?Sized + Reencode),
        data: &mut crate::DataSection,
        section: wasmparser::DataSectionReader<'_>,
    ) -> Result<()> {
        for datum in section {
            reencoder.parse_data(data, datum?)?;
        }
        Ok(())
    }

    /// Parses a single [`wasmparser::Data`] and adds it to the `data` section.
    pub fn parse_data(
        reencoder: &mut (impl ?Sized + Reencode),
        data: &mut crate::DataSection,
        datum: wasmparser::Data<'_>,
    ) -> Result<()> {
        match datum.kind {
            wasmparser::DataKind::Active {
                memory_index,
                offset_expr,
            } => data.active(
                reencoder.memory_index(memory_index),
                &reencoder.const_expr(offset_expr)?,
                datum.data.iter().copied(),
            ),
            wasmparser::DataKind::Passive => data.passive(datum.data.iter().copied()),
        };
        Ok(())
    }

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the elements to the `element` section.
    pub fn parse_element_section(
        reencoder: &mut (impl ?Sized + Reencode),
        elements: &mut crate::ElementSection,
        section: wasmparser::ElementSectionReader<'_>,
    ) -> Result<()> {
        for element in section {
            reencoder.parse_element(elements, element?)?;
        }
        Ok(())
    }

    /// Parses the single [`wasmparser::Element`] provided and adds it to the
    /// `element` section.
    pub fn parse_element(
        reencoder: &mut (impl ?Sized + Reencode),
        elements: &mut crate::ElementSection,
        element: wasmparser::Element<'_>,
    ) -> Result<()> {
        let mut funcs;
        let mut exprs;
        let elems = match element.items {
            wasmparser::ElementItems::Functions(f) => {
                funcs = Vec::new();
                for func in f {
                    funcs.push(reencoder.function_index(func?));
                }
                crate::Elements::Functions(&funcs)
            }
            wasmparser::ElementItems::Expressions(ty, e) => {
                exprs = Vec::new();
                for expr in e {
                    exprs.push(reencoder.const_expr(expr?)?);
                }
                crate::Elements::Expressions(reencoder.ref_type(ty)?, &exprs)
            }
        };
        match element.kind {
            wasmparser::ElementKind::Active {
                table_index,
                offset_expr,
            } => elements.active(
                table_index.map(|t| reencoder.table_index(t)),
                &reencoder.const_expr(offset_expr)?,
                elems,
            ),
            wasmparser::ElementKind::Passive => elements.passive(elems),
            wasmparser::ElementKind::Declared => elements.declared(elems),
        };
        Ok(())
    }

    pub fn table_index(_reencoder: &mut (impl ?Sized + Reencode), table: u32) -> u32 {
        table
    }

    pub fn global_index(_reencoder: &mut (impl ?Sized + Reencode), global: u32) -> u32 {
        global
    }

    pub fn data_index(_reencoder: &mut (impl ?Sized + Reencode), data: u32) -> u32 {
        data
    }

    pub fn element_index(_reencoder: &mut (impl ?Sized + Reencode), element: u32) -> u32 {
        element
    }

    pub fn const_expr(
        reencoder: &mut (impl ?Sized + Reencode),
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
        reencoder: &mut (impl ?Sized + Reencode),
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
        reencoder: &mut (impl ?Sized + Reencode),
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
                    .collect::<Result<Vec<_>, wasmparser::BinaryReaderError>>()?
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

    /// Parses the input `section` given from the `wasmparser` crate and adds
    /// all the code to the `code` section.
    pub fn parse_code_section(
        reencoder: &mut (impl ?Sized + Reencode),
        code: &mut crate::CodeSection,
        section: wasmparser::CodeSectionReader<'_>,
    ) -> Result<()> {
        for func in section {
            reencoder.parse_function_body(code, func?)?;
        }
        Ok(())
    }

    /// Parses a single [`wasmparser::FunctionBody`] and adds it to the `code` section.
    pub fn parse_function_body(
        reencoder: &mut (impl ?Sized + Reencode),
        code: &mut crate::CodeSection,
        func: wasmparser::FunctionBody<'_>,
    ) -> Result<()> {
        let mut f = reencoder.new_function_with_parsed_locals(&func)?;
        let mut reader = func.get_operators_reader()?;
        while !reader.eof() {
            reencoder.parse_instruction(&mut f, &mut reader)?;
        }
        code.function(&f);
        Ok(())
    }

    /// Create a new [`crate::Function`] by parsing the locals declarations from the
    /// provided [`wasmparser::FunctionBody`].
    pub fn new_function_with_parsed_locals(
        reencoder: &mut (impl ?Sized + Reencode),
        func: &wasmparser::FunctionBody<'_>,
    ) -> Result<crate::Function> {
        let mut locals = Vec::new();
        for pair in func.get_locals_reader()? {
            let (cnt, ty) = pair?;
            locals.push((cnt, reencoder.val_type(ty)?));
        }
        Ok(crate::Function::new(locals))
    }

    /// Parses a single instruction from `reader` and adds it to `function`.
    pub fn parse_instruction(
        reencoder: &mut (impl ?Sized + Reencode),
        function: &mut crate::Function,
        reader: &mut wasmparser::OperatorsReader<'_>,
    ) -> Result<()> {
        function.instruction(&reencoder.instruction(reader.read()?)?);
        Ok(())
    }
}

impl From<wasmparser::PrimitiveValType> for crate::PrimitiveValType {
    fn from(ty: wasmparser::PrimitiveValType) -> Self {
        RoundtripReencoder.component_primitive_val_type(ty)
    }
}

impl From<wasmparser::MemArg> for crate::MemArg {
    fn from(arg: wasmparser::MemArg) -> Self {
        RoundtripReencoder.mem_arg(arg)
    }
}

impl From<wasmparser::Ordering> for crate::Ordering {
    fn from(arg: wasmparser::Ordering) -> Self {
        RoundtripReencoder.ordering(arg)
    }
}

impl TryFrom<wasmparser::BlockType> for crate::BlockType {
    type Error = Error;

    fn try_from(arg: wasmparser::BlockType) -> Result<Self, Self::Error> {
        RoundtripReencoder.block_type(arg)
    }
}

impl<'a> TryFrom<wasmparser::Operator<'a>> for crate::Instruction<'a> {
    type Error = Error;

    fn try_from(arg: wasmparser::Operator<'a>) -> Result<Self, Self::Error> {
        RoundtripReencoder.instruction(arg)
    }
}

impl From<wasmparser::Catch> for crate::Catch {
    fn from(arg: wasmparser::Catch) -> Self {
        RoundtripReencoder.catch(arg)
    }
}

impl<'a> TryFrom<wasmparser::ConstExpr<'a>> for crate::ConstExpr {
    type Error = Error;

    fn try_from(const_expr: wasmparser::ConstExpr) -> Result<Self, Self::Error> {
        RoundtripReencoder.const_expr(const_expr)
    }
}

impl<'a> From<wasmparser::CustomSectionReader<'a>> for crate::CustomSection<'a> {
    fn from(section: wasmparser::CustomSectionReader<'a>) -> Self {
        RoundtripReencoder.custom_section(section)
    }
}

impl From<wasmparser::ExternalKind> for crate::ExportKind {
    fn from(external_kind: wasmparser::ExternalKind) -> Self {
        RoundtripReencoder.export_kind(external_kind)
    }
}

impl TryFrom<wasmparser::GlobalType> for crate::GlobalType {
    type Error = Error;

    fn try_from(global_ty: wasmparser::GlobalType) -> Result<Self, Self::Error> {
        RoundtripReencoder.global_type(global_ty)
    }
}

impl TryFrom<wasmparser::TypeRef> for crate::EntityType {
    type Error = Error;

    fn try_from(type_ref: wasmparser::TypeRef) -> Result<Self, Self::Error> {
        RoundtripReencoder.entity_type(type_ref)
    }
}

impl From<wasmparser::MemoryType> for crate::MemoryType {
    fn from(memory_ty: wasmparser::MemoryType) -> Self {
        RoundtripReencoder.memory_type(memory_ty)
    }
}

impl TryFrom<wasmparser::TableType> for crate::TableType {
    type Error = Error;

    fn try_from(table_ty: wasmparser::TableType) -> Result<Self, Self::Error> {
        RoundtripReencoder.table_type(table_ty)
    }
}

impl From<wasmparser::TagKind> for crate::TagKind {
    fn from(kind: wasmparser::TagKind) -> Self {
        RoundtripReencoder.tag_kind(kind)
    }
}

impl From<wasmparser::TagType> for crate::TagType {
    fn from(tag_ty: wasmparser::TagType) -> Self {
        RoundtripReencoder.tag_type(tag_ty)
    }
}

impl TryFrom<wasmparser::SubType> for crate::SubType {
    type Error = Error;

    fn try_from(sub_ty: wasmparser::SubType) -> Result<Self, Self::Error> {
        RoundtripReencoder.sub_type(sub_ty)
    }
}

impl TryFrom<wasmparser::CompositeType> for crate::CompositeType {
    type Error = Error;

    fn try_from(composite_ty: wasmparser::CompositeType) -> Result<Self, Self::Error> {
        RoundtripReencoder.composite_type(composite_ty)
    }
}

impl TryFrom<wasmparser::FuncType> for crate::FuncType {
    type Error = Error;

    fn try_from(func_ty: wasmparser::FuncType) -> Result<Self, Self::Error> {
        RoundtripReencoder.func_type(func_ty)
    }
}

impl TryFrom<wasmparser::ArrayType> for crate::ArrayType {
    type Error = Error;

    fn try_from(array_ty: wasmparser::ArrayType) -> Result<Self, Self::Error> {
        RoundtripReencoder.array_type(array_ty)
    }
}

impl TryFrom<wasmparser::StructType> for crate::StructType {
    type Error = Error;

    fn try_from(struct_ty: wasmparser::StructType) -> Result<Self, Self::Error> {
        RoundtripReencoder.struct_type(struct_ty)
    }
}

impl TryFrom<wasmparser::FieldType> for crate::FieldType {
    type Error = Error;

    fn try_from(field_ty: wasmparser::FieldType) -> Result<Self, Self::Error> {
        RoundtripReencoder.field_type(field_ty)
    }
}

impl TryFrom<wasmparser::StorageType> for crate::StorageType {
    type Error = Error;

    fn try_from(storage_ty: wasmparser::StorageType) -> Result<Self, Self::Error> {
        RoundtripReencoder.storage_type(storage_ty)
    }
}

impl TryFrom<wasmparser::ValType> for crate::ValType {
    type Error = Error;

    fn try_from(val_ty: wasmparser::ValType) -> Result<Self, Self::Error> {
        RoundtripReencoder.val_type(val_ty)
    }
}

impl TryFrom<wasmparser::RefType> for crate::RefType {
    type Error = Error;

    fn try_from(ref_type: wasmparser::RefType) -> Result<Self, Self::Error> {
        RoundtripReencoder.ref_type(ref_type)
    }
}

impl TryFrom<wasmparser::HeapType> for crate::HeapType {
    type Error = Error;

    fn try_from(heap_type: wasmparser::HeapType) -> Result<Self, Self::Error> {
        crate::reencode::utils::heap_type(&mut crate::reencode::RoundtripReencoder, heap_type)
    }
}

impl From<wasmparser::AbstractHeapType> for crate::AbstractHeapType {
    fn from(value: wasmparser::AbstractHeapType) -> Self {
        RoundtripReencoder.abstract_heap_type(value)
    }
}
