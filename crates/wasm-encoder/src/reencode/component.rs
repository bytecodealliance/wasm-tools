use crate::reencode::{Error, Reencode, RoundtripReencoder};

#[allow(missing_docs)] // FIXME
pub trait ReencodeComponent: Reencode {
    fn component_type_index(&mut self, ty: u32) -> u32 {
        ty
    }

    fn component_instance_index(&mut self, ty: u32) -> u32 {
        ty
    }

    fn component_func_index(&mut self, ty: u32) -> u32 {
        ty
    }

    fn component_index(&mut self, ty: u32) -> u32 {
        ty
    }

    fn module_index(&mut self, ty: u32) -> u32 {
        ty
    }

    fn instance_index(&mut self, ty: u32) -> u32 {
        ty
    }

    fn component_value_index(&mut self, ty: u32) -> u32 {
        ty
    }

    fn component_external_index(
        &mut self,
        kind: wasmparser::ComponentExternalKind,
        index: u32,
    ) -> u32 {
        match kind {
            wasmparser::ComponentExternalKind::Func => self.component_func_index(index),
            wasmparser::ComponentExternalKind::Module => self.module_index(index),
            wasmparser::ComponentExternalKind::Component => self.component_index(index),
            wasmparser::ComponentExternalKind::Type => self.component_type_index(index),
            wasmparser::ComponentExternalKind::Instance => self.component_instance_index(index),
            wasmparser::ComponentExternalKind::Value => self.component_value_index(index),
        }
    }

    fn parse_component(
        &mut self,
        component: &mut crate::Component,
        parser: wasmparser::Parser,
        data: &[u8],
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component(self, component, parser, data, data)
    }

    fn parse_component_payload(
        &mut self,
        component: &mut crate::Component,
        payload: wasmparser::Payload<'_>,
        whole_component: &[u8],
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_payload(self, component, payload, whole_component)
    }

    fn parse_component_submodule(
        &mut self,
        component: &mut crate::Component,
        parser: wasmparser::Parser,
        module: &[u8],
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_submodule(self, component, parser, module)
    }

    fn parse_component_subcomponent(
        &mut self,
        component: &mut crate::Component,
        parser: wasmparser::Parser,
        subcomponent: &[u8],
        whole_component: &[u8],
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_subcomponent(
            self,
            component,
            parser,
            subcomponent,
            whole_component,
        )
    }

    fn parse_unknown_component_section(
        &mut self,
        component: &mut crate::Component,
        id: u8,
        contents: &[u8],
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_unknown_component_section(self, component, id, contents)
    }

    fn parse_component_custom_section(
        &mut self,
        component: &mut crate::Component,
        section: wasmparser::CustomSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_custom_section(self, component, section)
    }

    fn parse_component_type_section(
        &mut self,
        types: &mut crate::ComponentTypeSection,
        section: wasmparser::ComponentTypeSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_type_section(self, types, section)
    }

    fn parse_component_type(
        &mut self,
        dst: crate::ComponentTypeEncoder<'_>,
        ty: wasmparser::ComponentType<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_type(self, dst, ty)
    }

    fn component_instance_type(
        &mut self,
        ty: Box<[wasmparser::InstanceTypeDeclaration<'_>]>,
    ) -> Result<crate::InstanceType, Error<Self::Error>> {
        component_utils::component_instance_type(self, ty)
    }

    fn parse_component_instance_type_declaration(
        &mut self,
        ty: &mut crate::InstanceType,
        decl: wasmparser::InstanceTypeDeclaration<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_instance_type_declaration(self, ty, decl)
    }

    fn parse_component_core_type(
        &mut self,
        ty: crate::CoreTypeEncoder<'_>,
        core: wasmparser::CoreType<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_core_type(self, ty, core)
    }

    fn component_type(
        &mut self,
        ty: Box<[wasmparser::ComponentTypeDeclaration<'_>]>,
    ) -> Result<crate::ComponentType, Error<Self::Error>> {
        component_utils::component_type(self, ty)
    }

    fn parse_component_type_declaration(
        &mut self,
        component: &mut crate::ComponentType,
        decl: wasmparser::ComponentTypeDeclaration<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_type_declaration(self, component, decl)
    }

    fn parse_component_func_type(
        &mut self,
        func: crate::ComponentFuncTypeEncoder<'_>,
        ty: wasmparser::ComponentFuncType<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_func_type(self, func, ty)
    }

    fn parse_component_defined_type(
        &mut self,
        defined: crate::ComponentDefinedTypeEncoder<'_>,
        ty: wasmparser::ComponentDefinedType<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_defined_type(self, defined, ty)
    }

    fn component_module_type(
        &mut self,
        ty: Box<[wasmparser::ModuleTypeDeclaration<'_>]>,
    ) -> Result<crate::ModuleType, Error<Self::Error>> {
        component_utils::component_module_type(self, ty)
    }

    fn parse_component_module_type_declaration(
        &mut self,
        module: &mut crate::ModuleType,
        decl: wasmparser::ModuleTypeDeclaration<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_module_type_declaration(self, module, decl)
    }

    fn component_alias<'a>(
        &mut self,
        alias: wasmparser::ComponentAlias<'a>,
    ) -> Result<crate::Alias<'a>, Error<Self::Error>> {
        component_utils::component_alias(self, alias)
    }

    fn parse_component_sub_type(
        &mut self,
        ty: crate::CoreTypeEncoder<'_>,
        sub: wasmparser::SubType,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_sub_type(self, ty, sub)
    }

    fn parse_component_import_section(
        &mut self,
        imports: &mut crate::ComponentImportSection,
        section: wasmparser::ComponentImportSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_import_section(self, imports, section)
    }

    fn parse_component_canonical_section(
        &mut self,
        canonical: &mut crate::CanonicalFunctionSection,
        section: wasmparser::ComponentCanonicalSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_canonical_section(self, canonical, section)
    }

    fn parse_component_canonical(
        &mut self,
        section: &mut crate::CanonicalFunctionSection,
        func: wasmparser::CanonicalFunction,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_canonical(self, section, func)
    }

    fn parse_component_alias_section(
        &mut self,
        aliases: &mut crate::ComponentAliasSection,
        section: wasmparser::ComponentAliasSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_alias_section(self, aliases, section)
    }

    fn parse_component_instance_section(
        &mut self,
        instances: &mut crate::ComponentInstanceSection,
        section: wasmparser::ComponentInstanceSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_instance_section(self, instances, section)
    }

    fn parse_component_instance(
        &mut self,
        instances: &mut crate::ComponentInstanceSection,
        instance: wasmparser::ComponentInstance<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_instance(self, instances, instance)
    }

    fn parse_instance_section(
        &mut self,
        instances: &mut crate::InstanceSection,
        section: wasmparser::InstanceSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_instance_section(self, instances, section)
    }

    fn parse_instance(
        &mut self,
        instances: &mut crate::InstanceSection,
        instance: wasmparser::Instance<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_instance(self, instances, instance)
    }

    fn parse_core_type_section(
        &mut self,
        types: &mut crate::CoreTypeSection,
        section: wasmparser::CoreTypeSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_core_type_section(self, types, section)
    }

    fn parse_component_export_section(
        &mut self,
        exports: &mut crate::ComponentExportSection,
        section: wasmparser::ComponentExportSectionReader<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_export_section(self, exports, section)
    }

    fn parse_component_export(
        &mut self,
        exports: &mut crate::ComponentExportSection,
        export: wasmparser::ComponentExport<'_>,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_export(self, exports, export)
    }

    fn parse_component_start_section(
        &mut self,
        component: &mut crate::Component,
        func: wasmparser::ComponentStartFunction,
    ) -> Result<(), Error<Self::Error>> {
        component_utils::parse_component_start_section(self, component, func)
    }

    fn component_type_ref(
        &mut self,
        ty: wasmparser::ComponentTypeRef,
    ) -> crate::component::ComponentTypeRef {
        component_utils::component_type_ref(self, ty)
    }

    fn component_primitive_val_type(
        &mut self,
        ty: wasmparser::PrimitiveValType,
    ) -> crate::component::PrimitiveValType {
        component_utils::component_primitive_val_type(self, ty)
    }

    fn component_export_kind(
        &mut self,
        ty: wasmparser::ComponentExternalKind,
    ) -> crate::component::ComponentExportKind {
        component_utils::component_export_kind(self, ty)
    }

    fn component_outer_alias_kind(
        &mut self,
        kind: wasmparser::ComponentOuterAliasKind,
    ) -> crate::component::ComponentOuterAliasKind {
        component_utils::component_outer_alias_kind(self, kind)
    }

    fn component_val_type(
        &mut self,
        ty: wasmparser::ComponentValType,
    ) -> crate::component::ComponentValType {
        component_utils::component_val_type(self, ty)
    }

    fn type_bounds(&mut self, ty: wasmparser::TypeBounds) -> crate::component::TypeBounds {
        component_utils::type_bounds(self, ty)
    }

    fn canonical_option(
        &mut self,
        ty: wasmparser::CanonicalOption,
    ) -> crate::component::CanonicalOption {
        component_utils::canonical_option(self, ty)
    }
}

impl ReencodeComponent for RoundtripReencoder {}

#[allow(missing_docs)] // FIXME
pub mod component_utils {
    use super::ReencodeComponent;
    use crate::reencode::Error;

    pub fn parse_component<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        component: &mut crate::Component,
        mut parser: wasmparser::Parser,
        data: &[u8],
        whole_component: &[u8],
    ) -> Result<(), Error<T::Error>> {
        let mut remaining = data;
        while !remaining.is_empty() {
            let section = match parser.parse(remaining, true)? {
                wasmparser::Chunk::Parsed { consumed, payload } => {
                    remaining = &remaining[consumed..];
                    payload
                }
                wasmparser::Chunk::NeedMoreData(_) => unreachable!(),
            };
            match &section {
                wasmparser::Payload::ComponentSection {
                    unchecked_range, ..
                }
                | wasmparser::Payload::ModuleSection {
                    unchecked_range, ..
                } => {
                    remaining = &remaining[unchecked_range.len()..];
                }
                _ => {}
            }
            reencoder.parse_component_payload(component, section, whole_component)?;
        }

        Ok(())
    }

    pub fn parse_component_payload<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        component: &mut crate::Component,
        payload: wasmparser::Payload<'_>,
        whole_component: &[u8],
    ) -> Result<(), Error<T::Error>> {
        match payload {
            wasmparser::Payload::Version {
                encoding: wasmparser::Encoding::Component,
                ..
            } => (),
            wasmparser::Payload::Version { .. } => {
                return Err(Error::UnexpectedNonComponentSection)
            }
            wasmparser::Payload::TypeSection(_)
            | wasmparser::Payload::ImportSection(_)
            | wasmparser::Payload::FunctionSection(_)
            | wasmparser::Payload::TableSection(_)
            | wasmparser::Payload::MemorySection(_)
            | wasmparser::Payload::TagSection(_)
            | wasmparser::Payload::GlobalSection(_)
            | wasmparser::Payload::ExportSection(_)
            | wasmparser::Payload::StartSection { .. }
            | wasmparser::Payload::ElementSection(_)
            | wasmparser::Payload::DataCountSection { .. }
            | wasmparser::Payload::DataSection(_)
            | wasmparser::Payload::CodeSectionStart { .. }
            | wasmparser::Payload::CodeSectionEntry(_) => {
                return Err(Error::UnexpectedNonComponentSection)
            }
            wasmparser::Payload::ComponentTypeSection(section) => {
                let mut types = crate::ComponentTypeSection::new();
                reencoder.parse_component_type_section(&mut types, section)?;
                component.section(&types);
            }
            wasmparser::Payload::ComponentImportSection(section) => {
                let mut imports = crate::ComponentImportSection::new();
                reencoder.parse_component_import_section(&mut imports, section)?;
                component.section(&imports);
            }
            wasmparser::Payload::ComponentCanonicalSection(section) => {
                let mut canonical = crate::CanonicalFunctionSection::new();
                reencoder.parse_component_canonical_section(&mut canonical, section)?;
                component.section(&canonical);
            }
            wasmparser::Payload::ComponentAliasSection(section) => {
                let mut aliases = crate::ComponentAliasSection::new();
                reencoder.parse_component_alias_section(&mut aliases, section)?;
                component.section(&aliases);
            }
            wasmparser::Payload::ComponentInstanceSection(section) => {
                let mut instances = crate::ComponentInstanceSection::new();
                reencoder.parse_component_instance_section(&mut instances, section)?;
                component.section(&instances);
            }
            wasmparser::Payload::InstanceSection(section) => {
                let mut instances = crate::InstanceSection::new();
                reencoder.parse_instance_section(&mut instances, section)?;
                component.section(&instances);
            }
            wasmparser::Payload::CoreTypeSection(section) => {
                let mut types = crate::CoreTypeSection::new();
                reencoder.parse_core_type_section(&mut types, section)?;
                component.section(&types);
            }
            wasmparser::Payload::ComponentExportSection(section) => {
                let mut exports = crate::ComponentExportSection::new();
                reencoder.parse_component_export_section(&mut exports, section)?;
                component.section(&exports);
            }
            wasmparser::Payload::CustomSection(section) => {
                reencoder.parse_component_custom_section(component, section)?;
            }
            wasmparser::Payload::UnknownSection { id, contents, .. } => {
                reencoder.parse_unknown_component_section(component, id, contents)?;
            }
            wasmparser::Payload::ModuleSection {
                parser,
                unchecked_range,
            } => {
                reencoder.parse_component_submodule(
                    component,
                    parser,
                    &whole_component[unchecked_range],
                )?;
            }
            wasmparser::Payload::ComponentSection {
                parser,
                unchecked_range,
            } => {
                reencoder.parse_component_subcomponent(
                    component,
                    parser,
                    &whole_component[unchecked_range],
                    whole_component,
                )?;
            }
            wasmparser::Payload::ComponentStartSection { start, range: _ } => {
                reencoder.parse_component_start_section(component, start)?;
            }
            wasmparser::Payload::End(_) => {}
        }
        Ok(())
    }

    pub fn parse_component_submodule<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        component: &mut crate::Component,
        parser: wasmparser::Parser,
        submodule: &[u8],
    ) -> Result<(), Error<T::Error>> {
        let mut module = crate::Module::new();
        crate::reencode::utils::parse_core_module(reencoder, &mut module, parser, submodule)?;
        component.section(&crate::ModuleSection(&module));
        Ok(())
    }

    pub fn parse_component_subcomponent<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        component: &mut crate::Component,
        parser: wasmparser::Parser,
        data: &[u8],
        whole_component: &[u8],
    ) -> Result<(), Error<T::Error>> {
        let mut subcomponent = crate::Component::new();
        parse_component(reencoder, &mut subcomponent, parser, data, whole_component)?;
        component.section(&crate::NestedComponentSection(&subcomponent));
        Ok(())
    }

    pub fn parse_unknown_component_section<T: ?Sized + ReencodeComponent>(
        _reencoder: &mut T,
        component: &mut crate::Component,
        id: u8,
        contents: &[u8],
    ) -> Result<(), Error<T::Error>> {
        component.section(&crate::RawSection { id, data: contents });
        Ok(())
    }

    pub fn parse_component_custom_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        component: &mut crate::Component,
        section: wasmparser::CustomSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        component.section(&reencoder.custom_section(section));
        Ok(())
    }

    pub fn parse_component_type_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        types: &mut crate::ComponentTypeSection,
        section: wasmparser::ComponentTypeSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        for ty in section {
            reencoder.parse_component_type(types.ty(), ty?)?;
        }
        Ok(())
    }

    pub fn parse_component_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        dst: crate::ComponentTypeEncoder,
        ty: wasmparser::ComponentType<'_>,
    ) -> Result<(), Error<T::Error>> {
        match ty {
            wasmparser::ComponentType::Defined(ty) => {
                reencoder.parse_component_defined_type(dst.defined_type(), ty)?;
            }
            wasmparser::ComponentType::Func(func) => {
                reencoder.parse_component_func_type(dst.function(), func)?;
            }
            wasmparser::ComponentType::Component(component) => {
                let ty = reencoder.component_type(component)?;
                dst.component(&ty);
            }
            wasmparser::ComponentType::Instance(instance) => {
                let ty = reencoder.component_instance_type(instance)?;
                dst.instance(&ty);
            }
            wasmparser::ComponentType::Resource { rep, dtor } => {
                let rep = reencoder.val_type(rep)?;
                let dtor = dtor.map(|i| reencoder.function_index(i));
                dst.resource(rep, dtor);
            }
        }
        Ok(())
    }

    pub fn component_instance_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: Box<[wasmparser::InstanceTypeDeclaration<'_>]>,
    ) -> Result<crate::InstanceType, Error<T::Error>> {
        let mut ret = crate::InstanceType::new();
        for decl in ty {
            reencoder.parse_component_instance_type_declaration(&mut ret, decl)?;
        }
        Ok(ret)
    }

    pub fn parse_component_instance_type_declaration<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        instance: &mut crate::InstanceType,
        decl: wasmparser::InstanceTypeDeclaration<'_>,
    ) -> Result<(), Error<T::Error>> {
        match decl {
            wasmparser::InstanceTypeDeclaration::CoreType(core) => {
                reencoder.parse_component_core_type(instance.core_type(), core)
            }
            wasmparser::InstanceTypeDeclaration::Type(t) => {
                reencoder.parse_component_type(instance.ty(), t)
            }
            wasmparser::InstanceTypeDeclaration::Alias(a) => {
                let a = reencoder.component_alias(a)?;
                instance.alias(a);
                Ok(())
            }
            wasmparser::InstanceTypeDeclaration::Export { name, ty } => {
                let ty = reencoder.component_type_ref(ty);
                instance.export(name.0, ty);
                Ok(())
            }
        }
    }

    pub fn parse_component_core_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: crate::CoreTypeEncoder<'_>,
        decl: wasmparser::CoreType<'_>,
    ) -> Result<(), Error<T::Error>> {
        match decl {
            wasmparser::CoreType::Sub(core) => reencoder.parse_component_sub_type(ty, core)?,
            wasmparser::CoreType::Module(decls) => {
                ty.module(&reencoder.component_module_type(decls)?);
            }
        }
        Ok(())
    }

    pub fn component_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: Box<[wasmparser::ComponentTypeDeclaration<'_>]>,
    ) -> Result<crate::ComponentType, Error<T::Error>> {
        let mut ret = crate::ComponentType::new();
        for decl in ty {
            reencoder.parse_component_type_declaration(&mut ret, decl)?;
        }
        Ok(ret)
    }

    pub fn parse_component_type_declaration<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        component: &mut crate::ComponentType,
        decl: wasmparser::ComponentTypeDeclaration<'_>,
    ) -> Result<(), Error<T::Error>> {
        match decl {
            wasmparser::ComponentTypeDeclaration::CoreType(ty) => {
                reencoder.parse_component_core_type(component.core_type(), ty)
            }
            wasmparser::ComponentTypeDeclaration::Type(ty) => {
                reencoder.parse_component_type(component.ty(), ty)
            }
            wasmparser::ComponentTypeDeclaration::Alias(a) => {
                let a = reencoder.component_alias(a)?;
                component.alias(a);
                Ok(())
            }
            wasmparser::ComponentTypeDeclaration::Export { name, ty } => {
                let ty = reencoder.component_type_ref(ty);
                component.export(name.0, ty);
                Ok(())
            }
            wasmparser::ComponentTypeDeclaration::Import(import) => {
                let ty = reencoder.component_type_ref(import.ty);
                component.import(import.name.0, ty);
                Ok(())
            }
        }
    }

    pub fn parse_component_func_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        mut func: crate::ComponentFuncTypeEncoder<'_>,
        ty: wasmparser::ComponentFuncType<'_>,
    ) -> Result<(), Error<T::Error>> {
        func.params(
            Vec::from(ty.params)
                .into_iter()
                .map(|(name, ty)| (name, reencoder.component_val_type(ty))),
        );
        match ty.results {
            wasmparser::ComponentFuncResult::Unnamed(ty) => {
                func.result(reencoder.component_val_type(ty));
            }
            wasmparser::ComponentFuncResult::Named(list) => {
                func.results(
                    Vec::from(list)
                        .into_iter()
                        .map(|(name, ty)| (name, reencoder.component_val_type(ty))),
                );
            }
        }
        Ok(())
    }

    pub fn parse_component_defined_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        defined: crate::ComponentDefinedTypeEncoder<'_>,
        ty: wasmparser::ComponentDefinedType<'_>,
    ) -> Result<(), Error<T::Error>> {
        match ty {
            wasmparser::ComponentDefinedType::Primitive(p) => {
                defined.primitive(reencoder.component_primitive_val_type(p));
            }
            wasmparser::ComponentDefinedType::Record(r) => {
                defined.record(
                    r.iter()
                        .map(|(name, ty)| (*name, reencoder.component_val_type(*ty))),
                );
            }
            wasmparser::ComponentDefinedType::Variant(v) => {
                defined.variant(v.iter().map(|case| {
                    (
                        case.name,
                        case.ty.map(|t| reencoder.component_val_type(t)),
                        case.refines,
                    )
                }));
            }
            wasmparser::ComponentDefinedType::List(t) => {
                defined.list(reencoder.component_val_type(t));
            }
            wasmparser::ComponentDefinedType::Tuple(t) => {
                defined.tuple(t.iter().map(|t| reencoder.component_val_type(*t)));
            }
            wasmparser::ComponentDefinedType::Flags(t) => {
                defined.flags(t.iter().copied());
            }
            wasmparser::ComponentDefinedType::Enum(t) => {
                defined.enum_type(t.iter().copied());
            }
            wasmparser::ComponentDefinedType::Option(t) => {
                defined.option(reencoder.component_val_type(t));
            }
            wasmparser::ComponentDefinedType::Result { ok, err } => {
                let ok = ok.map(|t| reencoder.component_val_type(t));
                let err = err.map(|t| reencoder.component_val_type(t));
                defined.result(ok, err);
            }
            wasmparser::ComponentDefinedType::Own(i) => {
                defined.own(reencoder.component_type_index(i));
            }
            wasmparser::ComponentDefinedType::Borrow(i) => {
                defined.borrow(reencoder.component_type_index(i));
            }
        }
        Ok(())
    }

    pub fn component_module_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: Box<[wasmparser::ModuleTypeDeclaration<'_>]>,
    ) -> Result<crate::ModuleType, Error<T::Error>> {
        let mut ret = crate::ModuleType::new();
        for decl in ty {
            reencoder.parse_component_module_type_declaration(&mut ret, decl)?;
        }
        Ok(ret)
    }

    pub fn parse_component_module_type_declaration<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        module: &mut crate::ModuleType,
        decl: wasmparser::ModuleTypeDeclaration<'_>,
    ) -> Result<(), Error<T::Error>> {
        match decl {
            wasmparser::ModuleTypeDeclaration::Type(ty) => {
                reencoder.parse_component_sub_type(module.ty(), ty)?
            }
            wasmparser::ModuleTypeDeclaration::Export { name, ty } => {
                module.export(name, reencoder.entity_type(ty)?);
            }
            wasmparser::ModuleTypeDeclaration::OuterAlias {
                kind: wasmparser::OuterAliasKind::Type,
                count,
                index,
            } => {
                // TODO: how to inform `reencoder` that `index` is being used
                // at the particular depth?
                module.alias_outer_core_type(count, index);
            }
            wasmparser::ModuleTypeDeclaration::Import(import) => {
                module.import(
                    import.module,
                    import.name,
                    reencoder.entity_type(import.ty)?,
                );
            }
        }
        Ok(())
    }

    pub fn component_alias<'a, T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        alias: wasmparser::ComponentAlias<'a>,
    ) -> Result<crate::Alias<'a>, Error<T::Error>> {
        match alias {
            wasmparser::ComponentAlias::InstanceExport {
                kind,
                instance_index,
                name,
            } => Ok(crate::Alias::InstanceExport {
                instance: reencoder.component_instance_index(instance_index),
                kind: kind.into(),
                name,
            }),
            wasmparser::ComponentAlias::CoreInstanceExport {
                kind,
                instance_index,
                name,
            } => Ok(crate::Alias::CoreInstanceExport {
                instance: reencoder.instance_index(instance_index),
                kind: kind.into(),
                name,
            }),
            wasmparser::ComponentAlias::Outer { kind, count, index } => Ok(crate::Alias::Outer {
                kind: kind.into(),
                // TODO: how to inform `reencoder` of aliases
                count,
                index,
            }),
        }
    }

    pub fn parse_component_sub_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: crate::CoreTypeEncoder<'_>,
        sub: wasmparser::SubType,
    ) -> Result<(), Error<T::Error>> {
        if !sub.is_final || sub.supertype_idx.is_some() || sub.composite_type.shared {
            return Err(Error::UnsupportedCoreTypeInComponent);
        }

        let func = match sub.composite_type.inner {
            wasmparser::CompositeInnerType::Func(f) => f,
            _ => return Err(Error::UnsupportedCoreTypeInComponent),
        };

        ty.function(
            func.params()
                .iter()
                .map(|t| reencoder.val_type(*t))
                .collect::<Result<Vec<_>, _>>()?,
            func.results()
                .iter()
                .map(|t| reencoder.val_type(*t))
                .collect::<Result<Vec<_>, _>>()?,
        );

        Ok(())
    }

    pub fn parse_component_import_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        imports: &mut crate::ComponentImportSection,
        section: wasmparser::ComponentImportSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        for import in section {
            let import = import?;
            imports.import(import.name.0, reencoder.component_type_ref(import.ty));
        }
        Ok(())
    }

    pub fn parse_component_canonical_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        canonical: &mut crate::CanonicalFunctionSection,
        section: wasmparser::ComponentCanonicalSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        for c in section {
            reencoder.parse_component_canonical(canonical, c?)?;
        }
        Ok(())
    }

    pub fn parse_component_canonical<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        section: &mut crate::CanonicalFunctionSection,
        func: wasmparser::CanonicalFunction,
    ) -> Result<(), Error<T::Error>> {
        match func {
            wasmparser::CanonicalFunction::Lift {
                core_func_index,
                type_index,
                options,
            } => {
                let func = reencoder.function_index(core_func_index);
                let ty = reencoder.component_type_index(type_index);
                section.lift(
                    func,
                    ty,
                    options.iter().map(|o| reencoder.canonical_option(*o)),
                );
            }
            wasmparser::CanonicalFunction::Lower {
                func_index,
                options,
            } => {
                let func = reencoder.component_func_index(func_index);
                section.lower(func, options.iter().map(|o| reencoder.canonical_option(*o)));
            }
            wasmparser::CanonicalFunction::ResourceNew { resource } => {
                let resource = reencoder.component_type_index(resource);
                section.resource_new(resource);
            }
            wasmparser::CanonicalFunction::ResourceDrop { resource } => {
                let resource = reencoder.component_type_index(resource);
                section.resource_drop(resource);
            }
            wasmparser::CanonicalFunction::ResourceRep { resource } => {
                let resource = reencoder.component_type_index(resource);
                section.resource_rep(resource);
            }
        }
        Ok(())
    }

    pub fn parse_component_alias_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        aliases: &mut crate::ComponentAliasSection,
        section: wasmparser::ComponentAliasSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        for a in section {
            aliases.alias(reencoder.component_alias(a?)?);
        }
        Ok(())
    }

    pub fn parse_component_instance_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        instances: &mut crate::ComponentInstanceSection,
        section: wasmparser::ComponentInstanceSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        for i in section {
            reencoder.parse_component_instance(instances, i?)?;
        }
        Ok(())
    }

    pub fn parse_component_instance<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        instances: &mut crate::ComponentInstanceSection,
        instance: wasmparser::ComponentInstance<'_>,
    ) -> Result<(), Error<T::Error>> {
        match instance {
            wasmparser::ComponentInstance::Instantiate {
                component_index,
                args,
            } => {
                instances.instantiate(
                    reencoder.component_index(component_index),
                    args.iter().map(|arg| {
                        (
                            arg.name,
                            arg.kind.into(),
                            reencoder.component_external_index(arg.kind, arg.index),
                        )
                    }),
                );
            }
            wasmparser::ComponentInstance::FromExports(exports) => {
                instances.export_items(exports.iter().map(|export| {
                    (
                        export.name.0,
                        export.kind.into(),
                        reencoder.component_external_index(export.kind, export.index),
                    )
                }));
            }
        }
        Ok(())
    }

    pub fn parse_instance_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        instances: &mut crate::InstanceSection,
        section: wasmparser::InstanceSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        for i in section {
            reencoder.parse_instance(instances, i?)?;
        }
        Ok(())
    }

    pub fn parse_instance<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        instances: &mut crate::InstanceSection,
        instance: wasmparser::Instance<'_>,
    ) -> Result<(), Error<T::Error>> {
        match instance {
            wasmparser::Instance::Instantiate { module_index, args } => {
                instances.instantiate(
                    reencoder.module_index(module_index),
                    args.iter().map(|arg| match arg.kind {
                        wasmparser::InstantiationArgKind::Instance => (
                            arg.name,
                            crate::ModuleArg::Instance(reencoder.instance_index(arg.index)),
                        ),
                    }),
                );
            }
            wasmparser::Instance::FromExports(exports) => {
                instances.export_items(exports.iter().map(|export| {
                    (
                        export.name,
                        reencoder.export_kind(export.kind),
                        reencoder.external_index(export.kind, export.index),
                    )
                }));
            }
        }
        Ok(())
    }

    pub fn parse_core_type_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        types: &mut crate::CoreTypeSection,
        section: wasmparser::CoreTypeSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        for t in section {
            reencoder.parse_component_core_type(types.ty(), t?)?;
        }
        Ok(())
    }

    pub fn parse_component_export_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        exports: &mut crate::ComponentExportSection,
        section: wasmparser::ComponentExportSectionReader<'_>,
    ) -> Result<(), Error<T::Error>> {
        for e in section {
            reencoder.parse_component_export(exports, e?)?;
        }
        Ok(())
    }

    pub fn parse_component_export<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        exports: &mut crate::ComponentExportSection,
        export: wasmparser::ComponentExport<'_>,
    ) -> Result<(), Error<T::Error>> {
        exports.export(
            export.name.0,
            export.kind.into(),
            reencoder.component_external_index(export.kind, export.index),
            export.ty.map(|t| reencoder.component_type_ref(t)),
        );
        Ok(())
    }

    pub fn parse_component_start_section<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        component: &mut crate::Component,
        func: wasmparser::ComponentStartFunction,
    ) -> Result<(), Error<T::Error>> {
        component.section(&crate::ComponentStartSection {
            function_index: reencoder.component_func_index(func.func_index),
            args: func
                .arguments
                .iter()
                .map(|i| reencoder.component_value_index(*i))
                .collect::<Vec<_>>(),
            results: func.results,
        });
        Ok(())
    }

    pub fn component_type_ref<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: wasmparser::ComponentTypeRef,
    ) -> crate::component::ComponentTypeRef {
        match ty {
            wasmparser::ComponentTypeRef::Module(u) => {
                crate::component::ComponentTypeRef::Module(reencoder.component_type_index(u))
            }
            wasmparser::ComponentTypeRef::Func(u) => {
                crate::component::ComponentTypeRef::Func(reencoder.component_type_index(u))
            }
            wasmparser::ComponentTypeRef::Value(valty) => {
                crate::component::ComponentTypeRef::Value(reencoder.component_val_type(valty))
            }
            wasmparser::ComponentTypeRef::Type(bounds) => {
                crate::component::ComponentTypeRef::Type(reencoder.type_bounds(bounds))
            }
            wasmparser::ComponentTypeRef::Instance(u) => {
                crate::component::ComponentTypeRef::Instance(reencoder.component_type_index(u))
            }
            wasmparser::ComponentTypeRef::Component(u) => {
                crate::component::ComponentTypeRef::Component(reencoder.component_type_index(u))
            }
        }
    }

    pub fn component_primitive_val_type<T: ?Sized + ReencodeComponent>(
        _reencoder: &mut T,
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

    pub fn component_export_kind<T: ?Sized + ReencodeComponent>(
        _reencoder: &mut T,
        ty: wasmparser::ComponentExternalKind,
    ) -> crate::component::ComponentExportKind {
        match ty {
            wasmparser::ComponentExternalKind::Module => crate::ComponentExportKind::Module,
            wasmparser::ComponentExternalKind::Func => crate::ComponentExportKind::Func,
            wasmparser::ComponentExternalKind::Value => crate::ComponentExportKind::Value,
            wasmparser::ComponentExternalKind::Type => crate::ComponentExportKind::Type,
            wasmparser::ComponentExternalKind::Instance => crate::ComponentExportKind::Instance,
            wasmparser::ComponentExternalKind::Component => crate::ComponentExportKind::Component,
        }
    }

    pub fn component_outer_alias_kind<T: ?Sized + ReencodeComponent>(
        _reencoder: &mut T,
        ty: wasmparser::ComponentOuterAliasKind,
    ) -> crate::component::ComponentOuterAliasKind {
        match ty {
            wasmparser::ComponentOuterAliasKind::CoreModule => {
                crate::component::ComponentOuterAliasKind::CoreModule
            }
            wasmparser::ComponentOuterAliasKind::CoreType => {
                crate::component::ComponentOuterAliasKind::CoreType
            }
            wasmparser::ComponentOuterAliasKind::Type => {
                crate::component::ComponentOuterAliasKind::Type
            }
            wasmparser::ComponentOuterAliasKind::Component => {
                crate::ComponentOuterAliasKind::Component
            }
        }
    }

    pub fn component_val_type<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: wasmparser::ComponentValType,
    ) -> crate::component::ComponentValType {
        match ty {
            wasmparser::ComponentValType::Type(u) => {
                crate::component::ComponentValType::Type(reencoder.component_type_index(u))
            }
            wasmparser::ComponentValType::Primitive(pty) => {
                crate::component::ComponentValType::Primitive(
                    crate::component::PrimitiveValType::from(pty),
                )
            }
        }
    }

    pub fn type_bounds<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: wasmparser::TypeBounds,
    ) -> crate::component::TypeBounds {
        match ty {
            wasmparser::TypeBounds::Eq(u) => {
                crate::component::TypeBounds::Eq(reencoder.component_type_index(u))
            }
            wasmparser::TypeBounds::SubResource => crate::component::TypeBounds::SubResource,
        }
    }

    pub fn canonical_option<T: ?Sized + ReencodeComponent>(
        reencoder: &mut T,
        ty: wasmparser::CanonicalOption,
    ) -> crate::component::CanonicalOption {
        match ty {
            wasmparser::CanonicalOption::UTF8 => crate::component::CanonicalOption::UTF8,
            wasmparser::CanonicalOption::UTF16 => crate::component::CanonicalOption::UTF16,
            wasmparser::CanonicalOption::CompactUTF16 => {
                crate::component::CanonicalOption::CompactUTF16
            }
            wasmparser::CanonicalOption::Memory(u) => {
                crate::component::CanonicalOption::Memory(reencoder.memory_index(u))
            }
            wasmparser::CanonicalOption::Realloc(u) => {
                crate::component::CanonicalOption::Realloc(reencoder.function_index(u))
            }
            wasmparser::CanonicalOption::PostReturn(u) => {
                crate::component::CanonicalOption::PostReturn(reencoder.function_index(u))
            }
        }
    }
}

impl From<wasmparser::ComponentValType> for crate::ComponentValType {
    fn from(ty: wasmparser::ComponentValType) -> Self {
        RoundtripReencoder.component_val_type(ty)
    }
}

impl From<wasmparser::TypeBounds> for crate::TypeBounds {
    fn from(ty: wasmparser::TypeBounds) -> Self {
        RoundtripReencoder.type_bounds(ty)
    }
}

impl From<wasmparser::CanonicalOption> for crate::CanonicalOption {
    fn from(opt: wasmparser::CanonicalOption) -> Self {
        RoundtripReencoder.canonical_option(opt)
    }
}

impl From<wasmparser::ComponentExternalKind> for crate::ComponentExportKind {
    fn from(kind: wasmparser::ComponentExternalKind) -> Self {
        RoundtripReencoder.component_export_kind(kind)
    }
}

impl From<wasmparser::ComponentOuterAliasKind> for crate::ComponentOuterAliasKind {
    fn from(kind: wasmparser::ComponentOuterAliasKind) -> Self {
        RoundtripReencoder.component_outer_alias_kind(kind)
    }
}

impl From<wasmparser::ComponentTypeRef> for crate::ComponentTypeRef {
    fn from(ty: wasmparser::ComponentTypeRef) -> Self {
        RoundtripReencoder.component_type_ref(ty)
    }
}

impl From<wasmparser::PrimitiveValType> for crate::PrimitiveValType {
    fn from(ty: wasmparser::PrimitiveValType) -> Self {
        RoundtripReencoder.component_primitive_val_type(ty)
    }
}