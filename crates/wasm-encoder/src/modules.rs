use crate::{
    encoders, AdapterModule, AdapterModuleSectionId, Component, ComponentSectionId, Module,
    Section, SectionEncodingFormat,
};

/// An encoder for the module section.
///
/// Module sections are only supported for adapter modules and components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Module, AdapterModule, Component, ModuleSection, SectionEncodingFormat};
///
/// let mut modules = ModuleSection::new(SectionEncodingFormat::Component);
/// modules.module(&Module::new());
/// modules.adapter(&AdapterModule::new());
/// modules.component(&Component::new());
///
/// let mut component = Component::new();
/// component.section(&modules);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug)]
pub struct ModuleSection {
    bytes: Vec<u8>,
    num_added: u32,
    format: SectionEncodingFormat,
}

impl ModuleSection {
    /// Create a new module section encoder.
    pub fn new(format: SectionEncodingFormat) -> Self {
        if format == SectionEncodingFormat::Module {
            panic!("instance sections are not supported for module encoding");
        }

        Self {
            bytes: Vec::new(),
            num_added: 0,
            format,
        }
    }

    /// The number of modules in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Writes a module into this module section.
    pub fn module(&mut self, module: &Module) -> &mut Self {
        self.bytes.extend(
            encoders::u32(u32::try_from(module.bytes.len()).unwrap())
                .chain(module.bytes.iter().copied()),
        );
        self.num_added += 1;
        self
    }

    /// Writes an adapter module into this module section.
    pub fn adapter(&mut self, module: &AdapterModule) -> &mut Self {
        self.bytes.extend(
            encoders::u32(u32::try_from(module.bytes.len()).unwrap())
                .chain(module.bytes.iter().copied()),
        );
        self.num_added += 1;
        self
    }

    /// Writes a component into this module section.
    ///
    /// This is only supported for module sections of components.
    pub fn component(&mut self, component: &Component) -> &mut Self {
        if self.format == SectionEncodingFormat::AdapterModule {
            panic!("cannot add a component to the module section of an adapter module");
        }
        self.bytes.extend(
            encoders::u32(u32::try_from(component.bytes.len()).unwrap())
                .chain(component.bytes.iter().copied()),
        );
        self.num_added += 1;
        self
    }

    fn encode(&self, format: SectionEncodingFormat, sink: &mut impl Extend<u8>) {
        assert_eq!(self.format, format, "module section format mismatch");
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

impl Section<AdapterModuleSectionId> for ModuleSection {
    fn id(&self) -> AdapterModuleSectionId {
        AdapterModuleSectionId::Module
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::AdapterModule, sink);
    }
}

impl Section<ComponentSectionId> for ModuleSection {
    fn id(&self) -> ComponentSectionId {
        ComponentSectionId::Module
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::Component, sink);
    }
}
