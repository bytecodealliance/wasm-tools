use super::*;

/// An encoder for the function section.
///
/// Function sections are only supported for modules and components.
///
/// # Example
///
/// ```
/// use wasm_encoder::{Module, FunctionSection, SectionEncodingFormat, ValType};
///
/// let mut functions = FunctionSection::new(SectionEncodingFormat::Module);
/// let type_index = 0;
/// functions.function(type_index);
///
/// let mut module = Module::new();
/// module.section(&functions);
///
/// // Note: this will generate an invalid module because we didn't generate a
/// // code section containing the function body. See the documentation for
/// // `CodeSection` for details.
///
/// let wasm_bytes = module.finish();
/// ```
#[derive(Clone, Debug)]
pub struct FunctionSection {
    bytes: Vec<u8>,
    num_added: u32,
    format: SectionEncodingFormat,
}

impl FunctionSection {
    /// Construct a new function section encoder.
    pub fn new(format: SectionEncodingFormat) -> Self {
        Self {
            bytes: Vec::default(),
            num_added: 0,
            format,
        }
    }

    /// The number of functions in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define a function in a module's function section.
    ///
    /// This method is only supported for encoding modules.
    pub fn function(&mut self, type_index: u32) -> &mut Self {
        if self.format != SectionEncodingFormat::Module {
            panic!("functions can only be encoded for modules");
        }
        self.bytes.extend(encoders::u32(type_index));
        self.num_added += 1;
        self
    }

    /// Define a function in a component's function section.
    ///
    /// This method is only supported for encoding components.
    pub fn canonical_function(
        &mut self,
        type_index: u32,
        options: &[CanonicalOption],
        target_index: u32,
    ) -> &mut Self {
        if self.format != SectionEncodingFormat::Module {
            panic!("canonical functions can only be encoded for components");
        }
        self.bytes.extend(encoders::u32(type_index));
        self.bytes.push(0x00); // This byte is a placeholder for future cases.
        self.bytes
            .extend(encoders::u32(u32::try_from(options.len()).unwrap()));
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.bytes.extend(encoders::u32(target_index));
        self
    }

    fn encode(&self, format: SectionEncodingFormat, sink: &mut impl Extend<u8>) {
        assert_eq!(self.format, format, "function section format mismatch");
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

impl Section<ModuleSectionId> for FunctionSection {
    fn id(&self) -> ModuleSectionId {
        ModuleSectionId::Function
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::Module, sink)
    }
}

impl Section<ComponentSectionId> for FunctionSection {
    fn id(&self) -> ComponentSectionId {
        ComponentSectionId::Function
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::Component, sink)
    }
}
