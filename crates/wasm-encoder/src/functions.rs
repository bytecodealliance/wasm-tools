use super::*;

/// An encoder for the function section.
///
/// Function sections are only supported for modules and components.
///
/// # Example
///
/// ```
/// use wasm_encoder::{Module, FunctionSection, ValType};
///
/// let mut functions = FunctionSection::new();
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
#[derive(Clone, Debug, Default)]
pub struct FunctionSection {
    bytes: Vec<u8>,
    num_added: u32,
    format: Option<EncodingFormat>,
}

impl FunctionSection {
    /// Construct a new function section encoder.
    pub fn new() -> Self {
        Self::default()
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
        assert_eq!(
            *self.format.get_or_insert(EncodingFormat::Module),
            EncodingFormat::Module,
            "cannot encode a function for a WebAssembly component"
        );
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
        assert_eq!(
            *self.format.get_or_insert(EncodingFormat::Component),
            EncodingFormat::Component,
            "cannot encode a canonical function for a WebAssembly module"
        );
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

    fn encode(&self, expected: EncodingFormat, sink: &mut impl Extend<u8>) {
        match self.format {
            Some(format) => {
                assert_eq!(format, expected, "function section format mismatch");
            }
            None => assert_eq!(self.num_added, 0),
        }

        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

impl Section for FunctionSection {
    fn id(&self) -> SectionId {
        SectionId::Function
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(EncodingFormat::Module, sink)
    }
}

impl ComponentSection for FunctionSection {
    fn id(&self) -> ComponentSectionId {
        ComponentSectionId::Function
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(EncodingFormat::Component, sink)
    }
}
