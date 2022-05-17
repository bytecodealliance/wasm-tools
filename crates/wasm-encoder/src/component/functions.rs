use crate::{encode_section, encoders, ComponentSection, ComponentSectionId, Encode};

/// Represents options for component functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalOption {
    /// The string types in the function signature are UTF-8 encoded.
    UTF8,
    /// The string types in the function signature are UTF-16 encoded.
    UTF16,
    /// The string types in the function signature are compact UTF-16 encoded.
    CompactUTF16,
    /// The lifting or lowering operation requires access to a memory, realloc, or
    /// free function.
    ///
    /// The value is expected to be an instance exporting the canonical ABI memory
    /// and functions.
    Into(u32),
}

impl Encode for CanonicalOption {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::UTF8 => sink.push(0x00),
            Self::UTF16 => sink.push(0x01),
            Self::CompactUTF16 => sink.push(0x02),
            Self::Into(index) => {
                sink.push(0x03);
                sink.extend(encoders::u32(*index));
            }
        }
    }
}

/// An encoder for the function section of WebAssembly components.
///
/// # Example
///
/// ```
/// use wasm_encoder::{Component, ComponentFunctionSection, CanonicalOption};
///
/// let mut functions = ComponentFunctionSection::new();
/// functions.lift(0, 0, [CanonicalOption::UTF8, CanonicalOption::Into(0)]);
///
/// let mut component = Component::new();
/// component.section(&functions);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ComponentFunctionSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ComponentFunctionSection {
    /// Construct a new component function section encoder.
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

    /// Define a function that will lift a core WebAssembly function to the canonical interface ABI.
    pub fn lift<O>(&mut self, type_index: u32, func_index: u32, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        let options = options.into_iter();
        self.bytes.push(0x00);
        self.bytes.extend(encoders::u32(type_index));
        self.bytes
            .extend(encoders::u32(u32::try_from(options.len()).unwrap()));
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.bytes.extend(encoders::u32(func_index));
        self.num_added += 1;
        self
    }

    /// Define a function that will lower a canonical interface ABI function to a core WebAssembly function.
    pub fn lower<O>(&mut self, func_index: u32, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        let options = options.into_iter();
        self.bytes.push(0x01);
        self.bytes
            .extend(encoders::u32(u32::try_from(options.len()).unwrap()));
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.bytes.extend(encoders::u32(func_index));
        self.num_added += 1;
        self
    }
}

impl Encode for ComponentFunctionSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(
            sink,
            ComponentSectionId::Function,
            self.num_added,
            &self.bytes,
        );
    }
}

impl ComponentSection for ComponentFunctionSection {}
