use crate::{encode_section, CustomSection, Encode, Section, SectionId};

/// An encoder for the [producers custom
/// section](https://github.com/WebAssembly/tool-conventions/blob/main/ProducersSection.md).
///
/// This section is a non-standard convention that is supported by many toolchains.
///
/// # Example
///
/// ```
/// use wasm_encoder::{ProducersSection, Module, SymbolTable};
///
/// // Create a new producers section.
/// let mut producers = ProducersSection::new();
///
/// // Add the linking section to a new Wasm module and get the encoded bytes.
/// let mut module = Module::new();
/// module.section(&linking);
/// let wasm_bytes = module.finish();
/// ```
#[derive(Clone, Debug)]
pub struct ProducersSection {
    bytes: Vec<u8>,
    num_fields: u32,
}

impl ProducersSection {
    /// Construct a new encoder for the linking custom section.
    pub fn new() -> Self {
        Self::default()
    }

    /// TK
    pub fn field(&mut self, producers_field: &ProducersField) -> &mut Self {
        producers_field.encode(&mut self.bytes);
        self.num_fields += 1;
        self
    }
}

impl Default for ProducersSection {
    fn default() -> Self {
        Self {
            bytes: Vec::new(),
            num_fields: 0,
        }
    }
}

impl Encode for ProducersSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        let mut data = Vec::new();
        encode_section(&mut data, self.num_fields, &self.bytes);

        CustomSection {
            name: "producers",
            data: &data,
        }
        .encode(sink);
    }
}

impl Section for ProducersSection {
    fn id(&self) -> u8 {
        SectionId::Custom.into()
    }
}

/// TK
#[derive(Clone, Debug)]
pub struct ProducersField {
    bytes: Vec<u8>,
    num_values: u32,
}

impl ProducersField {
    /// TK
    pub fn new(name: &str) -> Self {
        let mut bytes = Vec::new();
        name.encode(&mut bytes);
        ProducersField {
            bytes,
            num_values: 0,
        }
    }

    /// TK
    pub fn value(&mut self, value: &ProducersFieldValue) -> &mut Self {
        value.encode(&mut self.bytes);
        self.num_values += 1;
        self
    }
}

impl Encode for ProducersField {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_values, &self.bytes);
    }
}

/// TK
#[derive(Clone, Debug)]
pub struct ProducersFieldValue {
    bytes: Vec<u8>,
}

impl ProducersFieldValue {
    /// TK
    pub fn name(name: &str) -> Self {
        Self::version(name, "")
    }
    /// TK
    pub fn version(name: &str, version: &str) -> Self {
        let mut bytes = Vec::new();
        name.encode(&mut bytes);
        version.encode(&mut bytes);
        ProducersFieldValue { bytes }
    }
}

impl Encode for ProducersFieldValue {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend_from_slice(&self.bytes)
    }
}
