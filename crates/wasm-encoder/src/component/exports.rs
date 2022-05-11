use super::{COMPONENT_SORT, CORE_SORT, FUNCTION_SORT, INSTANCE_SORT, TYPE_SORT, VALUE_SORT};
use crate::{encode_section, ComponentSection, ComponentSectionId, Encode, CORE_MODULE_SORT};

/// Represents the kind of an export from a WebAssembly component.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ComponentExportKind {
    /// The export is a core module.
    Module,
    /// The export is a function.
    Func,
    /// The export is a value.
    Value,
    /// The export is a type.
    Type,
    /// The export is an instance.
    Instance,
    /// The export is a component.
    Component,
}

impl Encode for ComponentExportKind {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::Module => {
                sink.push(CORE_SORT);
                sink.push(CORE_MODULE_SORT);
            }
            Self::Func => {
                sink.push(FUNCTION_SORT);
            }
            Self::Value => {
                sink.push(VALUE_SORT);
            }
            Self::Type => {
                sink.push(TYPE_SORT);
            }
            Self::Instance => {
                sink.push(INSTANCE_SORT);
            }
            Self::Component => {
                sink.push(COMPONENT_SORT);
            }
        }
    }
}

/// Represents an export from a WebAssembly component.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ComponentExport {
    /// The export is a core module.
    Module(u32),
    /// The export is a function.
    Func(u32),
    /// The export is a value.
    Value(u32),
    /// The export is a type.
    Type(u32),
    /// The export is an instance.
    Instance(u32),
    /// The export is a component.
    Component(u32),
}

impl ComponentExport {
    /// Gets the kind of the export.
    pub fn kind(&self) -> ComponentExportKind {
        match self {
            Self::Module(_) => ComponentExportKind::Module,
            Self::Func(_) => ComponentExportKind::Func,
            Self::Value(_) => ComponentExportKind::Value,
            Self::Type(_) => ComponentExportKind::Type,
            Self::Instance(_) => ComponentExportKind::Instance,
            Self::Component(_) => ComponentExportKind::Component,
        }
    }

    /// Gets the index of the export.
    fn index(&self) -> u32 {
        match self {
            Self::Module(idx)
            | Self::Func(idx)
            | Self::Value(idx)
            | Self::Type(idx)
            | Self::Instance(idx)
            | Self::Component(idx) => *idx,
        }
    }
}

impl Encode for ComponentExport {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.kind().encode(sink);
        self.index().encode(sink);
    }
}

/// An encoder for the export section of WebAssembly component.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, ComponentExportSection, ComponentExport};
///
/// // This exports a function named "foo"
/// let mut exports = ComponentExportSection::new();
/// exports.export("foo", ComponentExport::Func(0));
///
/// let mut component = Component::new();
/// component.section(&exports);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ComponentExportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ComponentExportSection {
    /// Create a new component export section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of exports in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an export in the export section.
    pub fn export(&mut self, name: &str, export: ComponentExport) -> &mut Self {
        name.encode(&mut self.bytes);
        export.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for ComponentExportSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl ComponentSection for ComponentExportSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Export.into()
    }
}
