use super::{
    COMPONENT_SORT, CORE_MODULE_SORT, CORE_SORT, FUNCTION_SORT, INSTANCE_SORT, TYPE_SORT,
    VALUE_SORT,
};
use crate::{encode_section, ComponentSection, ComponentSectionId, ComponentTypeRef, Encode};

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

/// An encoder for the export section of WebAssembly component.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, ComponentExportSection, ComponentExportKind, ComponentExportName};
///
/// // This exports a function named "foo"
/// let mut exports = ComponentExportSection::new();
/// let name = ComponentExportName::Kebab("foo");
/// exports.export(name, ComponentExportKind::Func, 0, None);
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
    pub fn export(
        &mut self,
        name: impl AsComponentExportName,
        kind: ComponentExportKind,
        index: u32,
        ty: Option<ComponentTypeRef>,
    ) -> &mut Self {
        name.as_component_export_name().encode(&mut self.bytes);
        kind.encode(&mut self.bytes);
        index.encode(&mut self.bytes);
        match ty {
            Some(ty) => {
                self.bytes.push(0x01);
                ty.encode(&mut self.bytes);
            }
            None => {
                self.bytes.push(0x00);
            }
        }
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

/// The different names that can be assigned to component exports
#[derive(Debug, Copy, Clone)]
pub enum ComponentExportName<'a> {
    /// This is a "kebab name" along the lines of "a-foo-bar"
    Kebab(&'a str),
    /// This is an ID along the lines of "wasi:http/types@2.0"
    Interface(&'a str),
}

impl Encode for ComponentExportName<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ComponentExportName::Kebab(name) => {
                sink.push(0x00);
                name.encode(sink);
            }
            ComponentExportName::Interface(name) => {
                sink.push(0x01);
                name.encode(sink);
            }
        }
    }
}

/// Helper trait to convert into a `ComponentExportName` either from that type
/// or from a string.
pub trait AsComponentExportName {
    /// Converts this receiver into a `ComponentExportName`.
    fn as_component_export_name(&self) -> ComponentExportName<'_>;
}

impl AsComponentExportName for ComponentExportName<'_> {
    fn as_component_export_name(&self) -> ComponentExportName<'_> {
        *self
    }
}

impl<S: AsRef<str>> AsComponentExportName for S {
    fn as_component_export_name(&self) -> ComponentExportName<'_> {
        let s = self.as_ref();
        if s.contains("/") {
            ComponentExportName::Interface(s)
        } else {
            ComponentExportName::Kebab(s)
        }
    }
}
