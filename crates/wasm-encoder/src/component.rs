//! Encoders for WebAssembly components.
//!
//! This is an implementation of the in-progress [component
//! model proposal](https://github.com/WebAssembly/component-model/).

use crate::{encoders, GlobalType, MemoryType, RawSection, Section, TableType};

mod adapters;
mod aliases;
mod exports;
mod functions;
mod imports;
mod instances;
mod modules;
mod types;

pub use adapters::*;
pub use aliases::*;
pub use exports::*;
pub use functions::*;
pub use imports::*;
pub use instances::*;
pub use modules::*;
pub use types::*;

const INDEX_REF_INSTANCE: u8 = 0x00;
const INDEX_REF_MODULE: u8 = 0x01;
const INDEX_REF_FUNCTION: u8 = 0x02;
const INDEX_REF_TABLE: u8 = 0x03;
const INDEX_REF_MEMORY: u8 = 0x04;
const INDEX_REF_GLOBAL: u8 = 0x05;
const INDEX_REF_ADAPTER_FUNCTION: u8 = 0x06;

const TYPE_REF_INSTANCE: u8 = 0x00;
const TYPE_REF_MODULE: u8 = 0x01;
const TYPE_REF_FUNCTION: u8 = 0x02;
const TYPE_REF_TABLE: u8 = 0x03;
const TYPE_REF_MEMORY: u8 = 0x04;
const TYPE_REF_GLOBAL: u8 = 0x05;
const TYPE_REF_ADAPTER_FUNCTION: u8 = 0x06;

const CANONICAL_OPTION_UTF8: u8 = 0x00;
const CANONICAL_OPTION_UTF16: u8 = 0x01;
const CANONICAL_OPTION_COMPACT_UTF16: u8 = 0x02;
const CANONICAL_OPTION_WITH_REALLOC: u8 = 0x03;
const CANONICAL_OPTION_WITH_FREE: u8 = 0x04;

/// A WebAssembly component section.
///
/// This trait marks sections that can be written to a `Component`.
///
/// Various builders defined in this crate already implement this trait, but you
/// can also implement it yourself for your own custom section builders, or use
/// `RawSection` to use a bunch of raw bytes as a section.
pub trait ComponentSection {
    /// This section's id.
    ///
    /// See `SectionId` for known section ids.
    fn id(&self) -> u8;

    /// Write this section's data and data length prefix into the given sink.
    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>;
}

impl ComponentSection for RawSection<'_> {
    fn id(&self) -> u8 {
        self.id
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        <Self as Section>::encode(self, sink);
    }
}

/// Represents a WebAssembly component that is being encoded.
#[derive(Clone, Debug)]
pub struct Component {
    bytes: Vec<u8>,
}

impl Component {
    /// Begin writing a new `Component`.
    pub fn new() -> Self {
        Self {
            bytes: vec![
                0x00, 0x61, 0x73, 0x6D, // magic (`\0asm`)
                0x0a, 0x00, 0x02, 0x00, // version
            ],
        }
    }

    /// Finish writing this component and extract ownership of the encoded bytes.
    pub fn finish(self) -> Vec<u8> {
        self.bytes
    }

    /// Write a section into this component.
    pub fn section(&mut self, section: &impl ComponentSection) -> &mut Self {
        self.bytes.push(section.id());
        section.encode(&mut self.bytes);
        self
    }
}

impl Default for Component {
    fn default() -> Self {
        Self::new()
    }
}

/// Known component section IDs.
///
/// Useful for implementing the `ComponentSection` trait, or for setting
/// `RawSection::id`.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[repr(u8)]
pub enum SectionId {
    /// The section is a custom section.
    Custom = 0,
    /// The section is a type section.
    Type = 1,
    /// The section is an import section.
    Import = 2,
    /// The section is a module section.
    Module = 3,
    /// The section is an instance section.
    Instance = 4,
    /// The section is an alias section.
    Alias = 5,
    /// The section is an export section.
    Export = 6,
    /// The section is a function section.
    Function = 7,
    /// The section is an adapter function section.
    AdapterFunction = 8,
}

impl From<SectionId> for u8 {
    #[inline]
    fn from(id: SectionId) -> u8 {
        id as u8
    }
}

/// Represents a reference to an index in a WebAssembly section.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum IndexRef {
    /// The reference is to an instance in the instance section.
    Instance(u32),
    /// The reference is to a module in the module section.
    Module(u32),
    /// The reference is to a function in the function section.
    Function(u32),
    /// The reference is to a table in the table section.
    Table(u32),
    /// The reference is to a memory in the memory section.
    Memory(u32),
    /// The reference is to a global in the global section.
    Global(u32),
    /// The reference is to an adapter function in the adapter function section.
    AdapterFunction(u32),
}

impl IndexRef {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            IndexRef::Instance(index) => {
                bytes.push(INDEX_REF_INSTANCE);
                bytes.extend(encoders::u32(*index));
            }
            IndexRef::Module(index) => {
                bytes.push(INDEX_REF_MODULE);
                bytes.extend(encoders::u32(*index));
            }
            IndexRef::Function(index) => {
                bytes.push(INDEX_REF_FUNCTION);
                bytes.extend(encoders::u32(*index));
            }
            IndexRef::Table(index) => {
                bytes.push(INDEX_REF_TABLE);
                bytes.extend(encoders::u32(*index));
            }
            IndexRef::Memory(index) => {
                bytes.push(INDEX_REF_MEMORY);
                bytes.extend(encoders::u32(*index));
            }
            IndexRef::Global(index) => {
                bytes.push(INDEX_REF_GLOBAL);
                bytes.extend(encoders::u32(*index));
            }
            IndexRef::AdapterFunction(index) => {
                bytes.push(INDEX_REF_ADAPTER_FUNCTION);
                bytes.extend(encoders::u32(*index));
            }
        }
    }
}

/// Represents a reference to a type definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeRef {
    /// The definition is an instance.
    ///
    /// The value is an index in the types index space.
    /// The index must be to an instance type.
    Instance(u32),
    /// The definition is a module.
    ///
    /// The value is an index in the types index space.
    /// The index must be to a module type.
    Module(u32),
    /// The definition is a core wasm function.
    ///
    /// The value is an index in the types index space.
    /// The index must be to a function type.
    Function(u32),
    /// The definition is a core wasm table.
    Table(TableType),
    /// The definition is a core wasm memory.
    Memory(MemoryType),
    /// The definition is a core wasm global.
    Global(GlobalType),
    /// The definition is an adapter function.
    ///
    /// The value is an index in the types index space.
    /// The index must be to an adapter function type.
    AdapterFunction(u32),
}

impl TypeRef {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::Instance(index) => {
                bytes.push(TYPE_REF_INSTANCE);
                bytes.extend(encoders::u32(*index));
            }
            Self::Module(index) => {
                bytes.push(TYPE_REF_MODULE);
                bytes.extend(encoders::u32(*index));
            }
            Self::Function(index) => {
                bytes.push(TYPE_REF_FUNCTION);
                bytes.extend(encoders::u32(*index));
            }
            Self::Table(ty) => {
                bytes.push(TYPE_REF_TABLE);
                ty.encode(bytes);
            }
            Self::Memory(ty) => {
                bytes.push(TYPE_REF_MEMORY);
                ty.encode(bytes);
            }
            Self::Global(ty) => {
                bytes.push(TYPE_REF_GLOBAL);
                ty.encode(bytes);
            }
            Self::AdapterFunction(index) => {
                bytes.push(TYPE_REF_ADAPTER_FUNCTION);
                bytes.extend(encoders::u32(*index));
            }
        }
    }
}

impl From<TableType> for TypeRef {
    fn from(t: TableType) -> Self {
        Self::Table(t)
    }
}

impl From<MemoryType> for TypeRef {
    fn from(t: MemoryType) -> Self {
        Self::Memory(t)
    }
}

impl From<GlobalType> for TypeRef {
    fn from(t: GlobalType) -> Self {
        Self::Global(t)
    }
}

/// Represents options for canonical functions and adapter functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalOption {
    /// The string types in the function signature are UTF-8 encoded.
    UTF8,
    /// The string types in the function signature are UTF-16 encoded.
    UTF16,
    /// The string types in the function signature are compact UTF-16 encoded.
    CompactUTF16,
    /// Specifies the function to use to reallocate memory.
    WithRealloc(u32),
    /// Specifies the function to use to free memory.
    WithFree(u32),
}

impl CanonicalOption {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::UTF8 => bytes.push(CANONICAL_OPTION_UTF8),
            Self::UTF16 => bytes.push(CANONICAL_OPTION_UTF16),
            Self::CompactUTF16 => bytes.push(CANONICAL_OPTION_COMPACT_UTF16),
            Self::WithRealloc(index) => {
                bytes.push(CANONICAL_OPTION_WITH_REALLOC);
                bytes.extend(encoders::u32(*index));
            }
            Self::WithFree(index) => {
                bytes.push(CANONICAL_OPTION_WITH_FREE);
                bytes.extend(encoders::u32(*index));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_encodes_an_empty_component() {
        let bytes = Component::new().finish();
        assert_eq!(
            bytes,
            [0x00, 'a' as u8, 's' as u8, 'm' as u8, 0x0a, 0x00, 0x02, 0x00]
        );
    }
}
