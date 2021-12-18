//! Encoders for WebAssembly module adapters.
//!
//! This is an implementation of the in-progress [component
//! model proposal](https://github.com/WebAssembly/component-model/).

use crate::{encoders, GlobalType, MemoryType, RawSection, Section, TableType};

mod aliases;
mod exports;
mod imports;
mod instances;
mod modules;
mod types;

pub use aliases::*;
pub use exports::*;
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

const TYPE_REF_INSTANCE: u8 = 0x00;
const TYPE_REF_MODULE: u8 = 0x01;
const TYPE_REF_FUNCTION: u8 = 0x02;
const TYPE_REF_TABLE: u8 = 0x03;
const TYPE_REF_MEMORY: u8 = 0x04;
const TYPE_REF_GLOBAL: u8 = 0x05;

/// A WebAssembly adapter module section.
///
/// This trait marks sections that can be written to an `AdapterModule`.
///
/// Various builders defined in this crate already implement this trait, but you
/// can also implement it yourself for your own custom section builders, or use
/// `RawSection` to use a bunch of raw bytes as a section.
pub trait AdapterModuleSection {
    /// This section's id.
    ///
    /// See `SectionId` for known section ids.
    fn id(&self) -> u8;

    /// Write this section's data and data length prefix into the given sink.
    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>;
}

impl AdapterModuleSection for RawSection<'_> {
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

/// Represents a WebAssembly adapter module that is being encoded.
#[derive(Clone, Debug)]
pub struct AdapterModule {
    pub(crate) bytes: Vec<u8>,
}

impl AdapterModule {
    /// Begin writing a new `AdapterModule`.
    pub fn new() -> Self {
        Self {
            bytes: vec![
                0x00, 0x61, 0x73, 0x6D, // magic (`\0asm`)
                0x0a, 0x00, 0x01, 0x00, // version
            ],
        }
    }

    /// Finish writing this adapter module and extract ownership of the encoded bytes.
    pub fn finish(self) -> Vec<u8> {
        self.bytes
    }

    /// Write a section into this adapter module.
    pub fn section(&mut self, section: &impl AdapterModuleSection) -> &mut Self {
        self.bytes.push(section.id());
        section.encode(&mut self.bytes);
        self
    }
}

impl Default for AdapterModule {
    fn default() -> Self {
        Self::new()
    }
}

/// Known component section IDs.
///
/// Useful for implementing the `AdapterModuleSection` trait, or for setting
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_encodes_an_adapter_module() {
        let bytes = AdapterModule::new().finish();
        assert_eq!(
            bytes,
            [0x00, 'a' as u8, 's' as u8, 'm' as u8, 0x0a, 0x00, 0x01, 0x00]
        );
    }
}
