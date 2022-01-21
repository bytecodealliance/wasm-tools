//! A WebAssembly encoder.
//!
//! The main builder is the [`Module`]. You can build a section with a
//! section-specific builder, like [`TypeSection`] or [`ImportSection`], and
//! then add it to the module with [`Module::section`]. When you are finished
//! building the module, call either [`Module::as_slice`] or [`Module::finish`]
//! to get the encoded bytes. The former gives a shared reference to the
//! underlying bytes as a slice, while the latter gives you ownership of them as
//! a vector.
//!
//! # Example
//!
//! If we wanted to build this module:
//!
//! ```wasm
//! (module
//!   (type (func (param i32 i32) (result i32)))
//!   (func (type 0)
//!     local.get 0
//!     local.get 1
//!     i32.add)
//!   (export "f" (func 0)))
//! ```
//!
//! then we would do this:
//!
//! ```
//! use wasm_encoder::{
//!     CodeSection, Export, ExportSection, Function, FunctionSection, Instruction,
//!     Module, TypeSection, ValType,
//! };
//!
//! let mut module = Module::new();
//!
//! // Encode the type section.
//! let mut types = TypeSection::new();
//! let params = vec![ValType::I32, ValType::I32];
//! let results = vec![ValType::I32];
//! types.function(params, results);
//! module.section(&types);
//!
//! // Encode the function section.
//! let mut functions = FunctionSection::new();
//! let type_index = 0;
//! functions.function(type_index);
//! module.section(&functions);
//!
//! // Encode the export section.
//! let mut exports = ExportSection::new();
//! exports.export("f", Export::Function(0));
//! module.section(&exports);
//!
//! // Encode the code section.
//! let mut codes = CodeSection::new();
//! let locals = vec![];
//! let mut f = Function::new(locals);
//! f.instruction(&Instruction::LocalGet(0));
//! f.instruction(&Instruction::LocalGet(1));
//! f.instruction(&Instruction::I32Add);
//! f.instruction(&Instruction::End);
//! codes.function(&f);
//! module.section(&codes);
//!
//! // Extract the encoded Wasm bytes for this module.
//! let wasm_bytes = module.finish();
//!
//! // We generated a valid Wasm module!
//! assert!(wasmparser::validate(&wasm_bytes).is_ok());
//! ```

#![deny(missing_docs, missing_debug_implementations)]

mod adapters;
mod aliases;
mod code;
mod custom;
mod data;
mod elements;
mod exports;
mod functions;
mod globals;
mod imports;
mod instances;
mod linking;
mod memories;
mod modules;
mod names;
mod start;
mod tables;
mod tags;
mod types;

pub use adapters::*;
pub use aliases::*;
pub use code::*;
pub use custom::*;
pub use data::*;
pub use elements::*;
pub use exports::*;
pub use functions::*;
pub use globals::*;
pub use imports::*;
pub use instances::*;
pub use linking::*;
pub use memories::*;
pub use modules::*;
pub use names::*;
pub use start::*;
pub use tables::*;
pub use tags::*;
pub use types::*;

pub mod encoders;

use std::convert::TryFrom;

/// A WebAssembly module section.
///
/// Various builders defined in this crate already implement this trait, but you
/// can also implement it yourself for your own custom section builders, or use
/// `RawSection` to use a bunch of raw bytes as a section.
pub trait Section {
    /// Gets the section's identifier.
    fn id(&self) -> ModuleSectionId;

    /// Write this section's header and data into the given sink.
    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>;
}

/// A WebAssembly component section.
///
/// Various builders defined in this crate already implement this trait, but you
/// can also implement it yourself for your own custom section builders, or use
/// `RawSection` to use a bunch of raw bytes as a section.
pub trait ComponentSection {
    /// Gets the section's identifier.
    fn id(&self) -> ComponentSectionId;

    /// Write this section's header and data into the given sink.
    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>;
}

/// A section made up of uninterpreted, raw bytes.
///
/// Allows you to splat any data into module or component.
#[derive(Clone, Copy, Debug)]
pub struct RawSection<'a> {
    /// The id for this section.
    pub id: u8,
    /// The raw data for this section.
    pub data: &'a [u8],
}

/// A Wasm module that is being encoded.
#[derive(Clone, Debug)]
pub struct Module {
    bytes: Vec<u8>,
}

impl Module {
    /// Begin writing a new `Module`.
    #[rustfmt::skip]
    pub fn new() -> Self {
        Module {
            bytes: vec![
                // Magic
                0x00, 0x61, 0x73, 0x6D,
                // Version
                0x01, 0x00, 0x00, 0x00,
            ],
        }
    }

    /// Write a section into this module.
    ///
    /// It is your responsibility to define the sections in the [proper
    /// order](https://webassembly.github.io/spec/core/binary/modules.html#binary-module),
    /// and to ensure that each kind of section (other than custom sections) is
    /// only defined once. While this is a potential footgun, it also allows you
    /// to use this crate to easily construct test cases for bad Wasm module
    /// encodings.
    pub fn section(&mut self, section: &impl Section) -> &mut Self {
        self.bytes.push(section.id().into());
        section.encode(&mut self.bytes);
        self
    }

    /// Write a raw section to this module.
    pub fn raw(&mut self, section: &RawSection) -> &mut Self {
        self.bytes.push(section.id);
        self.bytes.extend(
            encoders::u32(u32::try_from(section.data.len()).unwrap())
                .chain(section.data.iter().copied()),
        );
        self
    }

    /// Get the encoded Wasm module as a slice.
    pub fn as_slice(&self) -> &[u8] {
        &self.bytes
    }

    /// Finish writing this Wasm module and extract ownership of the encoded
    /// bytes.
    pub fn finish(self) -> Vec<u8> {
        self.bytes
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents a WebAssembly component that is being encoded.
///
/// Unlike core WebAssembly modules, the sections of a component
/// may appear in any order and may be repeated.
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

    /// Write a section to this component.
    pub fn section(&mut self, section: &impl ComponentSection) -> &mut Self {
        self.bytes.push(section.id().into());
        section.encode(&mut self.bytes);
        self
    }

    /// Write a raw section to this component.
    pub fn raw(&mut self, section: &RawSection) -> &mut Self {
        self.bytes.push(section.id);
        self.bytes.extend_from_slice(section.data);
        self
    }
}

impl Default for Component {
    fn default() -> Self {
        Self::new()
    }
}

/// Known section identifiers of WebAssembly modules.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[repr(u8)]
pub enum ModuleSectionId {
    /// The custom section.
    Custom = 0,
    /// The type section.
    Type = 1,
    /// The import section.
    Import = 2,
    /// The function section.
    Function = 3,
    /// The table section.
    Table = 4,
    /// The memory section.
    Memory = 5,
    /// The global section.
    Global = 6,
    /// The export section.
    Export = 7,
    /// The start section.
    Start = 8,
    /// The element section.
    Element = 9,
    /// The code section.
    Code = 10,
    /// The data section.
    Data = 11,
    /// The data count section.
    DataCount = 12,
    /// The tag section.
    ///
    /// This section is supported by the exception handling proposal.
    Tag = 13,
}

impl From<ModuleSectionId> for u8 {
    #[inline]
    fn from(id: ModuleSectionId) -> u8 {
        id as u8
    }
}

/// Known section identifiers of WebAssembly components.
///
/// These sections are supported by the component model proposal.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[repr(u8)]
pub enum ComponentSectionId {
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

impl From<ComponentSectionId> for u8 {
    #[inline]
    fn from(id: ComponentSectionId) -> u8 {
        id as u8
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EncodingFormat {
    Module,
    Component,
}

/// Represents a reference to a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeRef {
    /// The reference is to a function type.
    Function(u32),
    /// The reference is to a table type.
    Table(TableType),
    /// The reference is to a memory type.
    Memory(MemoryType),
    /// The reference is to a global type.
    Global(GlobalType),
    /// The reference is to a tag type.
    ///
    /// This variant is used with the exception handling proposal.
    Tag(TagType),
    /// The reference is to an instance type.
    ///
    /// This variant is used for components.
    Instance(u32),
    /// The reference is a module type.
    ///
    /// This variant is used for components.
    Module(u32),
    /// The reference is an adapter function type.
    ///
    /// This variant is used for components.
    AdapterFunction(u32),
}

impl TypeRef {
    pub(crate) fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Self::Function(i) => {
                bytes.push(0x00);
                bytes.extend(encoders::u32(*i));
            }
            Self::Table(t) => {
                bytes.push(0x01);
                t.encode(bytes);
            }
            Self::Memory(t) => {
                bytes.push(0x02);
                t.encode(bytes);
            }
            Self::Global(t) => {
                bytes.push(0x03);
                t.encode(bytes);
            }
            Self::Tag(t) => {
                bytes.push(0x04);
                t.encode(bytes);
            }
            Self::Instance(i) => {
                bytes.push(0x05);
                bytes.extend(encoders::u32(*i));
            }
            Self::Module(i) => {
                bytes.push(0x06);
                bytes.extend(encoders::u32(*i));
            }
            Self::AdapterFunction(i) => {
                bytes.push(0x07);
                bytes.extend(encoders::u32(*i));
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

impl From<TagType> for TypeRef {
    fn from(t: TagType) -> Self {
        Self::Tag(t)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_encodes_an_empty_module() {
        let bytes = Module::new().finish();
        assert_eq!(
            bytes,
            [0x00, 'a' as u8, 's' as u8, 'm' as u8, 0x01, 0x00, 0x00, 0x00]
        );
    }

    #[test]
    fn it_encodes_an_empty_component() {
        let bytes = Component::new().finish();
        assert_eq!(
            bytes,
            [0x00, 'a' as u8, 's' as u8, 'm' as u8, 0x0a, 0x00, 0x02, 0x00]
        );
    }
}
