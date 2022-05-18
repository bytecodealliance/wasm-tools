mod code;
mod data;
mod elements;
mod exports;
mod functions;
mod globals;
mod imports;
mod linking;
mod memories;
mod names;
mod start;
mod tables;
mod tags;
mod types;

pub use code::*;
pub use data::*;
pub use elements::*;
pub use exports::*;
pub use functions::*;
pub use globals::*;
pub use imports::*;
pub use linking::*;
pub use memories::*;
pub use names::*;
pub use start::*;
pub use tables::*;
pub use tags::*;
pub use types::*;

use crate::Encode;

/// A WebAssembly module section.
///
/// Various builders defined in this crate already implement this trait, but you
/// can also implement it yourself for your own custom section builders, or use
/// `RawSection` to use a bunch of raw bytes as a section.
pub trait Section: Encode {}

/// Known section identifiers of WebAssembly modules.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[repr(u8)]
pub enum SectionId {
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

impl From<SectionId> for u8 {
    #[inline]
    fn from(id: SectionId) -> u8 {
        id as u8
    }
}

/// Represents a WebAssembly component that is being encoded.
///
/// Sections within a WebAssembly module are encoded in a specific order.
///
/// Modules may also added as a section to a WebAssembly component.
#[derive(Clone, Debug)]
pub struct Module {
    pub(crate) bytes: Vec<u8>,
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
        section.encode(&mut self.bytes);
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
