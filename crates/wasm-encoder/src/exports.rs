use super::*;

/// An encoder for the export section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{
///     ItemKind, ExportSection, TableSection, TableType, Limits, Module, ValType,
/// };
///
/// let mut tables = TableSection::new();
/// tables.table(TableType {
///     element_type: ValType::FuncRef,
///     limits: Limits {
///         min: 128,
///         max: None,
///     },
/// });
///
/// let mut exports = ExportSection::new();
/// exports.export("my-table", ItemKind::Table, 0);
///
/// let mut module = Module::new();
/// module
///     .section(&tables)
///     .section(&exports);
///
/// let wasm_bytes = module.finish();
/// ```
pub struct ExportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ExportSection {
    /// Create a new export section encoder.
    pub fn new() -> ExportSection {
        ExportSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Define an export.
    pub fn export(&mut self, name: &str, kind: ItemKind, index: u32) -> &mut Self {
        self.bytes.extend(encoders::str(name));
        self.bytes.push(kind as u8);
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self
    }
}

impl Section for ExportSection {
    fn id(&self) -> u8 {
        SectionId::Export.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

/// Kinds of WebAssembly items
#[allow(missing_docs)]
#[repr(u8)]
pub enum ItemKind {
    Function = 0x00,
    Table = 0x01,
    Memory = 0x02,
    Global = 0x03,
    Module = 0x05,
    Instance = 0x06,
}
