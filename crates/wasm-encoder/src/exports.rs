use super::*;

/// An encoder for the export section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{
///     Export, ExportSection, TableSection, TableType, Limits, Module, ValType,
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
/// exports.export("my-table", Export::Table(0));
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
    pub fn export(&mut self, name: &str, export: Export) -> &mut Self {
        self.bytes.extend(encoders::str(name));
        export.encode(&mut self.bytes);
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

/// A WebAssembly export.
pub enum Export {
    /// An export of the `n`th function.
    Function(u32),
    /// An export of the `n`th table.
    Table(u32),
    /// An export of the `n`th memory.
    Memory(u32),
    /// An export of the `n`th global.
    Global(u32),
}

impl Export {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match *self {
            Export::Function(x) => {
                bytes.push(0x00);
                bytes.extend(encoders::u32(x));
            }
            Export::Table(x) => {
                bytes.push(0x01);
                bytes.extend(encoders::u32(x));
            }
            Export::Memory(x) => {
                bytes.push(0x02);
                bytes.extend(encoders::u32(x));
            }
            Export::Global(x) => {
                bytes.push(0x03);
                bytes.extend(encoders::u32(x));
            }
        }
    }
}
