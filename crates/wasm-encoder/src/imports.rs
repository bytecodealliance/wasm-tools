use super::*;
use std::convert::TryFrom;

/// An encoder for the import section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{Module, ImportSection, MemoryType, Limits};
///
/// let mut imports = ImportSection::new();
/// imports.import(
///     "env",
///     "memory",
///     MemoryType {
///         limits: Limits {
///             min: 1,
///             max: None,
///         }
///     }
/// );
///
/// let mut module = Module::new();
/// module.section(&imports);
///
/// let wasm_bytes = module.finish();
/// ```
pub struct ImportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ImportSection {
    /// Construct a new import section encoder.
    pub fn new() -> ImportSection {
        ImportSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Define an import.
    pub fn import(&mut self, module: &str, name: &str, ty: impl Into<ImportType>) -> &mut Self {
        self.bytes.extend(encoders::str(module));
        self.bytes.extend(encoders::str(name));
        match ty.into() {
            ImportType::Function(x) => {
                self.bytes.push(0x00);
                self.bytes.extend(encoders::u32(x));
            }
            ImportType::Table(ty) => {
                self.bytes.push(0x01);
                ty.encode(&mut self.bytes);
            }
            ImportType::Memory(ty) => {
                self.bytes.push(0x02);
                ty.encode(&mut self.bytes);
            }
            ImportType::Global(ty) => {
                self.bytes.push(0x03);
                ty.encode(&mut self.bytes);
            }
        }
        self.num_added += 1;
        self
    }
}

impl Section for ImportSection {
    fn id(&self) -> u8 {
        SectionId::Import.into()
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

/// The type of an import.
pub enum ImportType {
    /// The `n`th function type.
    Function(u32),
    /// A table type.
    Table(TableType),
    /// A memory type.
    Memory(MemoryType),
    /// A global type.
    Global(GlobalType),
}

// NB: no `impl From<u32> for ImportType` because instances and modules also use
// `u32` indices in module linking, so we would have to remove that impl when
// adding support for module linking anyways.

impl From<TableType> for ImportType {
    fn from(t: TableType) -> Self {
        ImportType::Table(t)
    }
}

impl From<MemoryType> for ImportType {
    fn from(m: MemoryType) -> Self {
        ImportType::Memory(m)
    }
}

impl From<GlobalType> for ImportType {
    fn from(g: GlobalType) -> Self {
        ImportType::Global(g)
    }
}
