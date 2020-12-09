use super::*;

/// An encoder for the module section.
///
/// Note that this is part of the [module linking proposal][proposal] and is not
/// currently part of stable WebAssembly.
///
/// [proposal]: https://github.com/webassembly/module-linking
///
/// # Example
///
/// ```
/// use wasm_encoder::{Module, ModuleSection, ValType};
///
/// let mut submodules = ModuleSection::new();
/// let type_index = 0;
/// submodules.module(type_index);
///
/// let mut module = Module::new();
/// module.section(&submodules);
///
/// // Note: this will generate an invalid module because we didn't generate a
/// // module code section containing the function body. See the documentation
/// // for `ModuleCodeSection` for details.
///
/// let wasm_bytes = module.finish();
/// ```
pub struct ModuleSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ModuleSection {
    /// Construct a new module section encoder.
    pub fn new() -> ModuleSection {
        ModuleSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Define a module that uses the given type.
    pub fn module(&mut self, type_index: u32) -> &mut Self {
        self.bytes.extend(encoders::u32(type_index));
        self.num_added += 1;
        self
    }
}

impl Section for ModuleSection {
    fn id(&self) -> u8 {
        SectionId::Module.into()
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
