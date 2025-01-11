use super::*;

/// An encoder for the module code section.
///
/// Note that this is part of the [module linking proposal][proposal] and is
/// not currently part of stable WebAssembly.
///
/// [proposal]: https://github.com/webassembly/module-linking
///
/// # Example
///
/// ```
/// use wasm_encoder::{
///     ModuleCodeSection, Function, ModuleSection, Instruction, Module,
///     TypeSection, ValType
/// };
///
/// let mut types = TypeSection::new();
/// types.module(vec![], vec![]);
///
/// let mut modules = ModuleSection::new();
/// let type_index = 0;
/// modules.module(type_index);
///
/// let mut module_code = ModuleCodeSection::new();
/// module_code.module(&Module::new());
///
/// let mut module = Module::new();
/// module
///     .section(&types)
///     .section(&modules)
///     .section(&module_code);
///
/// let wasm_bytes = module.finish();
/// ```
pub struct ModuleCodeSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ModuleCodeSection {
    /// Create a new code section encoder.
    pub fn new() -> ModuleCodeSection {
        ModuleCodeSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Write a function body into this code section.
    pub fn module(&mut self, module: &Module) -> &mut Self {
        self.bytes.extend(
            encoders::u32(u32::try_from(module.bytes.len()).unwrap())
                .chain(module.bytes.iter().copied()),
        );
        self.num_added += 1;
        self
    }
}

impl Section for ModuleCodeSection {
    fn id(&self) -> u8 {
        SectionId::ModuleCode.into()
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
