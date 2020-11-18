use super::*;
use std::convert::TryFrom;

/// An encoder for the type section.
///
/// # Example
///
/// ```
/// use wasm_encoder::{Module, TypeSection, ValType};
///
/// let mut types = TypeSection::new();
/// let params = vec![ValType::I32, ValType::I64];
/// let results = vec![ValType::I32];
/// types.function(params, results);
///
/// let mut module = Module::new();
/// module.section(&types);
///
/// let wasm_bytes = module.finish();
/// ```
pub struct TypeSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl TypeSection {
    /// Create a new type section encoder.
    pub fn new() -> TypeSection {
        TypeSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Define a function type.
    pub fn function<P, R>(&mut self, params: P, results: R) -> &mut Self
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        let params = params.into_iter();
        let results = results.into_iter();

        self.bytes.push(0x60);

        self.bytes
            .extend(encoders::u32(u32::try_from(params.len()).unwrap()));
        self.bytes.extend(params.map(|ty| ty as u8));

        self.bytes
            .extend(encoders::u32(u32::try_from(results.len()).unwrap()));
        self.bytes.extend(results.map(|ty| ty as u8));

        self.num_added += 1;
        self
    }
}

impl Section for TypeSection {
    fn id(&self) -> u8 {
        SectionId::Type as u8
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
