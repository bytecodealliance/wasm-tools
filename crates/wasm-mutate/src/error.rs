use wasmparser::{Operator, Type, TypeDef};

/// An error encountered when choosing or applying a Wasm mutation.
#[derive(thiserror::Error, Debug)]
pub enum Error {
    /// The input Wasm module did not parse or validate okay.
    #[error("Failed to parse or validate the input Wasm module.")]
    Parse(#[from] wasmparser::BinaryReaderError),
    #[error("There are not applicable mutations for this module.")]
    NoMutationsAplicable,
    #[error("Unsupported mapping.")]
    UnsupportedType(EitherType),
}

#[derive(Debug)]
pub enum EitherType {
    Type(Type),
    TypeDef(String),
    Operator(String)
}

/// A `Result` type that is either `Ok(T)` or `Err(wasm_mutate::Error)`.
pub type Result<T> = std::result::Result<T, Error>;
