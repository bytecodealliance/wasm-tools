use wasm_encoder::ValType;
use wasmparser::{Operator, Type, TypeDef};

/// An error encountered when choosing or applying a Wasm mutation.
#[derive(thiserror::Error, Debug)]
pub enum Error {
    /// The input Wasm module did not parse or validate okay.
    #[error("Failed to parse or validate the input Wasm module.")]
    Parse(#[from] wasmparser::BinaryReaderError),
    /// None of the available mutators are applicable to the input Wasm module
    #[error("There are not applicable mutations for this module.")]
    NoMutationsAplicable,
    /// There is a type/operator that wasm-mutate cannot process
    #[error("Unsupported mapping.")]
    UnsupportedType(EitherType),
    /// The input Wasm module has a code section, however, peephole mutation cannot be applied
    #[error("There is not applicable peephole mutator for this module.")]
    NotMatchingPeepholes,
}

#[derive(Debug)]
pub enum EitherType {
    Type(Type),
    TypeDef(String),
    Operator(String),
    ValType(ValType),
}

/// A `Result` type that is either `Ok(T)` or `Err(wasm_mutate::Error)`.
pub type Result<T> = std::result::Result<T, Error>;
