use wasm_encoder::ValType;
use wasmparser::{GlobalType, Type};

/// An error encountered when choosing or applying a Wasm mutation.
#[derive(thiserror::Error, Debug)]
pub enum Error {
    /// The input Wasm module did not parse or validate okay.
    #[error("Failed to parse or validate the input Wasm module.")]
    Parse(#[from] wasmparser::BinaryReaderError),
    /// None of the available mutators are applicable to the input Wasm module
    #[error("There are not applicable mutations for this module.")]
    NoMutationsApplicable,
    /// There is a type/operator that wasm-mutate cannot process
    #[error("Unsupported mapping.")]
    UnsupportedType(EitherType),
    /// Ast parsing error for code motion mutators
    #[error("Invalid Ast parsing for code motion")]
    InvalidAstOperation(String),
}

#[derive(Debug)]
pub enum EitherType {
    Type(Type),
    TypeDef(String),
    Operator(String),
    ValType(ValType),
    EggError(String),
    GlobalType(GlobalType),
}

/// A `Result` type that is either `Ok(T)` or `Err(wasm_mutate::Error)`.
pub type Result<T> = std::result::Result<T, Error>;
