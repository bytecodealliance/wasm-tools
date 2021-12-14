/// An error encountered when choosing or applying a Wasm mutation.
#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub struct Error {
    kind: Box<ErrorKind>,
}

impl Error {
    /// Construct a new `Error` from an `ErrorKind`.
    pub fn new(kind: ErrorKind) -> Self {
        kind.into()
    }

    /// Construct a new parse error.
    pub fn parse(err: wasmparser::BinaryReaderError) -> Self {
        err.into()
    }

    /// Construct a "no mutations applicable" error.
    pub fn no_mutations_applicable() -> Self {
        ErrorKind::NoMutationsApplicable.into()
    }

    /// Construct an "out of fuel" error.
    pub fn out_of_fuel() -> Self {
        ErrorKind::OutOfFuel.into()
    }

    /// Construct an unsupported error.
    pub fn unsupported(msg: impl Into<String>) -> Self {
        ErrorKind::Unsupported(msg.into()).into()
    }

    /// Construct another kind of `Error`.
    pub fn other(err: impl Into<String>) -> Self {
        ErrorKind::Other(err.into()).into()
    }

    /// Get the kind of error that this is.
    pub fn kind(&self) -> &ErrorKind {
        &*self.kind
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error {
            kind: Box::new(kind),
        }
    }
}

impl From<wasmparser::BinaryReaderError> for Error {
    fn from(e: wasmparser::BinaryReaderError) -> Self {
        ErrorKind::Parse(e).into()
    }
}

/// The kind of error.
#[derive(thiserror::Error, Debug)]
pub enum ErrorKind {
    /// Failed to parse the input Wasm module.
    #[error("Failed to parse the input Wasm module.")]
    Parse(#[from] wasmparser::BinaryReaderError),

    /// None of the available mutators are applicable to the input Wasm module
    #[error("There are not applicable mutations for the input Wasm module.")]
    NoMutationsApplicable,

    /// Ran out of fuel before a mutation could be applied succesfully.
    #[error("Out of fuel")]
    OutOfFuel,

    /// The Wasm is using an unsupported feature.
    #[error("Unsupported: {0}")]
    Unsupported(String),

    /// Another error.
    #[error("{0}")]
    Other(String),
}

/// A `Result` type that is either `Ok(T)` or `Err(wasm_mutate::Error)`.
pub type Result<T, E = Error> = std::result::Result<T, E>;
