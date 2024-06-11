//! Wasm type and value types

mod fmt;
mod func;
mod ty;
mod val;

pub use fmt::{DisplayFunc, DisplayFuncArgs, DisplayFuncResults, DisplayType, DisplayValue};
pub use func::WasmFunc;
pub use ty::{WasmType, WasmTypeKind};
pub use val::WasmValue;

pub(crate) use ty::maybe_unwrap_type;
pub(crate) use val::unwrap_val;

/// Returns an error if the given [`WasmType`] is not of the given [`WasmTypeKind`].
pub fn ensure_type_kind(ty: &impl WasmType, kind: WasmTypeKind) -> Result<(), WasmValueError> {
    if ty.kind() == kind {
        Ok(())
    } else {
        Err(WasmValueError::WrongTypeKind {
            ty: DisplayType(ty).to_string(),
            kind,
        })
    }
}

/// An error from creating a [`WasmValue`].
#[derive(Debug)]
#[allow(missing_docs)]
pub enum WasmValueError {
    MissingField(String),
    MissingPayload(String),
    UnexpectedPayload(String),
    UnknownCase(String),
    UnknownField(String),
    UnsupportedType(String),
    WrongNumberOfTupleValues { want: usize, got: usize },
    WrongTypeKind { kind: WasmTypeKind, ty: String },
    WrongValueType { ty: String, val: String },
    Other(String),
}

impl WasmValueError {
    pub(crate) fn wrong_value_type(ty: &impl WasmType, val: &impl WasmValue) -> Self {
        Self::WrongValueType {
            ty: DisplayType(ty).to_string(),
            val: DisplayValue(val).to_string(),
        }
    }
}

impl std::fmt::Display for WasmValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingField(name) => {
                write!(f, "missing field {name:?}")
            }
            Self::MissingPayload(name) => write!(f, "missing payload for {name:?} case"),
            Self::UnexpectedPayload(name) => write!(f, "unexpected payload for {name:?} case"),
            Self::UnknownCase(name) => write!(f, "unknown case {name:?}"),
            Self::UnknownField(name) => write!(f, "unknown field {name:?}"),
            Self::UnsupportedType(ty) => write!(f, "unsupported type {ty}"),
            Self::WrongNumberOfTupleValues { want, got } => {
                write!(f, "expected {want} tuple elements; got {got}")
            }
            Self::WrongTypeKind { kind, ty } => {
                write!(f, "expected a {kind}; got {ty}")
            }
            Self::WrongValueType { ty, val } => {
                write!(f, "expected a {ty}; got {val}")
            }
            Self::Other(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for WasmValueError {}
