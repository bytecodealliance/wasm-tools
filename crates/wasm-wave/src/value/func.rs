use crate::wasm::WasmFunc;

use super::{Type, WasmValueError};

/// A FuncType represents the parameter and result type(s) of a Wasm func.
#[derive(Clone, PartialEq)]
pub struct FuncType {
    params: Vec<(String, Type)>,
    results: Vec<(String, Type)>,
}

impl FuncType {
    /// Returns a new FuncType with the given param and result type(s).
    /// Returns an error if the resulting func type would be invalid in the
    /// component model, e.g. has any unnamed results with more than one
    /// result type.
    pub fn new(
        params: impl Into<Vec<(String, Type)>>,
        results: impl Into<Vec<(String, Type)>>,
    ) -> Result<Self, WasmValueError> {
        let params = params.into();
        if params.iter().any(|(name, _)| name.is_empty()) {
            return Err(WasmValueError::Other("func params must be named".into()));
        }
        let results = results.into();
        if results.len() > 1 && results.iter().any(|(name, _)| name.is_empty()) {
            return Err(WasmValueError::Other(
                "funcs with more than one result must have all results named".into(),
            ));
        }
        Ok(Self { params, results })
    }
}

impl WasmFunc for FuncType {
    type Type = Type;

    fn params(&self) -> Box<dyn Iterator<Item = Self::Type> + '_> {
        Box::new(self.params.iter().map(|(_, ty)| ty.clone()))
    }

    fn param_names(&self) -> Box<dyn Iterator<Item = std::borrow::Cow<str>> + '_> {
        Box::new(self.params.iter().map(|(name, _)| name.into()))
    }

    fn results(&self) -> Box<dyn Iterator<Item = Self::Type> + '_> {
        Box::new(self.results.iter().map(|(_, ty)| ty.clone()))
    }

    fn result_names(&self) -> Box<dyn Iterator<Item = std::borrow::Cow<str>> + '_> {
        Box::new(
            self.results
                .iter()
                .flat_map(|(name, _)| (!name.is_empty()).then_some(name.into())),
        )
    }
}

impl std::fmt::Display for FuncType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::wasm::DisplayFunc(self.clone()).fmt(f)
    }
}
