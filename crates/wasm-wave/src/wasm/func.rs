use std::borrow::Cow;

use crate::wasm::WasmType;

// TODO: Given recent versions of rustc we may want to take a look at changing the returns of boxed
// iterators to -> impl Iterator

/// The WasmFunc trait may be implemented to represent Wasm func type
/// signatures to be (de)serialized with WAVE.
pub trait WasmFunc {
    /// A type representing types of these params and results.
    type Type: WasmType;

    /// Returns an iterator of the func's parameter types.
    fn params(&self) -> Box<dyn Iterator<Item = Self::Type> + '_>;

    /// Returns an iterator of the func's parameter names. Must be the same
    /// length as the iterator returned by `params` or empty if this WasmFunc
    /// impl does not support param names.
    fn param_names(&self) -> Box<dyn Iterator<Item = Cow<str>> + '_> {
        Box::new(std::iter::empty())
    }

    /// Returns an iterator of the func's result types.
    fn results(&self) -> Box<dyn Iterator<Item = Self::Type> + '_>;

    /// Returns an iterator of the func's result names. Must be the same
    /// length as the iterator returned by `results` or empty if there are no
    /// named results or if this WasmFunc impl does not support result names.
    fn result_names(&self) -> Box<dyn Iterator<Item = Cow<str>> + '_> {
        Box::new(std::iter::empty())
    }
}
