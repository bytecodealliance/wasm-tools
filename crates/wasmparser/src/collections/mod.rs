//! Type aliases for maps used by `wasmparser`
//!
//! This module contains type aliases used for [`Map`], [`Set`],
//! [`IndexMap`], and [`IndexSet`]. Note that these differ from upstream types
//! in the `indexmap` crate and the standard library due to customization of the
//! hash algorithm type parameter.

pub mod hash;
pub mod index_map;
pub mod index_set;
pub mod map;
pub mod set;

#[doc(inline)]
pub use self::{index_map::IndexMap, index_set::IndexSet, map::Map, set::Set};
