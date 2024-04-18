//! A WIT encoder for Rust.
//!
//! This crate is modeled after the `wasm-encoder` crate but is used to encode
//! WIT documents instead of WebAssembly modules.
//!
//! The main builder is [`Resolve`].

mod docs;
mod enum_;
mod flags;
mod function;
mod handle;
mod interface;
mod package;
mod record;
mod resource;
mod result;
mod tuple;
mod ty;
mod variant;
mod world;

pub use docs::*;
pub use enum_::*;
pub use flags::*;
pub use function::*;
pub use handle::*;
pub use interface::*;
pub use package::*;
pub use record::*;
pub use resource::*;
pub use result::*;
pub use tuple::*;
pub use ty::*;
pub use variant::*;
pub use world::*;
