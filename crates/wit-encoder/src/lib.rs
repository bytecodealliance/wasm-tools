//! A WIT encoder for Rust.
//!
//! This crate is modeled after the `wasm-encoder` crate but is used to encode
//! WIT documents instead of WebAssembly modules.

mod docs;
mod enum_;
mod flags;
mod function;
mod ident;
mod include;
mod interface;
mod package;
mod record;
mod render;
mod resource;
mod result;
mod tuple;
mod ty;
mod use_;
mod variant;
mod world;

pub use docs::*;
pub use enum_::*;
pub use flags::*;
pub use function::*;
pub use ident::*;
pub use include::*;
pub use interface::*;
pub use package::*;
pub use record::*;
pub use render::*;
pub use resource::*;
pub use result::*;
pub use tuple::*;
pub use ty::*;
pub use use_::*;
pub use variant::*;
pub use world::*;
