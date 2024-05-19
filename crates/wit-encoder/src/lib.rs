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
mod interface;
mod package;
mod record;
mod resource;
mod result;
mod tuple;
mod ty;
mod variant;
mod world;

use std::fmt;

pub use docs::*;
pub use enum_::*;
pub use flags::*;
pub use function::*;
pub use interface::*;
pub use package::*;
pub use record::*;
pub use resource::*;
pub use result::*;
pub use tuple::*;
pub use ty::*;
pub use variant::*;
pub use world::*;

pub struct RenderOpts {
    indent_count: usize,
}

impl Default for RenderOpts {
    fn default() -> Self {
        Self { indent_count: 4 }
    }
}

impl RenderOpts {
    fn indent(&self, depth: usize) -> usize {
        self.indent_count * depth
    }
}

pub trait Render {
    fn render_opts(
        &self,
        f: &mut fmt::Formatter<'_>,
        depth: usize,
        options: RenderOpts,
    ) -> fmt::Result;

    fn render(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        Self::render_opts(&self, f, depth, Default::default())
    }
}
