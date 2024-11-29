//! Read and manipulate WebAssembly metadata

#![warn(missing_debug_implementations, missing_docs)]

pub use add_metadata::AddMetadata;
pub use metadata::Metadata;
pub use names::{ComponentNames, ModuleNames};
pub use producers::{Producers, ProducersField};
pub use registry::{CustomLicense, Link, LinkType, RegistryMetadata};

pub(crate) use rewrite::rewrite_wasm;

mod add_metadata;
mod metadata;
mod names;
mod producers;
mod registry;
mod rewrite;

pub(crate) mod utils;
