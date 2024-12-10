//! Read and manipulate WebAssembly metadata

#![warn(missing_debug_implementations, missing_docs)]

pub use add_metadata::AddMetadata;
pub use metadata::Metadata;
pub use names::{ComponentNames, ModuleNames};
pub use oci_annotations::{Author, Description, Homepage, Licenses, Revision, Source, Version};
pub use payload::Payload;
pub use producers::{Producers, ProducersField};

pub(crate) use rewrite::rewrite_wasm;

mod add_metadata;
mod metadata;
mod names;
mod oci_annotations;
mod payload;
mod producers;
mod rewrite;

pub(crate) mod utils;
