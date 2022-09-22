//! The WebAssembly component composing library.

#![deny(missing_docs)]

#[cfg(feature = "cli")]
pub mod cli;
pub mod composer;
pub mod config;
pub(crate) mod encoding;
pub mod graph;
