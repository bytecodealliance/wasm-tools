//! The WebAssembly component tooling.

#![deny(missing_docs)]

use crate::encoding::InterfaceEncoder;
use anyhow::{Context, Result};
use std::collections::HashMap;
use wasm_encoder::{Component, ComponentExportSection, ComponentTypeSection};
use wasmparser::{Validator, WasmFeatures};
use wit_parser::Interface;

#[cfg(feature = "cli")]
pub mod cli;
mod decoding;
mod encoding;
mod printing;

pub use printing::*;

/// Encodes an "interface-only" component from an interface definition file.
///
/// The resulting component file will only describe the types of the given interface.
pub fn encode_interface_component(interface: &Interface) -> Result<Vec<u8>> {
    let mut types = ComponentTypeSection::new();
    let mut exports = ComponentExportSection::new();
    let mut type_map = HashMap::new();
    let mut func_type_map = HashMap::new();
    let mut exported = HashMap::new();

    let mut encoder = InterfaceEncoder::new(
        interface,
        &mut types,
        &mut exports,
        &mut type_map,
        &mut func_type_map,
        &mut exported,
    )?;
    encoder.encode(true /* export the function types */)?;

    let mut component = Component::new();
    component.section(&types);
    component.section(&exports);
    let output = component.finish();

    let mut validator = Validator::new();
    validator.wasm_features(WasmFeatures {
        component_model: true,
        ..Default::default()
    });

    validator
        .validate_all(&output)
        .context("failed to validate component output")?;

    Ok(output)
}

/// Decode an "interface-only" component to a wit `Interface`.
pub fn decode_interface_component(bytes: &[u8]) -> Result<Interface> {
    decoding::InterfaceDecoder::new(&decoding::ComponentInfo::new(bytes)?).decode()
}
