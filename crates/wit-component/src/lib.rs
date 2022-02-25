//! The WebAssembly component tooling.

#![deny(missing_docs)]

use crate::encoding::InterfaceEncoder;
use anyhow::{bail, Context, Result};
use std::{collections::HashMap, path::Path};
use wasm_encoder::{Component, ComponentExportSection, ComponentTypeSection};
use wasmparser::{ComponentExportKind, ComponentTypeDef, Validator, WasmFeatures};
use wit_parser::Interface;

#[cfg(feature = "cli")]
pub mod cli;
mod decoding;
mod encoding;
mod printing;

pub use printing::*;

fn read_interface(path: impl AsRef<Path>) -> Result<Interface> {
    let path = path.as_ref();

    if !path.is_file() {
        bail!("interface file `{}` does not exist", path.display(),);
    }

    Interface::parse_file(&path)
        .with_context(|| format!("failed to parse interface file `{}`", path.display()))
}

/// Encodes an "interface-only" component from an interface definition file.
///
/// The resulting component file will only describe the types of the given interface.
pub fn encode_interface_component(name: &str, interface: impl AsRef<Path>) -> Result<Vec<u8>> {
    let interface = read_interface(interface)?;

    let mut types = ComponentTypeSection::new();
    let mut exports = ComponentExportSection::new();
    let mut type_map = HashMap::new();
    let mut func_type_map = HashMap::new();

    let mut encoder = InterfaceEncoder::new(
        &interface,
        name,
        &mut types,
        &mut exports,
        &mut type_map,
        &mut func_type_map,
    )?;
    encoder.encode()?;

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

/// Gets the imported and exported interfaces of a component.
pub fn decode_interface_component(component: impl AsRef<Path>) -> Result<Interface> {
    let component = component.as_ref();
    if !component.is_file() {
        bail!(
            "component `{}` does not exist as a file",
            component.display()
        );
    }

    let bytes = wat::parse_file(component)
        .with_context(|| format!("failed to parse component `{}`", component.display()))?;

    let info = decoding::ComponentInfo::new(&bytes)?;
    if !info.imports.is_empty() || info.exports.len() != 1 {
        bail!(
            "component `{}` is not an interface-only component",
            component.display()
        );
    }

    let export = &info.exports[0];

    Ok(match export.kind {
        ComponentExportKind::Type(ty) => match &info.types[ty as usize] {
            ComponentTypeDef::Instance(_) => {
                decoding::InterfaceDecoder::new(&info, export.name, ty)?.finish()?
            }
            _ => bail!("expected an instance type export"),
        },
        _ => bail!("expected a type export"),
    })
}
