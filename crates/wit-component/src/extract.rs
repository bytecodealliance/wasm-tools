use crate::decode_interface_component;
use anyhow::{Context, Result};
use wit_parser::Interface;

/// Result of extracting interfaces embedded within a core wasm file.
///
/// This structure is reated by the [`extract_module_interfaces`] function.
#[derive(Default)]
pub struct ModuleInterfaces {
    /// The core wasm binary with custom sections removed.
    pub wasm: Vec<u8>,

    /// Imported interfaces found in custom sections.
    pub imports: Vec<Interface>,

    /// Exported interfaces found in custom sections.
    pub exports: Vec<Interface>,

    /// The default exported interface found in a custom section.
    pub interface: Option<Interface>,
}

/// This function will parse the `wasm` binary given as input and return a
/// [`ModuleInterfaces`] which extracts the custom sections describing
/// component-level types from within the binary itself.
///
/// This is used to parse the output of `wit-bindgen`-generated modules and is
/// one of the earliest phases in transitioning such a module to a component.
/// The extraction here provides the metadata necessary to continue the process
/// later on.
pub fn extract_module_interfaces(wasm: &[u8]) -> Result<ModuleInterfaces> {
    let mut ret = ModuleInterfaces::default();

    for payload in wasmparser::Parser::new(0).parse_all(wasm) {
        match payload.context("decoding item in module")? {
            wasmparser::Payload::CustomSection(cs) => {
                if let Some(export) = cs.name().strip_prefix("component-type:export:") {
                    let mut i = decode_interface_component(cs.data()).with_context(|| {
                        format!("decoding component-type in export section {}", export)
                    })?;
                    i.name = export.to_owned();
                    ret.interface = Some(i);
                } else if let Some(import) = cs.name().strip_prefix("component-type:import:") {
                    let mut i = decode_interface_component(cs.data()).with_context(|| {
                        format!("decoding component-type in import section {}", import)
                    })?;
                    i.name = import.to_owned();
                    ret.imports.push(i);
                } else if let Some(export_instance) =
                    cs.name().strip_prefix("component-type:export-instance:")
                {
                    let mut i = decode_interface_component(cs.data()).with_context(|| {
                        format!(
                            "decoding component-type in export-instance section {}",
                            export_instance
                        )
                    })?;
                    i.name = export_instance.to_owned();
                    ret.exports.push(i);
                }
            }
            _ => {}
        }
    }

    // TODO: should remove the custom setions decoded above from the wasm binary
    // created here, and bytecodealliance/wasmparser#792 should help with that
    // to make the loop above pretty small.
    ret.wasm = wasm.to_vec();

    Ok(ret)
}
