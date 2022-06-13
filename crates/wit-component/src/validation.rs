use anyhow::{anyhow, bail, Result};
use indexmap::{map::Entry, IndexMap, IndexSet};
use std::borrow::Cow;
use wasmparser::{
    types::Types, Encoding, ExternalKind, FuncType, Parser, Payload, TypeRef, ValType,
    ValidPayload, Validator,
};
use wit_parser::{
    abi::{AbiVariant, WasmSignature, WasmType},
    Interface,
};

fn is_wasi(name: &str) -> bool {
    name == "wasi_unstable" || name == "wasi_snapshot_preview1"
}

fn is_canonical_function(name: &str) -> bool {
    name.starts_with("canonical_abi_")
}

pub fn expected_export_name<'a>(interface: Option<&str>, func: &'a str) -> Cow<'a, str> {
    // TODO: wit-bindgen currently doesn't mangle its export names, so this
    // only works with the default (i.e. `None`) interface.
    match interface {
        Some(interface) => format!("{}#{}", interface, func).into(),
        None => func.into(),
    }
}

fn wasm_sig_to_func_type(signature: WasmSignature) -> FuncType {
    fn from_wasm_type(ty: &WasmType) -> ValType {
        match ty {
            WasmType::I32 => ValType::I32,
            WasmType::I64 => ValType::I64,
            WasmType::F32 => ValType::F32,
            WasmType::F64 => ValType::F64,
        }
    }

    FuncType {
        params: signature
            .params
            .iter()
            .map(from_wasm_type)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        returns: signature
            .results
            .iter()
            .map(from_wasm_type)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

/// This function validates the following:
/// * The bytes represent a core WebAssembly module.
/// * The module's imports are all satisfied by the given import interfaces.
/// * The given default and exported interfaces are satisfied by the module's exports.
///
/// Returns a tuple of the set of imported interfaces required by the module, whether
/// the module exports a memory, and whether the module exports a realloc function.
pub fn validate_module<'a>(
    bytes: &'a [u8],
    interface: &Option<&Interface>,
    imports: &[Interface],
    exports: &[Interface],
) -> Result<(IndexSet<&'a str>, bool, bool)> {
    let imports: IndexMap<&str, &Interface> =
        imports.iter().map(|i| (i.name.as_str(), i)).collect();
    let exports: IndexMap<&str, &Interface> =
        exports.iter().map(|i| (i.name.as_str(), i)).collect();

    let mut validator = Validator::new();
    let mut types = None;
    let mut import_funcs = IndexMap::new();
    let mut export_funcs = IndexMap::new();
    let mut has_memory = false;
    let mut has_realloc = false;

    for payload in Parser::new(0).parse_all(bytes) {
        let payload = payload?;
        if let ValidPayload::End(tys) = validator.payload(&payload)? {
            types = Some(tys);
            break;
        }

        match payload {
            Payload::Version { encoding, .. } if encoding != Encoding::Module => {
                bail!("data is not a WebAssembly module");
            }
            Payload::ImportSection(s) => {
                for import in s {
                    let import = import?;
                    if is_wasi(import.module) {
                        continue;
                    }
                    match import.ty {
                        TypeRef::Func(ty) => {
                            let map = match import_funcs.entry(import.module) {
                                Entry::Occupied(e) => e.into_mut(),
                                Entry::Vacant(e) => e.insert(IndexMap::new()),
                            };

                            assert!(map.insert(import.name, ty).is_none());
                        }
                        _ => bail!("module is only allowed to import functions"),
                    }
                }
            }
            Payload::ExportSection(s) => {
                for export in s {
                    let export = export?;

                    match export.kind {
                        ExternalKind::Func => {
                            if is_canonical_function(export.name) {
                                if export.name == "canonical_abi_realloc" {
                                    // TODO: validate that the canonical_abi_realloc function is [i32, i32, i32, i32] -> [i32]
                                    has_realloc = true;
                                }
                                continue;
                            }

                            assert!(export_funcs.insert(export.name, export.index).is_none())
                        }
                        ExternalKind::Memory => {
                            if export.name == "memory" {
                                has_memory = true;
                            }
                        }
                        _ => continue,
                    }
                }
            }
            _ => continue,
        }
    }

    let types = types.unwrap();

    for (name, funcs) in &import_funcs {
        if name.is_empty() {
            bail!("module imports from an empty module name");
        }

        match imports.get(name) {
            Some(interface) => {
                validate_imported_interface(interface, name, funcs, &types)?;
            }
            None => bail!("module requires an import interface named `{}`", name),
        }
    }

    if let Some(interface) = interface {
        validate_exported_interface(interface, None, &export_funcs, &types)?;
    }

    for (name, interface) in exports {
        if name.is_empty() {
            bail!("cannot export an interface with an empty name");
        }

        validate_exported_interface(interface, Some(name), &export_funcs, &types)?;
    }

    Ok((
        import_funcs.keys().cloned().collect(),
        has_memory,
        has_realloc,
    ))
}

fn validate_imported_interface(
    interface: &Interface,
    name: &str,
    imports: &IndexMap<&str, u32>,
    types: &Types,
) -> Result<()> {
    for (func_name, ty) in imports {
        let f = interface
            .functions
            .iter()
            .find(|f| f.name == *func_name)
            .ok_or_else(|| {
                anyhow!(
                    "import interface `{}` is missing function `{}` that is required by the module",
                    name,
                    func_name,
                )
            })?;

        let expected = wasm_sig_to_func_type(interface.wasm_signature(AbiVariant::GuestImport, f));
        let ty = types.func_type_at(*ty).unwrap();
        if ty != &expected {
            bail!(
                    "type mismatch for function `{}` on imported interface `{}`: expected `{:?} -> {:?}` but found `{:?} -> {:?}`",
                    func_name,
                    name,
                    expected.params,
                    expected.returns,
                    ty.params,
                    ty.returns
                );
        }
    }

    Ok(())
}

fn validate_exported_interface(
    interface: &Interface,
    name: Option<&str>,
    exports: &IndexMap<&str, u32>,
    types: &Types,
) -> Result<()> {
    for f in &interface.functions {
        let expected_export = expected_export_name(name, &f.name);
        match exports.get(expected_export.as_ref()) {
            Some(func_index) => {
                let expected_ty =
                    wasm_sig_to_func_type(interface.wasm_signature(AbiVariant::GuestExport, f));
                let ty = types.function_at(*func_index).unwrap();
                if ty != &expected_ty {
                    match name {
                        Some(name) => bail!(
                            "type mismatch for function `{}` from exported interface `{}`: expected `{:?} -> {:?}` but found `{:?} -> {:?}`",
                            f.name,
                            name,
                            expected_ty.params,
                            expected_ty.returns,
                            ty.params,
                            ty.returns
                        ),
                        None => bail!(
                            "type mismatch for default interface function `{}`: expected `{:?} -> {:?}` but found `{:?} -> {:?}`",
                            f.name,
                            expected_ty.params,
                            expected_ty.returns,
                            ty.params,
                            ty.returns
                        )
                    }
                }
            }
            None => bail!(
                "module does not export required function `{}`",
                expected_export
            ),
        }
    }

    Ok(())
}
