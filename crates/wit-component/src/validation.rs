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

fn is_canonical_function(name: &str) -> bool {
    name.starts_with("cabi_")
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

    FuncType::new(
        signature.params.iter().map(from_wasm_type),
        signature.results.iter().map(from_wasm_type),
    )
}

/// Metadata about a validated module and what was found internally.
///
/// All imports to the module are described by the union of `required_imports`
/// and `adapters_required`.
///
/// This structure is created by the `validate_module` function.
#[derive(Default)]
pub struct ValidatedModule<'a> {
    /// The required imports into this module which are to be satisfied by
    /// imported component model instances.
    ///
    /// The key of this map is the name of the interface that the module imports
    /// from and the value is the set of functions required from that interface.
    /// This is used to generate an appropriate instance import in the generated
    /// component which imports only the set of required functions.
    pub required_imports: IndexMap<&'a str, IndexSet<&'a str>>,

    /// This is the set of imports into the module which were not satisfied by
    /// imported interfaces but are required to be satisfied by adapter modules.
    ///
    /// The key of this map is the name of the adapter that was imported into
    /// the module and the value is a further map from function to function type
    /// as required by this module. This map is used to shrink adapter modules
    /// to the precise size required for this module by ensuring it doesn't
    /// export (and subsequently import) extraneous functions.
    pub adapters_required: IndexMap<&'a str, IndexMap<&'a str, FuncType>>,

    /// Whether or not this module exported a linear memory.
    pub has_memory: bool,

    /// Whether or not this module exported a `cabi_realloc` function.
    pub has_realloc: bool,
}

/// This function validates the following:
///
/// * The `bytes` represent a valid core WebAssembly module.
/// * The module's imports are all satisfied by the given `imports` interfaces
///   or the `adapters` set.
/// * The given default and exported interfaces are satisfied by the module's
///   exports.
///
/// The `ValidatedModule` return value contains the metadata which describes the
/// input module on success. This is then further used to generate a component
/// for this module.
pub fn validate_module<'a>(
    bytes: &'a [u8],
    interface: &Option<Interface>,
    imports: &IndexMap<String, Interface>,
    exports: &IndexMap<String, Interface>,
    adapters: &IndexSet<&str>,
) -> Result<ValidatedModule<'a>> {
    let mut validator = Validator::new();
    let mut types = None;
    let mut import_funcs = IndexMap::new();
    let mut export_funcs = IndexMap::new();
    let mut ret = ValidatedModule::default();

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
                                if export.name == "cabi_realloc" {
                                    // TODO: validate that the cabi_realloc function is [i32, i32, i32, i32] -> [i32]
                                    ret.has_realloc = true;
                                }
                                continue;
                            }

                            assert!(export_funcs.insert(export.name, export.index).is_none())
                        }
                        ExternalKind::Memory => {
                            if export.name == "memory" {
                                ret.has_memory = true;
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

        match imports.get(*name) {
            Some(interface) => {
                validate_imported_interface(interface, name, funcs, &types)?;
                let funcs = funcs.into_iter().map(|(f, _ty)| *f).collect();
                let prev = ret.required_imports.insert(name, funcs);
                assert!(prev.is_none());
            }
            None if adapters.contains(name) => {
                let map = ret.adapters_required.entry(name).or_insert(IndexMap::new());
                for (func, ty) in funcs {
                    let ty = types.func_type_at(*ty).unwrap();
                    map.insert(func, ty.clone());
                }
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

    Ok(ret)
}

/// Validation information from an "adapter module" which is distinct from a
/// "main module" validated above.
///
/// This is created by the `validate_adapter_module` function.
#[derive(Default, Debug)]
pub struct ValidatedAdapter<'a> {
    /// If specified then this is the name of the required interface imported
    /// into the adapter module.
    ///
    /// At this time only one interface import is supported. If this is `None`
    /// then the adapter module didn't import any component model functions to
    /// implement the required functionality.
    pub required_import: Option<&'a str>,

    /// This is the set of required functions imported from `required_import`,
    /// if `required_import` is specified.
    pub required_funcs: IndexSet<&'a str>,

    /// This is the module and field name of the memory import, if one is
    /// specified.
    ///
    /// Due to LLVM codegen this is typically `env::memory` as a totally separte
    /// import from the `required_import` above.
    pub needs_memory: Option<(String, String)>,

    /// Flag for whether a `cabi_realloc` function was found within this module.
    pub has_realloc: bool,
}

/// This function will validate the `bytes` provided as a wasm adapter module.
/// Notably this will validate the wasm module itself in addition to ensuring
/// that it has the "shape" of an adapter module. Current constraints are:
///
/// * The adapter module can import only one memory
/// * The adapter module can only import from the name of `interface` specified,
///   and all function imports must match the `required` types which correspond
///   to the lowered types of the functions in `interface`.
///
/// The wasm module passed into this function is the output of the GC pass of an
/// adapter module's original source. This means that the adapter module is
/// already minimized and this is a double-check that the minimization pass
/// didn't accidentally break the wasm module.
pub fn validate_adapter_module<'a>(
    bytes: &[u8],
    interface: &'a Interface,
    required: &IndexMap<&str, FuncType>,
) -> Result<ValidatedAdapter<'a>> {
    let mut validator = Validator::new();
    let mut import_funcs = IndexMap::new();
    let mut export_funcs = IndexMap::new();
    let mut types = None;
    let mut funcs = Vec::new();
    let mut ret = ValidatedAdapter::default();

    for payload in Parser::new(0).parse_all(bytes) {
        let payload = payload?;
        match validator.payload(&payload)? {
            ValidPayload::End(tys) => {
                types = Some(tys);
                break;
            }
            ValidPayload::Func(validator, body) => {
                funcs.push((validator, body));
            }
            _ => {}
        }

        match payload {
            Payload::Version { encoding, .. } if encoding != Encoding::Module => {
                bail!("data is not a WebAssembly module");
            }

            Payload::ImportSection(s) => {
                for import in s {
                    let import = import?;
                    match import.ty {
                        TypeRef::Func(ty) => {
                            let map = match import_funcs.entry(import.module) {
                                Entry::Occupied(e) => e.into_mut(),
                                Entry::Vacant(e) => e.insert(IndexMap::new()),
                            };

                            assert!(map.insert(import.name, ty).is_none());
                        }

                        // A memory is allowed to be imported into the adapter
                        // module so that's skipped here
                        TypeRef::Memory(_) => {
                            ret.needs_memory =
                                Some((import.module.to_string(), import.name.to_string()));
                        }

                        _ => {
                            bail!("adapter module is only allowed to import functions and memories")
                        }
                    }
                }
            }
            Payload::ExportSection(s) => {
                for export in s {
                    let export = export?;

                    match export.kind {
                        ExternalKind::Func => {
                            export_funcs.insert(export.name, export.index);
                            if export.name == "cabi_realloc" {
                                ret.has_realloc = true;
                            }
                        }
                        _ => continue,
                    }
                }
            }
            _ => continue,
        }
    }

    let mut resources = Default::default();
    for (validator, body) in funcs {
        let mut validator = validator.into_validator(resources);
        validator.validate(&body)?;
        resources = validator.into_allocations();
    }

    let types = types.unwrap();
    let mut import_funcs = import_funcs.iter();
    if let Some((name, funcs)) = import_funcs.next() {
        if *name != interface.name {
            bail!(
                "adapter module imports from `{name}` which does not match \
                 its interface `{}`",
                interface.name
            );
        }
        ret.required_funcs = validate_imported_interface(interface, name, funcs, &types)?;
        ret.required_import = Some(interface.name.as_str());

        if let Some((name, _)) = import_funcs.next() {
            bail!("adapter module cannot import from a second interface `{name}`")
        }
    }

    for (name, ty) in required {
        let idx = match export_funcs.get(name) {
            Some(idx) => *idx,
            None => bail!("adapter module did not export `{name}`"),
        };
        let actual = types.function_at(idx).unwrap();
        if ty == actual {
            continue;
        }
        bail!(
            "adapter module export `{name}` does not match the expected signature:\n\
            expected: {:?} -> {:?}\n\
            actual:   {:?} -> {:?}\n\
            ",
            ty.params(),
            ty.results(),
            actual.params(),
            actual.results(),
        );
    }

    Ok(ret)
}

fn validate_imported_interface<'a>(
    interface: &'a Interface,
    name: &str,
    imports: &IndexMap<&str, u32>,
    types: &Types,
) -> Result<IndexSet<&'a str>> {
    let mut funcs = IndexSet::new();
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
                f.name,
                name,
                expected.params(),
                expected.results(),
                ty.params(),
                ty.results()
            );
        }

        funcs.insert(f.name.as_str());
    }

    Ok(funcs)
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
                            expected_ty.params(),
                            expected_ty.results(),
                            ty.params(),
                            ty.results()
                        ),
                        None => bail!(
                            "type mismatch for default interface function `{}`: expected `{:?} -> {:?}` but found `{:?} -> {:?}`",
                            f.name,
                            expected_ty.params(),
                            expected_ty.results(),
                            ty.params(),
                            ty.results()
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
