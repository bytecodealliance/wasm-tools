use wit_parser::abi::{AbiVariant, WasmType};
use wit_parser::{
    Function, Mangling, Resolve, ResourceIntrinsic, TypeDefKind, TypeId, WasmExport, WasmImport,
    WorldId, WorldItem, WorldKey,
};

/// Generate a dummy implementation core Wasm module for a given WIT document
pub fn dummy_module(resolve: &Resolve, world: WorldId, mangling: Mangling) -> Vec<u8> {
    let world = &resolve.worlds[world];
    let mut wat = String::new();
    wat.push_str("(module\n");
    for (name, import) in world.imports.iter() {
        match import {
            WorldItem::Function(func) => {
                let sig = resolve.wasm_signature(AbiVariant::GuestImport, func);

                let (module, name) = resolve.wasm_import_name(
                    mangling,
                    WasmImport::Func {
                        interface: None,
                        func,
                    },
                );

                wat.push_str(&format!("(import {module:?} {name:?} (func"));
                push_tys(&mut wat, "param", &sig.params);
                push_tys(&mut wat, "result", &sig.results);
                wat.push_str("))\n");
            }
            WorldItem::Interface { id: import, .. } => {
                for (_, func) in resolve.interfaces[*import].functions.iter() {
                    let sig = resolve.wasm_signature(AbiVariant::GuestImport, func);

                    let (module, name) = resolve.wasm_import_name(
                        mangling,
                        WasmImport::Func {
                            interface: Some(name),
                            func,
                        },
                    );
                    wat.push_str(&format!("(import {module:?} {name:?} (func"));
                    push_tys(&mut wat, "param", &sig.params);
                    push_tys(&mut wat, "result", &sig.results);
                    wat.push_str("))\n");
                }
                for (_, ty) in resolve.interfaces[*import].types.iter() {
                    push_resource_func_imports(&mut wat, resolve, Some(name), *ty, mangling);
                }
            }
            WorldItem::Type(id) => {
                push_resource_func_imports(&mut wat, resolve, None, *id, mangling);
            }
        }
    }

    // Import any resource-related functions for exports.
    for (name, export) in world.exports.iter() {
        let export = match export {
            WorldItem::Interface { id, .. } => *id,
            _ => continue,
        };
        for resource in resolve.interfaces[export].types.values().copied() {
            let ty = &resolve.types[resource];
            match ty.kind {
                TypeDefKind::Resource => {}
                _ => continue,
            }
            let intrinsics = [
                (ResourceIntrinsic::ExportedDrop, "(func (param i32))"),
                (
                    ResourceIntrinsic::ExportedNew,
                    "(func (param i32) (result i32))",
                ),
                (
                    ResourceIntrinsic::ExportedRep,
                    "(func (param i32) (result i32))",
                ),
            ];
            for (intrinsic, sig) in intrinsics {
                let (module, name) = resolve.wasm_import_name(
                    mangling,
                    WasmImport::ResourceIntrinsic {
                        interface: Some(name),
                        resource,
                        intrinsic,
                    },
                );
                wat.push_str(&format!("(import {module:?} {name:?} {sig})\n"));
            }
        }
    }

    for (name, export) in world.exports.iter() {
        match export {
            WorldItem::Function(func) => {
                push_func_export(&mut wat, resolve, None, func, mangling);
            }
            WorldItem::Interface { id: export, .. } => {
                for (_, func) in resolve.interfaces[*export].functions.iter() {
                    push_func_export(&mut wat, resolve, Some(name), func, mangling);
                }

                // Feign destructors for any resource that this interface
                // exports
                for resource in resolve.interfaces[*export].types.values().copied() {
                    let ty = &resolve.types[resource];
                    match ty.kind {
                        TypeDefKind::Resource => {}
                        _ => continue,
                    }
                    let name = resolve.wasm_export_name(
                        mangling,
                        WasmExport::ResourceDtor {
                            interface: name,
                            resource,
                        },
                    );
                    wat.push_str(&format!("(func (export {name:?}) (param i32))"));
                }
            }
            WorldItem::Type(_) => {}
        }
    }

    let memory = resolve.wasm_export_name(mangling, WasmExport::Memory);
    wat.push_str(&format!("(memory (export {memory:?}) 0)\n"));
    let realloc = resolve.wasm_export_name(mangling, WasmExport::Realloc);
    wat.push_str(&format!(
        "(func (export {realloc:?}) (param i32 i32 i32 i32) (result i32) unreachable)\n"
    ));
    let initialize = resolve.wasm_export_name(mangling, WasmExport::Initialize);
    wat.push_str(&format!("(func (export {initialize:?}))"));
    wat.push_str(")\n");

    return wat::parse_str(&wat).unwrap();

    fn push_resource_func_imports(
        wat: &mut String,
        resolve: &Resolve,
        interface: Option<&WorldKey>,
        resource: TypeId,
        mangling: Mangling,
    ) {
        let ty = &resolve.types[resource];
        match ty.kind {
            TypeDefKind::Resource => {}
            _ => return,
        }
        let (module, name) = resolve.wasm_import_name(
            mangling,
            WasmImport::ResourceIntrinsic {
                interface,
                resource,
                intrinsic: ResourceIntrinsic::ImportedDrop,
            },
        );
        wat.push_str(&format!("(import {module:?} {name:?} (func (param i32)))"));
    }

    fn push_func_export(
        wat: &mut String,
        resolve: &Resolve,
        interface: Option<&WorldKey>,
        func: &Function,
        mangling: Mangling,
    ) {
        let sig = resolve.wasm_signature(AbiVariant::GuestExport, func);
        let name = resolve.wasm_export_name(
            mangling,
            WasmExport::Func {
                interface,
                func,
                post_return: false,
            },
        );
        wat.push_str(&format!("(func (export \"{name}\")"));
        push_tys(wat, "param", &sig.params);
        push_tys(wat, "result", &sig.results);
        wat.push_str(" unreachable)\n");

        let name = resolve.wasm_export_name(
            mangling,
            WasmExport::Func {
                interface,
                func,
                post_return: true,
            },
        );
        wat.push_str(&format!("(func (export \"{name}\")"));
        push_tys(wat, "param", &sig.results);
        wat.push_str(")\n");
    }

    fn push_tys(dst: &mut String, desc: &str, params: &[WasmType]) {
        if params.is_empty() {
            return;
        }
        dst.push_str(" (");
        dst.push_str(desc);
        for ty in params {
            dst.push(' ');
            match ty {
                WasmType::I32 => dst.push_str("i32"),
                WasmType::I64 => dst.push_str("i64"),
                WasmType::F32 => dst.push_str("f32"),
                WasmType::F64 => dst.push_str("f64"),
                WasmType::Pointer => dst.push_str("i32"),
                WasmType::PointerOrI64 => dst.push_str("i64"),
                WasmType::Length => dst.push_str("i32"),
            }
        }
        dst.push(')');
    }
}
