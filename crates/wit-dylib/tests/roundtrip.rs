//! A test that exercises sending arbitrary WIT values across an import/export
//! boundary and ensuring that they are transmitted succesfully.
//!
//! This test will use the `roundtrip_caller.rs` file to invoke functions in
//! `roundtrip_callee.rs`. These precompiled components, provided through the
//! `artifacts` crate, work in close conjunction with this test file to work. At
//! a high level what happens here is:
//!
//! 1. An arbitrary WIT package set is generated with `wit-smith`.
//! 2. This WIT is augmented with a custom interface for running the test
//!    and various other intrinsics, use for the test harness.
//! 3. Both Rust components use this augmented WIT to get turned into a
//!    component. Notably the `caller` world is used to bind
//!    `roundtrip_caller.rs` and similarly for `callee`. Each component is
//!    linked with `wit_component::Linker`
//! 4. Both components are composed together into a single component (where the
//!    caller component imports the callee component).
//! 5. The component is run in Wasmtime with a number of iterations and a seed.
//! 6. The caller component uses the seed to pick an arbitrary import and then
//!    invokes it.
//! 7. The caller component uses the seed to generate arbitrary values to pass
//!    to the import.
//! 8. The callee uses the same seed as the caller to ensure that the received
//!    values are the same as the ones that it generates.
//! 9. The same procedure works in reverse for the result.
//! 10. The caller double-checks that it did not leak memory, nor did the
//!     callee, during this invocation.
//!
//! This provides end-to-end testing that arbitrarily shaped WIT values with
//! arbitrarily shaped runtime values in arbitrary positions all get
//! communicated correctly. Basically this is intended to provide a high degree
//! of confidence that the bindings generated are actually correct and don't
//! leak memory.

use arbitrary::{Result, Unstructured};
use indexmap::IndexMap;
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;
use wit_parser::decoding::DecodedWasm;
use wit_parser::{
    Function, FunctionKind, Handle, Interface, Package, PackageName, Resolve, Type, TypeDef,
    TypeDefKind, TypeOwner, World, WorldItem, WorldKey,
};

#[test]
fn run() {
    let _ = env_logger::try_init();
    if cfg!(target_family = "wasm") {
        return;
    }

    arbtest::arbtest(run_one)
        // To repro...
        // .seed(0x573d795000000078)
        .run();
}

fn run_one(u: &mut Unstructured<'_>) -> Result<()> {
    println!("iter...");
    let seed = u.arbitrary::<u64>()?;
    let iters = 200;
    let mut config = u.arbitrary::<wit_smith::Config>()?;
    config.error_context = false;
    config.fixed_size_list = false;
    config.futures = false; // TODO
    config.streams = false; // TODO
    config.async_ = false;
    let wasm = wit_smith::smith(&config, u)?;
    std::fs::write("./hello.wasm", &wasm).unwrap();
    let (mut resolve, _pkg) = match wit_parser::decoding::decode(&wasm).unwrap() {
        DecodedWasm::WitPackage(resolve, pkg) => (resolve, pkg),
        DecodedWasm::Component(..) => unreachable!(),
    };
    update_resources(&mut resolve);

    let interfaces = resolve
        .packages
        .iter()
        .flat_map(|(_, pkg)| pkg.interfaces.values().cloned())
        .collect::<Vec<_>>();

    let world_items = interfaces
        .iter()
        .map(|id| {
            (
                WorldKey::Interface(*id),
                WorldItem::Interface {
                    id: *id,
                    stability: Default::default(),
                },
            )
        })
        .collect::<IndexMap<_, _>>();

    // Create a new package which represents the custom intrinsics used to
    // implement the test.
    let package = resolve.packages.alloc(Package {
        name: PackageName {
            namespace: "wit-dylib".to_string(),
            name: "roundtrip-test".to_string(),
            version: None,
        },
        interfaces: Default::default(),
        worlds: Default::default(),
        docs: Default::default(),
    });

    // Inject an interface in this package which the caller imports and the
    // callee exports. There are a few functions such as:
    //
    // * `allocated-bytes` - used to let the caller know how many bytes are
    //   allocated in the callee to help detect leaks.
    // * `set-seed` - at the start of the test informs the callee of the
    //   original seed to ensure that both the callee and caller are using the
    //   same one.
    // * `checkpoint` - used to double-check after a call that the random state
    //   is the same in the caller and callee.
    let alloc = resolve.interfaces.alloc(Interface {
        name: Some("alloc".to_string()),
        stability: Default::default(),
        package: Some(package),
        docs: Default::default(),
        types: Default::default(),
        functions: {
            let mut funcs = IndexMap::new();
            funcs.insert(
                "allocated-bytes".to_string(),
                Function {
                    name: "allocated-bytes".to_string(),
                    kind: FunctionKind::Freestanding,
                    params: Vec::new(),
                    result: Some(Type::U32),
                    stability: Default::default(),
                    docs: Default::default(),
                },
            );
            funcs.insert(
                "set-seed".to_string(),
                Function {
                    name: "set-seed".to_string(),
                    kind: FunctionKind::Freestanding,
                    params: vec![("seed".to_string(), Type::U64)],
                    result: None,
                    stability: Default::default(),
                    docs: Default::default(),
                },
            );
            funcs.insert(
                "checkpoint".to_string(),
                Function {
                    name: "checkpoint".to_string(),
                    kind: FunctionKind::Freestanding,
                    params: Vec::new(),
                    result: Some(Type::U32),
                    stability: Default::default(),
                    docs: Default::default(),
                },
            );
            funcs
        },
    });

    // Generate two worlds in our custom package, one for the callee and one for
    // the caller which differ only in their name and whether the interfaces are
    // imported or exported.
    let callee = resolve.worlds.alloc(World {
        name: "callee".to_string(),
        stability: Default::default(),
        package: Some(package),
        exports: world_items.clone(),
        imports: Default::default(),
        includes: Default::default(),
        include_names: Default::default(),
        docs: Default::default(),
    });
    let caller = resolve.worlds.alloc(World {
        name: "caller".to_string(),
        stability: Default::default(),
        package: Some(package),
        imports: world_items,
        exports: Default::default(),
        includes: Default::default(),
        include_names: Default::default(),
        docs: Default::default(),
    });

    // Add an extra import/export for our synthesized interfaces as well.
    resolve.worlds[callee].exports.insert(
        WorldKey::Interface(alloc),
        WorldItem::Interface {
            id: alloc,
            stability: Default::default(),
        },
    );
    resolve.worlds[caller].imports.insert(
        WorldKey::Interface(alloc),
        WorldItem::Interface {
            id: alloc,
            stability: Default::default(),
        },
    );

    // Inject the actual entrypoint of the test.
    resolve.worlds[caller].exports.insert(
        WorldKey::Name("run".to_string()),
        WorldItem::Function(Function {
            name: "run".to_string(),
            kind: FunctionKind::Freestanding,
            params: vec![
                ("iters".to_string(), Type::U32),
                ("seed".to_string(), Type::U64),
            ],
            result: None,
            stability: Default::default(),
            docs: Default::default(),
        }),
    );

    // Fixup the WIT data structures in memory.
    resolve.packages[package]
        .interfaces
        .insert("alloc".to_string(), alloc);
    resolve.packages[package]
        .worlds
        .insert("callee".to_string(), callee);
    resolve.packages[package]
        .worlds
        .insert("caller".to_string(), caller);

    // For debugging, print the WIT being used.
    if false {
        let mut printer = wit_component::WitPrinter::default();
        printer
            .print(
                &resolve,
                package,
                &resolve
                    .packages
                    .iter()
                    .map(|(id, _)| id)
                    .filter(|i| *i != package)
                    .collect::<Vec<_>>(),
            )
            .unwrap();
        println!("{}", printer.output);
    }

    // Run the test by composing the caller/callee together and running the
    // result in Wasmtime.
    let mut tempdir =
        TempDir::new_in(Path::new(artifacts::ROUNDTRIP_CALLER).parent().unwrap()).unwrap();
    tempdir.disable_cleanup(true);

    let composition = artifacts::compose(
        &tempdir,
        &resolve,
        (artifacts::ROUNDTRIP_CALLER.as_ref(), caller),
        (artifacts::ROUNDTRIP_CALLEE.as_ref(), callee),
    )
    .expect("failed to compose");

    let mut cmd = Command::new("wasmtime");
    cmd.arg("run")
        .arg(format!("--invoke=run({iters}, {seed})"))
        .arg("-Shttp")
        .arg("-Wcomponent-model-async")
        .arg("-Wcomponent-model-error-context")
        .arg(&composition);
    let result = cmd.output().expect("failed to run wasmtime");
    if result.status.success() {
        tempdir.disable_cleanup(false);
        return Ok(());
    }
    let mut error = String::new();
    error.push_str(&format!("command: {cmd:?}\n"));
    error.push_str(&format!("status:  {}\n", result.status));
    if !result.stdout.is_empty() {
        error.push_str(&format!(
            "stdout:\n  {}\n",
            String::from_utf8_lossy(&result.stdout).replace("\n", "\n  ")
        ));
    }
    if !result.stderr.is_empty() {
        error.push_str(&format!(
            "stderr:\n  {}\n",
            String::from_utf8_lossy(&result.stderr).replace("\n", "\n  ")
        ));
    }

    panic!("{error}")
}

/// Updates all resources found in `resolve` to ensure that the constructor
/// takes a `u32` and there's a function to learn the `rep`.
fn update_resources(resolve: &mut Resolve) {
    let interface_resources = resolve
        .interfaces
        .iter()
        .flat_map(|(id, iface)| {
            iface
                .types
                .iter()
                .filter(|(_name, id)| {
                    let ty = &resolve.types[**id];
                    matches!(ty.kind, TypeDefKind::Resource)
                })
                .map(move |(name, resource_id)| (id, *resource_id, name.clone()))
        })
        .collect::<Vec<_>>();

    for (interface_id, resource_id, resource_name) in interface_resources {
        let own = resolve.types.alloc(TypeDef {
            name: None,
            kind: TypeDefKind::Handle(Handle::Own(resource_id)),
            owner: TypeOwner::None,
            docs: Default::default(),
            stability: Default::default(),
        });
        let borrow = resolve.types.alloc(TypeDef {
            name: None,
            kind: TypeDefKind::Handle(Handle::Borrow(resource_id)),
            owner: TypeOwner::None,
            docs: Default::default(),
            stability: Default::default(),
        });
        let iface = &mut resolve.interfaces[interface_id];
        let ctor = format!("[constructor]{resource_name}");
        let rep = format!("[method]{resource_name}.rep");
        iface.functions.swap_remove(&ctor);
        iface.functions.swap_remove(&rep);

        iface.functions.insert(
            ctor.clone(),
            Function {
                name: ctor,
                kind: FunctionKind::Constructor(resource_id),
                params: vec![("rep".to_string(), Type::U32)],
                result: Some(Type::Id(own)),
                stability: Default::default(),
                docs: Default::default(),
            },
        );
        iface.functions.insert(
            rep.clone(),
            Function {
                name: rep,
                kind: FunctionKind::Method(resource_id),
                params: vec![("self".to_string(), Type::Id(borrow))],
                result: Some(Type::U32),
                stability: Default::default(),
                docs: Default::default(),
            },
        );
    }
}
