use anyhow::{Context, Result};
use pretty_assertions::assert_eq;
use std::fs;
use std::path::Path;
use wit_component::{ComponentEncoder, StringEncoding};
use wit_parser::abi::{AbiVariant, WasmType};
use wit_parser::{Function, Interface, World};

/// Tests the encoding of the "types only" mode of `wit-component`.
///
/// This test looks in the `interfaces/` directory for test cases in a similar
/// format to those in the `components/` where there's a bunch of:
///
/// * `import-*.wit`
/// * `export-*.wit`
/// * `default.wit`
///
/// Where these represent the "world" of a component. Eventually this should
/// probably become just one file. For now though this suffices and plumbs
/// through the respective arguments of `wit-component`. The `*.wit` files are
/// encoded in "types only" mode and verified against `types_only.wat` and then
/// additionally encoded in normal mode with a dummy module to verify that works
/// as well.
///
/// Run the test with the environment variable `BLESS` set to update
/// the wat baseline file.
#[test]
fn interface_encoding() -> Result<()> {
    for entry in fs::read_dir("tests/interfaces")? {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }
        run_test(&path).context(format!("failed test `{}`", path.display()))?;
    }

    Ok(())
}

fn run_test(path: &Path) -> Result<()> {
    let test_case = path.file_stem().unwrap().to_str().unwrap();
    println!("test {test_case}");
    let world_path = path.join("world.wit");
    let world = World::parse_file(&world_path)?;
    let world_name = world.name.clone();

    let assert_output = |wasm: &[u8], wat: &Path| -> Result<()> {
        let output = wasmprinter::print_bytes(wasm)?;

        if std::env::var_os("BLESS").is_some() {
            fs::write(wat, output)?;
        } else {
            assert_eq!(
                fs::read_to_string(wat)?.replace("\r\n", "\n"),
                output,
                "encoding of `{test_case}` did not match the expected wat file `{}`",
                wat.display(),
            );
        }

        let decoded = wit_component::decode_world(&world_name, wasm)
            .context(format!("failed to decode bytes for test `{test_case}`"))?;

        if test_case == "empty" {
            return Ok(());
        }

        assert_eq!(decoded.imports.len(), world.imports.len());
        assert_eq!(decoded.exports.len(), world.exports.len());
        assert_eq!(decoded.default.is_some(), world.default.is_some());

        assert_wit(&world_path, &decoded)?;
        Ok(())
    };

    // Test a types-only component. This ensures that in "types only" mode we
    // can recover all the original `*.wit` interfaces from the generated
    // artifact.

    println!("testing types only");
    let bytes = ComponentEncoder::default()
        .types_only(true)
        .validate(true)
        .world(world.clone(), StringEncoding::UTF8)?
        .encode()
        .with_context(|| {
            format!("failed to encode a types-only component for test case `{test_case}`")
        })?;
    assert_output(&bytes, &path.join("types_only.wat"))?;

    // Test a full component with a dummy module as the implementation. This
    // tests a different path through `wit-component` to ensure that we can
    // recover the original `*.wit` interfaces from the component output.

    println!("test dummy module");
    let module = dummy_module(&world);
    let bytes = ComponentEncoder::default()
        .module(&module)?
        .validate(true)
        .world(world.clone(), StringEncoding::UTF8)?
        .encode()
        .with_context(|| format!("failed to encode a component for test case `{test_case}`"))?;
    assert_output(&bytes, &path.join("component.wat"))?;

    Ok(())
}

fn assert_wit(wit_path: &Path, world: &World) -> Result<()> {
    let mut printer = wit_component::WorldPrinter::default();
    let output = printer.print(world).context("failed to print interface")?;

    if std::env::var_os("BLESS").is_some() {
        fs::write(&wit_path, output)?;
    } else {
        assert_eq!(
            fs::read_to_string(&wit_path)?.replace("\r\n", "\n"),
            output,
            "encoding of wit file `{}` did not match the the decoded interface",
            wit_path.display(),
        );
    }
    Ok(())
}

fn dummy_module(world: &World) -> Vec<u8> {
    let mut wat = String::new();
    wat.push_str("(module\n");
    for (name, import) in world.imports.iter() {
        for func in import.functions.iter() {
            let sig = import.wasm_signature(AbiVariant::GuestImport, func);

            wat.push_str(&format!("(import \"{name}\" \"{}\" (func", func.name));
            push_tys(&mut wat, "param", &sig.params);
            push_tys(&mut wat, "result", &sig.results);
            wat.push_str("))\n");
        }
    }

    for (name, export) in world.exports.iter() {
        for func in export.functions.iter() {
            let name = func.core_export_name(Some(name));
            push_func(&mut wat, &name, export, func);
        }
    }

    if let Some(default) = &world.default {
        for func in default.functions.iter() {
            push_func(&mut wat, &func.name, default, func);
        }
    }

    wat.push_str("(memory (export \"memory\") 0)\n");
    wat.push_str(
        "(func (export \"cabi_realloc\") (param i32 i32 i32 i32) (result i32) unreachable)\n",
    );
    wat.push_str(")\n");

    return wat::parse_str(&wat).unwrap();

    fn push_func(wat: &mut String, name: &str, iface: &Interface, func: &Function) {
        let sig = iface.wasm_signature(AbiVariant::GuestExport, func);
        wat.push_str(&format!("(func (export \"{name}\")"));
        push_tys(wat, "param", &sig.params);
        push_tys(wat, "result", &sig.results);
        wat.push_str(" unreachable)\n");

        if iface.guest_export_needs_post_return(func) {
            wat.push_str(&format!("(func (export \"cabi_post_{name}\")"));
            push_tys(wat, "param", &sig.results);
            wat.push_str(")\n");
        }
    }

    fn push_tys(dst: &mut String, desc: &str, params: &[WasmType]) {
        if params.is_empty() {
            return;
        }
        dst.push_str(" (");
        dst.push_str(desc);
        for ty in params {
            dst.push_str(" ");
            match ty {
                WasmType::I32 => dst.push_str("i32"),
                WasmType::I64 => dst.push_str("i64"),
                WasmType::F32 => dst.push_str("f32"),
                WasmType::F64 => dst.push_str("f64"),
            }
        }
        dst.push_str(")");
    }
}
