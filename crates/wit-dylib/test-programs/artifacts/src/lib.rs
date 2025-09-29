include!(concat!(env!("OUT_DIR"), "/out.rs"));

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use tempfile::TempDir;
use wit_component::Linker;
use wit_parser::{Resolve, WorldId};

pub fn compose(
    tempdir: &TempDir,
    resolve: &Resolve,
    caller: (&Path, WorldId),
    callee: (&Path, WorldId),
) -> Result<PathBuf> {
    // First load up the `caller` and link it all together into a component.
    let (_caller, caller_file) =
        create_component(tempdir, &resolve, caller).context("failed to link caller together")?;

    // Next do the same for the `callee`
    let (_callee, callee_file) =
        create_component(tempdir, &resolve, callee).context("failed to link callee together")?;

    // Use `wasm-compose` to create a single component where `caller` imports
    // the `callee`.
    let config = wasm_compose::config::Config {
        definitions: vec![callee_file],
        ..Default::default()
    };
    let composition = wasm_compose::composer::ComponentComposer::new(&caller_file, &config)
        .compose()
        .context("failed to compose")?;

    let composition_file = tempdir.path().join("composition.wasm");
    std::fs::write(&composition_file, &composition)
        .context("failed to write `composition.wasm`")?;

    Ok(composition_file)
}

fn create_component(
    tempdir: &TempDir,
    resolve: &Resolve,
    wasm: (&Path, WorldId),
) -> Result<(Vec<u8>, PathBuf)> {
    let mut adapter = wit_dylib::create(resolve, wasm.1, None);
    let name = &resolve.worlds[wasm.1].name;

    wit_component::embed_component_metadata(
        &mut adapter,
        resolve,
        wasm.1,
        wit_component::StringEncoding::UTF8,
    )?;

    let adapter_file = tempdir.path().join(format!("{name}_adapter.wasm"));
    std::fs::write(&adapter_file, &adapter).context("failed to write `callee.wasm`")?;
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
        .validate_all(&adapter)
        .with_context(|| format!("adapter is invalid {adapter_file:?}"))?;

    let mut linker = Linker::default();

    // First define our caller/callee component with its own filename.
    linker = linker
        .library(
            wasm.0.file_name().unwrap().to_str().unwrap(),
            &std::fs::read(wasm.0).context("failed to read wasm")?,
            false,
        )
        .context("failed to link wasm as library")?;

    // Next insert the synthesized adapter that is what we're primarily testing
    // in this file.
    linker = linker
        .library(&format!("{name}-interpreter-adapter.wasm"), &adapter, false)
        .context("failed to link adapter as library")?;

    // Next load up `libc.so` as we specified in the compilation of the original
    // file that it should be linked dynamically.
    linker = linker
        .library(
            "libc.so",
            &std::fs::read(LIBC_SO).context("failed to read libc.so")?,
            false,
        )
        .context("failed to link adapter as library")?;

    // Rust-sourced compiles still, as of the time of this writing, require the
    // WASIp1 adapter. Load that in here.
    linker = linker
        .adapter(
            "wasi_snapshot_preview1",
            wasi_preview1_component_adapter_provider::WASI_SNAPSHOT_PREVIEW1_REACTOR_ADAPTER,
        )
        .context("failed to load adapter")?;

    let wasm = linker.encode().with_context(|| {
        format!(
            "failed to link:\n\
original wasm: {wasm:?}\n\
adapter:       {adapter_file:?}\n\
libc:          {LIBC_SO:?}\n\
",
        )
    })?;

    let wasm_file = tempdir.path().join(format!("{name}.wasm"));
    std::fs::write(&wasm_file, &wasm).with_context(|| format!("failed to write {wasm_file:?}"))?;
    Ok((wasm, wasm_file))
}
