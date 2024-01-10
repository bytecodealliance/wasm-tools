use arbitrary::{Result, Unstructured};

// Define a fuzz target that accepts arbitrary
// `Module`s or `Component`s as input.
pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    // We want to prioritize fuzzing of modules for the time being
    // so we'll only generate a component 10% of the time
    //
    // TODO: remove this `false && ...` once this fuzzer works again for
    // components.
    let generate_component = false
        && match u.ratio::<u8>(1, 10) {
            Ok(b) => b,
            Err(_) => false,
        };
    let (wasm_bytes, config) = if generate_component {
        crate::generate_valid_component(u, |c, u| {
            c.max_components = u.int_in_range(0..=1_000)?;
            c.max_instances = u.int_in_range(0..=1_000)?;
            c.max_values = u.int_in_range(0..=1_000)?;
            Ok(())
        })?
    } else {
        crate::generate_valid_module(u, |_, _| Ok(()))?
    };

    // Validate the module or component and assert that it passes validation.
    let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        component_model: generate_component,
        multi_value: config.multi_value_enabled,
        multi_memory: config.max_memories > 1,
        bulk_memory: config.bulk_memory_enabled,
        reference_types: config.reference_types_enabled,
        simd: config.simd_enabled,
        relaxed_simd: config.relaxed_simd_enabled,
        memory64: config.memory64_enabled,
        threads: config.threads_enabled,
        exceptions: config.exceptions_enabled,
        // TODO: determine our larger story for function-references in
        // wasm-tools and whether we should just have a Wasm GC flag since
        // function-references is effectively part of the Wasm GC proposal at
        // this point.
        function_references: config.gc_enabled,
        gc: config.gc_enabled,
        ..wasmparser::WasmFeatures::default()
    });
    if let Err(e) = validator.validate_all(&wasm_bytes) {
        let component_or_module = if generate_component {
            "component"
        } else {
            "module"
        };
        panic!("Invalid {}: {}", component_or_module, e);
    }

    // After validation make sure that binary-to-text and text-to-binary
    // transforms all work as well.
    let wat_string = wasmprinter::print_bytes(&wasm_bytes).unwrap_or_else(|e| {
        panic!(
            "failed first disassembly of Wasm into wat with `wasmprinter::print_bytes`: {}",
            e
        )
    });

    let wasm_bytes = wat::parse_str(&wat_string).unwrap_or_else(|e| {
        panic!(
            "failed to assemble wat into Wasm with `wat::parse_str`: {}",
            e
        )
    });
    if log::log_enabled!(log::Level::Debug) {
        log::debug!("Writing roundtripped wasm to `test2.wasm`...");
        std::fs::write("test2.wasm", &wasm_bytes).unwrap();
    }

    let wat_string2 = wasmprinter::print_bytes(&wasm_bytes).unwrap_or_else(|e| {
        panic!(
            "failed second disassembly of Wasm into wat with `wasmprinter::print_bytes`: {}",
            e
        )
    });
    if log::log_enabled!(log::Level::Debug) {
        log::debug!("Writing round tripped text format to `test2.wat`...");
        std::fs::write("test2.wat", &wat_string2).unwrap();
    }

    if wat_string != wat_string2 {
        panic!("failed to roundtrip valid module");
    }
    Ok(())
}
