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
    let mut validator = crate::validator_for_config(&config);
    if let Err(e) = validator.validate_all(&wasm_bytes) {
        let component_or_module = if generate_component {
            "component"
        } else {
            "module"
        };
        panic!("Invalid {}: {}", component_or_module, e);
    }

    // Round-trip `wasm_bytes` through text and back to binary.
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
    crate::log_wasm(&wasm_bytes, &config);

    let mut wat_string2 = String::new();
    // Now round-trip the result one more time, but this time with "folded
    // instructions" (e.g. s-expressions in the text format).
    wasmprinter::Config::new()
        .fold_instructions(true)
        .print(
            &wasm_bytes,
            &mut wasmprinter::PrintFmtWrite(&mut wat_string2),
        )
        .unwrap_or_else(|e| {
            panic!(
                "failed second disassembly of Wasm into wat with `wasmprinter::print_bytes`: {}",
                e
            )
        });
    let wasm_bytes2 = wat::parse_str(&wat_string2).unwrap_or_else(|e| {
        panic!(
            "failed to assemble wat into Wasm with `wat::parse_str`: {}",
            e
        )
    });
    crate::log_wasm(&wasm_bytes2, &config);

    if wasm_bytes != wasm_bytes2 {
        panic!("failed to roundtrip valid module");
    }
    Ok(())
}
