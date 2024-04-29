use arbitrary::{Result, Unstructured};
use wasmparser::{Parser, Validator, WasmFeatures};

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    // Either use `wasm-smith` to generate a module with possibly invalid
    // functions or try validating raw bytes from the input itself.
    if u.arbitrary()? {
        validate_maybe_invalid_module(u)?;
    } else {
        validate_raw_bytes(u)?;
    }
    Ok(())
}

pub fn validate_maybe_invalid_module(u: &mut Unstructured<'_>) -> Result<()> {
    // Generate a "valid" module but specifically allow invalid functions which
    // means that some functions may be defined from the input bytes raw. This
    // means that most of the module is valid but only some functions may be
    // invalid which can help stress various bits and pieces of validation.
    let (wasm, config) = crate::generate_valid_module(u, |config, _| {
        config.allow_invalid_funcs = true;
        Ok(())
    })?;
    validate_all(crate::validator_for_config(&config), &wasm);
    Ok(())
}

pub fn validate_raw_bytes(u: &mut Unstructured<'_>) -> Result<()> {
    // Enable arbitrary combinations of features to validate the input bytes.
    let validator = Validator::new_with_features(WasmFeatures::from_bits_truncate(u.arbitrary()?));
    let wasm = u.bytes(u.len())?;
    crate::log_wasm(wasm, "");
    validate_all(validator, wasm);
    Ok(())
}

fn validate_all(mut validator: Validator, wasm: &[u8]) {
    for payload in Parser::new(0).parse_all(wasm) {
        let payload = match payload {
            Ok(p) => p,
            Err(_) => return,
        };

        if validator.payload(&payload).is_err() {
            return;
        }

        // Check that the payload's range is in bounds, since the payload is
        // supposedly valid.
        use wasmparser::Payload::*;
        match payload {
            Version { range, .. } => assert!(wasm.get(range).is_some()),
            TypeSection(s) => assert!(wasm.get(s.range()).is_some()),
            ImportSection(s) => assert!(wasm.get(s.range()).is_some()),
            FunctionSection(s) => assert!(wasm.get(s.range()).is_some()),
            TableSection(s) => assert!(wasm.get(s.range()).is_some()),
            MemorySection(s) => assert!(wasm.get(s.range()).is_some()),
            TagSection(s) => assert!(wasm.get(s.range()).is_some()),
            GlobalSection(s) => assert!(wasm.get(s.range()).is_some()),
            ExportSection(s) => assert!(wasm.get(s.range()).is_some()),
            StartSection { range, .. } => assert!(wasm.get(range).is_some()),
            ElementSection(s) => assert!(wasm.get(s.range()).is_some()),
            DataCountSection { range, .. } => assert!(wasm.get(range).is_some()),
            DataSection(s) => assert!(wasm.get(s.range()).is_some()),
            CodeSectionStart { range, .. } => assert!(wasm.get(range).is_some()),
            CodeSectionEntry(body) => assert!(wasm.get(body.range()).is_some()),
            InstanceSection(s) => assert!(wasm.get(s.range()).is_some()),
            CoreTypeSection(s) => assert!(wasm.get(s.range()).is_some()),
            ComponentInstanceSection(s) => assert!(wasm.get(s.range()).is_some()),
            ComponentAliasSection(s) => assert!(wasm.get(s.range()).is_some()),
            ComponentTypeSection(s) => assert!(wasm.get(s.range()).is_some()),
            ComponentCanonicalSection(s) => assert!(wasm.get(s.range()).is_some()),
            ComponentStartSection { range, .. } => assert!(wasm.get(range).is_some()),
            ComponentImportSection(s) => assert!(wasm.get(s.range()).is_some()),
            ComponentExportSection(s) => assert!(wasm.get(s.range()).is_some()),
            CustomSection(s) => assert!(wasm.get(s.range()).is_some()),
            UnknownSection { range, .. } => assert!(wasm.get(range).is_some()),

            // In order to support streaming parsing and validation, these
            // sections' ranges are not checked during validation, since they
            // contain nested sections and we don't want to require all nested
            // sections are present before we can parse/validate any of them.
            ComponentSection {
                unchecked_range: _, ..
            }
            | ModuleSection {
                unchecked_range: _, ..
            } => {}

            // No associated range.
            End(_) => {}
        }
    }
}
