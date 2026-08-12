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
    validate_all(u, crate::validator_for_config(&config), &wasm)
}

pub fn validate_raw_bytes(u: &mut Unstructured<'_>) -> Result<()> {
    // Enable arbitrary combinations of features to validate the input bytes.
    let validator = Validator::new_with_features(WasmFeatures::from_bits_truncate(u.arbitrary()?));
    let wasm = u.bytes(u.len())?;
    crate::log_wasm(wasm, "");
    validate_all(u, validator, wasm)
}

fn validate_all(u: &mut Unstructured<'_>, mut validator: Validator, wasm: &[u8]) -> Result<()> {
    // First try printing this module. Generate a random configuration for
    // printing and then see what happens. Mostly making sure nothing panics
    // here.
    let mut cfg = wasmprinter::Config::new();
    cfg.fold_instructions(u.arbitrary()?);
    cfg.print_skeleton(u.arbitrary()?);
    cfg.print_offsets(u.arbitrary()?);
    cfg.name_unnamed(u.arbitrary()?);
    log::debug!("print config {cfg:?}");
    let mut wat = String::new();
    let _ = cfg.print(wasm, &mut wasmprinter::PrintFmtWrite(&mut wat));
    let is_valid_range = |r: std::ops::Range<u64>| {
        r.start <= r.end && usize::try_from(r.end).is_ok_and(|end| end <= wasm.len())
    };

    // After printing then try to parse and validate the module. See how far we
    // get as invalid modules are explicitly allowed here. Generally looking for
    // panics and excessive resource usage here.
    for payload in Parser::new(0).parse_all(wasm) {
        let payload = match payload {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        if validator.payload(&payload).is_err() {
            return Ok(());
        }

        // Check that the payload's range is in bounds, since the payload is
        // supposedly valid.
        use wasmparser::Payload::*;
        match payload {
            Version { range, .. } => assert!(is_valid_range(range)),
            TypeSection(s) => assert!(is_valid_range(s.range())),
            ImportSection(s) => assert!(is_valid_range(s.range())),
            FunctionSection(s) => assert!(is_valid_range(s.range())),
            TableSection(s) => assert!(is_valid_range(s.range())),
            MemorySection(s) => assert!(is_valid_range(s.range())),
            TagSection(s) => assert!(is_valid_range(s.range())),
            GlobalSection(s) => assert!(is_valid_range(s.range())),
            ExportSection(s) => assert!(is_valid_range(s.range())),
            StartSection { range, .. } => assert!(is_valid_range(range)),
            ElementSection(s) => assert!(is_valid_range(s.range())),
            DataCountSection { range, .. } => assert!(is_valid_range(range)),
            DataSection(s) => assert!(is_valid_range(s.range())),
            CodeSectionStart { range, .. } => assert!(is_valid_range(range)),
            CodeSectionEntry(body) => assert!(is_valid_range(body.range())),
            InstanceSection(s) => assert!(is_valid_range(s.range())),
            CoreTypeSection(s) => assert!(is_valid_range(s.range())),
            ComponentInstanceSection(s) => assert!(is_valid_range(s.range())),
            ComponentAliasSection(s) => assert!(is_valid_range(s.range())),
            ComponentTypeSection(s) => assert!(is_valid_range(s.range())),
            ComponentCanonicalSection(s) => assert!(is_valid_range(s.range())),
            ComponentStartSection { range, .. } => assert!(is_valid_range(range)),
            ComponentImportSection(s) => assert!(is_valid_range(s.range())),
            ComponentExportSection(s) => assert!(is_valid_range(s.range())),
            CustomSection(s) => assert!(is_valid_range(s.range())),
            UnknownSection { range, .. } => assert!(is_valid_range(range)),

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

            _ => {}
        }
    }

    Ok(())
}

#[test]
fn smoke() {
    super::test::test_n_times(100, run);
}
