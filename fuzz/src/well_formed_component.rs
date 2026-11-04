use anyhow::bail;
use arbitrary::{Result, Unstructured};
use wasm_smith::WellFormedComponent;
use wasmparser::{ComponentName, KnownCustom, Parser, Payload, Validator, WasmFeatures};

// Generates components from the component model's binary grammar, which must
// decode, and checks that the valid ones round-trip through the text format.
pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    let mut config: wasm_smith::Config = u.arbitrary()?;
    config.max_modules = u.int_in_range(0..=10)?;
    config.max_components = u.int_in_range(0..=10)?;
    config.max_instances = u.int_in_range(0..=100)?;
    config.generate_custom_sections = u.arbitrary()?;
    // Mostly near-valid components, which get furthest into validation.
    config.component_steering_percent = match u.ratio(3, 4)? {
        true => u.int_in_range(80..=100)?,
        false => u.int_in_range(0..=100)?,
    };
    config.name_pool_size = u.int_in_range(1..=100)?;
    config.name_reuse_percent = u.int_in_range(0..=100)?;
    config.name_collision_percent = match u.ratio(1, 4)? {
        true => u.int_in_range(1..=100)?,
        false => 0,
    };
    let wasm = WellFormedComponent::new(config.clone(), u)?.to_bytes();
    crate::log_wasm(&wasm, &config);

    if let Err(e) = decode(&wasm) {
        panic!("failed to decode well-formed component: {e:?}");
    }

    let mut cfg = wasmprinter::Config::new();
    cfg.print_skeleton(u.arbitrary()?);
    cfg.name_unnamed(u.arbitrary()?);
    let _ = cfg.print(&wasm, &mut wasmprinter::PrintFmtWrite(&mut String::new()));

    let features = if u.arbitrary()? {
        WasmFeatures::all()
    } else {
        WasmFeatures::from_bits_truncate(u.arbitrary()?)
    };
    if Validator::new_with_features(features)
        .validate_all(&wasm)
        .is_err()
    {
        return Ok(());
    }
    let wasm1 = roundtrip(&wasm);
    if let Err(e) = Validator::new_with_features(features).validate_all(&wasm1) {
        panic!("printed component is invalid: {e}");
    }
    let wasm2 = roundtrip(&wasm1);
    if wasm1 != wasm2 {
        panic!("failed to roundtrip valid component");
    }
    Ok(())
}

fn roundtrip(wasm: &[u8]) -> Vec<u8> {
    let wat = wasmprinter::print_bytes(wasm)
        .unwrap_or_else(|e| panic!("failed to print valid component: {e}"));
    wat::parse_str(&wat).unwrap_or_else(|e| panic!("failed to parse printed component: {e}"))
}

/// Decodes every item of every component section, including the
/// `component-name` section.
fn decode(wasm: &[u8]) -> anyhow::Result<()> {
    fn items<T>(reader: impl IntoIterator<Item = wasmparser::Result<T>>) -> anyhow::Result<()> {
        for item in reader {
            item?;
        }
        Ok(())
    }

    for payload in Parser::new(0).parse_all(wasm) {
        match payload? {
            Payload::InstanceSection(s) => items(s)?,
            Payload::CoreTypeSection(s) => items(s)?,
            Payload::ComponentInstanceSection(s) => items(s)?,
            Payload::ComponentAliasSection(s) => items(s)?,
            Payload::ComponentTypeSection(s) => items(s)?,
            Payload::ComponentCanonicalSection(s) => items(s)?,
            Payload::ComponentImportSection(s) => items(s)?,
            Payload::ComponentExportSection(s) => items(s)?,
            Payload::CustomSection(s) => {
                if let KnownCustom::ComponentName(names) = s.as_known() {
                    for name in names {
                        match name? {
                            ComponentName::Component { .. } => {}
                            ComponentName::CoreFuncs(map)
                            | ComponentName::CoreGlobals(map)
                            | ComponentName::CoreMemories(map)
                            | ComponentName::CoreTables(map)
                            | ComponentName::CoreTags(map)
                            | ComponentName::CoreModules(map)
                            | ComponentName::CoreInstances(map)
                            | ComponentName::CoreTypes(map)
                            | ComponentName::Types(map)
                            | ComponentName::Instances(map)
                            | ComponentName::Components(map)
                            | ComponentName::Funcs(map)
                            | ComponentName::Values(map) => items(map)?,
                            ComponentName::Unknown { ty, .. } => {
                                bail!("unknown component name subsection {ty}")
                            }
                        }
                    }
                }
            }
            Payload::UnknownSection { id, .. } => bail!("unknown section {id}"),
            _ => {}
        }
    }
    Ok(())
}

#[test]
fn smoke() {
    super::test::test_n_times(100, run);
}
