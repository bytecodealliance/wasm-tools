use std::{
    collections::HashMap,
    fmt::Write,
    path::{Path, PathBuf},
    sync::OnceLock,
};

use tryfn::{Case, Harness};
use wasm_wave::{
    parser::ParserError,
    untyped::UntypedFuncCall,
    value::{resolve_wit_func_type, FuncType, Value},
    wasm::{DisplayValue, WasmFunc},
};
use wit_parser::Resolve;

fn setup(path: PathBuf) -> Case {
    let name = format!("ui::{}", path.file_stem().unwrap().to_string_lossy());
    let expected = tryfn::Data::read_from(&path.with_extension("out"), None);
    Case {
        name,
        fixture: path,
        expected,
    }
}

fn test(path: &Path) -> Result<String, Box<dyn std::error::Error>> {
    let filename = path.file_name().unwrap().to_string_lossy();
    let inputs = std::fs::read_to_string(path)?;
    let mut output = String::new();
    let out = &mut output;
    let inputs = inputs.trim_end().trim_end_matches(';');
    for mut input in inputs.split(";\n") {
        // Copy leading comments into the output
        while input.starts_with("//") {
            let Some((comment, remainder)) = input.split_once('\n') else {
                break;
            };
            input = remainder;
            writeln!(out, "{comment}")?;
            continue;
        }

        fn parse_func_call(
            input: &str,
        ) -> Result<(String, &'static FuncType, Vec<Value>), ParserError> {
            let untyped_call = UntypedFuncCall::parse(input)?;
            let func_name = untyped_call.name().to_string();
            let func_type = get_func_type(&func_name).unwrap_or_else(|| {
                panic!("unknown test func {func_name:?}");
            });
            let param_types = func_type.params().collect::<Vec<_>>();
            let values = untyped_call.to_wasm_params::<Value>(&param_types)?;
            Ok((func_name, func_type, values))
        }

        match parse_func_call(input) {
            Ok((func_name, func_type, values)) => {
                assert!(
                    !filename.starts_with("reject-"),
                    "accepted input {input:?} in {filename}"
                );
                write!(out, "{func_name}(")?;
                let mut first = true;
                for (name, value) in func_type.param_names().zip(values) {
                    if first {
                        first = false;
                    } else {
                        write!(out, ", ")?;
                    }
                    write!(out, "{name}: {value}", value = DisplayValue(&value))?;
                }
                writeln!(out, ")")?;
            }
            Err(err) => {
                assert!(
                    !filename.starts_with("accept-"),
                    "rejected input {input:?} in {filename}: {err:#}"
                );
                writeln!(out, "{err}")?;
            }
        }
    }
    Ok(output)
}

fn get_func_type(func_name: &str) -> Option<&'static FuncType> {
    static FUNC_TYPES: OnceLock<HashMap<String, FuncType>> = OnceLock::new();
    FUNC_TYPES
        .get_or_init(|| {
            let mut resolve = Resolve::new();
            resolve.push_path("tests/ui/ui.wit").unwrap();
            resolve
                .interfaces
                .iter()
                .flat_map(|(_, i)| &i.functions)
                .map(|(name, func)| (name.clone(), resolve_wit_func_type(&resolve, func).unwrap()))
                .collect::<HashMap<_, _>>()
        })
        .get(func_name)
}

fn main() {
    use snapbox::assert::{Action, Assert};
    let action = match std::env::var("BLESS").unwrap_or_default().as_str() {
        "" => Action::Skip,
        _ => Action::Overwrite,
    };
    Harness::new("tests/ui", setup, test)
        .select(["*.waves"])
        .with_assert(Assert::new().action(action))
        .test();
}
