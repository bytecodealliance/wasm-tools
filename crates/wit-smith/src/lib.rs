//! A small crate to generate arbitrary WIT documents.
//!
//! This crate is modeled after the `wasm-smith` crate but is used to generate
//! WIT documents instead of WebAssembly modules. This crate is intended to
//! generate "interesting" WIT package structures in addition to interesting
//! type structures.

use arbitrary::{Result, Unstructured};
use std::collections::HashMap;
use wit_parser::Resolve;

mod config;
pub use self::config::Config;
mod generate;

/// Generates an arbitrary WIT document encoded as a WebAssembly binary.
///
/// The `config` guides the generation of the document and the `u` bytes are
/// used as input to construct the document.
pub fn smith(config: &Config, u: &mut Unstructured<'_>) -> Result<Vec<u8>> {
    let pkgs = generate::Generator::new(config.clone()).gen(u)?;
    let mut resolve = Resolve::default();
    let mut deps = HashMap::new();
    let mut last = None;
    for pkg in pkgs {
        let url = format!("my-scheme:/{}", pkg.name);
        let unresolved = pkg.sources.parse(&pkg.name, Some(&url)).unwrap();
        let id = match resolve.push(unresolved, &deps) {
            Ok(id) => id,
            Err(e) => {
                let err = e.to_string();
                if err.contains("conflicts with a previous")
                    || err.contains("shadows previously imported")
                    || err.contains("shadows previously exported")
                {
                    return Err(arbitrary::Error::IncorrectFormat);
                }
                panic!("bad wit parse: {e:?}")
            }
        };
        let prev = deps.insert(pkg.name, id);
        assert!(prev.is_none());
        last = Some(id);
    }
    let pkg = last.unwrap();

    Ok(wit_component::encode(&resolve, pkg).expect("failed to encode WIT document"))
}
