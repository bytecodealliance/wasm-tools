//! A small crate to generate arbitrary WIT documents.
//!
//! This crate is modeled after the `wasm-smith` crate but is used to generate
//! WIT documents instead of WebAssembly modules. This crate is intended to
//! generate "interesting" WIT package structures in addition to interesting
//! type structures.

use arbitrary::{Result, Unstructured};
use wit_parser::{InvalidTransitiveDependency, Resolve};

mod config;
pub use self::config::Config;
mod generate;

/// Generates an arbitrary WIT document encoded as a WebAssembly binary.
///
/// The `config` guides the generation of the document and the `u` bytes are
/// used as input to construct the document.
pub fn smith(config: &Config, u: &mut Unstructured<'_>) -> Result<Vec<u8>> {
    let pkgs = generate::Generator::new(config.clone()).generate(u)?;
    let mut resolve = Resolve::default();
    resolve.all_features = true;
    let mut last = None;
    for pkg in pkgs {
        log::trace!("appending package {:?}", pkg.name);
        let group = pkg.sources.parse().unwrap();
        let id = match resolve.push_group(group) {
            Ok(id) => id,
            Err(e) => {
                if e.is::<InvalidTransitiveDependency>() {
                    return Err(arbitrary::Error::IncorrectFormat);
                }
                let err = e.to_string();
                if err.contains("shadows previously") || err.contains("mismatch in stability") {
                    log::error!("{e}");
                    return Err(arbitrary::Error::IncorrectFormat);
                }
                panic!("bad wit parse: {e:?}")
            }
        };
        last = Some(id);
    }
    let pkg = last.unwrap();

    Ok(wit_component::encode(&resolve, pkg).expect("failed to encode WIT document"))
}
