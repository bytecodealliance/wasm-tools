use anyhow::Result;
use clap::Parser;
use std::path::PathBuf;
use wit_parser::{PackageId, Resolve};

#[derive(Parser)]
pub struct WitResolve {
    /// Path to WIT files to load.
    ///
    /// This can be a directory containing `*.wit` files, a `*.wit` file itself,
    /// or a `*.wasm` file which is a WIT package encoded as WebAssembly.
    pub wit: PathBuf,

    /// Features to enable when parsing the `wit` option.
    ///
    /// This flag enables the `@unstable` feature in WIT documents where the
    /// items are otherwise hidden by default.
    #[clap(long)]
    pub features: Vec<String>,

    /// Enable all features when parsing the `wit` option.
    ///
    /// This flag enables all `@unstable` features in WIT documents where the
    /// items are otherwise hidden by default.
    #[clap(long)]
    pub all_features: bool,
}

impl WitResolve {
    pub fn resolve_with_features(features: &[String], all_features: bool) -> Resolve {
        let mut resolve = Resolve::default();
        resolve.all_features = all_features;
        for feature in features {
            for f in feature.split_whitespace() {
                for f in f.split(',').filter(|s| !s.is_empty()) {
                    resolve.features.insert(f.to_string());
                }
            }
        }
        return resolve;
    }

    pub fn load(&self) -> Result<(Resolve, PackageId)> {
        let mut resolve = Self::resolve_with_features(&self.features, self.all_features);
        let (pkg_id, _) = resolve.push_path(&self.wit)?;
        Ok((resolve, pkg_id))
    }
}
