//! Module for CLI configuration.

use anyhow::{anyhow, Context, Result};
use indexmap::IndexMap;
use serde::Deserialize;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

/// An import for a composed WebAssembly component.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Import {
    /// The name of the import.
    ///
    /// Defaults to the name used in the import mapping.
    pub name: Option<String>,

    /// The path to the import.
    pub path: PathBuf,

    /// Whether or not to embed the import in the composed component.
    #[serde(default)]
    pub embed: bool,
}

/// An instantiation in a composed WebAssembly component.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Instantiation {
    /// The name of the import being instantiated.
    ///
    /// Defaults to the name specified in the instantiation map.
    pub import: Option<String>,

    /// The list of instantiation dependencies.
    ///
    /// Instantiation arguments are implicitly mapped to these dependencies.
    #[serde(default)]
    pub dependencies: Vec<String>,

    /// The explicit instantiation arguments.
    #[serde(default, rename = "with")]
    pub arguments: IndexMap<String, String>,
}

/// Exports from a composed WebAssembly component.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Exports {
    /// The name of the default export instance.
    ///
    /// If specified, all exports from the given instance will be exported
    /// from the composed component.
    pub default: Option<String>,
}

/// The configuration for composing a WebAssembly component.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Config {
    /// The path of the configuration file.
    ///
    /// Imports should be resolved relative to this path.
    #[serde(skip)]
    pub path: PathBuf,

    /// The output file name for the composed component.
    pub output: Option<PathBuf>,

    /// Whether or not to skip validation of the output component.
    #[serde(default)]
    pub skip_validation: bool,

    /// The imports for the composed component.
    #[serde(default)]
    pub imports: IndexMap<String, Import>,

    /// The instantiations for the composed component.
    #[serde(default)]
    pub instantiations: IndexMap<String, Instantiation>,

    /// The instantiations for the composed component.
    #[serde(default)]
    pub exports: Exports,
}

impl Config {
    /// Reads a composition configuration from the given path.
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();

        log::debug!("reading configuration file `{}`", path.display());

        let config = fs::read_to_string(path)
            .with_context(|| format!("failed to read configuration file `{}`", path.display()))?;

        let mut config: Config = match path.extension().and_then(OsStr::to_str) {
            Some("yml") | Some("yaml") => serde_yaml::from_str(&config).map_err(|e| anyhow!(e)),
            _ => toml::from_str(&config).map_err(|e| anyhow!(e)),
        }
        .with_context(|| format!("failed to parse configuration file `{}`", path.display()))?;

        config.path = match path.parent() {
            Some(p) => p.to_path_buf(),
            None => std::env::current_dir()?,
        };

        Ok(config)
    }
}
