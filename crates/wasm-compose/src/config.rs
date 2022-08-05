//! Module for composition configuration.

use anyhow::{Context, Result};
use indexmap::IndexMap;
use serde::Deserialize;
use std::{
    fs,
    path::{Path, PathBuf},
};

/// A component defined or imported in a composed WebAssembly component.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Component {
    /// The path to the component file.
    pub path: PathBuf,

    /// The name to import the component with.
    ///
    /// By default, components are defined (embedded) in the composed component.
    /// By specifying an import name, the component will be imported instead.
    pub import: Option<String>,
}

/// An instantiation of a component.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Instantiation {
    /// The name of the component being instantiated.
    ///
    /// Defaults to a component with the same name as the instantiation.
    pub component: Option<String>,

    /// The explicit instantiation arguments.
    ///
    /// Maps the argument name to the name of the instance use pass as
    /// the argument.
    #[serde(default, rename = "with")]
    pub arguments: IndexMap<String, String>,

    /// The list of instantiation dependencies.
    ///
    /// Each entry in the list is the name of an instantiation.
    ///
    /// The list of dependencies are used to automatically satisfy
    /// unspecified instantiation arguments for the instantiation.
    #[serde(default)]
    pub dependencies: Vec<String>,
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

    /// The components of the composed component.
    #[serde(default)]
    pub components: IndexMap<String, Component>,

    /// The instantiations of the composed component.
    #[serde(default)]
    pub instantiations: IndexMap<String, Instantiation>,

    /// The exports of the composed component.
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

        let mut config: Config = serde_yaml::from_str(&config)
            .with_context(|| format!("failed to parse configuration file `{}`", path.display()))?;

        config.path = match path.parent() {
            Some(p) => p.to_path_buf(),
            None => std::env::current_dir()?,
        };

        Ok(config)
    }
}
