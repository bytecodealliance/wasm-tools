//! Module for composition configuration.

use anyhow::Result;
use indexmap::IndexMap;
use std::{path::PathBuf, str::FromStr};

/// An explicit transitive dependency of a composed component.
#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde_derive::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(rename_all = "kebab-case", deny_unknown_fields)
)]
pub struct Dependency {
    /// The path to the dependency's component file.
    pub path: PathBuf,
}

impl FromStr for Dependency {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self { path: s.into() })
    }
}

/// An argument of an instantiation.
#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde_derive::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(rename_all = "kebab-case", deny_unknown_fields)
)]
pub struct InstantiationArg {
    /// The name of the instance passed as the argument.
    pub instance: String,

    /// The name of the instance export to use as the argument.
    ///
    /// If `None`, the instance itself will be used as the argument.
    #[cfg_attr(feature = "serde", serde(default))]
    pub export: Option<String>,
}

impl FromStr for InstantiationArg {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            instance: s.to_string(),
            export: None,
        })
    }
}

/// An instantiation of a component.
#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde_derive::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(rename_all = "kebab-case", deny_unknown_fields)
)]
pub struct Instantiation {
    /// The name of the dependency being instantiated.
    ///
    /// Defaults to a dependency with the same name as the instantiation.
    pub dependency: Option<String>,

    /// The explicit instantiation arguments.
    ///
    /// Maps the argument name to the name of the instance to pass as
    /// the argument.
    #[cfg_attr(feature = "serde", serde(default, deserialize_with = "de::index_map"))]
    pub arguments: IndexMap<String, InstantiationArg>,
}

/// The configuration for composing a WebAssembly component.
#[derive(Default, Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde_derive::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(rename_all = "kebab-case", deny_unknown_fields)
)]
pub struct Config {
    /// The path of the configuration file's directory.
    ///
    /// All paths are relative to this directory.
    #[cfg_attr(feature = "serde", serde(skip))]
    pub dir: PathBuf,

    /// Components whose exports define import dependencies to fulfill from.
    #[cfg_attr(feature = "serde", serde(default))]
    pub definitions: Vec<PathBuf>,

    /// The paths to search when automatically resolving dependencies.
    ///
    /// The config directory is always searched first.
    #[cfg_attr(feature = "serde", serde(default))]
    pub search_paths: Vec<PathBuf>,

    /// Whether or not to skip validation of the output component.
    #[cfg_attr(feature = "serde", serde(default))]
    pub skip_validation: bool,

    /// Whether or not to import components in the composed component.
    ///
    /// By default, components are defined rather than imported in
    /// the composed component.
    #[cfg_attr(feature = "serde", serde(default))]
    pub import_components: bool,

    /// Whether or not to disallow instance imports in the output component.
    ///
    /// Enabling this option will cause an error if a dependency cannot be
    /// located.
    #[cfg_attr(feature = "serde", serde(default))]
    pub disallow_imports: bool,

    /// The explicit, transitive dependencies of the root component.
    #[cfg_attr(feature = "serde", serde(default, deserialize_with = "de::index_map"))]
    pub dependencies: IndexMap<String, Dependency>,

    /// The explicit instantiations of the composed component.
    #[cfg_attr(feature = "serde", serde(default))]
    pub instantiations: IndexMap<String, Instantiation>,
}

impl Config {
    /// Reads a composition configuration from the given path.
    #[cfg(feature = "yaml")]
    pub fn from_file(path: impl Into<PathBuf>) -> Result<Self> {
        use anyhow::Context;
        use std::path::Path;

        let path = path.into();

        log::info!("reading configuration file `{}`", path.display());

        let config = std::fs::read_to_string(&path)
            .with_context(|| format!("failed to read configuration file `{}`", path.display()))?;

        let mut config: Config = serde_yaml2::from_str(&config)
            .with_context(|| format!("failed to parse configuration file `{}`", path.display()))?;

        config.dir = path.parent().map(Path::to_path_buf).unwrap_or_default();

        Ok(config)
    }

    /// Gets the dependency name for the given instance name.
    pub fn dependency_name<'a>(&'a self, instance: &'a str) -> &'a str {
        self.instantiations
            .get(instance)
            .and_then(|i| i.dependency.as_deref())
            .unwrap_or(instance)
    }
}

#[cfg(feature = "serde")]
mod de {
    use indexmap::IndexMap;
    use serde::{
        Deserialize, Deserializer,
        de::{self, MapAccess, Visitor},
    };
    use std::{fmt, hash::Hash, marker::PhantomData, str::FromStr};

    /// Utility function for deserializing index maps where the values can
    /// be deserialized either from a string or from a map value.
    pub fn index_map<'de, K, V, D>(deserializer: D) -> Result<IndexMap<K, V>, D::Error>
    where
        K: Hash + Eq + Deserialize<'de>,
        V: Deserialize<'de> + FromStr<Err = ()>,
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(MapVisitor(PhantomData))
    }

    struct MapVisitor<K, V>(PhantomData<(K, V)>);

    impl<'de, K, V> Visitor<'de> for MapVisitor<K, V>
    where
        K: Hash + Eq + Deserialize<'de>,
        V: Deserialize<'de> + FromStr<Err = ()>,
    {
        type Value = IndexMap<K, V>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("map")
        }

        fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
        where
            M: MapAccess<'de>,
        {
            struct Wrapper<V>(V);

            impl<'de, V> Deserialize<'de> for Wrapper<V>
            where
                V: Deserialize<'de> + FromStr<Err = ()>,
            {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: Deserializer<'de>,
                {
                    Ok(Self(
                        deserializer.deserialize_any(StringOrMapVisitor(PhantomData))?,
                    ))
                }
            }

            let mut map = Self::Value::with_capacity(access.size_hint().unwrap_or(0));
            while let Some((key, value)) = access.next_entry::<_, Wrapper<V>>()? {
                map.insert(key, value.0);
            }

            Ok(map)
        }
    }

    struct StringOrMapVisitor<V>(PhantomData<V>);

    impl<'de, V> Visitor<'de> for StringOrMapVisitor<V>
    where
        V: Deserialize<'de> + FromStr<Err = ()>,
    {
        type Value = V;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("string or map")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(V::from_str(value).unwrap())
        }

        fn visit_map<M>(self, access: M) -> Result<Self::Value, M::Error>
        where
            M: MapAccess<'de>,
        {
            Deserialize::deserialize(de::value::MapAccessDeserializer::new(access))
        }
    }
}
