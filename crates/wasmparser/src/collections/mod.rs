//! Type aliases for maps used by `wasmparser`
//!
//! This module contains type aliases used for [`Map`], [`Set`],
//! [`IndexMap`], and [`IndexSet`]. Note that these differ from upstream types
//! in the `indexmap` crate and the standard library due to customization of the
//! hash algorithm type parameter.

mod random_state;

pub use self::random_state::{RandomState, RandomStateHasher};

/// Wasmparser-specific type alias for an ordered map.
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;

/// Wasmparser-specific type alias for an ordered set.
pub type IndexSet<K> = indexmap::IndexSet<K, RandomState>;

/// Wasmparser-specific type alias for map.
#[cfg(not(feature = "no-hash-maps"))]
pub type Map<K, V> = hashbrown::HashMap<K, V, RandomState>;

/// Wasmparser-specific type alias for map.
#[cfg(feature = "no-hash-maps")]
pub type Map<K, V> = alloc::collections::BTreeMap<K, V>;

/// Wasmparser-specific type alias for set.
#[cfg(not(feature = "no-hash-maps"))]
pub type Set<K> = hashbrown::HashSet<K, RandomState>;

/// Wasmparser-specific type alias for set.
#[cfg(feature = "no-hash-maps")]
pub type Set<K> = alloc::collections::BTreeSet<K>;
