//! Type definitions for a default map.

#[cfg(not(feature = "no-hash-maps"))]
use crate::collections::hash;

/// Wasmparser-specific type alias for map.
#[cfg(not(feature = "no-hash-maps"))]
pub type Map<K, V> = hashbrown::HashMap<K, V, hash::RandomState>;

/// Wasmparser-specific type alias for map.
#[cfg(feature = "no-hash-maps")]
pub type Map<K, V> = alloc::collections::BTreeMap<K, V>;
