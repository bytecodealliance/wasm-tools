//! Type definitions for a default set.

#[cfg(not(feature = "no-hash-maps"))]
use crate::collections::hash;

/// Wasmparser-specific type for set.
#[cfg(not(feature = "no-hash-maps"))]
pub type Set<K> = hashbrown::HashSet<K, hash::RandomState>;

/// Wasmparser-specific type for set.
#[cfg(feature = "no-hash-maps")]
pub type Set<K> = alloc::collections::BTreeSet<K>;
