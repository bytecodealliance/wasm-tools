//! Type definitions for a default set.

#[cfg(not(feature = "no-hash-maps"))]
use crate::collections::hash;

/// A default set of values.
#[cfg(not(feature = "no-hash-maps"))]
pub type Set<K> = hashbrown::HashSet<K, hash::RandomState>;

/// A default set of values.
#[cfg(feature = "no-hash-maps")]
pub type Set<K> = alloc::collections::BTreeSet<K>;
