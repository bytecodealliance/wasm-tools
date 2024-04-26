//! Type definitions for a default map.

#[cfg(not(feature = "no-hash-maps"))]
use crate::collections::hash;

/// A default key-value mapping.
#[cfg(not(feature = "no-hash-maps"))]
pub type Map<K, V> = hashbrown::HashMap<K, V, hash::RandomState>;

/// A default key-value mapping.
#[cfg(feature = "no-hash-maps")]
pub type Map<K, V> = alloc::collections::BTreeMap<K, V>;

/// A view into a single entry in a map, which may either be vacant or occupied.
///
/// This enum is constructed from the entry method on [`Map`].
#[cfg(not(feature = "no-hash-maps"))]
pub type Entry<'a, K, V> = hashbrown::hash_map::Entry<'a, K, V, hash::RandomState>;

/// A view into a single entry in a map, which may either be vacant or occupied.
///
/// This enum is constructed from the entry method on [`Map`].
#[cfg(feature = "no-hash-maps")]
pub type Entry<'a, K, V> = alloc::collections::btree_map::Entry<'a, K, V>;
