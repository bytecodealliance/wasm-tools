//! Type definitions for an ordered map.

use crate::collections::hash;

/// A hash table where the iteration order of the key-value pairs is independent of the hash values of the keys.
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, hash::RandomState>;

/// Entry for an existing key-value pair in an [`IndexMap`] or a vacant location to insert one.
pub type Entry<'a, K, V> = indexmap::map::Entry<'a, K, V>;
