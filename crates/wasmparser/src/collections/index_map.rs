//! Type definitions for an ordered map.

use crate::collections::hash;

/// Wasmparser-specific type for an ordered map.
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, hash::RandomState>;
