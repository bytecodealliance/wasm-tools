//! Type definitions for an ordered set.

use crate::collections::hash;

/// Wasmparser-specific type alias for an ordered set.
pub type IndexSet<K> = indexmap::IndexSet<K, hash::RandomState>;
