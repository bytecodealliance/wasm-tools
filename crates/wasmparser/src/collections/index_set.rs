//! Type definitions for an ordered set.

use crate::collections::hash;

/// A hash set where the iteration order of the values is independent of their hash values.
pub type IndexSet<K> = indexmap::IndexSet<K, hash::RandomState>;
