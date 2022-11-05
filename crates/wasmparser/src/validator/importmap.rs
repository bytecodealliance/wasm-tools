use super::types::EntityType;
use hashbrown::raw::RawTable;
use std::collections::hash_map::RandomState;
use std::hash::{BuildHasher, Hash, Hasher};

/// An ordered map of the imports of a Wasm module.
///
/// It's possible for a Wasm module to have multiple imports with the same module/name pair, since
/// the Wasm itself only refers to imports by their index. For example, the following is a
/// perfectly legal module (represented in the [WebAssembly text format]):
/// ```text
/// (module
///   (import "env" "" (func))
///   (import "env" "" (func (param i32))))
/// ```
///
/// As such, this type provides [`get()`][ImportMap::get] for retrieving the **first** import with
/// a given module/name pair, as well as an [iterator](ImportMap::iter) that returns each import
/// entry in the order they were declared in the original module.
///
/// [WebAssembly text format]: https://webassembly.github.io/spec/core/text/index.html
pub struct ImportMap {
    /// Just a Vec of the import entries in the order they were declared
    entries: Vec<(Key, EntityType)>,
    /// A hashtable, where the values stored in the buckets are indices into the entries Vec,
    /// and the hash values are that of the Key stored at entries[index].
    ///
    /// Only the index of the first entry to have a given Key will be stored here.
    /// e.g., the imports section:
    /// ```text
    /// (module
    ///   (import "a" "a" (func $0))
    ///   (import "a" "b" (func $1))
    ///   (import "a" "a" (func $2)) ; same import key as $0!
    ///   (import "b" "c" (func $3)))
    /// ```
    /// will be represented as:
    /// ```ignore
    /// ImportMap {
    ///     entries: [
    ///         (("a", "a"), func_0),
    ///         (("a", "b"), func_1),
    ///         (("a", "a"), func_2),
    ///         (("b", "c"), func_3),
    ///     ],
    ///     table: {
    ///         hash("a", "a") => 0,
    ///         hash("a", "b") => 1,
    ///         hash("b", "c") => 3,
    ///     },
    /// }
    /// ```
    table: RawTable<usize>,
    /// BuildHasher for the hashtable. Can this be a faster hash function?
    builder: RandomState,
}

/// (module, name)
type Key = (String, String);

impl ImportMap {
    pub(super) fn new() -> Self {
        Self {
            entries: Vec::new(),
            table: RawTable::new(),
            builder: Default::default(),
        }
    }

    /// Hash a Key with this map's hasher.
    fn hash(builder: &RandomState, key: (&str, &str)) -> u64 {
        // BuildHasher::hash_one once stabilized?
        let mut hasher = builder.build_hasher();
        key.hash(&mut hasher);
        hasher.finish()
    }

    /// Add an import entry to the map.
    pub(super) fn add(&mut self, module: String, name: String, entity: EntityType) {
        // add the entry to the entries vec
        let index = self.entries.len();
        self.entries.push(((module, name), entity));
        let ((module, name), _) = &self.entries[index];

        if let (hash, None) = self.get_index(module, name) {
            // if the index for the entry wasn't in the hashtable already, add it
            self.table.insert(hash, index, |&index| {
                let ((module, name), _) = &self.entries[index];
                Self::hash(&self.builder, (&**module, &**name))
            });
        }
    }

    /// Lookup the index of the import entry, and also return the hash of the key.
    fn get_index(&self, module: &str, name: &str) -> (u64, Option<usize>) {
        let key = (module, name);
        let hash = Self::hash(&self.builder, key);
        let index = self.table.get(hash, |&index| {
            let ((module, name), _) = &self.entries[index];
            key == (&**module, &**name)
        });
        (hash, index.copied())
    }

    /// Lookup an import given its module and name.
    ///
    /// Note that if there were multiple imports with the given module and name, only the first in
    /// order of its declaration in the imports section will be returned.
    pub fn get(&self, module: &str, name: &str) -> Option<EntityType> {
        let (_, index) = self.get_index(module, name);
        index.map(|index| {
            let (_, entity) = &self.entries[index];
            *entity
        })
    }

    /// Iterate over all the entries in this `ImportMap`.
    ///
    /// The iterator returns tuples of the module string, the name string, and the `EntityType` of
    /// each import.
    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&'_ str, &'_ str, EntityType)> {
        self.into_iter()
    }

    /// Returns the total number of imports in the map.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns true if the map holds no imports.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'a> IntoIterator for &'a ImportMap {
    type IntoIter = ImportMapIter<'a>;
    type Item = <Self::IntoIter as Iterator>::Item;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        ImportMapIter {
            inner: self.entries.iter(),
        }
    }
}

/// An iterator over an `ImportMap`. See [`ImportMap::iter()`] for details.
pub struct ImportMapIter<'a> {
    inner: std::slice::Iter<'a, (Key, EntityType)>,
}

impl<'a> Iterator for ImportMapIter<'a> {
    type Item = (&'a str, &'a str, EntityType);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|((m, n), e)| (&**m, &**n, *e))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl ExactSizeIterator for ImportMapIter<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }
}
