//! Type definitions for an ordered set.

use crate::collections::IndexMap;
use core::{borrow::Borrow, hash::Hash, iter::FusedIterator, ops::Index};

/// A default set of values.
///
/// Provides an API compatible with both [`IndexSet`] and a custom implementation based on [`BTreeMap`].
///
/// [`IndexSet`]: indexmap::IndexSet
/// [`BTreeMap`]: alloc::collections::BTreeMap
#[derive(Debug, Clone)]
pub struct IndexSet<T> {
    inner: IndexMap<T, ()>,
}

impl<T> Default for IndexSet<T> {
    fn default() -> Self {
        Self {
            inner: IndexMap::default(),
        }
    }
}

impl<T> IndexSet<T> {
    /// Clears the [`IndexSet`], removing all elements.
    pub fn clear(&mut self) {
        self.inner.clear()
    }

    /// Returns the number of elements in the [`IndexSet`].
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns `true` if the [`IndexSet`] contains no elements.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Returns an iterator that yields the items in the [`IndexSet`].
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            inner: self.inner.iter(),
        }
    }
}

impl<T> IndexSet<T>
where
    T: Eq + Hash + Ord + Clone,
{
    /// Reserves capacity for at least `additional` more elements to be inserted in the [`IndexSet`].
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// Returns true if the [`IndexSet`] contains an element equal to the `value`.
    pub fn contains<Q: ?Sized>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + Ord,
    {
        self.inner.contains_key(value)
    }

    /// Returns a reference to the element in the [`IndexSet`], if any, that is equal to the `value`.
    pub fn get<Q: ?Sized>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + Ord,
    {
        self.inner.get_key_value(value).map(|(x, &())| x)
    }

    /// Adds `value` to the [`IndexSet`].
    ///
    /// Returns whether the value was newly inserted:
    ///
    /// - Returns `true` if the set did not previously contain an equal value.
    /// - Returns `false` otherwise and the entry is not updated.
    pub fn insert(&mut self, value: T) -> bool {
        self.inner.insert(value, ()).is_none()
    }

    /// Remove the value from the [`IndexSet`], and return `true` if it was present.
    ///
    /// Like [`Vec::swap_remove`], the value is removed by swapping it with the
    /// last element of the set and popping it off. **This perturbs
    /// the position of what used to be the last element!**
    ///
    /// Return `false` if `value` was not in the set.
    ///
    /// Computes in **O(1)** time (average).
    ///
    /// [`Vec::swap_remove`]: alloc::vec::Vec::swap_remove
    pub fn swap_remove<Q: ?Sized>(&mut self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + Ord,
    {
        self.inner.swap_remove(value).is_some()
    }

    /// Adds a value to the [`IndexSet`], replacing the existing value, if any, that is equal to the given
    /// one. Returns the replaced value.
    pub fn replace(&mut self, value: T) -> Option<T> {
        let removed = self.inner.swap_remove_entry(&value);
        self.inner.insert(value, ());
        removed.map(|(key, _value)| key)
    }

    /// Returns `true` if `self` has no elements in common with `other`.
    /// This is equivalent to checking for an empty intersection.
    pub fn is_disjoint(&self, other: &Self) -> bool {
        if self.len() <= other.len() {
            self.iter().all(move |value| !other.contains(value))
        } else {
            other.iter().all(move |value| !self.contains(value))
        }
    }

    /// Returns `true` if the [`IndexSet`] is a subset of another,
    /// i.e., `other` contains at least all the values in `self`.
    pub fn is_subset(&self, other: &Self) -> bool {
        self.len() <= other.len() && self.iter().all(move |value| other.contains(value))
    }

    /// Returns `true` if the [`IndexSet`] is a superset of another,
    /// i.e., `self` contains at least all the values in `other`.
    pub fn is_superset(&self, other: &Self) -> bool {
        other.is_subset(self)
    }
}

impl<T> Index<usize> for IndexSet<T>
where
    T: Hash + Eq + Ord,
{
    type Output = T;

    fn index(&self, index: usize) -> &T {
        let Some((value, _)) = self.inner.get_index(index) else {
            panic!("out of bounds index: {index}");
        };
        value
    }
}

impl<T> FromIterator<T> for IndexSet<T>
where
    T: Hash + Eq + Ord + Clone,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        Self {
            inner: <IndexMap<T, ()>>::from_iter(iter.into_iter().map(|value| (value, ()))),
        }
    }
}

impl<'a, T> IntoIterator for &'a IndexSet<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> Extend<T> for IndexSet<T>
where
    T: Hash + Eq + Ord + Clone,
{
    fn extend<Iter: IntoIterator<Item = T>>(&mut self, iter: Iter) {
        self.inner.extend(iter.into_iter().map(|value| (value, ())))
    }
}

/// An iterator over the items of a [`IndexSet`].
#[derive(Debug, Clone)]
pub struct Iter<'a, T> {
    inner: <&'a IndexMap<T, ()> as IntoIterator>::IntoIter,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(key, _value)| key)
    }
}

impl<'a, T> ExactSizeIterator for Iter<'a, T> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<'a, T> FusedIterator for Iter<'a, T> {}

impl<T> IntoIterator for IndexSet<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.inner.into_iter(),
        }
    }
}

/// An iterator over the owned items of an [`IndexSet`].
#[derive(Debug)]
pub struct IntoIter<T> {
    inner: <IndexMap<T, ()> as IntoIterator>::IntoIter,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(key, _value)| key)
    }
}

impl<T> ExactSizeIterator for IntoIter<T> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<T> FusedIterator for IntoIter<T> {}
