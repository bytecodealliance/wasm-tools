//! An ordered set based on a B-Tree that keeps insertion order of elements.

pub type IndexSetImpl<T> = IndexSet<T>;
pub type IterImpl<'a, T> = Iter<'a, T>;
pub type IntoIterImpl<T> = IntoIter<T>;

use crate::collections::index_map::detail;
use core::borrow::Borrow;
use core::iter::FusedIterator;
use core::ops::Index;

/// A b-tree set where the iteration order of the values
/// is independent of the ordering of the values.
///
/// The interface is closely compatible with the [`indexmap` crate]
/// and a subset of the features that is relevant for the
/// [`wasmparser-nostd` crate].
///
/// # Differences to original `IndexSet`
///
/// Since the goal of this crate was to maintain a simple
/// `no_std` compatible fork of the [`indexmap` crate] there are some
/// downsides and differences.
///
/// - Some operations such as `IndexSet::insert` now require `K: Clone`.
/// - It is to be expected that this fork performs worse than the original
/// [`indexmap` crate] implementation.
/// - The implementation is based on `BTreeMap` internally instead of
/// `HashMap` which has the effect that methods no longer require `K: Hash`
/// but `K: Ord` instead.
///
/// [`indexmap` crate]: https://crates.io/crates/indexmap
/// [`wasmparser-nostd` crate]: https://crates.io/crates/wasmparser-nostd
#[derive(Debug, Clone)]
pub struct IndexSet<T> {
    inner: detail::IndexMapImpl<T, ()>,
}

impl<T> Default for IndexSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IndexSet<T> {
    /// Makes a new, empty [`IndexSet`].
    ///
    /// Does not allocate anything on its own.
    pub fn new() -> Self {
        Self {
            inner: detail::IndexMapImpl::new(),
        }
    }

    /// Constructs a new, empty [`IndexSet`] with at least the specified capacity.
    ///
    /// Does not allocate if `capacity` is zero.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: detail::IndexMapImpl::with_capacity(capacity),
        }
    }

    /// Reserve capacity for at least `additional` more values in the [`IndexSet`].
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// Returns the number of elements in the [`IndexSet`].
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns `true` if the [`IndexSet`] contains no elements.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Gets an iterator that visits the elements in the [`IndexSet`]
    /// in the order in which they have been inserted into the set unless
    /// there have been removals.
    pub fn iter(&self) -> Iter<T> {
        Iter {
            iter: self.inner.iter(),
        }
    }

    /// Clears the [`IndexSet`], removing all elements.
    pub fn clear(&mut self) {
        self.inner.clear();
    }
}

impl<T> IndexSet<T>
where
    T: Ord,
{
    /// Returns `true` if `self` has no elements in common with `other`.
    /// This is equivalent to checking for an empty intersection.
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.iter().all(|value| !other.contains(value))
            && other.iter().all(|value| !self.contains(value))
    }

    /// Returns `true` if the [`IndexSet`] is a subset of another,
    /// i.e., `other` contains at least all the elements in `self`.
    pub fn is_subset(&self, other: &Self) -> bool {
        self.iter().all(|value| other.contains(value))
    }

    /// Returns `true` if the [`IndexSet`] is a superset of another,
    /// i.e., `self` contains at least all the elements in `other`.
    pub fn is_superset(&self, other: &Self) -> bool {
        other.is_subset(self)
    }

    /// Returns `true` if the [`IndexSet`] contains an element equal to the value.
    ///
    /// The value may be any borrowed form of the set's element type,
    /// but the ordering on the borrowed form *must* match the
    /// ordering on the element type.
    pub fn contains<Q: ?Sized>(&self, key: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Ord,
    {
        self.inner.contains_key(key)
    }

    /// Returns a reference to the element in the [`IndexSet`], if any, that is equal to
    /// the value.
    ///
    /// The value may be any borrowed form of the set's element type,
    /// but the ordering on the borrowed form *must* match the
    /// ordering on the element type.
    pub fn get<Q: ?Sized>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: Ord,
    {
        self.inner.get_full(value).map(|(_index, value, _)| value)
    }

    /// Returns the index-value pair corresponding to the supplied value.
    ///
    /// The supplied key may be any borrowed form of the map's key type,
    /// but the ordering on the borrowed form *must* match the ordering
    /// on the key type.
    pub fn get_full<Q: ?Sized>(&self, value: &Q) -> Option<(usize, &T)>
    where
        T: Borrow<Q>,
        Q: Ord,
    {
        self.inner
            .get_full(value)
            .map(|(index, value, _)| (index, value))
    }

    /// Returns the unique index corresponding to the supplied value.
    ///
    /// The supplied key may be any borrowed form of the map's key type,
    /// but the ordering on the borrowed form *must* match the ordering
    /// on the key type.
    pub fn get_index_of<Q: ?Sized>(&self, value: &Q) -> Option<usize>
    where
        T: Borrow<Q>,
        Q: Ord,
    {
        self.inner.get_index_of(value)
    }

    /// Returns a shared reference to the value at the given index.
    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.inner.get_index(index).map(|(value, _)| value)
    }

    /// Adds a value to the [`IndexSet`].
    ///
    /// Returns whether the value was newly inserted. That is:
    ///
    /// - If the set did not previously contain an equal value, `true` is
    ///   returned.
    /// - If the set already contained an equal value, `false` is returned, and
    ///   the entry is not updated.
    pub fn insert(&mut self, value: T) -> bool
    where
        T: Clone,
    {
        self.insert_full(value).1
    }

    /// Adds a value to the [`IndexSet`].
    ///
    /// Returns the unique index to the value as well as a `bool` flag telling
    /// whether the value was newly inserted. That is:
    ///
    /// - If the set did not previously contain an equal value, `true` is
    ///   returned.
    /// - If the set already contained an equal value, `false` is returned, and
    ///   the entry is not updated.
    pub fn insert_full(&mut self, value: T) -> (usize, bool)
    where
        T: Clone,
    {
        let (index, value) = self.inner.insert_full(value, ());
        (index, value.is_some())
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
    pub fn swap_remove<Q>(&mut self, key: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Ord,
    {
        self.inner.swap_remove_full(key).is_some()
    }

    /// Adds a value to the [`IndexSet`], replacing the existing value, if any, that is
    /// equal to the given one, without altering its insertion order. Returns
    /// the replaced value.
    pub fn replace(&mut self, value: T) -> Option<T>
    where
        T: Clone,
    {
        // Note: this implementation does not really replace the value but instead
        //       clones the old one which to the outside has similar effects. Since
        //       values here are the keys of the underlying map their `Ord` and `Eq`
        //       must be the same for `get_full` to find them.
        self.inner
            .get_full(&value)
            .map(|(_index, value, _)| value.clone())
    }
}

impl<T> Index<usize> for IndexSet<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.inner
            .get_index(index)
            .map(|(value, _)| value)
            .unwrap_or_else(|| panic!("index out of bounds: {index}"))
    }
}

impl<'a, T> Extend<&'a T> for IndexSet<T>
where
    T: Ord + Copy,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = &'a T>,
    {
        self.extend(iter.into_iter().map(|value| *value))
    }
}

impl<T> Extend<T> for IndexSet<T>
where
    T: Ord + Clone,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        iter.into_iter().for_each(move |value| {
            self.insert(value);
        });
    }
}

impl<T> FromIterator<T> for IndexSet<T>
where
    T: Ord + Clone,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut set = IndexSet::new();
        set.extend(iter);
        set
    }
}

impl<T, const N: usize> From<[T; N]> for IndexSet<T>
where
    T: Ord + Clone,
{
    fn from(items: [T; N]) -> Self {
        items.into_iter().collect()
    }
}

impl<'a, T> IntoIterator for &'a IndexSet<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> IntoIterator for IndexSet<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            iter: self.inner.into_iter(),
        }
    }
}

/// An iterator over the items of a [`IndexSet`].
///
/// This `struct` is created by the [`iter`] method on [`IndexSet`].
///
/// [`iter`]: IndexSet::iter
#[derive(Debug, Clone)]
pub struct Iter<'a, T> {
    iter: detail::Iter<'a, T, ()>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }

    fn count(self) -> usize {
        self.iter.count()
    }

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(value, _)| value)
    }
}

impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().map(|(value, _)| value)
    }
}

impl<'a, T> ExactSizeIterator for Iter<'a, T> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<'a, T> FusedIterator for Iter<'a, T> {}

/// An owning iterator over the items of a [`IndexSet`].
///
/// This `struct` is created by the [`into_iter`] method on [`IndexSet`]
/// (provided by the [`IntoIterator`] trait).
///
/// [`into_iter`]: IntoIterator::into_iter
/// [`IntoIterator`]: core::iter::IntoIterator
#[derive(Debug)]
pub struct IntoIter<T> {
    iter: detail::IntoIter<T, ()>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }

    fn count(self) -> usize {
        self.iter.count()
    }

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(value, _)| value)
    }
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().map(|(value, _)| value)
    }
}

impl<T> ExactSizeIterator for IntoIter<T> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<T> FusedIterator for IntoIter<T> {}
