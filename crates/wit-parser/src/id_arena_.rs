use core::marker::PhantomData;
use id_arena;
use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};

/// An identifier for an object allocated within an arena.
pub struct Id<T> {
    idx: usize,
    arena_id: u32,
    _ty: PhantomData<fn() -> T>,
}

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Id").field("idx", &self.idx).finish()
    }
}

impl<T> Copy for Id<T> {}

impl<T> Clone for Id<T> {
    #[inline]
    fn clone(&self) -> Id<T> {
        *self
    }
}

impl<T> PartialEq for Id<T> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.arena_id == rhs.arena_id && self.idx == rhs.idx
    }
}

impl<T> Eq for Id<T> {}

impl<T> Hash for Id<T> {
    #[inline]
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.arena_id.hash(h);
        self.idx.hash(h);
    }
}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.arena_id
            .cmp(&rhs.arena_id)
            .then(self.idx.cmp(&rhs.idx))
    }
}

impl<T> Id<T> {
    /// Get the index within the arena that this id refers to.
    #[inline]
    pub fn index(&self) -> usize {
        self.idx
    }
}

/// Our `ArenaBehavior` implementation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArenaBehavior<T> {
    _phantom: PhantomData<fn() -> T>,
}

impl<T> id_arena::ArenaBehavior for ArenaBehavior<T> {
    type Id = Id<T>;

    #[inline]
    fn new_id(arena_id: u32, idx: usize) -> Self::Id {
        Id {
            idx,
            arena_id,
            _ty: PhantomData,
        }
    }

    #[inline]
    fn index(id: Self::Id) -> usize {
        id.idx
    }

    #[inline]
    fn arena_id(id: Self::Id) -> u32 {
        id.arena_id
    }
}

pub type Arena<T, A=ArenaBehavior<T>> = id_arena::Arena<T, A>;
