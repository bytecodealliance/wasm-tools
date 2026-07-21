/* Copyright 2026 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Logical offsets into the input wasm file are strictly limited to fit into
//! an integer of type [u64]. Data in each chunk is addressed through an offset
//! into an `[u8]` slice, which uses `usize`-addressing.
//!
//! The structures in this file bridge the gap. Given a logical offset,
//! we can compute a maximally allowed length of data at that offset.

use core::ops::{Add, AddAssign};

// An (not necessarily exhaustive) list of properties we use of `u64` in relation
// to usize:
// - u64::MAX as an upper bound and sometimes invalid offset
// - 0u64 as the starting offset
// - we can add and subtract small offsets to recalculate the original position
//   in some error paths, where saving the position directly would clutter registers.

/// Compute the maximum allowable memory offset under both contraints
fn max_memory_offset(mut max_logical: u64, max: usize) -> usize {
    if u64::BITS > usize::BITS {
        max_logical = max_logical.max(usize::MAX as u64)
    }
    // we now know that max_logical fits into a usize
    let max_logical = max_logical as usize;

    // the more "natural" `max_logical.min(max)` generates a cmov which this avoids
    if max <= max_logical {
        max
    } else {
        // unlikely
        #[cold]
        fn smaller(constrained: usize) -> usize {
            constrained
        }
        smaller(max_logical)
    }
}

// TODO: on platforms where usize::BITS > u64::BITS (currently almost no-where),
// this could wrap a u64 instead of a usize to be a bit smaller.
/// An offset into some chunk of memory at some specified logical offset in
/// the file.
///
/// The represented offset can always be converted into a `usize`, and can
/// always be added to the logical offset without overflow.
///
/// The other function of this newtype is to allow `u64: Add<MemOffset>` and
/// `MemOffset: Add<usize>` without confusing the two notions of offsets.
///
/// We explicitly do not have `MemOffset: From<usize>` as not all offsets are
/// valid at all offsets.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct MemOffset {
    rep: usize,
}

impl MemOffset {
    pub fn max(max_logical: u64, max: usize) -> Self {
        Self {
            rep: max_memory_offset(max_logical, max),
        }
    }
    pub fn zero() -> Self {
        // 0 is always a valid offset
        Self { rep: 0 }
    }
    pub fn into_usize(self) -> usize {
        self.into()
    }
    pub fn try_add(self, additional: usize, max: MemOffset) -> Result<MemOffset, usize> {
        let remaining = max.into_usize().strict_sub(self.into_usize());
        if remaining < additional {
            Err(additional - remaining)
        } else {
            Ok(Self {
                rep: self.rep + additional,
            })
        }
    }
    // convinience method we should put on u64, but can't since inherent impls are not allowed there
    pub fn logical_try_add_u32(logical: u64, additional: u32, max_logical: u64) -> Option<u64> {
        let summed = logical.checked_add(additional as u64)?;
        (summed <= max_logical).then_some(summed)
    }
}

impl From<MemOffset> for usize {
    fn from(value: MemOffset) -> Self {
        value.rep
    }
}

impl Add<MemOffset> for u64 {
    type Output = u64;
    fn add(self, rhs: MemOffset) -> Self::Output {
        debug_assert!(
            rhs <= MemOffset::max(u64::MAX - self, usize::MAX),
            "offset too large",
        );
        self.strict_add(rhs.rep as u64)
    }
}

impl AddAssign<MemOffset> for u64 {
    fn add_assign(&mut self, rhs: MemOffset) {
        *self = *self + rhs
    }
}

impl Add<usize> for MemOffset {
    type Output = MemOffset;
    fn add(self, rhs: usize) -> Self::Output {
        debug_assert!(rhs <= (usize::MAX - self.rep), "offset too large",);
        Self {
            rep: self.rep.strict_add(rhs),
        }
    }
}

impl AddAssign<usize> for MemOffset {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs
    }
}
