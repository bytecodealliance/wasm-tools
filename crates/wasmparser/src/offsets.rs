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
//! an integer of type [LogicalOffset]. Data in each chunk is addressed
//! through an offset into an `[u8]` slice, which uses `usize`-addressing.
//!
//! The structures in this file bridge the gap. Given a logical offset,
//! we can compute a maximally allowed length of data at that offset.
//!

use core::{
    num::TryFromIntError,
    ops::{Add, AddAssign},
};

pub type LogicalOffset = u64;

// TODO: on platforms where usize::BITS > u64::BITS (currently almost no-where),
// this could wrap a u64 instead of a usize to be a bit smaller.
#[derive(Clone, Copy, Debug, Hash)]
pub struct MemOffset {
    rep: usize,
    #[cfg(debug_assertions)]
    max: usize,
}

impl PartialEq for MemOffset {
    fn eq(&self, other: &Self) -> bool {
        self.rep == other.rep
    }
}

impl PartialOrd for MemOffset {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for MemOffset {}
impl Ord for MemOffset {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.rep.cmp(&other.rep)
    }
}

impl MemOffset {
    pub fn max(max_logical: LogicalOffset, max: usize) -> Self {
        let max_len_logical = max_logical;
        let max_len = if LogicalOffset::BITS > usize::BITS {
            let max_len_usize = usize::MAX as LogicalOffset;
            // this now fits into a usize
            max_len_logical.max(max_len_usize) as usize
        } else {
            // since usize seems to have more or equal bits, this fits always.
            max_len_logical as usize
        };
        let max_len = max_len.min(max);
        Self {
            rep: max_len,
            #[cfg(debug_assertions)]
            max: max_len,
        }
    }
    pub fn zero_at(logical: LogicalOffset, max: usize) -> Self {
        Self::try_from(logical, 0, max).unwrap()
    }
    pub fn try_from(
        logical: LogicalOffset,
        mem: usize,
        max: usize,
    ) -> Result<Self, TryFromIntError> {
        let max = Self::max(LogicalOffset::MAX - logical, max).into_usize();
        if mem <= max {
            Ok(Self {
                rep: mem,
                #[cfg(debug_assertions)]
                max,
            })
        } else {
            Err(u32::try_from(u64::MAX).unwrap_err())
        }
    }
    pub fn into_usize(self) -> usize {
        self.into()
    }
    pub fn try_add(self, additional: usize, max: MemOffset) -> Result<MemOffset, usize> {
        #[cfg(debug_assertions)]
        {
            assert!(
                self.max == max.max,
                "self and max should be form the same memory extent"
            );
        }
        let remaining = max.into_usize().strict_sub(self.into_usize());
        if remaining < additional {
            Err(additional - remaining)
        } else {
            Ok(Self {
                rep: self.rep + additional,
                #[cfg(debug_assertions)]
                max: self.max,
            })
        }
    }
    // convinience method we should put on LogicalOffset, but can't since inherent impls are not allowed there
    pub fn logical_try_add(logical: LogicalOffset, additional: u32) -> Option<LogicalOffset> {
        logical.checked_add(additional as LogicalOffset)
    }
}

impl From<MemOffset> for usize {
    fn from(value: MemOffset) -> Self {
        value.rep
    }
}

impl Add<MemOffset> for LogicalOffset {
    type Output = LogicalOffset;
    fn add(self, rhs: MemOffset) -> Self::Output {
        debug_assert!(
            rhs <= MemOffset::max(LogicalOffset::MAX - self, usize::MAX),
            "offset too large"
        );
        self.strict_add(rhs.rep as u64)
    }
}

impl AddAssign<MemOffset> for LogicalOffset {
    fn add_assign(&mut self, rhs: MemOffset) {
        *self = *self + rhs
    }
}

impl Add<usize> for MemOffset {
    type Output = MemOffset;
    fn add(self, rhs: usize) -> Self::Output {
        let sum = if cfg!(debug_assertions) {
            self.rep.checked_add(rhs).expect("shouldn't overflow")
        } else {
            self.rep.strict_add(rhs)
        };
        #[cfg(debug_assertions)]
        {
            if sum > self.max {
                panic!("unexpectedly large offset");
            }
        }
        Self {
            rep: sum,
            #[cfg(debug_assertions)]
            max: self.max,
        }
    }
}

impl AddAssign<usize> for MemOffset {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs
    }
}
