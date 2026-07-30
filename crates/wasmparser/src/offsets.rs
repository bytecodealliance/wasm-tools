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

use core::{ops::Range, u64};

// An (not necessarily exhaustive) list of properties we use of `u64` in relation
// to usize:
// - u64::MAX as an upper bound and sometimes invalid offset
// - 0u64 as the starting offset
// - we can add and subtract small offsets to recalculate the original position
//   in some error paths, where saving the position directly would clutter registers.

/// An offset into some chunk of memory occurs at some specified logical offset in
/// the file. We currently use `usize` to represent these offsets.
///
/// This offset can always be added onto the logical offset without overflow.
/// Compute the maximum allowable memory offset under both contraints
// TODO: on platforms where usize::BITS > u64::BITS (currently almost no-where),
// we could use u64 directly instead of usize to represent offsets.
pub fn max_memory_offset(mut max_logical: u64, max: usize) -> usize {
    if u64::BITS > usize::BITS {
        max_logical = max_logical.min(usize::MAX as u64)
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
/// Converts offsets from the parser back into offsets into the input.
#[derive(Clone, Copy, Debug)]
pub struct OffsetConverter {
    start: u64,
}

impl OffsetConverter {
    /// Convert ranges as if parsing a chunk of input data started at `start`.
    pub fn from_start(start: u64) -> Self {
        Self { start }
    }
    /// Return the offset at the start of the parsed input.
    pub fn start(&self) -> u64 {
        self.start
    }
    /// Convert an offset into a byte offset into the input.
    ///
    /// Returns `None` if the offset is before the offset at the start of
    /// parsing, or if the offset from start is too large to represent as a
    /// `usize`.
    pub fn try_convert_offset(&self, offset: u64) -> Option<usize> {
        let from_start = offset.checked_sub(self.start)?;
        usize::try_from(from_start).ok()
    }
    /// Convert an offset into a byte offset into the input.
    ///
    /// Panics if the offset is before `start` or if the offset is too large to represent
    /// as a `usize`. Both of these cases can generally not happen when passed an offset
    /// inside the [`Payload`](crate::Payload) returned from a parsing function.
    pub fn convert_offset(&self, offset: u64) -> usize {
        self.try_convert_offset(offset).expect("invalid offset")
    }
    /// Convert a range into a range in the input.
    pub fn try_convert_range(&self, range: &Range<u64>) -> Option<Range<usize>> {
        let start = self.try_convert_offset(range.start)?;
        let end = self.try_convert_offset(range.end)?;
        Some(start..end)
    }
    /// Convert a range into a range in the input.
    pub fn convert_range(&self, range: &Range<u64>) -> Range<usize> {
        let start = self.try_convert_offset(range.start).expect("invalid start");
        let end = self.try_convert_offset(range.end).expect("invalid end");
        start..end
    }
}
