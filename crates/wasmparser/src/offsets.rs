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

// An (not necessarily exhaustive) list of properties we use of `u64` in relation
// to usize:
// - u64::MAX as an upper bound and sometimes invalid offset
// - 0u64 as the starting offset
// - we can add and subtract small offsets to recalculate the original position
//   in some error paths, where saving the position directly would clutter registers.

/// An memory offset into some chunk of bytes occurs at some specified logical
/// offset in the file. We currently use `usize` to represent memory offsets.
///
/// This offset can always be added onto the logical offset without overflow.
/// Compute the maximum allowable memory offset under both contraints
// TODO: on platforms where usize::BITS > u64::BITS (currently almost no-where),
// we could use u64 directly instead of usize to represent memory offsets.
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
