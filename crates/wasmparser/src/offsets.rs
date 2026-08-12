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
//! This module contains functionality to bridge the gap.

// An (not necessarily exhaustive) list of properties we use of `u64` in relation
// to usize:
// - u64::MAX as an upper bound and sometimes invalid offset
// - 0u64 as the starting offset
// - we can add and subtract small offsets to recalculate the original position
//   in some error paths, where saving the position directly would clutter registers.

// An memory offset into some chunk of bytes occurs at some specified logical
// offset in the file. We currently use `usize` to represent memory offsets.
// TODO: on platforms where usize::BITS > u64::BITS (currently almost no-where),
// we could use u64 directly instead of usize to represent memory offsets.

use crate::Error;

/// Return the largest memory offset that can be added to `offset` without going
/// past `max_offset` or overflowing.
pub fn max_data_len(offset: u64, max_offset: u64) -> usize {
    let mut max_logical = max_offset - offset;
    if u64::BITS > usize::BITS {
        max_logical = max_logical.min(usize::MAX as u64)
    }
    // we now know that max_logical fits into a usize
    max_logical as usize
}

#[cold]
pub fn panic_too_many_bytes(offset: u64, len: usize, max_len: usize) -> ! {
    panic!(
        "Content too large to parse. Got {len}, expected at most {max_len} bytes at offset 0x{offset:x}."
    )
}
pub fn err_too_many_bytes(offset: u64, len: usize, max_len: usize) -> Error {
    format_err!(
        offset,
        "Content too large to parse. Got {len}, expected at most {max_len} bytes."
    )
}
