//! Low-level encoders.
//!
//! This module provides low-level encoders that can be used (for example) to
//! define your own custom section encodings.

use std::convert::TryFrom;

/// Encode a `u32` as a ULEB128.
pub fn u32(n: u32) -> impl ExactSizeIterator<Item = u8> {
    let mut buf = [0; 5];
    let n = leb128::write::unsigned(&mut &mut buf[..], n as u64).unwrap();
    Buf5Iter { buf, n, i: 0 }
}

/// Encode an `i32` as a SLEB128.
pub fn s32(x: i32) -> impl ExactSizeIterator<Item = u8> {
    let mut buf = [0; 5];
    let n = leb128::write::signed(&mut &mut buf[..], x as i64).unwrap();
    Buf5Iter { buf, n, i: 0 }
}

/// Encode an `i64` that uses at most 33 bits as a SLEB128.
///
/// # Panics
///
/// Panics if more than 33 bits are used.
///
/// ```
/// wasm_encoder::encoders::s33(1 << 32);
/// ```
///
/// ```should_panic
/// wasm_encoder::encoders::s33(1 << 33);
/// ```
///
/// ```
/// wasm_encoder::encoders::s33(-1 << 32);
/// ```
///
/// ```should_panic
/// wasm_encoder::encoders::s33(-1 << 33);
/// ```
pub fn s33(x: i64) -> impl ExactSizeIterator<Item = u8> {
    assert!({
        let mask = 1 << 33 << 30 >> 30;
        x != mask && (x & mask == 0) == (x >= 0)
    });
    let mut buf = [0; 5];
    let n = leb128::write::signed(&mut &mut buf[..], x).unwrap();
    Buf5Iter { buf, n, i: 0 }
}

/// Encode an `i64` as a SLEB128.
pub fn s64(x: i64) -> impl ExactSizeIterator<Item = u8> {
    let mut buf = [0; 9];
    let n = leb128::write::signed(&mut &mut buf[..], x as i64).unwrap();
    Buf9Iter { buf, n, i: 0 }
}

/// Encode a length-prefixed UTF-8 string.
pub fn str<'a>(s: &'a str) -> impl Iterator<Item = u8> + 'a {
    u32(u32::try_from(s.len()).unwrap()).chain(s.as_bytes().iter().copied())
}

// `[u8; 5]` doesn't have `into_iter()` so we can't do
// `[..].into_iter().take(n)` :(
struct Buf5Iter {
    buf: [u8; 5],
    n: usize,
    i: usize,
}

impl Iterator for Buf5Iter {
    type Item = u8;

    #[inline]
    fn next(&mut self) -> Option<u8> {
        debug_assert!(self.i <= self.n);
        if self.i < self.n {
            let x = self.buf[self.i];
            self.i += 1;
            Some(x)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.n - self.i, Some(self.n - self.i))
    }
}

impl ExactSizeIterator for Buf5Iter {}

// `[u8; 9]` doesn't have `into_iter()` so we can't do
// `[..].into_iter().take(n)` :(
struct Buf9Iter {
    buf: [u8; 9],
    n: usize,
    i: usize,
}

impl Iterator for Buf9Iter {
    type Item = u8;

    #[inline]
    fn next(&mut self) -> Option<u8> {
        debug_assert!(self.i <= self.n);
        if self.i < self.n {
            let x = self.buf[self.i];
            self.i += 1;
            Some(x)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.n - self.i, Some(self.n - self.i))
    }
}

impl ExactSizeIterator for Buf9Iter {}
