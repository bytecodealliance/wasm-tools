/* Copyright 2018 Mozilla Foundation
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

use crate::prelude::*;
use crate::{limits::*, *};
use core::fmt;
use core::marker;
use core::ops::Range;
use core::str;

pub(crate) const WASM_MAGIC_NUMBER: &[u8; 4] = b"\0asm";

/// A binary reader for WebAssembly modules.
#[derive(Debug, Clone)]
pub struct BinaryReaderError {
    // Wrap the actual error data in a `Box` so that the error is just one
    // word. This means that we can continue returning small `Result`s in
    // registers.
    pub(crate) inner: Box<BinaryReaderErrorInner>,
}

#[derive(Debug, Clone)]
pub(crate) struct BinaryReaderErrorInner {
    pub(crate) message: String,
    pub(crate) kind: BinaryReaderErrorKind,
    pub(crate) offset: usize,
    pub(crate) needed_hint: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BinaryReaderErrorKind {
    Custom,
    Invalid,
}

/// The result for `BinaryReader` operations.
pub type Result<T, E = BinaryReaderError> = core::result::Result<T, E>;

#[cfg(feature = "std")]
impl std::error::Error for BinaryReaderError {}

#[cfg(all(not(feature = "std"), core_error))]
impl core::error::Error for BinaryReaderError {}

impl fmt::Display for BinaryReaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} (at offset 0x{:x})",
            self.inner.message, self.inner.offset
        )
    }
}

impl BinaryReaderError {
    #[cold]
    pub(crate) fn _new(kind: BinaryReaderErrorKind, message: String, offset: usize) -> Self {
        BinaryReaderError {
            inner: Box::new(BinaryReaderErrorInner {
                kind,
                message,
                offset,
                needed_hint: None,
            }),
        }
    }

    #[cold]
    pub(crate) fn new(message: impl Into<String>, offset: usize) -> Self {
        Self::_new(BinaryReaderErrorKind::Custom, message.into(), offset)
    }

    #[cold]
    pub(crate) fn invalid(msg: &'static str, offset: usize) -> Self {
        Self::_new(BinaryReaderErrorKind::Invalid, msg.into(), offset)
    }

    #[cold]
    pub(crate) fn fmt(args: fmt::Arguments<'_>, offset: usize) -> Self {
        BinaryReaderError::new(args.to_string(), offset)
    }

    #[cold]
    pub(crate) fn eof(offset: usize, needed_hint: usize) -> Self {
        let mut err = BinaryReaderError::new("unexpected end-of-file", offset);
        err.inner.needed_hint = Some(needed_hint);
        err
    }

    pub(crate) fn kind(&mut self) -> BinaryReaderErrorKind {
        self.inner.kind
    }

    /// Get this error's message.
    pub fn message(&self) -> &str {
        &self.inner.message
    }

    /// Get the offset within the Wasm binary where the error occurred.
    pub fn offset(&self) -> usize {
        self.inner.offset
    }

    #[cfg(all(feature = "validate", feature = "component-model"))]
    pub(crate) fn add_context(&mut self, context: String) {
        self.inner.message = format!("{context}\n{}", self.inner.message);
    }

    pub(crate) fn set_message(&mut self, message: &str) {
        self.inner.message = message.to_string();
    }
}

/// A binary reader of the WebAssembly structures and types.
#[derive(Clone, Debug, Hash)]
pub struct BinaryReader<'a> {
    buffer: &'a [u8],
    position: usize,
    original_offset: usize,

    // When the `features` feature is disabled then the `WasmFeatures` type
    // still exists but this field is still omitted. When `features` is
    // disabled then the only constructor of this type is `BinaryReader::new`
    // which documents all known features being active. All known features
    // being active isn't represented by `WasmFeatures` when the feature is
    // disabled so the field is omitted here to prevent accidentally using the
    // wrong listing of features.
    //
    // Feature accessors are defined by `foreach_wasm_feature!` below with a
    // method-per-feature on `BinaryReader` which when the `features` feature
    // is disabled returns `true` by default.
    #[cfg(feature = "features")]
    features: WasmFeatures,
}

impl<'a> BinaryReader<'a> {
    /// Creates a new binary reader which will parse the `data` provided.
    ///
    /// The `original_offset` provided is used for byte offsets in errors that
    /// are generated. That offset is added to the current position in `data`.
    /// This can be helpful when `data` is just a window of a view into a larger
    /// wasm binary perhaps not even entirely stored locally.
    ///
    /// The returned binary reader will have all features known to this crate
    /// enabled. To reject binaries that aren't valid unless a certain feature
    /// is enabled use the [`BinaryReader::new_features`] constructor instead.
    pub fn new(data: &[u8], original_offset: usize) -> BinaryReader {
        BinaryReader {
            buffer: data,
            position: 0,
            original_offset,
            #[cfg(feature = "features")]
            features: WasmFeatures::all(),
        }
    }

    /// Creates a new binary reader which will parse the `data` provided.
    ///
    /// The `original_offset` provided is used for byte offsets in errors that
    /// are generated. That offset is added to the current position in `data`.
    /// This can be helpful when `data` is just a window of a view into a larger
    /// wasm binary perhaps not even entirely stored locally.
    ///
    /// The `features` argument provided controls which WebAssembly features are
    /// active when parsing this data. Wasm features typically don't affect
    /// parsing too much and are generally more applicable during
    /// validation, but features and proposals will often reinterpret
    /// previously-invalid constructs as now-valid things meaning something
    /// slightly different. This means that invalid bytes before a feature may
    /// now be interpreted differently after a feature is implemented. This
    /// means that the set of activated features can affect what errors are
    /// generated and when they are generated.
    ///
    /// In general it's safe to pass `WasmFeatures::all()` here. There's no
    /// downside to enabling all features while parsing and only enabling a
    /// subset of features during validation.
    ///
    /// Note that the activated set of features does not guarantee that
    /// `BinaryReader` will return an error for disabled features. For example
    /// if SIMD is disabled then SIMD instructions will still be parsed via
    /// [`OperatorsReader::visit_operator`]. Validation must still be performed to
    /// provide a strict guarantee that if a feature is disabled that a binary
    /// doesn't leverage the feature. The activated set of features here instead
    /// only affects locations where preexisting bytes are reinterpreted in
    /// different ways with future proposals, such as the `memarg` moving from a
    /// 32-bit offset to a 64-bit offset with the `memory64` proposal.
    #[cfg(feature = "features")]
    pub fn new_features(
        data: &[u8],
        original_offset: usize,
        features: WasmFeatures,
    ) -> BinaryReader {
        BinaryReader {
            buffer: data,
            position: 0,
            original_offset,
            features,
        }
    }

    /// "Shrinks" this binary reader to retain only the buffer left-to-parse.
    ///
    /// The primary purpose of this method is to change the return value of the
    /// `range()` method. That method returns the range of the original buffer
    /// within the wasm binary so calling `range()` on the returned
    /// `BinaryReader` will return a smaller range than if `range()` is called
    /// on `self`.
    ///
    /// Otherwise parsing values from either `self` or the return value should
    /// return the same thing.
    pub(crate) fn shrink(&self) -> BinaryReader<'a> {
        BinaryReader {
            buffer: &self.buffer[self.position..],
            position: 0,
            original_offset: self.original_offset + self.position,
            #[cfg(feature = "features")]
            features: self.features,
        }
    }

    /// Gets the original position of the binary reader.
    #[inline]
    pub fn original_position(&self) -> usize {
        self.original_offset + self.position
    }

    /// Returns the currently active set of wasm features that this reader is
    /// using while parsing.
    ///
    /// For more information see [`BinaryReader::new`].
    #[cfg(feature = "features")]
    pub fn features(&self) -> WasmFeatures {
        self.features
    }

    /// Sets the wasm features active while parsing to the `features` specified.
    ///
    /// For more information see [`BinaryReader::new`].
    #[cfg(feature = "features")]
    pub fn set_features(&mut self, features: WasmFeatures) {
        self.features = features;
    }

    /// Returns a range from the starting offset to the end of the buffer.
    pub fn range(&self) -> Range<usize> {
        self.original_offset..self.original_offset + self.buffer.len()
    }

    pub(crate) fn remaining_buffer(&self) -> &'a [u8] {
        &self.buffer[self.position..]
    }

    fn ensure_has_byte(&self) -> Result<()> {
        if self.position < self.buffer.len() {
            Ok(())
        } else {
            Err(BinaryReaderError::eof(self.original_position(), 1))
        }
    }

    pub(crate) fn ensure_has_bytes(&self, len: usize) -> Result<()> {
        if self.position + len <= self.buffer.len() {
            Ok(())
        } else {
            let hint = self.position + len - self.buffer.len();
            Err(BinaryReaderError::eof(self.original_position(), hint))
        }
    }

    /// Reads a value of type `T` from this binary reader, advancing the
    /// internal position in this reader forward as data is read.
    #[inline]
    pub fn read<T>(&mut self) -> Result<T>
    where
        T: FromReader<'a>,
    {
        T::from_reader(self)
    }

    pub(crate) fn read_u7(&mut self) -> Result<u8> {
        let b = self.read_u8()?;
        if (b & 0x80) != 0 {
            return Err(BinaryReaderError::new(
                "invalid u7",
                self.original_position() - 1,
            ));
        }
        Ok(b)
    }

    pub(crate) fn external_kind_from_byte(byte: u8, offset: usize) -> Result<ExternalKind> {
        match byte {
            0x00 => Ok(ExternalKind::Func),
            0x01 => Ok(ExternalKind::Table),
            0x02 => Ok(ExternalKind::Memory),
            0x03 => Ok(ExternalKind::Global),
            0x04 => Ok(ExternalKind::Tag),
            x => Err(Self::invalid_leading_byte_error(x, "external kind", offset)),
        }
    }

    /// Reads a variable-length 32-bit size from the byte stream while checking
    /// against a limit.
    pub fn read_size(&mut self, limit: usize, desc: &str) -> Result<usize> {
        let pos = self.original_position();
        let size = self.read_var_u32()? as usize;
        if size > limit {
            bail!(pos, "{desc} size is out of bounds");
        }
        Ok(size)
    }

    /// Reads a variable-length 32-bit size from the byte stream while checking
    /// against a limit.
    ///
    /// Then reads that many values of type `T` and returns them as an iterator.
    ///
    /// Note that regardless of how many items are read from the returned
    /// iterator the items will still be parsed from this reader.
    pub fn read_iter<'me, T>(
        &'me mut self,
        limit: usize,
        desc: &str,
    ) -> Result<BinaryReaderIter<'a, 'me, T>>
    where
        T: FromReader<'a>,
    {
        let size = self.read_size(limit, desc)?;
        Ok(BinaryReaderIter {
            remaining: size,
            reader: self,
            _marker: marker::PhantomData,
        })
    }

    /// Returns whether the `BinaryReader` has reached the end of the file.
    #[inline]
    pub fn eof(&self) -> bool {
        self.position >= self.buffer.len()
    }

    /// Returns the `BinaryReader`'s current position.
    #[inline]
    pub fn current_position(&self) -> usize {
        self.position
    }

    /// Returns the number of bytes remaining in the `BinaryReader`.
    #[inline]
    pub fn bytes_remaining(&self) -> usize {
        self.buffer.len() - self.position
    }

    /// Advances the `BinaryReader` `size` bytes, and returns a slice from the
    /// current position of `size` length.
    ///
    /// # Errors
    /// If `size` exceeds the remaining length in `BinaryReader`.
    pub fn read_bytes(&mut self, size: usize) -> Result<&'a [u8]> {
        self.ensure_has_bytes(size)?;
        let start = self.position;
        self.position += size;
        Ok(&self.buffer[start..self.position])
    }

    /// Reads a length-prefixed list of bytes from this reader and returns a
    /// new `BinaryReader` to read that list of bytes.
    pub fn read_reader(&mut self) -> Result<BinaryReader<'a>> {
        let size = self.read_var_u32()? as usize;
        self.skip(|reader| {
            reader.read_bytes(size)?;
            Ok(())
        })
    }

    /// Advances the `BinaryReader` four bytes and returns a `u32`.
    /// # Errors
    /// If `BinaryReader` has less than four bytes remaining.
    pub fn read_u32(&mut self) -> Result<u32> {
        self.ensure_has_bytes(4)?;
        let word = u32::from_le_bytes(
            self.buffer[self.position..self.position + 4]
                .try_into()
                .unwrap(),
        );
        self.position += 4;
        Ok(word)
    }

    /// Advances the `BinaryReader` eight bytes and returns a `u64`.
    /// # Errors
    /// If `BinaryReader` has less than eight bytes remaining.
    pub fn read_u64(&mut self) -> Result<u64> {
        self.ensure_has_bytes(8)?;
        let word = u64::from_le_bytes(
            self.buffer[self.position..self.position + 8]
                .try_into()
                .unwrap(),
        );
        self.position += 8;
        Ok(word)
    }

    /// Advances the `BinaryReader` a single byte.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has no bytes remaining.
    #[inline]
    pub fn read_u8(&mut self) -> Result<u8> {
        let b = match self.buffer.get(self.position) {
            Some(b) => *b,
            None => return Err(self.eof_err()),
        };
        self.position += 1;
        Ok(b)
    }

    #[cold]
    fn eof_err(&self) -> BinaryReaderError {
        BinaryReaderError::eof(self.original_position(), 1)
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a `u32`.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has less than one or up to four bytes remaining, or
    /// the integer is larger than 32 bits.
    #[inline]
    pub fn read_var_u32(&mut self) -> Result<u32> {
        // Optimization for single byte i32.
        let byte = self.read_u8()?;
        if (byte & 0x80) == 0 {
            Ok(u32::from(byte))
        } else {
            self.read_var_u32_big(byte)
        }
    }

    fn read_var_u32_big(&mut self, byte: u8) -> Result<u32> {
        let mut result = (byte & 0x7F) as u32;
        let mut shift = 7;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as u32) << shift;
            if shift >= 25 && (byte >> (32 - shift)) != 0 {
                let msg = if byte & 0x80 != 0 {
                    "invalid var_u32: integer representation too long"
                } else {
                    "invalid var_u32: integer too large"
                };
                // The continuation bit or unused bits are set.
                return Err(BinaryReaderError::new(msg, self.original_position() - 1));
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        Ok(result)
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a `u64`.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has less than one or up to eight bytes remaining, or
    /// the integer is larger than 64 bits.
    #[inline]
    pub fn read_var_u64(&mut self) -> Result<u64> {
        // Optimization for single byte u64.
        let byte = u64::from(self.read_u8()?);
        if (byte & 0x80) == 0 {
            Ok(byte)
        } else {
            self.read_var_u64_big(byte)
        }
    }

    fn read_var_u64_big(&mut self, byte: u64) -> Result<u64> {
        let mut result = byte & 0x7F;
        let mut shift = 7;
        loop {
            let byte = u64::from(self.read_u8()?);
            result |= (byte & 0x7F) << shift;
            if shift >= 57 && (byte >> (64 - shift)) != 0 {
                let msg = if byte & 0x80 != 0 {
                    "invalid var_u64: integer representation too long"
                } else {
                    "invalid var_u64: integer too large"
                };
                // The continuation bit or unused bits are set.
                return Err(BinaryReaderError::new(msg, self.original_position() - 1));
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        Ok(result)
    }

    /// Executes `f` to skip some data in this binary reader and then returns a
    /// reader which will read the skipped data.
    pub fn skip(&mut self, f: impl FnOnce(&mut Self) -> Result<()>) -> Result<Self> {
        let start = self.position;
        f(self)?;
        let mut ret = self.clone();
        ret.buffer = &self.buffer[start..self.position];
        ret.position = 0;
        ret.original_offset = self.original_offset + start;
        Ok(ret)
    }

    /// Advances the `BinaryReader` past a WebAssembly string. This method does
    /// not perform any utf-8 validation.
    /// # Errors
    /// If `BinaryReader` has less than four bytes, the string's length exceeds
    /// the remaining bytes, or the string length
    /// exceeds `limits::MAX_WASM_STRING_SIZE`.
    pub fn skip_string(&mut self) -> Result<()> {
        let len = self.read_var_u32()? as usize;
        if len > MAX_WASM_STRING_SIZE {
            return Err(BinaryReaderError::new(
                "string size out of bounds",
                self.original_position() - 1,
            ));
        }
        self.ensure_has_bytes(len)?;
        self.position += len;
        Ok(())
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a `i32`.
    /// # Errors
    /// If `BinaryReader` has less than one or up to four bytes remaining, or
    /// the integer is larger than 32 bits.
    #[inline]
    pub fn read_var_i32(&mut self) -> Result<i32> {
        // Optimization for single byte i32.
        let byte = self.read_u8()?;
        if (byte & 0x80) == 0 {
            Ok(((byte as i32) << 25) >> 25)
        } else {
            self.read_var_i32_big(byte)
        }
    }

    fn read_var_i32_big(&mut self, byte: u8) -> Result<i32> {
        let mut result = (byte & 0x7F) as i32;
        let mut shift = 7;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as i32) << shift;
            if shift >= 25 {
                let continuation_bit = (byte & 0x80) != 0;
                let sign_and_unused_bit = (byte << 1) as i8 >> (32 - shift);
                if continuation_bit || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                    let msg = if continuation_bit {
                        "invalid var_i32: integer representation too long"
                    } else {
                        "invalid var_i32: integer too large"
                    };
                    return Err(BinaryReaderError::new(msg, self.original_position() - 1));
                }
                return Ok(result);
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        let ashift = 32 - shift;
        Ok((result << ashift) >> ashift)
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a signed 33 bit integer, returned as a `i64`.
    /// # Errors
    /// If `BinaryReader` has less than one or up to five bytes remaining, or
    /// the integer is larger than 33 bits.
    pub fn read_var_s33(&mut self) -> Result<i64> {
        // Optimization for single byte.
        let byte = self.read_u8()?;
        if (byte & 0x80) == 0 {
            return Ok(((byte as i8) << 1) as i64 >> 1);
        }

        let mut result = (byte & 0x7F) as i64;
        let mut shift = 7;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as i64) << shift;
            if shift >= 25 {
                let continuation_bit = (byte & 0x80) != 0;
                let sign_and_unused_bit = (byte << 1) as i8 >> (33 - shift);
                if continuation_bit || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                    return Err(BinaryReaderError::new(
                        "invalid var_s33: integer representation too long",
                        self.original_position() - 1,
                    ));
                }
                return Ok(result);
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        let ashift = 64 - shift;
        Ok((result << ashift) >> ashift)
    }

    /// Advances the `BinaryReader` up to eight bytes to parse a variable
    /// length integer as a 64 bit integer, returned as a `i64`.
    /// # Errors
    /// If `BinaryReader` has less than one or up to eight bytes remaining, or
    /// the integer is larger than 64 bits.
    pub fn read_var_i64(&mut self) -> Result<i64> {
        let mut result: i64 = 0;
        let mut shift = 0;
        loop {
            let byte = self.read_u8()?;
            result |= i64::from(byte & 0x7F) << shift;
            if shift >= 57 {
                let continuation_bit = (byte & 0x80) != 0;
                let sign_and_unused_bit = ((byte << 1) as i8) >> (64 - shift);
                if continuation_bit || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                    let msg = if continuation_bit {
                        "invalid var_i64: integer representation too long"
                    } else {
                        "invalid var_i64: integer too large"
                    };
                    return Err(BinaryReaderError::new(msg, self.original_position() - 1));
                }
                return Ok(result);
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        let ashift = 64 - shift;
        Ok((result << ashift) >> ashift)
    }

    /// Advances the `BinaryReader` four bytes to parse a 32 bit floating point
    /// number, returned as `Ieee32`.
    /// # Errors
    /// If `BinaryReader` has less than four bytes remaining.
    pub fn read_f32(&mut self) -> Result<Ieee32> {
        let value = self.read_u32()?;
        Ok(Ieee32(value))
    }

    /// Advances the `BinaryReader` eight bytes to parse a 64 bit floating point
    /// number, returned as `Ieee64`.
    /// # Errors
    /// If `BinaryReader` has less than eight bytes remaining.
    pub fn read_f64(&mut self) -> Result<Ieee64> {
        let value = self.read_u64()?;
        Ok(Ieee64(value))
    }

    /// (internal) Reads a fixed-size WebAssembly string from the module.
    fn internal_read_string(&mut self, len: usize) -> Result<&'a str> {
        let bytes = self.read_bytes(len)?;
        str::from_utf8(bytes).map_err(|_| {
            BinaryReaderError::new("malformed UTF-8 encoding", self.original_position() - 1)
        })
    }

    /// Reads a WebAssembly string from the module.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has less than up to four bytes remaining, the string's
    /// length exceeds the remaining bytes, the string's length exceeds
    /// `limits::MAX_WASM_STRING_SIZE`, or the string contains invalid utf-8.
    pub fn read_string(&mut self) -> Result<&'a str> {
        let len = self.read_var_u32()? as usize;
        if len > MAX_WASM_STRING_SIZE {
            return Err(BinaryReaderError::new(
                "string size out of bounds",
                self.original_position() - 1,
            ));
        }
        return self.internal_read_string(len);
    }

    /// Reads a unlimited WebAssembly string from the module.
    ///
    /// Note that this is similar to [`BinaryReader::read_string`] except that
    /// it will not limit the size of the returned string by
    /// `limits::MAX_WASM_STRING_SIZE`.
    pub fn read_unlimited_string(&mut self) -> Result<&'a str> {
        let len = self.read_var_u32()? as usize;
        return self.internal_read_string(len);
    }

    #[cold]
    pub(crate) fn invalid_leading_byte<T>(&self, byte: u8, desc: &str) -> Result<T> {
        Err(Self::invalid_leading_byte_error(
            byte,
            desc,
            self.original_position() - 1,
        ))
    }

    pub(crate) fn invalid_leading_byte_error(
        byte: u8,
        desc: &str,
        offset: usize,
    ) -> BinaryReaderError {
        format_err!(offset, "invalid leading byte (0x{byte:x}) for {desc}")
    }

    pub(crate) fn peek(&self) -> Result<u8> {
        self.ensure_has_byte()?;
        Ok(self.buffer[self.position])
    }

    pub(crate) fn read_block_type(&mut self) -> Result<BlockType> {
        let b = self.peek()?;

        // Block types are encoded as either 0x40, a `valtype`, or `s33`. All
        // current `valtype` encodings are negative numbers when encoded with
        // sleb128, but it's also required that valtype encodings are in their
        // canonical form. For example an overlong encoding of -1 as `0xff 0x7f`
        // is not valid and it is required to be `0x7f`. This means that we
        // can't simply match on the `s33` that pops out below since reading the
        // whole `s33` might read an overlong encoding.
        //
        // To test for this the first byte `b` is inspected. The highest bit,
        // the continuation bit in LEB128 encoding, must be clear. The next bit,
        // the sign bit, must be set to indicate that the number is negative. If
        // these two conditions hold then we're guaranteed that this is a
        // negative number.
        //
        // After this a value type is read directly instead of looking for an
        // indexed value type.
        if b & 0x80 == 0 && b & 0x40 != 0 {
            if b == 0x40 {
                self.position += 1;
                return Ok(BlockType::Empty);
            }
            return Ok(BlockType::Type(self.read()?));
        }

        // Not empty or a singular type, so read the function type index
        let idx = self.read_var_s33()?;
        match u32::try_from(idx) {
            Ok(idx) => Ok(BlockType::FuncType(idx)),
            Err(_) => {
                return Err(BinaryReaderError::new(
                    "invalid function type",
                    self.original_position(),
                ));
            }
        }
    }

    /// Returns whether there is an `end` opcode followed by eof remaining in
    /// this reader.
    pub fn is_end_then_eof(&self) -> bool {
        self.remaining_buffer() == &[0x0b]
    }

    pub(crate) fn read_header_version(&mut self) -> Result<u32> {
        let magic_number = self.read_bytes(4)?;
        if magic_number != WASM_MAGIC_NUMBER {
            return Err(BinaryReaderError::new(
                format!("magic header not detected: bad magic number - expected={WASM_MAGIC_NUMBER:#x?} actual={magic_number:#x?}"),
                self.original_position() - 4,
            ));
        }
        self.read_u32()
    }
}

// See documentation on `BinaryReader::features` for more on what's going on
// here.
macro_rules! define_feature_accessor {
    ($feature:ident = $default:expr) => {
        impl BinaryReader<'_> {
            #[inline]
            #[allow(dead_code)]
            pub(crate) fn $feature(&self) -> bool {
                #[cfg(feature = "features")]
                {
                    self.features.$feature()
                }
                #[cfg(not(feature = "features"))]
                {
                    true
                }
            }
        }
    };
}

super::features::foreach_wasm_feature!(define_feature_accessor);

/// Iterator returned from [`BinaryReader::read_iter`].
pub struct BinaryReaderIter<'a, 'me, T: FromReader<'a>> {
    remaining: usize,
    pub(crate) reader: &'me mut BinaryReader<'a>,
    _marker: marker::PhantomData<T>,
}

impl<'a, T> Iterator for BinaryReaderIter<'a, '_, T>
where
    T: FromReader<'a>,
{
    type Item = Result<T>;

    fn next(&mut self) -> Option<Result<T>> {
        if self.remaining == 0 {
            None
        } else {
            let ret = self.reader.read::<T>();
            if ret.is_err() {
                self.remaining = 0;
            } else {
                self.remaining -= 1;
            }
            Some(ret)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<'a, T> Drop for BinaryReaderIter<'a, '_, T>
where
    T: FromReader<'a>,
{
    fn drop(&mut self) {
        while self.next().is_some() {
            // ...
        }
    }
}
