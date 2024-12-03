//! Common tokens that implement the [`Parse`] trait which are otherwise not
//! associated specifically with the wasm text format per se (useful in other
//! contexts too perhaps).

use crate::annotation;
use crate::lexer::Float;
use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::str;

/// A position in the original source stream, used to render errors.
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Span {
    pub(crate) offset: usize,
}

impl Span {
    /// Construct a `Span` from a byte offset in the source file.
    pub fn from_offset(offset: usize) -> Self {
        Span { offset }
    }

    /// Returns the line/column information of this span within `text`.
    /// Line and column numbers are 0-indexed. User presentation is typically
    /// 1-indexed, but 0-indexing is appropriate for internal use with
    /// iterators and slices.
    pub fn linecol_in(&self, text: &str) -> (usize, usize) {
        let mut cur = 0;
        // Use split_terminator instead of lines so that if there is a `\r`,
        // it is included in the offset calculation. The `+1` values below
        // account for the `\n`.
        for (i, line) in text.split_terminator('\n').enumerate() {
            if cur + line.len() + 1 > self.offset {
                return (i, self.offset - cur);
            }
            cur += line.len() + 1;
        }
        (text.lines().count(), 0)
    }

    /// Returns the byte offset of this span.
    pub fn offset(&self) -> usize {
        self.offset
    }
}

/// An identifier in a WebAssembly module, prefixed by `$` in the textual
/// format.
///
/// An identifier is used to symbolically refer to items in a a wasm module,
/// typically via the [`Index`] type.
#[derive(Copy, Clone)]
pub struct Id<'a> {
    name: &'a str,
    generation: u32,
    span: Span,
}

impl<'a> Id<'a> {
    /// Construct a new identifier from given string.
    ///
    /// Note that `name` can be any arbitrary string according to the
    /// WebAssembly/annotations proposal.
    pub fn new(name: &'a str, span: Span) -> Id<'a> {
        Id {
            name,
            generation: 0,
            span,
        }
    }

    #[cfg(feature = "wasm-module")]
    pub(crate) fn gensym(span: Span, generation: u32) -> Id<'a> {
        Id {
            name: "gensym",
            generation,
            span,
        }
    }

    /// Returns the underlying name of this identifier.
    ///
    /// The name returned does not contain the leading `$`.
    pub fn name(&self) -> &'a str {
        self.name
    }

    /// Returns span of this identifier in the original source
    pub fn span(&self) -> Span {
        self.span
    }

    #[cfg(feature = "wasm-module")]
    pub(crate) fn is_gensym(&self) -> bool {
        self.generation != 0
    }
}

impl<'a> Hash for Id<'a> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
        self.generation.hash(hasher);
    }
}

impl<'a> PartialEq for Id<'a> {
    fn eq(&self, other: &Id<'a>) -> bool {
        self.name == other.name && self.generation == other.generation
    }
}

impl<'a> Eq for Id<'a> {}

impl<'a> Parse<'a> for Id<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((name, rest)) = c.id()? {
                return Ok((
                    Id {
                        name,
                        generation: 0,
                        span: c.cur_span(),
                    },
                    rest,
                ));
            }
            Err(c.error("expected an identifier"))
        })
    }
}

impl fmt::Debug for Id<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.generation != 0 {
            f.debug_struct("Id")
                .field("generation", &self.generation)
                .finish()
        } else {
            self.name.fmt(f)
        }
    }
}

impl Peek for Id<'_> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        cursor.peek_id()
    }

    fn display() -> &'static str {
        "an identifier"
    }
}

/// A reference to another item in a wasm module.
///
/// This type is used for items referring to other items (such as `call $foo`
/// referencing function `$foo`). References can be either an index (u32) or an
/// [`Id`] in the textual format.
///
/// The emission phase of a module will ensure that `Index::Id` is never used
/// and switch them all to `Index::Num`.
#[derive(Copy, Clone, Debug)]
pub enum Index<'a> {
    /// A numerical index that this references. The index space this is
    /// referencing is implicit based on where this [`Index`] is stored.
    Num(u32, Span),
    /// A human-readable identifier this references. Like `Num`, the namespace
    /// this references is based on where this is stored.
    Id(Id<'a>),
}

impl Index<'_> {
    /// Returns the source location where this `Index` was defined.
    pub fn span(&self) -> Span {
        match self {
            Index::Num(_, span) => *span,
            Index::Id(id) => id.span(),
        }
    }

    #[cfg(feature = "wasm-module")]
    pub(crate) fn is_resolved(&self) -> bool {
        matches!(self, Index::Num(..))
    }
}

impl<'a> Parse<'a> for Index<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<Id>()? {
            Ok(Index::Id(parser.parse()?))
        } else if parser.peek::<u32>()? {
            let (val, span) = parser.parse()?;
            Ok(Index::Num(val, span))
        } else {
            Err(parser.error(format!(
                "unexpected token, expected an index or an identifier"
            )))
        }
    }
}

impl Peek for Index<'_> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        Ok(u32::peek(cursor)? || Id::peek(cursor)?)
    }

    fn display() -> &'static str {
        "an index"
    }
}

impl<'a> From<Id<'a>> for Index<'a> {
    fn from(id: Id<'a>) -> Index<'a> {
        Index::Id(id)
    }
}

impl PartialEq for Index<'_> {
    fn eq(&self, other: &Index<'_>) -> bool {
        match (self, other) {
            (Index::Num(a, _), Index::Num(b, _)) => a == b,
            (Index::Id(a), Index::Id(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Index<'_> {}

impl Hash for Index<'_> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            Index::Num(a, _) => {
                0u8.hash(hasher);
                a.hash(hasher);
            }
            Index::Id(a) => {
                1u8.hash(hasher);
                a.hash(hasher);
            }
        }
    }
}

/// Parses `(func $foo)`
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct ItemRef<'a, K> {
    pub kind: K,
    pub idx: Index<'a>,
}

impl<'a, K: Parse<'a>> Parse<'a> for ItemRef<'a, K> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            let kind = parser.parse::<K>()?;
            let idx = parser.parse()?;
            Ok(ItemRef { kind, idx })
        })
    }
}

impl<'a, K: Peek> Peek for ItemRef<'a, K> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        match cursor.lparen()? {
            Some(remaining) => K::peek(remaining),
            None => Ok(false),
        }
    }

    fn display() -> &'static str {
        "an item reference"
    }
}

/// An `@name` annotation in source, currently of the form `@name "foo"`
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct NameAnnotation<'a> {
    /// The name specified for the item
    pub name: &'a str,
}

impl<'a> Parse<'a> for NameAnnotation<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<annotation::name>()?;
        let name = parser.parse()?;
        Ok(NameAnnotation { name })
    }
}

impl<'a> Parse<'a> for Option<NameAnnotation<'a>> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(if parser.peek2::<annotation::name>()? {
            Some(parser.parens(|p| p.parse())?)
        } else {
            None
        })
    }
}

macro_rules! integers {
    ($($i:ident($u:ident))*) => ($(
        impl<'a> Parse<'a> for $i {
            fn parse(parser: Parser<'a>) -> Result<Self> {
                Ok(parser.parse::<($i, Span)>()?.0)
            }
        }

        impl<'a> Parse<'a> for ($i, Span) {
            fn parse(parser: Parser<'a>) -> Result<Self> {
                parser.step(|c| {
                    if let Some((i, rest)) = c.integer()? {
                        let (s, base) = i.val();
                        let val = $i::from_str_radix(s, base)
                            .or_else(|_| {
                                $u::from_str_radix(s, base).map(|i| i as $i)
                            });
                        return match val {
                            Ok(n) => Ok(((n, c.cur_span()), rest)),
                            Err(_) => Err(c.error(concat!(
                                "invalid ",
                                stringify!($i),
                                " number: constant out of range",
                            ))),
                        };
                    }
                    Err(c.error(concat!("expected a ", stringify!($i))))
                })
            }
        }

        impl Peek for $i {
            fn peek(cursor: Cursor<'_>) -> Result<bool> {
                cursor.peek_integer()
            }

            fn display() -> &'static str {
                stringify!($i)
            }
        }
    )*)
}

integers! {
    u8(u8) u16(u16) u32(u32) u64(u64)
    i8(u8) i16(u16) i32(u32) i64(u64)
}

impl<'a> Parse<'a> for &'a [u8] {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((i, rest)) = c.string()? {
                return Ok((i, rest));
            }
            Err(c.error("expected a string"))
        })
    }
}

impl Peek for &'_ [u8] {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        cursor.peek_string()
    }

    fn display() -> &'static str {
        "string"
    }
}

impl<'a> Parse<'a> for &'a str {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        str::from_utf8(parser.parse()?)
            .map_err(|_| parser.error_at(parser.prev_span(), "malformed UTF-8 encoding"))
    }
}

impl Parse<'_> for String {
    fn parse(parser: Parser<'_>) -> Result<Self> {
        Ok(<&str>::parse(parser)?.to_string())
    }
}

impl Peek for &'_ str {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        <&[u8]>::peek(cursor)
    }

    fn display() -> &'static str {
        <&[u8]>::display()
    }
}

macro_rules! float {
    ($($name:ident => {
        bits: $int:ident,
        float: $float:ident,
        exponent_bits: $exp_bits:tt,
        name: $parse:ident,
    })*) => ($(
        /// A parsed floating-point type
        #[derive(Debug, Copy, Clone)]
        pub struct $name {
            /// The raw bits that this floating point number represents.
            pub bits: $int,
        }

        impl<'a> Parse<'a> for $name {
            fn parse(parser: Parser<'a>) -> Result<Self> {
                parser.step(|c| {
                    let (val, rest) = if let Some((f, rest)) = c.float()? {
                        ($parse(&f), rest)
                    } else if let Some((i, rest)) = c.integer()? {
                        let (s, base) = i.val();
                        (
                            $parse(&Float::Val {
                                hex: base == 16,
                                integral: s.into(),
                                fractional: None,
                                exponent: None,
                            }),
                            rest,
                        )
                    } else {
                        return Err(c.error("expected a float"));
                    };
                    match val {
                        Some(bits) => Ok(($name { bits }, rest)),
                        None => Err(c.error("invalid float value: constant out of range")),
                    }
                })
            }
        }

        fn $parse(val: &Float<'_>) -> Option<$int> {
            // Compute a few well-known constants about the float representation
            // given the parameters to the macro here.
            let width = std::mem::size_of::<$int>() * 8;
            let neg_offset = width - 1;
            let exp_offset = neg_offset - $exp_bits;
            let signif_bits = width - 1 - $exp_bits;
            let signif_mask = (1 << exp_offset) - 1;
            let bias = (1 << ($exp_bits - 1)) - 1;
            let msb = 1 << neg_offset;

            let (hex, integral, fractional, exponent_str) = match val {
                // Infinity is when the exponent bits are all set and
                // the significand is zero.
                Float::Inf { negative } => {
                    let exp_bits = (1 << $exp_bits) - 1;
                    let neg_bit = *negative as $int;
                    return Some(
                        (neg_bit << neg_offset) |
                        (exp_bits << exp_offset)
                    );
                }

                // NaN is when the exponent bits are all set and
                // the significand is nonzero. The default of NaN is
                // when only the highest bit of the significand is set.
                Float::Nan { negative, val } => {
                    let exp_bits = (1 << $exp_bits) - 1;
                    let neg_bit = *negative as $int;
                    let signif = match val {
                        Some(val) => $int::from_str_radix(val,16).ok()?,
                        None => 1 << (signif_bits - 1),
                    };
                    // If the significand is zero then this is actually infinity
                    // so we fail to parse it.
                    if signif & signif_mask == 0 {
                        return None;
                    }
                    return Some(
                        (neg_bit << neg_offset) |
                        (exp_bits << exp_offset) |
                        (signif & signif_mask)
                    );
                }

                // This is trickier, handle this below
                Float::Val { hex, integral, fractional, exponent } => {
                    (hex, integral, fractional, exponent)
                }
            };

            // Rely on Rust's standard library to parse base 10 floats
            // correctly.
            if !*hex {
                let mut s = integral.to_string();
                if let Some(fractional) = fractional {
                    s.push_str(".");
                    s.push_str(&fractional);
                }
                if let Some(exponent) = exponent_str {
                    s.push_str("e");
                    s.push_str(&exponent);
                }
                let float = s.parse::<$float>().ok()?;
                // looks like the `*.wat` format considers infinite overflow to
                // be invalid.
                if float.is_infinite() {
                    return None;
                }
                return Some(float.to_bits());
            }

            // Parse a hexadecimal floating-point value.
            //
            // The main loop here is simpler than for parsing decimal floats,
            // because we can just parse hexadecimal digits and then shift
            // their bits into place in the significand. But in addition to
            // that, we also need to handle non-normalized representations,
            // where the integral part is not "1", to convert them to
            // normalized results, to round, in case we get more digits than
            // the target format supports, and to handle overflow and subnormal
            // cases.

            // Get slices of digits for the integral and fractional parts. We
            // can trivially skip any leading zeros in the integral part.
            let is_negative = integral.starts_with('-');
            let integral = integral.trim_start_matches('-').trim_start_matches('0');
            let fractional = fractional.as_ref().map(|s| &**s).unwrap_or("");

            // Locate the first non-zero digit to determine the initial exponent.
            //
            // If there's no integral part, skip past leading zeros so that
            // something like "0x.0000000000000000000002" doesn't cause us to hit
            // a shift overflow when we try to shift the value into place. We'll
            // adjust the exponent below to account for these skipped zeros.
            let fractional_no_leading = fractional.trim_start_matches('0');
            let fractional_iter = if integral.is_empty() {
                fractional_no_leading.chars()
            } else {
                fractional.chars()
            };

            // Create a unified iterator over the digits of the integral part
            // followed by the digits of the fractional part. The boolean value
            // indicates which of these parts we're in.
            let mut digits = integral.chars()
                .map(|c| (to_hex(c) as $int, false))
                .chain(fractional_iter.map(|c| (to_hex(c) as $int, true)));

            // Compute the number of leading zeros in the first non-zero digit,
            // since if the first digit is not "1" we'll need to adjust for
            // normalization.
            let lead_nonzero_digit = match digits.next() {
                Some((c, _)) => c,
                // No non-zero digits? Must be `+0` or `-0`, being careful to
                // handle the sign encoding here.
                None if is_negative => return Some(msb),
                None => return Some(0),
            };
            let lz = (lead_nonzero_digit as u8).leading_zeros() as i32 - 4;

            // Prepare for the main parsing loop. Calculate the initial values
            // of `exponent` and `significand` based on what we've seen so far.
            let mut exponent = if !integral.is_empty() {
                1
            } else {
                // Adjust the exponent digits to account for any leading zeros
                // in the fractional part that we skipped above.
                -((fractional.len() - fractional_no_leading.len() + 1) as i32) + 1
            };
            let mut significand_pos = (width - (4 - (lz as usize))) as isize;
            let mut significand: $int = lead_nonzero_digit << significand_pos;
            let mut discarded_extra_nonzero = false;

            assert!(significand_pos >= 0, "$int should be at least 4 bits wide");

            // Adjust for leading zeros in the first digit.
            exponent = exponent.checked_mul(4)?.checked_sub(lz + 1)?;

            // Now that we've got an anchor in the string we parse the remaining
            // hexadecimal digits.
            for (digit, in_fractional) in digits {
                if !in_fractional {
                    exponent += 4;
                }
                if significand_pos > -4 {
                    significand_pos -= 4;
                }

                if significand_pos >= 0 {
                    significand |= digit << significand_pos;
                } else if significand_pos > -4 {
                    significand |= digit >> (4 - significand_pos);
                    discarded_extra_nonzero = (digit & !((!0) >> (4 - significand_pos))) != 0;
                } else if digit != 0 {
                    discarded_extra_nonzero = true;
                }
            }

            debug_assert!(significand != 0, "The case of no non-zero digits should have been handled above");

            // Parse the exponent string, which despite this being a hexadecimal
            // syntax, is a decimal number, and add it the exponent we've
            // computed from the potentially non-normalized significand.
            exponent = exponent.checked_add(match exponent_str {
                Some(s) => s.parse::<i32>().ok()?,
                None => 0,
            })?;

            // Encode the exponent and significand. Also calculate the bits of
            // the significand which are discarded, as we'll use them to
            // determine if we need to round up.
            let (encoded_exponent, encoded_significand, discarded_significand) =
                if exponent <= -bias {
                    // Underflow to subnormal or zero.
                    let shift = exp_offset as i32 + exponent + bias;
                    if shift == 0 {
                        (0, 0, significand)
                    } else if shift < 0 || shift >= width as i32 {
                        (0, 0, 0)
                    } else {
                        (
                            0,
                            significand >> (width as i32 - shift),
                            significand << shift,
                        )
                    }
                } else if exponent <= bias {
                    // Normal (non-zero). The significand's leading 1 is encoded
                    // implicitly.
                    (
                        ((exponent + bias) as $int) << exp_offset,
                        (significand >> (width - exp_offset - 1)) & signif_mask,
                        significand << (exp_offset + 1),
                    )
                } else {
                    // Overflow to infinity.
                    (
                        ((1 << $exp_bits) - 1) << exp_offset,
                        0,
                        0,
                    )
                };

            // Combine the encoded exponent and encoded significand to produce
            // the raw result, except for the sign bit, which we'll apply at
            // the end.
            let bits = encoded_exponent | encoded_significand;

            // Apply rounding. Do an integer add of `0` or `1` on the raw
            // result, depending on whether rounding is needed. Rounding can
            // lead to a floating-point overflow, but we don't need to
            // special-case that here because it turns out that IEEE 754 floats
            // are encoded such that when an integer add of `1` carries into
            // the bits of the exponent field, it produces the correct encoding
            // for infinity.
            let bits = bits
                + (((discarded_significand & msb != 0)
                    && ((discarded_significand & !msb != 0) ||
                         discarded_extra_nonzero ||
                         // ties to even
                         (encoded_significand & 1 != 0))) as $int);

            // Just before we return the bits, be sure to handle the sign bit we
            // found at the beginning.
            let bits = if is_negative {
                bits | msb
            } else {
                bits
            };
            // looks like the `*.wat` format considers infinite overflow to
            // be invalid.
            if $float::from_bits(bits).is_infinite() {
                return None;
            }
            Some(bits)
        }

    )*)
}

float! {
    F32 => {
        bits: u32,
        float: f32,
        exponent_bits: 8,
        name: strtof,
    }
    F64 => {
        bits: u64,
        float: f64,
        exponent_bits: 11,
        name: strtod,
    }
}

fn to_hex(c: char) -> u8 {
    match c {
        'a'..='f' => c as u8 - b'a' + 10,
        'A'..='F' => c as u8 - b'A' + 10,
        _ => c as u8 - b'0',
    }
}

/// A convenience type to use with [`Parser::peek`](crate::parser::Parser::peek)
/// to see if the next token is an s-expression.
pub struct LParen {
    _priv: (),
}

impl Peek for LParen {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        cursor.peek_lparen()
    }

    fn display() -> &'static str {
        "left paren"
    }
}

/// A convenience type to use with [`Parser::peek`](crate::parser::Parser::peek)
/// to see if the next token is the end of an s-expression.
pub struct RParen {
    _priv: (),
}

impl Peek for RParen {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        cursor.peek_rparen()
    }

    fn display() -> &'static str {
        "right paren"
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn hex_strtof() {
        macro_rules! f {
            ($a:tt) => (f!(@mk $a, None, None));
            ($a:tt p $e:tt) => (f!(@mk $a, None, Some($e.into())));
            ($a:tt . $b:tt) => (f!(@mk $a, Some($b.into()), None));
            ($a:tt . $b:tt p $e:tt) => (f!(@mk $a, Some($b.into()), Some($e.into())));
            (@mk $a:tt, $b:expr, $e:expr) => (crate::lexer::Float::Val {
                hex: true,
                integral: $a.into(),
                fractional: $b,
                exponent: $e
            });
        }
        assert_eq!(super::strtof(&f!("0")), Some(0));
        assert_eq!(super::strtof(&f!("0" . "0")), Some(0));
        assert_eq!(super::strtof(&f!("0" . "0" p "2354")), Some(0));
        assert_eq!(super::strtof(&f!("-0")), Some(1 << 31));
        assert_eq!(super::strtof(&f!("f32")), Some(0x45732000));
        assert_eq!(super::strtof(&f!("0" . "f32")), Some(0x3f732000));
        assert_eq!(super::strtof(&f!("1" . "2")), Some(0x3f900000));
        assert_eq!(
            super::strtof(&f!("0" . "00000100000000000" p "-126")),
            Some(0)
        );
        assert_eq!(
            super::strtof(&f!("1" . "fffff4" p "-106")),
            Some(0x0afffffa)
        );
        assert_eq!(super::strtof(&f!("fffff98" p "-133")), Some(0x0afffffa));
        assert_eq!(super::strtof(&f!("0" . "081" p "023")), Some(0x48810000));
        assert_eq!(
            super::strtof(&f!("1" . "00000100000000000" p "-50")),
            Some(0x26800000)
        );
    }
}
