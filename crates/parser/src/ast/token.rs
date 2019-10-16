use crate::lexer::FloatVal;
use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::str;

/// A position in the original source stream, used to render errors.
#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub(crate) offset: usize,
}

/// An identifier in a WebAssembly module, prefixed by `$` in the textual
/// format.
///
/// An identifier is used to symbolically refer to items in a a wasm module,
/// typically via the [`Index`] type.
#[derive(Copy, Clone)]
pub struct Id<'a> {
    name: &'a str,
    span: Span,
}

impl<'a> Id<'a> {
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
}

impl<'a> Hash for Id<'a> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
    }
}

impl<'a> PartialEq for Id<'a> {
    fn eq(&self, other: &Id<'a>) -> bool {
        self.name == other.name
    }
}

impl<'a> Eq for Id<'a> {}

impl<'a> Parse<'a> for Id<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((name, rest)) = c.id() {
                return Ok((
                    Id {
                        name,
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
        self.name.fmt(f)
    }
}

impl Peek for Id<'_> {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.id().is_some()
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
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Index<'a> {
    /// A numerical index that this references. The index space this is
    /// referencing is implicit based on where this [`Index`] is stored.
    Num(u32),
    /// A human-readable identifier this references. Like `Num`, the namespace
    /// this references is based on where this is stored.
    Id(Id<'a>),
}

impl<'a> Parse<'a> for Index<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<Id>() {
            Ok(Index::Id(parser.parse()?))
        } else if l.peek::<u32>() {
            Ok(Index::Num(parser.parse()?))
        } else {
            Err(l.error())
        }
    }
}

impl Peek for Index<'_> {
    fn peek(cursor: Cursor<'_>) -> bool {
        u32::peek(cursor) || Id::peek(cursor)
    }

    fn display() -> &'static str {
        "an index"
    }
}

macro_rules! integers {
    ($($i:ident($u:ident))*) => ($(
        impl<'a> Parse<'a> for $i {
            fn parse(parser: Parser<'a>) -> Result<Self> {
                parser.step(|c| {
                    if let Some((i, rest)) = c.integer() {
                        let (s, base) = i.val();
                        let val = $i::from_str_radix(s, base)
                            .or_else(|_| {
                                $u::from_str_radix(s, base).map(|i| i as $i)
                            });
                        return match val {
                            Ok(n) => Ok((n, rest)),
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
            fn peek(cursor: Cursor<'_>) -> bool {
                cursor.integer().is_some()
            }

            fn display() -> &'static str {
                stringify!($i)
            }
        }
    )*)
}

integers! { u32(u32) i32(u32) i64(u64) }

impl<'a> Parse<'a> for &'a [u8] {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.step(|c| {
            if let Some((i, rest)) = c.string() {
                return Ok((i, rest));
            }
            Err(c.error("expected a string"))
        })
    }
}

impl Peek for &'_ [u8] {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.string().is_some()
    }

    fn display() -> &'static str {
        "string"
    }
}

impl<'a> Parse<'a> for &'a str {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        str::from_utf8(parser.parse()?).map_err(|_| parser.error("invalid UTF-8 encoding"))
    }
}

impl Peek for &'_ str {
    fn peek(cursor: Cursor<'_>) -> bool {
        <&[u8]>::peek(cursor)
    }

    fn display() -> &'static str {
        <&[u8]>::display()
    }
}

macro_rules! float {
    ($($name:ident => ($int:ident, $float:ident, $exp_bits:tt, $parse:ident))*) => ($(
        /// A parsed floating-point type
        #[derive(Debug)]
        pub struct $name {
            /// The raw bits that this floating point number represents.
            pub bits: $int,
        }

        impl<'a> Parse<'a> for $name {
            fn parse(parser: Parser<'a>) -> Result<Self> {
                fn handle(val: &FloatVal<'_>) -> Option<$int> {
                    use std::ffi::CString;
                    use std::os::raw::c_char;
                    use std::ptr;

                    let width = std::mem::size_of::<$int>() * 8;
                    let neg_offset = width - 1;
                    let exp_offset = neg_offset - $exp_bits;
                    let signif_bits = width - 1 - $exp_bits;
                    let signif_mask = (1 << exp_offset) - 1;
                    let (hex, integral, decimal, exponent) = match val {
                        FloatVal::Inf { negative } => {
                            let exp_bits = (1 << $exp_bits) - 1;
                            let neg_bit = *negative as $int;
                            return Some(
                                (neg_bit << neg_offset) |
                                (exp_bits << exp_offset)
                            );
                        }

                        FloatVal::Nan { negative, val } => {
                            let exp_bits = (1 << $exp_bits) - 1;
                            let neg_bit = *negative as $int;
                            let signif = val.unwrap_or(1 << (signif_bits - 1)) as $int;
                            return Some(
                                (neg_bit << neg_offset) |
                                (exp_bits << exp_offset) |
                                (signif & signif_mask)
                            );
                        }

                        FloatVal::Val { hex, integral, decimal, exponent } => {
                            (hex, integral, decimal, exponent)
                        }
                    };

                    // Rely on Rust's standard library to parse base 10 floats
                    // correctly.
                    if !*hex {
                        let mut s = integral.to_string();
                        if let Some(decimal) = decimal {
                            s.push_str(".");
                            s.push_str(&decimal);
                        }
                        if let Some(exponent) = exponent {
                            s.push_str("e");
                            s.push_str(&exponent);
                        }
                        return s.parse::<$float>().ok().map(|s| s.to_bits());
                    }

                    // FIXME(#13) shouldn't use C here, we should use something
                    // Rust-based
                    let (sign, num) = if integral.starts_with("-") {
                        ("-", &integral[1..])
                    } else {
                        ("", &integral[..])
                    };
                    let mut s = format!("{}0x{}", sign, num);
                    if let Some(decimal) = decimal {
                        s.push_str(".");
                        s.push_str(&decimal);
                    }
                    if let Some(exponent) = exponent {
                        s.push_str("p");
                        s.push_str(&exponent);
                    }
                    let s = CString::new(s).unwrap();

                    // Match what wabt does for now and use
                    // `strtof` and `strtod` until hex float
                    // parsing in Rust is up to par.
                    extern {
                        fn $parse(input: *const c_char, other: *mut *mut c_char) -> $float;
                    }
                    let ret = unsafe {
                        $parse(
                            s.as_ptr(),
                            ptr::null_mut(),
                        )
                    };
                    Some(ret.to_bits())
                }

                parser.step(|c| {
                    let (val, rest) = if let Some((f, rest)) = c.float() {
                        (handle(f.val()), rest)
                    } else if let Some((i, rest)) = c.integer() {
                        let (s, base) = i.val();
                        (
                            handle(&FloatVal::Val {
                                hex: base == 16,
                                integral: s.into(),
                                decimal: None,
                                exponent: None,
                            }),
                            rest,
                        )
                    } else {
                        return Err(c.error("expected a float"));
                    };
                    match val {
                        Some(bits) => Ok(($name { bits }, rest)),
                        None => Err(c.error("invalid float value")),
                    }
                })
            }
        }
    )*)
}

float! {
    Float32 => (u32, f32, 8, strtof)
    Float64 => (u64, f64, 11, strtod)
}

/// A convenience type to use with [`Parser::peek`](crate::parser::Parser::peek)
/// to see if the next token is an s-expression.
pub struct LParen {
    _priv: (),
}

impl Peek for LParen {
    fn peek(cursor: Cursor<'_>) -> bool {
        cursor.lparen().is_some()
    }

    fn display() -> &'static str {
        "left paren"
    }
}
