/// A macro to create a custom keyword parser.
///
/// This macro is invoked in one of two forms:
///
/// ```
/// // keyword derived from the Rust identifier:
/// wast::custom_keyword!(foo);
///
/// // or an explicitly specified string representation of the keyword:
/// wast::custom_keyword!(my_keyword = "the-wasm-keyword");
/// ```
///
/// This can then be used to parse custom keyword for custom items, such as:
///
/// ```
/// use wast::parser::{Parser, Result, Parse};
///
/// struct InlineModule<'a> {
///     inline_text: &'a str,
/// }
///
/// mod kw {
///     wast::custom_keyword!(inline);
/// }
///
/// // Parse an inline string module of the form:
/// //
/// //    (inline "(module (func))")
/// impl<'a> Parse<'a> for InlineModule<'a> {
///     fn parse(parser: Parser<'a>) -> Result<Self> {
///         parser.parse::<kw::inline>()?;
///         Ok(InlineModule {
///             inline_text: parser.parse()?,
///         })
///     }
/// }
/// ```
#[macro_export]
macro_rules! custom_keyword {
    ($name:ident) => {
        $crate::custom_keyword!($name = stringify!($name));
    };
    ($name:ident = $kw:expr) => {
        #[allow(non_camel_case_types)]
        #[allow(missing_docs)]
        #[derive(Debug)]
        pub struct $name(pub $crate::Span);

        impl<'a> $crate::parser::Parse<'a> for $name {
            fn parse(parser: $crate::parser::Parser<'a>) -> $crate::parser::Result<Self> {
                parser.step(|c| {
                    if let Some((kw, rest)) = c.keyword() {
                        if kw == $kw {
                            return Ok(($name(c.cur_span()), rest));
                        }
                    }
                    Err(c.error(concat!("expected keyword `", $kw, "`")))
                })
            }
        }

        impl $crate::parser::Peek for $name {
            fn peek(cursor: $crate::parser::Cursor<'_>) -> bool {
                if let Some((kw, _rest)) = cursor.keyword() {
                    kw == $kw
                } else {
                    false
                }
            }

            fn display() -> &'static str {
                concat!("`", $kw, "`")
            }
        }
    };
}

macro_rules! reexport {
    ($(mod $name:ident;)*) => ($(mod $name; pub use self::$name::*;)*);
}

reexport! {
    mod token;
}

#[cfg(feature = "wasm-module")]
reexport! {
    mod assert_expr;
    mod export;
    mod expr;
    mod func;
    mod global;
    mod import;
    mod memory;
    mod module;
    mod table;
    mod types;
    mod wast;
}

/// Common keyword used to parse WebAssembly text files.
pub mod kw {
    custom_keyword!(anyfunc);
    custom_keyword!(anyref);
    custom_keyword!(assert_exhaustion);
    custom_keyword!(assert_invalid);
    custom_keyword!(assert_malformed);
    custom_keyword!(assert_return);
    custom_keyword!(assert_return_arithmetic_nan);
    custom_keyword!(assert_return_arithmetic_nan_f32x4);
    custom_keyword!(assert_return_arithmetic_nan_f64x2);
    custom_keyword!(assert_return_canonical_nan);
    custom_keyword!(assert_return_canonical_nan_f32x4);
    custom_keyword!(assert_return_canonical_nan_f64x2);
    custom_keyword!(assert_return_func);
    custom_keyword!(assert_trap);
    custom_keyword!(assert_unlinkable);
    custom_keyword!(binary);
    custom_keyword!(block);
    custom_keyword!(data);
    custom_keyword!(elem);
    custom_keyword!(end);
    custom_keyword!(export);
    custom_keyword!(f32);
    custom_keyword!(f32x4);
    custom_keyword!(f64);
    custom_keyword!(f64x2);
    custom_keyword!(func);
    custom_keyword!(funcref);
    custom_keyword!(get);
    custom_keyword!(global);
    custom_keyword!(i16x8);
    custom_keyword!(i32);
    custom_keyword!(i32x4);
    custom_keyword!(i64);
    custom_keyword!(i64x2);
    custom_keyword!(i8x16);
    custom_keyword!(import);
    custom_keyword!(invoke);
    custom_keyword!(local);
    custom_keyword!(memory);
    custom_keyword!(module);
    custom_keyword!(nan_arithmetic = "nan:arithmetic");
    custom_keyword!(nan_canonical = "nan:canonical");
    custom_keyword!(nullref);
    custom_keyword!(offset);
    custom_keyword!(param);
    custom_keyword!(passive);
    custom_keyword!(quote);
    custom_keyword!(r#else = "else");
    custom_keyword!(r#if = "if");
    custom_keyword!(r#loop = "loop");
    custom_keyword!(r#mut = "mut");
    custom_keyword!(r#type = "type");
    custom_keyword!(ref_func = "ref.func");
    custom_keyword!(ref_null = "ref.null");
    custom_keyword!(register);
    custom_keyword!(result);
    custom_keyword!(shared);
    custom_keyword!(start);
    custom_keyword!(table);
    custom_keyword!(then);
    custom_keyword!(v128);
}
