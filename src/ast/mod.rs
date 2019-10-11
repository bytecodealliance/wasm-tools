#[macro_export]
macro_rules! custom_keyword {
    ($name:ident) => {
        $crate::custom_keyword!($name = stringify!($name));
    };
    ($name:ident = $kw:expr) => {
        #[allow(non_camel_case_types)]
        pub struct $name {
            _priv: (),
        }

        impl<'a> $crate::parser::Parse<'a> for $name {
            fn parse(parser: $crate::parser::Parser<'a>) -> $crate::parser::Result<Self> {
                parser.step(|c| {
                    if let Some((kw, rest)) = c.keyword() {
                        if kw == $kw {
                            return Ok(($name { _priv: () }, rest));
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
        }
    };
}

mod data;
pub use data::*;
mod types;
pub use types::*;
mod token;
pub use token::*;

pub mod kw {
    custom_keyword!(i32);
    custom_keyword!(i64);
    custom_keyword!(f32);
    custom_keyword!(f64);
    custom_keyword!(func);
    custom_keyword!(param);
    custom_keyword!(result);
    custom_keyword!(funcref);
    custom_keyword!(r#mut = "mut");
}
