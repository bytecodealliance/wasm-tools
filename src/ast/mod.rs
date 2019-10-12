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

            fn display() -> &'static str {
                concat!("`", $kw, "`")
            }
        }
    };
}

mod export;
mod expr;
mod func;
mod global;
mod import;
mod memory;
mod module;
mod table;
mod token;
mod types;
pub use export::*;
pub use expr::*;
pub use func::*;
pub use global::*;
pub use import::*;
pub use memory::*;
pub use module::*;
pub use table::*;
pub use token::*;
pub use types::*;

pub mod kw {
    custom_keyword!(anyfunc);
    custom_keyword!(anyref);
    custom_keyword!(block);
    custom_keyword!(data);
    custom_keyword!(elem);
    custom_keyword!(end);
    custom_keyword!(export);
    custom_keyword!(f32);
    custom_keyword!(f64);
    custom_keyword!(func);
    custom_keyword!(funcref);
    custom_keyword!(global);
    custom_keyword!(i32);
    custom_keyword!(i64);
    custom_keyword!(import);
    custom_keyword!(local);
    custom_keyword!(memory);
    custom_keyword!(module);
    custom_keyword!(offset);
    custom_keyword!(param);
    custom_keyword!(r#else = "else");
    custom_keyword!(r#if = "if");
    custom_keyword!(r#loop = "loop");
    custom_keyword!(r#mut = "mut");
    custom_keyword!(r#type = "type");
    custom_keyword!(result);
    custom_keyword!(shared);
    custom_keyword!(start);
    custom_keyword!(table);
    custom_keyword!(then);
}
