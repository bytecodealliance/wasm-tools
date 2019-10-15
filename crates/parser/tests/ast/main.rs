use wast_parser::parser::{Parse, ParseBuffer};

macro_rules! assert_parses {
    ($text:expr, $item:expr) => {{
        let pb = wast_parser::parser::ParseBuffer::new($text).unwrap();
        crate::assert_parses($text, &pb, $item);
    }};
}

macro_rules! assert_not_parses {
    ($text:expr, $ty:ty, $msg:expr) => {{
        let pb = wast_parser::parser::ParseBuffer::new($text).unwrap();
        crate::assert_not_parses::<$ty>(&pb, $msg);
    }};
}

mod expr;
mod token;
mod types;

fn assert_parses<'a, T>(orig: &str, buf: &'a ParseBuffer<'a>, reference: T)
where
    T: Parse<'a> + PartialEq + std::fmt::Debug,
{
    let result = wast_parser::parser::parse::<T>(buf).unwrap_or_else(|e| {
        let mut err = wast_parser::Error::from(e);
        err.set_text(orig);
        panic!("{}", err)
    });
    assert_eq!(result, reference);
}

fn assert_not_parses<'a, T>(buf: &'a ParseBuffer<'a>, msg: &str)
where
    T: Parse<'a> + PartialEq + std::fmt::Debug,
{
    let err = wast_parser::parser::parse::<T>(buf).unwrap_err();
    assert!(
        err.to_string().contains(msg),
        "`{}` did not contain `{}`",
        err,
        msg,
    );
}
