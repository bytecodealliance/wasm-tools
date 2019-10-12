use wart::parser::{Parse, ParseBuffer};

macro_rules! assert_parses {
    ($text:expr, $item:expr) => {{
        let pb = wart::parser::ParseBuffer::new($text).unwrap();
        crate::assert_parses(&pb, $item);
    }};
}

macro_rules! assert_not_parses {
    ($text:expr, $ty:ty, $msg:expr) => {{
        let pb = wart::parser::ParseBuffer::new($text).unwrap();
        crate::assert_not_parses::<$ty>(&pb, $msg);
    }};
}

mod expr;
mod memarg;
mod token;
mod types;

fn assert_parses<'a, T>(buf: &'a ParseBuffer<'a>, reference: T)
where
    T: Parse<'a> + PartialEq + std::fmt::Debug,
{
    let result = buf.parser().parse::<T>().expect("failed to parse");
    if !buf.parser().is_empty() {
        panic!("extra tokens");
    }
    assert_eq!(result, reference);
}

fn assert_not_parses<'a, T>(buf: &'a ParseBuffer<'a>, msg: &str)
where
    T: Parse<'a> + PartialEq + std::fmt::Debug,
{
    let err = buf.parser().parse::<T>().unwrap_err();
    assert!(
        err.to_string().contains(msg),
        "`{}` did not contain `{}`",
        err,
        msg,
    );
}
