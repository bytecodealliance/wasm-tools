use wast_parser::parser::{Parse, ParseBuffer};

macro_rules! assert_parses {
    ($text:expr, $item:expr) => {{
        let pb = wast_parser::parser::ParseBuffer::new($text).unwrap();
        crate::assert_parses($text, &pb, $item);
    }};
}

mod expr;

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
