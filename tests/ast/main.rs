use wast::parser::{Parse, ParseBuffer};

macro_rules! assert_parses {
    ($text:expr, $item:expr) => {{
        let pb = wast::parser::ParseBuffer::new($text).unwrap();
        crate::assert_parses($text, &pb, $item);
    }};
}

macro_rules! assert_not_parses {
    ($text:expr, $ty:ty, $msg:expr) => {{
        let pb = wast::parser::ParseBuffer::new($text).unwrap();
        crate::assert_not_parses::<$ty>(&pb, $msg);
    }};
}

mod expr;
mod memarg;
mod token;
mod types;

fn assert_parses<'a, T>(orig: &str, buf: &'a ParseBuffer<'a>, reference: T)
where
    T: Parse<'a> + PartialEq + std::fmt::Debug,
{
    let result = buf
        .parser()
        .parse::<T>()
        .unwrap_or_else(|e| render_err(orig, e));
    if !buf.parser().is_empty() {
        panic!("extra tokens when parsing {:?}", orig);
    }
    assert_eq!(result, reference);
}

fn render_err(input: &str, err: wast::parser::Error) -> ! {
    eprintln!("");
    eprintln!("error: {}", err);
    eprintln!("     --> <anon>:{}:{}", err.line() + 1, err.col() + 1);
    eprintln!("      |");
    eprintln!(
        " {:4} | {}",
        err.line() + 1,
        input.lines().nth(err.line()).unwrap()
    );
    eprintln!("      | {1:>0$}", err.col() + 1, "^");
    eprintln!("");
    panic!("{}", err);
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
