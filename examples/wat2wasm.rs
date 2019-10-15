use anyhow::Context;
use std::env;
use std::fmt;
use std::fs;
use std::io::{self, Read};

fn main() -> anyhow::Result<()> {
    let (file, input) = match env::args().nth(1) {
        Some(f) => {
            let contents = fs::read_to_string(&f).context(format!("failed to read: {}", f))?;
            (f, contents)
        }
        None => {
            let mut dst = String::new();
            io::stdin()
                .read_to_string(&mut dst)
                .context("failed to read stdin")?;
            ("<stdin>".to_string(), dst)
        }
    };
    let buf = match wast::parser::ParseBuffer::new(&input) {
        Ok(b) => b,
        Err(e) => return Err(render_error(&file, &input, e.line(), e.col(), &e)),
    };
    let mut wat = match wast::parser::parse::<wast::ast::Wat>(&buf) {
        Ok(m) => m,
        Err(e) => return Err(render_error(&file, &input, e.line(), e.col(), &e)),
    };
    match wast::resolve::resolve(&mut wat.module) {
        Ok(()) => (),
        Err(e) => return Err(render_error(&file, &input, e.line(), e.col(), &e)),
    }
    let _binary = wast::binary::encode(&wat.module);
    Ok(())
}

fn render_error(
    file: &str,
    contents: &str,
    line: usize,
    col: usize,
    err: &dyn fmt::Display,
) -> anyhow::Error {
    anyhow::anyhow!(
        "
error: {err}
     --> {file}:{line}:{col}
      |
 {line:4} | {text}
      | {marker:>0$}
",
        col + 1,
        file = file,
        line = line + 1,
        col = col + 1,
        err = err,
        text = contents.lines().nth(line).unwrap_or(""),
        marker = "^",
    )
}
