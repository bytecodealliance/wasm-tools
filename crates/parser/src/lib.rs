use std::fmt;
use std::path::{Path, PathBuf};

pub mod ast;
pub mod binary;
pub mod lexer;
pub mod parser;
pub mod resolve;

/// Converts an offset within `self.input` to a line/column number.
///
/// Note that both the line and the column are 0-based.
fn to_linecol(input: &str, offset: usize) -> (usize, usize) {
    let mut cur = 0;
    // Use split_terminator instead of lines so that if there is a `\r`,
    // it is included in the offset calculation. The `+1` values below
    // account for the `\n`.
    for (i, line) in input.split_terminator('\n').enumerate() {
        if cur + line.len() + 1 > offset {
            return (i, offset - cur);
        }
        cur += line.len() + 1;
    }
    (input.lines().count(), 0)
}

#[derive(Debug)]
pub struct Error {
    inner: Box<ErrorInner>,
}

#[derive(Debug)]
struct ErrorInner {
    text: Option<String>,
    file: Option<PathBuf>,
    kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    Lex(lexer::LexError),
    Parse(parser::Error),
    Resolve(resolve::ResolveError),
}

impl Error {
    fn new(kind: ErrorKind) -> Error {
        Error {
            inner: Box::new(ErrorInner {
                text: None,
                file: None,
                kind,
            }),
        }
    }

    /// To provide a more useful error this function can be used to extract
    /// relevant textual information about this error into the error itself.
    ///
    /// The `contents` here should be the full text of the original file being
    /// parsed, and this will extract a sub-slice as necessary to render in the
    /// `Display` implementation later on.
    pub fn set_text(&mut self, contents: &str) {
        if self.inner.text.is_some() {
            return;
        }
        self.inner.text = Some(contents.lines().nth(self.line()).unwrap_or("").to_string());
    }

    /// To provide a more useful error this function can be used to set
    /// the file name that this error is associated with.
    ///
    /// The `path` here will be stored in this error and later rendered in the
    /// `Display` implementation.
    pub fn set_path(&mut self, path: &Path) {
        if self.inner.file.is_some() {
            return;
        }
        self.inner.file = Some(path.to_path_buf());
    }

    /// Returns the 0-indexed line number that this error happened at
    pub fn line(&self) -> usize {
        match &self.inner.kind {
            ErrorKind::Lex(e) => e.line(),
            ErrorKind::Parse(e) => e.line(),
            ErrorKind::Resolve(e) => e.line(),
        }
    }

    /// Returns the 0-indexed column number that this error happened at
    pub fn col(&self) -> usize {
        match &self.inner.kind {
            ErrorKind::Lex(e) => e.col(),
            ErrorKind::Parse(e) => e.col(),
            ErrorKind::Resolve(e) => e.col(),
        }
    }
}

impl From<lexer::LexError> for Error {
    fn from(err: lexer::LexError) -> Error {
        Error::new(ErrorKind::Lex(err))
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Error {
        Error::new(ErrorKind::Parse(err))
    }
}

impl From<resolve::ResolveError> for Error {
    fn from(err: resolve::ResolveError) -> Error {
        Error::new(ErrorKind::Resolve(err))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let file = self.inner.file.as_ref().and_then(|p| p.to_str()).unwrap_or("<anon>");
        let empty = String::new();
        let text = self.inner.text.as_ref().unwrap_or(&empty);
        let err = match &self.inner.kind {
            ErrorKind::Lex(e) => e as &dyn fmt::Display,
            ErrorKind::Parse(e) => e as &dyn fmt::Display,
            ErrorKind::Resolve(e) => e as &dyn fmt::Display,
        };
        write!(
            f,
            "\
{err}
     --> {file}:{line}:{col}
      |
 {line:4} | {text}
      | {marker:>0$}",
            self.col() + 1,
            file = file,
            line = self.line() + 1,
            col = self.col() + 1,
            err = err,
            text = text,
            marker = "^",
        )
    }
}

impl std::error::Error for Error {}
