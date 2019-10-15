//! A crate for low-level parsing of the WebAssembly text formats: WAT and WAST.
//!
//! This crate is intended to be a low-level detail of the `wast` crate,
//! providing a low-level parsing API for parsing WebAssembly text format
//! structures. The API provided by this crate is very similar to
//! [`syn`](https://docs.rs/syn) and provides the ability to write customized
//! parsers which may be an extension to the core WebAssembly text format. For
//! more documentation see the [`parser`] module.
//!
//! # High-level Overview
//!
//! This crate provides a few major pieces of functionality
//!
//! * [`lexer`] - this is a raw lexer for the wasm text format. This is not
//!   customizable, but if you'd like to iterate over raw tokens this is the
//!   module for you. You likely won't use this much.
//!
//! * [`parser`] - this is the workhorse of this crate. The [`parser`] module
//!   provides the [`Parse`][] trait primarily and utilities
//!   around working with a [`Parser`](`parser::Parser`) to parse streams of
//!   tokens.
//!
//! * [`Module`] - this contains an Abstract Syntax Tree (AST) of the
//!   WebAssembly Text format (WAT) as well as the unofficial WAST format. This
//!   also has a [`Module::encode`] method to emit a module in its binary form.
//!
//! # Errors
//!
//! Naturally with parsing a lot of errors can happen. This crate strives to
//! provide useful error information wherever it can, also ideally providing
//! convenient ways to render the error in a user-readable fashion. The
//! low-level error types of [`LexError`][] or [`parser::Error`], and contain
//! positional and detailed information about what went wrong.
//!
//! A convenience [`Error`] type is also provided to unify all these errors and
//! provide utilities for rendering them all in a "pretty" format.
//!
//! # Stability and WebAssembly Features
//!
//! This crate provides support for many in-progress WebAssembly features such
//! as reference types, multi-value, etc. Be sure to check out the documentation
//! of the [`wast` crate](https://docs.rs/wast) for policy information on crate
//! stability vs WebAssembly Features. The tl;dr; version is that this crate
//! will issue semver-non-breaking releases which will break the parsing of the
//! text format. This crate, unlike `wast`, is expected to have numerous Rust
//! public API changes, all of which will be accompanied with a semver-breaking
//! release.
//!
//! [`Parse`]: parser::Parse
//! [`LexError`]: lexer::LexError

#![deny(missing_docs)]

use std::fmt;
use std::path::{Path, PathBuf};

mod binary;
mod resolve;

mod ast;
pub use self::ast::*;

pub mod lexer;
pub mod parser;

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

/// A convenience error type to tie together all the detailed errors produced by
/// this crate.
///
/// This type can be created from a [`lexer::LexError`] or [`parser::Error`].
/// This also contains storage for file/text information so a nice error can be
/// rendered along the same lines of rustc's own error messages (minus the
/// color).
///
/// This type is typically suitable for use in public APIs for consumers of this
/// crate.
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
enum ErrorKind {
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
        let file = self
            .inner
            .file
            .as_ref()
            .and_then(|p| p.to_str())
            .unwrap_or("<anon>");
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
