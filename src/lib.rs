//! A Rust parser for the [WebAssembly Text format][wat]
//!
//! This crate contains a stable interface to the parser for the [WAT][wat]
//! format of WebAssembly text files. The format parsed by this crate follows
//! the [online specification][wat].
//!
//! # Examples
//!
//! Parse an in-memory string:
//!
//! ```
//! # fn foo() -> wat::Result<()> {
//! let wat = r#"
//!     (module
//!         (func $foo)
//!
//!         (func (export "bar")
//!             call $foo
//!         )
//!     )
//! "#;
//!
//! let binary = wat::parse_str(wat)?;
//! // ...
//! # Ok(())
//! # }
//! ```
//!
//! Parse an on-disk file:
//!
//! ```
//! # fn foo() -> wat::Result<()> {
//! let binary = wat::parse_file("./foo.wat")?;
//! // ...
//! # Ok(())
//! # }
//! ```
//!
//! ## Evolution of the WAT Format
//!
//! WebAssembly, and the WAT format, are an evolving specification. Features are
//! added to WAT, WAT changes, and sometimes WAT breaks. The policy of this
//! crate is that it will always follow the [official specification][wat] for
//! WAT files.
//!
//! Future WebAssembly features will be accepted to this parser **and they will
//! not require a feature gate to opt-in**. All implemented WebAssembly features
//! will be enabled at all times. Using a future WebAssembly feature in the WAT
//! format may cause breakage because while specifications are in development
//! the WAT syntax (and/or binary encoding) will often change. This crate will
//! do its best to keep up with these proposals, but breaking textual changes
//! will be published as non-breaking semver changes to this crate.
//!
//! ## Stability
//!
//! This crate is intended to be a very stable shim over the `wast` crate
//! which is expected to be much more unstable. The `wast` crate contains
//! AST data structures for parsing `*.wat` files and they will evolve was the
//! WAT and WebAssembly specifications evolve over time.
//!
//! This crate is currently at version 1.x.y, and it is intended that it will
//! remain here for quite some time. Breaking changes to the WAT format will be
//! landed as a non-semver-breaking version change in this crate. This crate
//! will always follow the [official specification for WAT][wat].
//!
//! [wat]: http://webassembly.github.io/spec/core/text/index.html

#![deny(missing_docs)]

use std::fmt;
use std::path::Path;
use wast::parser::{self, ParseBuffer};

/// Parses a file on disk as a [WebAssembly Text format][wat] file, returning
/// the file translated to a WebAssembly binary file.
///
/// For more information see the [`parse_str`] documentation.
///
/// # Errors
///
/// For information about errors, see the [`parse_str`] documentation.
///
/// # Examples
///
/// ```
/// # fn foo() -> wat::Result<()> {
/// let binary = wat::parse_file("./foo.wat")?;
/// // ...
/// # Ok(())
/// # }
/// ```
///
/// [wat]: http://webassembly.github.io/spec/core/text/index.html
pub fn parse_file(file: impl AsRef<Path>) -> Result<Vec<u8>> {
    _parse_file(file.as_ref())
}

fn _parse_file(file: &Path) -> Result<Vec<u8>> {
    let contents = std::fs::read_to_string(file).map_err(|err| Error {
        kind: Box::new(ErrorKind::Io {
            err,
            msg: format!("failed to read `{}` to a string", file.display()),
        }),
    })?;
    parse_str(&contents).map_err(|mut e| {
        if let ErrorKind::Wast(e) = &mut *e.kind {
            e.set_path(file);
        }
        e
    })
}

/// Parses an in-memory string as the [WebAssembly Text format][wat], returning
/// the file as a binary WebAssembly file.
///
/// This function is intended to be a stable convenience function for parsing a
/// wat file into a WebAssembly binary file. This is a high-level operation
/// which does not expose any parsing internals, for that you'll want to use the
/// `wast` crate.
///
/// # Errors
///
/// This function can fail for a number of reasons, including (but not limited
/// to):
///
/// * The `wat` input may fail to lex, such as having invalid tokens or syntax
/// * The `wat` input may fail to parse, such as having incorrect syntactical
///   structure
/// * The `wat` input may contain names that could not be resolved
///
/// # Examples
///
/// ```
/// # fn foo() -> wat::Result<()> {
/// assert_eq!(wat::parse_str("(module)")?, b"\0asm\x01\0\0\0");
/// assert!(wat::parse_str("module").is_err());
///
/// let wat = r#"
///     (module
///         (func $foo)
///
///         (func (export "bar")
///             call $foo
///         )
///     )
/// "#;
///
/// let binary = wat::parse_str(wat)?;
/// // ...
/// # Ok(())
/// # }
/// ```
///
/// [wat]: http://webassembly.github.io/spec/core/text/index.html
pub fn parse_str(wat: impl AsRef<str>) -> Result<Vec<u8>> {
    _parse_str(wat.as_ref())
}

fn _parse_str(wat: &str) -> Result<Vec<u8>> {
    let buf = ParseBuffer::new(&wat).map_err(|e| Error::cvt(e, wat))?;
    let mut ast = parser::parse::<wast::Wat>(&buf).map_err(|e| Error::cvt(e, wat))?;
    Ok(ast.module.encode().map_err(|e| Error::cvt(e, wat))?)
}

/// A convenience type definition for `Result` where the error is [`Error`]
pub type Result<T> = std::result::Result<T, Error>;

/// Errors from this crate related to parsing WAT files
///
/// An error can during example phases like:
///
/// * Lexing can fail if the document is syntactically invalid.
/// * A string may not be utf-8
/// * The syntactical structure of the wat file may be invalid
/// * The wat file may be semantically invalid such as having name resolution
///   failures
#[derive(Debug)]
pub struct Error {
    kind: Box<ErrorKind>,
}

#[derive(Debug)]
enum ErrorKind {
    Wast(wast::Error),
    Io { err: std::io::Error, msg: String },
}

impl Error {
    fn cvt<E: Into<wast::Error>>(e: E, contents: &str) -> Error {
        let mut err = e.into();
        err.set_text(contents);
        Error {
            kind: Box::new(ErrorKind::Wast(err)),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.kind {
            ErrorKind::Wast(err) => err.fmt(f),
            ErrorKind::Io { msg, .. } => msg.fmt(f),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &*self.kind {
            ErrorKind::Wast(_) => None,
            ErrorKind::Io { err, .. } => Some(err),
        }
    }
}
