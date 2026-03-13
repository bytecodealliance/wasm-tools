use alloc::boxed::Box;
use alloc::string::{String, ToString};
use core::fmt;

use crate::{SourceMap, Span, ast::lex};

#[non_exhaustive]
#[derive(Debug, PartialEq, Eq)]
pub enum PackageParseErrorKind {
    /// Lexer error (invalid character, unterminated comment, etc.)
    Lex(lex::Error),
    /// Syntactic or semantic error within a single package (duplicate name,
    /// invalid attribute, etc.)
    Syntax { span: Span, message: String },
    /// A type/interface/world references a name that does not exist within
    /// the same package. Arises from within-package toposort.
    ItemNotFound {
        span: Span,
        name: String,
        kind: String,
        hint: Option<String>,
    },
    /// A type/interface/world depends on itself. Arises from within-package
    /// toposort.
    TypeCycle {
        span: Span,
        name: String,
        kind: String,
    },
}

impl PackageParseErrorKind {
    pub fn span(&self) -> Span {
        match self {
            PackageParseErrorKind::Lex(e) => Span::new(e.position(), e.position() + 1),
            PackageParseErrorKind::Syntax { span, .. }
            | PackageParseErrorKind::ItemNotFound { span, .. }
            | PackageParseErrorKind::TypeCycle { span, .. } => *span,
        }
    }
}

impl fmt::Display for PackageParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PackageParseErrorKind::Lex(e) => fmt::Display::fmt(e, f),
            PackageParseErrorKind::Syntax { message, .. } => message.fmt(f),
            PackageParseErrorKind::ItemNotFound {
                kind, name, hint, ..
            } => {
                write!(f, "{kind} `{name}` does not exist")?;
                if let Some(hint) = hint {
                    write!(f, "\n{hint}")?;
                }
                Ok(())
            }
            PackageParseErrorKind::TypeCycle { kind, name, .. } => {
                write!(f, "{kind} `{name}` depends on itself")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PackageParseErrors(Box<PackageParseErrorKind>);

impl PackageParseErrors {
    pub fn kind(&self) -> &PackageParseErrorKind {
        &self.0
    }

    /// Format this error with source context (file:line:col + snippet)
    pub fn highlight(&self, source_map: &SourceMap) -> String {
        let e = self.kind();
        source_map
            .highlight_span(e.span(), e)
            .unwrap_or_else(|| e.to_string())
    }
}

impl fmt::Display for PackageParseErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.kind(), f)
    }
}

impl core::error::Error for PackageParseErrors {}

impl From<PackageParseErrorKind> for PackageParseErrors {
    fn from(kind: PackageParseErrorKind) -> Self {
        PackageParseErrors(Box::new(kind))
    }
}

impl From<lex::Error> for PackageParseErrors {
    fn from(e: lex::Error) -> Self {
        PackageParseErrorKind::Lex(e).into()
    }
}
