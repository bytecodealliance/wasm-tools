use alloc::boxed::Box;
use alloc::string::{String, ToString};
use core::fmt;

use crate::{SourceMap, Span, ast::lex};

pub type ParseResult<T, E = ParseError> = Result<T, E>;

#[non_exhaustive]
#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrorKind {
    /// Lexer error (invalid character, unterminated comment, etc.)
    Lex(lex::Error),
    /// Syntactic or semantic error within a single package (duplicate name,
    /// invalid attribute, etc.)
    Syntax { span: Span, message: String },
    /// A type/interface/world references a name that does not exist within
    /// the same package.
    ItemNotFound {
        span: Span,
        name: String,
        kind: String,
        hint: Option<String>,
    },
    /// A type/interface/world depends on itself.
    TypeCycle {
        span: Span,
        name: String,
        kind: String,
    },
}

impl ParseErrorKind {
    pub fn span(&self) -> Span {
        match self {
            ParseErrorKind::Lex(e) => Span::new(e.position(), e.position() + 1),
            ParseErrorKind::Syntax { span, .. }
            | ParseErrorKind::ItemNotFound { span, .. }
            | ParseErrorKind::TypeCycle { span, .. } => *span,
        }
    }
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::Lex(e) => fmt::Display::fmt(e, f),
            ParseErrorKind::Syntax { message, .. } => message.fmt(f),
            ParseErrorKind::ItemNotFound {
                kind, name, hint, ..
            } => {
                write!(f, "{kind} `{name}` does not exist")?;
                if let Some(hint) = hint {
                    write!(f, "\n{hint}")?;
                }
                Ok(())
            }
            ParseErrorKind::TypeCycle { kind, name, .. } => {
                write!(f, "{kind} `{name}` depends on itself")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError(Box<ParseErrorKind>);

impl ParseError {
    pub fn new_syntax(span: Span, message: impl Into<String>) -> Self {
        ParseErrorKind::Syntax {
            span,
            message: message.into(),
        }
        .into()
    }

    pub fn kind(&self) -> &ParseErrorKind {
        &self.0
    }

    pub fn kind_mut(&mut self) -> &mut ParseErrorKind {
        &mut self.0
    }

    /// Format this error with source context (file:line:col + snippet)
    pub fn highlight(&self, source_map: &SourceMap) -> String {
        let e = self.kind();
        source_map
            .highlight_span(e.span(), e)
            .unwrap_or_else(|| e.to_string())
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.kind(), f)
    }
}

impl core::error::Error for ParseError {}

impl From<ParseErrorKind> for ParseError {
    fn from(kind: ParseErrorKind) -> Self {
        ParseError(Box::new(kind))
    }
}

impl From<lex::Error> for ParseError {
    fn from(e: lex::Error) -> Self {
        ParseErrorKind::Lex(e).into()
    }
}
