use alloc::boxed::Box;
use alloc::string::{String, ToString};
use core::fmt;

use crate::{
    SourceMap, Span,
    ast::{lex, toposort},
};

#[derive(Debug)]
pub struct ParseErrors(Box<[ParseErrorKind]>);

#[non_exhaustive]
#[derive(Debug)]
pub enum ParseErrorKind {
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

impl ParseErrors {
    pub(crate) fn single(kind: ParseErrorKind) -> Self {
        ParseErrors(Box::new([kind]))
    }

    pub fn iter(&self) -> impl Iterator<Item = &ParseErrorKind> {
        self.0.iter()
    }

    pub fn highlight(&self, source_map: &SourceMap) -> String {
        self.0
            .iter()
            .map(|e| {
                source_map
                    .highlight_span(e.span(), e)
                    .unwrap_or_else(|| e.to_string())
            })
            .collect::<alloc::vec::Vec<_>>()
            .join("\n")
    }
}

impl fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, e) in self.0.iter().enumerate() {
            if i > 0 {
                f.write_str("\n")?;
            }
            fmt::Display::fmt(e, f)?;
        }
        Ok(())
    }
}

impl core::error::Error for ParseErrors {}

impl From<lex::Error> for ParseErrors {
    fn from(e: lex::Error) -> Self {
        ParseErrors::single(ParseErrorKind::Lex(e))
    }
}

impl From<toposort::Error> for ParseErrors {
    fn from(e: toposort::Error) -> Self {
        let kind = match e {
            toposort::Error::NonexistentDep {
                span,
                name,
                kind,
                hint,
            } => ParseErrorKind::ItemNotFound {
                span,
                name,
                kind,
                hint,
            },
            toposort::Error::Cycle { span, name, kind } => {
                ParseErrorKind::TypeCycle { span, name, kind }
            }
        };
        ParseErrors::single(kind)
    }
}
