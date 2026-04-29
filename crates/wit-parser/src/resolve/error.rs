//! Error types for WIT package resolution.

use alloc::boxed::Box;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt::{self};

use crate::{PackageName, SourceMap, Span, Stability};

/// Convenience alias for a `Result` whose error type is [`ResolveError`].
pub type ResolveResult<T, E = ResolveError> = Result<T, E>;

/// The category of error that occurred while resolving a WIT package.
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq)]
pub enum ResolveErrorKind {
    /// A referenced package could not be found among the known packages.
    PackageNotFound {
        span: Span,
        requested: PackageName,
        known: Vec<PackageName>,
    },
    /// An interface has a transitive dependency that creates an incompatible
    /// import relationship.
    InvalidTransitiveDependency { span: Span, name: String },
    /// The same package is defined in two different locations.
    DuplicatePackage {
        name: PackageName,
        span1: Span,
        span2: Span,
    },
    /// Packages form a dependency cycle.
    PackageCycle { package: PackageName, span: Span },
    /// A world item shadows a previously-included item of the same kind
    ItemShadowing {
        span: Span,
        item_type: String,
        name: String,
    },
    /// Two stability annotations conflict during merge
    StabilityMismatch {
        span: Span,
        from: Stability,
        into: Stability,
    },
    /// A semantic error during resolution (type mismatch, invalid use, etc.)
    Semantic { span: Span, message: String },
}

impl ResolveErrorKind {
    /// Returns the source span associated with this error.
    pub fn span(&self) -> Span {
        match self {
            ResolveErrorKind::PackageNotFound { span, .. }
            | ResolveErrorKind::InvalidTransitiveDependency { span, .. }
            | ResolveErrorKind::PackageCycle { span, .. }
            | ResolveErrorKind::ItemShadowing { span, .. }
            | ResolveErrorKind::StabilityMismatch { span, .. }
            | ResolveErrorKind::Semantic { span, .. } => *span,
            ResolveErrorKind::DuplicatePackage { span1, .. } => *span1,
        }
    }
}

impl fmt::Display for ResolveErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolveErrorKind::PackageNotFound {
                requested, known, ..
            } => {
                if known.is_empty() {
                    write!(f, "package '{requested}' not found")
                } else {
                    write!(f, "package '{requested}' not found. known packages:")?;
                    for k in known {
                        write!(f, "\n    {k}")?;
                    }
                    Ok(())
                }
            }
            ResolveErrorKind::InvalidTransitiveDependency { name, .. } => write!(
                f,
                "interface `{name}` transitively depends on an interface in incompatible ways",
            ),
            ResolveErrorKind::DuplicatePackage { name, .. } => {
                write!(f, "package `{name}` is defined in two different locations",)
            }
            ResolveErrorKind::PackageCycle { package, .. } => {
                write!(f, "package `{package}` creates a dependency cycle")
            }
            ResolveErrorKind::ItemShadowing {
                item_type, name, ..
            } => {
                write!(
                    f,
                    "{item_type} of `{name}` shadows previously {item_type}ed items"
                )
            }
            ResolveErrorKind::StabilityMismatch { from, into, .. } => {
                write!(f, "mismatch in stability from '{from:?}' to '{into:?}'")
            }
            ResolveErrorKind::Semantic { message, .. } => message.fmt(f),
        }
    }
}

/// A single structured error from resolving a WIT package.
#[derive(Debug, PartialEq, Eq)]
pub struct ResolveError(Box<ResolveErrorKind>);

impl ResolveError {
    /// Creates a [`ResolveError`] with the [`ResolveErrorKind::Semantic`] variant.
    pub fn new_semantic(span: Span, message: impl Into<String>) -> Self {
        ResolveErrorKind::Semantic {
            span,
            message: message.into(),
        }
        .into()
    }

    /// Returns the underlying error kind.
    pub fn kind(&self) -> &ResolveErrorKind {
        &self.0
    }

    /// Returns the underlying error kind (mutable).
    pub fn kind_mut(&mut self) -> &mut ResolveErrorKind {
        &mut self.0
    }

    /// Format this error with source context (file:line:col + snippet).
    pub fn highlight(&self, source_map: &SourceMap) -> String {
        let e = self.kind();
        let msg = e.to_string();
        match e {
            ResolveErrorKind::DuplicatePackage { name, span1, span2 } => {
                let loc1 = source_map.render_location(*span1);
                let loc2 = source_map.render_location(*span2);
                format!(
                    "package `{name}` is defined in two different locations:\n  * {loc1}\n  * {loc2}"
                )
            }
            _ => source_map.highlight_span(e.span(), &msg).unwrap_or(msg),
        }
    }
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.kind(), f)
    }
}

impl core::error::Error for ResolveError {}

impl From<ResolveErrorKind> for ResolveError {
    fn from(kind: ResolveErrorKind) -> Self {
        ResolveError(Box::new(kind))
    }
}
