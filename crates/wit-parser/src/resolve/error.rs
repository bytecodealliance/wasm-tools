use alloc::boxed::Box;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;

use crate::{PackageName, SourceMap, Span};

#[derive(Clone, Debug)]
pub struct ResolveErrors(Box<[ResolveErrorKind]>);

impl ResolveErrors {
    pub(crate) fn single(kind: ResolveErrorKind) -> Self {
        ResolveErrors(Box::new([kind]))
    }

    pub fn iter(&self) -> impl Iterator<Item = &ResolveErrorKind> {
        self.0.iter()
    }

    pub fn highlight(&self, source_map: &SourceMap) -> String {
        self.0
              .iter()
              .map(|e| {
                  let msg = e.to_string();
                  match e {
                      ResolveErrorKind::DuplicatePackage { name, span1, span2 } => {
                          let loc1 = source_map.render_location(*span1);
                          let loc2 = source_map.render_location(*span2);
                          format!("package `{name}` is defined in two different locations:\n  * {loc1}\n  * {loc2}")
                      }
                      _ => source_map
                          .highlight_span(e.span(), &msg)
                          .unwrap_or(msg),
                  }
              })
              .collect::<alloc::vec::Vec<_>>()
              .join("\n")
    }
}

impl fmt::Display for ResolveErrors {
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

impl core::error::Error for ResolveErrors {}

#[non_exhaustive]
#[derive(Clone, Debug)]
pub enum ResolveErrorKind {
    PackageNotFound {
        span: Span,
        requested: PackageName,
        known: Vec<PackageName>,
    },
    InvalidTransitiveDependency {
        span: Span,
        name: String,
    },
    DuplicatePackage {
        name: PackageName,
        span1: Span,
        span2: Span,
    },
    PackageCycle {
        package: PackageName,
        span: Span,
    },
    Semantic {
        span: Span,
        message: String,
    },
}

impl ResolveErrorKind {
    pub fn span(&self) -> Span {
        match self {
            ResolveErrorKind::PackageNotFound { span, .. }
            | ResolveErrorKind::InvalidTransitiveDependency { span, .. }
            | ResolveErrorKind::PackageCycle { span, .. }
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
                    write!(f, "package `{requested}` not found")
                } else {
                    write!(f, "package `{requested}` not found; known packages:")?;
                    for k in known {
                        write!(f, "\n  {k}")?;
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
            ResolveErrorKind::Semantic { message, .. } => message.fmt(f),
        }
    }
}
