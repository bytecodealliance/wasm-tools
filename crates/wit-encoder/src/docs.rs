use std::fmt;

use crate::{Render, RenderOpts};

/// Documentation
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Docs {
    contents: String,
}

impl Docs {
    pub fn new(contents: impl Into<String>) -> Self {
        Self {
            contents: contents.into(),
        }
    }
}

impl<S> From<S> for Docs
where
    S: Into<String>,
{
    fn from(value: S) -> Self {
        Self {
            contents: value.into(),
        }
    }
}

impl Render for Docs {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &RenderOpts) -> fmt::Result {
        for line in self.contents.lines() {
            write!(f, "{}/// {}\n", opts.spaces(), line)?;
        }
        Ok(())
    }
}
