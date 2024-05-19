use crate::Render;

/// Documentation
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Default)]
pub struct Docs {
    pub contents: String,
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
    fn render_opts(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        opts: crate::RenderOpts,
    ) -> std::fmt::Result {
        for line in self.contents.lines() {
            write!(f, "{:depth$}/// {}\n", "", line, depth = opts.indent(depth))?;
        }
        Ok(())
    }
}
