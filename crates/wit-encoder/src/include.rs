use std::fmt;

use crate::{Ident, Render};

/// Enable the union of a world with another world
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Include {
    use_path: Ident,
    include_names_list: Vec<String>,
}

impl Include {
    pub fn new(use_path: impl Into<Ident>) -> Self {
        Self {
            use_path: use_path.into(),
            include_names_list: vec![],
        }
    }
}

impl Render for Include {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &crate::RenderOpts) -> fmt::Result {
        write!(f, "{}include {};\n", opts.spaces(), self.use_path)?;
        Ok(())
    }
}
