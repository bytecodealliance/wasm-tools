use std::fmt;

use crate::{Ident, Render};

/// Enable the union of a world with another world
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Include {
    use_path: Ident,
    include_names_list: Vec<(String, String)>,
}

impl Include {
    pub fn new(use_path: impl Into<Ident>) -> Self {
        Self {
            use_path: use_path.into(),
            include_names_list: vec![],
        }
    }

    pub fn with(&mut self, id: &str, alias: &str) {
        self.include_names_list
            .push((id.to_string(), alias.to_string()));
    }
}

impl Render for Include {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &crate::RenderOpts) -> fmt::Result {
        match self.include_names_list.len() {
            0 => write!(f, "{}include {};\n", opts.spaces(), self.use_path)?,
            len => {
                write!(f, "{}include {} with {{ ", opts.spaces(), self.use_path)?;
                for (i, (id, alias)) in self.include_names_list.iter().enumerate() {
                    if i == len - 1 {
                        write!(f, "{id} as {alias}")?;
                    } else {
                        write!(f, "{id} as {alias}, ")?;
                    }
                }
                write!(f, " }};\n")?;
            }
        }
        Ok(())
    }
}
