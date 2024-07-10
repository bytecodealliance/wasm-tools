use std::fmt;

use crate::{Ident, Render};

/// Enable the union of a world with another world
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Use {
    target: Ident,
    use_names_list: Vec<(String, Option<String>)>,
}

impl Use {
    pub fn new(use_target: impl Into<Ident>) -> Self {
        Self {
            target: use_target.into(),
            use_names_list: vec![],
        }
    }

    pub fn item(&mut self, id: &str, alias: Option<&str>) {
        self.use_names_list
            .push((id.to_string(), alias.map(|s| s.to_string())));
    }
}

impl Render for Use {
    fn render(&self, f: &mut fmt::Formatter<'_>, opts: &crate::RenderOpts) -> fmt::Result {
        let len = self.use_names_list.len();

        write!(f, "{}use {}.{{ ", opts.spaces(), self.target)?;
        for (i, (id, alias)) in self.use_names_list.iter().enumerate() {
            if let Some(alias) = alias {
                write!(f, "{id} as {alias}")?;
            } else {
                write!(f, "{id}")?;
            }
            if i < len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, " }};\n")?;
        Ok(())
    }
}
