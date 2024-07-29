use std::fmt;

use crate::{Ident, Render};

/// Enable the union of a world with another world
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Use {
    target: Ident,
    use_names_list: Vec<(Ident, Option<Ident>)>,
}

impl Use {
    pub fn new(use_target: impl Into<Ident>) -> Self {
        Self {
            target: use_target.into(),
            use_names_list: vec![],
        }
    }

    pub fn target(&self) -> &Ident {
        &self.target
    }

    pub fn set_target(&mut self, target: Ident) {
        self.target = target;
    }

    // `alias` is a concrete type because of https://github.com/rust-lang/rust/issues/36887
    pub fn item(&mut self, id: impl Into<Ident>, alias: Option<Ident>) {
        self.use_names_list
            .push((id.into(), alias.map(|s| s.into())));
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
