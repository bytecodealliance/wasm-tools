use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RenderOpts {
    /// width of each indent
    pub indent_width: usize,
    /// current indent depth
    pub ident_count: usize,
}

impl Default for RenderOpts {
    fn default() -> Self {
        Self {
            indent_width: 4,
            ident_count: 0,
        }
    }
}

impl RenderOpts {
    /// Indent
    ///
    /// This will clone Self, and increment self.ident_count.
    pub fn indent(&self) -> Self {
        Self {
            indent_width: self.indent_width,
            ident_count: self.ident_count + 1,
        }
    }

    /// Outdent
    ///
    /// This will clone Self, and decrement self.ident_count.
    pub fn outdent(&self) -> Self {
        Self {
            indent_width: self.indent_width,
            ident_count: self.ident_count - 1,
        }
    }

    /// Get the actual characters
    pub fn spaces(&self) -> String {
        let space_count = self.indent_width * self.ident_count;
        " ".repeat(space_count)
    }
}

pub trait Render {
    fn render(&self, f: &mut fmt::Formatter<'_>, options: &RenderOpts) -> fmt::Result;
}
