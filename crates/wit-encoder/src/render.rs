use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct RenderOpts {
    /// width of each indent
    pub indent_width: usize,
    /// current indent depth
    pub indent_count: usize,
}

impl Default for RenderOpts {
    fn default() -> Self {
        Self {
            indent_width: 2,
            indent_count: 0,
        }
    }
}

impl RenderOpts {
    /// Indent
    ///
    /// This will clone Self, and increment self.indent_count.
    pub fn indent(&self) -> Self {
        Self {
            indent_width: self.indent_width,
            indent_count: self.indent_count + 1,
        }
    }

    /// Outdent
    ///
    /// This will clone Self, and decrement self.indent_count.
    pub fn outdent(&self) -> Self {
        Self {
            indent_width: self.indent_width,
            indent_count: self.indent_count - 1,
        }
    }

    /// Get the actual characters
    pub fn spaces(&self) -> String {
        let space_count = self.indent_width * self.indent_count;
        " ".repeat(space_count)
    }
}

pub trait Render {
    fn render(&self, f: &mut fmt::Formatter<'_>, options: &RenderOpts) -> fmt::Result;
}
