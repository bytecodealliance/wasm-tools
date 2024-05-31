use std::fmt::Display;

use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    pub(crate) types: Vec<Type>,
}

impl Tuple {
    pub fn types(&self) -> &[Type] {
        &self.types
    }

    pub fn types_mut(&mut self) -> &mut Vec<Type> {
        &mut self.types
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "tuple<")?;
        let mut peekable = self.types.iter().peekable();
        while let Some(type_) = peekable.next() {
            type_.fmt(f)?;
            if peekable.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ">")?;
        Ok(())
    }
}
