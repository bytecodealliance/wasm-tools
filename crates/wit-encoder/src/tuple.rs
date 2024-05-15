use std::fmt::Display;

use crate::Type;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Tuple {
    pub types: Vec<Type>,
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
