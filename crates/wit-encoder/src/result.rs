use std::fmt::Display;

use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Result_ {
    ok: Option<Type>,
    err: Option<Type>,
}

impl Result_ {
    pub fn ok(type_: Type) -> Self {
        Self {
            ok: Some(type_),
            err: None,
        }
    }
    pub fn err(type_: Type) -> Self {
        Self {
            ok: None,
            err: Some(type_),
        }
    }
    pub fn both(ok: Type, err: Type) -> Self {
        Self {
            ok: Some(ok),
            err: Some(err),
        }
    }
    pub fn empty() -> Self {
        Self {
            ok: None,
            err: None,
        }
    }
    pub fn is_empty(&self) -> bool {
        self.ok.is_none() && self.err.is_none()
    }
}

impl Display for Result_ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "result")?;
        if !self.is_empty() {
            write!(f, "<")?;
            if let Some(type_) = &self.ok {
                type_.fmt(f)?;
            } else {
                write!(f, "_")?;
            }
            if let Some(type_) = &self.err {
                write!(f, ", ")?;
                type_.fmt(f)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}
