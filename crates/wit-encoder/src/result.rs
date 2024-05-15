use crate::Type;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Result_ {
    pub ok: Option<Type>,
    pub err: Option<Type>,
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
