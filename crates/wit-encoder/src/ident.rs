use std::{borrow::Cow, fmt};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "kebab-case"))]
pub struct Ident(Cow<'static, str>);

impl Ident {
    pub fn new(s: impl Into<Cow<'static, str>>) -> Self {
        Self(s.into())
    }

    /// Get the name without escaping keywords with '%'
    pub fn raw_name(&self) -> &str {
        self.0.as_ref()
    }
}

impl<S> From<S> for Ident
where
    S: Into<Cow<'static, str>>,
{
    fn from(value: S) -> Self {
        Self::new(value)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if is_keyword(&self.0) {
            write!(f, "%")?;
        }
        self.0.fmt(f)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

fn is_keyword(name: &str) -> bool {
    match name {
        "u8" | "u16" | "u32" | "u64" | "s8" | "s16" | "s32" | "s64" | "f32" | "f64" | "char"
        | "bool" | "string" | "tuple" | "list" | "option" | "result" | "use" | "type"
        | "resource" | "func" | "record" | "enum" | "flags" | "variant" | "static"
        | "interface" | "world" | "import" | "export" | "package" | "own" | "borrow" => true,
        _ => false,
    }
}
