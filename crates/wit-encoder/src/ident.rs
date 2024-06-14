use std::{borrow::Cow, fmt};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(Cow<'static, str>);

impl Ident {
    pub fn new(s: impl Into<Cow<'static, str>>) -> Self {
        let s: Cow<'static, str> = s.into();
        if is_keyword(&s) {
            Self(Cow::Owned(format!("%{}", s)))
        } else {
            Self(s)
        }
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
        self.0.fmt(f)
    }
}

fn is_keyword(name: &str) -> bool {
    match name {
        "u8" | "u16" | "u32" | "u64" | "s8" | "s16" | "s32" | "s64" | "float32" | "float64"
        | "char" | "bool" | "string" | "tuple" | "list" | "option" | "result" | "use" | "type"
        | "resource" | "func" | "record" | "enum" | "flags" | "variant" | "static"
        | "interface" | "world" | "import" | "export" | "package" => true,
        _ => false,
    }
}
