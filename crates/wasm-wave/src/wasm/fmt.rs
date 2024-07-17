use std::fmt::Display;

use crate::{
    wasm::{WasmFunc, WasmType, WasmTypeKind},
    writer::Writer,
    WasmValue,
};

/// Implements a WAVE-formatted [`Display`] for a [`WasmType`].
pub struct DisplayType<'a, T: WasmType>(pub &'a T);

impl<'a, T: WasmType> Display for DisplayType<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ty = self.0;
        match ty.kind() {
            WasmTypeKind::List => {
                write!(f, "list<{}>", DisplayType(&ty.list_element_type().unwrap()))
            }
            WasmTypeKind::Record => {
                f.write_str("record { ")?;
                for (idx, (name, field_type)) in ty.record_fields().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{name}: {}", DisplayType(&field_type))?;
                }
                f.write_str(" }")
            }
            WasmTypeKind::Tuple => {
                f.write_str("tuple<")?;
                for (idx, ty) in ty.tuple_element_types().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}", DisplayType(&ty))?;
                }
                f.write_str(">")
            }
            WasmTypeKind::Variant => {
                f.write_str("variant { ")?;
                for (idx, (name, payload)) in ty.variant_cases().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    f.write_str(name.as_ref())?;
                    if let Some(ty) = payload {
                        write!(f, "({})", DisplayType(&ty))?;
                    }
                }
                f.write_str(" }")
            }
            WasmTypeKind::Enum => {
                f.write_str("enum { ")?;
                for (idx, name) in ty.enum_cases().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    f.write_str(name.as_ref())?;
                }
                f.write_str(" }")
            }
            WasmTypeKind::Option => {
                write!(
                    f,
                    "option<{}>",
                    DisplayType(&ty.option_some_type().unwrap())
                )
            }
            WasmTypeKind::Result => {
                f.write_str("result")?;
                match ty.result_types().unwrap() {
                    (None, None) => Ok(()),
                    (None, Some(err)) => write!(f, "<_, {}>", DisplayType(&err)),
                    (Some(ok), None) => write!(f, "<{}>", DisplayType(&ok)),
                    (Some(ok), Some(err)) => {
                        write!(f, "<{}, {}>", DisplayType(&ok), DisplayType(&err))
                    }
                }
            }
            WasmTypeKind::Flags => {
                f.write_str("flags { ")?;
                for (idx, name) in ty.flags_names().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    f.write_str(name.as_ref())?;
                }
                f.write_str(" }")
            }
            simple => Display::fmt(&simple, f),
        }
    }
}

/// Implements a WAVE-formatted [`Display`] for a [`WasmValue`].
pub struct DisplayValue<'a, T: WasmValue>(pub &'a T);

impl<'a, T: WasmValue> Display for DisplayValue<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = vec![];
        Writer::new(&mut buf)
            .write_value(self.0)
            .map_err(|_| std::fmt::Error)?;
        f.write_str(String::from_utf8_lossy(&buf).as_ref())
    }
}

/// Implements a WAVE-formatted [`Display`] for a [`WasmFunc`].
pub struct DisplayFunc<T: WasmFunc>(pub T);

impl<T: WasmFunc> Display for DisplayFunc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("func(")?;
        let mut param_names = self.0.param_names();
        for (idx, ty) in self.0.params().enumerate() {
            if idx != 0 {
                f.write_str(", ")?;
            }
            if let Some(name) = param_names.next() {
                write!(f, "{name}: ")?;
            }
            DisplayType(&ty).fmt(f)?
        }
        f.write_str(")")?;

        let results = self.0.results().collect::<Vec<_>>();
        if results.is_empty() {
            return Ok(());
        }

        let mut result_names = self.0.result_names();
        if results.len() == 1 {
            let ty = DisplayType(&results.into_iter().next().unwrap()).to_string();
            if let Some(name) = result_names.next() {
                write!(f, " -> ({name}: {ty})")
            } else {
                write!(f, " -> {ty}")
            }
        } else {
            f.write_str(" -> (")?;
            for (idx, ty) in results.into_iter().enumerate() {
                if idx != 0 {
                    f.write_str(", ")?;
                }
                if let Some(name) = result_names.next() {
                    write!(f, "{name}: ")?;
                }
                DisplayType(&ty).fmt(f)?;
            }
            f.write_str(")")
        }
    }
}

/// Implements a WAVE-formatted [`Display`] for [`WasmValue`] func arguments.
pub struct DisplayFuncArgs<'a, T: WasmValue>(pub &'a [T]);

impl<'a, T: WasmValue> Display for DisplayFuncArgs<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(")?;
        for (idx, v) in self.0.iter().enumerate() {
            if idx != 0 {
                f.write_str(", ")?;
            }
            DisplayValue(v).fmt(f)?;
        }
        f.write_str(")")
    }
}

/// Implements a WAVE-formatted [`Display`] for [`WasmValue`] func results.
pub struct DisplayFuncResults<'a, T: WasmValue>(pub &'a [T]);

impl<'a, T: WasmValue> Display for DisplayFuncResults<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.len() == 1 {
            DisplayValue(&self.0[0]).fmt(f)
        } else {
            DisplayFuncArgs(self.0).fmt(f)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Type;

    #[test]
    fn test_type_display() {
        for (ty, expected) in [
            (Type::U8, "u8"),
            (Type::list(Type::U8), "list<u8>"),
            (
                Type::record([("a", Type::U8), ("b", Type::STRING)]).unwrap(),
                "record { a: u8, b: string }",
            ),
            (
                Type::tuple([Type::U8, Type::BOOL]).unwrap(),
                "tuple<u8, bool>",
            ),
            (
                Type::variant([("off", None), ("on", Some(Type::U8))]).unwrap(),
                "variant { off, on(u8) }",
            ),
            (
                Type::enum_ty(["east", "west"]).unwrap(),
                "enum { east, west }",
            ),
            (Type::option(Type::U8), "option<u8>"),
            (Type::result(None, None), "result"),
            (Type::result(Some(Type::U8), None), "result<u8>"),
            (Type::result(None, Some(Type::STRING)), "result<_, string>"),
            (
                Type::result(Some(Type::U8), Some(Type::STRING)),
                "result<u8, string>",
            ),
            (
                Type::flags(["read", "write"]).unwrap(),
                "flags { read, write }",
            ),
        ] {
            assert_eq!(ty.to_string(), expected);
        }
    }
}
