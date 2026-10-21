//! WAVE writer

use core::fmt::Write;

use thiserror::Error;

use crate::{
    lex::Keyword,
    wasm::{WasmTypeKind, WasmValue},
};

/// A Web Assembly Value Encoding writer.
///
/// Writes to the wrapped `W` writer.
pub struct Writer<W> {
    inner: W,
}

impl<W: Write> Writer<W> {
    /// Returns a new Writer for the given [`std::io::Write`].
    pub fn new(w: W) -> Self {
        Self { inner: w }
    }

    /// WAVE-encodes and writes the given [`WasmValue`] to the underlying writer.
    ///
    /// Returns [`WriterError::UnsupportedType`] if `val`, or any value nested
    /// within it, has a type that WAVE cannot encode. Output written before the
    /// error was encountered is left in the underlying writer.
    pub fn write_value<V>(&mut self, val: &V) -> Result<(), WriterError>
    where
        V: WasmValue,
    {
        match val.kind() {
            WasmTypeKind::Bool => self.write_str(if val.unwrap_bool() { "true" } else { "false" }),
            WasmTypeKind::S8 => self.write_display(val.unwrap_s8()),
            WasmTypeKind::S16 => self.write_display(val.unwrap_s16()),
            WasmTypeKind::S32 => self.write_display(val.unwrap_s32()),
            WasmTypeKind::S64 => self.write_display(val.unwrap_s64()),
            WasmTypeKind::U8 => self.write_display(val.unwrap_u8()),
            WasmTypeKind::U16 => self.write_display(val.unwrap_u16()),
            WasmTypeKind::U32 => self.write_display(val.unwrap_u32()),
            WasmTypeKind::U64 => self.write_display(val.unwrap_u64()),
            WasmTypeKind::F32 => {
                let f = val.unwrap_f32();
                if f.is_nan() {
                    self.write_str("nan") // Display is "NaN"
                } else {
                    self.write_display(f)
                }
            }
            WasmTypeKind::F64 => {
                let f = val.unwrap_f64();
                if f.is_nan() {
                    self.write_str("nan") // Display is "NaN"
                } else {
                    self.write_display(f)
                }
            }
            WasmTypeKind::Char => {
                self.write_str("'")?;
                self.write_char(val.unwrap_char())?;
                self.write_str("'")
            }
            WasmTypeKind::String => {
                self.write_str("\"")?;
                for ch in val.unwrap_string().chars() {
                    self.write_char(ch)?;
                }
                self.write_str("\"")
            }
            WasmTypeKind::List => {
                self.write_str("[")?;
                for (idx, val) in val.unwrap_list().enumerate() {
                    if idx != 0 {
                        self.write_str(", ")?;
                    }
                    self.write_value(&*val)?;
                }
                self.write_str("]")
            }
            WasmTypeKind::FixedLengthList => {
                self.write_str("[")?;
                for (idx, val) in val.unwrap_list().enumerate() {
                    if idx != 0 {
                        self.write_str(", ")?;
                    }
                    self.write_value(&*val)?;
                }
                self.write_str("]")
            }
            WasmTypeKind::Record => {
                self.write_str("{")?;
                let mut first = true;
                for (name, val) in val.unwrap_record() {
                    if !matches!(val.kind(), WasmTypeKind::Option) || val.unwrap_option().is_some()
                    {
                        if first {
                            first = false;
                        } else {
                            self.write_str(", ")?;
                        }
                        self.write_str(name)?;
                        self.write_str(": ")?;
                        self.write_value(&*val)?;
                    }
                }
                if first {
                    self.write_str(":")?;
                }
                self.write_str("}")
            }
            WasmTypeKind::Tuple => {
                self.write_str("(")?;
                for (idx, val) in val.unwrap_tuple().enumerate() {
                    if idx != 0 {
                        self.write_str(", ")?;
                    }
                    self.write_value(&*val)?;
                }
                self.write_str(")")
            }
            WasmTypeKind::Variant => {
                let (name, val) = val.unwrap_variant();
                if Keyword::decode(&name).is_some() {
                    self.write_char('%')?;
                }
                self.write_str(name)?;
                if let Some(val) = val {
                    self.write_str("(")?;
                    self.write_value(&*val)?;
                    self.write_str(")")?;
                }
                Ok(())
            }
            WasmTypeKind::Enum => {
                let case = val.unwrap_enum();
                if Keyword::decode(&case).is_some() {
                    self.write_char('%')?;
                }
                self.write_str(case)
            }
            WasmTypeKind::Option => match val.unwrap_option() {
                Some(val) => {
                    self.write_str("some(")?;
                    self.write_value(&*val)?;
                    self.write_str(")")
                }
                None => self.write_str("none"),
            },
            WasmTypeKind::Result => {
                let (name, val) = match val.unwrap_result() {
                    Ok(val) => ("ok", val),
                    Err(val) => ("err", val),
                };
                self.write_str(name)?;
                if let Some(val) = val {
                    self.write_str("(")?;
                    self.write_value(&*val)?;
                    self.write_str(")")?;
                }
                Ok(())
            }
            WasmTypeKind::Flags => {
                self.write_str("{")?;
                for (idx, name) in val.unwrap_flags().enumerate() {
                    if idx != 0 {
                        self.write_str(", ")?;
                    }
                    self.write_str(name)?;
                }
                self.write_str("}")?;
                Ok(())
            }
            WasmTypeKind::Unsupported => Err(WriterError::UnsupportedType),
        }
    }

    fn write_str(&mut self, s: impl AsRef<str>) -> Result<(), WriterError> {
        self.inner.write_str(s.as_ref())?;
        Ok(())
    }

    fn write_display(&mut self, d: impl core::fmt::Display) -> Result<(), WriterError> {
        write!(self.inner, "{d}")?;
        Ok(())
    }

    fn write_char(&mut self, ch: char) -> Result<(), WriterError> {
        if "\\\"\'\t\r\n".contains(ch) {
            write!(self.inner, "{}", ch.escape_default())?;
        } else if ch.is_control() {
            write!(self.inner, "{}", ch.escape_unicode())?;
        } else {
            write!(self.inner, "{}", ch.escape_debug())?;
        }
        Ok(())
    }
}

impl<W> AsMut<W> for Writer<W> {
    fn as_mut(&mut self) -> &mut W {
        &mut self.inner
    }
}

/// A Writer error.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum WriterError {
    /// An error from the underlying writer
    #[error("write failed: {0}")]
    Io(#[from] core::fmt::Error),
    /// The value (or a value nested within it) has a type that WAVE cannot
    /// encode.
    #[error("unsupported value type")]
    UnsupportedType,
}

#[cfg(test)]
mod tests {
    use alloc::{borrow::Cow, boxed::Box, string::String, vec, vec::Vec};
    use core::fmt::Write;

    use super::WriterError;
    use crate::{
        value::Type,
        wasm::{DisplayValue, WasmTypeKind, WasmValue},
    };

    /// A minimal [`WasmValue`] that can represent a value WAVE can't encode.
    #[derive(Clone)]
    enum TestValue {
        List(Vec<TestValue>),
        Unsupported,
    }

    impl WasmValue for TestValue {
        type Type = Type;

        fn kind(&self) -> WasmTypeKind {
            match self {
                TestValue::List(_) => WasmTypeKind::List,
                TestValue::Unsupported => WasmTypeKind::Unsupported,
            }
        }

        fn unwrap_list(&self) -> Box<dyn Iterator<Item = Cow<'_, Self>> + '_> {
            match self {
                TestValue::List(vals) => Box::new(vals.iter().map(Cow::Borrowed)),
                TestValue::Unsupported => panic!("not a list"),
            }
        }
    }

    #[test]
    fn unsupported_value_is_an_error() {
        let err = crate::to_string(&TestValue::Unsupported).unwrap_err();
        assert!(matches!(err, WriterError::UnsupportedType), "{err:?}");

        let nested = TestValue::List(vec![TestValue::List(vec![]), TestValue::Unsupported]);
        let err = crate::to_string(&nested).unwrap_err();
        assert!(matches!(err, WriterError::UnsupportedType), "{err:?}");

        // Through `Display` the error surfaces as a `fmt::Error`.
        let mut buf = String::new();
        assert!(write!(buf, "{}", DisplayValue(&nested)).is_err());
    }
}
