//! Crate to transform [`wast::Wast`] into a [`Wast`] which can be serialize
//! out to disk.
//!
//! Implementation of the `wasm-tools json-from-wast` subcommand.

use anyhow::Result;
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Error};
use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;
use wast::core::NanPattern;

mod build;

/// Top level `*.wast` structure.
#[derive(Serialize, Deserialize)]
pub struct Wast<'a> {
    #[serde(borrow)]
    pub source_filename: Cow<'a, str>,
    #[serde(borrow)]
    pub commands: Vec<Command<'a>>,

    /// This field is only present when created via `Wast::from_ast` and
    /// contains the collection of wasm modules, and their names, that should
    /// be emitted.
    #[serde(skip)]
    pub wasms: Vec<(String, Vec<u8>)>,
}

impl<'a> Wast<'a> {
    /// Parses the [`wast::Wast`] into [`Wast`] of this crate, serializing all
    /// modules for example to binary.
    ///
    /// The `source_filename` is used in the returned `Wast` object and
    /// `source_contents` is the original file used for line numbers.
    pub fn from_ast(
        source_filename: &'a str,
        source_contents: &'a str,
        ast: wast::Wast<'a>,
    ) -> Result<Wast<'a>> {
        Opts::default().convert(source_filename, source_contents, ast)
    }
}

#[derive(Default)]
pub struct Opts {
    dwarf: bool,
}

impl Opts {
    /// Whether or not DWARF debugging information is inserted into encoded
    /// modules.
    pub fn dwarf(&mut self, enable: bool) -> &mut Self {
        self.dwarf = enable;
        self
    }

    /// Converts `ast` to [`Wast`].
    pub fn convert<'a>(
        &self,
        source_filename: &'a str,
        source_contents: &'a str,
        ast: wast::Wast<'a>,
    ) -> Result<Wast<'a>> {
        build::run(self, source_filename, source_contents, ast)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Command<'a> {
    Module {
        line: u32,
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        name: Option<Cow<'a, str>>,
        #[serde(borrow, flatten)]
        file: WasmFile<'a>,
    },
    ModuleDefinition {
        line: u32,
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        name: Option<Cow<'a, str>>,
        #[serde(borrow, flatten)]
        file: WasmFile<'a>,
    },
    ModuleInstance {
        line: u32,
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        instance: Option<Cow<'a, str>>,
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        module: Option<Cow<'a, str>>,
    },
    AssertMalformed {
        line: u32,
        #[serde(borrow, flatten)]
        file: WasmFile<'a>,
        #[serde(borrow)]
        text: Cow<'a, str>,
    },
    AssertInvalid {
        line: u32,
        #[serde(borrow, flatten)]
        file: WasmFile<'a>,
        #[serde(borrow)]
        text: Cow<'a, str>,
    },
    Register {
        line: u32,
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        name: Option<Cow<'a, str>>,
        #[serde(borrow, rename = "as")]
        as_: Cow<'a, str>,
    },
    AssertUnlinkable {
        line: u32,
        #[serde(borrow, flatten)]
        file: WasmFile<'a>,
        #[serde(borrow)]
        text: Cow<'a, str>,
    },
    AssertReturn {
        line: u32,
        #[serde(borrow)]
        action: Action<'a>,
        #[serde(borrow)]
        expected: Vec<Const<'a>>,
    },
    Action {
        line: u32,
        #[serde(borrow)]
        action: Action<'a>,
    },
    AssertTrap {
        line: u32,
        #[serde(borrow)]
        action: Action<'a>,
        #[serde(borrow)]
        text: Cow<'a, str>,
    },
    AssertExhaustion {
        line: u32,
        #[serde(borrow)]
        action: Action<'a>,
        #[serde(borrow)]
        text: Cow<'a, str>,
    },
    AssertException {
        line: u32,
        #[serde(borrow)]
        action: Action<'a>,
    },
    AssertSuspension {
        line: u32,
        #[serde(borrow)]
        action: Action<'a>,
        #[serde(borrow)]
        text: Cow<'a, str>,
    },
    AssertUninstantiable {
        line: u32,
        #[serde(borrow, flatten)]
        file: WasmFile<'a>,
        #[serde(borrow)]
        text: Cow<'a, str>,
    },
    Thread {
        line: u32,
        #[serde(borrow)]
        name: Cow<'a, str>,
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        shared_module: Option<Cow<'a, str>>,
        #[serde(borrow)]
        commands: Vec<Command<'a>>,
    },
    Wait {
        line: u32,
        #[serde(borrow)]
        thread: Cow<'a, str>,
    },
}

#[derive(Serialize, Deserialize)]
pub struct WasmFile<'a> {
    #[serde(borrow)]
    pub filename: Cow<'a, str>,
    pub module_type: WasmFileType,
    #[serde(borrow, skip_serializing_if = "Option::is_none")]
    pub binary_filename: Option<Cow<'a, str>>,
}

impl Command<'_> {
    pub fn line(&self) -> u32 {
        use Command::*;

        match self {
            Module { line, .. }
            | ModuleDefinition { line, .. }
            | ModuleInstance { line, .. }
            | AssertMalformed { line, .. }
            | AssertInvalid { line, .. }
            | Register { line, .. }
            | AssertUnlinkable { line, .. }
            | AssertUninstantiable { line, .. }
            | AssertReturn { line, .. }
            | Action { line, .. }
            | AssertTrap { line, .. }
            | AssertExhaustion { line, .. }
            | AssertException { line, .. }
            | AssertSuspension { line, .. }
            | Thread { line, .. }
            | Wait { line, .. } => *line,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum WasmFileType {
    Text,
    Binary,
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Action<'a> {
    Invoke {
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        module: Option<Cow<'a, str>>,
        #[serde(borrow)]
        field: Cow<'a, str>,
        #[serde(borrow)]
        args: Vec<Const<'a>>,
    },
    Get {
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        module: Option<Cow<'a, str>>,
        #[serde(borrow)]
        field: Cow<'a, str>,
    },
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum Const<'a> {
    Core(CoreConst),
    #[serde(borrow)]
    Component(ComponentConst<'a>),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum CoreConst {
    I32 {
        value: IntString<i32>,
    },
    I64 {
        value: IntString<i64>,
    },
    F32 {
        value: FloatConst<f32>,
    },
    F64 {
        value: FloatConst<f64>,
    },
    FuncRef {
        #[serde(skip_serializing_if = "Option::is_none")]
        value: Option<FuncRef>,
    },
    ExternRef {
        #[serde(skip_serializing_if = "Option::is_none")]
        value: Option<ExternRef>,
    },
    AnyRef {
        #[serde(skip_serializing_if = "Option::is_none")]
        value: Option<AnyRef>,
    },
    V128(V128),
    Either {
        values: Vec<CoreConst>,
    },
    EqRef,
    ArrayRef,
    StructRef,
    I31Ref,
    I31RefShared,
    // (ref.null none)
    NullRef,
    // (ref.null nofunc)
    NullFuncRef,
    // (ref.null noextern)
    NullExternRef,
    // (ref.null noexn)
    NullExnRef,

    ExnRef {
        #[serde(skip_serializing_if = "Option::is_none")]
        value: Option<ExnRef>,
    },

    ContRef {
        #[serde(skip_serializing_if = "Option::is_none")]
        value: Option<ContRef>,
    },

    NullContRef,

    // any null reference, type doesn't matter
    RefNull,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum ExternRef {
    Null,
    #[serde(untagged)]
    Host(IntString<u32>),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum AnyRef {
    Null,
    #[serde(untagged)]
    Host(IntString<u32>),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum FuncRef {
    Null,
    Index(u32),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum ExnRef {
    Null,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum ContRef {
    Null,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "lane_type", rename_all = "lowercase")]
pub enum V128 {
    I8 { value: [IntString<i8>; 16] },
    I16 { value: [IntString<i16>; 8] },
    I32 { value: [IntString<i32>; 4] },
    I64 { value: [IntString<i64>; 2] },
    F32 { value: [FloatConst<f32>; 4] },
    F64 { value: [FloatConst<f64>; 2] },
}

impl V128 {
    /// Returns the value of this constant as a 128-bit value.
    pub fn to_u128(&self) -> u128 {
        let mut bytes = [0; 16];
        match self {
            V128::I8 { value } => {
                let value = value.map(|i| i.0.to_le_bytes());
                for (i, slot) in value.iter().flatten().zip(&mut bytes) {
                    *slot = *i;
                }
            }
            V128::I16 { value } => {
                let value = value.map(|i| i.0.to_le_bytes());
                for (i, slot) in value.iter().flatten().zip(&mut bytes) {
                    *slot = *i;
                }
            }
            V128::I32 { value } => {
                let value = value.map(|i| i.0.to_le_bytes());
                for (i, slot) in value.iter().flatten().zip(&mut bytes) {
                    *slot = *i;
                }
            }
            V128::I64 { value } => {
                let value = value.map(|i| i.0.to_le_bytes());
                for (i, slot) in value.iter().flatten().zip(&mut bytes) {
                    *slot = *i;
                }
            }
            V128::F32 { value } => {
                let value = value.map(|i| i.to_bits().to_le_bytes());
                for (i, slot) in value.iter().flatten().zip(&mut bytes) {
                    *slot = *i;
                }
            }
            V128::F64 { value } => {
                let value = value.map(|i| i.to_bits().to_le_bytes());
                for (i, slot) in value.iter().flatten().zip(&mut bytes) {
                    *slot = *i;
                }
            }
        }

        u128::from_le_bytes(bytes)
    }
}

/// A small wrapper around `T` which implements serialization with
/// `T::to_string` and `T::parse`.
#[derive(Copy, Clone)]
pub struct IntString<T>(pub T);

impl<'de, T: FromStr> Deserialize<'de> for IntString<T>
where
    T::Err: fmt::Display,
{
    fn deserialize<D>(d: D) -> Result<IntString<T>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Cow<'de, str> = Cow::deserialize(d)?;
        Ok(IntString(s.parse().map_err(D::Error::custom)?))
    }
}

impl<T: fmt::Display> Serialize for IntString<T> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.to_string().serialize(s)
    }
}

impl<T: fmt::Debug> fmt::Debug for IntString<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A small wrapper around a float which serializes/deserializes from a string.
#[derive(Copy, Clone)]
pub enum FloatConst<F> {
    ArithmeticNan,
    CanonicalNan,
    Value(F),
}

impl<F: Float> FloatConst<F> {
    pub fn to_bits(&self) -> F::Bits {
        match self {
            Self::ArithmeticNan => F::ARITHMETIC_NAN,
            Self::CanonicalNan => F::CANONICAL_NAN,
            Self::Value(f) => f.to_bits(),
        }
    }
}

impl<'de, F: Float> Deserialize<'de> for FloatConst<F>
where
    <F::Bits as FromStr>::Err: fmt::Display,
{
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Cow<'de, str> = Cow::deserialize(d)?;
        let s: &str = &s;
        Ok(match s {
            "nan:arithmetic" => FloatConst::ArithmeticNan,
            "nan:canonical" => FloatConst::CanonicalNan,
            other => {
                let bits = other.parse().map_err(D::Error::custom)?;
                FloatConst::Value(F::from_bits(bits))
            }
        })
    }
}

impl<F: Float> Serialize for FloatConst<F> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            FloatConst::ArithmeticNan => s.serialize_str("nan:arithmetic"),
            FloatConst::CanonicalNan => s.serialize_str("nan:canonical"),
            FloatConst::Value(other) => s.serialize_str(&other.to_bits().to_string()),
        }
    }
}

impl<F: fmt::Debug> fmt::Debug for FloatConst<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatConst::ArithmeticNan => f.write_str("nan:arithmetic"),
            FloatConst::CanonicalNan => f.write_str("nan:canonical"),
            FloatConst::Value(other) => other.fmt(f),
        }
    }
}

pub trait Float: Sized {
    type Bits: fmt::Display + FromStr;
    const ARITHMETIC_NAN: Self::Bits;
    const CANONICAL_NAN: Self::Bits;
    fn from_bits(bits: Self::Bits) -> Self;
    fn to_bits(&self) -> Self::Bits;
}

impl Float for f32 {
    type Bits = u32;

    const ARITHMETIC_NAN: u32 = 0x7f80_0000;
    const CANONICAL_NAN: u32 = 0x7fc0_0000;

    fn from_bits(bits: u32) -> Self {
        f32::from_bits(bits)
    }

    fn to_bits(&self) -> u32 {
        f32::to_bits(*self)
    }
}

impl Float for f64 {
    type Bits = u64;

    const ARITHMETIC_NAN: u64 = 0x7ff0_0000_0000_0000;
    const CANONICAL_NAN: u64 = 0x7ff8_0000_0000_0000;

    fn from_bits(bits: u64) -> Self {
        f64::from_bits(bits)
    }

    fn to_bits(&self) -> u64 {
        f64::to_bits(*self)
    }
}

impl From<wast::token::F32> for FloatConst<f32> {
    fn from(wast: wast::token::F32) -> FloatConst<f32> {
        FloatConst::Value(f32::from_bits(wast.bits))
    }
}

impl From<wast::token::F64> for FloatConst<f64> {
    fn from(wast: wast::token::F64) -> FloatConst<f64> {
        FloatConst::Value(f64::from_bits(wast.bits))
    }
}

impl<T, F> From<NanPattern<T>> for FloatConst<F>
where
    T: Into<FloatConst<F>>,
{
    fn from(wast: NanPattern<T>) -> FloatConst<F> {
        match wast {
            NanPattern::ArithmeticNan => FloatConst::ArithmeticNan,
            NanPattern::CanonicalNan => FloatConst::CanonicalNan,
            NanPattern::Value(v) => v.into(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum ComponentConst<'a> {
    #[serde(borrow)]
    String(Cow<'a, str>),
    Char(char),
    Bool(bool),
    U8(IntString<u8>),
    S8(IntString<i8>),
    U16(IntString<u16>),
    S16(IntString<i16>),
    U32(IntString<u32>),
    S32(IntString<i32>),
    U64(IntString<u64>),
    S64(IntString<i64>),
    F32(IntString<u32>),
    F64(IntString<u64>),
    #[serde(borrow)]
    List(Vec<ComponentConst<'a>>),
    #[serde(borrow)]
    Record(Vec<(Cow<'a, str>, ComponentConst<'a>)>),
    #[serde(borrow)]
    Tuple(Vec<ComponentConst<'a>>),
    Variant {
        #[serde(borrow)]
        case: Cow<'a, str>,
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        payload: Option<Box<ComponentConst<'a>>>,
    },
    #[serde(borrow)]
    Enum(Cow<'a, str>),
    #[serde(borrow)]
    Option(Option<Box<ComponentConst<'a>>>),
    #[serde(borrow)]
    Result(Result<Option<Box<ComponentConst<'a>>>, Option<Box<ComponentConst<'a>>>>),
    #[serde(borrow)]
    Flags(Vec<Cow<'a, str>>),
}
