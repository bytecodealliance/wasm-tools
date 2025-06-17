//! Crate to transform [`wast::Wast`] into a [`Wast`] which can be serialize
//! out to disk.
//!
//! Implementation of the `wasm-tools json-from-wast` subcommand.

use anyhow::Result;
use serde_derive::{Deserialize, Serialize};
use std::borrow::Cow;

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
    pub fn from_ast(
        source_filename: &'a str,
        source_contents: &'a str,
        ast: wast::Wast<'a>,
    ) -> Result<Wast<'a>> {
        build::run(source_filename, source_contents, ast)
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
    #[serde(borrow)]
    pub module_type: Cow<'a, str>,
    #[serde(borrow, skip_serializing_if = "Option::is_none")]
    pub binary_filename: Option<Cow<'a, str>>,
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

#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Const<'a> {
    I32 {
        #[serde(borrow)]
        value: Cow<'a, str>,
    },
    I64 {
        #[serde(borrow)]
        value: Cow<'a, str>,
    },
    F32 {
        #[serde(borrow)]
        value: Cow<'a, str>,
    },
    F64 {
        #[serde(borrow)]
        value: Cow<'a, str>,
    },
    FuncRef {
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        value: Option<Cow<'a, str>>,
    },
    ExternRef {
        #[serde(borrow)]
        value: Cow<'a, str>,
    },
    AnyRef {
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        value: Option<Cow<'a, str>>,
    },
    V128 {
        #[serde(borrow)]
        lane_type: Cow<'a, str>,
        #[serde(borrow)]
        value: Vec<Cow<'a, str>>,
    },
    Either {
        #[serde(borrow)]
        values: Vec<Const<'a>>,
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
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        value: Option<Cow<'a, str>>,
    },

    ContRef {
        #[serde(borrow, skip_serializing_if = "Option::is_none")]
        value: Option<Cow<'a, str>>,
    },

    NullContRef,

    // any null reference, type doesn't matter
    RefNull,
}
