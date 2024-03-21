use anyhow::{bail, Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};
use wast::core::{HeapType, NanPattern, V128Const, V128Pattern, WastRetCore};
use wast::lexer::Lexer;
use wast::parser::{self, ParseBuffer};
use wast::token::{Span, F32, F64};
use wast::{
    QuoteWat, QuoteWatTest, Wast, WastArg, WastDirective, WastExecute, WastInvoke, WastRet, Wat,
};

/// Convert a `*.wast` WebAssembly spec test into a `*.json` file and `*.wasm`
/// files.
///
/// This subcommand will parse a wasm spec test, in the `*.wast` format, and
/// then emit both a JSON file and a number of auxiliary modules. This can
/// perform all the parsing for various implementations of the wasm text format
/// while deferring the actual processing of all commands.
///
/// This subcommand is modelled after WABT's `wast2json` tool, found at
/// <https://github.com/WebAssembly/wabt>. More documentation of the format can
/// be found at
/// <https://github.com/WebAssembly/wabt/blob/main/docs/wast2json.md>.
///
/// Note that this subcommand does not output precisely the same output as
/// `wast2json`, but there should be no major discrepancies.
///
/// This will also emit a number of `*.wasm` and `*.wat` files in the current
/// directory (or in `--wasm-dir`) which the JSON will reference. This command
/// will print the JSON to stdout unless the `-o` flag is given.
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    #[clap(flatten)]
    output: wasm_tools::OutputArg,

    /// Where to place binary and text WebAssembly files referenced by tests.
    ///
    /// Defaults to the current directory.
    #[clap(long)]
    wasm_dir: Option<PathBuf>,

    /// Input `*.wast` file that will be parsed and converted to JSON.
    wast: String,

    /// Output pretty-printed JSON instead of compact json.
    #[clap(long)]
    pretty: bool,

    /// Controls the "allow confusing unicode" option which will reject parsing
    /// files that have unusual characters.
    ///
    /// This is defaulted to `true` to enable parsing all upstream spec tests
    /// but can be disabled if desired too.
    #[clap(long)]
    allow_confusing_unicode: Option<bool>,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    pub fn run(&self) -> Result<()> {
        let contents = std::fs::read_to_string(&self.wast)
            .with_context(|| format!("failed to read input wast file: {:?}", self.wast))?;
        let mut lexer = Lexer::new(&contents);
        lexer.allow_confusing_unicode(self.allow_confusing_unicode.unwrap_or(true));
        let adjust_error = |mut err: wast::Error| {
            err.set_path(self.wast.as_ref());
            err.set_text(&contents);
            err
        };
        let buf = ParseBuffer::new_with_lexer(lexer).map_err(&adjust_error)?;
        let Wast { directives } = parser::parse::<Wast>(&buf).map_err(&adjust_error)?;

        let mut builder = JsonBuilder {
            files: 0,
            contents: &contents,
            ret: json::Wast {
                source_filename: &self.wast,
                commands: Vec::new(),
            },
            opts: self,
            line_end_offsets: Vec::new(),
        };

        // Build a lookup table from span offset to which line it's in since
        // `Span::linecol_in` otherwise has to re-count bytes every time which
        // can be quite slow.
        let base = contents.as_ptr() as usize;
        for line in builder.contents.lines() {
            builder.line_end_offsets.push(line.as_ptr() as usize - base);
        }
        builder.line_end_offsets.push(contents.len());

        for directive in directives {
            let span = directive.span();
            let command = builder.directive(directive).with_context(|| {
                format!(
                    "failure processing directive on line {}",
                    builder.lineno(span)
                )
            })?;
            builder.ret.commands.push(command);
        }

        let json = if self.pretty {
            serde_json::to_string_pretty(&builder.ret)?
        } else {
            serde_json::to_string(&builder.ret)?
        };
        self.output.output(wasm_tools::Output::Json(&json))?;
        Ok(())
    }
}

struct JsonBuilder<'a> {
    files: u32,
    ret: json::Wast<'a>,
    opts: &'a Opts,
    contents: &'a str,
    line_end_offsets: Vec<usize>,
}

impl<'a> JsonBuilder<'a> {
    fn directive(&mut self, directive: WastDirective<'a>) -> Result<json::Command<'a>> {
        let line = self.lineno(directive.span());
        let command = match directive {
            WastDirective::Wat(module) => {
                let (name, _module_type, filename) = self.emit_file(module)?;
                json::Command::Module {
                    line,
                    name: name.map(|s| self.module_name(s)),
                    filename,
                }
            }
            WastDirective::AssertMalformed {
                span: _,
                module,
                message,
            } => {
                let line = self.lineno(module.span());
                let (_name, module_type, filename) = self.emit_file(module)?;
                json::Command::AssertMalformed {
                    line,
                    filename,
                    text: message,
                    module_type,
                }
            }
            WastDirective::AssertInvalid {
                span: _,
                module,
                message,
            } => {
                let line = self.lineno(module.span());
                let (_name, module_type, filename) = self.emit_file(module)?;
                json::Command::AssertInvalid {
                    line,
                    filename,
                    text: message,
                    module_type,
                }
            }
            WastDirective::Register {
                span: _,
                module,
                name,
            } => json::Command::Register {
                line,
                as_: name,
                name: module.map(|i| self.module_name(i.name())),
            },
            WastDirective::Invoke(i) => json::Command::Action {
                line,
                action: self.invoke(i)?,
            },
            WastDirective::AssertTrap {
                span: _,
                exec: WastExecute::Wat(module),
                message,
            } => {
                let line = self.lineno(module.span());
                let (_name, module_type, filename) = self.emit_file(QuoteWat::Wat(module))?;
                json::Command::AssertUninstantiable {
                    line,
                    filename,
                    text: message,
                    module_type,
                }
            }
            WastDirective::AssertTrap {
                span: _,
                exec,
                message,
            } => {
                let line = self.lineno(exec.span());
                json::Command::AssertTrap {
                    line,
                    action: self.action(exec)?,
                    text: message,
                }
            }
            WastDirective::AssertReturn {
                span: _,
                exec,
                results,
            } => json::Command::AssertReturn {
                line: self.lineno(exec.span()),
                action: self.action(exec)?,
                expected: self.expected(results)?,
            },
            WastDirective::AssertExhaustion {
                span: _,
                call,
                message,
            } => json::Command::AssertExhaustion {
                line,
                action: self.invoke(call)?,
                text: message,
            },
            WastDirective::AssertUnlinkable {
                span: _,
                module,
                message,
            } => {
                let line = self.lineno(module.span());
                let (_name, module_type, filename) = self.emit_file(QuoteWat::Wat(module))?;
                json::Command::AssertUnlinkable {
                    line,
                    text: message,
                    module_type,
                    filename,
                }
            }
            WastDirective::AssertException { span: _, exec } => json::Command::AssertException {
                line,
                action: self.action(exec)?,
            },
            WastDirective::Thread(thread) => json::Command::Thread {
                line,
                name: thread.name.name(),
                shared_module: thread.shared_module.map(|i| i.name()),
                commands: thread
                    .directives
                    .into_iter()
                    .map(|i| self.directive(i))
                    .collect::<Result<_>>()?,
            },
            WastDirective::Wait { span: _, thread } => json::Command::Wait {
                line,
                thread: thread.name(),
            },
        };
        Ok(command)
    }

    fn lineno(&self, span: Span) -> u32 {
        match self.line_end_offsets.binary_search(&span.offset()) {
            Ok(i) => (i + 1) as u32,
            Err(i) => i as u32,
        }
    }

    fn module_name(&self, s: &str) -> String {
        s.to_string()
    }

    fn emit_file(
        &mut self,
        mut module: QuoteWat<'a>,
    ) -> Result<(Option<&'a str>, &'a str, String)> {
        let name = match &module {
            QuoteWat::Wat(Wat::Module(m)) => m.id,
            QuoteWat::Wat(Wat::Component(m)) => m.id,
            QuoteWat::QuoteModule(..) | QuoteWat::QuoteComponent(..) => None,
        };
        let name = name.map(|i| i.name());
        let (contents, module_type, ext) = match module.to_test()? {
            QuoteWatTest::Text(s) => (s, "text", "wat"),
            QuoteWatTest::Binary(s) => (s, "binary", "wasm"),
        };
        let stem = Path::new(&self.opts.wast)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap();
        let fileno = self.files;
        self.files += 1;
        let filename = format!("{stem}.{fileno}.{ext}");
        let dst = match &self.opts.wasm_dir {
            Some(dir) => dir.join(&filename),
            None => filename.clone().into(),
        };
        std::fs::write(&dst, &contents).with_context(|| format!("failed to write file {dst:?}"))?;
        Ok((name, module_type, filename))
    }

    fn action(&self, exec: WastExecute<'a>) -> Result<json::Action<'a>> {
        match exec {
            WastExecute::Invoke(invoke) => self.invoke(invoke),
            WastExecute::Wat(_) => {
                bail!("unsupported `action` found using a module");
            }
            WastExecute::Get {
                span: _,
                module,
                global,
            } => Ok(json::Action::Get {
                module: module.map(|m| self.module_name(m.name())),
                field: global,
            }),
        }
    }

    fn invoke(&self, invoke: WastInvoke<'a>) -> Result<json::Action<'a>> {
        let args = self.args(invoke.args)?;
        Ok(json::Action::Invoke {
            module: invoke.module.map(|m| self.module_name(m.name())),
            field: invoke.name,
            args,
        })
    }

    fn args(&self, args: Vec<WastArg<'a>>) -> Result<Vec<json::Const<'a>>> {
        use wast::core::WastArgCore::*;

        let mut ret = Vec::new();
        for arg in args {
            let arg = match arg {
                WastArg::Core(core) => core,
                WastArg::Component(_) => bail!("component support not implemented yet"),
            };
            let val = match arg {
                I32(i) => json::Const::I32 {
                    value: self.print_i32(i),
                },
                I64(i) => json::Const::I64 {
                    value: self.print_i64(i),
                },
                F32(i) => json::Const::F32 {
                    value: f32_to_string(i),
                },
                F64(i) => json::Const::F64 {
                    value: f64_to_string(i),
                },
                V128(V128Const::I8x16(vals)) => json::Const::V128 {
                    lane_type: "i8",
                    value: vals.iter().map(|i| self.print_i8(*i)).collect(),
                },
                V128(V128Const::I16x8(vals)) => json::Const::V128 {
                    lane_type: "i16",
                    value: vals.iter().map(|i| self.print_i16(*i)).collect(),
                },
                V128(V128Const::I32x4(vals)) => json::Const::V128 {
                    lane_type: "i32",
                    value: vals.iter().map(|i| self.print_i32(*i)).collect(),
                },
                V128(V128Const::I64x2(vals)) => json::Const::V128 {
                    lane_type: "i64",
                    value: vals.iter().map(|i| self.print_i64(*i)).collect(),
                },
                V128(V128Const::F32x4(vals)) => json::Const::V128 {
                    lane_type: "f32",
                    value: vals.iter().map(|i| f32_to_string(*i)).collect(),
                },
                V128(V128Const::F64x2(vals)) => json::Const::V128 {
                    lane_type: "f64",
                    value: vals.iter().map(|i| f64_to_string(*i)).collect(),
                },
                RefNull(ty) => null_heap_ty(ty)?,
                RefExtern(i) => json::Const::ExternRef {
                    value: i.to_string(),
                },
                RefHost(i) => json::Const::AnyRef {
                    value: Some(i.to_string()),
                },
            };
            ret.push(val);
        }
        Ok(ret)
    }

    fn expected(&self, rets: Vec<WastRet<'a>>) -> Result<Vec<json::Const<'a>>> {
        let mut ret = Vec::new();
        for r in rets {
            let r = match r {
                WastRet::Core(core) => self.core_ret(core)?,
                WastRet::Component(_) => bail!("component support not implemented yet"),
            };
            ret.push(r);
        }
        Ok(ret)
    }

    fn core_ret(&self, ret: WastRetCore<'a>) -> Result<json::Const<'a>> {
        use wast::core::WastRetCore::*;

        Ok(match ret {
            I32(i) => json::Const::I32 {
                value: self.print_i32(i),
            },
            I64(i) => json::Const::I64 {
                value: self.print_i64(i),
            },
            F32(i) => json::Const::F32 {
                value: nan_pattern_to_string(i, f32_to_string),
            },
            F64(i) => json::Const::F64 {
                value: nan_pattern_to_string(i, f64_to_string),
            },
            V128(V128Pattern::I8x16(vals)) => json::Const::V128 {
                lane_type: "i8",
                value: vals.iter().map(|i| self.print_i8(*i)).collect(),
            },
            V128(V128Pattern::I16x8(vals)) => json::Const::V128 {
                lane_type: "i16",
                value: vals.iter().map(|i| self.print_i16(*i)).collect(),
            },
            V128(V128Pattern::I32x4(vals)) => json::Const::V128 {
                lane_type: "i32",
                value: vals.iter().map(|i| self.print_i32(*i)).collect(),
            },
            V128(V128Pattern::I64x2(vals)) => json::Const::V128 {
                lane_type: "i64",
                value: vals.iter().map(|i| self.print_i64(*i)).collect(),
            },
            V128(V128Pattern::F32x4(vals)) => json::Const::V128 {
                lane_type: "f32",
                value: vals
                    .iter()
                    .map(|i| nan_pattern_to_string(*i, f32_to_string))
                    .collect(),
            },
            V128(V128Pattern::F64x2(vals)) => json::Const::V128 {
                lane_type: "f64",
                value: vals
                    .iter()
                    .map(|i| nan_pattern_to_string(*i, f64_to_string))
                    .collect(),
            },

            RefNull(Some(ty)) => null_heap_ty(ty)?,
            RefNull(None) => json::Const::RefNull,
            RefExtern(None) => json::Const::ExternRef {
                value: "null".to_string(),
            },
            RefExtern(Some(i)) => json::Const::ExternRef {
                value: i.to_string(),
            },
            RefHost(i) => json::Const::AnyRef {
                value: Some(i.to_string()),
            },
            RefFunc(None) => json::Const::FuncRef { value: None },
            RefFunc(Some(_)) => bail!("TODO"),
            RefAny => json::Const::AnyRef { value: None },
            RefEq => json::Const::EqRef,
            RefArray => json::Const::ArrayRef,
            RefStruct => json::Const::StructRef,
            RefI31 => json::Const::I31Ref,
            Either(either) => json::Const::Either {
                values: either
                    .into_iter()
                    .map(|i| self.core_ret(i))
                    .collect::<Result<_>>()?,
            },
        })
    }

    fn print_i8(&self, i: i8) -> String {
        i.to_string()
    }

    fn print_i16(&self, i: i16) -> String {
        i.to_string()
    }

    fn print_i32(&self, i: i32) -> String {
        i.to_string()
    }

    fn print_i64(&self, i: i64) -> String {
        i.to_string()
    }
}

fn nan_pattern_to_string<T>(pat: NanPattern<T>, to_string: fn(T) -> String) -> String {
    match pat {
        NanPattern::ArithmeticNan => "nan:arithmetic".to_string(),
        NanPattern::CanonicalNan => "nan:canonical".to_string(),
        NanPattern::Value(val) => to_string(val),
    }
}

fn f32_to_string(f: F32) -> String {
    f.bits.to_string()
}

fn f64_to_string(f: F64) -> String {
    f.bits.to_string()
}

fn null_heap_ty(ty: HeapType<'_>) -> Result<json::Const> {
    Ok(match ty {
        HeapType::Func => json::Const::FuncRef {
            value: Some("null".to_string()),
        },
        HeapType::Extern => json::Const::ExternRef {
            value: "null".to_string(),
        },
        HeapType::Any => json::Const::AnyRef {
            value: Some("null".to_string()),
        },
        HeapType::None => json::Const::NullRef,
        HeapType::NoFunc => json::Const::NullFuncRef,
        HeapType::NoExtern => json::Const::NullExternRef,
        HeapType::Exn => json::Const::ExnRef {
            value: Some("null".to_string()),
        },
        _ => bail!("unsupported heap type found in `ref.null`"),
    })
}

mod json {
    use serde_derive::Serialize;

    #[derive(Serialize)]
    pub struct Wast<'a> {
        pub source_filename: &'a str,
        pub commands: Vec<Command<'a>>,
    }

    #[derive(Serialize)]
    #[serde(tag = "type", rename_all = "snake_case")]
    pub enum Command<'a> {
        Module {
            line: u32,
            #[serde(skip_serializing_if = "Option::is_none")]
            name: Option<String>,
            filename: String,
        },
        AssertMalformed {
            line: u32,
            filename: String,
            text: &'a str,
            module_type: &'a str,
        },
        AssertInvalid {
            line: u32,
            filename: String,
            text: &'a str,
            module_type: &'a str,
        },
        Register {
            line: u32,
            #[serde(skip_serializing_if = "Option::is_none")]
            name: Option<String>,
            #[serde(rename = "as")]
            as_: &'a str,
        },
        AssertUnlinkable {
            line: u32,
            filename: String,
            text: &'a str,
            module_type: &'a str,
        },
        AssertReturn {
            line: u32,
            action: Action<'a>,
            expected: Vec<Const<'a>>,
        },
        Action {
            line: u32,
            action: Action<'a>,
        },
        AssertTrap {
            line: u32,
            action: Action<'a>,
            text: &'a str,
        },
        AssertExhaustion {
            line: u32,
            action: Action<'a>,
            text: &'a str,
        },
        AssertException {
            line: u32,
            action: Action<'a>,
        },
        AssertUninstantiable {
            line: u32,
            filename: String,
            text: &'a str,
            module_type: &'a str,
        },
        Thread {
            line: u32,
            name: &'a str,
            #[serde(skip_serializing_if = "Option::is_none")]
            shared_module: Option<&'a str>,
            commands: Vec<Command<'a>>,
        },
        Wait {
            line: u32,
            thread: &'a str,
        },
    }

    #[derive(Serialize)]
    #[serde(tag = "type", rename_all = "snake_case")]
    pub enum Action<'a> {
        Invoke {
            #[serde(skip_serializing_if = "Option::is_none")]
            module: Option<String>,
            field: &'a str,
            args: Vec<Const<'a>>,
        },
        Get {
            #[serde(skip_serializing_if = "Option::is_none")]
            module: Option<String>,
            field: &'a str,
        },
    }

    #[derive(Serialize)]
    #[serde(tag = "type", rename_all = "lowercase")]
    pub enum Const<'a> {
        I32 {
            value: String,
        },
        I64 {
            value: String,
        },
        F32 {
            value: String,
        },
        F64 {
            value: String,
        },
        FuncRef {
            #[serde(skip_serializing_if = "Option::is_none")]
            value: Option<String>,
        },
        ExternRef {
            value: String,
        },
        AnyRef {
            #[serde(skip_serializing_if = "Option::is_none")]
            value: Option<String>,
        },
        V128 {
            lane_type: &'a str,
            value: Vec<String>,
        },
        Either {
            values: Vec<Const<'a>>,
        },
        EqRef,
        ArrayRef,
        StructRef,
        I31Ref,
        // (ref.null none)
        NullRef,
        // (ref.null nofunc)
        NullFuncRef,
        // (ref.null noextern)
        NullExternRef,

        ExnRef {
            #[serde(skip_serializing_if = "Option::is_none")]
            value: Option<String>,
        },

        // any null reference, type doesn't matter
        RefNull,
    }
}
