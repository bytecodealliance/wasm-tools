use anyhow::{Context, Result, bail};
use std::borrow::Cow;
use std::path::Path;
use wast::core::{AbstractHeapType, HeapType, NanPattern, V128Const, V128Pattern, WastRetCore};
use wast::token::{F32, F64, Span};
use wast::{
    QuoteWat, QuoteWatTest, Wast, WastArg, WastDirective, WastExecute, WastInvoke, WastRet,
};

pub fn run<'a>(
    source_filename: &'a str,
    contents: &'a str,
    wast: Wast<'a>,
) -> Result<crate::Wast<'a>> {
    let Wast { directives } = wast;

    let mut builder = JsonBuilder {
        files: 0,
        contents,
        ret: crate::Wast {
            source_filename: source_filename.into(),
            commands: Vec::new(),
            wasms: Vec::new(),
        },
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

    Ok(builder.ret)
}

struct JsonBuilder<'a> {
    files: u32,
    ret: crate::Wast<'a>,
    contents: &'a str,
    line_end_offsets: Vec<usize>,
}

impl<'a> JsonBuilder<'a> {
    fn directive(&mut self, directive: WastDirective<'a>) -> Result<crate::Command<'a>> {
        let line = self.lineno(directive.span());
        let command = match directive {
            WastDirective::Module(module) => {
                let (name, file) = self.emit_file(module, false)?;
                crate::Command::Module {
                    line,
                    name: name.map(|s| self.module_name(s)),
                    file,
                }
            }
            WastDirective::ModuleDefinition(module) => {
                let (name, file) = self.emit_file(module, false)?;
                crate::Command::ModuleDefinition {
                    line,
                    name: name.map(|s| self.module_name(s)),
                    file,
                }
            }
            WastDirective::ModuleInstance {
                instance, module, ..
            } => crate::Command::ModuleInstance {
                line,
                instance: instance.map(|s| self.module_name(s.name())),
                module: module.map(|s| self.module_name(s.name())),
            },
            WastDirective::AssertMalformed {
                span: _,
                module,
                message,
            } => {
                let line = self.lineno(module.span());
                let (_name, file) = self.emit_file(module, true)?;
                crate::Command::AssertMalformed {
                    line,
                    text: message.into(),
                    file,
                }
            }
            WastDirective::AssertInvalid {
                span: _,
                module,
                message,
            } => {
                let line = self.lineno(module.span());
                let (_name, file) = self.emit_file(module, false)?;
                crate::Command::AssertInvalid {
                    line,
                    text: message.into(),
                    file,
                }
            }
            WastDirective::Register {
                span: _,
                module,
                name,
            } => crate::Command::Register {
                line,
                as_: name.into(),
                name: module.map(|i| self.module_name(i.name())),
            },
            WastDirective::Invoke(i) => crate::Command::Action {
                line,
                action: self.invoke(i)?,
            },
            WastDirective::AssertTrap {
                span: _,
                exec: WastExecute::Wat(module),
                message,
            } => {
                let line = self.lineno(module.span());
                let (_name, file) = self.emit_file(QuoteWat::Wat(module), false)?;
                crate::Command::AssertUninstantiable {
                    line,
                    text: message.into(),
                    file,
                }
            }
            WastDirective::AssertTrap {
                span: _,
                exec,
                message,
            } => {
                let line = self.lineno(exec.span());
                crate::Command::AssertTrap {
                    line,
                    action: self.action(exec)?,
                    text: message.into(),
                }
            }
            WastDirective::AssertReturn {
                span: _,
                exec,
                results,
            } => crate::Command::AssertReturn {
                line: self.lineno(exec.span()),
                action: self.action(exec)?,
                expected: self.expected(results)?,
            },
            WastDirective::AssertExhaustion {
                span: _,
                call,
                message,
            } => crate::Command::AssertExhaustion {
                line,
                action: self.invoke(call)?,
                text: message.into(),
            },
            WastDirective::AssertUnlinkable {
                span: _,
                module,
                message,
            } => {
                let line = self.lineno(module.span());
                let (_name, file) = self.emit_file(QuoteWat::Wat(module), false)?;
                crate::Command::AssertUnlinkable {
                    line,
                    text: message.into(),
                    file,
                }
            }
            WastDirective::AssertException { span: _, exec } => crate::Command::AssertException {
                line,
                action: self.action(exec)?,
            },
            WastDirective::AssertSuspension {
                span: _,
                exec,
                message,
            } => crate::Command::AssertSuspension {
                line,
                text: message.into(),
                action: self.action(exec)?,
            },
            WastDirective::Thread(thread) => crate::Command::Thread {
                line,
                name: thread.name.name().into(),
                shared_module: thread.shared_module.map(|i| i.name().into()),
                commands: thread
                    .directives
                    .into_iter()
                    .map(|i| self.directive(i))
                    .collect::<Result<_>>()?,
            },
            WastDirective::Wait { span: _, thread } => crate::Command::Wait {
                line,
                thread: thread.name().into(),
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

    fn module_name(&self, s: &str) -> Cow<'a, str> {
        s.to_string().into()
    }

    fn emit_file(
        &mut self,
        mut module: QuoteWat<'a>,
        malformed: bool,
    ) -> Result<(Option<&'a str>, crate::WasmFile<'static>)> {
        let name = module.name().map(|i| i.name());
        let (contents, module_type, ext) = match module.to_test()? {
            QuoteWatTest::Text(s) => (s, "text", "wat"),
            QuoteWatTest::Binary(s) => (s, "binary", "wasm"),
        };
        let filename: &str = &self.ret.source_filename;
        let stem = Path::new(filename).file_stem().unwrap().to_str().unwrap();
        let fileno = self.files;
        self.files += 1;
        let filename = format!("{stem}.{fileno}.{ext}");
        let binary_filename = format!("{stem}.{fileno}.wasm");
        self.ret.wasms.push((filename.clone(), contents));
        let mut ret = crate::WasmFile {
            module_type: module_type.into(),
            filename: filename.into(),
            binary_filename: None,
        };
        if module_type == "text" && !malformed {
            if let Ok(bytes) = module.encode() {
                self.ret.wasms.push((binary_filename.clone(), bytes));
                ret.binary_filename = Some(binary_filename.into());
            }
        }
        Ok((name, ret))
    }

    fn action(&self, exec: WastExecute<'a>) -> Result<crate::Action<'a>> {
        match exec {
            WastExecute::Invoke(invoke) => self.invoke(invoke),
            WastExecute::Wat(_) => {
                bail!("unsupported `action` found using a module");
            }
            WastExecute::Get {
                span: _,
                module,
                global,
            } => Ok(crate::Action::Get {
                module: module.map(|m| self.module_name(m.name())),
                field: global.into(),
            }),
        }
    }

    fn invoke(&self, invoke: WastInvoke<'a>) -> Result<crate::Action<'a>> {
        let args = self.args(invoke.args)?;
        Ok(crate::Action::Invoke {
            module: invoke.module.map(|m| self.module_name(m.name())),
            field: invoke.name.into(),
            args,
        })
    }

    fn args(&self, args: Vec<WastArg<'a>>) -> Result<Vec<crate::Const<'a>>> {
        use wast::core::WastArgCore::*;

        let mut ret = Vec::new();
        for arg in args {
            let arg = match arg {
                WastArg::Core(core) => core,
                _ => bail!("encountered unsupported Wast argument: {arg:?}"),
            };
            let val = match arg {
                I32(i) => crate::Const::I32 {
                    value: self.print_i32(i).into(),
                },
                I64(i) => crate::Const::I64 {
                    value: self.print_i64(i).into(),
                },
                F32(i) => crate::Const::F32 {
                    value: f32_to_string(i).into(),
                },
                F64(i) => crate::Const::F64 {
                    value: f64_to_string(i).into(),
                },
                V128(V128Const::I8x16(vals)) => crate::Const::V128 {
                    lane_type: "i8".into(),
                    value: vals.iter().map(|i| self.print_i8(*i).into()).collect(),
                },
                V128(V128Const::I16x8(vals)) => crate::Const::V128 {
                    lane_type: "i16".into(),
                    value: vals.iter().map(|i| self.print_i16(*i).into()).collect(),
                },
                V128(V128Const::I32x4(vals)) => crate::Const::V128 {
                    lane_type: "i32".into(),
                    value: vals.iter().map(|i| self.print_i32(*i).into()).collect(),
                },
                V128(V128Const::I64x2(vals)) => crate::Const::V128 {
                    lane_type: "i64".into(),
                    value: vals.iter().map(|i| self.print_i64(*i).into()).collect(),
                },
                V128(V128Const::F32x4(vals)) => crate::Const::V128 {
                    lane_type: "f32".into(),
                    value: vals.iter().map(|i| f32_to_string(*i).into()).collect(),
                },
                V128(V128Const::F64x2(vals)) => crate::Const::V128 {
                    lane_type: "f64".into(),
                    value: vals.iter().map(|i| f64_to_string(*i).into()).collect(),
                },
                RefNull(ty) => null_heap_ty(ty)?,
                RefExtern(i) => crate::Const::ExternRef {
                    value: i.to_string().into(),
                },
                RefHost(i) => crate::Const::AnyRef {
                    value: Some(i.to_string().into()),
                },
            };
            ret.push(val);
        }
        Ok(ret)
    }

    fn expected(&self, rets: Vec<WastRet<'a>>) -> Result<Vec<crate::Const<'a>>> {
        let mut ret = Vec::new();
        for r in rets {
            let r = match r {
                WastRet::Core(core) => self.core_ret(core)?,
                _ => bail!("encountered unsupported Wast result: {r:?}"),
            };
            ret.push(r);
        }
        Ok(ret)
    }

    fn core_ret(&self, ret: WastRetCore<'a>) -> Result<crate::Const<'a>> {
        use wast::core::WastRetCore::*;

        Ok(match ret {
            I32(i) => crate::Const::I32 {
                value: self.print_i32(i).into(),
            },
            I64(i) => crate::Const::I64 {
                value: self.print_i64(i).into(),
            },
            F32(i) => crate::Const::F32 {
                value: nan_pattern_to_string(i, f32_to_string).into(),
            },
            F64(i) => crate::Const::F64 {
                value: nan_pattern_to_string(i, f64_to_string).into(),
            },
            V128(V128Pattern::I8x16(vals)) => crate::Const::V128 {
                lane_type: "i8".into(),
                value: vals.iter().map(|i| self.print_i8(*i).into()).collect(),
            },
            V128(V128Pattern::I16x8(vals)) => crate::Const::V128 {
                lane_type: "i16".into(),
                value: vals.iter().map(|i| self.print_i16(*i).into()).collect(),
            },
            V128(V128Pattern::I32x4(vals)) => crate::Const::V128 {
                lane_type: "i32".into(),
                value: vals.iter().map(|i| self.print_i32(*i).into()).collect(),
            },
            V128(V128Pattern::I64x2(vals)) => crate::Const::V128 {
                lane_type: "i64".into(),
                value: vals.iter().map(|i| self.print_i64(*i).into()).collect(),
            },
            V128(V128Pattern::F32x4(vals)) => crate::Const::V128 {
                lane_type: "f32".into(),
                value: vals
                    .iter()
                    .map(|i| nan_pattern_to_string(*i, f32_to_string).into())
                    .collect(),
            },
            V128(V128Pattern::F64x2(vals)) => crate::Const::V128 {
                lane_type: "f64".into(),
                value: vals
                    .iter()
                    .map(|i| nan_pattern_to_string(*i, f64_to_string).into())
                    .collect(),
            },

            RefNull(Some(ty)) => null_heap_ty(ty)?,
            RefNull(None) => crate::Const::RefNull,
            RefExtern(None) => crate::Const::ExternRef {
                value: "null".to_string().into(),
            },
            RefExtern(Some(i)) => crate::Const::ExternRef {
                value: i.to_string().into(),
            },
            RefHost(i) => crate::Const::AnyRef {
                value: Some(i.to_string().into()),
            },
            RefFunc(None) => crate::Const::FuncRef { value: None },
            RefFunc(Some(_)) => bail!("TODO"),
            RefAny => crate::Const::AnyRef { value: None },
            RefEq => crate::Const::EqRef,
            RefArray => crate::Const::ArrayRef,
            RefStruct => crate::Const::StructRef,
            RefI31 => crate::Const::I31Ref,
            RefI31Shared => crate::Const::I31RefShared,
            Either(either) => crate::Const::Either {
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

fn null_heap_ty(ty: HeapType<'_>) -> Result<crate::Const<'_>> {
    Ok(match ty {
        HeapType::Abstract { shared, ty } => {
            use AbstractHeapType::*;
            if shared {
                bail!("shared abstract types are not supported in `ref.null`")
            }
            match ty {
                Func => crate::Const::FuncRef {
                    value: Some("null".to_string().into()),
                },
                Extern => crate::Const::ExternRef {
                    value: "null".to_string().into(),
                },
                Any => crate::Const::AnyRef {
                    value: Some("null".to_string().into()),
                },
                None => crate::Const::NullRef,
                NoFunc => crate::Const::NullFuncRef,
                NoExtern => crate::Const::NullExternRef,
                Exn => crate::Const::ExnRef {
                    value: Some("null".to_string().into()),
                },
                Eq => crate::Const::EqRef,
                Struct => crate::Const::StructRef,
                Array => crate::Const::ArrayRef,
                I31 => crate::Const::I31Ref,
                NoExn => crate::Const::NullExnRef,
                Cont => crate::Const::ContRef {
                    value: Some("null".to_string().into()),
                },
                NoCont => crate::Const::NullContRef,
            }
        }
        _ => bail!("unsupported heap type found in `ref.null`"),
    })
}
