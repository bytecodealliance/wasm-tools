use crate::{AnyRef, ContRef, ExnRef, ExternRef, FuncRef, IntString, Opts, WasmFileType};
use anyhow::{Context, Result, bail};
use std::borrow::Cow;
use std::path::Path;
use wast::component::WastVal;
use wast::core::{
    AbstractHeapType, EncodeOptions, GenerateDwarf, HeapType, V128Const, V128Pattern, WastArgCore,
    WastRetCore,
};
use wast::token::Span;
use wast::{
    QuoteWat, QuoteWatTest, Wast, WastArg, WastDirective, WastExecute, WastInvoke, WastRet,
};

pub fn run<'a>(
    opts: &Opts,
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
        opts,
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

struct JsonBuilder<'a, 'b> {
    files: u32,
    ret: crate::Wast<'a>,
    contents: &'a str,
    line_end_offsets: Vec<usize>,
    opts: &'b Opts,
}

impl<'a> JsonBuilder<'a, '_> {
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
        let (contents, module_type, ext) = match &mut module {
            QuoteWat::Wat(wat) => {
                let mut opts = EncodeOptions::new();
                if self.opts.dwarf {
                    let filename: &str = &self.ret.source_filename;
                    opts.dwarf(filename.as_ref(), self.contents, GenerateDwarf::Lines);
                }
                let contents = opts.encode_wat(wat)?;
                (contents, WasmFileType::Binary, "wasm")
            }
            other => match other.to_test()? {
                QuoteWatTest::Text(s) => (s, WasmFileType::Text, "wat"),
                QuoteWatTest::Binary(s) => (s, WasmFileType::Binary, "wasm"),
            },
        };
        let filename: &str = &self.ret.source_filename;
        let stem = Path::new(filename).file_stem().unwrap().to_str().unwrap();
        let fileno = self.files;
        self.files += 1;
        let filename = format!("{stem}.{fileno}.{ext}");
        let binary_filename = format!("{stem}.{fileno}.wasm");
        self.ret.wasms.push((filename.clone(), contents));
        let mut ret = crate::WasmFile {
            module_type,
            filename: filename.into(),
            binary_filename: None,
        };
        if module_type == WasmFileType::Text && !malformed {
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
        let mut ret = Vec::new();
        for arg in args {
            let arg = match arg {
                WastArg::Core(core) => crate::Const::Core(self.core_arg(core)?),
                WastArg::Component(arg) => crate::Const::Component(self.component_val(arg)?),
                _ => bail!("unsupported arg {arg:?}"),
            };
            ret.push(arg);
        }
        Ok(ret)
    }

    fn core_arg(&self, arg: WastArgCore<'_>) -> Result<crate::CoreConst> {
        use WastArgCore::*;
        Ok(match arg {
            I32(i) => crate::CoreConst::I32 {
                value: IntString(i),
            },
            I64(i) => crate::CoreConst::I64 {
                value: IntString(i),
            },
            F32(i) => crate::CoreConst::F32 { value: i.into() },
            F64(i) => crate::CoreConst::F64 { value: i.into() },
            V128(V128Const::I8x16(vals)) => crate::CoreConst::V128(crate::V128::I8 {
                value: vals.map(IntString),
            }),
            V128(V128Const::I16x8(vals)) => crate::CoreConst::V128(crate::V128::I16 {
                value: vals.map(IntString),
            }),
            V128(V128Const::I32x4(vals)) => crate::CoreConst::V128(crate::V128::I32 {
                value: vals.map(IntString),
            }),
            V128(V128Const::I64x2(vals)) => crate::CoreConst::V128(crate::V128::I64 {
                value: vals.map(IntString),
            }),
            V128(V128Const::F32x4(vals)) => crate::CoreConst::V128(crate::V128::F32 {
                value: vals.map(|i| i.into()),
            }),
            V128(V128Const::F64x2(vals)) => crate::CoreConst::V128(crate::V128::F64 {
                value: vals.map(|i| i.into()),
            }),
            RefNull(ty) => null_heap_ty(ty)?,
            RefExtern(i) => crate::CoreConst::ExternRef {
                value: Some(ExternRef::Host(IntString(i))),
            },
            RefHost(i) => crate::CoreConst::AnyRef {
                value: Some(AnyRef::Host(IntString(i))),
            },
        })
    }

    fn component_val(&self, arg: WastVal<'a>) -> Result<crate::ComponentConst<'a>> {
        Ok(match arg {
            WastVal::Bool(b) => crate::ComponentConst::Bool(b),
            WastVal::U8(i) => crate::ComponentConst::U8(IntString(i)),
            WastVal::S8(i) => crate::ComponentConst::S8(IntString(i)),
            WastVal::U16(i) => crate::ComponentConst::U16(IntString(i)),
            WastVal::S16(i) => crate::ComponentConst::S16(IntString(i)),
            WastVal::U32(i) => crate::ComponentConst::U32(IntString(i)),
            WastVal::S32(i) => crate::ComponentConst::S32(IntString(i)),
            WastVal::U64(i) => crate::ComponentConst::U64(IntString(i)),
            WastVal::S64(i) => crate::ComponentConst::S64(IntString(i)),
            WastVal::F32(i) => crate::ComponentConst::F32(IntString(i.bits)),
            WastVal::F64(i) => crate::ComponentConst::F64(IntString(i.bits)),
            WastVal::Char(c) => crate::ComponentConst::Char(c),
            WastVal::String(s) => crate::ComponentConst::String(s.into()),
            WastVal::List(s) => crate::ComponentConst::List(
                s.into_iter()
                    .map(|i| self.component_val(i))
                    .collect::<Result<_>>()?,
            ),
            WastVal::Record(s) => crate::ComponentConst::Record(
                s.into_iter()
                    .map(|(name, i)| Ok((name.into(), self.component_val(i)?)))
                    .collect::<Result<_>>()?,
            ),
            WastVal::Tuple(s) => crate::ComponentConst::Tuple(
                s.into_iter()
                    .map(|i| self.component_val(i))
                    .collect::<Result<_>>()?,
            ),
            WastVal::Variant(case, value) => crate::ComponentConst::Variant {
                case: case.into(),
                payload: value
                    .map(|i| self.component_val(*i))
                    .transpose()?
                    .map(Box::new),
            },
            WastVal::Enum(case) => crate::ComponentConst::Enum(case.into()),
            WastVal::Option(val) => crate::ComponentConst::Option(
                val.map(|i| self.component_val(*i))
                    .transpose()?
                    .map(Box::new),
            ),
            WastVal::Result(val) => crate::ComponentConst::Result(match val {
                Ok(val) => Ok(val
                    .map(|i| self.component_val(*i))
                    .transpose()?
                    .map(Box::new)),
                Err(val) => Err(val
                    .map(|i| self.component_val(*i))
                    .transpose()?
                    .map(Box::new)),
            }),
            WastVal::Flags(vals) => {
                crate::ComponentConst::Flags(vals.into_iter().map(|v| v.into()).collect())
            }
        })
    }

    fn expected(&self, rets: Vec<WastRet<'a>>) -> Result<Vec<crate::Const<'a>>> {
        let mut ret = Vec::new();
        for r in rets {
            let r = match r {
                WastRet::Core(core) => crate::Const::Core(self.core_ret(core)?),
                WastRet::Component(val) => crate::Const::Component(self.component_val(val)?),
                _ => bail!("encountered unsupported Wast result: {r:?}"),
            };
            ret.push(r);
        }
        Ok(ret)
    }

    fn core_ret(&self, ret: WastRetCore<'a>) -> Result<crate::CoreConst> {
        use wast::core::WastRetCore::*;

        Ok(match ret {
            I32(i) => crate::CoreConst::I32 {
                value: IntString(i),
            },
            I64(i) => crate::CoreConst::I64 {
                value: IntString(i),
            },
            F32(i) => crate::CoreConst::F32 { value: i.into() },
            F64(i) => crate::CoreConst::F64 { value: i.into() },
            V128(V128Pattern::I8x16(vals)) => crate::CoreConst::V128(crate::V128::I8 {
                value: vals.map(IntString),
            }),
            V128(V128Pattern::I16x8(vals)) => crate::CoreConst::V128(crate::V128::I16 {
                value: vals.map(IntString),
            }),
            V128(V128Pattern::I32x4(vals)) => crate::CoreConst::V128(crate::V128::I32 {
                value: vals.map(IntString),
            }),
            V128(V128Pattern::I64x2(vals)) => crate::CoreConst::V128(crate::V128::I64 {
                value: vals.map(IntString),
            }),
            V128(V128Pattern::F32x4(vals)) => crate::CoreConst::V128(crate::V128::F32 {
                value: vals.map(|i| i.into()),
            }),
            V128(V128Pattern::F64x2(vals)) => crate::CoreConst::V128(crate::V128::F64 {
                value: vals.map(|i| i.into()),
            }),

            RefNull(Some(ty)) => null_heap_ty(ty)?,
            RefNull(None) => crate::CoreConst::RefNull,
            RefExtern(None) => crate::CoreConst::ExternRef { value: None },
            RefExtern(Some(i)) => crate::CoreConst::ExternRef {
                value: Some(ExternRef::Host(IntString(i))),
            },
            RefHost(i) => crate::CoreConst::AnyRef {
                value: Some(AnyRef::Host(IntString(i))),
            },
            RefFunc(None) => crate::CoreConst::FuncRef { value: None },
            RefFunc(Some(i)) => crate::CoreConst::FuncRef {
                value: Some(FuncRef::Index(i.into())),
            },
            RefAny => crate::CoreConst::AnyRef { value: None },
            RefEq => crate::CoreConst::EqRef,
            RefArray => crate::CoreConst::ArrayRef,
            RefStruct => crate::CoreConst::StructRef,
            RefI31 => crate::CoreConst::I31Ref,
            RefI31Shared => crate::CoreConst::I31RefShared,
            Either(either) => crate::CoreConst::Either {
                values: either
                    .into_iter()
                    .map(|i| self.core_ret(i))
                    .collect::<Result<_>>()?,
            },
        })
    }
}

fn null_heap_ty(ty: HeapType<'_>) -> Result<crate::CoreConst> {
    Ok(match ty {
        HeapType::Abstract { shared, ty } => {
            use AbstractHeapType::*;
            if shared {
                bail!("shared abstract types are not supported in `ref.null`")
            }
            match ty {
                Func => crate::CoreConst::FuncRef {
                    value: Some(FuncRef::Null),
                },
                Extern => crate::CoreConst::ExternRef {
                    value: Some(ExternRef::Null),
                },
                Any => crate::CoreConst::AnyRef {
                    value: Some(AnyRef::Null),
                },
                None => crate::CoreConst::NullRef,
                NoFunc => crate::CoreConst::NullFuncRef,
                NoExtern => crate::CoreConst::NullExternRef,
                Exn => crate::CoreConst::ExnRef {
                    value: Some(ExnRef::Null),
                },
                Eq => crate::CoreConst::EqRef,
                Struct => crate::CoreConst::StructRef,
                Array => crate::CoreConst::ArrayRef,
                I31 => crate::CoreConst::I31Ref,
                NoExn => crate::CoreConst::NullExnRef,
                Cont => crate::CoreConst::ContRef {
                    value: Some(ContRef::Null),
                },
                NoCont => crate::CoreConst::NullContRef,
            }
        }
        _ => bail!("unsupported heap type found in `ref.null`"),
    })
}
