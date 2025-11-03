use crate::component::*;
use crate::kw;
use crate::parser::{Cursor, Lookahead1, Parse, Parser, Peek, Result};
use crate::token::{Id, Index, LParen, NameAnnotation, Span};

/// A declared core function.
///
/// This is a member of both the core alias and canon sections.
#[derive(Debug)]
pub struct CoreFunc<'a> {
    /// Where this `core func` was defined.
    pub span: Span,
    /// An identifier that this function is resolved with (optionally) for name
    /// resolution.
    pub id: Option<Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// The kind of core function.
    pub kind: CoreFuncKind<'a>,
}

impl<'a> Parse<'a> for CoreFunc<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::core>()?.0;
        parser.parse::<kw::func>()?;
        let id = parser.parse()?;
        let name = parser.parse()?;
        let kind = parser.parens(|p| p.parse())?;

        Ok(Self {
            span,
            id,
            name,
            kind,
        })
    }
}

/// Represents the kind of core functions.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum CoreFuncKind<'a> {
    /// The core function is defined in terms of lowering a component function.
    ///
    /// The core function is actually a member of the canon section.
    Lower(CanonLower<'a>),
    /// The core function is defined in terms of aliasing a module instance export.
    ///
    /// The core function is actually a member of the core alias section.
    Alias(InlineExportAlias<'a, true>),
    ResourceNew(CanonResourceNew<'a>),
    ResourceDrop(CanonResourceDrop<'a>),
    ResourceRep(CanonResourceRep<'a>),
    ThreadSpawnRef(CanonThreadSpawnRef<'a>),
    ThreadSpawnIndirect(CanonThreadSpawnIndirect<'a>),
    ThreadAvailableParallelism(CanonThreadAvailableParallelism),
    BackpressureSet,
    BackpressureInc,
    BackpressureDec,
    TaskReturn(CanonTaskReturn<'a>),
    TaskCancel,
    ContextGet(u32),
    ContextSet(u32),
    ThreadYield(CanonThreadYield),
    SubtaskDrop,
    SubtaskCancel(CanonSubtaskCancel),
    StreamNew(CanonStreamNew<'a>),
    StreamRead(CanonStreamRead<'a>),
    StreamWrite(CanonStreamWrite<'a>),
    StreamCancelRead(CanonStreamCancelRead<'a>),
    StreamCancelWrite(CanonStreamCancelWrite<'a>),
    StreamDropReadable(CanonStreamDropReadable<'a>),
    StreamDropWritable(CanonStreamDropWritable<'a>),
    FutureNew(CanonFutureNew<'a>),
    FutureRead(CanonFutureRead<'a>),
    FutureWrite(CanonFutureWrite<'a>),
    FutureCancelRead(CanonFutureCancelRead<'a>),
    FutureCancelWrite(CanonFutureCancelWrite<'a>),
    FutureDropReadable(CanonFutureDropReadable<'a>),
    FutureDropWritable(CanonFutureDropWritable<'a>),
    ErrorContextNew(CanonErrorContextNew<'a>),
    ErrorContextDebugMessage(CanonErrorContextDebugMessage<'a>),
    ErrorContextDrop,
    WaitableSetNew,
    WaitableSetWait(CanonWaitableSetWait<'a>),
    WaitableSetPoll(CanonWaitableSetPoll<'a>),
    WaitableSetDrop,
    WaitableJoin,
    ThreadIndex,
    ThreadNewIndirect(CanonThreadNewIndirect<'a>),
    ThreadSwitchTo(CanonThreadSwitchTo),
    ThreadSuspend(CanonThreadSuspend),
    ThreadResumeLater,
    ThreadYieldTo(CanonThreadYieldTo),
}

impl<'a> Parse<'a> for CoreFuncKind<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::canon>()? {
            parser.parse::<kw::canon>()?;
        } else if l.peek::<kw::alias>()? {
            return Ok(Self::Alias(parser.parse()?));
        } else {
            return Err(l.error());
        }

        CoreFuncKind::parse_lookahead(parser.lookahead1())
    }
}

impl<'a> CoreFuncKind<'a> {
    fn parse_lookahead(mut l: Lookahead1<'a>) -> Result<CoreFuncKind<'a>> {
        let parser = l.parser();
        if l.peek::<kw::lower>()? {
            Ok(CoreFuncKind::Lower(parser.parse()?))
        } else if l.peek::<kw::resource_new>()? {
            Ok(CoreFuncKind::ResourceNew(parser.parse()?))
        } else if l.peek::<kw::resource_drop>()? {
            Ok(CoreFuncKind::ResourceDrop(parser.parse()?))
        } else if l.peek::<kw::resource_rep>()? {
            Ok(CoreFuncKind::ResourceRep(parser.parse()?))
        } else if l.peek::<kw::thread_spawn_ref>()? {
            Ok(CoreFuncKind::ThreadSpawnRef(parser.parse()?))
        } else if l.peek::<kw::thread_spawn_indirect>()? {
            Ok(CoreFuncKind::ThreadSpawnIndirect(parser.parse()?))
        } else if l.peek::<kw::thread_available_parallelism>()? {
            Ok(CoreFuncKind::ThreadAvailableParallelism(parser.parse()?))
        } else if l.peek::<kw::backpressure_set>()? {
            parser.parse::<kw::backpressure_set>()?;
            Ok(CoreFuncKind::BackpressureSet)
        } else if l.peek::<kw::backpressure_inc>()? {
            parser.parse::<kw::backpressure_inc>()?;
            Ok(CoreFuncKind::BackpressureInc)
        } else if l.peek::<kw::backpressure_dec>()? {
            parser.parse::<kw::backpressure_dec>()?;
            Ok(CoreFuncKind::BackpressureDec)
        } else if l.peek::<kw::task_return>()? {
            Ok(CoreFuncKind::TaskReturn(parser.parse()?))
        } else if l.peek::<kw::task_cancel>()? {
            parser.parse::<kw::task_cancel>()?;
            Ok(CoreFuncKind::TaskCancel)
        } else if l.peek::<kw::context_get>()? {
            parser.parse::<kw::context_get>()?;
            parser.parse::<kw::i32>()?;
            Ok(CoreFuncKind::ContextGet(parser.parse()?))
        } else if l.peek::<kw::context_set>()? {
            parser.parse::<kw::context_set>()?;
            parser.parse::<kw::i32>()?;
            Ok(CoreFuncKind::ContextSet(parser.parse()?))
        } else if l.peek::<kw::thread_yield>()? {
            Ok(CoreFuncKind::ThreadYield(parser.parse()?))
        } else if l.peek::<kw::subtask_drop>()? {
            parser.parse::<kw::subtask_drop>()?;
            Ok(CoreFuncKind::SubtaskDrop)
        } else if l.peek::<kw::subtask_cancel>()? {
            Ok(CoreFuncKind::SubtaskCancel(parser.parse()?))
        } else if l.peek::<kw::stream_new>()? {
            Ok(CoreFuncKind::StreamNew(parser.parse()?))
        } else if l.peek::<kw::stream_read>()? {
            Ok(CoreFuncKind::StreamRead(parser.parse()?))
        } else if l.peek::<kw::stream_write>()? {
            Ok(CoreFuncKind::StreamWrite(parser.parse()?))
        } else if l.peek::<kw::stream_cancel_read>()? {
            Ok(CoreFuncKind::StreamCancelRead(parser.parse()?))
        } else if l.peek::<kw::stream_cancel_write>()? {
            Ok(CoreFuncKind::StreamCancelWrite(parser.parse()?))
        } else if l.peek::<kw::stream_drop_readable>()? {
            Ok(CoreFuncKind::StreamDropReadable(parser.parse()?))
        } else if l.peek::<kw::stream_drop_writable>()? {
            Ok(CoreFuncKind::StreamDropWritable(parser.parse()?))
        } else if l.peek::<kw::future_new>()? {
            Ok(CoreFuncKind::FutureNew(parser.parse()?))
        } else if l.peek::<kw::future_read>()? {
            Ok(CoreFuncKind::FutureRead(parser.parse()?))
        } else if l.peek::<kw::future_write>()? {
            Ok(CoreFuncKind::FutureWrite(parser.parse()?))
        } else if l.peek::<kw::future_cancel_read>()? {
            Ok(CoreFuncKind::FutureCancelRead(parser.parse()?))
        } else if l.peek::<kw::future_cancel_write>()? {
            Ok(CoreFuncKind::FutureCancelWrite(parser.parse()?))
        } else if l.peek::<kw::future_drop_readable>()? {
            Ok(CoreFuncKind::FutureDropReadable(parser.parse()?))
        } else if l.peek::<kw::future_drop_writable>()? {
            Ok(CoreFuncKind::FutureDropWritable(parser.parse()?))
        } else if l.peek::<kw::error_context_new>()? {
            Ok(CoreFuncKind::ErrorContextNew(parser.parse()?))
        } else if l.peek::<kw::error_context_debug_message>()? {
            Ok(CoreFuncKind::ErrorContextDebugMessage(parser.parse()?))
        } else if l.peek::<kw::error_context_drop>()? {
            parser.parse::<kw::error_context_drop>()?;
            Ok(CoreFuncKind::ErrorContextDrop)
        } else if l.peek::<kw::waitable_set_new>()? {
            parser.parse::<kw::waitable_set_new>()?;
            Ok(CoreFuncKind::WaitableSetNew)
        } else if l.peek::<kw::waitable_set_wait>()? {
            Ok(CoreFuncKind::WaitableSetWait(parser.parse()?))
        } else if l.peek::<kw::waitable_set_poll>()? {
            Ok(CoreFuncKind::WaitableSetPoll(parser.parse()?))
        } else if l.peek::<kw::waitable_set_drop>()? {
            parser.parse::<kw::waitable_set_drop>()?;
            Ok(CoreFuncKind::WaitableSetDrop)
        } else if l.peek::<kw::waitable_join>()? {
            parser.parse::<kw::waitable_join>()?;
            Ok(CoreFuncKind::WaitableJoin)
        } else if l.peek::<kw::thread_index>()? {
            parser.parse::<kw::thread_index>()?;
            Ok(CoreFuncKind::ThreadIndex)
        } else if l.peek::<kw::thread_new_indirect>()? {
            Ok(CoreFuncKind::ThreadNewIndirect(parser.parse()?))
        } else if l.peek::<kw::thread_switch_to>()? {
            Ok(CoreFuncKind::ThreadSwitchTo(parser.parse()?))
        } else if l.peek::<kw::thread_suspend>()? {
            Ok(CoreFuncKind::ThreadSuspend(parser.parse()?))
        } else if l.peek::<kw::thread_resume_later>()? {
            parser.parse::<kw::thread_resume_later>()?;
            Ok(CoreFuncKind::ThreadResumeLater)
        } else if l.peek::<kw::thread_yield_to>()? {
            Ok(CoreFuncKind::ThreadYieldTo(parser.parse()?))
        } else {
            Err(l.error())
        }
    }
}

/// A declared component function.
///
/// This may be a member of the import, alias, or canon sections.
#[derive(Debug)]
pub struct Func<'a> {
    /// Where this `func` was defined.
    pub span: Span,
    /// An identifier that this function is resolved with (optionally) for name
    /// resolution.
    pub id: Option<Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: InlineExport<'a>,
    /// The kind of function.
    pub kind: FuncKind<'a>,
}

impl<'a> Parse<'a> for Func<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::func>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;
        let kind = parser.parse()?;

        Ok(Self {
            span,
            id,
            name,
            exports,
            kind,
        })
    }
}

/// Represents the kind of component functions.
#[derive(Debug)]
pub enum FuncKind<'a> {
    /// A function which is actually defined as an import, such as:
    ///
    /// ```text
    /// (func (import "foo") (param string))
    /// ```
    Import {
        /// The import name of this import.
        import: InlineImport<'a>,
        /// The type that this function will have.
        ty: ComponentTypeUse<'a, ComponentFunctionType<'a>>,
    },
    /// The function is defined in terms of lifting a core function.
    ///
    /// The function is actually a member of the canon section.
    Lift {
        /// The lifted function's type.
        ty: ComponentTypeUse<'a, ComponentFunctionType<'a>>,
        /// Information relating to the lifting of the core function.
        info: CanonLift<'a>,
    },
    /// The function is defined in terms of aliasing a component instance export.
    ///
    /// The function is actually a member of the alias section.
    Alias(InlineExportAlias<'a, false>),
}

impl<'a> Parse<'a> for FuncKind<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if let Some(import) = parser.parse()? {
            Ok(Self::Import {
                import,
                ty: parser.parse()?,
            })
        } else if parser.peek::<LParen>()? && parser.peek2::<kw::alias>()? {
            parser.parens(|parser| Ok(Self::Alias(parser.parse()?)))
        } else {
            Ok(Self::Lift {
                ty: parser.parse()?,
                info: parser.parens(|parser| {
                    parser.parse::<kw::canon>()?;
                    parser.parse()
                })?,
            })
        }
    }
}

/// A WebAssembly canonical function to be inserted into a component.
///
/// This is a member of the canonical section.
#[derive(Debug)]
pub struct CanonicalFunc<'a> {
    /// Where this `func` was defined.
    pub span: Span,
    /// An identifier that this function is resolved with (optionally) for name
    /// resolution.
    pub id: Option<Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// What kind of function this is, be it a lowered or lifted function.
    pub kind: CanonicalFuncKind<'a>,
}

impl<'a> Parse<'a> for CanonicalFunc<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::canon>()?.0;
        let mut l = parser.lookahead1();

        if l.peek::<kw::lift>()? {
            let info = parser.parse()?;
            let (id, name, ty) = parser.parens(|parser| {
                parser.parse::<kw::func>()?;
                let id = parser.parse()?;
                let name = parser.parse()?;
                let ty = parser.parse()?;
                Ok((id, name, ty))
            })?;

            Ok(Self {
                span,
                id,
                name,
                kind: CanonicalFuncKind::Lift { info, ty },
            })
        } else {
            let kind = CoreFuncKind::parse_lookahead(l)?;
            let (id, name) = parser.parens(|parser| {
                parser.parse::<kw::core>()?;
                parser.parse::<kw::func>()?;
                let id = parser.parse()?;
                let name = parser.parse()?;
                Ok((id, name))
            })?;

            Ok(Self {
                span,
                id,
                name,
                kind: CanonicalFuncKind::Core(kind),
            })
        }
    }
}

/// Possible ways to define a canonical function in the text format.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum CanonicalFuncKind<'a> {
    /// A canonical function that is defined in terms of lifting a core function.
    Lift {
        /// The lifted function's type.
        ty: ComponentTypeUse<'a, ComponentFunctionType<'a>>,
        /// Information relating to the lifting of the core function.
        info: CanonLift<'a>,
    },

    /// A canonical function that defines a core function, whose variants are
    /// delegated to `CoreFuncKind`.
    Core(CoreFuncKind<'a>),
}

/// Information relating to lifting a core function.
#[derive(Debug)]
pub struct CanonLift<'a> {
    /// The core function being lifted.
    pub func: CoreItemRef<'a, kw::func>,
    /// The canonical options for the lifting.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonLift<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::lift>()?;

        Ok(Self {
            func: parser.parens(|parser| {
                parser.parse::<kw::core>()?;
                parser.parse()
            })?,
            opts: parser.parse()?,
        })
    }
}

impl Default for CanonLift<'_> {
    fn default() -> Self {
        let span = Span::from_offset(0);
        Self {
            func: CoreItemRef {
                kind: kw::func(span),
                idx: Index::Num(0, span),
                export_name: None,
            },
            opts: Vec::new(),
        }
    }
}

/// Information relating to lowering a component function.
#[derive(Debug)]
pub struct CanonLower<'a> {
    /// The function being lowered.
    pub func: ItemRef<'a, kw::func>,
    /// The canonical options for the lowering.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonLower<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::lower>()?;

        Ok(Self {
            func: parser.parens(|parser| parser.parse())?,
            opts: parser.parse()?,
        })
    }
}

impl Default for CanonLower<'_> {
    fn default() -> Self {
        let span = Span::from_offset(0);
        Self {
            func: ItemRef {
                kind: kw::func(span),
                idx: Index::Num(0, span),
                export_names: Vec::new(),
            },
            opts: Vec::new(),
        }
    }
}

/// Information relating to the `resource.new` intrinsic.
#[derive(Debug)]
pub struct CanonResourceNew<'a> {
    /// The resource type that this intrinsic creates an owned reference to.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonResourceNew<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::resource_new>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `resource.drop` intrinsic.
#[derive(Debug)]
pub struct CanonResourceDrop<'a> {
    /// The resource type that this intrinsic is dropping.
    pub ty: Index<'a>,
    /// Whether or not this function is async
    pub async_: bool,
}

impl<'a> Parse<'a> for CanonResourceDrop<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::resource_drop>()?;

        Ok(Self {
            ty: parser.parse()?,
            async_: parser.parse::<Option<kw::r#async>>()?.is_some(),
        })
    }
}

/// Information relating to the `resource.rep` intrinsic.
#[derive(Debug)]
pub struct CanonResourceRep<'a> {
    /// The resource type that this intrinsic is accessing.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonResourceRep<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::resource_rep>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `thread.spawn-ref` intrinsic.
#[derive(Debug)]
pub struct CanonThreadSpawnRef<'a> {
    /// The function type that is being spawned.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonThreadSpawnRef<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_spawn_ref>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `thread.spawn-indirect` intrinsic.
///
/// This should look quite similar to parsing of `CallIndirect`.
#[derive(Debug)]
pub struct CanonThreadSpawnIndirect<'a> {
    /// The function type that is being spawned.
    pub ty: Index<'a>,
    /// The table that this spawn is going to be indexing.
    pub table: CoreItemRef<'a, kw::table>,
}

impl<'a> Parse<'a> for CanonThreadSpawnIndirect<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_spawn_indirect>()?;
        let ty = parser.parse()?;
        let table = parser.parens(|p| p.parse())?;
        Ok(Self { ty, table })
    }
}

/// Information relating to the `thread.spawn` intrinsic.
#[derive(Debug)]
pub struct CanonThreadAvailableParallelism;

impl<'a> Parse<'a> for CanonThreadAvailableParallelism {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_available_parallelism>()?;
        Ok(Self)
    }
}

/// Information relating to the `task.return` intrinsic.
#[derive(Debug)]
pub struct CanonTaskReturn<'a> {
    /// The type of the result which may be returned with this intrinsic.
    pub result: Option<ComponentValType<'a>>,
    /// The canonical options for storing values.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonTaskReturn<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::task_return>()?;

        Ok(Self {
            result: if parser.peek2::<kw::result>()? {
                Some(parser.parens(|p| {
                    p.parse::<kw::result>()?.0;
                    p.parse()
                })?)
            } else {
                None
            },
            opts: parser.parse()?,
        })
    }
}

/// Information relating to the `waitable-set.wait` intrinsic.
#[derive(Debug)]
pub struct CanonWaitableSetWait<'a> {
    /// If true, the component instance may be reentered during a call to this
    /// intrinsic.
    pub async_: bool,
    /// The memory to use when returning an event to the caller.
    pub memory: CoreItemRef<'a, kw::memory>,
}

impl<'a> Parse<'a> for CanonWaitableSetWait<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::waitable_set_wait>()?;
        let async_ = parser.parse::<Option<kw::cancellable>>()?.is_some();
        let memory = parser.parens(|p| p.parse())?;

        Ok(Self { async_, memory })
    }
}

/// Information relating to the `waitable-set.poll` intrinsic.
#[derive(Debug)]
pub struct CanonWaitableSetPoll<'a> {
    /// If true, the component instance may be reentered during a call to this
    /// intrinsic.
    pub async_: bool,
    /// The memory to use when returning an event to the caller.
    pub memory: CoreItemRef<'a, kw::memory>,
}

impl<'a> Parse<'a> for CanonWaitableSetPoll<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::waitable_set_poll>()?;
        let async_ = parser.parse::<Option<kw::cancellable>>()?.is_some();
        let memory = parser.parens(|p| p.parse())?;

        Ok(Self { async_, memory })
    }
}

/// Information relating to the `thread.yield` intrinsic.
#[derive(Debug)]
pub struct CanonThreadYield {
    /// If true, the component instance may be reentered during a call to this
    /// intrinsic.
    pub cancellable: bool,
}

impl<'a> Parse<'a> for CanonThreadYield {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_yield>()?;
        let cancellable = parser.parse::<Option<kw::cancellable>>()?.is_some();

        Ok(Self { cancellable })
    }
}

/// Information relating to the `subtask.cancel` intrinsic.
#[derive(Debug)]
pub struct CanonSubtaskCancel {
    /// If false, block until cancel is finished; otherwise return BLOCKED if
    /// necessary.
    pub async_: bool,
}

impl<'a> Parse<'a> for CanonSubtaskCancel {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::subtask_cancel>()?;
        let async_ = parser.parse::<Option<kw::r#async>>()?.is_some();

        Ok(Self { async_ })
    }
}

/// Information relating to the `stream.new` intrinsic.
#[derive(Debug)]
pub struct CanonStreamNew<'a> {
    /// The stream type to instantiate.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonStreamNew<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_new>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `stream.read` intrinsic.
#[derive(Debug)]
pub struct CanonStreamRead<'a> {
    /// The stream type to instantiate.
    pub ty: Index<'a>,
    /// The canonical options for storing values.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonStreamRead<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_read>()?;

        Ok(Self {
            ty: parser.parse()?,
            opts: parser.parse()?,
        })
    }
}

/// Information relating to the `stream.write` intrinsic.
#[derive(Debug)]
pub struct CanonStreamWrite<'a> {
    /// The stream type to instantiate.
    pub ty: Index<'a>,
    /// The canonical options for loading values.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonStreamWrite<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_write>()?;

        Ok(Self {
            ty: parser.parse()?,
            opts: parser.parse()?,
        })
    }
}

/// Information relating to the `stream.cancel-read` intrinsic.
#[derive(Debug)]
pub struct CanonStreamCancelRead<'a> {
    /// The stream type to instantiate.
    pub ty: Index<'a>,
    /// If false, block until cancel is finished; otherwise return BLOCKED if
    /// necessary.
    pub async_: bool,
}

impl<'a> Parse<'a> for CanonStreamCancelRead<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_cancel_read>()?;

        Ok(Self {
            ty: parser.parse()?,
            async_: parser.parse::<Option<kw::r#async>>()?.is_some(),
        })
    }
}

/// Information relating to the `stream.cancel-write` intrinsic.
#[derive(Debug)]
pub struct CanonStreamCancelWrite<'a> {
    /// The stream type to instantiate.
    pub ty: Index<'a>,
    /// If false, block until cancel is finished; otherwise return BLOCKED if
    /// necessary.
    pub async_: bool,
}

impl<'a> Parse<'a> for CanonStreamCancelWrite<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_cancel_write>()?;

        Ok(Self {
            ty: parser.parse()?,
            async_: parser.parse::<Option<kw::r#async>>()?.is_some(),
        })
    }
}

/// Information relating to the `stream.drop-readable` intrinsic.
#[derive(Debug)]
pub struct CanonStreamDropReadable<'a> {
    /// The stream type to drop.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonStreamDropReadable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_drop_readable>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `stream.drop-writable` intrinsic.
#[derive(Debug)]
pub struct CanonStreamDropWritable<'a> {
    /// The stream type to drop.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonStreamDropWritable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_drop_writable>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `future.new` intrinsic.
#[derive(Debug)]
pub struct CanonFutureNew<'a> {
    /// The future type to instantiate.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonFutureNew<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_new>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `future.read` intrinsic.
#[derive(Debug)]
pub struct CanonFutureRead<'a> {
    /// The future type to instantiate.
    pub ty: Index<'a>,
    /// The canonical options for storing values.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonFutureRead<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_read>()?;

        Ok(Self {
            ty: parser.parse()?,
            opts: parser.parse()?,
        })
    }
}

/// Information relating to the `future.write` intrinsic.
#[derive(Debug)]
pub struct CanonFutureWrite<'a> {
    /// The future type to instantiate.
    pub ty: Index<'a>,
    /// The canonical options for loading values.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonFutureWrite<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_write>()?;

        Ok(Self {
            ty: parser.parse()?,
            opts: parser.parse()?,
        })
    }
}

/// Information relating to the `future.cancel-read` intrinsic.
#[derive(Debug)]
pub struct CanonFutureCancelRead<'a> {
    /// The future type to instantiate.
    pub ty: Index<'a>,
    /// If false, block until cancel is finished; otherwise return BLOCKED if
    /// necessary.
    pub async_: bool,
}

impl<'a> Parse<'a> for CanonFutureCancelRead<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_cancel_read>()?;

        Ok(Self {
            ty: parser.parse()?,
            async_: parser.parse::<Option<kw::r#async>>()?.is_some(),
        })
    }
}

/// Information relating to the `future.cancel-write` intrinsic.
#[derive(Debug)]
pub struct CanonFutureCancelWrite<'a> {
    /// The future type to instantiate.
    pub ty: Index<'a>,
    /// If false, block until cancel is finished; otherwise return BLOCKED if
    /// necessary.
    pub async_: bool,
}

impl<'a> Parse<'a> for CanonFutureCancelWrite<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_cancel_write>()?;

        Ok(Self {
            ty: parser.parse()?,
            async_: parser.parse::<Option<kw::r#async>>()?.is_some(),
        })
    }
}

/// Information relating to the `future.drop-readable` intrinsic.
#[derive(Debug)]
pub struct CanonFutureDropReadable<'a> {
    /// The future type to drop.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonFutureDropReadable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_drop_readable>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `future.drop-writable` intrinsic.
#[derive(Debug)]
pub struct CanonFutureDropWritable<'a> {
    /// The future type to drop.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonFutureDropWritable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_drop_writable>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `error-context.new` intrinsic.
#[derive(Debug)]
pub struct CanonErrorContextNew<'a> {
    /// The canonical options for loading the debug message.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonErrorContextNew<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::error_context_new>()?;

        Ok(Self {
            opts: parser.parse()?,
        })
    }
}

/// Information relating to the `error-context.debug-message` intrinsic.
#[derive(Debug)]
pub struct CanonErrorContextDebugMessage<'a> {
    /// The canonical options for storing the debug message.
    pub opts: Vec<CanonOpt<'a>>,
}

impl<'a> Parse<'a> for CanonErrorContextDebugMessage<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::error_context_debug_message>()?;

        Ok(Self {
            opts: parser.parse()?,
        })
    }
}

/// Information relating to the `thread.new-indirect` intrinsic.
#[derive(Debug)]
pub struct CanonThreadNewIndirect<'a> {
    /// The function type for the thread start function.
    pub ty: Index<'a>,
    /// The table to index.
    pub table: CoreItemRef<'a, kw::table>,
}

impl<'a> Parse<'a> for CanonThreadNewIndirect<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_new_indirect>()?;
        let ty = parser.parse()?;
        let table = parser.parens(|p| p.parse())?;
        Ok(Self { ty, table })
    }
}

/// Information relating to the `thread.switch-to` intrinsic.
#[derive(Debug)]
pub struct CanonThreadSwitchTo {
    /// Whether the thread can be cancelled while suspended at this point.
    pub cancellable: bool,
}

impl<'a> Parse<'a> for CanonThreadSwitchTo {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_switch_to>()?;
        let cancellable = parser.parse::<Option<kw::cancellable>>()?.is_some();
        Ok(Self { cancellable })
    }
}

/// Information relating to the `thread.suspend` intrinsic.
#[derive(Debug)]
pub struct CanonThreadSuspend {
    /// Whether the thread can be cancelled while suspended at this point.
    pub cancellable: bool,
}
impl<'a> Parse<'a> for CanonThreadSuspend {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_suspend>()?;
        let cancellable = parser.parse::<Option<kw::cancellable>>()?.is_some();
        Ok(Self { cancellable })
    }
}

/// Information relating to the `thread.yield-to` intrinsic.
#[derive(Debug)]
pub struct CanonThreadYieldTo {
    /// Whether the thread can be cancelled while yielding at this point.
    pub cancellable: bool,
}
impl<'a> Parse<'a> for CanonThreadYieldTo {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_yield_to>()?;
        let cancellable = parser.parse::<Option<kw::cancellable>>()?.is_some();
        Ok(Self { cancellable })
    }
}

#[derive(Debug)]
/// Canonical ABI options.
pub enum CanonOpt<'a> {
    /// Encode strings as UTF-8.
    StringUtf8,
    /// Encode strings as UTF-16.
    StringUtf16,
    /// Encode strings as "compact UTF-16".
    StringLatin1Utf16,
    /// Use the specified memory for canonical ABI memory access.
    Memory(CoreItemRef<'a, kw::memory>),
    /// Use the specified reallocation function for memory allocations.
    Realloc(CoreItemRef<'a, kw::func>),
    /// Call the specified function after the lifted function has returned.
    PostReturn(CoreItemRef<'a, kw::func>),
    /// Use the async ABI for lifting or lowering.
    Async,
    /// Use the specified function to deliver async events to stackless coroutines.
    Callback(CoreItemRef<'a, kw::func>),
    /// Lower this component function into the specified core function type.
    CoreType(CoreItemRef<'a, kw::r#type>),
    /// Use the GC variant of the canonical ABI.
    Gc,
}

impl Default for kw::r#type {
    fn default() -> Self {
        Self(Span::from_offset(0))
    }
}

impl<'a> Parse<'a> for CanonOpt<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::string_utf8>()? {
            parser.parse::<kw::string_utf8>()?;
            Ok(Self::StringUtf8)
        } else if l.peek::<kw::string_utf16>()? {
            parser.parse::<kw::string_utf16>()?;
            Ok(Self::StringUtf16)
        } else if l.peek::<kw::string_latin1_utf16>()? {
            parser.parse::<kw::string_latin1_utf16>()?;
            Ok(Self::StringLatin1Utf16)
        } else if l.peek::<kw::r#async>()? {
            parser.parse::<kw::r#async>()?;
            Ok(Self::Async)
        } else if l.peek::<kw::gc>()? {
            parser.parse::<kw::gc>()?;
            Ok(Self::Gc)
        } else if l.peek::<LParen>()? {
            parser.parens(|parser| {
                let mut l = parser.lookahead1();
                if l.peek::<kw::memory>()? {
                    Ok(CanonOpt::Memory(parser.parse()?))
                } else if l.peek::<kw::realloc>()? {
                    parser.parse::<kw::realloc>()?;
                    Ok(CanonOpt::Realloc(
                        parser.parse::<IndexOrCoreRef<'_, _>>()?.0,
                    ))
                } else if l.peek::<kw::post_return>()? {
                    parser.parse::<kw::post_return>()?;
                    Ok(CanonOpt::PostReturn(
                        parser.parse::<IndexOrCoreRef<'_, _>>()?.0,
                    ))
                } else if l.peek::<kw::callback>()? {
                    parser.parse::<kw::callback>()?;
                    Ok(CanonOpt::Callback(
                        parser.parse::<IndexOrCoreRef<'_, _>>()?.0,
                    ))
                } else if l.peek::<kw::core_type>()? {
                    parser.parse::<kw::core_type>()?;
                    Ok(CanonOpt::CoreType(
                        parser.parse::<IndexOrCoreRef<'_, _>>()?.0,
                    ))
                } else {
                    Err(l.error())
                }
            })
        } else {
            Err(l.error())
        }
    }
}

impl Peek for CanonOpt<'_> {
    fn peek(cursor: Cursor<'_>) -> Result<bool> {
        Ok(kw::string_utf8::peek(cursor)?
            || kw::string_utf16::peek(cursor)?
            || kw::string_latin1_utf16::peek(cursor)?
            || kw::r#async::peek(cursor)?
            || kw::gc::peek(cursor)?
            || match cursor.lparen()? {
                Some(next) => {
                    kw::memory::peek(next)?
                        || kw::realloc::peek(next)?
                        || kw::post_return::peek(next)?
                        || kw::callback::peek(next)?
                        || kw::core_type::peek(next)?
                }
                None => false,
            })
    }

    fn display() -> &'static str {
        "canonical option"
    }
}

impl<'a> Parse<'a> for Vec<CanonOpt<'a>> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut funcs = Vec::new();
        while parser.peek::<CanonOpt<'_>>()? {
            funcs.push(parser.parse()?);
        }
        Ok(funcs)
    }
}
