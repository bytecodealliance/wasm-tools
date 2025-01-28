use crate::component::*;
use crate::kw;
use crate::parser::{Parse, Parser, Result};
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
        let kind = parser.parse()?;

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
    ThreadSpawn(CanonThreadSpawn<'a>),
    ThreadHwConcurrency(CanonThreadHwConcurrency),
    TaskBackpressure,
    TaskReturn(CanonTaskReturn<'a>),
    TaskWait(CanonTaskWait<'a>),
    TaskPoll(CanonTaskPoll<'a>),
    TaskYield(CanonTaskYield),
    SubtaskDrop,
    StreamNew(CanonStreamNew<'a>),
    StreamRead(CanonStreamRead<'a>),
    StreamWrite(CanonStreamWrite<'a>),
    StreamCancelRead(CanonStreamCancelRead<'a>),
    StreamCancelWrite(CanonStreamCancelWrite<'a>),
    StreamCloseReadable(CanonStreamCloseReadable<'a>),
    StreamCloseWritable(CanonStreamCloseWritable<'a>),
    FutureNew(CanonFutureNew<'a>),
    FutureRead(CanonFutureRead<'a>),
    FutureWrite(CanonFutureWrite<'a>),
    FutureCancelRead(CanonFutureCancelRead<'a>),
    FutureCancelWrite(CanonFutureCancelWrite<'a>),
    FutureCloseReadable(CanonFutureCloseReadable<'a>),
    FutureCloseWritable(CanonFutureCloseWritable<'a>),
    ErrorContextNew(CanonErrorContextNew<'a>),
    ErrorContextDebugMessage(CanonErrorContextDebugMessage<'a>),
    ErrorContextDrop,
}

impl<'a> Parse<'a> for CoreFuncKind<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            let mut l = parser.lookahead1();
            if l.peek::<kw::canon>()? {
                parser.parse::<kw::canon>()?;
            } else if l.peek::<kw::alias>()? {
                return Ok(Self::Alias(parser.parse()?));
            } else {
                return Err(l.error());
            }
            let mut l = parser.lookahead1();
            if l.peek::<kw::lower>()? {
                Ok(CoreFuncKind::Lower(parser.parse()?))
            } else if l.peek::<kw::resource_new>()? {
                Ok(CoreFuncKind::ResourceNew(parser.parse()?))
            } else if l.peek::<kw::resource_drop>()? {
                Ok(CoreFuncKind::ResourceDrop(parser.parse()?))
            } else if l.peek::<kw::resource_rep>()? {
                Ok(CoreFuncKind::ResourceRep(parser.parse()?))
            } else if l.peek::<kw::thread_spawn>()? {
                Ok(CoreFuncKind::ThreadSpawn(parser.parse()?))
            } else if l.peek::<kw::thread_hw_concurrency>()? {
                Ok(CoreFuncKind::ThreadHwConcurrency(parser.parse()?))
            } else if l.peek::<kw::task_backpressure>()? {
                parser.parse::<kw::task_backpressure>()?;
                Ok(CoreFuncKind::TaskBackpressure)
            } else if l.peek::<kw::task_return>()? {
                Ok(CoreFuncKind::TaskReturn(parser.parse()?))
            } else if l.peek::<kw::task_wait>()? {
                Ok(CoreFuncKind::TaskWait(parser.parse()?))
            } else if l.peek::<kw::task_poll>()? {
                Ok(CoreFuncKind::TaskPoll(parser.parse()?))
            } else if l.peek::<kw::task_yield>()? {
                Ok(CoreFuncKind::TaskYield(parser.parse()?))
            } else if l.peek::<kw::subtask_drop>()? {
                parser.parse::<kw::subtask_drop>()?;
                Ok(CoreFuncKind::SubtaskDrop)
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
            } else if l.peek::<kw::stream_close_readable>()? {
                Ok(CoreFuncKind::StreamCloseReadable(parser.parse()?))
            } else if l.peek::<kw::stream_close_writable>()? {
                Ok(CoreFuncKind::StreamCloseWritable(parser.parse()?))
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
            } else if l.peek::<kw::future_close_readable>()? {
                Ok(CoreFuncKind::FutureCloseReadable(parser.parse()?))
            } else if l.peek::<kw::future_close_writable>()? {
                Ok(CoreFuncKind::FutureCloseWritable(parser.parse()?))
            } else if l.peek::<kw::error_context_new>()? {
                Ok(CoreFuncKind::ErrorContextNew(parser.parse()?))
            } else if l.peek::<kw::error_context_debug_message>()? {
                Ok(CoreFuncKind::ErrorContextDebugMessage(parser.parse()?))
            } else if l.peek::<kw::error_context_drop>()? {
                parser.parse::<kw::error_context_drop>()?;
                Ok(CoreFuncKind::ErrorContextDrop)
            } else {
                Err(l.error())
            }
        })
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

        if parser.peek::<kw::lift>()? {
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
        } else if parser.peek::<kw::lower>()? {
            Self::parse_core_func(span, parser, CanonicalFuncKind::Lower)
        } else if parser.peek::<kw::resource_new>()? {
            Self::parse_core_func(span, parser, CanonicalFuncKind::ResourceNew)
        } else if parser.peek::<kw::resource_drop>()? {
            Self::parse_core_func(span, parser, CanonicalFuncKind::ResourceDrop)
        } else if parser.peek::<kw::resource_rep>()? {
            Self::parse_core_func(span, parser, CanonicalFuncKind::ResourceRep)
        } else {
            Err(parser.error("expected `canon lift` or `canon lower`"))
        }
    }
}

impl<'a> CanonicalFunc<'a> {
    fn parse_core_func<T>(
        span: Span,
        parser: Parser<'a>,
        variant: fn(T) -> CanonicalFuncKind<'a>,
    ) -> Result<Self>
    where
        T: Parse<'a>,
    {
        let info = parser.parse()?;
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
            kind: variant(info),
        })
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
    /// A canonical function that is defined in terms of lowering a component function.
    Lower(CanonLower<'a>),

    ResourceNew(CanonResourceNew<'a>),
    ResourceDrop(CanonResourceDrop<'a>),
    ResourceRep(CanonResourceRep<'a>),

    ThreadSpawn(CanonThreadSpawn<'a>),
    ThreadHwConcurrency(CanonThreadHwConcurrency),

    TaskBackpressure,
    TaskReturn(CanonTaskReturn<'a>),
    TaskWait(CanonTaskWait<'a>),
    TaskPoll(CanonTaskPoll<'a>),
    TaskYield(CanonTaskYield),
    SubtaskDrop,
    StreamNew(CanonStreamNew<'a>),
    StreamRead(CanonStreamRead<'a>),
    StreamWrite(CanonStreamWrite<'a>),
    StreamCancelRead(CanonStreamCancelRead<'a>),
    StreamCancelWrite(CanonStreamCancelWrite<'a>),
    StreamCloseReadable(CanonStreamCloseReadable<'a>),
    StreamCloseWritable(CanonStreamCloseWritable<'a>),
    FutureNew(CanonFutureNew<'a>),
    FutureRead(CanonFutureRead<'a>),
    FutureWrite(CanonFutureWrite<'a>),
    FutureCancelRead(CanonFutureCancelRead<'a>),
    FutureCancelWrite(CanonFutureCancelWrite<'a>),
    FutureCloseReadable(CanonFutureCloseReadable<'a>),
    FutureCloseWritable(CanonFutureCloseWritable<'a>),
    ErrorContextNew(CanonErrorContextNew<'a>),
    ErrorContextDebugMessage(CanonErrorContextDebugMessage<'a>),
    ErrorContextDrop,
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
}

impl<'a> Parse<'a> for CanonResourceDrop<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::resource_drop>()?;

        Ok(Self {
            ty: parser.parse()?,
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

/// Information relating to the `thread.spawn` intrinsic.
#[derive(Debug)]
pub struct CanonThreadSpawn<'a> {
    /// The function type that is being spawned.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonThreadSpawn<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_spawn>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `thread.spawn` intrinsic.
#[derive(Debug)]
pub struct CanonThreadHwConcurrency;

impl<'a> Parse<'a> for CanonThreadHwConcurrency {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::thread_hw_concurrency>()?;
        Ok(Self)
    }
}

/// Information relating to the `task.return` intrinsic.
#[derive(Debug)]
pub struct CanonTaskReturn<'a> {
    /// The type of the result which may be returned with this intrinsic.
    pub result: Option<ComponentValType<'a>>,
}

impl<'a> Parse<'a> for CanonTaskReturn<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::task_return>()?;

        let result = if parser.peek2::<kw::result>()? {
            Some(parser.parens(|p| {
                p.parse::<kw::result>()?.0;
                p.parse()
            })?)
        } else {
            None
        };

        Ok(Self { result })
    }
}

/// Information relating to the `task.wait` intrinsic.
#[derive(Debug)]
pub struct CanonTaskWait<'a> {
    /// If true, the component instance may be reentered during a call to this
    /// intrinsic.
    pub async_: bool,
    /// The memory to use when returning an event to the caller.
    pub memory: CoreItemRef<'a, kw::memory>,
}

impl<'a> Parse<'a> for CanonTaskWait<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::task_wait>()?;
        let async_ = parser.parse::<Option<kw::r#async>>()?.is_some();
        let memory = parser.parens(|parser| {
            let span = parser.parse::<kw::memory>()?.0;
            parse_trailing_item_ref(kw::memory(span), parser)
        })?;

        Ok(Self { async_, memory })
    }
}

/// Information relating to the `task.poll` intrinsic.
#[derive(Debug)]
pub struct CanonTaskPoll<'a> {
    /// If true, the component instance may be reentered during a call to this
    /// intrinsic.
    pub async_: bool,
    /// The memory to use when returning an event to the caller.
    pub memory: CoreItemRef<'a, kw::memory>,
}

impl<'a> Parse<'a> for CanonTaskPoll<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::task_poll>()?;
        let async_ = parser.parse::<Option<kw::r#async>>()?.is_some();
        let memory = parser.parens(|parser| {
            let span = parser.parse::<kw::memory>()?.0;
            parse_trailing_item_ref(kw::memory(span), parser)
        })?;

        Ok(Self { async_, memory })
    }
}

/// Information relating to the `task.yield` intrinsic.
#[derive(Debug)]
pub struct CanonTaskYield {
    /// If true, the component instance may be reentered during a call to this
    /// intrinsic.
    pub async_: bool,
}

impl<'a> Parse<'a> for CanonTaskYield {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::task_yield>()?;
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

/// Information relating to the `stream.close-readable` intrinsic.
#[derive(Debug)]
pub struct CanonStreamCloseReadable<'a> {
    /// The stream type to close.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonStreamCloseReadable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_close_readable>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `stream.close-writable` intrinsic.
#[derive(Debug)]
pub struct CanonStreamCloseWritable<'a> {
    /// The stream type to close.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonStreamCloseWritable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::stream_close_writable>()?;

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

/// Information relating to the `future.close-readable` intrinsic.
#[derive(Debug)]
pub struct CanonFutureCloseReadable<'a> {
    /// The future type to close.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonFutureCloseReadable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_close_readable>()?;

        Ok(Self {
            ty: parser.parse()?,
        })
    }
}

/// Information relating to the `future.close-writable` intrinsic.
#[derive(Debug)]
pub struct CanonFutureCloseWritable<'a> {
    /// The future type to close.
    pub ty: Index<'a>,
}

impl<'a> Parse<'a> for CanonFutureCloseWritable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::future_close_writable>()?;

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
        } else if l.peek::<LParen>()? {
            parser.parens(|parser| {
                let mut l = parser.lookahead1();
                if l.peek::<kw::memory>()? {
                    let span = parser.parse::<kw::memory>()?.0;
                    Ok(CanonOpt::Memory(parse_trailing_item_ref(
                        kw::memory(span),
                        parser,
                    )?))
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
                } else {
                    Err(l.error())
                }
            })
        } else {
            Err(l.error())
        }
    }
}

fn parse_trailing_item_ref<T>(kind: T, parser: Parser) -> Result<CoreItemRef<T>> {
    Ok(CoreItemRef {
        kind,
        idx: parser.parse()?,
        export_name: parser.parse()?,
    })
}

impl<'a> Parse<'a> for Vec<CanonOpt<'a>> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut funcs = Vec::new();
        while !parser.is_empty() {
            funcs.push(parser.parse()?);
        }
        Ok(funcs)
    }
}
