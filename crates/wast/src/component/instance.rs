use crate::component::*;
use crate::core;
use crate::kw;
use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use crate::token::{Id, Index, ItemRef, LParen, NameAnnotation, Span};

/// A nested WebAssembly instance to be created as part of a module.
#[derive(Debug)]
pub struct Instance<'a> {
    /// Where this `instance` was defined.
    pub span: Span,
    /// An identifier that this instance is resolved with (optionally) for name
    /// resolution.
    pub id: Option<Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: core::InlineExport<'a>,
    /// What kind of instance this is, be it an inline-defined or imported one.
    pub kind: InstanceKind<'a>,
}

/// Possible ways to define a instance in the text format.
#[derive(Debug)]
pub enum InstanceKind<'a> {
    /// The `(instance (import "x"))` sugar syntax
    Import {
        /// The name of the import
        import: InlineImport<'a>,
        /// The type of the instance being imported
        ty: ComponentTypeUse<'a, InstanceType<'a>>,
    },

    /// Instantiate a core module.
    Module {
        /// Module that we're instantiating
        module: ItemRef<'a, kw::module>,
        /// Arguments used to instantiate the instance
        args: Vec<NamedModuleArg<'a>>,
    },

    /// Instantiate a component.
    Component {
        /// Component that we're instantiating
        component: ItemRef<'a, kw::component>,
        /// Arguments used to instantiate the instance
        args: Vec<NamedComponentArg<'a>>,
    },

    /// A bundle of module exports which isn't an instance, but can be used
    /// in places that need an instance.
    BundleOfExports {
        /// Arguments used to create the anonymous instance
        args: Vec<core::Export<'a>>,
    },

    /// A bundle of component exports which isn't an instance, but can be used
    /// in places that need an instance.
    BundleOfComponentExports {
        /// Arguments used to create the anonymous instance
        args: Vec<ComponentExport<'a>>,
    },
}

/// Arguments to the module `instantiate` instruction
#[derive(Debug)]
#[allow(missing_docs)]
pub struct NamedModuleArg<'a> {
    pub name: &'a str,
    pub arg: ModuleArg<'a>,
}

impl<'a> Parse<'a> for NamedModuleArg<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::with>()?;
        Ok(NamedModuleArg {
            name: parser.parse()?,
            arg: parser.parse()?,
        })
    }
}

/// Arguments to the component `instantiate` instruction
#[derive(Debug)]
#[allow(missing_docs)]
pub struct NamedComponentArg<'a> {
    pub name: &'a str,
    pub arg: ComponentArg<'a>,
}

impl<'a> Parse<'a> for NamedComponentArg<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::with>()?;
        Ok(NamedComponentArg {
            name: parser.parse()?,
            arg: parser.parse()?,
        })
    }
}

/// modulearg    ::= (instance <instanceidx>)
///                | (instance <core:export>*)
#[derive(Debug)]
pub enum ModuleArg<'a> {
    /// Core modules can reference instances.
    Def(ItemRef<'a, kw::instance>),
    /// `instance`, but it isn't actually an instance; it's a tuple of exports
    /// which can be used in place of an instance.
    BundleOfExports(Span, Vec<core::Export<'a>>),
}

/// componentarg ::= (module <moduleidx>)
///                | (component <componentidx>)
///                | (instance <instanceidx>)
///                | (func <funcidx>)
///                | (value <valueidx>)
///                | (instance <export>*)
#[derive(Debug)]
pub enum ComponentArg<'a> {
    /// A reference to an item of one of the deftype kinds.
    Def(ItemRef<'a, DefTypeKind>),
    /// `type`, which is separate here because it isn't yet a deftype kind.
    Type(ItemRef<'a, kw::r#type>),
    /// `instance`, but it isn't actually an instance; it's a tuple of exports
    /// which can be used in place of an instance.
    BundleOfExports(Span, Vec<ComponentExport<'a>>),
}

impl<'a> Parse<'a> for Instance<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::instance>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        let kind = if let Some(import) = parser.parse()? {
            InstanceKind::Import {
                import,
                ty: parser.parse()?,
            }
        } else if parser.peek::<LParen>() && parser.peek2::<kw::instantiate>() {
            parser.parens(|p| {
                p.parse::<kw::instantiate>()?;
                if p.peek2::<kw::module>() {
                    let module = p.parse()?;
                    let mut args = Vec::new();
                    while !p.is_empty() {
                        args.push(p.parens(|p| p.parse())?);
                    }
                    Ok(InstanceKind::Module { module, args })
                } else if p.peek2::<kw::component>() {
                    let component = p.parse()?;
                    let mut args = Vec::new();
                    while !p.is_empty() {
                        args.push(p.parens(|p| p.parse())?);
                    }
                    Ok(InstanceKind::Component { component, args })
                } else {
                    return Err(parser.error("expected module or component"));
                }
            })?
        } else if parser.peek::<kw::core>() {
            parser.parse::<kw::core>()?;
            let mut args = Vec::new();
            while !parser.is_empty() {
                args.push(parser.parens(|p| p.parse())?);
            }
            InstanceKind::BundleOfExports { args }
        } else if parser.peek2::<LParen>() && parser.peek2::<kw::export>() {
            let mut args = Vec::new();
            while !parser.is_empty() {
                args.push(parser.parse()?);
            }
            InstanceKind::BundleOfComponentExports { args }
        } else if parser.is_empty() {
            let args = Vec::new();
            InstanceKind::BundleOfComponentExports { args }
        } else {
            return Err(parser.error("expected `(instantiate` or `(export`"));
        };

        Ok(Instance {
            span,
            id,
            name,
            exports,
            kind,
        })
    }
}

impl<'a> Parse<'a> for ModuleArg<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<LParen>() && parser.peek2::<kw::instance>() && parser.peek3::<Index>() {
            // `(instance <index>)`
            let def = parser.parse::<ItemRef<kw::instance>>()?;
            Ok(ModuleArg::Def(def))
        } else if parser.peek::<LParen>() && parser.peek2::<kw::instance>() {
            let (span, exports) = parser.parens(|p| {
                let span = p.parse::<kw::instance>()?.0;
                let mut exports = Vec::new();
                while !parser.is_empty() {
                    exports.push(parser.parens(|parser| parser.parse())?);
                }
                Ok((span, exports))
            })?;
            Ok(ModuleArg::BundleOfExports(span, exports))
        } else {
            Err(parser.error("expected an instance"))
        }
    }
}

impl<'a> Parse<'a> for ComponentArg<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<ItemRef<'a, DefTypeKind>>() && parser.peek3::<Index>() {
            // `(<deftypekind> <index>)`
            let def = parser.parse::<ItemRef<'a, DefTypeKind>>()?;
            Ok(ComponentArg::Def(def))
        } else if parser.peek::<ItemRef<kw::r#type>>() {
            let def = parser.parse::<ItemRef<'a, kw::r#type>>()?;
            Ok(ComponentArg::Type(def))
        } else if parser.peek::<LParen>() && parser.peek2::<kw::instance>() {
            let (span, exports) = parser.parens(|p| {
                let span = p.parse::<kw::instance>()?.0;
                let mut exports = Vec::new();
                while !p.is_empty() {
                    exports.push(p.parens(|p| p.parse())?);
                }
                Ok((span, exports))
            })?;
            Ok(ComponentArg::BundleOfExports(span, exports))
        } else {
            Err(parser.error("expected def type, type, or instance"))
        }
    }
}

impl Peek for ComponentArg<'_> {
    fn peek(cursor: Cursor<'_>) -> bool {
        ItemRef::<DefTypeKind>::peek(cursor)
            || ItemRef::<kw::r#type>::peek2(cursor)
            || (LParen::peek(cursor) && kw::instance::peek2(cursor))
    }
    fn display() -> &'static str {
        "componentarg"
    }
}
