use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

/// A WebAssembly function to be inserted into a module.
///
/// This is a member of both the function and code sections.
#[derive(Debug)]
pub struct ComponentFunc<'a> {
    /// Where this `func` was defined.
    pub span: ast::Span,
    /// An identifier that this function is resolved with (optionally) for name
    /// resolution.
    pub id: Option<ast::Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<ast::NameAnnotation<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: ast::InlineExport<'a>,
    /// What kind of function this is, be it an inline-defined or imported
    /// function.
    pub kind: ComponentFuncKind<'a>,
    /// The type that this function will have.
    pub ty: ast::TypeUse<'a, ast::ComponentFunctionType<'a>>,
}

/// Possible ways to define a function in the text format.
#[derive(Debug)]
pub enum ComponentFuncKind<'a> {
    /// A function which is actually defined as an import, such as:
    ///
    /// ```text
    /// (func (type 3) (import "foo" "bar"))
    /// ```
    Import(ast::InlineImport<'a>),

    /// Almost all functions, those defined inline in a wasm module.
    Inline {
        /// The body of the function.
        body: ast::ComponentFuncBody<'a>,
    },
}

impl<'a> Parse<'a> for ComponentFunc<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::func>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;

        let (ty, kind) = if let Some(import) = parser.parse()? {
            (parser.parse()?, ComponentFuncKind::Import(import))
        } else {
            let ty = parser.parse()?;
            (
                ty,
                ComponentFuncKind::Inline {
                    body: parser.parse()?,
                },
            )
        };

        Ok(ComponentFunc {
            span,
            id,
            name,
            exports,
            ty,
            kind,
        })
    }
}

/// The body of a `ComponentFunc`.
#[derive(Debug)]
pub enum ComponentFuncBody<'a> {
    /// A `canon.lift`.
    CanonLift(CanonLift<'a>),
    /// A `canon.lower`.
    CanonLower(CanonLower<'a>),
}

impl<'a> Parse<'a> for ComponentFuncBody<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek2::<kw::canon_lift>() {
            Ok(ComponentFuncBody::CanonLift(parser.parse()?))
        } else if parser.peek2::<kw::canon_lower>() {
            Ok(ComponentFuncBody::CanonLower(parser.parse()?))
        } else {
            Err(parser.error("Expected canon.lift or canon.lower"))
        }
    }
}

/// Extra information associated with canon.lift instructions.
#[derive(Debug)]
#[allow(dead_code)] // TODO: encoding
pub struct CanonLift<'a> {
    type_: ast::ComponentFunctionType<'a>,
    opts: Vec<CanonOpt<'a>>,
    func: ast::IndexOrRef<'a, kw::func>,
}

impl<'a> Parse<'a> for CanonLift<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::canon_lift>()?;
            let type_ = parser.parse()?;
            let mut opts = Vec::new();
            while !parser.is_empty()
                && (!parser.peek::<ast::LParen>() || !parser.peek2::<kw::func>())
            {
                opts.push(parser.parse()?);
            }
            let func = parser.parse()?;
            Ok(CanonLift { type_, opts, func })
        })
    }
}

/// Extra information associated with canon.lower instructions.
#[derive(Debug)]
#[allow(dead_code)] // TODO: encoding
pub struct CanonLower<'a> {
    opts: Vec<CanonOpt<'a>>,
    func: ast::IndexOrRef<'a, kw::func>,
}

impl<'a> Parse<'a> for CanonLower<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::canon_lower>()?;
            let mut opts = Vec::new();
            while !parser.is_empty()
                && (!parser.peek::<ast::LParen>() || !parser.peek2::<kw::func>())
            {
                opts.push(parser.parse()?);
            }
            let func = parser.parse()?;
            Ok(CanonLower { opts, func })
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
    /// A target instance which supplies the memory that the canonical ABI
    /// should operate on as well as functions that the canonical ABI can call
    /// to allocate, reallocate and free linear memory
    Into(ast::IndexOrRef<'a, kw::instance>),
}

impl<'a> Parse<'a> for CanonOpt<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::string_utf8>() {
            parser.parse::<kw::string_utf8>()?;
            Ok(CanonOpt::StringUtf8)
        } else if l.peek::<kw::string_utf16>() {
            parser.parse::<kw::string_utf16>()?;
            Ok(CanonOpt::StringUtf16)
        } else if l.peek::<kw::string_latin1_utf16>() {
            parser.parse::<kw::string_latin1_utf16>()?;
            Ok(CanonOpt::StringLatin1Utf16)
        } else if l.peek::<ast::LParen>() {
            parser.parens(|parser| {
                let mut l = parser.lookahead1();
                if l.peek::<kw::into>() {
                    parser.parse::<kw::into>()?;
                    Ok(CanonOpt::Into(parser.parse()?))
                } else {
                    Err(l.error())
                }
            })
        } else {
            Err(l.error())
        }
    }
}
