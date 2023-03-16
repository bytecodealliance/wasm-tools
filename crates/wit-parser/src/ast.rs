use crate::{Error, UnresolvedPackage};
use anyhow::{bail, Context, Result};
use lex::{Span, Token, Tokenizer};
use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt;
use std::path::{Path, PathBuf};

pub mod lex;

pub use resolve::Resolver;
mod resolve;
pub mod toposort;

pub use lex::validate_id;

pub struct Ast<'a> {
    pub items: Vec<AstItem<'a>>,
}

impl<'a> Ast<'a> {
    pub fn parse(lexer: &mut Tokenizer<'a>) -> Result<Self> {
        let mut items = Vec::new();
        while lexer.clone().next()?.is_some() {
            let docs = parse_docs(lexer)?;
            items.push(AstItem::parse(lexer, docs)?);
        }
        Ok(Self { items })
    }

    fn for_each_path<'b>(
        &'b self,
        mut f: impl FnMut(Option<&'b Id<'a>>, &'b UsePath<'a>, Option<&[UseName<'a>]>) -> Result<()>,
    ) -> Result<()> {
        for item in self.items.iter() {
            match item {
                AstItem::World(world) => {
                    // Visit imports here first before exports to help preserve
                    // round-tripping of documents because printing a world puts
                    // imports first but textually they can be listed with
                    // exports first.
                    let mut imports = Vec::new();
                    let mut exports = Vec::new();
                    for item in world.items.iter() {
                        match item {
                            WorldItem::Use(u) => f(None, &u.from, Some(&u.names))?,
                            WorldItem::Type(_) => {}
                            WorldItem::Import(Import { kind, .. }) => imports.push(kind),
                            WorldItem::Export(Export { kind, .. }) => exports.push(kind),
                        }
                    }

                    let mut visit_kind = |kind: &'b ExternKind<'a>| match kind {
                        ExternKind::Interface(_, items) => {
                            for item in items {
                                match item {
                                    InterfaceItem::Use(u) => f(None, &u.from, Some(&u.names))?,
                                    _ => {}
                                }
                            }
                            Ok(())
                        }
                        ExternKind::Path(path) => f(None, path, None),
                        ExternKind::Func(_) => Ok(()),
                    };

                    for kind in imports {
                        visit_kind(kind)?;
                    }
                    for kind in exports {
                        visit_kind(kind)?;
                    }
                }
                AstItem::Interface(i) => {
                    for item in i.items.iter() {
                        match item {
                            InterfaceItem::Use(u) => f(Some(&i.name), &u.from, Some(&u.names))?,
                            _ => {}
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

pub enum AstItem<'a> {
    Interface(Interface<'a>),
    World(World<'a>),
}

impl<'a> AstItem<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        let mut clone = tokens.clone();
        let token = match clone.next()? {
            Some((_span, Token::Default)) => clone.next()?,
            other => other,
        };
        match token {
            Some((_span, Token::Interface)) => Interface::parse(tokens, docs).map(Self::Interface),
            Some((_span, Token::World)) => World::parse(tokens, docs).map(Self::World),
            other => Err(err_expected(tokens, "`default`, `world` or `interface`", other).into()),
        }
    }
}

pub struct World<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    items: Vec<WorldItem<'a>>,
    default: bool,
}

impl<'a> World<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        let default = tokens.eat(Token::Default)?;
        tokens.expect(Token::World)?;
        let name = parse_id(tokens)?;
        let items = Self::parse_items(tokens)?;
        Ok(World {
            docs,
            name,
            items,
            default,
        })
    }

    fn parse_items(tokens: &mut Tokenizer<'a>) -> Result<Vec<WorldItem<'a>>> {
        tokens.expect(Token::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            let docs = parse_docs(tokens)?;
            if tokens.eat(Token::RightBrace)? {
                break;
            }
            items.push(WorldItem::parse(tokens, docs)?);
        }
        Ok(items)
    }
}

pub enum WorldItem<'a> {
    Import(Import<'a>),
    Export(Export<'a>),
    Use(Use<'a>),
    Type(TypeDef<'a>),
}

impl<'a> WorldItem<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<WorldItem<'a>> {
        match tokens.clone().next()? {
            Some((_span, Token::Import)) => Import::parse(tokens, docs).map(WorldItem::Import),
            Some((_span, Token::Export)) => Export::parse(tokens, docs).map(WorldItem::Export),
            Some((_span, Token::Use)) => Use::parse(tokens).map(WorldItem::Use),
            Some((_span, Token::Type)) => TypeDef::parse(tokens, docs).map(WorldItem::Type),
            Some((_span, Token::Flags)) => TypeDef::parse_flags(tokens, docs).map(WorldItem::Type),
            Some((_span, Token::Record)) => {
                TypeDef::parse_record(tokens, docs).map(WorldItem::Type)
            }
            Some((_span, Token::Variant)) => {
                TypeDef::parse_variant(tokens, docs).map(WorldItem::Type)
            }
            Some((_span, Token::Union)) => TypeDef::parse_union(tokens, docs).map(WorldItem::Type),
            Some((_span, Token::Enum)) => TypeDef::parse_enum(tokens, docs).map(WorldItem::Type),
            other => Err(err_expected(
                tokens,
                "`import`, `export`, `use`, or type definition",
                other,
            )
            .into()),
        }
    }
}

pub struct Import<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    kind: ExternKind<'a>,
}

impl<'a> Import<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Import<'a>> {
        tokens.expect(Token::Import)?;
        let name = parse_id(tokens)?;
        tokens.expect(Token::Colon)?;
        let kind = ExternKind::parse(tokens)?;
        Ok(Import { docs, name, kind })
    }
}

pub struct Export<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    kind: ExternKind<'a>,
}

impl<'a> Export<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Export<'a>> {
        tokens.expect(Token::Export)?;
        let name = parse_id(tokens)?;
        tokens.expect(Token::Colon)?;
        let kind = ExternKind::parse(tokens)?;
        Ok(Export { docs, name, kind })
    }
}

pub enum ExternKind<'a> {
    Interface(Span, Vec<InterfaceItem<'a>>),
    Path(UsePath<'a>),
    Func(Func<'a>),
}

impl<'a> ExternKind<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<ExternKind<'a>> {
        match tokens.clone().next()? {
            Some((_span, Token::Id | Token::ExplicitId | Token::Pkg | Token::Self_)) => {
                UsePath::parse(tokens).map(ExternKind::Path)
            }
            Some((_span, Token::Interface)) => {
                let span = tokens.expect(Token::Interface)?;
                let items = Interface::parse_items(tokens)?;
                Ok(ExternKind::Interface(span, items))
            }
            Some((_span, Token::Func)) => Ok(ExternKind::Func(Func::parse(tokens)?)),
            other => Err(err_expected(tokens, "path, value, or interface", other).into()),
        }
    }
}

pub struct Interface<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    items: Vec<InterfaceItem<'a>>,
    default: bool,
}

impl<'a> Interface<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        let default = tokens.eat(Token::Default)?;
        tokens.expect(Token::Interface)?;
        let name = parse_id(tokens)?;
        let items = Self::parse_items(tokens)?;
        Ok(Interface {
            docs,
            name,
            items,
            default,
        })
    }

    pub(super) fn parse_items(tokens: &mut Tokenizer<'a>) -> Result<Vec<InterfaceItem<'a>>> {
        tokens.expect(Token::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            let docs = parse_docs(tokens)?;
            if tokens.eat(Token::RightBrace)? {
                break;
            }
            items.push(InterfaceItem::parse(tokens, docs)?);
        }
        Ok(items)
    }
}

pub enum InterfaceItem<'a> {
    TypeDef(TypeDef<'a>),
    Value(Value<'a>),
    Use(Use<'a>),
}

pub struct Use<'a> {
    pub from: UsePath<'a>,
    pub names: Vec<UseName<'a>>,
}

pub enum UsePath<'a> {
    Self_(Id<'a>),
    Package {
        doc: Id<'a>,
        iface: Option<Id<'a>>,
    },
    Dependency {
        dep: Id<'a>,
        doc: Id<'a>,
        iface: Option<Id<'a>>,
    },
}

pub struct UseName<'a> {
    pub name: Id<'a>,
    pub as_: Option<Id<'a>>,
}

impl<'a> Use<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<Self> {
        tokens.expect(Token::Use)?;
        let from = UsePath::parse(tokens)?;
        tokens.expect(Token::Period)?;
        tokens.expect(Token::LeftBrace)?;

        let mut names = Vec::new();
        while !tokens.eat(Token::RightBrace)? {
            let mut name = UseName {
                name: parse_id(tokens)?,
                as_: None,
            };
            if tokens.eat(Token::As)? {
                name.as_ = Some(parse_id(tokens)?);
            }
            names.push(name);
            if !tokens.eat(Token::Comma)? {
                tokens.expect(Token::RightBrace)?;
                break;
            }
        }
        Ok(Use { from, names })
    }
}

impl<'a> UsePath<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<Self> {
        match tokens.clone().next()? {
            Some((_span, Token::Self_)) => {
                tokens.expect(Token::Self_)?;
                tokens.expect(Token::Period)?;
                let name = parse_id(tokens)?;
                Ok(UsePath::Self_(name))
            }
            Some((_span, Token::Pkg)) => {
                tokens.expect(Token::Pkg)?;
                tokens.expect(Token::Period)?;
                let doc = parse_id(tokens)?;
                let mut clone = tokens.clone();
                let iface = if clone.eat(Token::Period)? && !clone.eat(Token::LeftBrace)? {
                    tokens.expect(Token::Period)?;
                    Some(parse_id(tokens)?)
                } else {
                    None
                };
                Ok(UsePath::Package { doc, iface })
            }
            Some((_span, Token::Id | Token::ExplicitId)) => {
                let dep = parse_id(tokens)?;
                tokens.expect(Token::Period)?;
                let doc = parse_id(tokens)?;
                let mut clone = tokens.clone();
                let iface = if clone.eat(Token::Period)? && !clone.eat(Token::LeftBrace)? {
                    tokens.expect(Token::Period)?;
                    Some(parse_id(tokens)?)
                } else {
                    None
                };
                Ok(UsePath::Dependency { dep, doc, iface })
            }
            other => return Err(err_expected(tokens, "`self`, `pkg`, or identifier", other).into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Id<'a> {
    pub name: &'a str,
    pub span: Span,
}

impl<'a> From<&'a str> for Id<'a> {
    fn from(s: &'a str) -> Id<'a> {
        Id {
            name: s.into(),
            span: Span { start: 0, end: 0 },
        }
    }
}

#[derive(Default)]
pub struct Docs<'a> {
    docs: Vec<Cow<'a, str>>,
}

pub struct TypeDef<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    ty: Type<'a>,
}

enum Type<'a> {
    Bool,
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
    Float32,
    Float64,
    Char,
    String,
    Name(Id<'a>),
    List(Box<Type<'a>>),
    Record(Record<'a>),
    Flags(Flags<'a>),
    Variant(Variant<'a>),
    Tuple(Vec<Type<'a>>),
    Enum(Enum<'a>),
    Option(Box<Type<'a>>),
    Result(Result_<'a>),
    Future(Option<Box<Type<'a>>>),
    Stream(Stream<'a>),
    Union(Union<'a>),
}

struct Record<'a> {
    fields: Vec<Field<'a>>,
}

struct Field<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    ty: Type<'a>,
}

struct Flags<'a> {
    flags: Vec<Flag<'a>>,
}

struct Flag<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
}

struct Variant<'a> {
    span: Span,
    cases: Vec<Case<'a>>,
}

struct Case<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    ty: Option<Type<'a>>,
}

struct Enum<'a> {
    span: Span,
    cases: Vec<EnumCase<'a>>,
}

struct EnumCase<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
}

struct Result_<'a> {
    ok: Option<Box<Type<'a>>>,
    err: Option<Box<Type<'a>>>,
}

struct Stream<'a> {
    element: Option<Box<Type<'a>>>,
    end: Option<Box<Type<'a>>>,
}

pub struct Value<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    kind: ValueKind<'a>,
}

struct Union<'a> {
    span: Span,
    cases: Vec<UnionCase<'a>>,
}

struct UnionCase<'a> {
    docs: Docs<'a>,
    ty: Type<'a>,
}

type ParamList<'a> = Vec<(Id<'a>, Type<'a>)>;

enum ResultList<'a> {
    Named(ParamList<'a>),
    Anon(Type<'a>),
}

enum ValueKind<'a> {
    Func(Func<'a>),
}

pub struct Func<'a> {
    params: ParamList<'a>,
    results: ResultList<'a>,
}

impl<'a> Func<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<Func<'a>> {
        fn parse_params<'a>(tokens: &mut Tokenizer<'a>, left_paren: bool) -> Result<ParamList<'a>> {
            if left_paren {
                tokens.expect(Token::LeftParen)?;
            };
            parse_list_trailer(tokens, Token::RightParen, |_docs, tokens| {
                let name = parse_id(tokens)?;
                tokens.expect(Token::Colon)?;
                let ty = Type::parse(tokens)?;
                Ok((name, ty))
            })
        }

        tokens.expect(Token::Func)?;
        let params = parse_params(tokens, true)?;
        let results = if tokens.eat(Token::RArrow)? {
            // If we eat a '(', parse the remainder of the named
            // result types. Otherwise parse a single anonymous type.
            if tokens.eat(Token::LeftParen)? {
                let results = parse_params(tokens, false)?;
                ResultList::Named(results)
            } else {
                let ty = Type::parse(tokens)?;
                ResultList::Anon(ty)
            }
        } else {
            ResultList::Named(Vec::new())
        };
        Ok(Func { params, results })
    }
}

impl<'a> ValueKind<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<ValueKind<'a>> {
        Func::parse(tokens).map(ValueKind::Func)
    }
}

impl<'a> InterfaceItem<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<InterfaceItem<'a>> {
        match tokens.clone().next()? {
            Some((_span, Token::Type)) => TypeDef::parse(tokens, docs).map(InterfaceItem::TypeDef),
            Some((_span, Token::Flags)) => {
                TypeDef::parse_flags(tokens, docs).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Enum)) => {
                TypeDef::parse_enum(tokens, docs).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Variant)) => {
                TypeDef::parse_variant(tokens, docs).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Record)) => {
                TypeDef::parse_record(tokens, docs).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Union)) => {
                TypeDef::parse_union(tokens, docs).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Id)) | Some((_span, Token::ExplicitId)) => {
                Value::parse(tokens, docs).map(InterfaceItem::Value)
            }
            Some((_span, Token::Use)) => Use::parse(tokens).map(InterfaceItem::Use),
            other => Err(err_expected(tokens, "`type` or `func`", other).into()),
        }
    }
}

impl<'a> TypeDef<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        tokens.expect(Token::Type)?;
        let name = parse_id(tokens)?;
        tokens.expect(Token::Equals)?;
        let ty = Type::parse(tokens)?;
        Ok(TypeDef { docs, name, ty })
    }

    fn parse_flags(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        tokens.expect(Token::Flags)?;
        let name = parse_id(tokens)?;
        let ty = Type::Flags(Flags {
            flags: parse_list(
                tokens,
                Token::LeftBrace,
                Token::RightBrace,
                |docs, tokens| {
                    let name = parse_id(tokens)?;
                    Ok(Flag { docs, name })
                },
            )?,
        });
        Ok(TypeDef { docs, name, ty })
    }

    fn parse_record(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        tokens.expect(Token::Record)?;
        let name = parse_id(tokens)?;
        let ty = Type::Record(Record {
            fields: parse_list(
                tokens,
                Token::LeftBrace,
                Token::RightBrace,
                |docs, tokens| {
                    let name = parse_id(tokens)?;
                    tokens.expect(Token::Colon)?;
                    let ty = Type::parse(tokens)?;
                    Ok(Field { docs, name, ty })
                },
            )?,
        });
        Ok(TypeDef { docs, name, ty })
    }

    fn parse_variant(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        tokens.expect(Token::Variant)?;
        let name = parse_id(tokens)?;
        let ty = Type::Variant(Variant {
            span: name.span,
            cases: parse_list(
                tokens,
                Token::LeftBrace,
                Token::RightBrace,
                |docs, tokens| {
                    let name = parse_id(tokens)?;
                    let ty = if tokens.eat(Token::LeftParen)? {
                        let ty = Type::parse(tokens)?;
                        tokens.expect(Token::RightParen)?;
                        Some(ty)
                    } else {
                        None
                    };
                    Ok(Case { docs, name, ty })
                },
            )?,
        });
        Ok(TypeDef { docs, name, ty })
    }

    fn parse_union(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        tokens.expect(Token::Union)?;
        let name = parse_id(tokens)?;
        let ty = Type::Union(Union {
            span: name.span,
            cases: parse_list(
                tokens,
                Token::LeftBrace,
                Token::RightBrace,
                |docs, tokens| {
                    let ty = Type::parse(tokens)?;
                    Ok(UnionCase { docs, ty })
                },
            )?,
        });
        Ok(TypeDef { docs, name, ty })
    }

    fn parse_enum(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        tokens.expect(Token::Enum)?;
        let name = parse_id(tokens)?;
        let ty = Type::Enum(Enum {
            span: name.span,
            cases: parse_list(
                tokens,
                Token::LeftBrace,
                Token::RightBrace,
                |docs, tokens| {
                    let name = parse_id(tokens)?;
                    Ok(EnumCase { docs, name })
                },
            )?,
        });
        Ok(TypeDef { docs, name, ty })
    }
}

impl<'a> Value<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        let name = parse_id(tokens)?;
        tokens.expect(Token::Colon)?;
        let kind = ValueKind::parse(tokens)?;
        Ok(Value { docs, name, kind })
    }
}

fn parse_id<'a>(tokens: &mut Tokenizer<'a>) -> Result<Id<'a>> {
    match tokens.next()? {
        Some((span, Token::Id)) => Ok(Id {
            name: tokens.parse_id(span)?,
            span,
        }),
        Some((span, Token::ExplicitId)) => Ok(Id {
            name: tokens.parse_explicit_id(span)?,
            span,
        }),
        other => Err(err_expected(tokens, "an identifier or string", other).into()),
    }
}

fn parse_docs<'a>(tokens: &mut Tokenizer<'a>) -> Result<Docs<'a>> {
    let mut docs = Docs::default();
    let mut clone = tokens.clone();
    while let Some((span, token)) = clone.next_raw()? {
        match token {
            Token::Whitespace => {}
            Token::Comment => docs.docs.push(tokens.get_span(span).into()),
            _ => break,
        };
        *tokens = clone.clone();
    }
    Ok(docs)
}

impl<'a> Type<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<Self> {
        match tokens.next()? {
            Some((_span, Token::U8)) => Ok(Type::U8),
            Some((_span, Token::U16)) => Ok(Type::U16),
            Some((_span, Token::U32)) => Ok(Type::U32),
            Some((_span, Token::U64)) => Ok(Type::U64),
            Some((_span, Token::S8)) => Ok(Type::S8),
            Some((_span, Token::S16)) => Ok(Type::S16),
            Some((_span, Token::S32)) => Ok(Type::S32),
            Some((_span, Token::S64)) => Ok(Type::S64),
            Some((_span, Token::Float32)) => Ok(Type::Float32),
            Some((_span, Token::Float64)) => Ok(Type::Float64),
            Some((_span, Token::Char)) => Ok(Type::Char),

            // tuple<T, U, ...>
            Some((_span, Token::Tuple)) => {
                let types = parse_list(
                    tokens,
                    Token::LessThan,
                    Token::GreaterThan,
                    |_docs, tokens| Type::parse(tokens),
                )?;
                Ok(Type::Tuple(types))
            }

            Some((_span, Token::Bool)) => Ok(Type::Bool),
            Some((_span, Token::String_)) => Ok(Type::String),

            // list<T>
            Some((_span, Token::List)) => {
                tokens.expect(Token::LessThan)?;
                let ty = Type::parse(tokens)?;
                tokens.expect(Token::GreaterThan)?;
                Ok(Type::List(Box::new(ty)))
            }

            // option<T>
            Some((_span, Token::Option_)) => {
                tokens.expect(Token::LessThan)?;
                let ty = Type::parse(tokens)?;
                tokens.expect(Token::GreaterThan)?;
                Ok(Type::Option(Box::new(ty)))
            }

            // result<T, E>
            // result<_, E>
            // result<T>
            // result
            Some((_span, Token::Result_)) => {
                let mut ok = None;
                let mut err = None;

                if tokens.eat(Token::LessThan)? {
                    if tokens.eat(Token::Underscore)? {
                        tokens.expect(Token::Comma)?;
                        err = Some(Box::new(Type::parse(tokens)?));
                    } else {
                        ok = Some(Box::new(Type::parse(tokens)?));
                        if tokens.eat(Token::Comma)? {
                            err = Some(Box::new(Type::parse(tokens)?));
                        }
                    };
                    tokens.expect(Token::GreaterThan)?;
                };
                Ok(Type::Result(Result_ { ok, err }))
            }

            // future<T>
            // future
            Some((_span, Token::Future)) => {
                let mut ty = None;

                if tokens.eat(Token::LessThan)? {
                    ty = Some(Box::new(Type::parse(tokens)?));
                    tokens.expect(Token::GreaterThan)?;
                };
                Ok(Type::Future(ty))
            }

            // stream<T, Z>
            // stream<_, Z>
            // stream<T>
            // stream
            Some((_span, Token::Stream)) => {
                let mut element = None;
                let mut end = None;

                if tokens.eat(Token::LessThan)? {
                    if tokens.eat(Token::Underscore)? {
                        tokens.expect(Token::Comma)?;
                        end = Some(Box::new(Type::parse(tokens)?));
                    } else {
                        element = Some(Box::new(Type::parse(tokens)?));
                        if tokens.eat(Token::Comma)? {
                            end = Some(Box::new(Type::parse(tokens)?));
                        }
                    };
                    tokens.expect(Token::GreaterThan)?;
                };
                Ok(Type::Stream(Stream { element, end }))
            }

            // `foo`
            Some((span, Token::Id)) => Ok(Type::Name(Id {
                name: tokens.parse_id(span)?.into(),
                span,
            })),
            // `%foo`
            Some((span, Token::ExplicitId)) => Ok(Type::Name(Id {
                name: tokens.parse_explicit_id(span)?.into(),
                span,
            })),

            other => Err(err_expected(tokens, "a type", other).into()),
        }
    }
}

fn parse_list<'a, T>(
    tokens: &mut Tokenizer<'a>,
    start: Token,
    end: Token,
    parse: impl FnMut(Docs<'a>, &mut Tokenizer<'a>) -> Result<T>,
) -> Result<Vec<T>> {
    tokens.expect(start)?;
    parse_list_trailer(tokens, end, parse)
}

fn parse_list_trailer<'a, T>(
    tokens: &mut Tokenizer<'a>,
    end: Token,
    mut parse: impl FnMut(Docs<'a>, &mut Tokenizer<'a>) -> Result<T>,
) -> Result<Vec<T>> {
    let mut items = Vec::new();
    loop {
        // get docs before we skip them to try to eat the end token
        let docs = parse_docs(tokens)?;

        // if we found an end token then we're done
        if tokens.eat(end)? {
            break;
        }

        let item = parse(docs, tokens)?;
        items.push(item);

        // if there's no trailing comma then this is required to be the end,
        // otherwise we go through the loop to try to get another item
        if !tokens.eat(Token::Comma)? {
            tokens.expect(end)?;
            break;
        }
    }
    Ok(items)
}

fn err_expected(
    tokens: &Tokenizer<'_>,
    expected: &'static str,
    found: Option<(Span, Token)>,
) -> Error {
    match found {
        Some((span, token)) => Error {
            span,
            msg: format!("expected {}, found {}", expected, token.describe()),
        },
        None => Error {
            span: Span {
                start: u32::try_from(tokens.input().len()).unwrap(),
                end: u32::try_from(tokens.input().len()).unwrap(),
            },
            msg: format!("expected {}, found eof", expected),
        },
    }
}

/// A listing of source files which are used to get parsed into an
/// [`UnresolvedPackage`].
#[derive(Clone, Default)]
pub struct SourceMap {
    sources: Vec<Source>,
    offset: u32,
}

#[derive(Clone)]
struct Source {
    offset: u32,
    path: PathBuf,
    name: String,
    contents: String,
}

impl SourceMap {
    /// Creates a new empty source map.
    pub fn new() -> SourceMap {
        SourceMap::default()
    }

    /// Reads the file `path` on the filesystem and appends its contents to this
    /// [`SourceMap`].
    ///
    /// This method pushes a new document into the source map. The name of the
    /// document is derived from the filename of the `path` provided.
    pub fn push_file(&mut self, path: &Path) -> Result<()> {
        let contents = std::fs::read_to_string(path)
            .with_context(|| format!("failed to read file {path:?}"))?;
        let filename = match path.file_name().and_then(|s| s.to_str()) {
            Some(stem) => stem,
            None => bail!("no filename for {path:?}"),
        };
        let name = match filename.find('.') {
            Some(i) => &filename[..i],
            None => filename,
        };
        self.push(path, name, contents);
        Ok(())
    }

    /// Appends the given contents with the given path into this source map.
    ///
    /// Each path added to a [`SourceMap`] will become a document in the final
    /// package. The `path` provided is not read from the filesystem and is
    /// instead only used during error messages. The `name` provided is the name
    /// of the document within the WIT package and must be a valid WIT
    /// identifier.
    pub fn push(&mut self, path: &Path, name: &str, contents: impl Into<String>) {
        let mut contents = contents.into();
        if path.extension().and_then(|s| s.to_str()) == Some("md") {
            log::debug!("automatically unwrapping markdown container");
            contents = unwrap_md(&contents);
        }
        let new_offset = self.offset + u32::try_from(contents.len()).unwrap();
        self.sources.push(Source {
            offset: self.offset,
            path: path.to_path_buf(),
            contents,
            name: name.to_string(),
        });
        self.offset = new_offset;

        fn unwrap_md(contents: &str) -> String {
            use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag};

            let mut wit = String::new();
            let mut last_pos = 0;
            let mut in_wit_code_block = false;
            Parser::new_ext(contents, Options::empty())
                .into_offset_iter()
                .for_each(|(event, range)| match (event, range) {
                    (
                        Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed(
                            "wit",
                        )))),
                        _,
                    ) => {
                        in_wit_code_block = true;
                    }
                    (Event::Text(text), range) if in_wit_code_block => {
                        // Ensure that offsets are correct by inserting newlines to
                        // cover the Markdown content outside of wit code blocks.
                        for _ in contents[last_pos..range.start].lines() {
                            wit.push('\n');
                        }
                        wit.push_str(&text);
                        last_pos = range.end;
                    }
                    (
                        Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed("wit")))),
                        _,
                    ) => {
                        in_wit_code_block = false;
                    }
                    _ => {}
                });
            wit
        }
    }

    /// Parses the files added to this source map into an [`UnresolvedPackage`].
    ///
    /// All files previously added are considered documents of the package to be
    /// returned.
    pub fn parse(self, name: &str, url: Option<&str>) -> Result<UnresolvedPackage> {
        let mut doc = self.rewrite_error(|| {
            let mut resolver = Resolver::default();
            let mut srcs = self.sources.iter().collect::<Vec<_>>();
            srcs.sort_by_key(|src| &src.name);
            for src in srcs {
                let mut tokens = Tokenizer::new(&src.contents, src.offset)
                    .with_context(|| format!("failed to tokenize path: {}", src.path.display()))?;
                let ast = Ast::parse(&mut tokens)?;
                resolver.push(&src.name, ast).with_context(|| {
                    format!("failed to start resolving path: {}", src.path.display())
                })?;
            }
            resolver.resolve(name, url)
        })?;
        doc.source_map = self;
        Ok(doc)
    }

    pub(crate) fn rewrite_error<F, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce() -> Result<T>,
    {
        let err = match f() {
            Ok(t) => return Ok(t),
            Err(e) => e,
        };
        if let Some(parse) = err.downcast_ref::<Error>() {
            let msg = self.highlight_err(parse.span.start, Some(parse.span.end), parse);
            bail!("{msg}")
        }

        if let Some(lex) = err.downcast_ref::<lex::Error>() {
            let pos = match lex {
                lex::Error::Unexpected(at, _)
                | lex::Error::UnterminatedComment(at)
                | lex::Error::Wanted { at, .. }
                | lex::Error::InvalidCharInId(at, _)
                | lex::Error::IdPartEmpty(at)
                | lex::Error::InvalidEscape(at, _) => *at,
            };
            let msg = self.highlight_err(pos, None, lex);
            bail!("{msg}")
        }

        if let Some(sort) = err.downcast_ref::<toposort::Error>() {
            let span = match sort {
                toposort::Error::NonexistentDep { span, .. }
                | toposort::Error::Cycle { span, .. } => *span,
            };
            let msg = self.highlight_err(span.start, Some(span.end), sort);
            bail!("{msg}")
        }

        Err(err)
    }

    fn highlight_err(&self, start: u32, end: Option<u32>, err: impl fmt::Display) -> String {
        let i = match self.sources.binary_search_by_key(&start, |src| src.offset) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let src = &self.sources[i];
        let start = usize::try_from(start - src.offset).unwrap();
        let end = end.map(|end| usize::try_from(end - src.offset).unwrap());
        let (line, col) = linecol_in(start, &src.contents);
        let snippet = src.contents.lines().nth(line).unwrap_or("");
        let mut msg = format!(
            "\
{err}
     --> {file}:{line}:{col}
      |
 {line:4} | {snippet}
      | {marker:>0$}",
            col + 1,
            file = src.path.display(),
            line = line + 1,
            col = col + 1,
            marker = "^",
        );
        if let Some(end) = end {
            if let Some(s) = src.contents.get(start..end) {
                for _ in s.chars().skip(1) {
                    msg.push('-');
                }
            }
        }
        return msg;

        fn linecol_in(pos: usize, text: &str) -> (usize, usize) {
            let mut cur = 0;
            // Use split_terminator instead of lines so that if there is a `\r`,
            // it is included in the offset calculation. The `+1` values below
            // account for the `\n`.
            for (i, line) in text.split_terminator('\n').enumerate() {
                if cur + line.len() + 1 > pos {
                    return (i, pos - cur);
                }
                cur += line.len() + 1;
            }
            (text.lines().count(), 0)
        }
    }

    /// Returns an iterator over all filenames added to this source map.
    pub fn source_files(&self) -> impl Iterator<Item = &Path> {
        self.sources.iter().map(|src| src.path.as_path())
    }
}
