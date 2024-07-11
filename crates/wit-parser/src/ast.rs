use crate::{Error, UnresolvedPackage, UnresolvedPackageGroup};
use anyhow::{bail, Context, Result};
use lex::{Span, Token, Tokenizer};
use semver::Version;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;
use std::path::{Path, PathBuf};

pub mod lex;

pub use resolve::Resolver;
mod resolve;
pub mod toposort;

pub use lex::validate_id;

enum Ast<'a> {
    ExplicitPackages(Vec<ExplicitPackage<'a>>),
    PartialImplicitPackage(PartialImplicitPackage<'a>),
}

pub struct ExplicitPackage<'a> {
    package_id: PackageName<'a>,
    decl_list: DeclList<'a>,
}

pub struct PartialImplicitPackage<'a> {
    package_id: Option<PackageName<'a>>,
    decl_list: DeclList<'a>,
}

/// Stores all of the declarations in a package's scope. In AST terms, this
/// means everything except the `package` declaration that demarcates a package
/// scope. In the traditional implicit format, these are all of the declarations
/// non-`package` declarations in the file:
///
/// ```wit
/// package foo:name;
///
/// /* START DECL LIST */
/// // Some comment...
/// interface i {}
/// world w {}
/// /* END DECL LIST */
/// ```
///
/// In the explicit package style, a [`DeclList`] is everything inside of each
/// `package` element's brackets:
///
/// ```wit
/// package foo:name {
///   /* START FIRST DECL LIST */
///   // Some comment...
///   interface i {}
///   world w {}
///   /* END FIRST DECL LIST */
/// }
///
/// package bar:name {
///   /* START SECOND DECL LIST */
///   // Some comment...
///   interface i {}
///   world w {}
///   /* END SECOND DECL LIST */
/// }
/// ```
pub struct DeclList<'a> {
    items: Vec<AstItem<'a>>,
}

impl<'a> DeclList<'a> {
    fn parse_explicit_package_items(tokens: &mut Tokenizer<'a>) -> Result<DeclList<'a>> {
        let mut items = Vec::new();
        let mut docs = parse_docs(tokens)?;
        loop {
            if tokens.eat(Token::RightBrace)? {
                return Ok(DeclList { items });
            }
            items.push(AstItem::parse(tokens, docs)?);
            docs = parse_docs(tokens)?;
        }
    }

    fn parse_implicit_package_items(
        tokens: &mut Tokenizer<'a>,
        mut docs: Docs<'a>,
    ) -> Result<DeclList<'a>> {
        let mut items = Vec::new();
        while tokens.clone().next()?.is_some() {
            items.push(AstItem::parse(tokens, docs)?);
            docs = parse_docs(tokens)?;
        }
        Ok(DeclList { items })
    }
}

impl<'a> Ast<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<Self> {
        let mut maybe_package_id = None;
        let mut packages = Vec::new();
        let mut docs = parse_docs(tokens)?;
        loop {
            if tokens.clone().next()?.is_none() {
                break;
            }
            if !tokens.eat(Token::Package)? {
                if !packages.is_empty() {
                    bail!("WIT files cannot mix top-level explicit `package` declarations with other declaration kinds");
                }
                break;
            }

            let package_id = PackageName::parse(tokens, std::mem::take(&mut docs))?;
            if tokens.eat(Token::LeftBrace)? {
                packages.push(ExplicitPackage {
                    package_id: package_id,
                    decl_list: DeclList::parse_explicit_package_items(tokens)?,
                });
                docs = parse_docs(tokens)?;
            } else {
                maybe_package_id = Some(package_id);
                tokens.expect_semicolon()?;
                if !packages.is_empty() {
                    bail!("WIT files cannot mix top-level explicit `package` declarations with other declaration kinds");
                }
                docs = parse_docs(tokens)?;
                break;
            }
        }

        if packages.is_empty() {
            return Ok(Ast::PartialImplicitPackage(PartialImplicitPackage {
                package_id: maybe_package_id,
                decl_list: DeclList::parse_implicit_package_items(tokens, docs)?,
            }));
        }
        Ok(Ast::ExplicitPackages(packages))
    }
}

impl<'a> DeclList<'a> {
    fn for_each_path<'b>(
        &'b self,
        mut f: impl FnMut(
            Option<&'b Id<'a>>,
            &'b UsePath<'a>,
            Option<&'b [UseName<'a>]>,
            WorldOrInterface,
        ) -> Result<()>,
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
                            WorldItem::Use(u) => {
                                f(None, &u.from, Some(&u.names), WorldOrInterface::Interface)?
                            }
                            WorldItem::Include(i) => {
                                f(Some(&world.name), &i.from, None, WorldOrInterface::World)?
                            }
                            WorldItem::Type(_) => {}
                            WorldItem::Import(Import { kind, .. }) => imports.push(kind),
                            WorldItem::Export(Export { kind, .. }) => exports.push(kind),
                        }
                    }

                    let mut visit_kind = |kind: &'b ExternKind<'a>| match kind {
                        ExternKind::Interface(_, items) => {
                            for item in items {
                                match item {
                                    InterfaceItem::Use(u) => f(
                                        None,
                                        &u.from,
                                        Some(&u.names),
                                        WorldOrInterface::Interface,
                                    )?,
                                    _ => {}
                                }
                            }
                            Ok(())
                        }
                        ExternKind::Path(path) => f(None, path, None, WorldOrInterface::Interface),
                        ExternKind::Func(..) => Ok(()),
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
                            InterfaceItem::Use(u) => f(
                                Some(&i.name),
                                &u.from,
                                Some(&u.names),
                                WorldOrInterface::Interface,
                            )?,
                            _ => {}
                        }
                    }
                }
                AstItem::Use(u) => {
                    // At the top-level, we don't know if this is a world or an interface
                    // It is up to the resolver to decides how to handle this ambiguity.
                    f(None, &u.item, None, WorldOrInterface::Unknown)?;
                }
            }
        }
        Ok(())
    }
}

enum AstItem<'a> {
    Interface(Interface<'a>),
    World(World<'a>),
    Use(ToplevelUse<'a>),
}

impl<'a> AstItem<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        let attributes = Attribute::parse_list(tokens)?;
        match tokens.clone().next()? {
            Some((_span, Token::Interface)) => {
                Interface::parse(tokens, docs, attributes).map(Self::Interface)
            }
            Some((_span, Token::World)) => World::parse(tokens, docs, attributes).map(Self::World),
            Some((_span, Token::Use)) => ToplevelUse::parse(tokens, attributes).map(Self::Use),
            other => Err(err_expected(tokens, "`world`, `interface` or `use`", other).into()),
        }
    }
}

#[derive(Debug, Clone)]
struct PackageName<'a> {
    docs: Docs<'a>,
    span: Span,
    namespace: Id<'a>,
    name: Id<'a>,
    version: Option<(Span, Version)>,
}

impl<'a> PackageName<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, docs: Docs<'a>) -> Result<Self> {
        let namespace = parse_id(tokens)?;
        tokens.expect(Token::Colon)?;
        let name = parse_id(tokens)?;
        let version = parse_opt_version(tokens)?;
        Ok(PackageName {
            docs,
            span: Span {
                start: namespace.span.start,
                end: version
                    .as_ref()
                    .map(|(s, _)| s.end)
                    .unwrap_or(name.span.end),
            },
            namespace,
            name,
            version,
        })
    }

    fn package_name(&self) -> crate::PackageName {
        crate::PackageName {
            namespace: self.namespace.name.to_string(),
            name: self.name.name.to_string(),
            version: self.version.as_ref().map(|(_, v)| v.clone()),
        }
    }
}

struct ToplevelUse<'a> {
    span: Span,
    attributes: Vec<Attribute<'a>>,
    item: UsePath<'a>,
    as_: Option<Id<'a>>,
}

impl<'a> ToplevelUse<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, attributes: Vec<Attribute<'a>>) -> Result<Self> {
        let span = tokens.expect(Token::Use)?;
        let item = UsePath::parse(tokens)?;
        let as_ = if tokens.eat(Token::As)? {
            Some(parse_id(tokens)?)
        } else {
            None
        };
        tokens.expect_semicolon()?;
        Ok(ToplevelUse {
            span,
            attributes,
            item,
            as_,
        })
    }
}

struct World<'a> {
    docs: Docs<'a>,
    attributes: Vec<Attribute<'a>>,
    name: Id<'a>,
    items: Vec<WorldItem<'a>>,
}

impl<'a> World<'a> {
    fn parse(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
        tokens.expect(Token::World)?;
        let name = parse_id(tokens)?;
        let items = Self::parse_items(tokens)?;
        Ok(World {
            docs,
            attributes,
            name,
            items,
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
            let attributes = Attribute::parse_list(tokens)?;
            items.push(WorldItem::parse(tokens, docs, attributes)?);
        }
        Ok(items)
    }
}

enum WorldItem<'a> {
    Import(Import<'a>),
    Export(Export<'a>),
    Use(Use<'a>),
    Type(TypeDef<'a>),
    Include(Include<'a>),
}

impl<'a> WorldItem<'a> {
    fn parse(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<WorldItem<'a>> {
        match tokens.clone().next()? {
            Some((_span, Token::Import)) => {
                Import::parse(tokens, docs, attributes).map(WorldItem::Import)
            }
            Some((_span, Token::Export)) => {
                Export::parse(tokens, docs, attributes).map(WorldItem::Export)
            }
            Some((_span, Token::Use)) => Use::parse(tokens, attributes).map(WorldItem::Use),
            Some((_span, Token::Type)) => {
                TypeDef::parse(tokens, docs, attributes).map(WorldItem::Type)
            }
            Some((_span, Token::Flags)) => {
                TypeDef::parse_flags(tokens, docs, attributes).map(WorldItem::Type)
            }
            Some((_span, Token::Resource)) => {
                TypeDef::parse_resource(tokens, docs, attributes).map(WorldItem::Type)
            }
            Some((_span, Token::Record)) => {
                TypeDef::parse_record(tokens, docs, attributes).map(WorldItem::Type)
            }
            Some((_span, Token::Variant)) => {
                TypeDef::parse_variant(tokens, docs, attributes).map(WorldItem::Type)
            }
            Some((_span, Token::Enum)) => {
                TypeDef::parse_enum(tokens, docs, attributes).map(WorldItem::Type)
            }
            Some((_span, Token::Include)) => {
                Include::parse(tokens, attributes).map(WorldItem::Include)
            }
            other => Err(err_expected(
                tokens,
                "`import`, `export`, `include`, `use`, or type definition",
                other,
            )
            .into()),
        }
    }
}

struct Import<'a> {
    docs: Docs<'a>,
    attributes: Vec<Attribute<'a>>,
    kind: ExternKind<'a>,
}

impl<'a> Import<'a> {
    fn parse(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Import<'a>> {
        tokens.expect(Token::Import)?;
        let kind = ExternKind::parse(tokens)?;
        Ok(Import {
            docs,
            attributes,
            kind,
        })
    }
}

struct Export<'a> {
    docs: Docs<'a>,
    attributes: Vec<Attribute<'a>>,
    kind: ExternKind<'a>,
}

impl<'a> Export<'a> {
    fn parse(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Export<'a>> {
        tokens.expect(Token::Export)?;
        let kind = ExternKind::parse(tokens)?;
        Ok(Export {
            docs,
            attributes,
            kind,
        })
    }
}

enum ExternKind<'a> {
    Interface(Id<'a>, Vec<InterfaceItem<'a>>),
    Path(UsePath<'a>),
    Func(Id<'a>, Func<'a>),
}

impl<'a> ExternKind<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<ExternKind<'a>> {
        // Create a copy of the token stream to test out if this is a function
        // or an interface import. In those situations the token stream gets
        // reset to the state of the clone and we continue down those paths.
        //
        // If neither a function nor an interface appears here though then the
        // clone is thrown away and the original token stream is parsed for an
        // interface. This will redo the original ID parse and the original
        // colon parse, but that shouldn't be too too bad perf-wise.
        let mut clone = tokens.clone();
        let id = parse_id(&mut clone)?;
        if clone.eat(Token::Colon)? {
            // import foo: func(...)
            if clone.clone().eat(Token::Func)? {
                *tokens = clone;
                let ret = ExternKind::Func(id, Func::parse(tokens)?);
                tokens.expect_semicolon()?;
                return Ok(ret);
            }

            // import foo: interface { ... }
            if clone.eat(Token::Interface)? {
                *tokens = clone;
                return Ok(ExternKind::Interface(id, Interface::parse_items(tokens)?));
            }
        }

        // import foo
        // import foo/bar
        // import foo:bar/baz
        let ret = ExternKind::Path(UsePath::parse(tokens)?);
        tokens.expect_semicolon()?;
        Ok(ret)
    }

    fn span(&self) -> Span {
        match self {
            ExternKind::Interface(id, _) => id.span,
            ExternKind::Path(UsePath::Id(id)) => id.span,
            ExternKind::Path(UsePath::Package { name, .. }) => name.span,
            ExternKind::Func(id, _) => id.span,
        }
    }
}

struct Interface<'a> {
    docs: Docs<'a>,
    attributes: Vec<Attribute<'a>>,
    name: Id<'a>,
    items: Vec<InterfaceItem<'a>>,
}

impl<'a> Interface<'a> {
    fn parse(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
        tokens.expect(Token::Interface)?;
        let name = parse_id(tokens)?;
        let items = Self::parse_items(tokens)?;
        Ok(Interface {
            docs,
            attributes,
            name,
            items,
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
            let attributes = Attribute::parse_list(tokens)?;
            items.push(InterfaceItem::parse(tokens, docs, attributes)?);
        }
        Ok(items)
    }
}

#[derive(Debug)]
pub enum WorldOrInterface {
    World,
    Interface,
    Unknown,
}

enum InterfaceItem<'a> {
    TypeDef(TypeDef<'a>),
    Func(NamedFunc<'a>),
    Use(Use<'a>),
}

struct Use<'a> {
    attributes: Vec<Attribute<'a>>,
    from: UsePath<'a>,
    names: Vec<UseName<'a>>,
}

#[derive(Debug)]
enum UsePath<'a> {
    Id(Id<'a>),
    Package { id: PackageName<'a>, name: Id<'a> },
}

impl<'a> UsePath<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<Self> {
        let id = parse_id(tokens)?;
        if tokens.eat(Token::Colon)? {
            // `foo:bar/baz@1.0`
            let namespace = id;
            let pkg_name = parse_id(tokens)?;
            tokens.expect(Token::Slash)?;
            let name = parse_id(tokens)?;
            let version = parse_opt_version(tokens)?;
            Ok(UsePath::Package {
                id: PackageName {
                    docs: Default::default(),
                    span: Span {
                        start: namespace.span.start,
                        end: pkg_name.span.end,
                    },
                    namespace,
                    name: pkg_name,
                    version,
                },
                name,
            })
        } else {
            // `foo`
            Ok(UsePath::Id(id))
        }
    }

    fn name(&self) -> &Id<'a> {
        match self {
            UsePath::Id(id) => id,
            UsePath::Package { name, .. } => name,
        }
    }
}

struct UseName<'a> {
    name: Id<'a>,
    as_: Option<Id<'a>>,
}

impl<'a> Use<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, attributes: Vec<Attribute<'a>>) -> Result<Self> {
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
        tokens.expect_semicolon()?;
        Ok(Use {
            attributes,
            from,
            names,
        })
    }
}

struct Include<'a> {
    from: UsePath<'a>,
    attributes: Vec<Attribute<'a>>,
    names: Vec<IncludeName<'a>>,
}

struct IncludeName<'a> {
    name: Id<'a>,
    as_: Id<'a>,
}

impl<'a> Include<'a> {
    fn parse(tokens: &mut Tokenizer<'a>, attributes: Vec<Attribute<'a>>) -> Result<Self> {
        tokens.expect(Token::Include)?;
        let from = UsePath::parse(tokens)?;

        let names = if tokens.eat(Token::With)? {
            parse_list(
                tokens,
                Token::LeftBrace,
                Token::RightBrace,
                |_docs, tokens| {
                    let name = parse_id(tokens)?;
                    tokens.expect(Token::As)?;
                    let as_ = parse_id(tokens)?;
                    Ok(IncludeName { name, as_ })
                },
            )?
        } else {
            tokens.expect_semicolon()?;
            Vec::new()
        };

        Ok(Include {
            attributes,
            from,
            names,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Id<'a> {
    name: &'a str,
    span: Span,
}

impl<'a> From<&'a str> for Id<'a> {
    fn from(s: &'a str) -> Id<'a> {
        Id {
            name: s.into(),
            span: Span { start: 0, end: 0 },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Docs<'a> {
    docs: Vec<Cow<'a, str>>,
    span: Span,
}

impl<'a> Default for Docs<'a> {
    fn default() -> Self {
        Self {
            docs: Default::default(),
            span: Span { start: 0, end: 0 },
        }
    }
}

struct TypeDef<'a> {
    docs: Docs<'a>,
    attributes: Vec<Attribute<'a>>,
    name: Id<'a>,
    ty: Type<'a>,
}

enum Type<'a> {
    Bool(Span),
    U8(Span),
    U16(Span),
    U32(Span),
    U64(Span),
    S8(Span),
    S16(Span),
    S32(Span),
    S64(Span),
    F32(Span),
    F64(Span),
    Char(Span),
    String(Span),
    Name(Id<'a>),
    List(List<'a>),
    Handle(Handle<'a>),
    Resource(Resource<'a>),
    Record(Record<'a>),
    Flags(Flags<'a>),
    Variant(Variant<'a>),
    Tuple(Tuple<'a>),
    Enum(Enum<'a>),
    Option(Option_<'a>),
    Result(Result_<'a>),
    Future(Future<'a>),
    Stream(Stream<'a>),
}

enum Handle<'a> {
    Own { resource: Id<'a> },
    Borrow { resource: Id<'a> },
}

impl Handle<'_> {
    fn span(&self) -> Span {
        match self {
            Handle::Own { resource } | Handle::Borrow { resource } => resource.span,
        }
    }
}

struct Resource<'a> {
    span: Span,
    funcs: Vec<ResourceFunc<'a>>,
}

enum ResourceFunc<'a> {
    Method(NamedFunc<'a>),
    Static(NamedFunc<'a>),
    Constructor(NamedFunc<'a>),
}

impl<'a> ResourceFunc<'a> {
    fn parse(
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
        tokens: &mut Tokenizer<'a>,
    ) -> Result<Self> {
        match tokens.clone().next()? {
            Some((span, Token::Constructor)) => {
                tokens.expect(Token::Constructor)?;
                tokens.expect(Token::LeftParen)?;
                let params = parse_list_trailer(tokens, Token::RightParen, |_docs, tokens| {
                    let name = parse_id(tokens)?;
                    tokens.expect(Token::Colon)?;
                    let ty = Type::parse(tokens)?;
                    Ok((name, ty))
                })?;
                tokens.expect_semicolon()?;
                Ok(ResourceFunc::Constructor(NamedFunc {
                    docs,
                    attributes,
                    name: Id {
                        span,
                        name: "constructor",
                    },
                    func: Func {
                        span,
                        params,
                        results: ResultList::Named(Vec::new()),
                    },
                }))
            }
            Some((_span, Token::Id | Token::ExplicitId)) => {
                let name = parse_id(tokens)?;
                tokens.expect(Token::Colon)?;
                let ctor = if tokens.eat(Token::Static)? {
                    ResourceFunc::Static
                } else {
                    ResourceFunc::Method
                };
                let func = Func::parse(tokens)?;
                tokens.expect_semicolon()?;
                Ok(ctor(NamedFunc {
                    docs,
                    attributes,
                    name,
                    func,
                }))
            }
            other => Err(err_expected(tokens, "`constructor` or identifier", other).into()),
        }
    }

    fn named_func(&self) -> &NamedFunc<'a> {
        use ResourceFunc::*;
        match self {
            Method(f) | Static(f) | Constructor(f) => f,
        }
    }
}

struct Record<'a> {
    span: Span,
    fields: Vec<Field<'a>>,
}

struct Field<'a> {
    docs: Docs<'a>,
    name: Id<'a>,
    ty: Type<'a>,
}

struct Flags<'a> {
    span: Span,
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

struct Option_<'a> {
    span: Span,
    ty: Box<Type<'a>>,
}

struct List<'a> {
    span: Span,
    ty: Box<Type<'a>>,
}

struct Future<'a> {
    span: Span,
    ty: Option<Box<Type<'a>>>,
}

struct Tuple<'a> {
    span: Span,
    types: Vec<Type<'a>>,
}

struct Result_<'a> {
    span: Span,
    ok: Option<Box<Type<'a>>>,
    err: Option<Box<Type<'a>>>,
}

struct Stream<'a> {
    span: Span,
    element: Option<Box<Type<'a>>>,
    end: Option<Box<Type<'a>>>,
}

struct NamedFunc<'a> {
    docs: Docs<'a>,
    attributes: Vec<Attribute<'a>>,
    name: Id<'a>,
    func: Func<'a>,
}

type ParamList<'a> = Vec<(Id<'a>, Type<'a>)>;

enum ResultList<'a> {
    Named(ParamList<'a>),
    Anon(Type<'a>),
}

struct Func<'a> {
    span: Span,
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

        let span = tokens.expect(Token::Func)?;
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
        Ok(Func {
            span,
            params,
            results,
        })
    }
}

impl<'a> InterfaceItem<'a> {
    fn parse(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<InterfaceItem<'a>> {
        match tokens.clone().next()? {
            Some((_span, Token::Type)) => {
                TypeDef::parse(tokens, docs, attributes).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Flags)) => {
                TypeDef::parse_flags(tokens, docs, attributes).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Enum)) => {
                TypeDef::parse_enum(tokens, docs, attributes).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Variant)) => {
                TypeDef::parse_variant(tokens, docs, attributes).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Resource)) => {
                TypeDef::parse_resource(tokens, docs, attributes).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Record)) => {
                TypeDef::parse_record(tokens, docs, attributes).map(InterfaceItem::TypeDef)
            }
            Some((_span, Token::Id)) | Some((_span, Token::ExplicitId)) => {
                NamedFunc::parse(tokens, docs, attributes).map(InterfaceItem::Func)
            }
            Some((_span, Token::Use)) => Use::parse(tokens, attributes).map(InterfaceItem::Use),
            other => Err(err_expected(tokens, "`type`, `resource` or `func`", other).into()),
        }
    }
}

impl<'a> TypeDef<'a> {
    fn parse(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
        tokens.expect(Token::Type)?;
        let name = parse_id(tokens)?;
        tokens.expect(Token::Equals)?;
        let ty = Type::parse(tokens)?;
        tokens.expect_semicolon()?;
        Ok(TypeDef {
            docs,
            attributes,
            name,
            ty,
        })
    }

    fn parse_flags(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
        tokens.expect(Token::Flags)?;
        let name = parse_id(tokens)?;
        let ty = Type::Flags(Flags {
            span: name.span,
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
        Ok(TypeDef {
            docs,
            attributes,
            name,
            ty,
        })
    }

    fn parse_resource(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
        tokens.expect(Token::Resource)?;
        let name = parse_id(tokens)?;
        let mut funcs = Vec::new();
        if tokens.eat(Token::LeftBrace)? {
            while !tokens.eat(Token::RightBrace)? {
                let docs = parse_docs(tokens)?;
                let attributes = Attribute::parse_list(tokens)?;
                funcs.push(ResourceFunc::parse(docs, attributes, tokens)?);
            }
        } else {
            tokens.expect_semicolon()?;
        }
        let ty = Type::Resource(Resource {
            span: name.span,
            funcs,
        });
        Ok(TypeDef {
            docs,
            attributes,
            name,
            ty,
        })
    }

    fn parse_record(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
        tokens.expect(Token::Record)?;
        let name = parse_id(tokens)?;
        let ty = Type::Record(Record {
            span: name.span,
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
        Ok(TypeDef {
            docs,
            attributes,
            name,
            ty,
        })
    }

    fn parse_variant(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
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
        Ok(TypeDef {
            docs,
            attributes,
            name,
            ty,
        })
    }

    fn parse_enum(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
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
        Ok(TypeDef {
            docs,
            attributes,
            name,
            ty,
        })
    }
}

impl<'a> NamedFunc<'a> {
    fn parse(
        tokens: &mut Tokenizer<'a>,
        docs: Docs<'a>,
        attributes: Vec<Attribute<'a>>,
    ) -> Result<Self> {
        let name = parse_id(tokens)?;
        tokens.expect(Token::Colon)?;
        let func = Func::parse(tokens)?;
        tokens.expect_semicolon()?;
        Ok(NamedFunc {
            docs,
            attributes,
            name,
            func,
        })
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

fn parse_opt_version(tokens: &mut Tokenizer<'_>) -> Result<Option<(Span, Version)>> {
    if tokens.eat(Token::At)? {
        parse_version(tokens).map(Some)
    } else {
        Ok(None)
    }
}

fn parse_version(tokens: &mut Tokenizer<'_>) -> Result<(Span, Version)> {
    let start = tokens.expect(Token::Integer)?.start;
    tokens.expect(Token::Period)?;
    tokens.expect(Token::Integer)?;
    tokens.expect(Token::Period)?;
    let end = tokens.expect(Token::Integer)?.end;
    let mut span = Span { start, end };
    eat_ids(tokens, Token::Minus, &mut span)?;
    eat_ids(tokens, Token::Plus, &mut span)?;
    let string = tokens.get_span(span);
    let version = Version::parse(string).map_err(|e| Error::new(span, e.to_string()))?;
    return Ok((span, version));

    fn eat_ids(tokens: &mut Tokenizer<'_>, prefix: Token, end: &mut Span) -> Result<()> {
        if !tokens.eat(prefix)? {
            return Ok(());
        }
        loop {
            match tokens.next()? {
                Some((span, Token::Id)) | Some((span, Token::Integer)) => end.end = span.end,
                other => break Err(err_expected(tokens, "an id or integer", other).into()),
            }

            // If there's no trailing period, then this semver identifier is
            // done.
            let mut clone = tokens.clone();
            if !clone.eat(Token::Period)? {
                break Ok(());
            }

            // If there's more to the identifier, then eat the period for real
            // and continue
            if clone.eat(Token::Id)? || clone.eat(Token::Integer)? {
                tokens.eat(Token::Period)?;
                continue;
            }

            // Otherwise for something like `use foo:bar/baz@1.2.3+foo.{` stop
            // the parsing here.
            break Ok(());
        }
    }
}

fn parse_docs<'a>(tokens: &mut Tokenizer<'a>) -> Result<Docs<'a>> {
    let mut docs = Docs::default();
    let mut clone = tokens.clone();
    let mut started = false;
    while let Some((span, token)) = clone.next_raw()? {
        match token {
            Token::Whitespace => {}
            Token::Comment => {
                let comment = tokens.get_span(span);
                if !started {
                    docs.span.start = span.start;
                    started = true;
                }
                let trailing_ws = comment
                    .bytes()
                    .rev()
                    .take_while(|ch| ch.is_ascii_whitespace())
                    .count();
                docs.span.end = span.end - (trailing_ws as u32);
                docs.docs.push(comment.into());
            }
            _ => break,
        };
        *tokens = clone.clone();
    }
    Ok(docs)
}

impl<'a> Type<'a> {
    fn parse(tokens: &mut Tokenizer<'a>) -> Result<Self> {
        match tokens.next()? {
            Some((span, Token::U8)) => Ok(Type::U8(span)),
            Some((span, Token::U16)) => Ok(Type::U16(span)),
            Some((span, Token::U32)) => Ok(Type::U32(span)),
            Some((span, Token::U64)) => Ok(Type::U64(span)),
            Some((span, Token::S8)) => Ok(Type::S8(span)),
            Some((span, Token::S16)) => Ok(Type::S16(span)),
            Some((span, Token::S32)) => Ok(Type::S32(span)),
            Some((span, Token::S64)) => Ok(Type::S64(span)),
            Some((span, Token::F32)) => Ok(Type::F32(span)),
            Some((span, Token::F64)) => Ok(Type::F64(span)),
            Some((span, Token::Char)) => Ok(Type::Char(span)),

            // tuple<T, U, ...>
            Some((span, Token::Tuple)) => {
                let types = parse_list(
                    tokens,
                    Token::LessThan,
                    Token::GreaterThan,
                    |_docs, tokens| Type::parse(tokens),
                )?;
                Ok(Type::Tuple(Tuple { span, types }))
            }

            Some((span, Token::Bool)) => Ok(Type::Bool(span)),
            Some((span, Token::String_)) => Ok(Type::String(span)),

            // list<T>
            Some((span, Token::List)) => {
                tokens.expect(Token::LessThan)?;
                let ty = Type::parse(tokens)?;
                tokens.expect(Token::GreaterThan)?;
                Ok(Type::List(List {
                    span,
                    ty: Box::new(ty),
                }))
            }

            // option<T>
            Some((span, Token::Option_)) => {
                tokens.expect(Token::LessThan)?;
                let ty = Type::parse(tokens)?;
                tokens.expect(Token::GreaterThan)?;
                Ok(Type::Option(Option_ {
                    span,
                    ty: Box::new(ty),
                }))
            }

            // result<T, E>
            // result<_, E>
            // result<T>
            // result
            Some((span, Token::Result_)) => {
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
                Ok(Type::Result(Result_ { span, ok, err }))
            }

            // future<T>
            // future
            Some((span, Token::Future)) => {
                let mut ty = None;

                if tokens.eat(Token::LessThan)? {
                    ty = Some(Box::new(Type::parse(tokens)?));
                    tokens.expect(Token::GreaterThan)?;
                };
                Ok(Type::Future(Future { span, ty }))
            }

            // stream<T, Z>
            // stream<_, Z>
            // stream<T>
            // stream
            Some((span, Token::Stream)) => {
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
                Ok(Type::Stream(Stream { span, element, end }))
            }

            // own<T>
            Some((_span, Token::Own)) => {
                tokens.expect(Token::LessThan)?;
                let resource = parse_id(tokens)?;
                tokens.expect(Token::GreaterThan)?;
                Ok(Type::Handle(Handle::Own { resource }))
            }

            // borrow<T>
            Some((_span, Token::Borrow)) => {
                tokens.expect(Token::LessThan)?;
                let resource = parse_id(tokens)?;
                tokens.expect(Token::GreaterThan)?;
                Ok(Type::Handle(Handle::Borrow { resource }))
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

    fn span(&self) -> Span {
        match self {
            Type::Bool(span)
            | Type::U8(span)
            | Type::U16(span)
            | Type::U32(span)
            | Type::U64(span)
            | Type::S8(span)
            | Type::S16(span)
            | Type::S32(span)
            | Type::S64(span)
            | Type::F32(span)
            | Type::F64(span)
            | Type::Char(span)
            | Type::String(span) => *span,
            Type::Name(id) => id.span,
            Type::List(l) => l.span,
            Type::Handle(h) => h.span(),
            Type::Resource(r) => r.span,
            Type::Record(r) => r.span,
            Type::Flags(f) => f.span,
            Type::Variant(v) => v.span,
            Type::Tuple(t) => t.span,
            Type::Enum(e) => e.span,
            Type::Option(o) => o.span,
            Type::Result(r) => r.span,
            Type::Future(f) => f.span,
            Type::Stream(s) => s.span,
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
        Some((span, token)) => Error::new(
            span,
            format!("expected {}, found {}", expected, token.describe()),
        ),
        None => Error::new(
            tokens.eof_span(),
            format!("expected {}, found eof", expected),
        ),
    }
}

enum Attribute<'a> {
    Since {
        span: Span,
        version: Version,
        feature: Option<Id<'a>>,
    },
    Unstable {
        span: Span,
        feature: Id<'a>,
    },
}

impl<'a> Attribute<'a> {
    fn parse_list(tokens: &mut Tokenizer<'a>) -> Result<Vec<Attribute<'a>>> {
        let mut ret = Vec::new();
        while tokens.eat(Token::At)? {
            let id = parse_id(tokens)?;
            let attr = match id.name {
                "since" => {
                    tokens.eat(Token::LeftParen)?;
                    eat_id(tokens, "version")?;
                    tokens.eat(Token::Equals)?;
                    let (_span, version) = parse_version(tokens)?;
                    let feature = if tokens.eat(Token::Comma)? {
                        eat_id(tokens, "feature")?;
                        tokens.eat(Token::Equals)?;
                        Some(parse_id(tokens)?)
                    } else {
                        None
                    };
                    tokens.eat(Token::RightParen)?;
                    Attribute::Since {
                        span: id.span,
                        version,
                        feature,
                    }
                }
                "unstable" => {
                    tokens.eat(Token::LeftParen)?;
                    eat_id(tokens, "feature")?;
                    tokens.eat(Token::Equals)?;
                    let feature = parse_id(tokens)?;
                    tokens.eat(Token::RightParen)?;
                    Attribute::Unstable {
                        span: id.span,
                        feature,
                    }
                }
                other => {
                    bail!(Error::new(id.span, format!("unknown attribute `{other}`"),))
                }
            };
            ret.push(attr);
        }
        Ok(ret)
    }

    fn span(&self) -> Span {
        match self {
            Attribute::Since { span, .. } | Attribute::Unstable { span, .. } => *span,
        }
    }
}

fn eat_id(tokens: &mut Tokenizer<'_>, expected: &str) -> Result<Span> {
    let id = parse_id(tokens)?;
    if id.name != expected {
        bail!(Error::new(
            id.span,
            format!("expected `{expected}`, found `{}`", id.name),
        ));
    }
    Ok(id.span)
}

/// A listing of source files which are used to get parsed into an
/// [`UnresolvedPackage`].
#[derive(Clone, Default)]
pub struct SourceMap {
    sources: Vec<Source>,
    offset: u32,
    require_semicolons: Option<bool>,
    require_f32_f64: Option<bool>,
}

#[derive(Clone)]
struct Source {
    offset: u32,
    path: PathBuf,
    contents: String,
}

enum ResolverKind<'a> {
    Unknown,
    Explicit(Vec<UnresolvedPackage>),
    PartialImplicit(Resolver<'a>),
}

fn parse_package(
    unparsed_pkgs: Vec<ExplicitPackage>,
    src: &Source,
    explicit_pkg_names: &mut HashSet<crate::PackageName>,
    parsed_pkgs: &mut Vec<UnresolvedPackage>,
) -> Result<()> {
    for pkg in unparsed_pkgs {
        let mut resolver = Resolver::default();
        let pkg_name = pkg.package_id.package_name();
        let ingested = resolver
            .push_then_resolve(pkg)
            .with_context(|| format!("failed to start resolving path: {}", src.path.display()))?;

        match explicit_pkg_names.get(&pkg_name) {
            Some(_) => bail!(
                "colliding explicit package names, multiple packages named `{}`",
                pkg_name
            ),
            None => explicit_pkg_names.insert(pkg_name),
        };

        if let Some(unresolved) = ingested {
            parsed_pkgs.push(unresolved);
        }
    }
    Ok(())
}

impl SourceMap {
    /// Creates a new empty source map.
    pub fn new() -> SourceMap {
        SourceMap::default()
    }

    #[doc(hidden)] // NB: only here for a transitionary period
    pub fn set_require_semicolons(&mut self, enable: bool) {
        self.require_semicolons = Some(enable);
    }

    #[doc(hidden)] // NB: only here for a transitionary period
    pub fn set_require_f32_f64(&mut self, enable: bool) {
        self.require_f32_f64 = Some(enable);
    }

    /// Reads the file `path` on the filesystem and appends its contents to this
    /// [`SourceMap`].
    pub fn push_file(&mut self, path: &Path) -> Result<()> {
        let contents = std::fs::read_to_string(path)
            .with_context(|| format!("failed to read file {path:?}"))?;
        self.push(path, contents);
        Ok(())
    }

    /// Appends the given contents with the given path into this source map.
    ///
    /// The `path` provided is not read from the filesystem and is instead only
    /// used during error messages. Each file added to a [`SourceMap`] is
    /// used to create the final parsed package namely by unioning all the
    /// interfaces and worlds defined together. Note that each file has its own
    /// personal namespace, however, for top-level `use` and such.
    pub fn push(&mut self, path: &Path, contents: impl Into<String>) {
        let mut contents = contents.into();
        // Guarantee that there's at least one character in these contents by
        // appending a single newline to the end. This is excluded from
        // tokenization below so it's only here to ensure that spans which point
        // one byte beyond the end of a file (eof) point to the same original
        // file.
        contents.push('\n');
        let new_offset = self.offset + u32::try_from(contents.len()).unwrap();
        self.sources.push(Source {
            offset: self.offset,
            path: path.to_path_buf(),
            contents,
        });
        self.offset = new_offset;
    }

    /// Parses the files added to this source map into one or more [`UnresolvedPackage`]s.
    pub fn parse(self) -> Result<UnresolvedPackageGroup> {
        let mut resolver_kind = ResolverKind::Unknown;
        let parsed_pkgs = self.rewrite_error(|| {
            let mut explicit_pkg_names: HashSet<crate::PackageName> = HashSet::new();
            let mut srcs = self.sources.iter().collect::<Vec<_>>();
            srcs.sort_by_key(|src| &src.path);
            for src in srcs {
                let mut tokens = Tokenizer::new(
                    // chop off the forcibly appended `\n` character when
                    // passing through the source to get tokenized.
                    &src.contents[..src.contents.len() - 1],
                    src.offset,
                    self.require_semicolons,
                    self.require_f32_f64,
                )
                .with_context(|| format!("failed to tokenize path: {}", src.path.display()))?;

                match Ast::parse(&mut tokens)? {
                    Ast::ExplicitPackages(pkgs) => {
                        match &mut resolver_kind {
                            ResolverKind::Unknown => {
                                let mut parsed_pkgs = Vec::new();
                                parse_package(pkgs, src, &mut explicit_pkg_names, &mut parsed_pkgs)?;
                                resolver_kind = ResolverKind::Explicit(parsed_pkgs);
                            },
                            ResolverKind::Explicit(parsed_pkgs) => {
                                parse_package(pkgs, src, &mut explicit_pkg_names, parsed_pkgs)?;
                            },
                            ResolverKind::PartialImplicit(_) => bail!("WIT files cannot mix top-level explicit `package` declarations with other declaration kinds"),
                        }
                    }
                    Ast::PartialImplicitPackage(partial) => {
                        match &mut resolver_kind {
                            ResolverKind::Unknown => {
                                let mut resolver = Resolver::default();
                                resolver.push_partial(partial).with_context(|| {
                                    format!("failed to start resolving path: {}", src.path.display())
                                })?;
                                resolver_kind = ResolverKind::PartialImplicit(resolver);
                            },
                            ResolverKind::Explicit(_) => bail!("WIT files cannot mix top-level explicit `package` declarations with other declaration kinds"),
                            ResolverKind::PartialImplicit(resolver) => {
                                resolver.push_partial(partial).with_context(|| {
                                    format!("failed to start resolving path: {}", src.path.display())
                                })?;
                            }
                        }
                    }
                }
            }

            match resolver_kind {
                ResolverKind::Unknown => bail!("No WIT packages found in the supplied source"),
                ResolverKind::Explicit(pkgs) => Ok(pkgs),
                ResolverKind::PartialImplicit(mut resolver) => match resolver.resolve()? {
                    Some(pkg) => Ok(vec![pkg]),
                    None => bail!("No WIT packages found in the supplied source"),
                },
            }
        })?;

        Ok(UnresolvedPackageGroup {
            packages: parsed_pkgs,
            source_map: self,
        })
    }

    pub(crate) fn rewrite_error<F, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce() -> Result<T>,
    {
        let mut err = match f() {
            Ok(t) => return Ok(t),
            Err(e) => e,
        };
        if let Some(parse) = err.downcast_mut::<Error>() {
            if parse.highlighted.is_none() {
                let msg = self.highlight_err(parse.span.start, Some(parse.span.end), &parse.msg);
                parse.highlighted = Some(msg);
            }
            return Err(err);
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

pub enum ParsedUsePath {
    Name(String),
    Package(crate::PackageName, String),
}

pub fn parse_use_path(s: &str) -> Result<ParsedUsePath> {
    let mut tokens = Tokenizer::new(s, 0, Some(true), None)?;
    let path = UsePath::parse(&mut tokens)?;
    if tokens.next()?.is_some() {
        bail!("trailing tokens in path specifier");
    }
    Ok(match path {
        UsePath::Id(id) => ParsedUsePath::Name(id.name.to_string()),
        UsePath::Package { id, name } => {
            ParsedUsePath::Package(id.package_name(), name.name.to_string())
        }
    })
}
