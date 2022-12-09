use anyhow::{bail, Context, Result};
use ast::lex::Tokenizer;
use id_arena::{Arena, Id};
use indexmap::IndexMap;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag};
use std::borrow::Cow;
use std::collections::HashSet;
use std::path::Path;

pub mod abi;
mod ast;
mod sizealign;
pub use sizealign::*;
mod merge;
pub use merge::*;

/// Checks if the given string is a legal identifier in wit.
pub fn validate_id(s: &str) -> Result<()> {
    ast::validate_id(0, s)?;
    Ok(())
}

fn unwrap_md(contents: &str) -> String {
    let mut wit = String::new();
    let mut last_pos = 0;
    let mut in_wit_code_block = false;
    Parser::new_ext(contents, Options::empty())
        .into_offset_iter()
        .for_each(|(event, range)| match (event, range) {
            (Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed("wit")))), _) => {
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
            (Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(CowStr::Borrowed("wit")))), _) => {
                in_wit_code_block = false;
            }
            _ => {}
        });
    wit
}

/// Represents the result of parsing a wit document.
#[derive(Default, Clone)]
pub struct Document {
    /// The worlds contained in the document.
    pub worlds: Arena<World>,
    /// The top-level interfaces contained in the document.
    pub interfaces: Arena<Interface>,
    /// All types in all interfaces
    pub types: Arena<TypeDef>,
}

pub type WorldId = Id<World>;
pub type InterfaceId = Id<Interface>;
pub type TypeId = Id<TypeDef>;

impl Document {
    /// Parses the given string as a wit document.
    ///
    /// The `path` argument is used for error reporting.
    pub fn parse(path: &Path, contents: &str) -> Result<Self> {
        Self::_parse(path, contents)
    }

    /// Parses the given string as a wit document.
    ///
    /// The `path` argument is used for error reporting.
    pub fn parse_file(path: &Path) -> Result<Self> {
        let contents = std::fs::read_to_string(path)
            .with_context(|| format!("failed to read file {path:?}"))?;
        Self::_parse(path, &contents)
    }

    // This will eventually grow a closure which is "go read some other file"
    // for `use` statements
    fn _parse(path: &Path, contents: &str) -> Result<Self> {
        // If we have a ".md" file, it's a wit file wrapped in a markdown file;
        // parse the markdown to extract the `wit` code blocks.
        let contents: Cow<'_, str> = if path.extension().and_then(|s| s.to_str()) == Some("md") {
            unwrap_md(contents).into()
        } else {
            contents.into()
        };

        Self::rewrite_error(path, &contents, || {
            let mut lexer = Tokenizer::new(&contents)?;
            let ast = ast::Ast::parse(&mut lexer)?;
            ast::Resolver::default().resolve(&ast)
        })
    }

    /// Converts the document into a single world definition.
    ///
    /// Returns an error if there were no worlds defined in the document or
    /// if there were multiple worlds defined.
    pub fn default_world(&self) -> Result<WorldId> {
        match self.worlds.len() {
            0 => bail!("no worlds were defined in the document"),
            1 => Ok(self.worlds.iter().next().unwrap().0),
            _ => bail!("more than one world was defined in the document",),
        }
    }

    /// Converts the document into a single interface definition.
    ///
    /// Returns an error if there were no worlds defined in the document or
    /// if there were multiple worlds defined.
    pub fn default_interface(&self) -> Result<InterfaceId> {
        if self.worlds.len() > 0 {
            bail!("a world may not be defined in an interface definition");
        }

        match self.interfaces.len() {
            0 => bail!("no interfaces were defined in the document"),
            1 => Ok(self.interfaces.iter().next().unwrap().0),
            _ => bail!("more than one interface was defined in the document"),
        }
    }

    fn rewrite_error<F, T>(path: &Path, contents: &str, f: F) -> Result<T>
    where
        F: FnOnce() -> Result<T>,
    {
        match f() {
            Ok(t) => Ok(t),
            Err(mut e) => {
                let file = path.display().to_string();
                ast::rewrite_error(&mut e, &file, contents);
                Err(e)
            }
        }
    }

    pub fn topological_types(&self) -> Vec<TypeId> {
        let mut ret = Vec::new();
        let mut visited = HashSet::new();
        for (id, _) in self.types.iter() {
            self.topo_visit(id, &mut ret, &mut visited);
        }
        ret
    }

    fn topo_visit(&self, id: TypeId, list: &mut Vec<TypeId>, visited: &mut HashSet<TypeId>) {
        if !visited.insert(id) {
            return;
        }
        match &self.types[id].kind {
            TypeDefKind::Flags(_) | TypeDefKind::Enum(_) => {}
            TypeDefKind::Type(t) | TypeDefKind::List(t) => self.topo_visit_ty(t, list, visited),
            TypeDefKind::Record(r) => {
                for f in r.fields.iter() {
                    self.topo_visit_ty(&f.ty, list, visited);
                }
            }
            TypeDefKind::Tuple(t) => {
                for t in t.types.iter() {
                    self.topo_visit_ty(t, list, visited);
                }
            }
            TypeDefKind::Variant(v) => {
                for v in v.cases.iter() {
                    if let Some(ty) = v.ty {
                        self.topo_visit_ty(&ty, list, visited);
                    }
                }
            }
            TypeDefKind::Option(ty) => self.topo_visit_ty(ty, list, visited),
            TypeDefKind::Result(r) => {
                if let Some(ok) = r.ok {
                    self.topo_visit_ty(&ok, list, visited);
                }
                if let Some(err) = r.err {
                    self.topo_visit_ty(&err, list, visited);
                }
            }
            TypeDefKind::Union(u) => {
                for t in u.cases.iter() {
                    self.topo_visit_ty(&t.ty, list, visited);
                }
            }
            TypeDefKind::Future(ty) => {
                if let Some(ty) = ty {
                    self.topo_visit_ty(ty, list, visited);
                }
            }
            TypeDefKind::Stream(s) => {
                if let Some(element) = s.element {
                    self.topo_visit_ty(&element, list, visited);
                }
                if let Some(end) = s.end {
                    self.topo_visit_ty(&end, list, visited);
                }
            }
        }
        list.push(id);
    }

    fn topo_visit_ty(&self, ty: &Type, list: &mut Vec<TypeId>, visited: &mut HashSet<TypeId>) {
        if let Type::Id(id) = ty {
            self.topo_visit(*id, list, visited);
        }
    }

    pub fn all_bits_valid(&self, ty: &Type) -> bool {
        match ty {
            Type::U8
            | Type::S8
            | Type::U16
            | Type::S16
            | Type::U32
            | Type::S32
            | Type::U64
            | Type::S64
            | Type::Float32
            | Type::Float64 => true,

            Type::Bool | Type::Char | Type::String => false,

            Type::Id(id) => match &self.types[*id].kind {
                TypeDefKind::List(_)
                | TypeDefKind::Variant(_)
                | TypeDefKind::Enum(_)
                | TypeDefKind::Option(_)
                | TypeDefKind::Result(_)
                | TypeDefKind::Future(_)
                | TypeDefKind::Stream(_)
                | TypeDefKind::Union(_) => false,
                TypeDefKind::Type(t) => self.all_bits_valid(t),
                TypeDefKind::Record(r) => r.fields.iter().all(|f| self.all_bits_valid(&f.ty)),
                TypeDefKind::Tuple(t) => t.types.iter().all(|t| self.all_bits_valid(t)),

                // FIXME: this could perhaps be `true` for multiples-of-32 but
                // seems better to probably leave this as unconditionally
                // `false` for now, may want to reconsider later?
                TypeDefKind::Flags(_) => false,
            },
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct World {
    pub name: String,
    pub docs: Docs,
    pub default: Option<InterfaceId>,
    pub imports: IndexMap<String, InterfaceId>,
    pub exports: IndexMap<String, InterfaceId>,
}

impl World {
    /// Returns an iterator which visits all the exported interfaces, both named
    /// and default. The second entry in each pair the export name of the
    /// interface, or `None` if it's the default export interface.
    pub fn exports(&self) -> impl Iterator<Item = (InterfaceId, Option<&str>)> + '_ {
        self.exports
            .iter()
            .map(|(name, i)| (*i, Some(name.as_str())))
            .chain(self.default.iter().map(|i| (*i, None)))
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Interface {
    pub name: String,
    pub url: Option<String>,
    pub docs: Docs,
    pub types: Vec<TypeId>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub docs: Docs,
    pub kind: TypeDefKind,
    pub name: Option<String>,
    pub interface: Option<InterfaceId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefKind {
    Record(Record),
    Flags(Flags),
    Tuple(Tuple),
    Variant(Variant),
    Enum(Enum),
    Option(Type),
    Result(Result_),
    Union(Union),
    List(Type),
    Future(Option<Type>),
    Stream(Stream),
    Type(Type),
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum Type {
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
    Id(TypeId),
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Int {
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub docs: Docs,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Flags {
    pub flags: Vec<Flag>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Flag {
    pub docs: Docs,
    pub name: String,
}

#[derive(Debug)]
pub enum FlagsRepr {
    U8,
    U16,
    U32(usize),
}

impl Flags {
    pub fn repr(&self) -> FlagsRepr {
        match self.flags.len() {
            0 => FlagsRepr::U32(0),
            n if n <= 8 => FlagsRepr::U8,
            n if n <= 16 => FlagsRepr::U16,
            n => FlagsRepr::U32(sizealign::align_to(n, 32) / 32),
        }
    }
}

impl FlagsRepr {
    pub fn count(&self) -> usize {
        match self {
            FlagsRepr::U8 => 1,
            FlagsRepr::U16 => 1,
            FlagsRepr::U32(n) => *n,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub types: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub cases: Vec<Case>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub docs: Docs,
    pub name: String,
    pub ty: Option<Type>,
}

impl Variant {
    pub fn tag(&self) -> Int {
        match self.cases.len() {
            n if n <= u8::max_value() as usize => Int::U8,
            n if n <= u16::max_value() as usize => Int::U16,
            n if n <= u32::max_value() as usize => Int::U32,
            _ => panic!("too many cases to fit in a repr"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub cases: Vec<EnumCase>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumCase {
    pub docs: Docs,
    pub name: String,
}

impl Enum {
    pub fn tag(&self) -> Int {
        match self.cases.len() {
            n if n <= u8::max_value() as usize => Int::U8,
            n if n <= u16::max_value() as usize => Int::U16,
            n if n <= u32::max_value() as usize => Int::U32,
            _ => panic!("too many cases to fit in a repr"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Result_ {
    pub ok: Option<Type>,
    pub err: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    pub cases: Vec<UnionCase>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionCase {
    pub docs: Docs,
    pub ty: Type,
}

impl Union {
    pub fn tag(&self) -> Int {
        match self.cases.len() {
            n if n <= u8::max_value() as usize => Int::U8,
            n if n <= u16::max_value() as usize => Int::U16,
            n if n <= u32::max_value() as usize => Int::U32,
            _ => panic!("too many cases to fit in a repr"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stream {
    pub element: Option<Type>,
    pub end: Option<Type>,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Docs {
    pub contents: Option<String>,
}

pub type Params = Vec<(String, Type)>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Results {
    Named(Params),
    Anon(Type),
}

pub enum ResultsTypeIter<'a> {
    Named(std::slice::Iter<'a, (String, Type)>),
    Anon(std::iter::Once<&'a Type>),
}

impl<'a> Iterator for ResultsTypeIter<'a> {
    type Item = &'a Type;

    fn next(&mut self) -> Option<&'a Type> {
        match self {
            ResultsTypeIter::Named(ps) => ps.next().map(|p| &p.1),
            ResultsTypeIter::Anon(ty) => ty.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            ResultsTypeIter::Named(ps) => ps.size_hint(),
            ResultsTypeIter::Anon(ty) => ty.size_hint(),
        }
    }
}

impl<'a> ExactSizeIterator for ResultsTypeIter<'a> {}

impl Results {
    // For the common case of an empty results list.
    pub fn empty() -> Results {
        Results::Named(Vec::new())
    }

    pub fn len(&self) -> usize {
        match self {
            Results::Named(params) => params.len(),
            Results::Anon(_) => 1,
        }
    }

    pub fn throws<'a>(&self, doc: &'a Document) -> Option<(Option<&'a Type>, Option<&'a Type>)> {
        if self.len() != 1 {
            return None;
        }
        match self.iter_types().next().unwrap() {
            Type::Id(id) => match &doc.types[*id].kind {
                TypeDefKind::Result(r) => Some((r.ok.as_ref(), r.err.as_ref())),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn iter_types(&self) -> ResultsTypeIter {
        match self {
            Results::Named(ps) => ResultsTypeIter::Named(ps.iter()),
            Results::Anon(ty) => ResultsTypeIter::Anon(std::iter::once(ty)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub docs: Docs,
    pub name: String,
    pub kind: FunctionKind,
    pub params: Params,
    pub results: Results,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionKind {
    Freestanding,
}

impl Function {
    pub fn item_name(&self) -> &str {
        match &self.kind {
            FunctionKind::Freestanding => &self.name,
        }
    }

    /// Gets the core export name for this function.
    pub fn core_export_name<'a>(&'a self, interface: Option<&str>) -> Cow<'a, str> {
        match interface {
            Some(interface) => Cow::Owned(format!("{interface}#{}", self.name)),
            None => Cow::Borrowed(&self.name),
        }
    }
}
