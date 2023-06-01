use anyhow::{Context, Result};
use id_arena::{Arena, Id};
use indexmap::IndexMap;
use semver::Version;
use std::borrow::Cow;
use std::fmt;
use std::path::Path;

pub mod abi;
mod ast;
use ast::lex::Span;
pub use ast::SourceMap;
mod sizealign;
pub use sizealign::*;
mod resolve;
pub use resolve::{Package, PackageId, Remap, Resolve};
mod live;
pub use live::LiveTypes;

/// Checks if the given string is a legal identifier in wit.
pub fn validate_id(s: &str) -> Result<()> {
    ast::validate_id(0, s)?;
    Ok(())
}

pub type WorldId = Id<World>;
pub type InterfaceId = Id<Interface>;
pub type TypeId = Id<TypeDef>;

/// Representation of a parsed WIT package which has not resolved external
/// dependencies yet.
///
/// This representation has performed internal resolution of the WIT package
/// itself, ensuring that all references internally are valid and the WIT was
/// syntactically valid and such.
///
/// The fields of this structure represent a flat list of arrays unioned from
/// all documents within the WIT package. This means, for example, that all
/// types from all documents are located in `self.types`. The fields of each
/// item can help splitting back out into packages/interfaces/etc as necessary.
///
/// Note that an `UnresolvedPackage` cannot be queried in general about
/// information such as size or alignment as that would require resolution of
/// foreign dependencies. Translations such as to-binary additionally are not
/// supported on an `UnresolvedPackage` due to the lack of knowledge about the
/// foreign types. This is intended to be an intermediate state which can be
/// inspected by embedders, if necessary, before quickly transforming to a
/// [`Resolve`] to fully work with a WIT package.
///
/// After an [`UnresolvedPackage`] is parsed it can be fully resolved with
/// [`Resolve::push`]. During this operation a dependency map is specified which
/// will connect the `foreign_deps` field of this structure to packages
/// previously inserted within the [`Resolve`]. Embedders are responsible for
/// performing this resolution themselves.
#[derive(Clone)]
pub struct UnresolvedPackage {
    /// The namespace, name, and version information for this package.
    pub name: PackageName,

    /// All worlds from all documents within this package.
    ///
    /// Each world lists the document that it is from.
    pub worlds: Arena<World>,

    /// All interfaces from all documents within this package.
    ///
    /// Each interface lists the document that it is from. Interfaces are listed
    /// in topological order as well so iteration through this arena will only
    /// reference prior elements already visited when working with recursive
    /// references.
    pub interfaces: Arena<Interface>,

    /// All types from all documents within this package.
    ///
    /// Each type lists the interface or world that defined it, or nothing if
    /// it's an anonymous type. Types are listed in this arena in topological
    /// order to ensure that iteration through this arena will only reference
    /// other types transitively that are already iterated over.
    pub types: Arena<TypeDef>,

    /// All foreign dependencies that this package depends on.
    ///
    /// These foreign dependencies must be resolved to convert this unresolved
    /// package into a `Resolve`. The map here is keyed by the name of the
    /// foreign package that this depends on, and the sub-map is keyed by an
    /// interface name followed by the identifier within `self.interfaces`. The
    /// fields of `self.interfaces` describes the required types that are from
    /// each foreign interface.
    pub foreign_deps: IndexMap<PackageName, IndexMap<String, InterfaceId>>,

    unknown_type_spans: Vec<Span>,
    world_spans: Vec<(Vec<Span>, Vec<Span>)>,
    interface_spans: Vec<Span>,
    foreign_dep_spans: Vec<Span>,
    source_map: SourceMap,
}

/// A structure used to keep track of the name of a package, containing optional
/// information such as a namespace and version information.
///
/// This is directly encoded as an "ID" in the binary component representation
/// with an interfaced tacked on as well.
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct PackageName {
    /// A namespace such as `wasi` in `wasi:foo/bar`
    pub namespace: String,
    /// The kebab-name of this package, which is always specified.
    pub name: String,
    /// Optional major/minor version information.
    pub version: Option<Version>,
}

impl PackageName {
    /// Returns the ID that this package name would assign the `interface` name
    /// specified.
    pub fn interface_id(&self, interface: &str) -> String {
        let mut s = String::new();
        s.push_str(&format!("{}:{}/{interface}", self.namespace, self.name));
        if let Some(version) = &self.version {
            s.push_str(&format!("@{version}"));
        }
        s
    }
}

impl fmt::Display for PackageName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.namespace, self.name)?;
        if let Some(version) = &self.version {
            write!(f, "@{version}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Error {
    span: Span,
    msg: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.msg.fmt(f)
    }
}

impl std::error::Error for Error {}

impl UnresolvedPackage {
    /// Parses the given string as a wit document.
    ///
    /// The `path` argument is used for error reporting. The `contents` provided
    /// will not be able to use `pkg` use paths to other documents.
    pub fn parse(path: &Path, contents: &str) -> Result<Self> {
        let mut map = SourceMap::default();
        map.push(path, contents);
        map.parse()
    }

    /// Parse a WIT package at the provided path.
    ///
    /// The path provided is inferred whether it's a file or a directory. A file
    /// is parsed with [`UnresolvedPackage::parse_file`] and a directory is
    /// parsed with [`UnresolvedPackage::parse_dir`].
    pub fn parse_path(path: &Path) -> Result<Self> {
        if path.is_dir() {
            UnresolvedPackage::parse_dir(path)
        } else {
            UnresolvedPackage::parse_file(path)
        }
    }

    /// Parses a WIT package from the file provided.
    ///
    /// The WIT package returned will be a single-document package and will not
    /// be able to use `pkg` paths to other documents.
    pub fn parse_file(path: &Path) -> Result<Self> {
        let contents = std::fs::read_to_string(path)
            .with_context(|| format!("failed to read file {path:?}"))?;
        Self::parse(path, &contents)
    }

    /// Parses a WIT package from the directory provided.
    ///
    /// All files with the extension `*.wit` or `*.wit.md` will be loaded from
    /// `path` into the returned package.
    pub fn parse_dir(path: &Path) -> Result<Self> {
        let mut map = SourceMap::default();
        let cx = || format!("failed to read directory {path:?}");
        for entry in path.read_dir().with_context(&cx)? {
            let entry = entry.with_context(&cx)?;
            let path = entry.path();
            let ty = entry.file_type().with_context(&cx)?;
            if ty.is_dir() {
                continue;
            }
            if ty.is_symlink() {
                if path.is_dir() {
                    continue;
                }
            }
            let filename = match path.file_name().and_then(|s| s.to_str()) {
                Some(name) => name,
                None => continue,
            };
            if !filename.ends_with(".wit") && !filename.ends_with(".wit.md") {
                continue;
            }
            map.push_file(&path)?;
        }
        map.parse()
    }

    /// Returns an iterator over the list of source files that were read when
    /// parsing this package.
    pub fn source_files(&self) -> impl Iterator<Item = &Path> {
        self.source_map.source_files()
    }
}

#[derive(Debug, Clone)]
pub struct World {
    /// The WIT identifier name of this world.
    pub name: String,

    /// Documentation associated with this world declaration.
    pub docs: Docs,

    /// All imported items into this interface, both worlds and functions.
    pub imports: IndexMap<WorldKey, WorldItem>,

    /// All exported items from this interface, both worlds and functions.
    pub exports: IndexMap<WorldKey, WorldItem>,

    /// The package that owns this world.
    pub package: Option<PackageId>,
}

/// The key to the import/export maps of a world. Either a kebab-name or a
/// unique interface.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WorldKey {
    /// A kebab-name.
    Name(String),
    /// An interface which is assigned no kebab-name.
    Interface(InterfaceId),
}

impl WorldKey {
    /// Asserts that this is `WorldKey::Name` and returns the name.
    #[track_caller]
    pub fn unwrap_name(self) -> String {
        match self {
            WorldKey::Name(name) => name,
            WorldKey::Interface(_) => panic!("expected a name, found interface"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum WorldItem {
    /// An interface is being imported or exported from a world, indicating that
    /// it's a namespace of functions.
    Interface(InterfaceId),

    /// A function is being directly imported or exported from this world.
    Function(Function),

    /// A type is being exported from this world.
    ///
    /// Note that types are never imported into worlds at this time.
    Type(TypeId),
}

#[derive(Debug, Clone)]
pub struct Interface {
    /// Optionally listed name of this interface.
    ///
    /// This is `None` for inline interfaces in worlds.
    pub name: Option<String>,

    /// Documentation associated with this interface.
    pub docs: Docs,

    /// Exported types from this interface.
    ///
    /// Export names are listed within the types themselves. Note that the
    /// export name here matches the name listed in the `TypeDef`.
    pub types: IndexMap<String, TypeId>,

    /// Exported functions from this interface.
    pub functions: IndexMap<String, Function>,

    /// The package that owns this interface.
    pub package: Option<PackageId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub docs: Docs,
    pub kind: TypeDefKind,
    pub name: Option<String>,
    pub owner: TypeOwner,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefKind {
    Record(Record),
    Resource(Resource),
    Handle(Handle),
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

    /// This represents a type of unknown structure imported from a foreign
    /// interface.
    ///
    /// This variant is only used during the creation of `UnresolvedPackage` but
    /// by the time a `Resolve` is created then this will not exist.
    Unknown,
}

impl TypeDefKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            TypeDefKind::Record(_) => "record",
            TypeDefKind::Resource(_) => "resource",
            TypeDefKind::Handle(handle) => match handle {
                Handle::Shared(_) => "shared",
            },
            TypeDefKind::Flags(_) => "flags",
            TypeDefKind::Tuple(_) => "tuple",
            TypeDefKind::Variant(_) => "variant",
            TypeDefKind::Enum(_) => "enum",
            TypeDefKind::Option(_) => "option",
            TypeDefKind::Result(_) => "result",
            TypeDefKind::Union(_) => "union",
            TypeDefKind::List(_) => "list",
            TypeDefKind::Future(_) => "future",
            TypeDefKind::Stream(_) => "stream",
            TypeDefKind::Type(_) => "type",
            TypeDefKind::Unknown => "unknown",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeOwner {
    /// This type was defined within a `world` block.
    World(WorldId),
    /// This type was defined within an `interface` block.
    Interface(InterfaceId),
    /// This type wasn't inherently defined anywhere, such as a `list<T>`, which
    /// doesn't need an owner.
    None,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum Handle {
    Shared(Type),
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
pub struct Resource {
    pub methods: Vec<Function>,
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

#[derive(Clone, Default, Debug, PartialEq, Eq)]
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

    pub fn throws<'a>(&self, resolve: &'a Resolve) -> Option<(Option<&'a Type>, Option<&'a Type>)> {
        if self.len() != 1 {
            return None;
        }
        match self.iter_types().next().unwrap() {
            Type::Id(id) => match &resolve.types[*id].kind {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub docs: Docs,
    pub name: String,
    pub kind: FunctionKind,
    pub params: Params,
    pub results: Results,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionKind {
    Freestanding,
    Method,
    Static,
}

impl Function {
    pub fn item_name(&self) -> &str {
        match &self.kind {
            FunctionKind::Freestanding => &self.name,
            FunctionKind::Method => &self.name,
            FunctionKind::Static => &self.name,
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
