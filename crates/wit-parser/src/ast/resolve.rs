use super::{Error, ParamList, ResultList, ValueKind};
use crate::ast::toposort::toposort;
use crate::*;
use anyhow::{anyhow, bail, Result};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::mem;

#[derive(Default)]
pub struct Resolver<'a> {
    asts: IndexMap<&'a str, ast::Ast<'a>>,
    types: Arena<TypeDef>,
    anon_types: HashMap<Key, TypeId>,
    interfaces: Arena<Interface>,
    documents: Arena<Document>,
    worlds: Arena<World>,
    document_lookup: IndexMap<&'a str, DocumentId>,
    document_interfaces: Vec<IndexMap<&'a str, DocumentItem>>,
    interface_types: Vec<IndexMap<&'a str, (TypeOrItem, Span)>>,
    foreign_deps: IndexMap<&'a str, IndexMap<&'a str, DocumentId>>,
    type_lookup: IndexMap<&'a str, (TypeOrItem, Span)>,

    unknown_type_spans: Vec<Span>,
    world_spans: Vec<(Vec<Span>, Vec<Span>)>,
    document_spans: Vec<Span>,
    interface_spans: Vec<Span>,
    foreign_dep_spans: Vec<Span>,
}

#[derive(Copy, Clone)]
enum DocumentItem {
    Interface(InterfaceId),
    World(WorldId),
}

#[derive(PartialEq, Eq, Hash)]
enum Key {
    Variant(Vec<(String, Option<Type>)>),
    Record(Vec<(String, Type)>),
    Flags(Vec<String>),
    Tuple(Vec<Type>),
    Enum(Vec<String>),
    List(Type),
    Option(Type),
    Result(Option<Type>, Option<Type>),
    Union(Vec<Type>),
    Future(Option<Type>),
    Stream(Option<Type>, Option<Type>),
}

enum TypeItem<'a, 'b> {
    Use(&'b ast::Use<'a>),
    Def(&'b ast::TypeDef<'a>),
}

enum TypeOrItem {
    Type(TypeId),
    Item(&'static str),
}

impl<'a> Resolver<'a> {
    pub(crate) fn push(&mut self, name: &'a str, ast: ast::Ast<'a>) -> Result<()> {
        // Note that this specifically uses `map_err` instead of `with_context`
        // since the error returned from `validate_id` is an `Error` which has a
        // filename and a line number, but there's no filename or line number
        // associated with this error so we just want the string message.
        crate::validate_id(name)
            .map_err(|e| anyhow!("name of document isn't a valid WIT identifier `{name}`: {e}"))?;

        let prev = self.asts.insert(name, ast);
        if prev.is_some() {
            bail!("document `{name}` is defined more than once");
        }
        Ok(())
    }

    pub(crate) fn resolve(&mut self, name: &str, url: Option<&str>) -> Result<UnresolvedPackage> {
        self.populate_foreign_deps();

        // Determine the dependencies between documents in the current package
        // we're parsing to perform a topological sort and how to visit the
        // documents in order.
        let mut doc_deps = IndexMap::new();
        for (name, ast) in self.asts.iter() {
            let mut deps = Vec::new();
            ast.for_each_path(|_, path, _names| {
                let doc = match path {
                    ast::UsePath::Package { doc, iface: _ } => doc,
                    _ => return Ok(()),
                };
                // If this document imports from itself using `pkg` syntax
                // that's ok and skip this dependency to prevent it from
                // otherwise being flagged as cyclic.
                if doc.name == *name {
                    return Ok(());
                }
                deps.push(doc.clone());
                Ok(())
            })
            .unwrap();

            let prev = doc_deps.insert(*name, deps);
            assert!(prev.is_none());
        }

        let order = toposort("document", &doc_deps)?;
        log::debug!("toposort for documents is {order:?}");
        let mut asts = mem::take(&mut self.asts);
        for name in order {
            let ast = asts.remove(&name).unwrap();
            self.resolve_document(name, &ast)?;
        }

        Ok(UnresolvedPackage {
            name: name.to_string(),
            url: url.map(|s| s.to_string()),
            worlds: mem::take(&mut self.worlds),
            types: mem::take(&mut self.types),
            interfaces: mem::take(&mut self.interfaces),
            documents: mem::take(&mut self.documents),
            foreign_deps: self
                .foreign_deps
                .iter()
                .map(|(name, deps)| {
                    (
                        name.to_string(),
                        deps.iter()
                            .map(|(name, id)| (name.to_string(), *id))
                            .collect(),
                    )
                })
                .collect(),
            unknown_type_spans: mem::take(&mut self.unknown_type_spans),
            world_spans: mem::take(&mut self.world_spans),
            document_spans: mem::take(&mut self.document_spans),
            interface_spans: mem::take(&mut self.interface_spans),
            foreign_dep_spans: mem::take(&mut self.foreign_dep_spans),
            source_map: SourceMap::default(),
        })
    }

    /// Populate "unknown" for all types referenced from foreign packages as
    /// well as inserting interfaces for referenced interfaces.
    ///
    /// This will populate the initial set of documents/interfaces/types with
    /// everything referenced from foreign packages, or those through
    /// `UsePath::Dependency`. The items created here are extracted later via
    /// `resolve_path`.
    fn populate_foreign_deps(&mut self) {
        for (_, ast) in self.asts.iter() {
            ast.for_each_path(|_, path, names| {
                let (dep, doc, iface) = match path {
                    ast::UsePath::Dependency { dep, doc, iface } => (dep, doc, iface),
                    _ => return Ok(()),
                };

                let deps = self.foreign_deps.entry(dep.name).or_insert_with(|| {
                    self.foreign_dep_spans.push(dep.span);
                    IndexMap::new()
                });
                let doc_span = doc.span;
                let doc = *deps.entry(doc.name).or_insert_with(|| {
                    log::trace!("creating a document for foreign dep: {}", dep.name);
                    self.document_interfaces.push(IndexMap::new());
                    self.document_spans.push(doc_span);
                    self.documents.alloc(Document {
                        name: doc.name.to_string(),
                        default_interface: None,
                        default_world: None,
                        interfaces: IndexMap::new(),
                        worlds: IndexMap::new(),
                        package: None,
                    })
                });

                let iface = match iface {
                    Some(iface) => {
                        let item = self.document_interfaces[doc.index()]
                            .entry(iface.name)
                            .or_insert_with(|| {
                                self.interface_types.push(IndexMap::new());
                                self.interface_spans.push(iface.span);
                                let id = self.interfaces.alloc(Interface {
                                    name: Some(iface.name.to_string()),
                                    types: IndexMap::new(),
                                    docs: Docs::default(),
                                    document: doc,
                                    functions: IndexMap::new(),
                                });
                                DocumentItem::Interface(id)
                            });
                        match item {
                            DocumentItem::Interface(id) => *id,
                            _ => unreachable!(),
                        }
                    }
                    None => *self.documents[doc]
                        .default_interface
                        .get_or_insert_with(|| {
                            self.interface_types.push(IndexMap::new());
                            self.interface_spans.push(doc_span);
                            self.interfaces.alloc(Interface {
                                name: None,
                                types: IndexMap::new(),
                                docs: Docs::default(),
                                document: doc,
                                functions: IndexMap::new(),
                            })
                        }),
                };

                let names = match names {
                    Some(names) => names,
                    None => return Ok(()),
                };
                let lookup = &mut self.interface_types[iface.index()];
                for name in names {
                    // If this name has already been defined then use that prior
                    // definition, otherwise create a new type with an unknown
                    // representation and insert it into the various maps.
                    if lookup.contains_key(name.name.name) {
                        continue;
                    }
                    let id = self.types.alloc(TypeDef {
                        docs: Docs::default(),
                        kind: TypeDefKind::Unknown,
                        name: Some(name.name.name.to_string()),
                        owner: TypeOwner::Interface(iface),
                    });
                    self.unknown_type_spans.push(name.name.span);
                    lookup.insert(name.name.name, (TypeOrItem::Type(id), name.name.span));
                    self.interfaces[iface]
                        .types
                        .insert(name.name.name.to_string(), id);
                }

                Ok(())
            })
            .unwrap();
        }
    }

    fn resolve_document(&mut self, name: &'a str, ast: &ast::Ast<'a>) -> Result<DocumentId> {
        // Verify all top-level names in this document are unique, and save off
        // the name of the default interface for drawing topological
        // dependencies in a moment.
        let mut names = HashMap::new();
        let mut default_interface_name = None;
        let mut deps = IndexMap::new();
        for item in ast.items.iter() {
            let name = match item {
                ast::AstItem::Interface(i) => {
                    if i.default && default_interface_name.is_none() {
                        default_interface_name = Some(i.name.name);
                    }
                    &i.name
                }
                ast::AstItem::World(w) => &w.name,
            };
            deps.insert(name.name, Vec::new());
            if names.insert(name.name, item).is_some() {
                return Err(Error {
                    span: name.span,
                    msg: format!("name `{}` previously defined in document", name.name),
                }
                .into());
            }
        }

        // Walk all `UsePath` entries in this AST and record dependencies
        // between interfaces. These are dependencies specified via `use
        // self...` or via `use pkg.this-doc...`.
        ast.for_each_path(|iface, path, _names| {
            // If this import isn't contained within an interface then it's in a
            // world and it doesn't need to participate in our topo-sort.
            let iface = match iface {
                Some(name) => name,
                None => return Ok(()),
            };
            let deps = &mut deps[iface.name];
            match path {
                // Self-deps are what we're mostly interested in here.
                ast::UsePath::Self_(name) => deps.push(name.clone()),

                // Self-deps via the package happen when the `doc` name listed
                // is the same as the name of the document being defined.
                ast::UsePath::Package { doc, iface } => {
                    if doc.name != name {
                        return Ok(());
                    }
                    let name = match iface {
                        Some(name) => name.clone(),
                        None => {
                            let name = default_interface_name.ok_or_else(|| Error {
                                span: doc.span,
                                msg: format!("no `default` interface in document to use from"),
                            })?;
                            ast::Id {
                                span: doc.span,
                                name,
                            }
                        }
                    };
                    deps.push(name);
                }

                // Dependencies on other packages don't participate in this
                // topological ordering.
                ast::UsePath::Dependency { .. } => {}
            }
            Ok(())
        })?;
        let order = toposort("interface", &deps)?;
        log::debug!("toposort for interfaces in `{name}` is {order:?}");

        // Allocate a document ID and then start processing all of the
        // interfaces as defined by our `order` to insert new interfaces into
        // this.
        let document_id = self.documents.alloc(Document {
            name: name.to_string(),
            default_interface: None,
            default_world: None,
            interfaces: IndexMap::new(),
            worlds: IndexMap::new(),
            package: None,
        });
        self.document_interfaces.push(IndexMap::new());
        self.document_lookup.insert(name, document_id);

        let mut worlds = Vec::new();
        for name in order {
            let interface = match names.remove(&name).unwrap() {
                ast::AstItem::Interface(i) => i,
                ast::AstItem::World(world) => {
                    worlds.push((name, world));
                    continue;
                }
            };
            let id = self.resolve_interface(
                document_id,
                Some(interface.name.name),
                &interface.items,
                &interface.docs,
            )?;
            if interface.default {
                let prev = &mut self.documents[document_id].default_interface;
                if prev.is_some() {
                    return Err(Error {
                        span: interface.name.span,
                        msg: format!("cannot specify more than one `default` interface"),
                    }
                    .into());
                }
                *prev = Some(id);
            }
            let prev = self.documents[document_id]
                .interfaces
                .insert(name.to_string(), id);
            assert!(prev.is_none());
        }

        // After all interfaces are defined then process all worlds as all items
        // they import from should now be available.
        for (name, world) in worlds {
            let id = self.resolve_world(document_id, world)?;
            if world.default {
                let prev = &mut self.documents[document_id].default_world;
                if prev.is_some() {
                    return Err(Error {
                        span: world.name.span,
                        msg: format!("cannot specify more than one `default` world"),
                    }
                    .into());
                }
                *prev = Some(id);
            }
            let prev = self.documents[document_id]
                .worlds
                .insert(name.to_string(), id);
            assert!(prev.is_none());
        }

        Ok(document_id)
    }

    fn resolve_world(&mut self, document: DocumentId, world: &ast::World<'a>) -> Result<WorldId> {
        let docs = self.docs(&world.docs);
        let world_id = self.worlds.alloc(World {
            docs,
            document,
            name: world.name.name.to_string(),
            exports: IndexMap::new(),
            imports: IndexMap::new(),
        });
        self.document_interfaces[document.index()]
            .insert(world.name.name, DocumentItem::World(world_id));

        self.resolve_types(
            TypeOwner::World(world_id),
            world.items.iter().filter_map(|i| match i {
                ast::WorldItem::Use(u) => Some(TypeItem::Use(u)),
                ast::WorldItem::Type(t) => Some(TypeItem::Def(t)),
                ast::WorldItem::Import(_) | ast::WorldItem::Export(_) => None,
            }),
        )?;

        let mut export_spans = Vec::new();
        let mut import_spans = Vec::new();
        let mut used_names = HashMap::new();
        for (name, (item, span)) in self.type_lookup.iter() {
            match *item {
                TypeOrItem::Type(id) => {
                    let prev = used_names.insert(*name, "import");
                    if let Some(prev) = prev {
                        return Err(Error {
                            span: *span,
                            msg: format!(
                                "import `{name}` conflicts with prior {prev} of same name",
                            ),
                        }
                        .into());
                    }
                    self.worlds[world_id]
                        .imports
                        .insert(name.to_string(), WorldItem::Type(id));
                    import_spans.push(*span);
                }
                TypeOrItem::Item(_) => unreachable!(),
            }
        }

        let mut imported_interfaces = HashMap::new();
        let mut exported_interfaces = HashMap::new();
        for item in world.items.iter() {
            let (name, kind, desc, spans, interfaces) = match item {
                // handled in `resolve_types`
                ast::WorldItem::Use(_) | ast::WorldItem::Type(_) => continue,

                ast::WorldItem::Import(import) => (
                    &import.name,
                    &import.kind,
                    "import",
                    &mut import_spans,
                    &mut imported_interfaces,
                ),
                ast::WorldItem::Export(export) => (
                    &export.name,
                    &export.kind,
                    "export",
                    &mut export_spans,
                    &mut exported_interfaces,
                ),
            };
            let prev = used_names.insert(name.name, desc);
            if let Some(prev) = prev {
                return Err(Error {
                    span: name.span,
                    msg: format!(
                        "{desc} `{name}` conflicts with prior {prev} of same name",
                        name = name.name
                    ),
                }
                .into());
            }
            let world_item = self.resolve_world_item(name.name, document, kind)?;
            if let WorldItem::Interface(id) = world_item {
                if interfaces.insert(id, name.name).is_some() {
                    return Err(Error {
                        span: name.span,
                        msg: format!(
                            "interface `{name}` cannot be {desc}ed more than once",
                            name = name.name
                        ),
                    }
                    .into());
                }
            }
            let dst = if desc == "import" {
                &mut self.worlds[world_id].imports
            } else {
                &mut self.worlds[world_id].exports
            };
            let prev = dst.insert(name.name.to_string(), world_item);
            assert!(prev.is_none());
            spans.push(name.span);
        }
        self.world_spans.push((import_spans, export_spans));
        self.type_lookup.clear();

        Ok(world_id)
    }

    fn resolve_world_item(
        &mut self,
        name: &str,
        document: DocumentId,
        kind: &ast::ExternKind<'a>,
    ) -> Result<WorldItem> {
        match kind {
            ast::ExternKind::Interface(_span, items) => {
                let prev = mem::take(&mut self.type_lookup);
                let id = self.resolve_interface(document, None, items, &Default::default())?;
                self.type_lookup = prev;
                Ok(WorldItem::Interface(id))
            }
            ast::ExternKind::Path(path) => {
                let id = self.resolve_path(path)?;
                Ok(WorldItem::Interface(id))
            }
            ast::ExternKind::Func(func) => {
                let func = self.resolve_function(Docs::default(), name, func)?;
                Ok(WorldItem::Function(func))
            }
        }
    }

    fn resolve_interface(
        &mut self,
        document: DocumentId,
        name: Option<&'a str>,
        fields: &[ast::InterfaceItem<'a>],
        docs: &super::Docs<'a>,
    ) -> Result<InterfaceId> {
        let docs = self.docs(docs);
        let interface_id = self.interfaces.alloc(Interface {
            docs,
            document,
            name: name.map(|s| s.to_string()),
            functions: IndexMap::new(),
            types: IndexMap::new(),
        });
        if let Some(name) = name {
            self.document_interfaces[document.index()]
                .insert(name, DocumentItem::Interface(interface_id));
        }

        self.resolve_types(
            TypeOwner::Interface(interface_id),
            fields.iter().filter_map(|i| match i {
                ast::InterfaceItem::Use(u) => Some(TypeItem::Use(u)),
                ast::InterfaceItem::TypeDef(t) => Some(TypeItem::Def(t)),
                ast::InterfaceItem::Value(_) => None,
            }),
        )?;

        for (name, (ty, _)) in self.type_lookup.iter() {
            match *ty {
                TypeOrItem::Type(id) => {
                    self.interfaces[interface_id]
                        .types
                        .insert(name.to_string(), id);
                }
                TypeOrItem::Item(_) => unreachable!(),
            }
        }

        // Finally process all function definitions now that all types are
        // defined.
        for field in fields {
            match field {
                ast::InterfaceItem::Value(value) => {
                    let docs = self.docs(&value.docs);
                    match &value.kind {
                        ValueKind::Func(func) => {
                            self.define_interface_name(&value.name, TypeOrItem::Item("function"))?;
                            let func = self.resolve_function(docs, value.name.name, func)?;
                            let prev = self.interfaces[interface_id]
                                .functions
                                .insert(value.name.name.to_string(), func);
                            assert!(prev.is_none());
                        }
                    }
                }
                ast::InterfaceItem::Use(_) | ast::InterfaceItem::TypeDef(_) => {}
            }
        }

        let lookup = mem::take(&mut self.type_lookup);
        assert_eq!(interface_id.index(), self.interface_types.len());
        self.interface_types.push(lookup);

        Ok(interface_id)
    }

    fn resolve_types<'b>(
        &mut self,
        owner: TypeOwner,
        fields: impl Iterator<Item = TypeItem<'a, 'b>> + Clone,
    ) -> Result<()>
    where
        'a: 'b,
    {
        assert!(self.type_lookup.is_empty());

        // First, populate our namespace with `use` statements
        for field in fields.clone() {
            match field {
                TypeItem::Use(u) => {
                    self.resolve_use(owner, u)?;
                }
                TypeItem::Def(_) => {}
            }
        }

        // Next determine dependencies between types, perform a topological
        // sort, and then define all types. This will define types in a
        // topological fashion, forbid cycles, and weed out references to
        // undefined types all in one go.
        let mut type_deps = IndexMap::new();
        let mut type_defs = IndexMap::new();
        for field in fields {
            match field {
                TypeItem::Def(t) => {
                    let prev = type_defs.insert(t.name.name, Some(t));
                    if prev.is_some() {
                        return Err(Error {
                            span: t.name.span,
                            msg: format!("name `{}` is defined more than once", t.name.name),
                        }
                        .into());
                    }
                    let mut deps = Vec::new();
                    collect_deps(&t.ty, &mut deps);
                    type_deps.insert(t.name.name, deps);
                }
                TypeItem::Use(u) => {
                    for name in u.names.iter() {
                        let name = name.as_.as_ref().unwrap_or(&name.name);
                        type_deps.insert(name.name, Vec::new());
                        type_defs.insert(name.name, None);
                    }
                }
            }
        }
        let order = toposort("type", &type_deps)?;
        for ty in order {
            let def = match type_defs.remove(&ty).unwrap() {
                Some(def) => def,
                None => continue,
            };
            let docs = self.docs(&def.docs);
            let kind = self.resolve_type_def(&def.ty)?;
            let id = self.types.alloc(TypeDef {
                docs,
                kind,
                name: Some(def.name.name.to_string()),
                owner,
            });
            self.define_interface_name(&def.name, TypeOrItem::Type(id))?;
        }
        Ok(())
    }

    fn resolve_use(&mut self, owner: TypeOwner, u: &ast::Use<'a>) -> Result<()> {
        let use_from = self.resolve_path(&u.from)?;
        for name in u.names.iter() {
            let lookup = &self.interface_types[use_from.index()];
            let id = match lookup.get(name.name.name) {
                Some((TypeOrItem::Type(id), _)) => *id,
                Some((TypeOrItem::Item(s), _)) => {
                    bail!(Error {
                        span: name.name.span,
                        msg: format!("cannot import {s} `{}`", name.name.name),
                    })
                }
                None => bail!(Error {
                    span: name.name.span,
                    msg: format!("name `{}` is not defined", name.name.name),
                }),
            };
            let name = name.as_.as_ref().unwrap_or(&name.name);
            let id = self.types.alloc(TypeDef {
                docs: Docs::default(),
                kind: TypeDefKind::Type(Type::Id(id)),
                name: Some(name.name.to_string()),
                owner,
            });
            self.define_interface_name(name, TypeOrItem::Type(id))?;
        }
        Ok(())
    }

    fn resolve_function(&mut self, docs: Docs, name: &str, func: &ast::Func) -> Result<Function> {
        let params = self.resolve_params(&func.params)?;
        let results = self.resolve_results(&func.results)?;
        Ok(Function {
            docs,
            name: name.to_string(),
            kind: FunctionKind::Freestanding,
            params,
            results,
        })
    }

    fn resolve_path(&self, path: &ast::UsePath<'a>) -> Result<InterfaceId> {
        match path {
            ast::UsePath::Self_(iface) => {
                match self.document_interfaces.last().unwrap().get(iface.name) {
                    Some(DocumentItem::Interface(id)) => Ok(*id),
                    Some(DocumentItem::World(_)) => bail!(Error {
                        span: iface.span,
                        msg: format!(
                            "name `{}` is defined as a world, not an interface",
                            iface.name
                        ),
                    }),
                    None => bail!(Error {
                        span: iface.span,
                        msg: format!("interface does not exist"),
                    }),
                }
            }
            ast::UsePath::Package { doc, iface } => {
                let doc_id = self.document_lookup[doc.name];
                match iface {
                    // If `iface` was provided then it must have been previously
                    // processed
                    Some(id) => {
                        let lookup = &self.document_interfaces[doc_id.index()];
                        match lookup.get(id.name) {
                            Some(DocumentItem::Interface(id)) => Ok(*id),
                            Some(DocumentItem::World(_)) => Err(Error {
                                span: id.span,
                                msg: format!("cannot import from world `{}`", id.name),
                            }
                            .into()),
                            None => bail!(Error {
                                span: id.span,
                                msg: format!("interface does not exist"),
                            }),
                        }
                    }
                    None => self.documents[doc_id].default_interface.ok_or_else(|| {
                        Error {
                            span: doc.span,
                            msg: format!("document does not specify a default interface"),
                        }
                        .into()
                    }),
                }
            }
            ast::UsePath::Dependency { dep, doc, iface } => {
                let doc = self.foreign_deps[dep.name][doc.name];
                match iface {
                    Some(name) => match self.document_interfaces[doc.index()][name.name] {
                        DocumentItem::Interface(id) => Ok(id),
                        DocumentItem::World(_) => unreachable!(),
                    },
                    None => Ok(self.documents[doc].default_interface.unwrap()),
                }
            }
        }
    }

    fn define_interface_name(&mut self, name: &ast::Id<'a>, item: TypeOrItem) -> Result<()> {
        let prev = self.type_lookup.insert(name.name, (item, name.span));
        if prev.is_some() {
            Err(Error {
                span: name.span,
                msg: format!("name `{}` is defined more than once", name.name),
            }
            .into())
        } else {
            Ok(())
        }
    }

    fn resolve_type_def(&mut self, ty: &ast::Type<'_>) -> Result<TypeDefKind> {
        Ok(match ty {
            ast::Type::Bool => TypeDefKind::Type(Type::Bool),
            ast::Type::U8 => TypeDefKind::Type(Type::U8),
            ast::Type::U16 => TypeDefKind::Type(Type::U16),
            ast::Type::U32 => TypeDefKind::Type(Type::U32),
            ast::Type::U64 => TypeDefKind::Type(Type::U64),
            ast::Type::S8 => TypeDefKind::Type(Type::S8),
            ast::Type::S16 => TypeDefKind::Type(Type::S16),
            ast::Type::S32 => TypeDefKind::Type(Type::S32),
            ast::Type::S64 => TypeDefKind::Type(Type::S64),
            ast::Type::Float32 => TypeDefKind::Type(Type::Float32),
            ast::Type::Float64 => TypeDefKind::Type(Type::Float64),
            ast::Type::Char => TypeDefKind::Type(Type::Char),
            ast::Type::String => TypeDefKind::Type(Type::String),
            ast::Type::Name(name) => {
                let id = match self.type_lookup.get(name.name) {
                    Some((TypeOrItem::Type(id), _)) => *id,
                    Some((TypeOrItem::Item(s), _)) => bail!(Error {
                        span: name.span,
                        msg: format!("cannot use {s} `{name}` as a type", name = name.name),
                    }),
                    None => bail!(Error {
                        span: name.span,
                        msg: format!("name `{name}` is not defined", name = name.name),
                    }),
                };
                TypeDefKind::Type(Type::Id(id))
            }
            ast::Type::List(list) => {
                let ty = self.resolve_type(list)?;
                TypeDefKind::List(ty)
            }
            ast::Type::Record(record) => {
                let fields = record
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(Field {
                            docs: self.docs(&field.docs),
                            name: field.name.name.to_string(),
                            ty: self.resolve_type(&field.ty)?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Record(Record { fields })
            }
            ast::Type::Flags(flags) => {
                let flags = flags
                    .flags
                    .iter()
                    .map(|flag| Flag {
                        docs: self.docs(&flag.docs),
                        name: flag.name.name.to_string(),
                    })
                    .collect::<Vec<_>>();
                TypeDefKind::Flags(Flags { flags })
            }
            ast::Type::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|ty| self.resolve_type(ty))
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Tuple(Tuple { types })
            }
            ast::Type::Variant(variant) => {
                if variant.cases.is_empty() {
                    return Err(Error {
                        span: variant.span,
                        msg: "empty variant".to_string(),
                    }
                    .into());
                }
                let cases = variant
                    .cases
                    .iter()
                    .map(|case| {
                        Ok(Case {
                            docs: self.docs(&case.docs),
                            name: case.name.name.to_string(),
                            ty: self.resolve_optional_type(case.ty.as_ref())?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Variant(Variant { cases })
            }
            ast::Type::Enum(e) => {
                if e.cases.is_empty() {
                    return Err(Error {
                        span: e.span,
                        msg: "empty enum".to_string(),
                    }
                    .into());
                }
                let cases = e
                    .cases
                    .iter()
                    .map(|case| {
                        Ok(EnumCase {
                            docs: self.docs(&case.docs),
                            name: case.name.name.to_string(),
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Enum(Enum { cases })
            }
            ast::Type::Option(ty) => TypeDefKind::Option(self.resolve_type(ty)?),
            ast::Type::Result(r) => TypeDefKind::Result(Result_ {
                ok: self.resolve_optional_type(r.ok.as_deref())?,
                err: self.resolve_optional_type(r.err.as_deref())?,
            }),
            ast::Type::Union(e) => {
                if e.cases.is_empty() {
                    return Err(Error {
                        span: e.span,
                        msg: "empty union".to_string(),
                    }
                    .into());
                }
                let cases = e
                    .cases
                    .iter()
                    .map(|case| {
                        Ok(UnionCase {
                            docs: self.docs(&case.docs),
                            ty: self.resolve_type(&case.ty)?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Union(Union { cases })
            }
            ast::Type::Future(t) => TypeDefKind::Future(self.resolve_optional_type(t.as_deref())?),
            ast::Type::Stream(s) => TypeDefKind::Stream(Stream {
                element: self.resolve_optional_type(s.element.as_deref())?,
                end: self.resolve_optional_type(s.end.as_deref())?,
            }),
        })
    }

    fn resolve_type(&mut self, ty: &super::Type<'_>) -> Result<Type> {
        let kind = self.resolve_type_def(ty)?;
        Ok(self.anon_type_def(TypeDef {
            kind,
            name: None,
            docs: Docs::default(),
            owner: TypeOwner::None,
        }))
    }

    fn resolve_optional_type(&mut self, ty: Option<&super::Type<'_>>) -> Result<Option<Type>> {
        match ty {
            Some(ty) => Ok(Some(self.resolve_type(ty)?)),
            None => Ok(None),
        }
    }

    fn anon_type_def(&mut self, ty: TypeDef) -> Type {
        let key = match &ty.kind {
            TypeDefKind::Type(t) => return *t,
            TypeDefKind::Variant(v) => Key::Variant(
                v.cases
                    .iter()
                    .map(|case| (case.name.clone(), case.ty))
                    .collect::<Vec<_>>(),
            ),
            TypeDefKind::Record(r) => Key::Record(
                r.fields
                    .iter()
                    .map(|case| (case.name.clone(), case.ty))
                    .collect::<Vec<_>>(),
            ),
            TypeDefKind::Flags(r) => {
                Key::Flags(r.flags.iter().map(|f| f.name.clone()).collect::<Vec<_>>())
            }
            TypeDefKind::Tuple(t) => Key::Tuple(t.types.clone()),
            TypeDefKind::Enum(r) => {
                Key::Enum(r.cases.iter().map(|f| f.name.clone()).collect::<Vec<_>>())
            }
            TypeDefKind::List(ty) => Key::List(*ty),
            TypeDefKind::Option(t) => Key::Option(*t),
            TypeDefKind::Result(r) => Key::Result(r.ok, r.err),
            TypeDefKind::Union(u) => Key::Union(u.cases.iter().map(|c| c.ty).collect()),
            TypeDefKind::Future(ty) => Key::Future(*ty),
            TypeDefKind::Stream(s) => Key::Stream(s.element, s.end),
            TypeDefKind::Unknown => unreachable!(),
        };
        let types = &mut self.types;
        let id = self
            .anon_types
            .entry(key)
            .or_insert_with(|| types.alloc(ty));
        Type::Id(*id)
    }

    fn docs(&mut self, doc: &super::Docs<'_>) -> Docs {
        let mut docs = None;
        for doc in doc.docs.iter() {
            // Comments which are not doc-comments are silently ignored
            if let Some(doc) = doc.strip_prefix("///") {
                let docs = docs.get_or_insert_with(String::new);
                docs.push_str(doc.trim_start_matches('/').trim());
                docs.push('\n');
            } else if let Some(doc) = doc.strip_prefix("/*") {
                // We have to strip this before checking if this is a doc
                // comment to avoid breaking on empty block comments, `/**/`.
                let doc = doc.strip_suffix("*/").unwrap();

                if let Some(doc) = doc.strip_prefix('*') {
                    let docs = docs.get_or_insert_with(String::new);
                    for line in doc.lines() {
                        docs.push_str(line);
                        docs.push('\n');
                    }
                }
            }
        }
        Docs { contents: docs }
    }

    fn resolve_params(&mut self, params: &ParamList<'_>) -> Result<Params> {
        params
            .iter()
            .map(|(name, ty)| Ok((name.name.to_string(), self.resolve_type(ty)?)))
            .collect::<Result<_>>()
    }

    fn resolve_results(&mut self, results: &ResultList<'_>) -> Result<Results> {
        match results {
            ResultList::Named(rs) => Ok(Results::Named(self.resolve_params(rs)?)),
            ResultList::Anon(ty) => Ok(Results::Anon(self.resolve_type(ty)?)),
        }
    }
}

fn collect_deps<'a>(ty: &ast::Type<'a>, deps: &mut Vec<ast::Id<'a>>) {
    match ty {
        ast::Type::Bool
        | ast::Type::U8
        | ast::Type::U16
        | ast::Type::U32
        | ast::Type::U64
        | ast::Type::S8
        | ast::Type::S16
        | ast::Type::S32
        | ast::Type::S64
        | ast::Type::Float32
        | ast::Type::Float64
        | ast::Type::Char
        | ast::Type::String
        | ast::Type::Flags(_)
        | ast::Type::Enum(_) => {}
        ast::Type::Name(name) => deps.push(name.clone()),
        ast::Type::List(list) => collect_deps(list, deps),
        ast::Type::Record(record) => {
            for field in record.fields.iter() {
                collect_deps(&field.ty, deps);
            }
        }
        ast::Type::Tuple(types) => {
            for ty in types {
                collect_deps(ty, deps);
            }
        }
        ast::Type::Variant(variant) => {
            for case in variant.cases.iter() {
                if let Some(ty) = &case.ty {
                    collect_deps(ty, deps);
                }
            }
        }
        ast::Type::Option(ty) => collect_deps(ty, deps),
        ast::Type::Result(r) => {
            if let Some(ty) = &r.ok {
                collect_deps(ty, deps);
            }
            if let Some(ty) = &r.err {
                collect_deps(ty, deps);
            }
        }
        ast::Type::Union(e) => {
            for case in e.cases.iter() {
                collect_deps(&case.ty, deps);
            }
        }
        ast::Type::Future(t) => {
            if let Some(t) = t {
                collect_deps(t, deps)
            }
        }
        ast::Type::Stream(s) => {
            if let Some(t) = &s.element {
                collect_deps(t, deps);
            }
            if let Some(t) = &s.end {
                collect_deps(t, deps);
            }
        }
    }
}
