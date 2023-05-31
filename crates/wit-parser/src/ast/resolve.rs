use super::{Error, ParamList, ResultList, ValueKind};
use crate::ast::toposort::toposort;
use crate::*;
use anyhow::{bail, Result};
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};
use std::mem;

#[derive(Default)]
pub struct Resolver<'a> {
    /// Current package named learned through the ASTs pushed onto this resolver.
    package_name: Option<PackageName>,

    /// All WIT files which are going to be resolved together.
    asts: Vec<ast::Ast<'a>>,

    // Arenas that get plumbed to the final `UnresolvedPackage`
    types: Arena<TypeDef>,
    interfaces: Arena<Interface>,
    worlds: Arena<World>,

    // Interning structure for types which-need-not-be-named such as
    // `list<string>` and such.
    anon_types: HashMap<Key, TypeId>,

    /// The index within `self.ast_items` that lookups should go through. This
    /// is updated as the ASTs are walked.
    cur_ast_index: usize,

    /// A map per `ast::Ast` which keeps track of the file's top level names in
    /// scope. This maps each name onto either a world or an interface, handling
    /// things like `use` at the top level.
    ast_items: Vec<IndexMap<&'a str, AstItem>>,

    /// A map for the entire package being created of all names defined within,
    /// along with the ID they're mapping to.
    package_items: IndexMap<&'a str, AstItem>,

    /// A per-interface map of name to item-in-the-interface. This is the same
    /// length as `self.types` and is pushed to whenever `self.types` is pushed
    /// to.
    interface_types: Vec<IndexMap<&'a str, (TypeOrItem, Span)>>,

    /// Metadata about foreign dependencies which are not defined in this
    /// package. This map is keyed by the name of the package being imported
    /// from. The next level of key is the name of the interface being imported
    /// from, and the final value is the assigned ID of the interface.
    foreign_deps: IndexMap<PackageName, IndexMap<&'a str, InterfaceId>>,

    /// All interfaces that are present within `self.foreign_deps`.
    foreign_interfaces: HashSet<InterfaceId>,

    /// The current type lookup scope which will eventually make its way into
    /// `self.interface_types`.
    type_lookup: IndexMap<&'a str, (TypeOrItem, Span)>,

    /// An assigned span for where all types inserted into `self.types` as
    /// imported from foreign interfaces. These types all show up first in the
    /// `self.types` arena and this span is used to generate an error message
    /// pointing to it if the item isn't actually defined.
    unknown_type_spans: Vec<Span>,

    /// Spans for each world in `self.world` to assign for each import/export
    /// for later error reporting.
    world_spans: Vec<(Vec<Span>, Vec<Span>)>,

    /// The span of each interface's definition which is used for error
    /// reporting during the final `Resolve` phase.
    interface_spans: Vec<Span>,

    /// Spans per entry in `self.foreign_deps` for where the dependency was
    /// introduced to print an error message if necessary.
    foreign_dep_spans: Vec<Span>,
}

#[derive(Debug, Copy, Clone)]
enum AstItem {
    Interface(InterfaceId),
    World(WorldId),
}

#[derive(PartialEq, Eq, Hash)]
enum Key {
    Variant(Vec<(String, Option<Type>)>),
    Handle(Handle),
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
    pub(crate) fn push(&mut self, ast: ast::Ast<'a>) -> Result<()> {
        // As each WIT file is pushed into this resolver keep track of the
        // current package name assigned. Only one file needs to mention it, but
        // if multiple mention it then they must all match.
        if let Some(cur) = &ast.package_id {
            let cur_name = cur.package_name();
            if let Some(prev) = &self.package_name {
                if cur_name != *prev {
                    bail!(Error {
                        span: cur.span,
                        msg: format!(
                            "package identifier `{cur_name}` does not match \
                             previous package name of `{prev}`"
                        ),
                    })
                }
            }
            self.package_name = Some(cur_name);
        }
        self.asts.push(ast);
        Ok(())
    }

    pub(crate) fn resolve(&mut self) -> Result<UnresolvedPackage> {
        // At least one of the WIT files must have a `package` annotation.
        let name = match &self.package_name {
            Some(name) => name.clone(),
            None => bail!("no `package` header was found in any WIT file for this package"),
        };

        // First populate information about foreign dependencies and the general
        // structure of the package. This should resolve the "base" of many
        // `use` statements and additionally generate a topological ordering of
        // all interfaces in the package to visit.
        let asts = mem::take(&mut self.asts);
        self.populate_foreign_interfaces(&asts);
        let order = self.populate_ast_items(&asts)?;
        self.populate_foreign_types(&asts)?;

        // Use the topological ordering of all interfaces to resolve all
        // interfaces in-order. Note that a reverse-mapping from ID to AST is
        // generated here to assist with this.
        let mut id_to_ast = IndexMap::new();
        for (i, ast) in asts.iter().enumerate() {
            for item in ast.items.iter() {
                if let ast::AstItem::Interface(interface) = item {
                    let id = match self.ast_items[i][interface.name.name] {
                        AstItem::Interface(id) => id,
                        AstItem::World(_) => unreachable!(),
                    };
                    id_to_ast.insert(id, (interface, i));
                }
            }
        }
        for id in order {
            let (interface, i) = &id_to_ast[&id];
            self.cur_ast_index = *i;
            self.resolve_interface(id, &interface.items, &interface.docs)?;
        }

        // With all interfaces out of the way the next order of business is to
        // take care of all the worlds. Worlds only depend on interfaces so no
        // topological ordering is necessary here.
        for (i, ast) in asts.iter().enumerate() {
            self.cur_ast_index = i;
            for item in ast.items.iter() {
                if let ast::AstItem::World(world) = item {
                    let id = match self.ast_items[i][world.name.name] {
                        AstItem::World(id) => id,
                        AstItem::Interface(_) => unreachable!(),
                    };
                    self.resolve_world(id, world)?;
                }
            }
        }

        Ok(UnresolvedPackage {
            name,
            worlds: mem::take(&mut self.worlds),
            types: mem::take(&mut self.types),
            interfaces: mem::take(&mut self.interfaces),
            foreign_deps: self
                .foreign_deps
                .iter()
                .map(|(name, deps)| {
                    (
                        name.clone(),
                        deps.iter()
                            .map(|(name, id)| (name.to_string(), *id))
                            .collect(),
                    )
                })
                .collect(),
            unknown_type_spans: mem::take(&mut self.unknown_type_spans),
            world_spans: mem::take(&mut self.world_spans),
            interface_spans: mem::take(&mut self.interface_spans),
            foreign_dep_spans: mem::take(&mut self.foreign_dep_spans),
            source_map: SourceMap::default(),
        })
    }

    /// Registers all foreign dependencies made within the ASTs provided.
    ///
    /// This will populate the `self.foreign_{deps,interfaces}` maps with all
    /// `UsePath::Package` entries.
    fn populate_foreign_interfaces(&mut self, asts: &[ast::Ast<'a>]) {
        let mut foreign_deps = mem::take(&mut self.foreign_deps);
        let mut foreign_interfaces = mem::take(&mut self.foreign_interfaces);
        for ast in asts {
            ast.for_each_path(|_, path, _names| {
                let (id, name) = match path {
                    ast::UsePath::Package { id, name } => (id, name),
                    _ => return Ok(()),
                };

                let deps = foreign_deps.entry(id.package_name()).or_insert_with(|| {
                    self.foreign_dep_spans.push(id.span);
                    IndexMap::new()
                });
                let id = *deps.entry(name.name).or_insert_with(|| {
                    log::trace!(
                        "creating an interface for foreign dep: {}/{}",
                        id.package_name(),
                        name.name
                    );
                    self.alloc_interface(name.span)
                });

                foreign_interfaces.insert(id);

                Ok(())
            })
            .unwrap();
        }
        self.foreign_deps = foreign_deps;
        self.foreign_interfaces = foreign_interfaces;
    }

    fn alloc_interface(&mut self, span: Span) -> InterfaceId {
        self.interface_types.push(IndexMap::new());
        self.interface_spans.push(span);
        self.interfaces.alloc(Interface {
            name: None,
            types: IndexMap::new(),
            docs: Docs::default(),
            functions: IndexMap::new(),
            package: None,
        })
    }

    /// This method will create a `World` and an `Interface` for all items
    /// present in the specified set of ASTs. Additionally maps for each AST are
    /// generated for resolving use-paths later on.
    fn populate_ast_items(&mut self, asts: &[ast::Ast<'a>]) -> Result<Vec<InterfaceId>> {
        let mut package_items = IndexMap::new();

        // Validate that all worlds and interfaces have unique names within this
        // package across all ASTs which make up the package.
        let mut ast_namespaces = Vec::new();
        let mut order = IndexMap::new();
        for ast in asts {
            let mut ast_ns = IndexMap::new();
            for item in ast.items.iter() {
                match item {
                    ast::AstItem::Interface(i) => {
                        if package_items.insert(i.name.name, i.name.span).is_some() {
                            bail!(Error {
                                span: i.name.span,
                                msg: format!("duplicate item named `{}`", i.name.name),
                            })
                        }
                        let prev = ast_ns.insert(i.name.name, ());
                        assert!(prev.is_none());
                        let prev = order.insert(i.name.name, Vec::new());
                        assert!(prev.is_none());
                    }
                    ast::AstItem::World(w) => {
                        if package_items.insert(w.name.name, w.name.span).is_some() {
                            bail!(Error {
                                span: w.name.span,
                                msg: format!("duplicate item named `{}", w.name.name),
                            })
                        }
                        let prev = ast_ns.insert(w.name.name, ());
                        assert!(prev.is_none());
                    }
                    // These are processed down below.
                    ast::AstItem::Use(_) => {}
                }
            }
            ast_namespaces.push(ast_ns);
        }

        // Next record dependencies between interfaces as induced via `use`
        // paths. This step is used to perform a topological sort of all
        // interfaces to ensure there are no cycles and to generate an ordering
        // which we can resolve in.
        enum ItemSource<'a> {
            Foreign,
            Local(ast::Id<'a>),
        }

        for ast in asts {
            // Record, in the context of this file, what all names are defined
            // at the top level and whether they point to other items in this
            // package or foreign items. Foreign deps are ignored for
            // topological ordering.
            let mut ast_ns = IndexMap::new();
            for item in ast.items.iter() {
                let (name, src) = match item {
                    ast::AstItem::Use(u) => {
                        let name = u.as_.as_ref().unwrap_or(u.item.name());
                        let src = match &u.item {
                            ast::UsePath::Id(id) => ItemSource::Local(id.clone()),
                            ast::UsePath::Package { .. } => ItemSource::Foreign,
                        };
                        (name, src)
                    }
                    ast::AstItem::Interface(i) => (&i.name, ItemSource::Local(i.name.clone())),
                    ast::AstItem::World(w) => (&w.name, ItemSource::Local(w.name.clone())),
                };
                if ast_ns.insert(name.name, (name.span, src)).is_some() {
                    bail!(Error {
                        span: name.span,
                        msg: format!("duplicate name `{}` in this file", name.name),
                    });
                }
            }

            // With this file's namespace information look at all `use` paths
            // and record dependencies between interfaces.
            ast.for_each_path(|iface, path, _names| {
                // If this import isn't contained within an interface then it's
                // in a world and it doesn't need to participate in our
                // topo-sort.
                let iface = match iface {
                    Some(name) => name,
                    None => return Ok(()),
                };
                let used_name = match path {
                    ast::UsePath::Id(id) => id,
                    ast::UsePath::Package { .. } => return Ok(()),
                };
                match ast_ns.get(used_name.name) {
                    Some((_, ItemSource::Foreign)) => return Ok(()),
                    Some((_, ItemSource::Local(id))) => {
                        order[iface.name].push(id.clone());
                    }
                    None => match package_items.get(used_name.name) {
                        Some(_) => {
                            order[iface.name].push(used_name.clone());
                        }
                        None => {
                            bail!(Error {
                                span: used_name.span,
                                msg: format!(
                                    "interface `{name}` not found in package",
                                    name = used_name.name
                                ),
                            })
                        }
                    },
                }
                Ok(())
            })?;
        }

        let order = toposort("interface", &order)?;

        // Allocate interfaces in-order now that the ordering is defined. This
        // is then used to build up internal maps for each AST which are stored
        // in `self.ast_items`.
        let mut interface_ids = IndexMap::new();
        let mut id_order = Vec::new();
        for name in order {
            let id = self.alloc_interface(package_items[name]);
            self.interfaces[id].name = Some(name.to_string());
            let prev = interface_ids.insert(name, id);
            assert!(prev.is_none());
            id_order.push(id);
        }
        for ast in asts {
            let mut items = IndexMap::new();
            for item in ast.items.iter() {
                let (name, ast_item) = match item {
                    ast::AstItem::Use(u) => {
                        let name = u.as_.as_ref().unwrap_or(u.item.name());
                        let item = match &u.item {
                            ast::UsePath::Id(name) => {
                                *interface_ids.get(name.name).ok_or_else(|| Error {
                                    span: name.span,
                                    msg: format!(
                                        "interface `{name}` does not exist",
                                        name = name.name
                                    ),
                                })?
                            }
                            ast::UsePath::Package { id, name } => {
                                self.foreign_deps[&id.package_name()][name.name]
                            }
                        };
                        (name.name, AstItem::Interface(item))
                    }
                    ast::AstItem::Interface(i) => {
                        (i.name.name, AstItem::Interface(interface_ids[i.name.name]))
                    }
                    ast::AstItem::World(w) => {
                        let id = self.worlds.alloc(World {
                            name: w.name.name.to_string(),
                            docs: Docs::default(),
                            exports: IndexMap::new(),
                            imports: IndexMap::new(),
                            package: None,
                        });
                        (w.name.name, AstItem::World(id))
                    }
                };
                let prev = items.insert(name, ast_item);
                assert!(prev.is_none());

                // Items defined via `use` don't go into the package namespace,
                // only the file namespace.
                if !matches!(item, ast::AstItem::Use(_)) {
                    let prev = self.package_items.insert(name, ast_item);
                    assert!(prev.is_none());
                }
            }
            self.ast_items.push(items);
        }
        Ok(id_order)
    }

    /// Generate a `Type::Unknown` entry for all types imported from foreign
    /// packages.
    ///
    /// This is done after all interfaces are generated so `self.resolve_path`
    /// can be used to determine if what's being imported from is a foreign
    /// interface or not.
    fn populate_foreign_types(&mut self, asts: &[ast::Ast<'a>]) -> Result<()> {
        for (i, ast) in asts.iter().enumerate() {
            self.cur_ast_index = i;
            ast.for_each_path(|_, path, names| {
                let names = match names {
                    Some(names) => names,
                    None => return Ok(()),
                };
                let iface = self.resolve_path(path)?;
                if !self.foreign_interfaces.contains(&iface) {
                    return Ok(());
                }

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
            })?;
        }
        Ok(())
    }

    fn resolve_world(&mut self, world_id: WorldId, world: &ast::World<'a>) -> Result<WorldId> {
        let docs = self.docs(&world.docs);
        self.worlds[world_id].docs = docs;

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
                        .insert(WorldKey::Name(name.to_string()), WorldItem::Type(id));
                    import_spans.push(*span);
                }
                TypeOrItem::Item(_) => unreachable!(),
            }
        }

        let mut imported_interfaces = HashSet::new();
        let mut exported_interfaces = HashSet::new();
        for item in world.items.iter() {
            let (docs, kind, desc, spans, interfaces) = match item {
                // handled in `resolve_types`
                ast::WorldItem::Use(_) | ast::WorldItem::Type(_) => continue,

                ast::WorldItem::Import(import) => (
                    &import.docs,
                    &import.kind,
                    "import",
                    &mut import_spans,
                    &mut imported_interfaces,
                ),
                ast::WorldItem::Export(export) => (
                    &export.docs,
                    &export.kind,
                    "export",
                    &mut export_spans,
                    &mut exported_interfaces,
                ),
            };
            let key = match kind {
                ast::ExternKind::Interface(name, _) | ast::ExternKind::Func(name, _) => {
                    let prev = used_names.insert(name.name, desc);
                    if let Some(prev) = prev {
                        return Err(Error {
                            span: kind.span(),
                            msg: format!(
                                "{desc} `{name}` conflicts with prior {prev} of same name",
                                name = name.name
                            ),
                        }
                        .into());
                    }
                    WorldKey::Name(name.name.to_string())
                }
                ast::ExternKind::Path(path) => WorldKey::Interface(self.resolve_path(path)?),
            };
            let world_item = self.resolve_world_item(docs, kind)?;
            if let WorldItem::Interface(id) = world_item {
                if !interfaces.insert(id) {
                    bail!(Error {
                        span: kind.span(),
                        msg: format!("interface cannot be {desc}ed more than once"),
                    })
                }
            }
            let dst = if desc == "import" {
                &mut self.worlds[world_id].imports
            } else {
                &mut self.worlds[world_id].exports
            };
            let prev = dst.insert(key, world_item);
            assert!(prev.is_none());
            spans.push(kind.span());
        }
        self.world_spans.push((import_spans, export_spans));
        self.type_lookup.clear();

        Ok(world_id)
    }

    fn resolve_world_item(
        &mut self,
        docs: &ast::Docs<'a>,
        kind: &ast::ExternKind<'a>,
    ) -> Result<WorldItem> {
        match kind {
            ast::ExternKind::Interface(name, items) => {
                let prev = mem::take(&mut self.type_lookup);
                let id = self.alloc_interface(name.span);
                self.resolve_interface(id, items, docs)?;
                self.type_lookup = prev;
                Ok(WorldItem::Interface(id))
            }
            ast::ExternKind::Path(path) => {
                let id = self.resolve_path(path)?;
                Ok(WorldItem::Interface(id))
            }
            ast::ExternKind::Func(name, func) => {
                let func = self.resolve_function(docs, name.name, func)?;
                Ok(WorldItem::Function(func))
            }
        }
    }

    fn resolve_interface(
        &mut self,
        interface_id: InterfaceId,
        fields: &[ast::InterfaceItem<'a>],
        docs: &ast::Docs<'a>,
    ) -> Result<()> {
        let docs = self.docs(docs);
        self.interfaces[interface_id].docs = docs;

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
                ast::InterfaceItem::Value(value) => match &value.kind {
                    ValueKind::Func(func) => {
                        self.define_interface_name(&value.name, TypeOrItem::Item("function"))?;
                        let func = self.resolve_function(&value.docs, value.name.name, func)?;
                        let prev = self.interfaces[interface_id]
                            .functions
                            .insert(value.name.name.to_string(), func);
                        assert!(prev.is_none());
                    }
                    ValueKind::Static(_) => {
                        bail!("static functions are only supported in resources")
                    }
                },
                ast::InterfaceItem::Use(_) | ast::InterfaceItem::TypeDef(_) => {}
            }
        }

        let lookup = mem::take(&mut self.type_lookup);
        self.interface_types[interface_id.index()] = lookup;

        Ok(())
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

    fn resolve_function(
        &mut self,
        docs: &ast::Docs<'_>,
        name: &str,
        func: &ast::Func,
    ) -> Result<Function> {
        let docs = self.docs(docs);
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
            ast::UsePath::Id(id) => {
                let item = self.ast_items[self.cur_ast_index]
                    .get(id.name)
                    .or_else(|| self.package_items.get(id.name));
                match item {
                    Some(AstItem::Interface(id)) => Ok(*id),
                    Some(AstItem::World(_)) => {
                        bail!(Error {
                            span: id.span,
                            msg: format!(
                                "name `{}` is defined as a world, not an interface",
                                id.name
                            ),
                        })
                    }
                    None => {
                        bail!(Error {
                            span: id.span,
                            msg: format!("interface `{name}` does not exist", name = id.name),
                        })
                    }
                }
            }
            ast::UsePath::Package { id, name } => {
                Ok(self.foreign_deps[&id.package_name()][name.name])
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
            ast::Type::Handle(handle) => match handle {
                ast::Handle::Shared { ty } => {
                    let ty = self.resolve_type(ty)?;
                    TypeDefKind::Handle(Handle::Shared(ty))
                }
            },
            ast::Type::Resource(resource) => {
                let methods = resource
                    .methods
                    .iter()
                    .map(|value| {
                        let (func, kind) = match &value.kind {
                            ValueKind::Func(func) => (func, FunctionKind::Method),
                            ValueKind::Static(func) => (func, FunctionKind::Static),
                        };

                        let params = func
                            .params
                            .iter()
                            .map(|(id, ty)| (id.name.to_owned(), self.resolve_type(ty).unwrap()))
                            .collect();

                        let results = match &func.results {
                            ResultList::Named(results) => {
                                let results = results
                                    .into_iter()
                                    .map(|(id, ty)| {
                                        (id.name.to_owned(), self.resolve_type(&ty).unwrap())
                                    })
                                    .collect();
                                Results::Named(results)
                            }
                            ResultList::Anon(ty) => Results::Anon(self.resolve_type(&ty).unwrap()),
                        };

                        Ok(Function {
                            docs: self.docs(&value.docs),
                            name: value.name.name.to_string(),
                            kind: kind,
                            params: params,
                            results: results,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Resource(Resource { methods })
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
            TypeDefKind::Handle(h) => Key::Handle(*h),
            TypeDefKind::Resource(_) => unreachable!("anonymous resources aren't supported"),
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
        ast::Type::Handle(handle) => match handle {
            ast::Handle::Shared { ty } => collect_deps(ty, deps),
        },
        ast::Type::Resource(resource) => {
            for method in resource.methods.iter() {
                let func = match &method.kind {
                    ValueKind::Func(func) => func,
                    ValueKind::Static(func) => func,
                };

                for (_, ty) in func.params.iter() {
                    collect_deps(ty, deps);
                }

                match &func.results {
                    ResultList::Named(results) => {
                        for (_, ty) in results.iter() {
                            collect_deps(ty, deps);
                        }
                    }
                    ResultList::Anon(ty) => collect_deps(&ty, deps),
                }
            }
        }
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
