use crate::ast::lex::Span;
use crate::{
    Document, DocumentId, Error, Interface, InterfaceId, Results, Type, TypeDef, TypeDefKind,
    TypeId, TypeOwner, UnresolvedPackage, World, WorldId, WorldItem,
};
use anyhow::{bail, Context, Result};
use id_arena::{Arena, Id};
use indexmap::{IndexMap, IndexSet};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::mem;
use std::path::{Path, PathBuf};

/// Representation of a fully resolved set of WIT packages.
///
/// This structure contains a graph of WIT packages and all of their contents
/// merged together into the contained arenas. All items are sorted
/// topologically and everything here is fully resolved, so with a `Resolve` no
/// name lookups are necessary and instead everything is index-based.
///
/// Working with a WIT package requires inserting it into a `Resolve` to ensure
/// that all of its dependencies are satisfied. This will give the full picture
/// of that package's types and such.
///
/// Each item in a `Resolve` has a parent link to trace it back to the original
/// package as necessary.
#[derive(Default, Clone)]
pub struct Resolve {
    pub worlds: Arena<World>,
    pub interfaces: Arena<Interface>,
    pub types: Arena<TypeDef>,
    pub documents: Arena<Document>,
    pub packages: Arena<Package>,
}

#[derive(Clone)]
pub struct Package {
    /// Documents contained within this package, organized by name.
    pub documents: IndexMap<String, DocumentId>,
}

pub type PackageId = Id<Package>;

impl Resolve {
    /// Creates a new [`Resolve`] with no packages/items inside of it.
    pub fn new() -> Resolve {
        Resolve::default()
    }

    /// Parses the filesystem directory at `path` as a WIT package and returns
    /// the fully resolved [`PackageId`] as a result.
    ///
    /// This method is intended for testing and convenience for now and isn't
    /// the only way to push packages into this [`Resolve`]. This will
    /// interpret `path` as a directory where all `*.wit` files in that
    /// directory are members of the package.
    ///
    /// Dependenies referenced by the WIT package at `path` will be loaded from
    /// a `deps/$name` directory under `path` where `$name` is the name of the
    /// dependency loaded. If `deps/$name` does not exist then an error will be
    /// returned indicating that the dependency is not defined. All dependencies
    /// are listed in a flat namespace under `$path/deps` so they can refer to
    /// each other.
    pub fn push_dir(&mut self, path: &Path) -> Result<PackageId> {
        // Maintain a `to_parse` stack of packages that have yet to be parsed
        // along with an `enqueued` set of all the prior parsed packages and
        // packges enqueued to be parsed. These are then used to fill the
        // `packages` map with parsed, but unresolved, packages. The `pkg_deps`
        // map then tracks dependencies between packages.
        let mut to_parse = Vec::new();
        let mut enqueued = HashSet::new();
        let mut packages = IndexMap::new();
        let mut pkg_deps = IndexMap::new();
        to_parse.push(path.to_path_buf());
        enqueued.insert(path.to_path_buf());
        while let Some(pkg_root) = to_parse.pop() {
            let pkg = UnresolvedPackage::parse_dir(&pkg_root)
                .with_context(|| format!("failed to parse package at {path:?}"))?;

            let mut deps = Vec::new();
            pkg.source_map.rewrite_error(|| {
                for (i, (dep, _)) in pkg.foreign_deps.iter().enumerate() {
                    let path = path.join("deps").join(dep);
                    let span = pkg.foreign_dep_spans[i];
                    // If this is the first request to parse `path` then push it
                    // onto our stack, otherwise it's already there so skip it.
                    if enqueued.insert(path.clone()) {
                        if !path.is_dir() {
                            bail!(Error {
                                span,
                                msg: format!("dependency on `{dep}` doesn't exist at {path:?}"),
                            })
                        }
                        to_parse.push(path.clone());
                    }
                    deps.push((path, span));
                }
                Ok(())
            })?;

            let prev = packages.insert(pkg_root.clone(), pkg);
            assert!(prev.is_none());
            pkg_deps.insert(pkg_root, deps);
        }

        // Perform a simple topological sort which will bail out on cycles
        // and otherwise determine the order that packages must be added to
        // this `Resolve`.
        let mut order = IndexSet::new();
        let mut visiting = HashSet::new();
        for (dep, _) in pkg_deps.iter() {
            visit(dep, &pkg_deps, &packages, &mut order, &mut visiting)?;
        }

        // Using the topological ordering insert each package incrementally.
        // Additionally note that the last item visited here is the root
        // package, which is the one returned here.
        let mut package_ids = IndexMap::new();
        let mut last = None;
        for path in order {
            let pkg = packages.remove(path).unwrap();
            let mut deps = HashMap::new();
            for ((dep, _), (path, _span)) in pkg.foreign_deps.iter().zip(&pkg_deps[path]) {
                deps.insert(dep.clone(), package_ids[&**path]);
            }
            let pkgid = self.push(pkg, &deps)?;
            package_ids.insert(path, pkgid);
            last = Some(pkgid);
        }

        return Ok(last.unwrap());

        fn visit<'a>(
            path: &'a Path,
            deps: &'a IndexMap<PathBuf, Vec<(PathBuf, Span)>>,
            pkgs: &IndexMap<PathBuf, UnresolvedPackage>,
            order: &mut IndexSet<&'a Path>,
            visiting: &mut HashSet<&'a Path>,
        ) -> Result<()> {
            if order.contains(path) {
                return Ok(());
            }
            pkgs[path].source_map.rewrite_error(|| {
                for (dep, span) in deps[path].iter() {
                    if !visiting.insert(dep) {
                        bail!(Error {
                            span: *span,
                            msg: format!("package depends on itself"),
                        });
                    }
                    visit(dep, deps, pkgs, order, visiting)?;
                    assert!(visiting.remove(&**dep));
                }
                assert!(order.insert(path));
                Ok(())
            })
        }
    }

    /// Appends a new [`UnresolvedPackage`] to this [`Resolve`], creating a
    /// fully resolved package with no dangling references.
    ///
    /// The `deps` argument indicates that the named dependencies in
    /// `unresolved` to packages are resolved by the mapping specified.
    ///
    /// Any dependency resolution error or otherwise world-elaboration error
    /// will be returned here. If successful a package identifier is returned.
    pub fn push(
        &mut self,
        mut unresolved: UnresolvedPackage,
        deps: &HashMap<String, PackageId>,
    ) -> Result<PackageId> {
        let source_map = mem::take(&mut unresolved.source_map);
        source_map.rewrite_error(|| Remap::default().append(self, unresolved, deps))
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

                TypeDefKind::Unknown => unreachable!(),
            },
        }
    }
}

#[derive(Default)]
struct Remap {
    types: Vec<TypeId>,
    interfaces: Vec<InterfaceId>,
    documents: Vec<DocumentId>,
    worlds: Vec<WorldId>,
}

impl Remap {
    fn append(
        &mut self,
        resolve: &mut Resolve,
        unresolved: UnresolvedPackage,
        deps: &HashMap<String, PackageId>,
    ) -> Result<PackageId> {
        self.process_foreign_deps(resolve, &unresolved, deps)?;

        let foreign_types = self.types.len();
        let foreign_interfaces = self.interfaces.len();
        let foreign_documents = self.documents.len();
        let foreign_worlds = self.worlds.len();

        // Copy over all types first, updating any intra-type references. Note
        // that types are sorted topologically which means this iteration
        // order should be sufficient. Also note though that the interface
        // owner of a type isn't updated here due to interfaces not being known
        // yet.
        for (id, mut ty) in unresolved.types.into_iter().skip(foreign_types) {
            self.update_typedef(&mut ty);
            let new_id = resolve.types.alloc(ty);
            assert_eq!(self.types.len(), id.index());
            self.types.push(new_id);
        }

        // Next transfer all interfaces into `Resolve`, updating type ids
        // referenced along the way.
        for (id, mut iface) in unresolved.interfaces.into_iter().skip(foreign_interfaces) {
            self.update_interface(&mut iface);
            let new_id = resolve.interfaces.alloc(iface);
            assert_eq!(self.interfaces.len(), id.index());
            self.interfaces.push(new_id);
        }

        // Now that interfaces are identified go back through the types and
        // update their interface owners.
        for id in self.types.iter().skip(foreign_types) {
            match &mut resolve.types[*id].owner {
                TypeOwner::Interface(id) => *id = self.interfaces[id.index()],
                TypeOwner::World(_) => unimplemented!(),
                TypeOwner::None => {}
            }
        }

        // Perform a weighty step of full resolution of worlds. This will fully
        // expand imports/exports for a world and create the topological
        // ordering necessary for this.
        //
        // This is done after types/interfaces are fully settled so the
        // transitive relation between interfaces, through types, is understood
        // here.
        assert_eq!(unresolved.worlds.len(), unresolved.world_spans.len());
        for ((id, mut world), (import_spans, export_spans)) in unresolved
            .worlds
            .into_iter()
            .skip(foreign_worlds)
            .zip(unresolved.world_spans)
        {
            self.update_world(&mut world, resolve, &import_spans, &export_spans)?;
            let new_id = resolve.worlds.alloc(world);
            assert_eq!(self.worlds.len(), id.index());
            self.worlds.push(new_id);
        }

        // And the final major step is transferring documents to `Resolve`
        // which is just updating a few identifiers here and there.
        for (id, mut doc) in unresolved.documents.into_iter().skip(foreign_documents) {
            self.update_document(&mut doc);
            let new_id = resolve.documents.alloc(doc);
            assert_eq!(self.documents.len(), id.index());
            self.documents.push(new_id);
        }

        // Fixup "parent" ids now that everything has been identifier
        for id in self.interfaces.iter().skip(foreign_interfaces) {
            let doc = &mut resolve.interfaces[*id].document;
            *doc = self.documents[doc.index()];
        }
        for id in self.worlds.iter().skip(foreign_worlds) {
            let doc = &mut resolve.worlds[*id].document;
            *doc = self.documents[doc.index()];
        }
        let mut documents = IndexMap::new();
        for id in self.documents.iter().skip(foreign_documents) {
            let prev = documents.insert(resolve.documents[*id].name.clone(), *id);
            assert!(prev.is_none());
        }

        Ok(resolve.packages.alloc(Package { documents }))
    }

    fn process_foreign_deps(
        &mut self,
        resolve: &mut Resolve,
        unresolved: &UnresolvedPackage,
        deps: &HashMap<String, PackageId>,
    ) -> Result<()> {
        // First, connect all references to foreign documents to actual
        // documents within `resolve`, building up the initial entries of
        // the `self.documents` mapping.
        for (i, (pkg, docs)) in unresolved.foreign_deps.iter().enumerate() {
            let pkgid = *deps.get(pkg).ok_or_else(|| Error {
                span: unresolved.foreign_dep_spans[i],
                msg: format!("no package dependency specified for `{pkg}`"),
            })?;
            let package = &resolve.packages[pkgid];
            for (doc, unresolved_doc_id) in docs {
                let span = unresolved.document_spans[unresolved_doc_id.index()];
                let docid = *package.documents.get(doc).ok_or_else(|| Error {
                    span,
                    msg: format!("package `{pkg}` does not define document `{doc}`"),
                })?;

                assert_eq!(self.documents.len(), unresolved_doc_id.index());
                self.documents.push(docid);
            }
        }

        // Next, for all documents that are referenced in this `Resolve`
        // determine the mapping of all interfaces that they refer to.
        for (unresolved_iface_id, unresolved_iface) in unresolved.interfaces.iter() {
            let doc_id = match self.documents.get(unresolved_iface.document.index()) {
                Some(i) => *i,
                // All foreign interfaces are defined first, so the first one
                // which is defined in a non-foreign document means that all
                // futher interfaces will be non-foreign as well.
                None => break,
            };

            // Functions can't be imported so this should be empty.
            assert!(unresolved_iface.functions.is_empty());

            let document = &resolve.documents[doc_id];
            let span = unresolved.interface_spans[unresolved_iface_id.index()];
            let iface_id = match &unresolved_iface.name {
                Some(name) => *document.interfaces.get(name).ok_or_else(|| Error {
                    span,
                    msg: format!("interface not defined in document"),
                })?,
                None => document.default_interface.ok_or_else(|| Error {
                    span,
                    msg: format!("default interface not specified in document"),
                })?,
            };
            assert_eq!(self.interfaces.len(), unresolved_iface_id.index());
            self.interfaces.push(iface_id);
        }

        // And finally iterate over all foreign-defined types and determine
        // what they map to.
        for (unresolved_type_id, unresolved_ty) in unresolved.types.iter() {
            // All "Unknown" types should appear first so once we're no longer
            // in unknown territory it's package-defined types so break out of
            // this loop.
            match unresolved_ty.kind {
                TypeDefKind::Unknown => {}
                _ => break,
            }
            let unresolved_iface_id = match unresolved_ty.owner {
                TypeOwner::Interface(id) => id,
                _ => unreachable!(),
            };
            let iface_id = self.interfaces[unresolved_iface_id.index()];
            let name = unresolved_ty.name.as_ref().unwrap();
            let span = unresolved.unknown_type_spans[unresolved_type_id.index()];
            let type_id = *resolve.interfaces[iface_id]
                .types
                .get(name)
                .ok_or_else(|| Error {
                    span,
                    msg: format!("type not defined in interface"),
                })?;
            assert_eq!(self.types.len(), unresolved_type_id.index());
            self.types.push(type_id);
        }

        Ok(())
    }

    fn update_typedef(&self, ty: &mut TypeDef) {
        // NB: note that `ty.owner` is not updated here since interfaces
        // haven't been mapped yet and that's done in a separate step.
        use crate::TypeDefKind::*;
        match &mut ty.kind {
            Record(r) => {
                for field in r.fields.iter_mut() {
                    self.update_ty(&mut field.ty);
                }
            }
            Tuple(t) => {
                for ty in t.types.iter_mut() {
                    self.update_ty(ty);
                }
            }
            Variant(v) => {
                for case in v.cases.iter_mut() {
                    if let Some(t) = &mut case.ty {
                        self.update_ty(t);
                    }
                }
            }
            Option(t) => self.update_ty(t),
            Result(r) => {
                if let Some(ty) = &mut r.ok {
                    self.update_ty(ty);
                }
                if let Some(ty) = &mut r.err {
                    self.update_ty(ty);
                }
            }
            Union(u) => {
                for case in u.cases.iter_mut() {
                    self.update_ty(&mut case.ty);
                }
            }
            List(t) => self.update_ty(t),
            Future(Some(t)) => self.update_ty(t),
            Stream(t) => {
                if let Some(ty) = &mut t.element {
                    self.update_ty(ty);
                }
                if let Some(ty) = &mut t.end {
                    self.update_ty(ty);
                }
            }
            Type(t) => self.update_ty(t),

            // nothing to do for these as they're just names or empty
            Flags(_) | Enum(_) | Future(None) => {}

            Unknown => unreachable!(),
        }
    }

    fn update_ty(&self, ty: &mut Type) {
        if let Type::Id(id) = ty {
            *id = self.types[id.index()];
        }
    }

    fn update_interface(&self, iface: &mut Interface) {
        // NB: note that `iface.doc` is not updated here since interfaces
        // haven't been mapped yet and that's done in a separate step.
        for (_name, ty) in iface.types.iter_mut() {
            *ty = self.types[ty.index()];
        }
        for func in iface.functions.iter_mut() {
            for (_, ty) in func.params.iter_mut() {
                self.update_ty(ty);
            }
            match &mut func.results {
                Results::Named(named) => {
                    for (_, ty) in named.iter_mut() {
                        self.update_ty(ty);
                    }
                }
                Results::Anon(ty) => self.update_ty(ty),
            }
        }
    }

    fn update_world(
        &self,
        world: &mut World,
        resolve: &Resolve,
        import_spans: &[Span],
        export_spans: &[Span],
    ) -> Result<()> {
        // NB: this function is more more complicated than the prior versions
        // of merging an item because this is the location that elaboration of
        // imports/exports of a world are fully resolved. With full transitive
        // knowledge of all interfaces a worlds imports, for example, are
        // expanded fully to ensure that all transitive items are necessarily
        // imported.
        assert_eq!(world.imports.len(), import_spans.len());
        assert_eq!(world.exports.len(), export_spans.len());

        // First up, process all the `imports` of the world. Note that this
        // starts by gutting the list of imports stored in `world` to get
        // rebuilt iteratively below.
        //
        // Here each import of an interface is recorded and then additionally
        // explicitly named imports of interfaces are recorded as well for
        // determining names later on.
        let mut explicit_import_names = HashMap::new();
        let mut explicit_export_names = HashMap::new();
        let mut imports = Vec::new();
        let mut exports = Vec::new();
        for ((name, item), span) in mem::take(&mut world.imports).into_iter().zip(import_spans) {
            match item {
                WorldItem::Interface(id) => {
                    let id = self.interfaces[id.index()];
                    imports.push((id, *span));
                    let prev = explicit_import_names.insert(id, name);
                    assert!(prev.is_none());
                }
                WorldItem::Function(_) => unimplemented!(),
            }
        }
        for ((name, item), span) in mem::take(&mut world.exports).into_iter().zip(export_spans) {
            match item {
                WorldItem::Interface(id) => {
                    let id = self.interfaces[id.index()];
                    exports.push((id, *span));
                    let prev = explicit_export_names.insert(id, name);
                    assert!(prev.is_none());
                }
                WorldItem::Function(_) => unimplemented!(),
            }
        }

        // Next all imports and their transitive imports are processed. This
        // is done through a `stack` of `Action` items which is processed in
        // LIFO order, meaning that an action of processing the dependencies
        // is pushed after processing the node itself. The dependency processing
        // will push more items onto the stack as necessary.
        let mut elaborate = WorldElaborator {
            resolve,
            world,
            imports_processed: Default::default(),
            exports_processed: Default::default(),
            resolving_stack: Default::default(),
            explicit_import_names: &explicit_import_names,
            explicit_export_names: &explicit_export_names,
        };
        for (id, span) in imports {
            elaborate.import(id, span)?;
        }
        for (id, span) in exports {
            elaborate.export(id, span)?;
        }

        log::trace!("imports = {:?}", world.imports);
        log::trace!("exports = {:?}", world.exports);

        Ok(())
    }

    fn update_document(&self, doc: &mut Document) {
        for (_name, iface) in doc.interfaces.iter_mut() {
            *iface = self.interfaces[iface.index()];
        }
        for world in doc.worlds.iter_mut() {
            *world = self.worlds[world.index()];
        }
        if let Some(default) = &mut doc.default_interface {
            *default = self.interfaces[default.index()];
        }
        if let Some(default) = &mut doc.default_world {
            *default = self.worlds[default.index()];
        }
    }
}

struct WorldElaborator<'a, 'b> {
    resolve: &'a Resolve,
    world: &'b mut World,
    explicit_import_names: &'a HashMap<InterfaceId, String>,
    explicit_export_names: &'a HashMap<InterfaceId, String>,

    /// Set of imports which are either imported into the world already or in
    /// the `stack` to get processed, used to ensure the same dependency isn't
    /// pushed multiple times into the stack.
    imports_processed: HashSet<InterfaceId>,
    exports_processed: HashSet<InterfaceId>,

    /// Dependency chain of why we're importing the top of `stack`, used to
    /// print an error message.
    resolving_stack: Vec<(InterfaceId, bool)>,
}

impl<'a> WorldElaborator<'a, '_> {
    fn import(&mut self, id: InterfaceId, span: Span) -> Result<()> {
        self.recurse(id, span, true)
    }

    fn export(&mut self, id: InterfaceId, span: Span) -> Result<()> {
        self.recurse(id, span, false)
    }

    fn recurse(&mut self, id: InterfaceId, span: Span, import: bool) -> Result<()> {
        let processed = if import {
            &mut self.imports_processed
        } else {
            &mut self.exports_processed
        };
        if !processed.insert(id) {
            return Ok(());
        }

        self.resolving_stack.push((id, import));
        for (_, ty) in self.resolve.interfaces[id].types.iter() {
            let ty = match self.resolve.types[*ty].kind {
                TypeDefKind::Type(Type::Id(id)) => id,
                _ => continue,
            };
            let dep = match self.resolve.types[ty].owner {
                TypeOwner::None => continue,
                TypeOwner::Interface(other) => other,
                TypeOwner::World(_) => unreachable!(),
            };
            let import = import || !self.explicit_export_names.contains_key(&dep);

            self.recurse(dep, span, import)?;
        }
        assert_eq!(self.resolving_stack.pop(), Some((id, import)));

        let name = self.name_of(id, import);
        let set = if import {
            &mut self.world.imports
        } else {
            &mut self.world.exports
        };
        let prev = set.insert(name.clone(), WorldItem::Interface(id));
        if prev.is_none() {
            return Ok(());
        }

        let desc = |import: bool| {
            if import {
                "import"
            } else {
                "export"
            }
        };

        let mut msg = format!("{} of `{}`", desc(import), self.name_of(id, import));
        if self.resolving_stack.is_empty() {
            msg.push_str(" ");
        } else {
            msg.push_str("\n");
        }
        for (i, import) in self.resolving_stack.iter().rev() {
            writeln!(
                msg,
                "  .. which is depended on by {} `{}`",
                desc(*import),
                self.name_of(*i, *import)
            )
            .unwrap();
        }
        writeln!(
            msg,
            "conflicts with a previously imported interface \
                             using the name `{name}`",
        )
        .unwrap();
        bail!(Error { span, msg })
    }

    fn name_of(&self, id: InterfaceId, import: bool) -> &'a String {
        let set = if import {
            &self.explicit_import_names
        } else {
            &self.explicit_export_names
        };
        set.get(&id)
            .unwrap_or_else(|| self.resolve.interfaces[id].name.as_ref().unwrap())
    }
}
