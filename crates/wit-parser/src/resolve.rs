use crate::ast::lex::Span;
use crate::ast::{parse_use_path, AstUsePath};
use crate::{
    Error, Function, Interface, InterfaceId, PackageName, Results, Type, TypeDef, TypeDefKind,
    TypeId, TypeOwner, UnresolvedPackage, World, WorldId, WorldItem, WorldKey,
};
use anyhow::{anyhow, bail, Context, Result};
use id_arena::{Arena, Id};
use indexmap::{IndexMap, IndexSet};
use std::collections::{BTreeMap, HashMap, HashSet};
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
    pub packages: Arena<Package>,
    pub package_names: IndexMap<PackageName, PackageId>,
}

/// A WIT package within a `Resolve`.
///
/// A package is a collection of interfaces and worlds. Packages additionally
/// have a unique identifier that affects generated components and uniquely
/// identifiers this particular package.
#[derive(Clone)]
pub struct Package {
    /// A unique name corresponding to this package.
    pub name: PackageName,

    /// All interfaces contained in this packaged, keyed by the interface's
    /// name.
    pub interfaces: IndexMap<String, InterfaceId>,

    /// All worlds contained in this package, keyed by the world's name.
    pub worlds: IndexMap<String, WorldId>,
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
    /// Dependencies referenced by the WIT package at `path` will be loaded from
    /// a `deps/..` directory under `path`. All directories under `deps/` will
    /// be parsed as a WIT package. The directory name containing each package
    /// is not used as each package is otherwise self-identifying.
    ///
    /// This function returns the [`PackageId`] of the root parsed package at
    /// `path`, along with a list of all paths that were consumed during parsing
    /// for the root package and all dependency packages.
    pub fn push_dir(&mut self, path: &Path) -> Result<(PackageId, Vec<PathBuf>)> {
        let pkg = UnresolvedPackage::parse_dir(path)
            .with_context(|| format!("failed to parse package: {}", path.display()))?;

        let deps = path.join("deps");
        let mut deps = parse_deps_dir(&deps)
            .with_context(|| format!("failed to parse dependency directory: {}", deps.display()))?;

        // Perform a simple topological sort which will bail out on cycles
        // and otherwise determine the order that packages must be added to
        // this `Resolve`.
        let mut order = IndexSet::new();
        let mut visiting = HashSet::new();
        for pkg in deps.values().chain([&pkg]) {
            visit(&pkg, &deps, &mut order, &mut visiting)?;
        }

        // Using the topological ordering insert each package incrementally.
        // Additionally note that the last item visited here is the root
        // package, which is the one returned here.
        let mut last = None;
        let mut files = Vec::new();
        let mut pkg = Some(pkg);
        for name in order {
            let pkg = deps.remove(&name).unwrap_or_else(|| pkg.take().unwrap());
            files.extend(pkg.source_files().map(|p| p.to_path_buf()));
            let pkgid = self.push(pkg)?;
            last = Some(pkgid);
        }

        return Ok((last.unwrap(), files));

        fn parse_deps_dir(path: &Path) -> Result<BTreeMap<PackageName, UnresolvedPackage>> {
            let mut ret = BTreeMap::new();
            // If there's no `deps` dir, then there's no deps, so return the
            // empty set.
            if !path.exists() {
                return Ok(ret);
            }
            for dep in path.read_dir().context("failed to read directory")? {
                let dep = dep.context("failed to read directory iterator")?;
                let path = dep.path();
                let pkg = UnresolvedPackage::parse_dir(&path)
                    .with_context(|| format!("failed to parse package: {}", path.display()))?;
                let prev = ret.insert(pkg.name.clone(), pkg);
                if let Some(prev) = prev {
                    bail!("duplicate definitions of package `{}` found", prev.name);
                }
            }
            Ok(ret)
        }

        fn visit<'a>(
            pkg: &'a UnresolvedPackage,
            deps: &'a BTreeMap<PackageName, UnresolvedPackage>,
            order: &mut IndexSet<PackageName>,
            visiting: &mut HashSet<&'a PackageName>,
        ) -> Result<()> {
            if order.contains(&pkg.name) {
                return Ok(());
            }
            pkg.source_map.rewrite_error(|| {
                for (i, (dep, _)) in pkg.foreign_deps.iter().enumerate() {
                    let span = pkg.foreign_dep_spans[i];
                    if !visiting.insert(dep) {
                        bail!(Error {
                            span,
                            msg: format!("package depends on itself"),
                        });
                    }
                    let dep = deps.get(dep).ok_or_else(|| Error {
                        span,
                        msg: format!("failed to find package `{dep}` in `deps` directory"),
                    })?;
                    visit(dep, deps, order, visiting)?;
                    assert!(visiting.remove(&dep.name));
                }
                assert!(order.insert(pkg.name.clone()));
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
    pub fn push(&mut self, mut unresolved: UnresolvedPackage) -> Result<PackageId> {
        let source_map = mem::take(&mut unresolved.source_map);
        source_map.rewrite_error(|| Remap::default().append(self, unresolved))
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

    /// Merges all the contents of a different `Resolve` into this one. The
    /// `Remap` structure returned provides a mapping from all old indices to
    /// new indices
    ///
    /// This operation can fail if `resolve` disagrees with `self` about the
    /// packages being inserted. Otherwise though this will additionally attempt
    /// to "union" packages found in `resolve` with those found in `self`.
    /// Unioning packages is keyed on the name/url of packages for those with
    /// URLs present. If found then it's assumed that both `Resolve` instances
    /// were originally created from the same contents and are two views
    /// of the same package.
    pub fn merge(&mut self, resolve: Resolve) -> Result<Remap> {
        log::trace!(
            "merging {} packages into {} packages",
            resolve.packages.len(),
            self.packages.len()
        );

        let mut map = MergeMap::new(&resolve, &self)?;
        map.build()?;
        let MergeMap {
            package_map,
            interface_map,
            type_map,
            world_map,
            interfaces_to_add,
            worlds_to_add,
            ..
        } = map;

        // With a set of maps from ids in `resolve` to ids in `self` the next
        // operation is to start moving over items and building a `Remap` to
        // update ids.
        //
        // Each component field of `resolve` is moved into `self` so long as
        // its ID is not within one of the maps above. If it's present in a map
        // above then that means the item is already present in `self` so a new
        // one need not be added. If it's not present in a map that means it's
        // not present in `self` so it must be added to an arena.
        //
        // When adding an item to an arena one of the `remap.update_*` methods
        // is additionally called to update all identifiers from pointers within
        // `resolve` to becoming pointers within `self`.
        //
        // Altogether this should weave all the missing items in `self` from
        // `resolve` into one structure while updating all identifiers to
        // be local within `self`.

        let mut remap = Remap::default();
        let Resolve {
            types,
            worlds,
            interfaces,
            packages,
            package_names,
        } = resolve;

        let mut moved_types = Vec::new();
        for (id, mut ty) in types {
            let new_id = type_map.get(&id).copied().unwrap_or_else(|| {
                moved_types.push(id);
                remap.update_typedef(&mut ty);
                self.types.alloc(ty)
            });
            assert_eq!(remap.types.len(), id.index());
            remap.types.push(new_id);
        }

        let mut moved_interfaces = Vec::new();
        for (id, mut iface) in interfaces {
            let new_id = interface_map.get(&id).copied().unwrap_or_else(|| {
                moved_interfaces.push(id);
                remap.update_interface(&mut iface);
                self.interfaces.alloc(iface)
            });
            assert_eq!(remap.interfaces.len(), id.index());
            remap.interfaces.push(new_id);
        }

        let mut moved_worlds = Vec::new();
        for (id, mut world) in worlds {
            let new_id = world_map.get(&id).copied().unwrap_or_else(|| {
                moved_worlds.push(id);
                let update = |map: &mut IndexMap<WorldKey, WorldItem>| {
                    for (mut name, mut item) in mem::take(map) {
                        remap.update_world_key(&mut name);
                        match &mut item {
                            WorldItem::Function(f) => remap.update_function(f),
                            WorldItem::Interface(i) => *i = remap.interfaces[i.index()],
                            WorldItem::Type(i) => *i = remap.types[i.index()],
                        }
                        map.insert(name, item);
                    }
                };
                update(&mut world.imports);
                update(&mut world.exports);
                self.worlds.alloc(world)
            });
            assert_eq!(remap.worlds.len(), id.index());
            remap.worlds.push(new_id);
        }

        for (id, mut pkg) in packages {
            let new_id = package_map.get(&id).copied().unwrap_or_else(|| {
                for (_, id) in pkg.interfaces.iter_mut() {
                    *id = remap.interfaces[id.index()];
                }
                for (_, id) in pkg.worlds.iter_mut() {
                    *id = remap.worlds[id.index()];
                }
                self.packages.alloc(pkg)
            });
            assert_eq!(remap.packages.len(), id.index());
            remap.packages.push(new_id);
        }

        for (name, id) in package_names {
            let id = remap.packages[id.index()];
            if let Some(prev) = self.package_names.insert(name, id) {
                assert_eq!(prev, id);
            }
        }

        // Fixup all "parent" links now.
        //
        // Note that this is only done for items that are actually moved from
        // `resolve` into `self`, which is tracked by the various `moved_*`
        // lists built incrementally above. The ids in the `moved_*` lists
        // are ids within `resolve`, so they're translated through `remap` to
        // ids within `self`.
        for id in moved_worlds {
            let id = remap.worlds[id.index()];
            let pkg = self.worlds[id].package.as_mut().unwrap();
            *pkg = remap.packages[pkg.index()];
        }
        for id in moved_interfaces {
            let id = remap.interfaces[id.index()];
            let pkg = self.interfaces[id].package.as_mut().unwrap();
            *pkg = remap.packages[pkg.index()];
        }
        for id in moved_types {
            let id = remap.types[id.index()];
            match &mut self.types[id].owner {
                TypeOwner::Interface(id) => *id = remap.interfaces[id.index()],
                TypeOwner::World(id) => *id = remap.worlds[id.index()],
                TypeOwner::None => {}
            }
        }

        // And finally process items that were present in `resolve` but were
        // not present in `self`. This is only done for merged packages as
        // documents may be added to `self.documents` but wouldn't otherwise be
        // present in the `documents` field of the corresponding package.
        for (name, pkg, iface) in interfaces_to_add {
            let prev = self.packages[pkg]
                .interfaces
                .insert(name, remap.interfaces[iface.index()]);
            assert!(prev.is_none());
        }
        for (name, pkg, world) in worlds_to_add {
            let prev = self.packages[pkg]
                .worlds
                .insert(name, remap.worlds[world.index()]);
            assert!(prev.is_none());
        }

        log::trace!("now have {} packages", self.packages.len());
        Ok(remap)
    }

    /// Merges the world `from` into the world `into`.
    ///
    /// This will attempt to merge one world into another, unioning all of its
    /// imports and exports together. This is an operation performed by
    /// `wit-component`, for example where two different worlds from two
    /// different libraries were linked into the same core wasm file and are
    /// producing a singular world that will be the final component's
    /// interface.
    ///
    /// This operation can fail if the imports/exports overlap.
    pub fn merge_worlds(&mut self, from: WorldId, into: WorldId) -> Result<()> {
        let mut new_imports = Vec::new();
        let mut new_exports = Vec::new();

        let from_world = &self.worlds[from];
        let into_world = &self.worlds[into];

        // Build a map of the imports/exports in `into` going the reverse
        // direction from what's listed. This is then consulted below to ensure
        // that the same item isn't exported or imported under two different
        // names which isn't allowed in the component model.
        let mut into_imports_by_id = HashMap::new();
        let mut into_exports_by_id = HashMap::new();
        for (name, import) in into_world.imports.iter() {
            if let WorldItem::Interface(id) = *import {
                let prev = into_imports_by_id.insert(id, name);
                assert!(prev.is_none());
            }
        }
        for (name, export) in into_world.exports.iter() {
            if let WorldItem::Interface(id) = *export {
                let prev = into_exports_by_id.insert(id, name);
                assert!(prev.is_none());
            }
        }
        for (name, import) in from_world.imports.iter() {
            // If the "from" world imports an interface which is already
            // imported by the "into" world then this is allowed if the names
            // are the same. Importing the same interface under different names
            // isn't allowed, but otherwise merging imports of
            // same-named-interfaces is allowed to merge them together.
            if let WorldItem::Interface(id) = import {
                if let Some(prev) = into_imports_by_id.get(id) {
                    if *prev != name {
                        let name = self.name_world_key(name);
                        let prev = self.name_world_key(prev);
                        bail!("import `{name}` conflicts with previous name of `{prev}`");
                    }
                }
            }
        }
        for (name, export) in from_world.exports.iter() {
            // Note that unlike imports same-named exports are not handled here
            // since if something is exported twice there's no way to "unify" it
            // so it's left as an error.
            if let WorldItem::Interface(id) = export {
                if let Some(prev) = into_exports_by_id.get(id) {
                    let name = self.name_world_key(name);
                    let prev = self.name_world_key(prev);
                    bail!("export `{name}` conflicts with previous name of `{prev}`");
                }
            }
        }

        // Next walk over the interfaces imported into `from_world` and queue up
        // imports to get inserted into `into_world`.
        for (name, from_import) in from_world.imports.iter() {
            match into_world.imports.get(name) {
                Some(into_import) => match (from_import, into_import) {
                    // If these imports, which have the same name, are of the
                    // same interface then union them together at this point.
                    (WorldItem::Interface(from), WorldItem::Interface(into)) if from == into => {
                        continue
                    }
                    _ => {
                        let name = self.name_world_key(name);
                        bail!("duplicate import found for interface `{name}`");
                    }
                },
                None => new_imports.push((name.clone(), from_import.clone())),
            }
        }

        // All exports at this time must be unique. For example the same
        // interface exported from two locations can't really be resolved to one
        // canonical definition, so make sure that merging worlds only succeeds
        // if the worlds have disjoint sets of exports.
        for (name, export) in from_world.exports.iter() {
            match into_world.exports.get(name) {
                Some(_) => {
                    let name = self.name_world_key(name);
                    bail!("duplicate export found for interface `{name}`");
                }
                None => new_exports.push((name.clone(), export.clone())),
            }
        }

        // Insert any new imports and new exports found first.
        let into = &mut self.worlds[into];
        for (name, import) in new_imports {
            let prev = into.imports.insert(name, import);
            assert!(prev.is_none());
        }
        for (name, export) in new_exports {
            let prev = into.exports.insert(name, export);
            assert!(prev.is_none());
        }

        Ok(())
    }

    /// Returns the ID of the specified `interface`.
    ///
    /// Returns `None` for unnamed interfaces.
    pub fn id_of(&self, interface: InterfaceId) -> Option<String> {
        let interface = &self.interfaces[interface];
        let package = &self.packages[interface.package.unwrap()];
        let mut base = String::new();
        base.push_str(&package.name.namespace);
        base.push_str(":");
        base.push_str(&package.name.name);
        base.push_str("/");
        base.push_str(interface.name.as_ref()?);
        if let Some(version) = &package.name.version {
            base.push_str(&format!("@{version}"));
        }
        Some(base)
    }

    /// Attempts to locate a world given the "default" package `pkg` and the
    /// optional string specifier `world`.
    ///
    /// This method is intended to be used by bindings generation tools to
    /// select a world from either `pkg` or a package in this `Resolve`.
    ///
    /// If `world` is `None` then `pkg` must have precisely one world which will
    /// be returned.
    ///
    /// If `world` is `Some` then it can either be:
    ///
    /// * A kebab-name of a world contained within `pkg` which is being
    ///   selected, such as `"the-world"`.
    ///
    /// * An ID-based form of a world which is selected within this `Resolve`,
    ///   ignoring `pkg`. For example `"wasi:http/proxy"`.
    ///
    /// If successful the corresponding `WorldId` is returned, otherwise an
    /// error is returned.
    pub fn select_world(&self, pkg: PackageId, world: Option<&str>) -> Result<WorldId> {
        let world = match world {
            Some(world) => world,
            None => {
                let pkg = &self.packages[pkg];
                match pkg.worlds.len() {
                    0 => bail!("no worlds found in package `{}`", pkg.name),
                    1 => return Ok(*pkg.worlds.values().next().unwrap()),
                    _ => bail!(
                        "multiple worlds found in package `{}`: one must be explicitly chosen",
                        pkg.name
                    ),
                }
            }
        };

        let path = parse_use_path(world)
            .with_context(|| format!("failed to parse world specifier `{world}`"))?;
        let (pkg, world) = match path {
            AstUsePath::Name(name) => (pkg, name),
            AstUsePath::Package(pkg, interface) => (
                *self
                    .package_names
                    .get(&pkg)
                    .ok_or_else(|| anyhow!("unknown package `{pkg}`"))?,
                interface,
            ),
        };
        let pkg = &self.packages[pkg];
        pkg.worlds
            .get(&world)
            .copied()
            .ok_or_else(|| anyhow!("no world named `{world}` in package"))
    }

    /// Assigns a human readable name to the `WorldKey` specified.
    pub fn name_world_key(&self, key: &WorldKey) -> String {
        match key {
            WorldKey::Name(s) => s.to_string(),
            WorldKey::Interface(i) => self.id_of(*i).expect("unexpected anonymous interface"),
        }
    }
}

/// Structure returned by [`Resolve::merge`] which contains mappings from
/// old-ids to new-ids after the merge.
#[derive(Default)]
pub struct Remap {
    pub types: Vec<TypeId>,
    pub interfaces: Vec<InterfaceId>,
    pub worlds: Vec<WorldId>,
    pub packages: Vec<PackageId>,
}

impl Remap {
    fn append(
        &mut self,
        resolve: &mut Resolve,
        unresolved: UnresolvedPackage,
    ) -> Result<PackageId> {
        self.process_foreign_deps(resolve, &unresolved)?;

        let foreign_types = self.types.len();
        let foreign_interfaces = self.interfaces.len();
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
                TypeOwner::World(_) | TypeOwner::None => {}
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

        // As with interfaces, now update the ids of world-owned types.
        for id in self.types.iter().skip(foreign_types) {
            match &mut resolve.types[*id].owner {
                TypeOwner::World(id) => *id = self.worlds[id.index()],
                TypeOwner::Interface(_) | TypeOwner::None => {}
            }
        }

        // Fixup "parent" ids now that everything has been identified
        let pkgid = resolve.packages.alloc(Package {
            name: unresolved.name.clone(),
            interfaces: Default::default(),
            worlds: Default::default(),
        });
        let prev = resolve.package_names.insert(unresolved.name.clone(), pkgid);
        assert!(prev.is_none());
        for id in self.interfaces.iter().skip(foreign_interfaces) {
            let iface = &mut resolve.interfaces[*id];
            iface.package = Some(pkgid);
            if let Some(name) = &iface.name {
                let prev = resolve.packages[pkgid].interfaces.insert(name.clone(), *id);
                assert!(prev.is_none());
            }
        }
        for id in self.worlds.iter().skip(foreign_worlds) {
            let world = &mut resolve.worlds[*id];
            world.package = Some(pkgid);
            let prev = resolve.packages[pkgid]
                .worlds
                .insert(world.name.clone(), *id);
            assert!(prev.is_none());
        }
        Ok(pkgid)
    }

    fn process_foreign_deps(
        &mut self,
        resolve: &mut Resolve,
        unresolved: &UnresolvedPackage,
    ) -> Result<()> {
        // Invert the `foreign_deps` map to be keyed by interface id to get
        // used in the loops below.
        let mut interface_to_package = HashMap::new();
        for (i, (pkg_name, interfaces)) in unresolved.foreign_deps.iter().enumerate() {
            for (interface, unresolved_interface_id) in interfaces {
                let prev = interface_to_package.insert(
                    *unresolved_interface_id,
                    (pkg_name, interface, unresolved.foreign_dep_spans[i]),
                );
                assert!(prev.is_none());
            }
        }

        // Connect all interfaces referred to in `interface_to_package`, which
        // are at the front of `unresolved.interfaces`, to interfaces already
        // contained within `resolve`.
        for (unresolved_iface_id, unresolved_iface) in unresolved.interfaces.iter() {
            let (pkg_name, interface, span) = match interface_to_package.get(&unresolved_iface_id) {
                Some(items) => *items,
                // All foreign interfaces are defined first, so the first one
                // which is defined in a non-foreign document means that all
                // further interfaces will be non-foreign as well.
                None => break,
            };
            let pkgid = resolve
                .package_names
                .get(pkg_name)
                .copied()
                .ok_or_else(|| Error {
                    span,
                    msg: format!("package not found"),
                })?;

            // Functions can't be imported so this should be empty.
            assert!(unresolved_iface.functions.is_empty());

            let pkg = &resolve.packages[pkgid];
            let span = unresolved.interface_spans[unresolved_iface_id.index()];
            let iface_id = pkg
                .interfaces
                .get(interface)
                .copied()
                .ok_or_else(|| Error {
                    span,
                    msg: format!("interface not found in package"),
                })?;
            assert_eq!(self.interfaces.len(), unresolved_iface_id.index());
            self.interfaces.push(iface_id);
        }

        for (id, _) in unresolved.interfaces.iter().skip(self.interfaces.len()) {
            assert!(
                interface_to_package.get(&id).is_none(),
                "found foreign interface after local interface"
            );
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
                    msg: format!("type `{name}` not defined in interface"),
                })?;
            assert_eq!(self.types.len(), unresolved_type_id.index());
            self.types.push(type_id);
        }

        for (_, ty) in unresolved.types.iter().skip(self.types.len()) {
            if let TypeDefKind::Unknown = ty.kind {
                panic!("unknown type after defined type");
            }
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
        for (_, func) in iface.functions.iter_mut() {
            self.update_function(func);
        }
    }

    fn update_function(&self, func: &mut Function) {
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
        let mut import_funcs = Vec::new();
        let mut import_types = Vec::new();
        for ((mut name, item), span) in mem::take(&mut world.imports).into_iter().zip(import_spans)
        {
            self.update_world_key(&mut name);
            match item {
                WorldItem::Interface(id) => {
                    let id = self.interfaces[id.index()];
                    self.add_world_import(resolve, world, name, id);
                }
                WorldItem::Function(mut f) => {
                    self.update_function(&mut f);
                    import_funcs.push((name.unwrap_name(), f, *span));
                }
                WorldItem::Type(id) => {
                    let id = self.types[id.index()];
                    import_types.push((name.unwrap_name(), id, *span));
                }
            }
        }

        for (_name, id, _span) in import_types.iter() {
            if let TypeDefKind::Type(Type::Id(other)) = resolve.types[*id].kind {
                if let TypeOwner::Interface(owner) = resolve.types[other].owner {
                    let name = WorldKey::Interface(owner);
                    self.add_world_import(resolve, world, name, owner);
                }
            }
        }

        let mut export_funcs = Vec::new();
        for ((mut name, item), span) in mem::take(&mut world.exports).into_iter().zip(export_spans)
        {
            self.update_world_key(&mut name);
            match item {
                WorldItem::Interface(id) => {
                    let id = self.interfaces[id.index()];
                    self.add_world_export(resolve, world, name, id);
                }
                WorldItem::Function(mut f) => {
                    self.update_function(&mut f);
                    let name = match name {
                        WorldKey::Name(name) => name,
                        WorldKey::Interface(_) => unreachable!(),
                    };
                    export_funcs.push((name, f, *span));
                }
                WorldItem::Type(_) => unreachable!(),
            }
        }

        for (name, id, span) in import_types {
            let prev = world
                .imports
                .insert(WorldKey::Name(name.clone()), WorldItem::Type(id));
            if prev.is_some() {
                bail!(Error {
                    msg: format!("export of type `{name}` shadows previously imported interface"),
                    span,
                })
            }
        }

        for (name, func, span) in import_funcs {
            let prev = world
                .imports
                .insert(WorldKey::Name(name.clone()), WorldItem::Function(func));
            if prev.is_some() {
                bail!(Error {
                    msg: format!(
                        "import of function `{name}` shadows previously imported interface"
                    ),
                    span,
                })
            }
        }
        for (name, func, span) in export_funcs {
            let prev = world
                .exports
                .insert(WorldKey::Name(name.clone()), WorldItem::Function(func));
            if prev.is_some() || world.imports.contains_key(&WorldKey::Name(name.clone())) {
                bail!(Error {
                    msg: format!(
                        "export of function `{name}` shadows previously exported interface"
                    ),
                    span,
                })
            }
        }

        log::trace!("imports = {:?}", world.imports);
        log::trace!("exports = {:?}", world.exports);

        Ok(())
    }

    fn update_world_key(&self, key: &mut WorldKey) {
        match key {
            WorldKey::Name(_) => {}
            WorldKey::Interface(id) => {
                *id = self.interfaces[id.index()];
            }
        }
    }

    fn add_world_import(
        &self,
        resolve: &Resolve,
        world: &mut World,
        key: WorldKey,
        id: InterfaceId,
    ) {
        if world.imports.contains_key(&key) {
            return;
        }

        foreach_interface_dep(resolve, id, |dep| {
            self.add_world_import(resolve, world, WorldKey::Interface(dep), dep);
        });
        let prev = world.imports.insert(key, WorldItem::Interface(id));
        assert!(prev.is_none());
    }

    fn add_world_export(
        &self,
        resolve: &Resolve,
        world: &mut World,
        key: WorldKey,
        id: InterfaceId,
    ) {
        if world
            .exports
            .insert(key, WorldItem::Interface(id))
            .is_some()
        {
            return;
        }

        foreach_interface_dep(resolve, id, |dep| {
            if !world.exports.contains_key(&WorldKey::Interface(dep)) {
                self.add_world_import(resolve, world, WorldKey::Interface(dep), dep);
            }
        });
    }
}

fn foreach_interface_dep(
    resolve: &Resolve,
    interface: InterfaceId,
    mut f: impl FnMut(InterfaceId),
) {
    for (_, ty) in resolve.interfaces[interface].types.iter() {
        let ty = match resolve.types[*ty].kind {
            TypeDefKind::Type(Type::Id(id)) => id,
            _ => continue,
        };
        let dep = match resolve.types[ty].owner {
            TypeOwner::None => continue,
            TypeOwner::Interface(other) => other,
            TypeOwner::World(_) => unreachable!(),
        };
        if dep != interface {
            f(dep);
        }
    }
}

struct MergeMap<'a> {
    /// A map of package ids in `from` to those in `into` for those that are
    /// found to be equivalent.
    package_map: HashMap<PackageId, PackageId>,

    /// A map of interface ids in `from` to those in `into` for those that are
    /// found to be equivalent.
    interface_map: HashMap<InterfaceId, InterfaceId>,

    /// A map of type ids in `from` to those in `into` for those that are
    /// found to be equivalent.
    type_map: HashMap<TypeId, TypeId>,

    /// A map of world ids in `from` to those in `into` for those that are
    /// found to be equivalent.
    world_map: HashMap<WorldId, WorldId>,

    /// A list of documents that need to be added to packages in `into`.
    ///
    /// The elements here are:
    ///
    /// * The name of the interface/world
    /// * The ID within `into` of the package being added to
    /// * The ID within `from` of the item being added.
    interfaces_to_add: Vec<(String, PackageId, InterfaceId)>,
    worlds_to_add: Vec<(String, PackageId, WorldId)>,

    /// Which `Resolve` is being merged from.
    from: &'a Resolve,

    /// Which `Resolve` is being merged into.
    into: &'a Resolve,
}

impl<'a> MergeMap<'a> {
    fn new(from: &'a Resolve, into: &'a Resolve) -> Result<MergeMap<'a>> {
        Ok(MergeMap {
            package_map: Default::default(),
            interface_map: Default::default(),
            type_map: Default::default(),
            world_map: Default::default(),
            interfaces_to_add: Default::default(),
            worlds_to_add: Default::default(),
            from,
            into,
        })
    }

    fn build(&mut self) -> Result<()> {
        for (from_id, from) in self.from.packages.iter() {
            let into_id = match self.into.package_names.get(&from.name) {
                Some(id) => *id,

                // This package, according to its name and url, is not present
                // in `self` so it needs to get added below.
                None => {
                    log::trace!("adding unique package {}", from.name);
                    continue;
                }
            };
            log::trace!("merging duplicate package {}", from.name);

            self.build_package(from_id, into_id).with_context(|| {
                format!("failed to merge package `{}` into existing copy", from.name)
            })?;
        }

        Ok(())
    }

    fn build_package(&mut self, from_id: PackageId, into_id: PackageId) -> Result<()> {
        let prev = self.package_map.insert(from_id, into_id);
        assert!(prev.is_none());

        let from = &self.from.packages[from_id];
        let into = &self.into.packages[into_id];

        // If an interface is present in `from_id` but not present in `into_id`
        // then it can be copied over wholesale. That copy is scheduled to
        // happen within the `self.interfaces_to_add` list.
        for (name, from_interface_id) in from.interfaces.iter() {
            let into_interface_id = match into.interfaces.get(name) {
                Some(id) => *id,
                None => {
                    self.interfaces_to_add
                        .push((name.clone(), into_id, *from_interface_id));
                    continue;
                }
            };

            self.build_interface(*from_interface_id, into_interface_id)
                .with_context(|| format!("failed to merge interface `{name}`"))?;
        }

        for (name, from_world_id) in from.worlds.iter() {
            let into_world_id = match into.worlds.get(name) {
                Some(id) => *id,
                None => {
                    self.worlds_to_add
                        .push((name.clone(), into_id, *from_world_id));
                    continue;
                }
            };

            self.build_world(*from_world_id, into_world_id)
                .with_context(|| format!("failed to merge world `{name}`"))?;
        }

        Ok(())
    }

    fn build_interface(&mut self, from_id: InterfaceId, into_id: InterfaceId) -> Result<()> {
        let prev = self.interface_map.insert(from_id, into_id);
        assert!(prev.is_none());

        let from_interface = &self.from.interfaces[from_id];
        let into_interface = &self.into.interfaces[into_id];

        // Unlike documents/interfaces above if an interface in `from`
        // differs from the interface in `into` then that's considered an
        // error. Changing interfaces can reflect changes in imports/exports
        // which may not be expected so it's currently required that all
        // interfaces, when merged, exactly match.
        //
        // One case to consider here, for example, is that if a world in
        // `into` exports the interface `into_id` then if `from_id` were to
        // add more items into `into` then it would unexpectedly require more
        // items to be exported which may not work. In an import context this
        // might work since it's "just more items available for import", but
        // for now a conservative route of "interfaces must match" is taken.

        for (name, from_type_id) in from_interface.types.iter() {
            let into_type_id = *into_interface
                .types
                .get(name)
                .ok_or_else(|| anyhow!("expected type `{name}` to be present"))?;
            let prev = self.type_map.insert(*from_type_id, into_type_id);
            assert!(prev.is_none());

            // FIXME: ideally the types should be "structurally
            // equal" but that's not trivial to do in the face of
            // resources.
        }

        for (name, _) in from_interface.functions.iter() {
            if !into_interface.functions.contains_key(name) {
                bail!("expected function `{name}` to be present");
            }

            // FIXME: ideally the functions should be "structurally
            // equal" but that's not trivial to do in the face of
            // resources.
        }

        Ok(())
    }

    fn build_world(&mut self, from_id: WorldId, into_id: WorldId) -> Result<()> {
        let prev = self.world_map.insert(from_id, into_id);
        assert!(prev.is_none());

        let from_world = &self.from.worlds[from_id];
        let into_world = &self.into.worlds[into_id];

        // Same as interfaces worlds are expected to exactly match to avoid
        // unexpectedly changing a particular component's view of imports and
        // exports.
        //
        // FIXME: this should probably share functionality with
        // `Resolve::merge_worlds` to support adding imports but not changing
        // exports.

        if from_world.imports.len() != into_world.imports.len() {
            bail!("world contains different number of imports than expected");
        }
        if from_world.exports.len() != into_world.exports.len() {
            bail!("world contains different number of exports than expected");
        }

        for (from_name, from) in from_world.imports.iter() {
            let into_name = self.map_name(from_name);
            let name_str = self.from.name_world_key(from_name);
            let into = into_world
                .imports
                .get(&into_name)
                .ok_or_else(|| anyhow!("import `{name_str}` not found in target world"))?;
            self.match_world_item(from, into)
                .with_context(|| format!("import `{name_str}` didn't match target world"))?;
        }

        for (from_name, from) in from_world.exports.iter() {
            let into_name = self.map_name(from_name);
            let name_str = self.from.name_world_key(from_name);
            let into = into_world
                .exports
                .get(&into_name)
                .ok_or_else(|| anyhow!("export `{name_str}` not found in target world"))?;
            self.match_world_item(from, into)
                .with_context(|| format!("export `{name_str}` didn't match target world"))?;
        }

        Ok(())
    }

    fn map_name(&self, from_name: &WorldKey) -> WorldKey {
        match from_name {
            WorldKey::Name(s) => WorldKey::Name(s.clone()),
            WorldKey::Interface(id) => {
                WorldKey::Interface(self.interface_map.get(id).copied().unwrap_or(*id))
            }
        }
    }

    fn match_world_item(&mut self, from: &WorldItem, into: &WorldItem) -> Result<()> {
        match (from, into) {
            (WorldItem::Interface(from), WorldItem::Interface(into)) => {
                match (
                    &self.from.interfaces[*from].name,
                    &self.into.interfaces[*into].name,
                ) {
                    // If one interface is unnamed then they must both be
                    // unnamed and they must both have the same structure for
                    // now.
                    (None, None) => self.build_interface(*from, *into)?,

                    // Otherwise both interfaces must be named and they must
                    // have been previously found to be equivalent. Note that
                    // if either is unnamed it won't be present in
                    // `interface_map` so this'll return an error.
                    _ => {
                        if self.interface_map.get(&from) != Some(&into) {
                            bail!("interfaces are not the same");
                        }
                    }
                }
            }
            (WorldItem::Function(from), WorldItem::Function(into)) => {
                let _ = (from, into);
                // FIXME: should assert an check that `from` structurally
                // matches `into`
            }
            (WorldItem::Type(from), WorldItem::Type(into)) => {
                // FIXME: should assert an check that `from` structurally
                // matches `into`
                let prev = self.type_map.insert(*from, *into);
                assert!(prev.is_none());
            }

            (WorldItem::Interface(_), _)
            | (WorldItem::Function(_), _)
            | (WorldItem::Type(_), _) => {
                bail!("world items do not have the same type")
            }
        }
        Ok(())
    }
}
