use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, bail, ensure, Context, Result};
use id_arena::{Arena, Id};
use indexmap::{IndexMap, IndexSet};
#[cfg(feature = "serde")]
use serde_derive::Serialize;

use crate::ast::lex::Span;
use crate::ast::{parse_use_path, ParsedUsePath};
#[cfg(feature = "serde")]
use crate::serde_::{serialize_arena, serialize_id_map};
use crate::{
    AstItem, Docs, Error, Function, FunctionKind, Handle, IncludeName, Interface, InterfaceId,
    InterfaceSpan, PackageName, Results, SourceMap, Stability, Type, TypeDef, TypeDefKind, TypeId,
    TypeOwner, UnresolvedPackage, UnresolvedPackageGroup, World, WorldId, WorldItem, WorldKey,
    WorldSpan,
};

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
#[derive(Default, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Resolve {
    /// All knowns worlds within this `Resolve`.
    ///
    /// Each world points at a `PackageId` which is stored below. No ordering is
    /// guaranteed between this list of worlds.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_arena"))]
    pub worlds: Arena<World>,

    /// All knowns interfaces within this `Resolve`.
    ///
    /// Each interface points at a `PackageId` which is stored below. No
    /// ordering is guaranteed between this list of interfaces.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_arena"))]
    pub interfaces: Arena<Interface>,

    /// All knowns types within this `Resolve`.
    ///
    /// Types are topologically sorted such that any type referenced from one
    /// type is guaranteed to be defined previously. Otherwise though these are
    /// not sorted by interface for example.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_arena"))]
    pub types: Arena<TypeDef>,

    /// All knowns packages within this `Resolve`.
    ///
    /// This list of packages is not sorted. Sorted packages can be queried
    /// through [`Resolve::topological_packages`].
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_arena"))]
    pub packages: Arena<Package>,

    /// A map of package names to the ID of the package with that name.
    #[cfg_attr(feature = "serde", serde(skip))]
    pub package_names: IndexMap<PackageName, PackageId>,

    /// Activated features for this [`Resolve`].
    ///
    /// This set of features is empty by default. This is consulted for
    /// `@unstable` annotations in loaded WIT documents. Any items with
    /// `@unstable` are filtered out unless their feature is present within this
    /// set.
    #[cfg_attr(feature = "serde", serde(skip))]
    pub features: IndexSet<String>,

    /// Activate all features for this [`Resolve`].
    #[cfg_attr(feature = "serde", serde(skip))]
    pub all_features: bool,
}

/// A WIT package within a `Resolve`.
///
/// A package is a collection of interfaces and worlds. Packages additionally
/// have a unique identifier that affects generated components and uniquely
/// identifiers this particular package.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Package {
    /// A unique name corresponding to this package.
    pub name: PackageName,

    /// Documentation associated with this package.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub docs: Docs,

    /// All interfaces contained in this packaged, keyed by the interface's
    /// name.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    pub interfaces: IndexMap<String, InterfaceId>,

    /// All worlds contained in this package, keyed by the world's name.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_id_map"))]
    pub worlds: IndexMap<String, WorldId>,
}

pub type PackageId = Id<Package>;

enum ParsedFile {
    #[cfg(feature = "decoding")]
    Package(PackageId),
    Unresolved(UnresolvedPackageGroup),
}

/// Visitor helper for performing topological sort on a group of packages.
fn visit<'a>(
    pkg: &'a UnresolvedPackage,
    pkg_details_map: &'a BTreeMap<PackageName, (UnresolvedPackage, usize)>,
    order: &mut IndexSet<PackageName>,
    visiting: &mut HashSet<&'a PackageName>,
    source_maps: &[SourceMap],
) -> Result<()> {
    if order.contains(&pkg.name) {
        return Ok(());
    }

    match pkg_details_map.get(&pkg.name) {
        Some(pkg_details) => {
            let (_, source_maps_index) = pkg_details;
            source_maps[*source_maps_index].rewrite_error(|| {
                for (i, (dep, _)) in pkg.foreign_deps.iter().enumerate() {
                    let span = pkg.foreign_dep_spans[i];
                    if !visiting.insert(dep) {
                        bail!(Error::new(span, "package depends on itself"));
                    }
                    if let Some(dep) = pkg_details_map.get(dep) {
                        let (dep_pkg, _) = dep;
                        visit(dep_pkg, pkg_details_map, order, visiting, source_maps)?;
                    }
                    assert!(visiting.remove(dep));
                }
                assert!(order.insert(pkg.name.clone()));
                Ok(())
            })
        }
        None => panic!("No pkg_details found for package when doing topological sort"),
    }
}

impl Resolve {
    /// Creates a new [`Resolve`] with no packages/items inside of it.
    pub fn new() -> Resolve {
        Resolve::default()
    }

    /// Parse WIT packages from the input `path`.
    ///
    /// The input `path` can be one of:
    ///
    /// * A directory containing a WIT package with an optional `deps` directory
    ///   for any dependent WIT packages it references.
    /// * A single standalone WIT file.
    /// * A wasm-encoded WIT package as a single file in the wasm binary format.
    /// * A wasm-encoded WIT package as a single file in the wasm text format.
    ///
    /// In all of these cases packages are allowed to depend on previously
    /// inserted packages into this `Resolve`. Resolution for packages is based
    /// on the name of each package and reference.
    ///
    /// This method returns a list of `PackageId` elements and additionally a
    /// list of `PathBuf` elements. The `PackageId` elements represent the "main
    /// package" that was parsed. For example if a single WIT file was specified
    /// this will be all the packages found in the file. For a directory this
    /// will be all the packages in the directory itself, but not in the `deps`
    /// directory. The list of `PackageId` values is useful to pass to
    /// [`Resolve::select_world`] to take a user-specified world in a
    /// conventional fashion and select which to use for bindings generation.
    ///
    /// The returned list of `PathBuf` elements represents all files parsed
    /// during this operation. This can be useful for systems that want to
    /// rebuild or regenerate bindings based on files modified.
    ///
    /// More information can also be found at [`Resolve::push_dir`] and
    /// [`Resolve::push_file`].
    pub fn push_path(&mut self, path: impl AsRef<Path>) -> Result<(PackageId, Vec<PathBuf>)> {
        self._push_path(path.as_ref())
    }

    fn _push_path(&mut self, path: &Path) -> Result<(PackageId, Vec<PathBuf>)> {
        if path.is_dir() {
            self.push_dir(path).with_context(|| {
                format!(
                    "failed to resolve directory while parsing WIT for path [{}]",
                    path.display()
                )
            })
        } else {
            let id = self.push_file(path)?;
            Ok((id, vec![path.to_path_buf()]))
        }
    }

    fn sort_unresolved_packages(
        &mut self,
        main: UnresolvedPackageGroup,
        deps: Vec<UnresolvedPackageGroup>,
    ) -> Result<(PackageId, Vec<PathBuf>)> {
        let mut pkg_details_map = BTreeMap::new();
        let mut source_maps = Vec::new();

        let mut insert = |group: UnresolvedPackageGroup| {
            let UnresolvedPackageGroup {
                main,
                nested,
                source_map,
            } = group;
            let i = source_maps.len();
            source_maps.push(source_map);

            for pkg in nested.into_iter().chain([main]) {
                let name = pkg.name.clone();
                let my_span = pkg.package_name_span;
                let (prev_pkg, prev_i) = match pkg_details_map.insert(name.clone(), (pkg, i)) {
                    Some(pair) => pair,
                    None => continue,
                };
                let loc1 = source_maps[i].render_location(my_span);
                let loc2 = source_maps[prev_i].render_location(prev_pkg.package_name_span);
                bail!(
                    "\
package {name} is defined in two different locations:\n\
  * {loc1}\n\
  * {loc2}\n\
                     "
                )
            }
            Ok(())
        };

        let main_name = main.main.name.clone();
        insert(main)?;
        for dep in deps {
            insert(dep)?;
        }

        // Perform a simple topological sort which will bail out on cycles
        // and otherwise determine the order that packages must be added to
        // this `Resolve`.
        let mut order = IndexSet::new();
        let mut visiting = HashSet::new();
        for pkg_details in pkg_details_map.values() {
            let (pkg, _) = pkg_details;
            visit(
                pkg,
                &pkg_details_map,
                &mut order,
                &mut visiting,
                &source_maps,
            )?;
        }

        // Ensure that the final output is topologically sorted. Use a set to ensure that we render
        // the buffers for each `SourceMap` only once, even though multiple packages may references
        // the same `SourceMap`.
        let mut main_pkg_id = None;
        for name in order {
            let (pkg, source_map_index) = pkg_details_map.remove(&name).unwrap();
            let source_map = &source_maps[source_map_index];
            let is_main = pkg.name == main_name;
            let id = self.push(pkg, source_map)?;
            if is_main {
                assert!(main_pkg_id.is_none());
                main_pkg_id = Some(id);
            }
        }

        let path_bufs = source_maps
            .iter()
            .flat_map(|s| s.source_files())
            .map(|p| p.to_path_buf())
            .collect();

        Ok((main_pkg_id.unwrap(), path_bufs))
    }

    /// Parses the filesystem directory at `path` as a WIT package and returns
    /// a fully resolved [`PackageId`] list as a result.
    ///
    /// The directory itself is parsed with [`UnresolvedPackageGroup::parse_dir`]
    /// and then all packages found are inserted into this `Resolve`. The `path`
    /// specified may have a `deps` subdirectory which is probed automatically
    /// for any other WIT dependencies.
    ///
    /// The `deps` folder may contain:
    ///
    /// * `$path/deps/my-package/*.wit` - a directory that may contain multiple
    ///   WIT files. This is parsed with [`UnresolvedPackageGroup::parse_dir`]
    ///   and then inserted into this [`Resolve`]. Note that cannot recursively
    ///   contain a `deps` directory.
    /// * `$path/deps/my-package.wit` - a single-file WIT package. This is
    ///   parsed with [`Resolve::push_file`] and then added to `self` for
    ///   name reoslution.
    /// * `$path/deps/my-package.{wasm,wat}` - a wasm-encoded WIT package either
    ///   in the text for binary format.
    ///
    /// In all cases entries in the `deps` folder are added to `self` first
    /// before adding files found in `path` itself. All WIT packages found are
    /// candidates for name-based resolution that other packages may used.
    ///
    /// This function returns a tuple of two values. The first value is a list
    /// of [`PackageId`] values which represents the WIT packages found within
    /// `path`, but not those within `deps`. The `path` provided may contain
    /// only a single WIT package but might also use the multi-package form of
    /// WIT, and the returned list will indicate which was used. This argument
    /// is useful for passing to [`Resolve::select_world`] for choosing
    /// something to bindgen with.
    ///
    /// The second value returned here is the list of paths that were parsed
    /// when generating the return value. This can be useful for build systems
    /// that want to rebuild bindings whenever one of the files change.
    pub fn push_dir(&mut self, path: impl AsRef<Path>) -> Result<(PackageId, Vec<PathBuf>)> {
        self._push_dir(path.as_ref())
    }

    fn _push_dir(&mut self, path: &Path) -> Result<(PackageId, Vec<PathBuf>)> {
        let top_pkg = UnresolvedPackageGroup::parse_dir(path)
            .with_context(|| format!("failed to parse package: {}", path.display()))?;
        let deps = path.join("deps");
        let deps = self
            .parse_deps_dir(&deps)
            .with_context(|| format!("failed to parse dependency directory: {}", deps.display()))?;

        self.sort_unresolved_packages(top_pkg, deps)
    }

    fn parse_deps_dir(&mut self, path: &Path) -> Result<Vec<UnresolvedPackageGroup>> {
        let mut ret = Vec::new();
        if !path.exists() {
            return Ok(ret);
        }
        let mut entries = path
            .read_dir()
            .and_then(|i| i.collect::<std::io::Result<Vec<_>>>())
            .context("failed to read directory")?;
        entries.sort_by_key(|e| e.file_name());
        for dep in entries {
            let path = dep.path();
            let pkg = if dep.file_type()?.is_dir() || path.metadata()?.is_dir() {
                // If this entry is a directory or a symlink point to a
                // directory then always parse it as an `UnresolvedPackage`
                // since it's intentional to not support recursive `deps`
                // directories.
                UnresolvedPackageGroup::parse_dir(&path)
                    .with_context(|| format!("failed to parse package: {}", path.display()))?
            } else {
                // If this entry is a file then we may want to ignore it but
                // this may also be a standalone WIT file or a `*.wasm` or
                // `*.wat` encoded package.
                let filename = dep.file_name();
                match Path::new(&filename).extension().and_then(|s| s.to_str()) {
                    Some("wit") | Some("wat") | Some("wasm") => match self._push_file(&path)? {
                        #[cfg(feature = "decoding")]
                        ParsedFile::Package(_) => continue,
                        ParsedFile::Unresolved(pkg) => pkg,
                    },

                    // Other files in deps dir are ignored for now to avoid
                    // accidentally including things like `.DS_Store` files in
                    // the call below to `parse_dir`.
                    _ => continue,
                }
            };
            ret.push(pkg);
        }
        Ok(ret)
    }

    /// Parses the contents of `path` from the filesystem and pushes the result
    /// into this `Resolve`.
    ///
    /// The `path` referenced here can be one of:
    ///
    /// * A WIT file. Note that in this case this single WIT file will be the
    ///   entire package and any dependencies it has must already be in `self`.
    /// * A WIT package encoded as WebAssembly, either in text or binary form.
    ///   In this the package and all of its dependencies are automatically
    ///   inserted into `self`.
    ///
    /// In both situations the `PackageId`s of the resulting resolved packages
    /// are returned from this method. The return value is mostly useful in
    /// conjunction with [`Resolve::select_world`].
    pub fn push_file(&mut self, path: impl AsRef<Path>) -> Result<PackageId> {
        match self._push_file(path.as_ref())? {
            #[cfg(feature = "decoding")]
            ParsedFile::Package(id) => Ok(id),
            ParsedFile::Unresolved(pkg) => self.push_group(pkg),
        }
    }

    fn _push_file(&mut self, path: &Path) -> Result<ParsedFile> {
        let contents = std::fs::read(path)
            .with_context(|| format!("failed to read path for WIT [{}]", path.display()))?;

        // If decoding is enabled at compile time then try to see if this is a
        // wasm file.
        #[cfg(feature = "decoding")]
        {
            use crate::decoding::{decode, DecodedWasm};

            #[cfg(feature = "wat")]
            let is_wasm = wat::Detect::from_bytes(&contents).is_wasm();
            #[cfg(not(feature = "wat"))]
            let is_wasm = wasmparser::Parser::is_component(&contents);

            if is_wasm {
                #[cfg(feature = "wat")]
                let contents = wat::parse_bytes(&contents).map_err(|mut e| {
                    e.set_path(path);
                    e
                })?;

                match decode(&contents)? {
                    DecodedWasm::Component(..) => {
                        bail!("found an actual component instead of an encoded WIT package in wasm")
                    }
                    DecodedWasm::WitPackage(resolve, pkg) => {
                        let remap = self.merge(resolve)?;
                        return Ok(ParsedFile::Package(remap.packages[pkg.index()]));
                    }
                }
            }
        }

        // If this wasn't a wasm file then assume it's a WIT file.
        let text = match std::str::from_utf8(&contents) {
            Ok(s) => s,
            Err(_) => bail!("input file is not valid utf-8 [{}]", path.display()),
        };
        let pkgs = UnresolvedPackageGroup::parse(path, text)?;
        Ok(ParsedFile::Unresolved(pkgs))
    }

    /// Appends a new [`UnresolvedPackage`] to this [`Resolve`], creating a
    /// fully resolved package with no dangling references.
    ///
    /// All the dependencies of `unresolved` must already have been loaded
    /// within this `Resolve` via previous calls to `push` or other methods such
    /// as [`Resolve::push_path`].
    ///
    /// Any dependency resolution error or otherwise world-elaboration error
    /// will be returned here, if successful a package identifier is returned
    /// which corresponds to the package that was just inserted.
    pub fn push(
        &mut self,
        unresolved: UnresolvedPackage,
        source_map: &SourceMap,
    ) -> Result<PackageId> {
        source_map.rewrite_error(|| Remap::default().append(self, unresolved))
    }

    /// Appends new [`UnresolvedPackageGroup`] to this [`Resolve`], creating a
    /// fully resolved package with no dangling references.
    ///
    /// Any dependency resolution error or otherwise world-elaboration error
    /// will be returned here, if successful a package identifier is returned
    /// which corresponds to the package that was just inserted.
    ///
    /// The returned [`PackageId`]s are listed in topologically sorted order.
    pub fn push_group(&mut self, unresolved_group: UnresolvedPackageGroup) -> Result<PackageId> {
        let (pkg_id, _) = self.sort_unresolved_packages(unresolved_group, Vec::new())?;
        Ok(pkg_id)
    }

    /// Convenience method for combining [`UnresolvedPackageGroup::parse`] and
    /// [`Resolve::push_group`].
    ///
    /// The `path` provided is used for error messages but otherwise is not
    /// read. This method does not touch the filesystem. The `contents` provided
    /// are the contents of a WIT package.
    pub fn push_str(&mut self, path: impl AsRef<Path>, contents: &str) -> Result<PackageId> {
        self.push_group(UnresolvedPackageGroup::parse(path.as_ref(), contents)?)
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
            | Type::F32
            | Type::F64 => true,

            Type::Bool | Type::Char | Type::String => false,

            Type::Id(id) => match &self.types[*id].kind {
                TypeDefKind::List(_)
                | TypeDefKind::Variant(_)
                | TypeDefKind::Enum(_)
                | TypeDefKind::Option(_)
                | TypeDefKind::Result(_)
                | TypeDefKind::Future(_)
                | TypeDefKind::Stream(_) => false,
                TypeDefKind::Type(t) => self.all_bits_valid(t),

                TypeDefKind::Handle(h) => match h {
                    crate::Handle::Own(_) => true,
                    crate::Handle::Borrow(_) => true,
                },

                TypeDefKind::Resource => false,
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

        let mut map = MergeMap::new(&resolve, &self);
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
            features: _,
            ..
        } = resolve;

        let mut moved_types = Vec::new();
        for (id, mut ty) in types {
            let new_id = match type_map.get(&id).copied() {
                Some(id) => {
                    update_stability(&ty.stability, &mut self.types[id].stability)?;
                    id
                }
                None => {
                    log::debug!("moving type {:?}", ty.name);
                    moved_types.push(id);
                    remap.update_typedef(self, &mut ty, None)?;
                    self.types.alloc(ty)
                }
            };
            assert_eq!(remap.types.len(), id.index());
            remap.types.push(Some(new_id));
        }

        let mut moved_interfaces = Vec::new();
        for (id, mut iface) in interfaces {
            let new_id = match interface_map.get(&id).copied() {
                Some(id) => {
                    update_stability(&iface.stability, &mut self.interfaces[id].stability)?;
                    id
                }
                None => {
                    log::debug!("moving interface {:?}", iface.name);
                    moved_interfaces.push(id);
                    remap.update_interface(self, &mut iface, None)?;
                    self.interfaces.alloc(iface)
                }
            };
            assert_eq!(remap.interfaces.len(), id.index());
            remap.interfaces.push(Some(new_id));
        }

        let mut moved_worlds = Vec::new();
        for (id, mut world) in worlds {
            let new_id = match world_map.get(&id).copied() {
                Some(id) => {
                    update_stability(&world.stability, &mut self.worlds[id].stability)?;
                    id
                }
                None => {
                    log::debug!("moving world {}", world.name);
                    moved_worlds.push(id);
                    let mut update = |map: &mut IndexMap<WorldKey, WorldItem>| -> Result<_> {
                        for (mut name, mut item) in mem::take(map) {
                            remap.update_world_key(&mut name, None)?;
                            match &mut item {
                                WorldItem::Function(f) => remap.update_function(self, f, None)?,
                                WorldItem::Interface { id, .. } => {
                                    *id = remap.map_interface(*id, None)?
                                }
                                WorldItem::Type(i) => *i = remap.map_type(*i, None)?,
                            }
                            map.insert(name, item);
                        }
                        Ok(())
                    };
                    update(&mut world.imports)?;
                    update(&mut world.exports)?;
                    self.worlds.alloc(world)
                }
            };
            assert_eq!(remap.worlds.len(), id.index());
            remap.worlds.push(Some(new_id));
        }

        for (id, mut pkg) in packages {
            let new_id = match package_map.get(&id).copied() {
                Some(id) => id,
                None => {
                    for (_, id) in pkg.interfaces.iter_mut() {
                        *id = remap.map_interface(*id, None)?;
                    }
                    for (_, id) in pkg.worlds.iter_mut() {
                        *id = remap.map_world(*id, None)?;
                    }
                    self.packages.alloc(pkg)
                }
            };
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
            let id = remap.map_world(id, None)?;
            let pkg = self.worlds[id].package.as_mut().unwrap();
            *pkg = remap.packages[pkg.index()];
        }
        for id in moved_interfaces {
            let id = remap.map_interface(id, None)?;
            let pkg = self.interfaces[id].package.as_mut().unwrap();
            *pkg = remap.packages[pkg.index()];
        }
        for id in moved_types {
            let id = remap.map_type(id, None)?;
            match &mut self.types[id].owner {
                TypeOwner::Interface(id) => *id = remap.map_interface(*id, None)?,
                TypeOwner::World(id) => *id = remap.map_world(*id, None)?,
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
                .insert(name, remap.map_interface(iface, None)?);
            assert!(prev.is_none());
        }
        for (name, pkg, world) in worlds_to_add {
            let prev = self.packages[pkg]
                .worlds
                .insert(name, remap.map_world(world, None)?);
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

        // First walk over all the imports of `from` world and figure out what
        // to do with them.
        //
        // If the same item exists in `from` and `into` then merge it together
        // below with `merge_world_item` which basically asserts they're the
        // same. Otherwise queue up a new import since if `from` has more
        // imports than `into` then it's fine to add new imports.
        for (name, from_import) in from_world.imports.iter() {
            match into_world.imports.get(name) {
                Some(into_import) => {
                    let name = self.name_world_key(name);
                    self.merge_world_item(from_import, into_import)
                        .with_context(|| format!("failed to merge world import {name}"))?;
                }
                None => {
                    new_imports.push((name.clone(), from_import.clone()));
                }
            }
        }

        // Next walk over exports of `from` and process these similarly to
        // imports.
        for (name, from_export) in from_world.exports.iter() {
            match into_world.exports.get(name) {
                Some(into_export) => {
                    let name = self.name_world_key(name);
                    self.merge_world_item(from_export, into_export)
                        .with_context(|| format!("failed to merge world export {name}"))?;
                }
                None => {
                    // See comments in `ensure_can_add_world_export` for why
                    // this is slightly different than imports.
                    self.ensure_can_add_world_export(into_world, name, from_export)
                        .with_context(|| {
                            format!("failed to add export `{}`", self.name_world_key(name))
                        })?;
                    new_exports.push((name.clone(), from_export.clone()));
                }
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

    fn merge_world_item(&self, from: &WorldItem, into: &WorldItem) -> Result<()> {
        let mut map = MergeMap::new(self, self);
        match (from, into) {
            (WorldItem::Interface { id: from, .. }, WorldItem::Interface { id: into, .. }) => {
                // If these imports are the same that can happen, for
                // example, when both worlds to `import foo:bar/baz;`. That
                // foreign interface will point to the same interface within
                // `Resolve`.
                if from == into {
                    return Ok(());
                }

                // .. otherwise this MUST be a case of
                // `import foo: interface { ... }`. If `from != into` but
                // both `from` and `into` have the same name then the
                // `WorldKey::Interface` case is ruled out as otherwise
                // they'd have different names.
                //
                // In the case of an anonymous interface all we can do is
                // ensure that the interfaces both match, so use `MergeMap`
                // for that.
                map.build_interface(*from, *into)
                    .context("failed to merge interfaces")?;
            }

            // Like `WorldKey::Name` interfaces for functions and types the
            // structure is asserted to be the same.
            (WorldItem::Function(from), WorldItem::Function(into)) => {
                map.build_function(from, into)
                    .context("failed to merge functions")?;
            }
            (WorldItem::Type(from), WorldItem::Type(into)) => {
                map.build_type_id(*from, *into)
                    .context("failed to merge types")?;
            }

            // Kind-level mismatches are caught here.
            (WorldItem::Interface { .. }, _)
            | (WorldItem::Function { .. }, _)
            | (WorldItem::Type { .. }, _) => {
                bail!("different kinds of items");
            }
        }
        assert!(map.interfaces_to_add.is_empty());
        assert!(map.worlds_to_add.is_empty());
        Ok(())
    }

    /// This method ensures that the world export of `name` and `item` can be
    /// added to the world `into` without changing the meaning of `into`.
    ///
    /// This is somewhat tricky due to how exports/imports are elaborated today
    /// but the basic idea is that the transitive dependencies of an `export`
    /// will be implicitly `import`ed if they're not otherwise listed as
    /// exports. That means that if a transitive dependency of a preexisting
    /// export is added as a new export it might change the meaning of an
    /// existing import if it was otherwise already hooked up to an import.
    ///
    /// This method rules out this situation.
    fn ensure_can_add_world_export(
        &self,
        into: &World,
        name: &WorldKey,
        item: &WorldItem,
    ) -> Result<()> {
        assert!(!into.exports.contains_key(name));
        let interface = match name {
            // Top-level exports always depend on imports, so these are always
            // allowed to be added.
            WorldKey::Name(_) => return Ok(()),

            // This is the case we're worried about. Here if the key is an
            // interface then the item must also be an interface.
            WorldKey::Interface(key) => {
                match item {
                    WorldItem::Interface { id, .. } => assert_eq!(id, key),
                    _ => unreachable!(),
                }
                *key
            }
        };

        // For `interface` to be added as a new export of `into` then it must be
        // the case that no previous export of `into` depends on `interface`.
        // Test that by walking all interface exports and seeing if any types
        // refer to this interface.
        for (export_name, export) in into.exports.iter() {
            let export_interface = match export_name {
                WorldKey::Name(_) => continue,
                WorldKey::Interface(key) => {
                    match export {
                        WorldItem::Interface { id, .. } => assert_eq!(id, key),
                        _ => unreachable!(),
                    }
                    *key
                }
            };
            assert!(export_interface != interface);
            let iface = &self.interfaces[export_interface];
            for (name, ty) in iface.types.iter() {
                let other_ty = match self.types[*ty].kind {
                    TypeDefKind::Type(Type::Id(ty)) => ty,
                    _ => continue,
                };
                if self.types[other_ty].owner != TypeOwner::Interface(interface) {
                    continue;
                }

                let export_name = self.name_world_key(export_name);
                bail!(
                    "export `{export_name}` has a type `{name}` which could \
                     change meaning if this world export were added"
                )
            }
        }

        Ok(())
    }

    /// Returns the ID of the specified `interface`.
    ///
    /// Returns `None` for unnamed interfaces.
    pub fn id_of(&self, interface: InterfaceId) -> Option<String> {
        let interface = &self.interfaces[interface];
        Some(self.id_of_name(interface.package.unwrap(), interface.name.as_ref()?))
    }

    /// Returns the ID of the specified `name` within the `pkg`.
    pub fn id_of_name(&self, pkg: PackageId, name: &str) -> String {
        let package = &self.packages[pkg];
        let mut base = String::new();
        base.push_str(&package.name.namespace);
        base.push_str(":");
        base.push_str(&package.name.name);
        base.push_str("/");
        base.push_str(name);
        if let Some(version) = &package.name.version {
            base.push_str(&format!("@{version}"));
        }
        base
    }

    /// Attempts to locate a world given the "default" set of `packages` and the
    /// optional string specifier `world`.
    ///
    /// This method is intended to be used by bindings generation tools to
    /// select a world from either `packages` or a package in this `Resolve`.
    /// The `packages` list is a return value from methods such as
    /// [`push_path`](Resolve::push_path), [`push_dir`](Resolve::push_dir),
    /// [`push_file`](Resolve::push_file), [`push_group`](Resolve::push_group),
    /// or [`push_str`](Resolve::push_str). The return values of those methods
    /// are the "main package list" which is specified by the user and is used
    /// as a heuristic for world selection.
    ///
    /// If `world` is `None` then `packages` must have one entry and that
    /// package must have exactly one world. If this is the case then that world
    /// will be returned, otherwise an error will be returned.
    ///
    /// If `world` is `Some` then it can either be:
    ///
    /// * A kebab-name of a world such as `"the-world"`. In this situation
    ///   the `packages` list must have only a single entry. If `packages` has
    ///   no entries or more than one, or if the kebab-name does not exist in
    ///   the one package specified, then an error will be returned.
    ///
    /// * An ID-based form of a world which is selected within this `Resolve`,
    ///   for example `"wasi:http/proxy"`. In this situation the `packages`
    ///   array is ignored and the ID specified is use to lookup a package. Note
    ///   that a version does not need to be specified in this string if there's
    ///   only one package of the same name and it has a version. In this
    ///   situation the version can be omitted.
    ///
    /// If successful the corresponding `WorldId` is returned, otherwise an
    /// error is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use anyhow::Result;
    /// use wit_parser::Resolve;
    ///
    /// fn main() -> Result<()> {
    ///     let mut resolve = Resolve::default();
    ///
    ///     // For inputs which have a single package and only one world `None`
    ///     // can be specified.
    ///     let id = resolve.push_str(
    ///         "./my-test.wit",
    ///         r#"
    ///             package example:wit1;
    ///
    ///             world foo {
    ///                 // ...
    ///             }
    ///         "#,
    ///     )?;
    ///     assert!(resolve.select_world(id, None).is_ok());
    ///
    ///     // For inputs which have a single package and multiple worlds then
    ///     // a world must be specified.
    ///     let id = resolve.push_str(
    ///         "./my-test.wit",
    ///         r#"
    ///             package example:wit2;
    ///
    ///             world foo { /* ... */ }
    ///
    ///             world bar { /* ... */ }
    ///         "#,
    ///     )?;
    ///     assert!(resolve.select_world(id, None).is_err());
    ///     assert!(resolve.select_world(id, Some("foo")).is_ok());
    ///     assert!(resolve.select_world(id, Some("bar")).is_ok());
    ///
    ///     // For inputs which have more than one package then a fully
    ///     // qualified name must be specified.
    ///
    ///     // Note that the `ids` or `packages` argument is ignored if a fully
    ///     // qualified world specified is provided meaning previous worlds
    ///     // can be selected.
    ///     assert!(resolve.select_world(id, Some("example:wit1/foo")).is_ok());
    ///     assert!(resolve.select_world(id, Some("example:wit2/foo")).is_ok());
    ///
    ///     // When selecting with a version it's ok to drop the version when
    ///     // there's only a single copy of that package in `Resolve`.
    ///     resolve.push_str(
    ///         "./my-test.wit",
    ///         r#"
    ///             package example:wit5@1.0.0;
    ///
    ///             world foo { /* ... */ }
    ///         "#,
    ///     )?;
    ///     assert!(resolve.select_world(id, Some("example:wit5/foo")).is_ok());
    ///
    ///     // However when a single package has multiple versions in a resolve
    ///     // it's required to specify the version to select which one.
    ///     resolve.push_str(
    ///         "./my-test.wit",
    ///         r#"
    ///             package example:wit5@2.0.0;
    ///
    ///             world foo { /* ... */ }
    ///         "#,
    ///     )?;
    ///     assert!(resolve.select_world(id, Some("example:wit5/foo")).is_err());
    ///     assert!(resolve.select_world(id, Some("example:wit5/foo@1.0.0")).is_ok());
    ///     assert!(resolve.select_world(id, Some("example:wit5/foo@2.0.0")).is_ok());
    ///
    ///     Ok(())
    /// }
    /// ```
    pub fn select_world(&self, package: PackageId, world: Option<&str>) -> Result<WorldId> {
        let world_path = match world {
            Some(world) => Some(
                parse_use_path(world)
                    .with_context(|| format!("failed to parse world specifier `{world}`"))?,
            ),
            None => None,
        };

        let (pkg, world_name) = match world_path {
            Some(ParsedUsePath::Name(name)) => (package, name),
            Some(ParsedUsePath::Package(pkg, interface)) => {
                let pkg = match self.package_names.get(&pkg) {
                    Some(pkg) => *pkg,
                    None => {
                        let mut candidates = self.package_names.iter().filter(|(name, _)| {
                            pkg.version.is_none()
                                && pkg.name == name.name
                                && pkg.namespace == name.namespace
                                && name.version.is_some()
                        });
                        let candidate = candidates.next();
                        if let Some((c2, _)) = candidates.next() {
                            let (c1, _) = candidate.unwrap();
                            bail!(
                                "package name `{pkg}` is available at both \
                                 versions {} and {} but which is not specified",
                                c1.version.as_ref().unwrap(),
                                c2.version.as_ref().unwrap(),
                            );
                        }
                        match candidate {
                            Some((_, id)) => *id,
                            None => bail!("unknown package `{pkg}`"),
                        }
                    }
                };
                (pkg, interface.to_string())
            }
            None => {
                let pkg = &self.packages[package];
                let worlds = pkg
                    .worlds
                    .values()
                    .map(|world| (package, *world))
                    .collect::<Vec<_>>();

                match &worlds[..] {
                    [] => bail!("The main package `{}` contains no worlds", pkg.name),
                    [(_, world)] => return Ok(*world),
                    _ => bail!(
                        "multiple worlds found; one must be explicitly chosen:{}",
                        worlds
                            .iter()
                            .map(|(pkg, world)| format!(
                                "\n  {}/{}",
                                self.packages[*pkg].name, self.worlds[*world].name
                            ))
                            .collect::<String>()
                    ),
                }
            }
        };
        let pkg = &self.packages[pkg];
        pkg.worlds
            .get(&world_name)
            .copied()
            .ok_or_else(|| anyhow!("no world named `{world_name}` in package"))
    }

    /// Assigns a human readable name to the `WorldKey` specified.
    pub fn name_world_key(&self, key: &WorldKey) -> String {
        match key {
            WorldKey::Name(s) => s.to_string(),
            WorldKey::Interface(i) => self.id_of(*i).expect("unexpected anonymous interface"),
        }
    }

    /// Returns the interface that `id` uses a type from, if it uses a type from
    /// a different interface than `id` is defined within.
    ///
    /// If `id` is not a use-of-a-type or it's using a type in the same
    /// interface then `None` is returned.
    pub fn type_interface_dep(&self, id: TypeId) -> Option<InterfaceId> {
        let ty = &self.types[id];
        let dep = match ty.kind {
            TypeDefKind::Type(Type::Id(id)) => id,
            _ => return None,
        };
        let other = &self.types[dep];
        if ty.owner == other.owner {
            None
        } else {
            match other.owner {
                TypeOwner::Interface(id) => Some(id),
                _ => unreachable!(),
            }
        }
    }

    /// Returns an iterator of all interfaces that the interface `id` depends
    /// on.
    ///
    /// Interfaces may depend on others for type information to resolve type
    /// imports.
    ///
    /// Note that the returned iterator may yield the same interface as a
    /// dependency multiple times. Additionally only direct dependencies of `id`
    /// are yielded, not transitive dependencies.
    pub fn interface_direct_deps(&self, id: InterfaceId) -> impl Iterator<Item = InterfaceId> + '_ {
        self.interfaces[id]
            .types
            .iter()
            .filter_map(move |(_name, ty)| self.type_interface_dep(*ty))
    }

    /// Returns an iterator of all packages that the package `id` depends
    /// on.
    ///
    /// Packages may depend on others for type information to resolve type
    /// imports or interfaces to resolve worlds.
    ///
    /// Note that the returned iterator may yield the same package as a
    /// dependency multiple times. Additionally only direct dependencies of `id`
    /// are yielded, not transitive dependencies.
    pub fn package_direct_deps(&self, id: PackageId) -> impl Iterator<Item = PackageId> + '_ {
        let pkg = &self.packages[id];

        pkg.interfaces
            .iter()
            .flat_map(move |(_name, id)| self.interface_direct_deps(*id))
            .chain(pkg.worlds.iter().flat_map(move |(_name, id)| {
                let world = &self.worlds[*id];
                world
                    .imports
                    .iter()
                    .chain(world.exports.iter())
                    .filter_map(move |(_name, item)| match item {
                        WorldItem::Interface { id, .. } => Some(*id),
                        WorldItem::Function(_) => None,
                        WorldItem::Type(t) => self.type_interface_dep(*t),
                    })
            }))
            .filter_map(move |iface_id| {
                let pkg = self.interfaces[iface_id].package?;
                if pkg == id {
                    None
                } else {
                    Some(pkg)
                }
            })
    }

    /// Returns a topological ordering of packages contained in this `Resolve`.
    ///
    /// This returns a list of `PackageId` such that when visited in order it's
    /// guaranteed that all dependencies will have been defined by prior items
    /// in the list.
    pub fn topological_packages(&self) -> Vec<PackageId> {
        let mut pushed = vec![false; self.packages.len()];
        let mut order = Vec::new();
        for (id, _) in self.packages.iter() {
            self.build_topological_package_ordering(id, &mut pushed, &mut order);
        }
        order
    }

    fn build_topological_package_ordering(
        &self,
        id: PackageId,
        pushed: &mut Vec<bool>,
        order: &mut Vec<PackageId>,
    ) {
        if pushed[id.index()] {
            return;
        }
        for dep in self.package_direct_deps(id) {
            self.build_topological_package_ordering(dep, pushed, order);
        }
        order.push(id);
        pushed[id.index()] = true;
    }

    #[doc(hidden)]
    pub fn assert_valid(&self) {
        let mut package_interfaces = Vec::new();
        let mut package_worlds = Vec::new();
        for (id, pkg) in self.packages.iter() {
            let mut interfaces = HashSet::new();
            for (name, iface) in pkg.interfaces.iter() {
                assert!(interfaces.insert(*iface));
                let iface = &self.interfaces[*iface];
                assert_eq!(name, iface.name.as_ref().unwrap());
                assert_eq!(iface.package.unwrap(), id);
            }
            package_interfaces.push(pkg.interfaces.values().copied().collect::<HashSet<_>>());
            let mut worlds = HashSet::new();
            for (name, world) in pkg.worlds.iter() {
                assert!(worlds.insert(*world));
                let world = &self.worlds[*world];
                assert_eq!(*name, world.name);
                assert_eq!(world.package.unwrap(), id);
            }
            package_worlds.push(pkg.worlds.values().copied().collect::<HashSet<_>>());
        }

        let mut interface_types = Vec::new();
        for (id, iface) in self.interfaces.iter() {
            assert!(self.packages.get(iface.package.unwrap()).is_some());
            if iface.name.is_some() {
                assert!(package_interfaces[iface.package.unwrap().index()].contains(&id));
            }

            for (name, ty) in iface.types.iter() {
                let ty = &self.types[*ty];
                assert_eq!(ty.name.as_ref(), Some(name));
                assert_eq!(ty.owner, TypeOwner::Interface(id));
            }
            interface_types.push(iface.types.values().copied().collect::<HashSet<_>>());
            for (name, f) in iface.functions.iter() {
                assert_eq!(*name, f.name);
            }
        }

        let mut world_types = Vec::new();
        for (id, world) in self.worlds.iter() {
            log::debug!("validating world {}", &world.name);
            assert!(self.packages.get(world.package.unwrap()).is_some());
            assert!(package_worlds[world.package.unwrap().index()].contains(&id));

            let mut types = HashSet::new();
            for (name, item) in world.imports.iter().chain(world.exports.iter()) {
                log::debug!("validating world item: {}", self.name_world_key(name));
                match item {
                    WorldItem::Interface { .. } => {}
                    WorldItem::Function(f) => {
                        assert_eq!(f.name, name.clone().unwrap_name());
                    }
                    WorldItem::Type(ty) => {
                        assert!(types.insert(*ty));
                        let ty = &self.types[*ty];
                        assert_eq!(ty.name, Some(name.clone().unwrap_name()));

                        // TODO: `Resolve::merge_worlds` doesn't uphold this
                        // invariant, and that should be fixed.
                        if false {
                            assert_eq!(ty.owner, TypeOwner::World(id));
                        }
                    }
                }
            }
            world_types.push(types);
        }

        for (ty_id, ty) in self.types.iter() {
            match ty.owner {
                TypeOwner::Interface(id) => {
                    assert!(self.interfaces.get(id).is_some());
                    assert!(interface_types[id.index()].contains(&ty_id));
                }
                TypeOwner::World(id) => {
                    assert!(self.worlds.get(id).is_some());
                    assert!(world_types[id.index()].contains(&ty_id));
                }
                TypeOwner::None => {}
            }
        }

        self.assert_topologically_sorted();
    }

    fn assert_topologically_sorted(&self) {
        let mut positions = IndexMap::new();
        for id in self.topological_packages() {
            let pkg = &self.packages[id];
            log::debug!("pkg {}", pkg.name);
            let prev = positions.insert(Some(id), IndexSet::new());
            assert!(prev.is_none());
        }
        positions.insert(None, IndexSet::new());

        for (id, iface) in self.interfaces.iter() {
            log::debug!("iface {:?}", iface.name);
            let ok = positions.get_mut(&iface.package).unwrap().insert(id);
            assert!(ok);
        }

        for (_, world) in self.worlds.iter() {
            log::debug!("world {:?}", world.name);

            let my_package = world.package;
            let my_package_pos = positions.get_index_of(&my_package).unwrap();

            for (_, item) in world.imports.iter().chain(&world.exports) {
                let id = match item {
                    WorldItem::Interface { id, .. } => *id,
                    _ => continue,
                };
                let other_package = self.interfaces[id].package;
                let other_package_pos = positions.get_index_of(&other_package).unwrap();

                assert!(other_package_pos <= my_package_pos);
            }
        }

        for (_id, ty) in self.types.iter() {
            log::debug!("type {:?} {:?}", ty.name, ty.owner);
            let other_id = match ty.kind {
                TypeDefKind::Type(Type::Id(ty)) => ty,
                _ => continue,
            };
            let other = &self.types[other_id];
            if ty.kind == other.kind {
                continue;
            }
            let my_interface = match ty.owner {
                TypeOwner::Interface(id) => id,
                _ => continue,
            };
            let other_interface = match other.owner {
                TypeOwner::Interface(id) => id,
                _ => continue,
            };

            let my_package = self.interfaces[my_interface].package;
            let other_package = self.interfaces[other_interface].package;
            let my_package_pos = positions.get_index_of(&my_package).unwrap();
            let other_package_pos = positions.get_index_of(&other_package).unwrap();

            if my_package_pos == other_package_pos {
                let interfaces = &positions[&my_package];
                let my_interface_pos = interfaces.get_index_of(&my_interface).unwrap();
                let other_interface_pos = interfaces.get_index_of(&other_interface).unwrap();
                assert!(other_interface_pos <= my_interface_pos);
            } else {
                assert!(other_package_pos < my_package_pos);
            }
        }
    }

    fn include_stability(&self, stability: &Stability, pkg_id: &PackageId) -> Result<bool> {
        Ok(match stability {
            Stability::Unknown => true,
            // NOTE: deprecations are intentionally omitted -- an existing `@since` takes precedence over `@deprecated`
            Stability::Stable { since, .. } => {
                let Some(p) = self.packages.get(*pkg_id) else {
                    // We can't check much without a package (possibly dealing with an item in an `UnresolvedPackage`),
                    // @since version & deprecations can't be checked because there's no package version to compare to.
                    //
                    // Feature requirements on stabilized features are ignored in resolved packages, so we do the same here.
                    return Ok(true);
                };

                // Use of feature gating with version specifiers inside a package that is not versioned is not allowed
                let package_version = p.name.version.as_ref().with_context(|| format!("package [{}] contains a feature gate with a version specifier, so it must have a version", p.name))?;

                // If the version on the feature gate is:
                // - released, then we can include it
                // - unreleased, then we must check the feature (if present)
                ensure!(
                    since <= package_version,
                    "feature gate cannot reference unreleased version {since} of package [{}] (current version {package_version})",
                    p.name
                );

                true
            }
            Stability::Unstable { feature, .. } => {
                self.features.contains(feature) || self.all_features
            }
        })
    }
}

/// Structure returned by [`Resolve::merge`] which contains mappings from
/// old-ids to new-ids after the merge.
#[derive(Default)]
pub struct Remap {
    pub types: Vec<Option<TypeId>>,
    pub interfaces: Vec<Option<InterfaceId>>,
    pub worlds: Vec<Option<WorldId>>,
    pub packages: Vec<PackageId>,

    /// A cache of anonymous `own<T>` handles for resource types.
    ///
    /// The appending operation of `Remap` is the one responsible for
    /// translating references to `T` where `T` is a resource into `own<T>`
    /// instead. This map is used to deduplicate the `own<T>` types generated
    /// to generate as few as possible.
    ///
    /// The key of this map is the resource id `T` in the new resolve, and
    /// the value is the `own<T>` type pointing to `T`.
    own_handles: HashMap<TypeId, TypeId>,

    type_has_borrow: Vec<Option<bool>>,
}

fn apply_map<T>(map: &[Option<Id<T>>], id: Id<T>, desc: &str, span: Option<Span>) -> Result<Id<T>> {
    match map.get(id.index()) {
        Some(Some(id)) => Ok(*id),
        Some(None) => {
            let msg = format!(
                "found a reference to a {desc} which is excluded \
                 due to its feature not being activated"
            );
            match span {
                Some(span) => Err(Error::new(span, msg).into()),
                None => bail!("{msg}"),
            }
        }
        None => panic!("request to remap a {desc} that has not yet been registered"),
    }
}

impl Remap {
    pub fn map_type(&self, id: TypeId, span: Option<Span>) -> Result<TypeId> {
        apply_map(&self.types, id, "type", span)
    }

    pub fn map_interface(&self, id: InterfaceId, span: Option<Span>) -> Result<InterfaceId> {
        apply_map(&self.interfaces, id, "interface", span)
    }

    pub fn map_world(&self, id: WorldId, span: Option<Span>) -> Result<WorldId> {
        apply_map(&self.worlds, id, "world", span)
    }

    fn append(
        &mut self,
        resolve: &mut Resolve,
        unresolved: UnresolvedPackage,
    ) -> Result<PackageId> {
        self.process_foreign_deps(resolve, &unresolved)?;

        let foreign_types = self.types.len();
        let foreign_interfaces = self.interfaces.len();
        let foreign_worlds = self.worlds.len();

        let pkgid = resolve.packages.alloc(Package {
            name: unresolved.name.clone(),
            docs: unresolved.docs.clone(),
            interfaces: Default::default(),
            worlds: Default::default(),
        });
        let prev = resolve.package_names.insert(unresolved.name.clone(), pkgid);
        assert!(prev.is_none());

        // Copy over all types first, updating any intra-type references. Note
        // that types are sorted topologically which means this iteration
        // order should be sufficient. Also note though that the interface
        // owner of a type isn't updated here due to interfaces not being known
        // yet.
        assert_eq!(unresolved.types.len(), unresolved.type_spans.len());
        for ((id, mut ty), span) in unresolved
            .types
            .into_iter()
            .zip(&unresolved.type_spans)
            .skip(foreign_types)
        {
            if !resolve
                .include_stability(&ty.stability, &pkgid)
                .with_context(|| {
                    format!(
                        "failed to process feature gate for type [{}] in package [{}]",
                        ty.name.as_ref().map(String::as_str).unwrap_or("<unknown>"),
                        resolve.packages[pkgid].name,
                    )
                })?
            {
                self.types.push(None);
                continue;
            }

            self.update_typedef(resolve, &mut ty, Some(*span))?;
            let new_id = resolve.types.alloc(ty);
            assert_eq!(self.types.len(), id.index());

            let new_id = match resolve.types[new_id] {
                // If this is an `own<T>` handle then either replace it with a
                // preexisting `own<T>` handle which may have been generated in
                // `update_ty`. If that doesn't exist though then insert it into
                // the `own_handles` cache.
                TypeDef {
                    name: None,
                    owner: TypeOwner::None,
                    kind: TypeDefKind::Handle(Handle::Own(id)),
                    docs: _,
                    stability: _,
                } => *self.own_handles.entry(id).or_insert(new_id),

                // Everything not-related to `own<T>` doesn't get its ID
                // modified.
                _ => new_id,
            };
            self.types.push(Some(new_id));
        }

        // Next transfer all interfaces into `Resolve`, updating type ids
        // referenced along the way.
        assert_eq!(
            unresolved.interfaces.len(),
            unresolved.interface_spans.len()
        );
        for ((id, mut iface), span) in unresolved
            .interfaces
            .into_iter()
            .zip(&unresolved.interface_spans)
            .skip(foreign_interfaces)
        {
            if !resolve
                .include_stability(&iface.stability, &pkgid)
                .with_context(|| {
                    format!(
                        "failed to process feature gate for interface [{}] in package [{}]",
                        iface
                            .name
                            .as_ref()
                            .map(String::as_str)
                            .unwrap_or("<unknown>"),
                        resolve.packages[pkgid].name,
                    )
                })?
            {
                self.interfaces.push(None);
                continue;
            }
            assert!(iface.package.is_none());
            iface.package = Some(pkgid);
            self.update_interface(resolve, &mut iface, Some(span))?;
            let new_id = resolve.interfaces.alloc(iface);
            assert_eq!(self.interfaces.len(), id.index());
            self.interfaces.push(Some(new_id));
        }

        // Now that interfaces are identified go back through the types and
        // update their interface owners.
        for (i, id) in self.types.iter().enumerate().skip(foreign_types) {
            let id = match id {
                Some(id) => *id,
                None => continue,
            };
            match &mut resolve.types[id].owner {
                TypeOwner::Interface(id) => {
                    let span = unresolved.type_spans[i];
                    *id = self.map_interface(*id, Some(span))
                        .with_context(|| {
                            "this type is not gated by a feature but its interface is gated by a feature"
                        })?;
                }
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
        for ((id, mut world), span) in unresolved
            .worlds
            .into_iter()
            .zip(&unresolved.world_spans)
            .skip(foreign_worlds)
        {
            if !resolve
                .include_stability(&world.stability, &pkgid)
                .with_context(|| {
                    format!(
                        "failed to process feature gate for world [{}] in package [{}]",
                        world.name, resolve.packages[pkgid].name,
                    )
                })?
            {
                self.worlds.push(None);
                continue;
            }
            self.update_world(&mut world, resolve, &pkgid, &span)?;

            let new_id = resolve.worlds.alloc(world);
            assert_eq!(self.worlds.len(), id.index());
            self.worlds.push(Some(new_id));
        }

        // As with interfaces, now update the ids of world-owned types.
        for (i, id) in self.types.iter().enumerate().skip(foreign_types) {
            let id = match id {
                Some(id) => *id,
                None => continue,
            };
            match &mut resolve.types[id].owner {
                TypeOwner::World(id) => {
                    let span = unresolved.type_spans[i];
                    *id = self.map_world(*id, Some(span))
                        .with_context(|| {
                            "this type is not gated by a feature but its interface is gated by a feature"
                        })?;
                }
                TypeOwner::Interface(_) | TypeOwner::None => {}
            }
        }

        // Fixup "parent" ids now that everything has been identified
        for id in self.interfaces.iter().skip(foreign_interfaces) {
            let id = match id {
                Some(id) => *id,
                None => continue,
            };
            let iface = &mut resolve.interfaces[id];
            iface.package = Some(pkgid);
            if let Some(name) = &iface.name {
                let prev = resolve.packages[pkgid].interfaces.insert(name.clone(), id);
                assert!(prev.is_none());
            }
        }
        for id in self.worlds.iter().skip(foreign_worlds) {
            let id = match id {
                Some(id) => *id,
                None => continue,
            };
            let world = &mut resolve.worlds[id];
            world.package = Some(pkgid);
            let prev = resolve.packages[pkgid]
                .worlds
                .insert(world.name.clone(), id);
            assert!(prev.is_none());
        }
        Ok(pkgid)
    }

    fn process_foreign_deps(
        &mut self,
        resolve: &mut Resolve,
        unresolved: &UnresolvedPackage,
    ) -> Result<()> {
        // Invert the `foreign_deps` map to be keyed by world id to get
        // used in the loops below.
        let mut world_to_package = HashMap::new();
        let mut interface_to_package = HashMap::new();
        for (i, (pkg_name, worlds_or_ifaces)) in unresolved.foreign_deps.iter().enumerate() {
            for (name, item) in worlds_or_ifaces {
                match item {
                    AstItem::Interface(unresolved_interface_id) => {
                        let prev = interface_to_package.insert(
                            *unresolved_interface_id,
                            (pkg_name, name, unresolved.foreign_dep_spans[i]),
                        );
                        assert!(prev.is_none());
                    }
                    AstItem::World(unresolved_world_id) => {
                        let prev = world_to_package.insert(
                            *unresolved_world_id,
                            (pkg_name, name, unresolved.foreign_dep_spans[i]),
                        );
                        assert!(prev.is_none());
                    }
                }
            }
        }

        // Connect all interfaces referred to in `interface_to_package`, which
        // are at the front of `unresolved.interfaces`, to interfaces already
        // contained within `resolve`.
        self.process_foreign_interfaces(unresolved, &interface_to_package, resolve)?;

        // Connect all worlds referred to in `world_to_package`, which
        // are at the front of `unresolved.worlds`, to worlds already
        // contained within `resolve`.
        self.process_foreign_worlds(unresolved, &world_to_package, resolve)?;

        // Finally, iterate over all foreign-defined types and determine
        // what they map to.
        self.process_foreign_types(unresolved, resolve)?;

        for (id, span) in unresolved.required_resource_types.iter() {
            let mut id = self.map_type(*id, Some(*span))?;
            loop {
                match resolve.types[id].kind {
                    TypeDefKind::Type(Type::Id(i)) => id = i,
                    TypeDefKind::Resource => break,
                    _ => bail!(Error::new(
                        *span,
                        format!("type used in a handle must be a resource"),
                    )),
                }
            }
        }

        Ok(())
    }

    fn process_foreign_interfaces(
        &mut self,
        unresolved: &UnresolvedPackage,
        interface_to_package: &HashMap<InterfaceId, (&PackageName, &String, Span)>,
        resolve: &mut Resolve,
    ) -> Result<(), anyhow::Error> {
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
                .ok_or_else(|| Error::new(span, "package not found"))?;

            // Functions can't be imported so this should be empty.
            assert!(unresolved_iface.functions.is_empty());

            let pkg = &resolve.packages[pkgid];
            let span = &unresolved.interface_spans[unresolved_iface_id.index()];
            let iface_id = pkg
                .interfaces
                .get(interface)
                .copied()
                .ok_or_else(|| Error::new(span.span, "interface not found in package"))?;
            assert_eq!(self.interfaces.len(), unresolved_iface_id.index());
            self.interfaces.push(Some(iface_id));
        }
        for (id, _) in unresolved.interfaces.iter().skip(self.interfaces.len()) {
            assert!(
                interface_to_package.get(&id).is_none(),
                "found foreign interface after local interface"
            );
        }
        Ok(())
    }

    fn process_foreign_worlds(
        &mut self,
        unresolved: &UnresolvedPackage,
        world_to_package: &HashMap<WorldId, (&PackageName, &String, Span)>,
        resolve: &mut Resolve,
    ) -> Result<(), anyhow::Error> {
        for (unresolved_world_id, _) in unresolved.worlds.iter() {
            let (pkg_name, world, span) = match world_to_package.get(&unresolved_world_id) {
                Some(items) => *items,
                // Same as above, all worlds are foreign until we find a
                // non-foreign one.
                None => break,
            };

            let pkgid = resolve
                .package_names
                .get(pkg_name)
                .copied()
                .ok_or_else(|| Error::new(span, "package not found"))?;
            let pkg = &resolve.packages[pkgid];
            let span = &unresolved.world_spans[unresolved_world_id.index()];
            let world_id = pkg
                .worlds
                .get(world)
                .copied()
                .ok_or_else(|| Error::new(span.span, "world not found in package"))?;
            assert_eq!(self.worlds.len(), unresolved_world_id.index());
            self.worlds.push(Some(world_id));
        }
        for (id, _) in unresolved.worlds.iter().skip(self.worlds.len()) {
            assert!(
                world_to_package.get(&id).is_none(),
                "found foreign world after local world"
            );
        }
        Ok(())
    }

    fn process_foreign_types(
        &mut self,
        unresolved: &UnresolvedPackage,
        resolve: &mut Resolve,
    ) -> Result<(), anyhow::Error> {
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
            let iface_id = self.map_interface(unresolved_iface_id, None)?;
            let name = unresolved_ty.name.as_ref().unwrap();
            let span = unresolved.unknown_type_spans[unresolved_type_id.index()];
            let type_id = *resolve.interfaces[iface_id]
                .types
                .get(name)
                .ok_or_else(|| {
                    Error::new(span, format!("type `{name}` not defined in interface"))
                })?;
            assert_eq!(self.types.len(), unresolved_type_id.index());
            self.types.push(Some(type_id));
        }
        for (_, ty) in unresolved.types.iter().skip(self.types.len()) {
            if let TypeDefKind::Unknown = ty.kind {
                panic!("unknown type after defined type");
            }
        }
        Ok(())
    }

    fn update_typedef(
        &mut self,
        resolve: &mut Resolve,
        ty: &mut TypeDef,
        span: Option<Span>,
    ) -> Result<()> {
        // NB: note that `ty.owner` is not updated here since interfaces
        // haven't been mapped yet and that's done in a separate step.
        use crate::TypeDefKind::*;
        match &mut ty.kind {
            Handle(handle) => match handle {
                crate::Handle::Own(ty) | crate::Handle::Borrow(ty) => {
                    self.update_type_id(ty, span)?
                }
            },
            Resource => {}
            Record(r) => {
                for field in r.fields.iter_mut() {
                    self.update_ty(resolve, &mut field.ty, span)
                        .with_context(|| format!("failed to update field `{}`", field.name))?;
                }
            }
            Tuple(t) => {
                for ty in t.types.iter_mut() {
                    self.update_ty(resolve, ty, span)?;
                }
            }
            Variant(v) => {
                for case in v.cases.iter_mut() {
                    if let Some(t) = &mut case.ty {
                        self.update_ty(resolve, t, span)?;
                    }
                }
            }
            Option(t) => self.update_ty(resolve, t, span)?,
            Result(r) => {
                if let Some(ty) = &mut r.ok {
                    self.update_ty(resolve, ty, span)?;
                }
                if let Some(ty) = &mut r.err {
                    self.update_ty(resolve, ty, span)?;
                }
            }
            List(t) => self.update_ty(resolve, t, span)?,
            Future(Some(t)) => self.update_ty(resolve, t, span)?,
            Stream(t) => {
                if let Some(ty) = &mut t.element {
                    self.update_ty(resolve, ty, span)?;
                }
                if let Some(ty) = &mut t.end {
                    self.update_ty(resolve, ty, span)?;
                }
            }

            // Note that `update_ty` is specifically not used here as typedefs
            // because for the `type a = b` form that doesn't force `a` to be a
            // handle type if `b` is a resource type, instead `a` is
            // simultaneously usable as a resource and a handle type
            Type(crate::Type::Id(id)) => self.update_type_id(id, span)?,
            Type(_) => {}

            // nothing to do for these as they're just names or empty
            Flags(_) | Enum(_) | Future(None) => {}

            Unknown => unreachable!(),
        }

        Ok(())
    }

    fn update_ty(
        &mut self,
        resolve: &mut Resolve,
        ty: &mut Type,
        span: Option<Span>,
    ) -> Result<()> {
        let id = match ty {
            Type::Id(id) => id,
            _ => return Ok(()),
        };
        self.update_type_id(id, span)?;

        // If `id` points to a `Resource` type then this means that what was
        // just discovered was a reference to what will implicitly become an
        // `own<T>` handle. This `own` handle is implicitly allocated here
        // and handled during the merging process.
        let mut cur = *id;
        let points_to_resource = loop {
            match resolve.types[cur].kind {
                TypeDefKind::Type(Type::Id(id)) => cur = id,
                TypeDefKind::Resource => break true,
                _ => break false,
            }
        };

        if points_to_resource {
            *id = *self.own_handles.entry(*id).or_insert_with(|| {
                resolve.types.alloc(TypeDef {
                    name: None,
                    owner: TypeOwner::None,
                    kind: TypeDefKind::Handle(Handle::Own(*id)),
                    docs: Default::default(),
                    stability: Default::default(),
                })
            });
        }
        Ok(())
    }

    fn update_type_id(&self, id: &mut TypeId, span: Option<Span>) -> Result<()> {
        *id = self.map_type(*id, span)?;
        Ok(())
    }

    fn update_interface(
        &mut self,
        resolve: &mut Resolve,
        iface: &mut Interface,
        spans: Option<&InterfaceSpan>,
    ) -> Result<()> {
        iface.types.retain(|_, ty| self.types[ty.index()].is_some());
        let iface_pkg_id = iface.package.as_ref().unwrap_or_else(|| {
            panic!(
                "unexpectedly missing package on interface [{}]",
                iface
                    .name
                    .as_ref()
                    .map(String::as_str)
                    .unwrap_or("<unknown>"),
            )
        });

        // NB: note that `iface.doc` is not updated here since interfaces
        // haven't been mapped yet and that's done in a separate step.
        for (_name, ty) in iface.types.iter_mut() {
            self.update_type_id(ty, spans.map(|s| s.span))?;
        }
        if let Some(spans) = spans {
            assert_eq!(iface.functions.len(), spans.funcs.len());
        }
        for (i, (func_name, func)) in iface.functions.iter_mut().enumerate() {
            if !resolve
                .include_stability(&func.stability, iface_pkg_id)
                .with_context(|| {
                    format!(
                        "failed to process feature gate for function [{func_name}] in package [{}]",
                        resolve.packages[*iface_pkg_id].name,
                    )
                })?
            {
                continue;
            }
            let span = spans.map(|s| s.funcs[i]);
            self.update_function(resolve, func, span)
                .with_context(|| format!("failed to update function `{}`", func.name))?;
        }

        // Filter out all of the existing functions in interface which fail the
        // `include_stability()` check, as they shouldn't be available.
        for (name, func) in mem::take(&mut iface.functions) {
            if resolve.include_stability(&func.stability, iface_pkg_id)? {
                iface.functions.insert(name, func);
            }
        }

        Ok(())
    }

    fn update_function(
        &mut self,
        resolve: &mut Resolve,
        func: &mut Function,
        span: Option<Span>,
    ) -> Result<()> {
        match &mut func.kind {
            FunctionKind::Freestanding => {}
            FunctionKind::Method(id) | FunctionKind::Constructor(id) | FunctionKind::Static(id) => {
                self.update_type_id(id, span)?;
            }
        }
        for (_, ty) in func.params.iter_mut() {
            self.update_ty(resolve, ty, span)?;
        }
        match &mut func.results {
            Results::Named(named) => {
                for (_, ty) in named.iter_mut() {
                    self.update_ty(resolve, ty, span)?;
                }
            }
            Results::Anon(ty) => self.update_ty(resolve, ty, span)?,
        }

        for ty in func.results.iter_types() {
            if !self.type_has_borrow(resolve, ty) {
                continue;
            }
            match span {
                Some(span) => {
                    bail!(Error::new(
                        span,
                        format!(
                            "function returns a type which contains \
                             a `borrow<T>` which is not supported"
                        )
                    ))
                }
                None => unreachable!(),
            }
        }

        Ok(())
    }

    fn update_world(
        &mut self,
        world: &mut World,
        resolve: &mut Resolve,
        pkg_id: &PackageId,
        spans: &WorldSpan,
    ) -> Result<()> {
        // NB: this function is more more complicated than the prior versions
        // of merging an item because this is the location that elaboration of
        // imports/exports of a world are fully resolved. With full transitive
        // knowledge of all interfaces a worlds imports, for example, are
        // expanded fully to ensure that all transitive items are necessarily
        // imported.
        assert_eq!(world.imports.len(), spans.imports.len());
        assert_eq!(world.exports.len(), spans.exports.len());

        // First up, process all the `imports` of the world. Note that this
        // starts by gutting the list of imports stored in `world` to get
        // rebuilt iteratively below.
        //
        // Here each import of an interface is recorded and then additionally
        // explicitly named imports of interfaces are recorded as well for
        // determining names later on.
        let mut import_funcs = Vec::new();
        let mut import_types = Vec::new();
        for ((mut name, mut item), span) in mem::take(&mut world.imports)
            .into_iter()
            .zip(&spans.imports)
        {
            // Update the `id` eagerly here so `item.stability(..)` below
            // works.
            if let WorldItem::Type(id) = &mut item {
                *id = self.map_type(*id, Some(*span))?;
            }
            let stability = item.stability(resolve);
            if !resolve
                .include_stability(stability, pkg_id)
                .with_context(|| {
                    format!(
                        "failed to process imported world item type [{}] in package [{}]",
                        resolve.name_world_key(&name),
                        resolve.packages[*pkg_id].name,
                    )
                })?
            {
                continue;
            }
            self.update_world_key(&mut name, Some(*span))?;
            match item {
                WorldItem::Interface { id, stability } => {
                    let id = self.map_interface(id, Some(*span))?;
                    self.add_world_import(resolve, world, name, id, &stability);
                }
                WorldItem::Function(mut f) => {
                    self.update_function(resolve, &mut f, Some(*span))?;
                    import_funcs.push((name.unwrap_name(), f, *span));
                }
                WorldItem::Type(id) => {
                    import_types.push((name.unwrap_name(), id, *span));
                }
            }
        }

        for (_name, id, _span) in import_types.iter() {
            if let TypeDefKind::Type(Type::Id(other)) = resolve.types[*id].kind {
                if let TypeOwner::Interface(owner) = resolve.types[other].owner {
                    let name = WorldKey::Interface(owner);
                    self.add_world_import(
                        resolve,
                        world,
                        name,
                        owner,
                        &resolve.types[*id].stability,
                    );
                }
            }
        }

        let mut export_funcs = Vec::new();
        let mut export_interfaces = IndexMap::new();
        for ((mut name, item), span) in mem::take(&mut world.exports)
            .into_iter()
            .zip(&spans.exports)
        {
            let stability = item.stability(resolve);
            if !resolve
                .include_stability(stability, pkg_id)
                .with_context(|| {
                    format!(
                        "failed to process feature gate for exported item [{}] in package [{}]",
                        resolve.name_world_key(&name),
                        resolve.packages[*pkg_id].name,
                    )
                })?
            {
                continue;
            }
            self.update_world_key(&mut name, Some(*span))?;
            match item {
                WorldItem::Interface { id, stability } => {
                    let id = self.map_interface(id, Some(*span))?;
                    let prev = export_interfaces.insert(id, (name, *span, stability));
                    assert!(prev.is_none());
                }
                WorldItem::Function(mut f) => {
                    self.update_function(resolve, &mut f, Some(*span))?;
                    let name = match name {
                        WorldKey::Name(name) => name,
                        WorldKey::Interface(_) => unreachable!(),
                    };
                    export_funcs.push((name, f, *span));
                }
                WorldItem::Type(_) => unreachable!(),
            }
        }

        self.add_world_exports(resolve, world, &export_interfaces)?;

        // Resolve all includes of the world
        assert_eq!(world.includes.len(), spans.includes.len());
        let includes = mem::take(&mut world.includes);
        let include_names = mem::take(&mut world.include_names);
        for (((stability, include_world), span), names) in includes
            .into_iter()
            .zip(&spans.includes)
            .zip(&include_names)
        {
            if !resolve
                .include_stability(&stability, pkg_id)
                .with_context(|| {
                    format!(
                        "failed to process feature gate for included world [{}] in package [{}]",
                        resolve.worlds[include_world].name.as_str(),
                        resolve.packages[*pkg_id].name
                    )
                })?
            {
                continue;
            }
            self.resolve_include(world, include_world, names, *span, resolve)?;
        }

        for (name, id, span) in import_types {
            let prev = world
                .imports
                .insert(WorldKey::Name(name.clone()), WorldItem::Type(id));
            if prev.is_some() {
                bail!(Error::new(
                    span,
                    format!("export of type `{name}` shadows previously imported interface"),
                ))
            }
        }

        for (name, func, span) in import_funcs {
            let prev = world
                .imports
                .insert(WorldKey::Name(name.clone()), WorldItem::Function(func));
            if prev.is_some() {
                bail!(Error::new(
                    span,
                    format!("import of function `{name}` shadows previously imported interface"),
                ))
            }
        }

        for (name, func, span) in export_funcs {
            let prev = world
                .exports
                .insert(WorldKey::Name(name.clone()), WorldItem::Function(func));
            if prev.is_some() {
                bail!(Error::new(
                    span,
                    format!("export of function `{name}` shadows previously exported interface"),
                ))
            }
        }

        // After all that sort functions in exports to come before interfaces in
        // exports. This is not strictly required for correctness but make
        // iterating over a world much easier for consumers. Exported functions
        // are guaranteed to use types from either imported interfaces or
        // imported types into the world itself. Currently there is no means by
        // which an export function, at the root, can use types from any other
        // exported interfaces (can't be modeled syntactically in WIT). This
        // means that by placing all functions first it guarantees that visitors
        // which visit imports first then exports will walk over types and
        // references in the order of what they're actually using.
        //
        // For example if an interface is both imported and exported and an
        // exported function uses a type from that interface, then a visitor
        // should visit the imported interface, then the exported function, then
        // the exported interface. That way tables about "where was this type
        // defined" will be correct as the last-inserted item will be used and
        // correctly account for this.
        world.exports.sort_by(|_, a, _, b| {
            let rank = |item: &WorldItem| match item {
                WorldItem::Type(_) => unreachable!(),
                WorldItem::Function(_) => 0,
                WorldItem::Interface { .. } => 1,
            };
            rank(a).cmp(&rank(b))
        });

        log::trace!("imports = {:?}", world.imports);
        log::trace!("exports = {:?}", world.exports);

        Ok(())
    }

    fn update_world_key(&self, key: &mut WorldKey, span: Option<Span>) -> Result<()> {
        match key {
            WorldKey::Name(_) => {}
            WorldKey::Interface(id) => {
                *id = self.map_interface(*id, span)?;
            }
        }
        Ok(())
    }

    fn add_world_import(
        &self,
        resolve: &Resolve,
        world: &mut World,
        key: WorldKey,
        id: InterfaceId,
        stability: &Stability,
    ) {
        if world.imports.contains_key(&key) {
            return;
        }
        for dep in resolve.interface_direct_deps(id) {
            self.add_world_import(resolve, world, WorldKey::Interface(dep), dep, stability);
        }
        let prev = world.imports.insert(
            key,
            WorldItem::Interface {
                id,
                stability: stability.clone(),
            },
        );
        assert!(prev.is_none());
    }

    /// This function adds all of the interfaces in `export_interfaces` to the
    /// list of exports of the `world` specified.
    ///
    /// This method is more involved than adding imports because it is fallible.
    /// Chiefly what can happen is that the dependencies of all exports must be
    /// satisfied by other exports or imports, but not both. For example given a
    /// situation such as:
    ///
    /// ```wit
    /// interface a {
    ///     type t = u32
    /// }
    /// interface b {
    ///     use a.{t}
    /// }
    /// interface c {
    ///     use a.{t}
    ///     use b.{t as t2}
    /// }
    /// ```
    ///
    /// where `c` depends on `b` and `a` where `b` depends on `a`, then the
    /// purpose of this method is to reject this world:
    ///
    /// ```wit
    /// world foo {
    ///     export a
    ///     export c
    /// }
    /// ```
    ///
    /// The reasoning here is unfortunately subtle and is additionally the
    /// subject of WebAssembly/component-model#208. Effectively the `c`
    /// interface depends on `b`, but it's not listed explicitly as an import,
    /// so it's then implicitly added as an import. This then transitively
    /// depends on `a` so it's also added as an import. At this point though `c`
    /// also depends on `a`, and it's also exported, so naively it should depend
    /// on the export and not implicitly add an import. This means though that
    /// `c` has access to two copies of `a`, one imported and one exported. This
    /// is not valid, especially in the face of resource types.
    ///
    /// Overall this method is tasked with rejecting the above world by walking
    /// over all the exports and adding their dependencies. Each dependency is
    /// recorded with whether it's required to be imported, and then if an
    /// export is added for something that's required to be an error then the
    /// operation fails.
    fn add_world_exports(
        &self,
        resolve: &Resolve,
        world: &mut World,
        export_interfaces: &IndexMap<InterfaceId, (WorldKey, Span, Stability)>,
    ) -> Result<()> {
        let mut required_imports = HashSet::new();
        for (id, (key, span, stability)) in export_interfaces.iter() {
            let ok = add_world_export(
                resolve,
                world,
                export_interfaces,
                &mut required_imports,
                *id,
                key,
                true,
                &stability,
            );
            if !ok {
                bail!(Error::new(
                    *span,
                    // FIXME: this is not a great error message and basically no
                    // one will know what to do when it gets printed. Improving
                    // this error message, however, is a chunk of work that may
                    // not be best spent doing this at this time, so I'm writing
                    // this comment instead.
                    //
                    // More-or-less what should happen here is that a "path"
                    // from this interface to the conflicting interface should
                    // be printed. It should be explained why an import is being
                    // injected, why that's conflicting with an export, and
                    // ideally with a suggestion of "add this interface to the
                    // export list to fix this error".
                    //
                    // That's a lot of info that's not easy to get at without
                    // more refactoring, so it's left to a future date in the
                    // hopes that most folks won't actually run into this for
                    // the time being.
                    format!(
                        "interface transitively depends on an interface in \
                         incompatible ways",
                    ),
                ));
            }
        }
        return Ok(());

        fn add_world_export(
            resolve: &Resolve,
            world: &mut World,
            export_interfaces: &IndexMap<InterfaceId, (WorldKey, Span, Stability)>,
            required_imports: &mut HashSet<InterfaceId>,
            id: InterfaceId,
            key: &WorldKey,
            add_export: bool,
            stability: &Stability,
        ) -> bool {
            if world.exports.contains_key(key) {
                if add_export {
                    return true;
                } else {
                    return false;
                }
            }
            // If this is an import and it's already in the `required_imports`
            // set then we can skip it as we've already visited this interface.
            if !add_export && required_imports.contains(&id) {
                return true;
            }
            let ok = resolve.interface_direct_deps(id).all(|dep| {
                let key = WorldKey::Interface(dep);
                let add_export = add_export && export_interfaces.contains_key(&dep);
                add_world_export(
                    resolve,
                    world,
                    export_interfaces,
                    required_imports,
                    dep,
                    &key,
                    add_export,
                    stability,
                )
            });
            if !ok {
                return false;
            }
            let item = WorldItem::Interface {
                id,
                stability: stability.clone(),
            };
            if add_export {
                if required_imports.contains(&id) {
                    return false;
                }
                world.exports.insert(key.clone(), item);
            } else {
                required_imports.insert(id);
                world.imports.insert(key.clone(), item);
            }
            true
        }
    }

    fn resolve_include(
        &self,
        world: &mut World,
        include_world: WorldId,
        names: &[IncludeName],
        span: Span,
        resolve: &Resolve,
    ) -> Result<()> {
        let include_world_id = self.map_world(include_world, Some(span))?;
        let include_world = &resolve.worlds[include_world_id];
        let mut names_ = names.to_owned();

        // remove all imports and exports that match the names we're including
        for import in include_world.imports.iter() {
            self.remove_matching_name(import, &mut names_);
        }
        for export in include_world.exports.iter() {
            self.remove_matching_name(export, &mut names_);
        }
        if !names_.is_empty() {
            bail!(Error::new(
                span,
                format!("no import or export kebab-name `{}`. Note that an ID does not support renaming", names_[0].name),
            ));
        }

        // copy the imports and exports from the included world into the current world
        for import in include_world.imports.iter() {
            self.resolve_include_item(names, &mut world.imports, import, span, "import")?;
        }

        for export in include_world.exports.iter() {
            self.resolve_include_item(names, &mut world.exports, export, span, "export")?;
        }
        Ok(())
    }

    fn resolve_include_item(
        &self,
        names: &[IncludeName],
        items: &mut IndexMap<WorldKey, WorldItem>,
        item: (&WorldKey, &WorldItem),
        span: Span,
        item_type: &str,
    ) -> Result<()> {
        match item.0 {
            WorldKey::Name(n) => {
                let n = if let Some(found) = names
                    .into_iter()
                    .find(|include_name| include_name.name == n.clone())
                {
                    found.as_.clone()
                } else {
                    n.clone()
                };

                let prev = items.insert(WorldKey::Name(n.clone()), item.1.clone());
                if prev.is_some() {
                    bail!(Error::new(
                        span,
                        format!("{item_type} of `{n}` shadows previously {item_type}ed items"),
                    ))
                }
            }
            key @ WorldKey::Interface(_) => {
                let prev = items.entry(key.clone()).or_insert(item.1.clone());
                match (&item.1, prev) {
                    (
                        WorldItem::Interface {
                            id: aid,
                            stability: astability,
                        },
                        WorldItem::Interface {
                            id: bid,
                            stability: bstability,
                        },
                    ) => {
                        assert_eq!(*aid, *bid);
                        update_stability(astability, bstability)?;
                    }
                    (WorldItem::Interface { .. }, _) => unreachable!(),
                    (WorldItem::Function(_), _) => unreachable!(),
                    (WorldItem::Type(_), _) => unreachable!(),
                }
            }
        };
        Ok(())
    }

    fn remove_matching_name(&self, item: (&WorldKey, &WorldItem), names: &mut Vec<IncludeName>) {
        match item.0 {
            WorldKey::Name(n) => {
                names.retain(|name| name.name != n.clone());
            }
            _ => {}
        }
    }

    fn type_has_borrow(&mut self, resolve: &Resolve, ty: &Type) -> bool {
        let id = match ty {
            Type::Id(id) => *id,
            _ => return false,
        };

        if let Some(Some(has_borrow)) = self.type_has_borrow.get(id.index()) {
            return *has_borrow;
        }

        let result = self.typedef_has_borrow(resolve, &resolve.types[id]);
        if self.type_has_borrow.len() <= id.index() {
            self.type_has_borrow.resize(id.index() + 1, None);
        }
        self.type_has_borrow[id.index()] = Some(result);
        result
    }

    fn typedef_has_borrow(&mut self, resolve: &Resolve, ty: &TypeDef) -> bool {
        match &ty.kind {
            TypeDefKind::Type(t) => self.type_has_borrow(resolve, t),
            TypeDefKind::Variant(v) => v
                .cases
                .iter()
                .filter_map(|case| case.ty.as_ref())
                .any(|ty| self.type_has_borrow(resolve, ty)),
            TypeDefKind::Handle(Handle::Borrow(_)) => true,
            TypeDefKind::Handle(Handle::Own(_)) => false,
            TypeDefKind::Resource => false,
            TypeDefKind::Record(r) => r
                .fields
                .iter()
                .any(|case| self.type_has_borrow(resolve, &case.ty)),
            TypeDefKind::Flags(_) => false,
            TypeDefKind::Tuple(t) => t.types.iter().any(|t| self.type_has_borrow(resolve, t)),
            TypeDefKind::Enum(_) => false,
            TypeDefKind::List(ty) | TypeDefKind::Future(Some(ty)) | TypeDefKind::Option(ty) => {
                self.type_has_borrow(resolve, ty)
            }
            TypeDefKind::Result(r) => [&r.ok, &r.err]
                .iter()
                .filter_map(|t| t.as_ref())
                .any(|t| self.type_has_borrow(resolve, t)),
            TypeDefKind::Stream(r) => [&r.element, &r.end]
                .iter()
                .filter_map(|t| t.as_ref())
                .any(|t| self.type_has_borrow(resolve, t)),
            TypeDefKind::Future(None) => false,
            TypeDefKind::Unknown => unreachable!(),
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
    fn new(from: &'a Resolve, into: &'a Resolve) -> MergeMap<'a> {
        MergeMap {
            package_map: Default::default(),
            interface_map: Default::default(),
            type_map: Default::default(),
            world_map: Default::default(),
            interfaces_to_add: Default::default(),
            worlds_to_add: Default::default(),
            from,
            into,
        }
    }

    fn build(&mut self) -> Result<()> {
        for from_id in self.from.topological_packages() {
            let from = &self.from.packages[from_id];
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
                    log::trace!("adding unique interface {}", name);
                    self.interfaces_to_add
                        .push((name.clone(), into_id, *from_interface_id));
                    continue;
                }
            };

            log::trace!("merging duplicate interfaces {}", name);
            self.build_interface(*from_interface_id, into_interface_id)
                .with_context(|| format!("failed to merge interface `{name}`"))?;
        }

        for (name, from_world_id) in from.worlds.iter() {
            let into_world_id = match into.worlds.get(name) {
                Some(id) => *id,
                None => {
                    log::trace!("adding unique world {}", name);
                    self.worlds_to_add
                        .push((name.clone(), into_id, *from_world_id));
                    continue;
                }
            };

            log::trace!("merging duplicate worlds {}", name);
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

            self.build_type_id(*from_type_id, into_type_id)
                .with_context(|| format!("mismatch in type `{name}`"))?;
        }

        for (name, from_func) in from_interface.functions.iter() {
            let into_func = match into_interface.functions.get(name) {
                Some(func) => func,
                None => bail!("expected function `{name}` to be present"),
            };
            self.build_function(from_func, into_func)
                .with_context(|| format!("mismatch in function `{name}`"))?;
        }

        Ok(())
    }

    fn build_type_id(&mut self, from_id: TypeId, into_id: TypeId) -> Result<()> {
        // FIXME: ideally the types should be "structurally
        // equal" but that's not trivial to do in the face of
        // resources.
        let _ = from_id;
        let _ = into_id;
        Ok(())
    }

    fn build_type(&mut self, from_ty: &Type, into_ty: &Type) -> Result<()> {
        match (from_ty, into_ty) {
            (Type::Id(from), Type::Id(into)) => {
                self.build_type_id(*from, *into)?;
            }
            (from, into) if from != into => bail!("different kinds of types"),
            _ => {}
        }
        Ok(())
    }

    fn build_function(&mut self, from_func: &Function, into_func: &Function) -> Result<()> {
        if from_func.name != into_func.name {
            bail!(
                "different function names `{}` and `{}`",
                from_func.name,
                into_func.name
            );
        }
        match (&from_func.kind, &into_func.kind) {
            (FunctionKind::Freestanding, FunctionKind::Freestanding) => {}

            (FunctionKind::Method(from), FunctionKind::Method(into))
            | (FunctionKind::Constructor(from), FunctionKind::Constructor(into))
            | (FunctionKind::Static(from), FunctionKind::Static(into)) => self
                .build_type_id(*from, *into)
                .context("different function kind types")?,

            (FunctionKind::Method(_), _)
            | (FunctionKind::Constructor(_), _)
            | (FunctionKind::Static(_), _)
            | (FunctionKind::Freestanding, _) => {
                bail!("different function kind types")
            }
        }

        if from_func.params.len() != into_func.params.len() {
            bail!("different number of function parameters");
        }
        for ((from_name, from_ty), (into_name, into_ty)) in
            from_func.params.iter().zip(&into_func.params)
        {
            if from_name != into_name {
                bail!("different function parameter names: {from_name} != {into_name}");
            }
            self.build_type(from_ty, into_ty)
                .with_context(|| format!("different function parameter types for `{from_name}`"))?;
        }
        if from_func.results.len() != into_func.results.len() {
            bail!("different number of function results");
        }
        for (from_ty, into_ty) in from_func
            .results
            .iter_types()
            .zip(into_func.results.iter_types())
        {
            self.build_type(from_ty, into_ty)
                .context("different function result types")?;
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
            (WorldItem::Interface { id: from, .. }, WorldItem::Interface { id: into, .. }) => {
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

            (WorldItem::Interface { .. }, _)
            | (WorldItem::Function(_), _)
            | (WorldItem::Type(_), _) => {
                bail!("world items do not have the same type")
            }
        }
        Ok(())
    }
}

/// Updates stability annotations when merging `from` into `into`.
///
/// This is done to keep up-to-date stability information if possible.
/// Components for example don't carry stability information but WIT does so
/// this tries to move from "unknown" to stable/unstable if possible.
fn update_stability(from: &Stability, into: &mut Stability) -> Result<()> {
    // If `from` is unknown or the two stability annotations are equal then
    // there's nothing to do here.
    if from == into || from.is_unknown() {
        return Ok(());
    }
    // Otherwise if `into` is unknown then inherit the stability listed in
    // `from`.
    if into.is_unknown() {
        *into = from.clone();
        return Ok(());
    }

    // Failing all that this means that the two attributes are different so
    // generate an error.
    bail!("mismatch in stability attributes")
}

#[cfg(test)]
mod tests {
    use crate::Resolve;
    use anyhow::Result;

    #[test]
    fn select_world() -> Result<()> {
        let mut resolve = Resolve::default();
        resolve.push_str(
            "test.wit",
            r#"
                package foo:bar@0.1.0;

                world foo {}
            "#,
        )?;
        resolve.push_str(
            "test.wit",
            r#"
                package foo:baz@0.1.0;

                world foo {}
            "#,
        )?;
        resolve.push_str(
            "test.wit",
            r#"
                package foo:baz@0.2.0;

                world foo {}
            "#,
        )?;

        let dummy = resolve.push_str(
            "test.wit",
            r#"
                package foo:dummy;

                world foo {}
            "#,
        )?;

        assert!(resolve.select_world(dummy, None).is_ok());
        assert!(resolve.select_world(dummy, Some("xx")).is_err());
        assert!(resolve.select_world(dummy, Some("")).is_err());
        assert!(resolve.select_world(dummy, Some("foo:bar/foo")).is_ok());
        assert!(resolve
            .select_world(dummy, Some("foo:bar/foo@0.1.0"))
            .is_ok());
        assert!(resolve.select_world(dummy, Some("foo:baz/foo")).is_err());
        assert!(resolve
            .select_world(dummy, Some("foo:baz/foo@0.1.0"))
            .is_ok());
        assert!(resolve
            .select_world(dummy, Some("foo:baz/foo@0.2.0"))
            .is_ok());
        Ok(())
    }
}
