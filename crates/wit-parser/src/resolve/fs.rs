//! Filesystem operations for [`Resolve`].

use alloc::format;
use std::path::Path;
use std::vec::Vec;

use anyhow::{Context, Result, bail};

use super::{PackageSources, Resolve};
use crate::UnresolvedPackageGroup;

/// All the sources used during resolving a directory or path.
#[derive(Clone, Debug)]
pub struct PackageSourceMap {
    inner: PackageSources,
}

impl PackageSourceMap {
    fn from_single_source(package_id: super::PackageId, source: &Path) -> Result<Self> {
        let path_str = source
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("path is not valid utf-8: {:?}", source))?;
        Ok(Self {
            inner: PackageSources::from_single_source(package_id, path_str),
        })
    }

    fn from_inner(inner: PackageSources) -> Self {
        Self { inner }
    }

    /// All unique source paths.
    pub fn paths(&self) -> impl Iterator<Item = &Path> {
        self.inner.source_names().map(Path::new)
    }

    /// Source paths for package
    pub fn package_paths(&self, id: super::PackageId) -> Option<impl Iterator<Item = &Path>> {
        self.inner
            .package_source_names(id)
            .map(|iter| iter.map(Path::new))
    }
}

enum ParsedFile {
    #[cfg(feature = "decoding")]
    Package(super::PackageId),
    Unresolved(UnresolvedPackageGroup),
}

impl Resolve {
    /// Parse WIT packages from the input `path`.
    ///
    /// The input `path` can be one of:
    ///
    /// * A directory containing a WIT package with an optional `deps` directory
    ///   for local dependencies. In this case `deps` is parsed first and then
    ///   the parent `path` is parsed and returned.
    /// * A single standalone WIT file.
    /// * A wasm-encoded WIT package as a single file in either the text or
    ///   binary format.
    ///
    /// More information can also be found at [`Resolve::push_dir`] and
    /// [`Resolve::push_file`].
    pub fn push_path(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Result<(super::PackageId, PackageSourceMap)> {
        self._push_path(path.as_ref())
    }

    fn _push_path(&mut self, path: &Path) -> Result<(super::PackageId, PackageSourceMap)> {
        if path.is_dir() {
            self.push_dir(path).with_context(|| {
                format!(
                    "failed to resolve directory while parsing WIT for path [{}]",
                    path.display()
                )
            })
        } else {
            let id = self.push_file(path)?;
            Ok((id, PackageSourceMap::from_single_source(id, path)?))
        }
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
    ///   name resolution.
    /// * `$path/deps/my-package.{wasm,wat}` - a wasm-encoded WIT package either
    ///   in the text for binary format.
    ///
    /// In all cases entries in the `deps` folder are added to `self` first
    /// before adding files found in `path` itself. All WIT packages found are
    /// candidates for name-based resolution that other packages may use.
    ///
    /// This function returns a tuple of two values. The first value is a
    /// [`PackageId`], which represents the main WIT package found within
    /// `path`. This argument is useful for passing to [`Resolve::select_world`]
    /// for choosing something to bindgen with.
    ///
    /// The second value returned is a [`PackageSourceMap`], which contains all the sources
    /// that were parsed during resolving. This can be useful for:
    /// * build systems that want to rebuild bindings whenever one of the files changed
    /// * or other tools, which want to identify the sources for the resolved packages
    pub fn push_dir(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Result<(super::PackageId, PackageSourceMap)> {
        self._push_dir(path.as_ref())
    }

    fn _push_dir(&mut self, path: &Path) -> Result<(super::PackageId, PackageSourceMap)> {
        let top_pkg = UnresolvedPackageGroup::parse_dir(path)
            .with_context(|| format!("failed to parse package: {}", path.display()))?;
        let deps = path.join("deps");
        let deps = self
            .parse_deps_dir(&deps)
            .with_context(|| format!("failed to parse dependency directory: {}", deps.display()))?;

        let (pkg_id, inner) = self.sort_unresolved_packages(top_pkg, deps)?;
        Ok((pkg_id, PackageSourceMap::from_inner(inner)))
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
    pub fn push_file(&mut self, path: impl AsRef<Path>) -> Result<super::PackageId> {
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
            use crate::decoding::{DecodedWasm, decode};

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
        let text = match core::str::from_utf8(&contents) {
            Ok(s) => s,
            Err(_) => bail!("input file is not valid utf-8 [{}]", path.display()),
        };
        let pkgs = UnresolvedPackageGroup::parse(path, text)?;
        Ok(ParsedFile::Unresolved(pkgs))
    }

    /// Convenience method for combining [`UnresolvedPackageGroup::parse`] and
    /// [`Resolve::push_group`].
    ///
    /// The `path` provided is used for error messages but otherwise is not
    /// read. This method does not touch the filesystem. The `contents` provided
    /// are the contents of a WIT package.
    pub fn push_str(&mut self, path: impl AsRef<Path>, contents: &str) -> Result<super::PackageId> {
        let path = path
            .as_ref()
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("path is not valid utf-8: {:?}", path.as_ref()))?;
        self.push_source(path, contents)
    }
}
