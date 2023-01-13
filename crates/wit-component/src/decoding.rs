use anyhow::{anyhow, bail, Context, Result};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use url::Url;
use wasmparser::{
    types, ComponentExport, ComponentExternalKind, ComponentImport, ComponentTypeRef, Parser,
    Payload, PrimitiveValType, ValidPayload, Validator, WasmFeatures,
};
use wit_parser::*;

/// Represents information about a decoded WebAssembly component.
struct ComponentInfo<'a> {
    /// Wasmparser-defined type information learned after a component is fully
    /// validated.
    types: types::Types,
    /// Map of imports and what type they're importing.
    imports: IndexMap<&'a str, ComponentImport<'a>>,
    /// Map of exports and what they're exporting.
    exports: IndexMap<&'a str, ComponentExport<'a>>,
}

impl<'a> ComponentInfo<'a> {
    /// Creates a new component info by parsing the given WebAssembly component bytes.
    fn new(bytes: &'a [u8]) -> Result<Self> {
        let mut validator = Validator::new_with_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });
        let mut exports = IndexMap::new();
        let mut imports = IndexMap::new();
        let mut depth = 1;
        let mut types = None;

        for payload in Parser::new(0).parse_all(bytes) {
            let payload = payload?;
            match validator.payload(&payload)? {
                ValidPayload::Ok => {}
                ValidPayload::Parser(_) => depth += 1,
                ValidPayload::End(t) => {
                    depth -= 1;
                    if depth == 0 {
                        types = Some(t);
                    }
                }
                ValidPayload::Func(..) => {}
            }

            match payload {
                Payload::ComponentImportSection(s) if depth == 1 => {
                    for import in s {
                        let import = import?;
                        let prev = imports.insert(import.name, import);
                        assert!(prev.is_none());
                    }
                }
                Payload::ComponentExportSection(s) if depth == 1 => {
                    for export in s {
                        let export = export?;
                        let prev = exports.insert(export.name, export);
                        assert!(prev.is_none());
                    }
                }
                _ => {}
            }
        }
        Ok(Self {
            types: types.unwrap(),
            imports,
            exports,
        })
    }

    fn is_wit_package(&self) -> bool {
        // wit packages only export component types and must export at least one
        if !self.imports.is_empty() || self.exports.is_empty() {
            return false;
        }

        // all wit package exports must be component types
        self.exports.iter().all(|(_, export)| match export.kind {
            ComponentExternalKind::Type => match self.types.type_at(export.index, false) {
                Some(types::Type::Component(_)) => true,
                _ => false,
            },
            _ => false,
        })
    }

    fn decode_wit_package(&self, name: &str) -> Result<(Resolve, PackageId)> {
        assert!(self.is_wit_package());
        let mut resolve = Resolve::default();
        let package = resolve.packages.alloc(Package {
            name: name.to_string(),
            documents: Default::default(),
            url: None,
        });
        let mut decoder = WitPackageDecoder {
            resolve,
            package,
            info: self,
            url_to_package: HashMap::default(),
            type_map: HashMap::new(),
            type_src_map: HashMap::new(),
            url_to_interface: HashMap::new(),
        };

        for (doc, export) in self.exports.iter() {
            let ty = match self.types.type_at(export.index, false) {
                Some(types::Type::Component(ty)) => ty,
                _ => unreachable!(),
            };
            decoder
                .decode_document(doc, ty)
                .with_context(|| format!("failed to decode document `{doc}`"))?;
        }
        Ok((decoder.resolve, package))
    }

    fn decode_component(&self, name: &str) -> Result<(Resolve, WorldId)> {
        assert!(!self.is_wit_package());
        let mut resolve = Resolve::default();
        let package = resolve.packages.alloc(Package {
            name: name.to_string(),
            documents: Default::default(),
            url: None,
        });
        let doc = resolve.documents.alloc(Document {
            name: "root".to_string(),
            interfaces: Default::default(),
            worlds: Default::default(),
            default_interface: None,
            default_world: None,
            package: Some(package),
        });
        let world = resolve.worlds.alloc(World {
            name: name.to_string(),
            docs: Default::default(),
            imports: Default::default(),
            exports: Default::default(),
            document: doc,
        });
        resolve.documents[doc]
            .worlds
            .insert(name.to_string(), world);
        resolve.documents[doc].default_world = Some(world);
        let mut decoder = WitPackageDecoder {
            resolve,
            package,
            info: self,
            url_to_package: HashMap::default(),
            type_map: HashMap::new(),
            type_src_map: HashMap::new(),
            url_to_interface: HashMap::new(),
        };

        for (name, import) in self.imports.iter() {
            let item = match import.ty {
                ComponentTypeRef::Instance(i) => {
                    let ty = match self.types.type_at(i, false) {
                        Some(types::Type::ComponentInstance(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let id = decoder
                        .register_interface(doc, Some(name), ty)
                        .with_context(|| format!("failed to decode WIT from import `{name}`"))?;
                    decoder.resolve.documents[doc]
                        .interfaces
                        .insert(name.to_string(), id);
                    WorldItem::Interface(id)
                }
                ComponentTypeRef::Func(i) => {
                    let ty = match self.types.type_at(i, false) {
                        Some(types::Type::ComponentFunc(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let func = decoder.convert_function(name, ty).with_context(|| {
                        format!("failed to decode function from import `{name}`")
                    })?;
                    WorldItem::Function(func)
                }
                _ => bail!("component import `{name}` was neither a function nor instance"),
            };
            decoder.resolve.worlds[world]
                .imports
                .insert(name.to_string(), item);
        }
        for (name, export) in self.exports.iter() {
            let item = match export.kind {
                ComponentExternalKind::Func => {
                    let ty = self.types.component_function_at(export.index).unwrap();
                    let func = decoder.convert_function(name, ty).with_context(|| {
                        format!("failed to decode function from export `{name}`")
                    })?;

                    WorldItem::Function(func)
                }
                ComponentExternalKind::Instance => {
                    let ty = self.types.component_instance_at(export.index).unwrap();
                    let id = decoder
                        .register_interface(doc, Some(name), ty)
                        .with_context(|| format!("failed to decode WIT from export `{name}`"))?;
                    decoder.resolve.documents[doc]
                        .interfaces
                        .insert(name.to_string(), id);
                    WorldItem::Interface(id)
                }
                _ => bail!("component export `{name}` was neither a function nor instance"),
            };
            decoder.resolve.worlds[world]
                .exports
                .insert(name.to_string(), item);
        }
        Ok((decoder.resolve, world))
    }
}

/// Result of the [`decode`] function.
pub enum DecodedWasm {
    /// The input to [`decode`] was a binary-encoded WIT package.
    ///
    /// The full resolve graph is here plus the identifier of the package that
    /// was encoded. Note that other packages may be within the resolve if this
    /// package refers to foreign packages.
    WitPackage(Resolve, PackageId),

    /// The input to [`decode`] was a component and its interface is specified
    /// by the world here.
    Component(Resolve, WorldId),
}

impl DecodedWasm {
    /// Returns the [`Resolve`] for WIT types contained.
    pub fn resolve(&self) -> &Resolve {
        match self {
            DecodedWasm::WitPackage(resolve, _) => resolve,
            DecodedWasm::Component(resolve, _) => resolve,
        }
    }

    /// Returns the main package of what was decoded.
    pub fn package(&self) -> PackageId {
        match self {
            DecodedWasm::WitPackage(_, id) => *id,
            DecodedWasm::Component(resolve, world) => {
                let doc = resolve.worlds[*world].document;
                resolve.documents[doc].package.unwrap()
            }
        }
    }
}

/// Decodes an in-memory WebAssembly binary into a WIT [`Resolve`] and
/// associated metadata.
///
/// The WebAssembly binary provided here can either be a
/// WIT-package-encoded-as-binary or an actual component itself. A [`Resolve`]
/// is always created and the return value indicates which was detected.
pub fn decode(name: &str, bytes: &[u8]) -> Result<DecodedWasm> {
    let info = ComponentInfo::new(bytes)?;

    if info.is_wit_package() {
        let (resolve, pkg) = info.decode_wit_package(name)?;
        Ok(DecodedWasm::WitPackage(resolve, pkg))
    } else {
        let (resolve, world) = info.decode_component(name)?;
        Ok(DecodedWasm::Component(resolve, world))
    }
}

struct WitPackageDecoder<'a> {
    resolve: Resolve,
    info: &'a ComponentInfo<'a>,
    package: PackageId,
    url_to_package: HashMap<Url, PackageId>,
    url_to_interface: HashMap<Url, InterfaceId>,

    /// A map from a type id to what it's been translated to.
    type_map: HashMap<types::TypeId, Type>,

    /// A second map, similar to `type_map`, which is keyed off a pointer hash
    /// instead of `TypeId`.
    ///
    /// The purpose of this is to detect when a type is aliased as there will
    /// be two unique `TypeId` structures pointing at the same `types::Type`
    /// structure, so the second layer of map here ensures that types are
    /// only defined once and the second `TypeId` referring to a type will end
    /// up as an alias and/or import.
    type_src_map: HashMap<PtrHash<'a, types::ComponentDefinedType>, Type>,
}

impl WitPackageDecoder<'_> {
    fn decode_document(&mut self, name: &str, ty: &types::ComponentType) -> Result<()> {
        // Process all imports for this document first, where imports are either
        // importing interfaces from previously defined documents or from remote
        // packages. Note that the URL must be specified here for these
        // reconstruction purposes.
        for (name, (url, ty)) in ty.imports.iter() {
            let url = match url {
                Some(url) => url,
                None => bail!("no url specified for import `{name}`"),
            };
            let ty = match ty {
                types::ComponentEntityType::Instance(idx) => {
                    match self.info.types.type_from_id(*idx) {
                        Some(types::Type::ComponentInstance(ty)) => ty,
                        _ => unreachable!(),
                    }
                }
                _ => bail!("import `{name}` is not an instance"),
            };
            self.register_import(url, ty)
                .with_context(|| format!("failed to process import `{name}`"))?;
        }

        let doc = self.resolve.documents.alloc(Document {
            name: name.to_string(),
            interfaces: IndexMap::new(),
            worlds: IndexMap::new(),
            default_interface: None,
            default_world: None,
            package: Some(self.package),
        });
        let prev = self.resolve.packages[self.package]
            .documents
            .insert(name.to_string(), doc);
        assert!(prev.is_none());

        for (name, (url, ty)) in ty.exports.iter() {
            match ty {
                types::ComponentEntityType::Instance(idx) => {
                    let ty = match self.info.types.type_from_id(*idx) {
                        Some(types::Type::ComponentInstance(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let id = self
                        .register_interface(doc, Some(name), ty)
                        .with_context(|| format!("failed to process export `{name}`"))?;
                    let prev = self.resolve.documents[doc]
                        .interfaces
                        .insert(name.to_string(), id);
                    assert!(prev.is_none());
                    if let Some(url) = url {
                        let prev = self.url_to_interface.insert(url.clone(), id);
                        assert!(prev.is_none());
                    }
                }
                types::ComponentEntityType::Component(idx) => {
                    let ty = match self.info.types.type_from_id(*idx) {
                        Some(types::Type::Component(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let id = self
                        .register_world(doc, name, ty)
                        .with_context(|| format!("failed to process export `{name}`"))?;
                    let prev = self.resolve.documents[doc]
                        .worlds
                        .insert(name.to_string(), id);
                    assert!(prev.is_none());
                }
                _ => bail!("component export `{name}` is not an instance or component"),
            }
        }
        Ok(())
    }

    fn register_import(
        &mut self,
        url: &Url,
        ty: &types::ComponentInstanceType,
    ) -> Result<InterfaceId> {
        let interface = self.extract_url_interface(url)?;

        for (name, export_url, ty) in ty.exports(self.info.types.as_ref()) {
            if export_url.is_some() {
                bail!("instance type export `{name}` should not have a url")
            }

            match ty {
                types::ComponentEntityType::Type(ty) => {
                    let def = match self.info.types.type_from_id(ty) {
                        Some(types::Type::Defined(ty)) => ty,
                        _ => unreachable!(),
                    };

                    let id = match self.resolve.interfaces[interface].types.get(name.as_str()) {
                        // If this name is already defined as a type in the
                        // specified interface then that's ok. For package-local
                        // interfaces that's expected since the interface was
                        // fully defined. For remote interfaces it means we're
                        // using something that was already used elsewhere. In
                        // both cases continue along.
                        //
                        // TODO: ideally this would verify that `def` matches
                        // the structure of `id`.
                        Some(id) => *id,

                        // If the name is not defined, however, then there's two
                        // possibilities:
                        //
                        // * For package-local interfaces this is an error
                        //   because the package-local interface defined
                        //   everything already and this is referencing
                        //   something that isn't defined.
                        //
                        // * For remote interfaces they're never fully declared
                        //   so it's lazily filled in here. This means that the
                        //   view of remote interfaces ends up being the minimal
                        //   slice needed for this resolve, which is what's
                        //   intended.
                        None => {
                            if url.scheme() == "pkg" {
                                bail!("instance type export `{name}` not defined in interface");
                            }
                            let kind = self.convert_defined(def)?;
                            let id = self.resolve.types.alloc(TypeDef {
                                name: Some(name.to_string()),
                                kind,
                                docs: Default::default(),
                                owner: TypeOwner::Interface(interface),
                            });
                            let prev = self.resolve.interfaces[interface]
                                .types
                                .insert(name.to_string(), id);
                            assert!(prev.is_none());
                            id
                        }
                    };

                    // Register the `types::TypeId` with our resolve `TypeId`
                    // for ensuring type information remains correct throughout
                    // decoding.
                    let prev = self.type_map.insert(ty, Type::Id(id));
                    assert!(prev.is_none());
                    self.type_src_map
                        .entry(PtrHash(def))
                        .or_insert(Type::Id(id));
                }

                // This has similar logic to types above where we lazily fill in
                // functions for remote dependencies and otherwise assert
                // they're already defined for local dependencies.
                types::ComponentEntityType::Func(ty) => {
                    let def = match self.info.types.type_from_id(ty) {
                        Some(types::Type::ComponentFunc(ty)) => ty,
                        _ => unreachable!(),
                    };
                    if self.resolve.interfaces[interface]
                        .functions
                        .contains_key(name.as_str())
                    {
                        // TODO: should ideally verify that function signatures
                        // match.
                        continue;
                    }
                    if url.scheme() == "pkg" {
                        bail!("instance function export `{name}` not defined in interface");
                    }
                    let func = self.convert_function(name, def)?;
                    let prev = self.resolve.interfaces[interface]
                        .functions
                        .insert(name.to_string(), func);
                    assert!(prev.is_none());
                }

                _ => bail!("instance type export `{name}` is not a type"),
            }
        }

        Ok(interface)
    }

    fn extract_url_interface(&mut self, url: &Url) -> Result<InterfaceId> {
        Ok(if url.scheme() == "pkg" {
            self.url_to_interface
                .get(url)
                .copied()
                .ok_or_else(|| anyhow!("no previously defined interface with url: {url}"))?
        } else {
            self.extract_dep_interface(url)
                .with_context(|| format!("failed to parse url: {url}"))?
        })
    }

    /// TODO: Ideally this function should not need to exist.
    ///
    /// This function parses the `url` provided and requires it to have a
    /// particular structure. That's not really great, however, since otherwise
    /// there's no need to impose structure on the url field of imports/exports.
    ///
    /// Note that this is only used for foreign dependencies of which the binary
    /// encoding does not currently reflect the package/document/interface
    /// organization. Instead foreign dependencies simply have their interfaces
    /// imported, and from this interface import we need to somehow translate
    /// back into a package/document structure as well.
    ///
    /// Resolving this may require changing the binary format for components, or
    /// otherwise encoding more pieces into the binary encoding of a WIT
    /// document. In any case this is "good enough" for now hopefully.
    fn extract_dep_interface(&mut self, url: &Url) -> Result<InterfaceId> {
        // Extract the interface and the document from the url
        let mut segments = url.path_segments().ok_or_else(|| anyhow!("invalid url"))?;
        let interface = segments.next_back().ok_or_else(|| anyhow!("invalid url"))?;
        let document = segments.next_back().ok_or_else(|| anyhow!("invalid url"))?;
        let package_name = segments.next_back().ok_or_else(|| anyhow!("invalid url"))?;

        // Then drop the two path segments from the url as a key to lookup the
        // dependency package by url.
        let mut url = url.clone();
        url.path_segments_mut().unwrap().pop().pop();

        // Lazily create a `Package` as necessary, along with the document and
        // interface.
        let package = *self.url_to_package.entry(url.clone()).or_insert_with(|| {
            self.resolve.packages.alloc(Package {
                name: package_name.to_string(),
                documents: Default::default(),
                url: Some(url.to_string()),
            })
        });
        let doc = *self.resolve.packages[package]
            .documents
            .entry(document.to_string())
            .or_insert_with(|| {
                self.resolve.documents.alloc(Document {
                    name: document.to_string(),
                    interfaces: IndexMap::new(),
                    worlds: IndexMap::new(),
                    default_interface: None,
                    default_world: None,
                    package: Some(package),
                })
            });
        let interface = *self.resolve.documents[doc]
            .interfaces
            .entry(interface.to_string())
            .or_insert_with(|| {
                self.resolve.interfaces.alloc(Interface {
                    name: Some(interface.to_string()),
                    docs: Default::default(),
                    types: IndexMap::default(),
                    functions: IndexMap::new(),
                    document: doc,
                })
            });
        Ok(interface)
    }

    fn register_interface(
        &mut self,
        doc: DocumentId,
        name: Option<&str>,
        ty: &types::ComponentInstanceType,
    ) -> Result<InterfaceId> {
        let mut interface = Interface {
            name: name.map(|n| n.to_string()),
            docs: Default::default(),
            types: IndexMap::default(),
            functions: IndexMap::new(),
            document: doc,
        };

        for (name, export_url, ty) in ty.exports(self.info.types.as_ref()) {
            if export_url.is_some() {
                bail!("instance type export `{name}` should not have a url")
            }

            match ty {
                types::ComponentEntityType::Type(id) => {
                    let ty = match self.info.types.type_from_id(id) {
                        Some(types::Type::Defined(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let key = PtrHash(ty);
                    let (kind, insert_src) = match self.type_src_map.get(&key) {
                        // If this `TypeId` points to a type which has
                        // previously been defined then a second `TypeId`
                        // pointing at it is indicative of an alias. Inject the
                        // alias here.
                        Some(prev) => (TypeDefKind::Type(*prev), false),

                        // ... or this `TypeId`'s source definition has never
                        // been seen before, so declare the full type.
                        None => {
                            let ty = self
                                .convert_defined(ty)
                                .with_context(|| format!("failed to decode type `{name}`"))?;
                            (ty, true)
                        }
                    };
                    let ty = self.resolve.types.alloc(TypeDef {
                        docs: Default::default(),
                        kind,
                        name: Some(name.to_string()),
                        owner: TypeOwner::Interface(self.resolve.interfaces.next_id()),
                    });

                    if insert_src {
                        self.type_src_map.insert(key, Type::Id(ty));
                    }
                    let prev = self.type_map.insert(id, Type::Id(ty));
                    assert!(prev.is_none());
                    let prev = interface.types.insert(name.to_string(), ty);
                    assert!(prev.is_none());
                }

                types::ComponentEntityType::Func(ty) => {
                    let ty = match self.info.types.type_from_id(ty) {
                        Some(types::Type::ComponentFunc(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let func = self.convert_function(&name, ty)?;
                    let prev = interface.functions.insert(name.to_string(), func);
                    assert!(prev.is_none());
                }
                _ => bail!("instance type export `{name}` is not a type or function"),
            };
        }
        Ok(self.resolve.interfaces.alloc(interface))
    }

    fn register_world(
        &mut self,
        document: DocumentId,
        name: &str,
        ty: &types::ComponentType,
    ) -> Result<WorldId> {
        let mut world = World {
            name: name.to_string(),
            docs: Default::default(),
            imports: Default::default(),
            exports: Default::default(),
            document,
        };

        // Imports in this component type represent all of the imported items
        // into the world itself, so all imports get registered.
        for (name, (url, ty)) in ty.imports.iter() {
            let item = match ty {
                types::ComponentEntityType::Instance(idx) => {
                    let ty = match self.info.types.type_from_id(*idx) {
                        Some(types::Type::ComponentInstance(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let id = match url {
                        // If a URL is specified then the import is either to a
                        // package-local or foreign interface, and both
                        // situations are handled in `register_import`.
                        Some(url) => self.register_import(url, ty)?,

                        // Without a URL this indicates an inline interface that
                        // wasn't declared explicitly elsewhere with a name, and
                        // `register_interface` will create a new `Interface`
                        // with no name.
                        None => self.register_interface(document, None, ty)?,
                    };
                    WorldItem::Interface(id)
                }

                types::ComponentEntityType::Func(idx) => {
                    let ty = match self.info.types.type_from_id(*idx) {
                        Some(types::Type::ComponentFunc(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let func = self.convert_function(name, ty)?;
                    WorldItem::Function(func)
                }

                _ => bail!("component import `{name}` is not an instance or function"),
            };
            world.imports.insert(name.to_string(), item);
        }

        for (name, (url, ty)) in ty.exports.iter() {
            let item = match ty {
                types::ComponentEntityType::Instance(idx) => {
                    let ty = match self.info.types.type_from_id(*idx) {
                        Some(types::Type::ComponentInstance(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let id = match url {
                        Some(url) => self.extract_url_interface(url)?,
                        None => self.register_interface(document, None, ty)?,
                    };
                    WorldItem::Interface(id)
                }

                types::ComponentEntityType::Func(idx) => {
                    let ty = match self.info.types.type_from_id(*idx) {
                        Some(types::Type::ComponentFunc(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let func = self.convert_function(name, ty)?;
                    WorldItem::Function(func)
                }

                _ => bail!("component export `{name}` is not an instance or function"),
            };
            world.exports.insert(name.to_string(), item);
        }
        Ok(self.resolve.worlds.alloc(world))
    }

    fn convert_function(&mut self, name: &str, ty: &types::ComponentFuncType) -> Result<Function> {
        let params = ty
            .params
            .iter()
            .map(|(name, ty)| Ok((name.to_string(), self.convert_valtype(ty)?)))
            .collect::<Result<Vec<_>>>()?;
        let results = if ty.results.len() == 1 && ty.results[0].0.is_none() {
            Results::Anon(self.convert_valtype(&ty.results[0].1)?)
        } else {
            Results::Named(
                ty.results
                    .iter()
                    .map(|(name, ty)| {
                        Ok((
                            name.as_ref().unwrap().to_string(),
                            self.convert_valtype(ty)?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
            )
        };
        Ok(Function {
            docs: Default::default(),
            kind: FunctionKind::Freestanding,
            name: name.to_string(),
            params,
            results,
        })
    }

    fn convert_valtype(&mut self, ty: &types::ComponentValType) -> Result<Type> {
        let id = match ty {
            types::ComponentValType::Primitive(ty) => return Ok(self.convert_primitive(*ty)),
            types::ComponentValType::Type(id) => *id,
        };

        // Don't create duplicate types for anything previously created.
        if let Some(ret) = self.type_map.get(&id) {
            return Ok(*ret);
        }

        // Otherwise create a new `TypeDef` without a name since this is an
        // anonymous valtype. Note that this is invalid for some types so return
        // errors on those types, but eventually the `bail!` here  is
        // more-or-less unreachable due to expected validation to be added to
        // the component model binary format itself.
        let ty = match self.info.types.type_from_id(id) {
            Some(types::Type::Defined(ty)) => ty,
            _ => unreachable!(),
        };
        let kind = self.convert_defined(ty)?;
        match &kind {
            TypeDefKind::Type(_)
            | TypeDefKind::List(_)
            | TypeDefKind::Tuple(_)
            | TypeDefKind::Option(_)
            | TypeDefKind::Result(_) => {}

            TypeDefKind::Record(_)
            | TypeDefKind::Enum(_)
            | TypeDefKind::Variant(_)
            | TypeDefKind::Union(_)
            | TypeDefKind::Flags(_)
            | TypeDefKind::Future(_)
            | TypeDefKind::Stream(_) => {
                bail!("unexpected unnamed type");
            }
            TypeDefKind::Unknown => unreachable!(),
        }
        let ty = self.resolve.types.alloc(TypeDef {
            name: None,
            docs: Default::default(),
            owner: TypeOwner::None,
            kind,
        });
        let prev = self.type_map.insert(id, Type::Id(ty));
        assert!(prev.is_none());
        Ok(Type::Id(ty))
    }

    /// Converts a wasmparser `ComponentDefinedType`, the definition of a type
    /// in the component model, to a WIT `TypeDefKind` to get inserted into the
    /// types arena by the caller.
    fn convert_defined(&mut self, ty: &types::ComponentDefinedType) -> Result<TypeDefKind> {
        match ty {
            types::ComponentDefinedType::Primitive(t) => {
                Ok(TypeDefKind::Type(self.convert_primitive(*t)))
            }

            types::ComponentDefinedType::List(t) => {
                let t = self.convert_valtype(t)?;
                Ok(TypeDefKind::List(t))
            }

            types::ComponentDefinedType::Tuple(t) => {
                let types = t
                    .types
                    .iter()
                    .map(|t| self.convert_valtype(t))
                    .collect::<Result<_>>()?;
                Ok(TypeDefKind::Tuple(Tuple { types }))
            }

            types::ComponentDefinedType::Option(t) => {
                let t = self.convert_valtype(t)?;
                Ok(TypeDefKind::Option(t))
            }

            types::ComponentDefinedType::Result { ok, err } => {
                let ok = match ok {
                    Some(t) => Some(self.convert_valtype(t)?),
                    None => None,
                };
                let err = match err {
                    Some(t) => Some(self.convert_valtype(t)?),
                    None => None,
                };
                Ok(TypeDefKind::Result(Result_ { ok, err }))
            }

            types::ComponentDefinedType::Record(r) => {
                let fields = r
                    .fields
                    .iter()
                    .map(|(name, ty)| {
                        Ok(Field {
                            name: name.to_string(),
                            ty: self.convert_valtype(ty)?,
                            docs: Default::default(),
                        })
                    })
                    .collect::<Result<_>>()?;
                Ok(TypeDefKind::Record(Record { fields }))
            }

            types::ComponentDefinedType::Variant(v) => {
                let cases = v
                    .cases
                    .iter()
                    .map(|(name, case)| {
                        if case.refines.is_some() {
                            bail!("unimplemented support for `refines`");
                        }
                        Ok(Case {
                            name: name.to_string(),
                            ty: match &case.ty {
                                Some(ty) => Some(self.convert_valtype(ty)?),
                                None => None,
                            },
                            docs: Default::default(),
                        })
                    })
                    .collect::<Result<_>>()?;
                Ok(TypeDefKind::Variant(Variant { cases }))
            }

            types::ComponentDefinedType::Flags(f) => {
                let flags = f
                    .iter()
                    .map(|name| Flag {
                        name: name.to_string(),
                        docs: Default::default(),
                    })
                    .collect();
                Ok(TypeDefKind::Flags(Flags { flags }))
            }

            types::ComponentDefinedType::Union(u) => {
                let cases = u
                    .types
                    .iter()
                    .map(|ty| {
                        Ok(UnionCase {
                            ty: self.convert_valtype(ty)?,
                            docs: Default::default(),
                        })
                    })
                    .collect::<Result<_>>()?;
                Ok(TypeDefKind::Union(Union { cases }))
            }

            types::ComponentDefinedType::Enum(e) => {
                let cases = e
                    .iter()
                    .cloned()
                    .map(|name| EnumCase {
                        name: name.into(),
                        docs: Default::default(),
                    })
                    .collect();
                Ok(TypeDefKind::Enum(Enum { cases }))
            }
        }
    }

    fn convert_primitive(&self, ty: PrimitiveValType) -> Type {
        match ty {
            PrimitiveValType::U8 => Type::U8,
            PrimitiveValType::S8 => Type::S8,
            PrimitiveValType::U16 => Type::U16,
            PrimitiveValType::S16 => Type::S16,
            PrimitiveValType::U32 => Type::U32,
            PrimitiveValType::S32 => Type::S32,
            PrimitiveValType::U64 => Type::U64,
            PrimitiveValType::S64 => Type::S64,
            PrimitiveValType::Bool => Type::Bool,
            PrimitiveValType::Char => Type::Char,
            PrimitiveValType::String => Type::String,
            PrimitiveValType::Float32 => Type::Float32,
            PrimitiveValType::Float64 => Type::Float64,
        }
    }
}

struct PtrHash<'a, T>(&'a T);

impl<T> PartialEq for PtrHash<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T> Eq for PtrHash<'_, T> {}

impl<T> Hash for PtrHash<'_, T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        std::ptr::hash(self.0, hasher)
    }
}
