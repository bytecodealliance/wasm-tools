use anyhow::{anyhow, bail, Context, Result};
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;
use std::mem;
use url::Url;
use wasmparser::{
    types, ComponentExport, ComponentExternalKind, ComponentImport, Parser, Payload,
    PrimitiveValType, ValidPayload, Validator, WasmFeatures,
};
use wit_parser::*;

/// Represents information about a decoded WebAssembly component.
struct ComponentInfo<'a> {
    /// Wasmparser-defined type information learned after a component is fully
    /// validated.
    types: types::Types,
    /// List of all imports and exports from this component.
    externs: Vec<(&'a str, Extern<'a>)>,
}

enum Extern<'a> {
    Import(ComponentImport<'a>),
    Export(ComponentExport<'a>),
}

impl<'a> ComponentInfo<'a> {
    /// Creates a new component info by parsing the given WebAssembly component bytes.
    fn new(bytes: &'a [u8]) -> Result<Self> {
        let mut validator = Validator::new_with_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });
        let mut externs = Vec::new();
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
                        externs.push((import.name, Extern::Import(import)));
                    }
                }
                Payload::ComponentExportSection(s) if depth == 1 => {
                    for export in s {
                        let export = export?;
                        externs.push((export.name, Extern::Export(export)));
                    }
                }
                _ => {}
            }
        }
        Ok(Self {
            types: types.unwrap(),
            externs,
        })
    }

    fn is_wit_package(&self) -> bool {
        // all wit package exports must be component types, and there must be at
        // least one
        !self.externs.is_empty()
            && self.externs.iter().all(|(_, item)| {
                let export = match item {
                    Extern::Export(e) => e,
                    _ => return false,
                };
                match export.kind {
                    ComponentExternalKind::Type => match self.types.type_at(export.index, false) {
                        Some(types::Type::Component(_)) => true,
                        _ => false,
                    },
                    _ => false,
                }
            })
    }

    fn decode_wit_package(&self, name: &str) -> Result<(Resolve, PackageId)> {
        assert!(self.is_wit_package());
        let resolve = Resolve::default();
        let mut decoder = WitPackageDecoder {
            resolve,
            info: self,
            url_to_package: IndexMap::default(),
            type_map: HashMap::new(),
            url_to_interface: HashMap::new(),
        };

        let mut docs = Vec::new();
        for (doc, item) in self.externs.iter() {
            let export = match item {
                Extern::Export(e) => e,
                _ => unreachable!(),
            };
            let ty = match self.types.type_at(export.index, false) {
                Some(types::Type::Component(ty)) => ty,
                _ => unreachable!(),
            };
            let id = decoder
                .decode_document(doc, ty)
                .with_context(|| format!("failed to decode document `{doc}`"))?;
            docs.push((doc, id));
        }

        let (resolve, package) = decoder.finish(Package {
            name: name.to_string(),
            documents: docs
                .iter()
                .map(|(name, d)| (name.to_string(), *d))
                .collect(),
            url: None,
        });

        Ok((resolve, package))
    }

    fn decode_component(&self, name: &str) -> Result<(Resolve, WorldId)> {
        assert!(!self.is_wit_package());
        let mut resolve = Resolve::default();
        let doc = resolve.documents.alloc(Document {
            name: "root".to_string(),
            interfaces: Default::default(),
            worlds: Default::default(),
            default_interface: None,
            default_world: None,
            package: None,
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
            info: self,
            url_to_package: IndexMap::default(),
            type_map: HashMap::new(),
            url_to_interface: HashMap::new(),
        };

        for (name, item) in self.externs.iter() {
            match item {
                Extern::Import(import) => {
                    let ty = self
                        .types
                        .component_entity_type_from_import(import)
                        .unwrap();
                    let item = match ty {
                        types::ComponentEntityType::Instance(i) => {
                            let ty = match self.types.type_from_id(i) {
                                Some(types::Type::ComponentInstance(ty)) => ty,
                                _ => unreachable!(),
                            };
                            let id = decoder
                                .register_interface(doc, Some(name), ty)
                                .with_context(|| {
                                    format!("failed to decode WIT from import `{name}`")
                                })?;
                            decoder.resolve.documents[doc]
                                .interfaces
                                .insert(name.to_string(), id);
                            WorldItem::Interface(id)
                        }
                        types::ComponentEntityType::Func(i) => {
                            let ty = match self.types.type_from_id(i) {
                                Some(types::Type::ComponentFunc(ty)) => ty,
                                _ => unreachable!(),
                            };
                            let func = decoder.convert_function(name, ty).with_context(|| {
                                format!("failed to decode function from import `{name}`")
                            })?;
                            WorldItem::Function(func)
                        }
                        types::ComponentEntityType::Type {
                            referenced,
                            created,
                        } => {
                            let id = decoder
                                .register_type_export(
                                    name,
                                    TypeOwner::World(world),
                                    referenced,
                                    created,
                                )
                                .with_context(|| {
                                    format!("failed to decode type from export `{name}`")
                                })?;
                            WorldItem::Type(id)
                        }
                        _ => {
                            bail!("component import `{name}` was not a function, instance, or type")
                        }
                    };
                    decoder.resolve.worlds[world]
                        .imports
                        .insert(name.to_string(), item);
                }

                Extern::Export(export) => {
                    let ty = self
                        .types
                        .component_entity_type_from_export(export)
                        .unwrap();
                    let item = match ty {
                        types::ComponentEntityType::Func(i) => {
                            let ty = match self.types.type_from_id(i) {
                                Some(types::Type::ComponentFunc(ty)) => ty,
                                _ => unreachable!(),
                            };
                            let func = decoder.convert_function(name, ty).with_context(|| {
                                format!("failed to decode function from export `{name}`")
                            })?;

                            WorldItem::Function(func)
                        }
                        types::ComponentEntityType::Instance(i) => {
                            let ty = match self.types.type_from_id(i) {
                                Some(types::Type::ComponentInstance(ty)) => ty,
                                _ => unreachable!(),
                            };
                            let id = decoder
                                .register_interface(doc, Some(name), ty)
                                .with_context(|| {
                                    format!("failed to decode WIT from export `{name}`")
                                })?;
                            decoder.resolve.documents[doc]
                                .interfaces
                                .insert(name.to_string(), id);
                            WorldItem::Interface(id)
                        }
                        _ => {
                            bail!("component export `{name}` was not a function or instance")
                        }
                    };
                    decoder.resolve.worlds[world]
                        .exports
                        .insert(name.to_string(), item);
                }
            }
        }

        let (resolve, _) = decoder.finish(Package {
            name: name.to_string(),
            documents: [("root".to_string(), doc)].into_iter().collect(),
            url: None,
        });
        Ok((resolve, world))
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
    url_to_package: IndexMap<Url, Package>,
    url_to_interface: HashMap<Url, InterfaceId>,

    /// A map from a type id to what it's been translated to.
    type_map: HashMap<types::TypeId, TypeId>,
}

impl WitPackageDecoder<'_> {
    fn decode_document(&mut self, name: &str, ty: &types::ComponentType) -> Result<DocumentId> {
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
            package: None,
        });

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
        Ok(doc)
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
                types::ComponentEntityType::Type {
                    referenced,
                    created,
                } => {
                    let def = match self.info.types.type_from_id(referenced) {
                        Some(types::Type::Defined(ty)) => ty,
                        _ => unreachable!(),
                    };

                    match self.resolve.interfaces[interface]
                        .types
                        .get(name.as_str())
                        .copied()
                    {
                        // If this name is already defined as a type in the
                        // specified interface then that's ok. For package-local
                        // interfaces that's expected since the interface was
                        // fully defined. For remote interfaces it means we're
                        // using something that was already used elsewhere. In
                        // both cases continue along.
                        //
                        // Notably for the remotely defined case this will also
                        // walk over the structure of the type and register
                        // internal wasmparser ids with wit-parser ids. This is
                        // necessary to ensure that anonymous types like
                        // `list<u8>` defined in original definitions are
                        // unified with anonymous types when duplicated inside
                        // of worlds. Overall this prevents, for example, extra
                        // `list<u8>` types from popping up when decoding. This
                        // is not strictly necessary but assists with
                        // roundtripping assertions during fuzzing.
                        Some(id) => {
                            self.register_defined(id, def)?;
                            let prev = self.type_map.insert(created, id);
                            assert!(prev.is_none());
                        }

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
                            let id = self.register_type_export(
                                name,
                                TypeOwner::Interface(interface),
                                referenced,
                                created,
                            )?;
                            let prev = self.resolve.interfaces[interface]
                                .types
                                .insert(name.to_string(), id);
                            assert!(prev.is_none());
                        }
                    }
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

    fn find_alias(&self, id: types::TypeId) -> Option<TypeId> {
        // Consult `type_map` for `referenced` or anything in its
        // chain of aliases to determine what it maps to. This may
        // bottom out in `None` in the case that this type is
        // just now being defined, but this should otherwise follow
        // chains of aliases to determine what exactly this was a
        // `use` of if it exists.
        let mut prev = None;
        let mut cur = id;
        while prev.is_none() {
            prev = self.type_map.get(&cur).copied();
            cur = match self.info.types.peel_alias(cur) {
                Some(next) => next,
                None => break,
            };
        }
        prev
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
        let package = self
            .url_to_package
            .entry(url.clone())
            .or_insert_with(|| Package {
                name: package_name.to_string(),
                documents: Default::default(),
                url: Some(url.to_string()),
            });
        let doc = *package
            .documents
            .entry(document.to_string())
            .or_insert_with(|| {
                self.resolve.documents.alloc(Document {
                    name: document.to_string(),
                    interfaces: IndexMap::new(),
                    worlds: IndexMap::new(),
                    default_interface: None,
                    default_world: None,
                    package: None,
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
                types::ComponentEntityType::Type {
                    referenced,
                    created,
                } => {
                    let ty = self.register_type_export(
                        name,
                        TypeOwner::Interface(self.resolve.interfaces.next_id()),
                        referenced,
                        created,
                    )?;
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

    fn register_type_export(
        &mut self,
        name: &str,
        owner: TypeOwner,
        referenced: types::TypeId,
        created: types::TypeId,
    ) -> Result<TypeId> {
        let ty = match self.info.types.type_from_id(referenced) {
            Some(types::Type::Defined(ty)) => ty,
            _ => unreachable!(),
        };
        let kind = match self.find_alias(referenced) {
            // If this `TypeId` points to a type which has
            // previously been defined, meaning we're aliasing a
            // prior definition.
            Some(prev) => TypeDefKind::Type(Type::Id(prev)),

            // ... or this `TypeId`'s source definition has never
            // been seen before, so declare the full type.
            None => self.convert_defined(ty)?,
        };
        let ty = self.resolve.types.alloc(TypeDef {
            name: Some(name.to_string()),
            kind,
            docs: Default::default(),
            owner,
        });

        let prev = self.type_map.insert(created, ty);
        assert!(prev.is_none());
        Ok(ty)
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
                types::ComponentEntityType::Type {
                    created,
                    referenced,
                } => {
                    let ty = self.register_type_export(
                        name,
                        TypeOwner::World(self.resolve.worlds.next_id()),
                        *referenced,
                        *created,
                    )?;
                    WorldItem::Type(ty)
                }
                types::ComponentEntityType::Func(idx) => {
                    let ty = match self.info.types.type_from_id(*idx) {
                        Some(types::Type::ComponentFunc(ty)) => ty,
                        _ => unreachable!(),
                    };
                    let func = self.convert_function(name, ty)?;
                    WorldItem::Function(func)
                }
                _ => bail!("component import `{name}` is not an instance, func, or type"),
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
                        // Note that despite this being an export this is
                        // calling `register_import`. With a URL this interface
                        // must have been previously defined so this will
                        // trigger the logic of either filling in a remotely
                        // defined interface or connecting items to local
                        // definitions of our own interface.
                        Some(url) => self.register_import(url, ty)?,
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
            return Ok(Type::Id(*ret));
        }

        // Otherwise create a new `TypeDef` without a name since this is an
        // anonymous valtype. Note that this is invalid for some types so return
        // errors on those types, but eventually the `bail!` here  is
        // more-or-less unreachable due to expected validation to be added to
        // the component model binary format itself.
        let def = match self.info.types.type_from_id(id) {
            Some(types::Type::Defined(ty)) => ty,
            _ => unreachable!(),
        };
        let kind = self.convert_defined(def)?;
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
        let prev = self.type_map.insert(id, ty);
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

    fn register_defined(&mut self, id: TypeId, def: &types::ComponentDefinedType) -> Result<()> {
        Registrar {
            types: &self.info.types,
            type_map: &mut self.type_map,
            resolve: &self.resolve,
        }
        .defined(id, def)
    }

    /// Completes the decoding of this resolve by finalizing all packages into
    /// their topological ordering within the returned `Resolve`.
    ///
    /// Takes the root package as an argument to insert.
    fn finish(mut self, package: Package) -> (Resolve, PackageId) {
        // First build a map from all documents to what index their package
        // resides at in the `url_to_package` array.
        let mut doc_to_package_index = HashMap::new();
        for (i, (_url, pkg)) in self.url_to_package.iter().enumerate() {
            for (_, doc) in pkg.documents.iter() {
                let prev = doc_to_package_index.insert(*doc, i);
                assert!(prev.is_none());
            }
        }

        // Using the above map a topological ordering is then calculated by
        // visiting all the transitive dependencies of packages.
        let mut order = IndexSet::new();
        for i in 0..self.url_to_package.len() {
            self.visit_package(&doc_to_package_index, i, &mut order);
        }

        // Using the topological ordering create a temporary map from
        // index-in-`url_to_package` to index-in-`order`
        let mut idx_to_pos = vec![0; self.url_to_package.len()];
        for (pos, idx) in order.iter().enumerate() {
            idx_to_pos[*idx] = pos;
        }
        // .. and then using `idx_to_pos` sort the `url_to_package` array based
        // on the position it's at in the topological ordering
        let mut deps = mem::take(&mut self.url_to_package)
            .into_iter()
            .enumerate()
            .collect::<Vec<_>>();
        deps.sort_by_key(|(idx, _)| idx_to_pos[*idx]);

        // .. and finally insert the packages, in their final topological
        // ordering, into the returned array.
        for (_idx, (_url, pkg)) in deps {
            self.insert_package(pkg);
        }

        let id = self.insert_package(package);
        (self.resolve, id)
    }

    fn insert_package(&mut self, package: Package) -> PackageId {
        let id = self.resolve.packages.alloc(package);
        for (_, doc) in self.resolve.packages[id].documents.iter() {
            self.resolve.documents[*doc].package = Some(id);
        }
        id
    }

    fn visit_package(
        &self,
        doc_to_package_index: &HashMap<DocumentId, usize>,
        idx: usize,
        order: &mut IndexSet<usize>,
    ) {
        if order.contains(&idx) {
            return;
        }

        let (_url, pkg) = self.url_to_package.get_index(idx).unwrap();
        for (_, id) in pkg.documents.iter() {
            let doc = &self.resolve.documents[*id];

            let interfaces = doc.interfaces.values().copied().chain(
                doc.worlds
                    .values()
                    .flat_map(|w| {
                        let world = &self.resolve.worlds[*w];
                        world.imports.values().chain(world.exports.values())
                    })
                    .filter_map(|item| match item {
                        WorldItem::Interface(id) => Some(*id),
                        WorldItem::Function(_) | WorldItem::Type(_) => None,
                    }),
            );
            for iface in interfaces {
                for ty in self.resolve.interfaces[iface].types.values() {
                    let id = match self.resolve.types[*ty].kind {
                        TypeDefKind::Type(Type::Id(id)) => id,
                        _ => continue,
                    };
                    let owner = match self.resolve.types[id].owner {
                        TypeOwner::Interface(i) => i,
                        _ => continue,
                    };
                    let doc = self.resolve.interfaces[owner].document;
                    let owner_idx = doc_to_package_index[&doc];
                    if owner_idx != idx {
                        self.visit_package(doc_to_package_index, owner_idx, order);
                    }
                }
            }
        }

        assert!(order.insert(idx));
    }
}

/// Helper type to register the structure of a wasm-defined type against a
/// wit-defined type.
struct Registrar<'a> {
    types: &'a types::Types,
    type_map: &'a mut HashMap<types::TypeId, TypeId>,
    resolve: &'a Resolve,
}

impl Registrar<'_> {
    /// Verifies that the wasm structure of `def` matches the wit structure of
    /// `id` and recursively registers types.
    fn defined(&mut self, id: TypeId, def: &types::ComponentDefinedType) -> Result<()> {
        match def {
            types::ComponentDefinedType::Primitive(_) => Ok(()),

            types::ComponentDefinedType::List(t) => {
                let ty = match &self.resolve.types[id].kind {
                    TypeDefKind::List(r) => r,
                    // Note that all cases below have this match and the general
                    // idea is that once a type is named or otherwise identified
                    // here there's no need to recurse. The purpose of this
                    // registrar is to build connections for anonymous types
                    // that don't otherwise have a name to ensure that they're
                    // decoded to reuse the same constructs consistently. For
                    // that reason once something is named we can bail out.
                    TypeDefKind::Type(Type::Id(_)) => return Ok(()),
                    _ => bail!("expected a list"),
                };
                self.valtype(t, ty)
            }

            types::ComponentDefinedType::Tuple(t) => {
                let ty = match &self.resolve.types[id].kind {
                    TypeDefKind::Tuple(r) => r,
                    TypeDefKind::Type(Type::Id(_)) => return Ok(()),
                    _ => bail!("expected a tuple"),
                };
                if ty.types.len() != t.types.len() {
                    bail!("mismatched number of tuple fields");
                }
                for (a, b) in t.types.iter().zip(ty.types.iter()) {
                    self.valtype(a, b)?;
                }
                Ok(())
            }

            types::ComponentDefinedType::Option(t) => {
                let ty = match &self.resolve.types[id].kind {
                    TypeDefKind::Option(r) => r,
                    TypeDefKind::Type(Type::Id(_)) => return Ok(()),
                    _ => bail!("expected an option"),
                };
                self.valtype(t, ty)
            }

            types::ComponentDefinedType::Result { ok, err } => {
                let ty = match &self.resolve.types[id].kind {
                    TypeDefKind::Result(r) => r,
                    TypeDefKind::Type(Type::Id(_)) => return Ok(()),
                    _ => bail!("expected a result"),
                };
                match (ok, &ty.ok) {
                    (Some(a), Some(b)) => self.valtype(a, b)?,
                    (None, None) => {}
                    _ => bail!("disagreement on result structure"),
                }
                match (err, &ty.err) {
                    (Some(a), Some(b)) => self.valtype(a, b)?,
                    (None, None) => {}
                    _ => bail!("disagreement on result structure"),
                }
                Ok(())
            }

            types::ComponentDefinedType::Record(def) => {
                let ty = match &self.resolve.types[id].kind {
                    TypeDefKind::Record(r) => r,
                    TypeDefKind::Type(Type::Id(_)) => return Ok(()),
                    _ => bail!("expected a record"),
                };
                if def.fields.len() != ty.fields.len() {
                    bail!("mismatched number of record fields");
                }
                for ((name, ty), field) in def.fields.iter().zip(&ty.fields) {
                    if name.as_str() != field.name {
                        bail!("mismatched field order");
                    }
                    self.valtype(ty, &field.ty)?;
                }
                Ok(())
            }

            types::ComponentDefinedType::Variant(def) => {
                let ty = match &self.resolve.types[id].kind {
                    TypeDefKind::Variant(r) => r,
                    TypeDefKind::Type(Type::Id(_)) => return Ok(()),
                    _ => bail!("expected a variant"),
                };
                if def.cases.len() != ty.cases.len() {
                    bail!("mismatched number of variant cases");
                }
                for ((name, ty), case) in def.cases.iter().zip(&ty.cases) {
                    if name.as_str() != case.name {
                        bail!("mismatched case order");
                    }
                    match (&ty.ty, &case.ty) {
                        (Some(a), Some(b)) => self.valtype(a, b)?,
                        (None, None) => {}
                        _ => bail!("disagreement on case type"),
                    }
                }
                Ok(())
            }

            types::ComponentDefinedType::Union(t) => {
                let ty = match &self.resolve.types[id].kind {
                    TypeDefKind::Union(r) => r,
                    TypeDefKind::Type(Type::Id(_)) => return Ok(()),
                    _ => bail!("expected a union"),
                };
                if ty.cases.len() != t.types.len() {
                    bail!("mismatched number of tuple fields");
                }
                for (a, b) in t.types.iter().zip(ty.cases.iter()) {
                    self.valtype(a, &b.ty)?;
                }
                Ok(())
            }

            // These have no recursive structure so they can bail out.
            types::ComponentDefinedType::Flags(_) => Ok(()),
            types::ComponentDefinedType::Enum(_) => Ok(()),
        }
    }

    fn valtype(&mut self, wasm: &types::ComponentValType, wit: &Type) -> Result<()> {
        match wasm {
            types::ComponentValType::Primitive(_wasm) => {
                assert!(!matches!(wit, Type::Id(_)));
                Ok(())
            }
            types::ComponentValType::Type(wasm) => {
                let wit = match wit {
                    Type::Id(id) => *id,
                    _ => bail!("expected id-based type"),
                };
                match self.type_map.insert(*wasm, wit) {
                    Some(prev) => {
                        assert_eq!(prev, wit);
                        Ok(())
                    }
                    None => {
                        let wasm = match self.types.type_from_id(*wasm) {
                            Some(types::Type::Defined(ty)) => ty,
                            _ => unreachable!(),
                        };
                        self.defined(wit, wasm)
                    }
                }
            }
        }
    }
}
