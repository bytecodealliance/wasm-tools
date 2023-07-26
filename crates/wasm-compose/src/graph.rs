//! Module for WebAssembly composition graphs.
use crate::encoding::{CompositionGraphEncoder, TypeEncoder};
use anyhow::{anyhow, bail, Context, Result};
use indexmap::{IndexMap, IndexSet};
use petgraph::{algo::toposort, graphmap::DiGraphMap, EdgeDirection};
use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap, HashSet},
    path::{Path, PathBuf},
    sync::atomic::{AtomicUsize, Ordering},
};
use wasmparser::{
    types::{ComponentEntityType, TypeId, Types, TypesRef},
    Chunk, ComponentExternalKind, ComponentTypeRef, Encoding, Parser, Payload, ValidPayload,
    Validator, WasmFeatures,
};

pub(crate) fn type_desc(item: ComponentEntityType) -> &'static str {
    match item {
        ComponentEntityType::Instance(_) => "instance",
        ComponentEntityType::Module(_) => "module",
        ComponentEntityType::Func(_) => "function",
        ComponentEntityType::Value(_) => "value",
        ComponentEntityType::Type { .. } => "type",
        ComponentEntityType::Component(_) => "component",
    }
}

/// Represents a component in a composition graph.
pub struct Component<'a> {
    /// The name of the component.
    pub(crate) name: String,
    /// The path to the component file if parsed via `Component::from_file`.
    pub(crate) path: Option<PathBuf>,
    /// The raw bytes of the component.
    pub(crate) bytes: Cow<'a, [u8]>,
    /// The type information of the component.
    pub(crate) types: Types,
    /// The import map of the component.
    pub(crate) imports: IndexMap<String, ComponentTypeRef>,
    /// The export map of the component.
    pub(crate) exports: IndexMap<String, (ComponentExternalKind, u32)>,
}

impl<'a> Component<'a> {
    /// Constructs a new component from reading the given file.
    pub fn from_file(name: impl Into<String>, path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        log::info!("parsing WebAssembly component file `{}`", path.display());

        let component = Self::parse(
            name.into(),
            Some(path.to_owned()),
            wat::parse_file(path)
                .with_context(|| {
                    format!("failed to parse component `{path}`", path = path.display())
                })?
                .into(),
        )
        .with_context(|| format!("failed to parse component `{path}`", path = path.display()))?;

        log::debug!(
            "WebAssembly component `{path}` parsed:\n{component:#?}",
            path = path.display()
        );

        Ok(component)
    }

    /// Constructs a new component from the given bytes.
    pub fn from_bytes(name: impl Into<String>, bytes: impl Into<Cow<'a, [u8]>>) -> Result<Self> {
        let mut bytes = bytes.into();

        match wat::parse_bytes(bytes.as_ref()).context("failed to parse component")? {
            Cow::Borrowed(_) => {
                // Original bytes were not modified
            }
            Cow::Owned(v) => bytes = v.into(),
        }

        log::info!("parsing WebAssembly component from bytes");
        let component =
            Self::parse(name.into(), None, bytes).context("failed to parse component")?;

        log::debug!("WebAssembly component parsed:\n{component:#?}",);

        Ok(component)
    }

    fn parse(name: String, path: Option<PathBuf>, bytes: Cow<'a, [u8]>) -> Result<Self> {
        let mut parser = Parser::new(0);
        let mut parsers = Vec::new();
        let mut validator = Validator::new_with_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });
        let mut imports = IndexMap::new();
        let mut exports = IndexMap::new();

        let mut cur = bytes.as_ref();
        loop {
            match parser.parse(cur, true)? {
                Chunk::Parsed { payload, consumed } => {
                    cur = &cur[consumed..];

                    match validator.payload(&payload)? {
                        ValidPayload::Ok => {
                            // Don't parse any sub-components or sub-modules
                            if !parsers.is_empty() {
                                continue;
                            }

                            match payload {
                                Payload::Version { encoding, .. } => {
                                    if encoding != Encoding::Component {
                                        bail!(
                                            "the {} is not a WebAssembly component",
                                            if path.is_none() { "given data" } else { "file" }
                                        );
                                    }
                                }
                                Payload::ComponentImportSection(s) => {
                                    for import in s {
                                        let import = import?;
                                        let name = match import.name {
                                            wasmparser::ComponentExternName::Kebab(s)
                                            | wasmparser::ComponentExternName::Interface(s) => {
                                                s.to_string()
                                            }
                                        };
                                        imports.insert(name, import.ty);
                                    }
                                }
                                Payload::ComponentExportSection(s) => {
                                    for export in s {
                                        let export = export?;
                                        let name = match export.name {
                                            wasmparser::ComponentExternName::Kebab(s)
                                            | wasmparser::ComponentExternName::Interface(s) => {
                                                s.to_string()
                                            }
                                        };
                                        exports.insert(name, (export.kind, export.index));
                                    }
                                }
                                _ => {}
                            }
                        }
                        ValidPayload::Func(_, _) => {}
                        ValidPayload::Parser(next) => {
                            parsers.push(parser);
                            parser = next;
                        }
                        ValidPayload::End(types) => match parsers.pop() {
                            Some(parent) => parser = parent,
                            None => {
                                return Ok(Component {
                                    name,
                                    path,
                                    bytes,
                                    types,
                                    imports,
                                    exports,
                                });
                            }
                        },
                    }
                }
                Chunk::NeedMoreData(_) => unreachable!(),
            }
        }
    }

    /// Gets the name of the component.
    ///
    /// Names must be unique within a composition graph.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Gets the path of the component.
    ///
    /// Returns `None` if the component was not loaded from a file.
    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }

    /// Gets the bytes of the component.
    pub fn bytes(&self) -> &[u8] {
        self.bytes.as_ref()
    }

    /// Gets the type information of the component.
    pub fn types(&self) -> TypesRef {
        self.types.as_ref()
    }

    /// Gets an export from the component for the given export index.
    pub fn export(
        &self,
        index: impl Into<ExportIndex>,
    ) -> Option<(&str, ComponentExternalKind, u32)> {
        let index = index.into();
        self.exports
            .get_index(index.0)
            .map(|(name, (kind, index))| (name.as_str(), *kind, *index))
    }

    /// Gets an export from the component for the given export name.
    pub fn export_by_name(&self, name: &str) -> Option<(ExportIndex, ComponentExternalKind, u32)> {
        self.exports
            .get_full(name)
            .map(|(i, _, (kind, index))| (ExportIndex(i), *kind, *index))
    }

    /// Gets an iterator over the component's exports.
    pub fn exports(
        &self,
    ) -> impl ExactSizeIterator<Item = (ExportIndex, &str, ComponentExternalKind, u32)> {
        self.exports
            .iter()
            .enumerate()
            .map(|(i, (name, (kind, index)))| (ExportIndex(i), name.as_str(), *kind, *index))
    }

    /// Gets an import from the component for the given import index.
    pub fn import(&self, index: impl Into<ImportIndex>) -> Option<(&str, ComponentTypeRef)> {
        let index = index.into();
        self.imports
            .get_index(index.0)
            .map(|(name, ty)| (name.as_str(), *ty))
    }

    /// Gets an import from the component for the given import name.
    pub fn import_by_name(&self, name: &str) -> Option<(ImportIndex, ComponentTypeRef)> {
        self.imports
            .get_full(name)
            .map(|(i, _, ty)| (ImportIndex(i), *ty))
    }

    /// Gets an iterator over the component's imports.
    pub fn imports(&self) -> impl ExactSizeIterator<Item = (ImportIndex, &str, ComponentTypeRef)> {
        self.imports
            .iter()
            .enumerate()
            .map(|(i, (name, ty))| (ImportIndex(i), name.as_str(), *ty))
    }

    pub(crate) fn ty(&self) -> wasm_encoder::ComponentType {
        let encoder = TypeEncoder::new(self);

        encoder.component(
            &mut Default::default(),
            self.imports()
                .map(|(i, ..)| self.import_entity_type(i).unwrap()),
            self.exports()
                .map(|(i, ..)| self.export_entity_type(i).unwrap()),
        )
    }

    pub(crate) fn export_entity_type(
        &self,
        index: ExportIndex,
    ) -> Option<(&str, ComponentEntityType)> {
        let (name, _kind, _index) = self.export(index)?;
        Some((name, self.types.component_entity_type_of_export(name)?))
    }

    pub(crate) fn import_entity_type(
        &self,
        index: ImportIndex,
    ) -> Option<(&str, ComponentEntityType)> {
        let (name, _ty) = self.import(index)?;
        Some((name, self.types.component_entity_type_of_import(name)?))
    }

    /// Finds a compatible instance export on the component for the given instance type.
    pub(crate) fn find_compatible_export(
        &self,
        ty: TypeId,
        types: TypesRef,
    ) -> Option<ExportIndex> {
        self.exports
            .iter()
            .position(|(_, (kind, index))| {
                if *kind != ComponentExternalKind::Instance {
                    return false;
                }
                ComponentEntityType::is_subtype_of(
                    &ComponentEntityType::Instance(self.types.component_instance_at(*index)),
                    self.types(),
                    &ComponentEntityType::Instance(ty),
                    types,
                )
            })
            .map(ExportIndex)
    }

    /// Checks to see if an instance of this component would be a
    /// subtype of the given instance type.
    pub(crate) fn is_instance_subtype_of(&self, ty: TypeId, types: TypesRef) -> bool {
        let exports = types[ty].unwrap_component_instance().exports.iter();

        for (k, b) in exports {
            match self.exports.get_full(k.as_str()) {
                Some((ai, _, _)) => {
                    let (_, a) = self.export_entity_type(ExportIndex(ai)).unwrap();
                    if !ComponentEntityType::is_subtype_of(&a, self.types(), b, types) {
                        return false;
                    }
                }
                None => return false,
            }
        }

        true
    }
}

impl std::fmt::Debug for Component<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Component")
            .field("imports", &self.imports)
            .field("exports", &self.exports)
            .finish_non_exhaustive()
    }
}

static NEXT_COMPONENT_ID: AtomicUsize = AtomicUsize::new(0);

/// Represents an identifier of a component in a composition graph.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ComponentId(pub usize);

impl ComponentId {
    fn next() -> Result<Self> {
        let next = NEXT_COMPONENT_ID.fetch_add(1, Ordering::SeqCst);
        if next == usize::MAX {
            bail!("component limit reached");
        }
        Ok(Self(next))
    }
}

impl std::fmt::Display for ComponentId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<usize> for ComponentId {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

static NEXT_INSTANCE_ID: AtomicUsize = AtomicUsize::new(0);

/// Represents an identifier of an instance in a composition graph.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct InstanceId(pub usize);

impl std::fmt::Display for InstanceId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl InstanceId {
    fn next() -> Result<Self> {
        let next = NEXT_INSTANCE_ID.fetch_add(1, Ordering::SeqCst);
        if next == usize::MAX {
            bail!("instance limit reached");
        }
        Ok(Self(next))
    }
}

impl From<usize> for InstanceId {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

/// Represents an index into a component's import list.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ImportIndex(pub usize);

impl std::fmt::Display for ImportIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<usize> for ImportIndex {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

/// Represents an index into a component's export list.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ExportIndex(pub usize);

impl std::fmt::Display for ExportIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<usize> for ExportIndex {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Debug)]
pub(crate) struct ComponentEntry<'a> {
    pub(crate) component: Component<'a>,
    pub(crate) instances: HashSet<InstanceId>,
}

#[derive(Debug)]
pub(crate) struct Instance {
    pub(crate) component: ComponentId,
    pub(crate) connected: IndexSet<ImportIndex>,
}

/// The options for encoding a composition graph.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct EncodeOptions {
    /// Whether or not to define instantiated components.
    ///
    /// If `false`, components will be imported instead.
    pub define_components: bool,

    /// The instance in the graph to export.
    ///
    /// If `Some`, the instance's exports will be aliased and
    /// exported from the resulting component.
    pub export: Option<InstanceId>,

    /// Whether or not to validate the encoded output.
    pub validate: bool,
}

/// Represents a composition graph used to compose a new component
/// from other components.
#[derive(Debug, Default)]
pub struct CompositionGraph<'a> {
    names: HashMap<String, ComponentId>,
    pub(crate) components: IndexMap<ComponentId, ComponentEntry<'a>>,
    pub(crate) instances: IndexMap<InstanceId, Instance>,
    // Map where each node is an instance in the graph.
    // An edge between nodes stores a map of target import index to source export index.
    // A source export index of `None` means that the source instance itself is being used.
    pub(crate) graph: DiGraphMap<InstanceId, IndexMap<ImportIndex, Option<ExportIndex>>>,
}

impl<'a> CompositionGraph<'a> {
    /// Constructs a new composition graph.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a new component to the graph.
    ///
    /// The component name must be unique.
    pub fn add_component(&mut self, component: Component<'a>) -> Result<ComponentId> {
        let id = match self.names.entry(component.name.clone()) {
            Entry::Occupied(e) => {
                bail!(
                    "a component with name `{name}` already exists",
                    name = e.key()
                )
            }
            Entry::Vacant(e) => *e.insert(ComponentId::next()?),
        };

        log::info!(
            "adding WebAssembly component `{name}` ({id}) to the graph",
            name = component.name(),
        );

        let entry = ComponentEntry {
            component,
            instances: HashSet::new(),
        };

        assert!(self.components.insert(id, entry).is_none());

        Ok(id)
    }

    /// Gets a component from the graph.
    pub fn get_component(&self, id: impl Into<ComponentId>) -> Option<&Component<'a>> {
        self.components.get(&id.into()).map(|e| &e.component)
    }

    /// Gets a component from the graph by name.
    pub fn get_component_by_name(&self, name: &str) -> Option<(ComponentId, &Component<'a>)> {
        let id = self.names.get(name)?;
        let entry = &self.components[id];
        Some((*id, &entry.component))
    }

    /// Removes a component from the graph.
    ///
    /// All instances and connections relating to the component
    /// will also be removed.
    pub fn remove_component(&mut self, id: impl Into<ComponentId>) {
        let id = id.into();
        if let Some(entry) = self.components.remove(&id) {
            log::info!(
                "removing WebAssembly component `{name}` ({id}) from the graph",
                name = entry.component.name(),
            );

            assert!(self.names.remove(&entry.component.name).is_some());

            for instance_id in entry.instances.iter().copied() {
                self.instances.remove(&instance_id);

                // Remove any connected indexes from outward edges from the instance being removed
                for (_, target_id, map) in self
                    .graph
                    .edges_directed(instance_id, EdgeDirection::Outgoing)
                {
                    let target = self.instances.get_mut(&target_id).unwrap();
                    for index in map.keys() {
                        target.connected.remove(index);
                    }
                }

                self.graph.remove_node(instance_id);
            }
        }
    }

    /// Creates a new instance of a component in the composition graph.
    pub fn instantiate(&mut self, id: impl Into<ComponentId>) -> Result<InstanceId> {
        let id = id.into();
        let entry = self
            .components
            .get_mut(&id)
            .ok_or_else(|| anyhow!("component does not exist in the graph"))?;

        let instance_id = InstanceId::next()?;

        log::info!(
            "instantiating WebAssembly component `{name}` ({id}) with instance identifier {instance_id}",
            name = entry.component.name(),
        );

        self.instances.insert(
            instance_id,
            Instance {
                component: id,
                connected: Default::default(),
            },
        );

        entry.instances.insert(instance_id);

        Ok(instance_id)
    }

    /// Gets the component of the given instance.
    pub fn get_component_of_instance(
        &self,
        id: impl Into<InstanceId>,
    ) -> Option<(ComponentId, &Component)> {
        let id = id.into();
        let instance = self.instances.get(&id)?;

        Some((
            instance.component,
            self.get_component(instance.component).unwrap(),
        ))
    }

    /// Removes an instance from the graph.
    ///
    /// All connections relating to the instance will also be removed.
    pub fn remove_instance(&mut self, id: impl Into<InstanceId>) {
        let id = id.into();
        if let Some(instance) = self.instances.remove(&id) {
            let entry = self.components.get_mut(&instance.component).unwrap();

            log::info!(
                "removing instance ({id}) of component `{name}` ({cid}) from the graph",
                name = entry.component.name(),
                cid = instance.component.0,
            );

            entry.instances.remove(&id);

            // Remove any connected indexes from outward edges from this instance
            for (_, target, map) in self.graph.edges_directed(id, EdgeDirection::Outgoing) {
                let target = self.instances.get_mut(&target).unwrap();
                for index in map.keys() {
                    target.connected.remove(index);
                }
            }

            self.graph.remove_node(id);
        }
    }

    /// Creates a connection (edge) between instances in the composition graph.
    ///
    /// A connection represents an instantiation argument.
    ///
    /// If `source_export` is `None`, the source instance itself
    /// is used as the instantiation argument.
    pub fn connect(
        &mut self,
        source: impl Into<InstanceId> + Copy,
        source_export: Option<impl Into<ExportIndex> + Copy>,
        target: impl Into<InstanceId> + Copy,
        target_import: impl Into<ImportIndex> + Copy,
    ) -> Result<()> {
        self.validate_connection(source, source_export, target, target_import)?;

        let source = source.into();
        let source_export = source_export.map(Into::into);
        let target = target.into();
        let target_import = target_import.into();

        match source_export {
            Some(export) => log::info!("connecting export {export} of instance {source} to import `{target_import}` of instance {target}"),
            None => log::info!("connecting instance {source} to import {target_import} of instance {target}"),
        }

        self.instances
            .get_mut(&target)
            .unwrap()
            .connected
            .insert(target_import);

        if let Some(map) = self.graph.edge_weight_mut(source, target) {
            assert!(map.insert(target_import, source_export).is_none());
        } else {
            let mut map = IndexMap::new();
            map.insert(target_import, source_export);
            self.graph.add_edge(source, target, map);
        }

        Ok(())
    }

    /// Disconnects a previous connection between instances.
    ///
    /// Requires that the source and target instances are valid.
    ///
    /// If the source and target are not connected via the target's import,
    /// then this is a no-op.
    pub fn disconnect(
        &mut self,
        source: impl Into<InstanceId>,
        target: impl Into<InstanceId>,
        target_import: impl Into<ImportIndex>,
    ) -> Result<()> {
        let source = source.into();
        let target = target.into();
        let target_import = target_import.into();

        log::info!("disconnecting import {target_import} of instance {target}");

        if !self.instances.contains_key(&source) {
            bail!("the source instance does not exist in the graph");
        }

        let target_instance = self
            .instances
            .get_mut(&target)
            .ok_or_else(|| anyhow!("the target instance does not exist in the graph"))?;

        target_instance.connected.remove(&target_import);

        let remove_edge = if let Some(set) = self.graph.edge_weight_mut(source, target) {
            set.remove(&target_import);
            set.is_empty()
        } else {
            false
        };

        if remove_edge {
            self.graph.remove_edge(source, target);
        }

        Ok(())
    }

    /// Validates a connection between two instances in the graph.
    ///
    /// Use `None` for `source_export` to signify that the instance
    /// itself should be the source for the connection.
    ///
    /// Returns `Err(_)` if the connection would not be valid.
    pub fn validate_connection(
        &self,
        source: impl Into<InstanceId>,
        source_export: Option<impl Into<ExportIndex>>,
        target: impl Into<InstanceId>,
        target_import: impl Into<ImportIndex>,
    ) -> Result<()> {
        let source = source.into();
        let source_export = source_export.map(Into::into);
        let target = target.into();
        let target_import = target_import.into();

        if source == target {
            bail!("an instance cannot be connected to itself");
        }

        let source_instance = self
            .instances
            .get(&source)
            .ok_or_else(|| anyhow!("the source instance does not exist in the graph"))?;

        let source_component = &self.components[&source_instance.component].component;

        let target_instance = self
            .instances
            .get(&target)
            .ok_or_else(|| anyhow!("the target instance does not exist in the graph"))?;

        let target_component = &self.components[&target_instance.component].component;
        let (import_name, import_ty) = target_component
            .import_entity_type(target_import)
            .ok_or_else(|| anyhow!("the target import index is invalid"))?;

        if target_instance.connected.contains(&target_import) {
            bail!(
                "{import_ty} import `{import_name}` is already connected",
                import_ty = type_desc(import_ty)
            );
        }

        if let Some(export_index) = source_export {
            let (export_name, export_ty) = source_component
                .export_entity_type(export_index)
                .ok_or_else(|| anyhow!("the source export index is invalid"))?;

            if !ComponentEntityType::is_subtype_of(
                &export_ty,
                source_component.types(),
                &import_ty,
                target_component.types(),
            ) {
                bail!(
                    "source {export_ty} export `{export_name}` is not compatible with target {import_ty} import `{import_name}`",
                    export_ty = type_desc(export_ty),
                    import_ty = type_desc(import_ty),
                );
            }
        } else {
            let ty = match import_ty {
                ComponentEntityType::Instance(id) => id,
                _ => bail!(
                    "source instance is not compatible with target {import_ty} import `{import_name}`",
                    import_ty = type_desc(import_ty)
                ),
            };

            if !source_component.is_instance_subtype_of(ty, target_component.types()) {
                bail!(
                    "source instance is not compatible with target {import_ty} import `{import_name}`",
                    import_ty = type_desc(import_ty)
                );
            }
        };

        Ok(())
    }

    /// Encodes the current composition graph as a WebAssembly component.
    pub fn encode(&self, options: EncodeOptions) -> Result<Vec<u8>> {
        let bytes = CompositionGraphEncoder::new(options, self).encode()?;

        if options.validate {
            Validator::new_with_features(WasmFeatures {
                component_model: true,
                ..Default::default()
            })
            .validate_all(&bytes)
            .context("failed to validate encoded graph bytes")?;
        }

        Ok(bytes)
    }

    /// Gets the topological instantiation order based on the composition graph.
    ///
    /// If an instance is not in the returned set, it is considered to be
    /// "independent" (i.e it has no dependencies on other instances).
    pub(crate) fn instantiation_order(&self) -> Result<Vec<InstanceId>> {
        toposort(&self.graph, None).map_err(|e| {
            let id = e.node_id();
            let instance = &self.instances[&id];
            anyhow!(
                "an instantiation of component `{name}` and its dependencies form a cycle in the instantiation graph",
                name = self.components[&instance.component].component.name,
            )
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn it_rejects_modules() -> Result<()> {
        match Component::from_bytes("a", b"(module)".as_ref()) {
            Ok(_) => panic!("expected a failure to parse"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "failed to parse component: the given data is not a WebAssembly component"
            ),
        }

        Ok(())
    }

    #[test]
    fn it_rejects_invalid_components() -> Result<()> {
        match Component::from_bytes("a", b"(component (export \"x\" (func 0)))".as_ref()) {
            Ok(_) => panic!("expected a failure to parse"),
            Err(e) => assert_eq!(format!("{e:#}"), "failed to parse component: unknown function 0: function index out of bounds (at offset 0xb)"),
        }

        Ok(())
    }

    #[test]
    fn it_ensures_unique_component_names() -> Result<()> {
        let mut graph = CompositionGraph::new();
        graph.add_component(Component::from_bytes("a", b"(component)".as_ref())?)?;

        match graph.add_component(Component::from_bytes("a", b"(component)".as_ref())?) {
            Ok(_) => panic!("expected a failure to add component"),
            Err(e) => assert_eq!(format!("{e:#}"), "a component with name `a` already exists"),
        }

        Ok(())
    }

    #[test]
    fn it_fails_to_instantiate_a_missing_component() -> Result<()> {
        let mut graph = CompositionGraph::new();
        match graph.instantiate(ComponentId(0)) {
            Ok(_) => panic!("expected a failure to instantiate"),
            Err(e) => assert_eq!(format!("{e:#}"), "component does not exist in the graph"),
        }

        Ok(())
    }

    #[test]
    fn it_instantiates_a_component() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let id = graph.add_component(Component::from_bytes("a", b"(component)".as_ref())?)?;
        let id = graph.instantiate(id)?;
        assert_eq!(graph.get_component_of_instance(id).unwrap().1.name(), "a");
        Ok(())
    }

    #[test]
    fn it_cannot_get_a_component_of_missing_instance() -> Result<()> {
        let graph = CompositionGraph::new();
        assert!(graph.get_component_of_instance(InstanceId(0)).is_none());
        Ok(())
    }

    #[test]
    fn it_gets_a_component() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let id = graph.add_component(Component::from_bytes("a", b"(component)".as_ref())?)?;
        assert_eq!(graph.get_component(id).unwrap().name(), "a");
        assert_eq!(graph.get_component_by_name("a").unwrap().1.name(), "a");
        Ok(())
    }

    #[test]
    fn it_removes_a_component() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let a = graph.add_component(Component::from_bytes(
            "a",
            b"(component (import \"x\" (func)))".as_ref(),
        )?)?;
        let b = graph.add_component(Component::from_bytes(
            "b",
            b"(component (import \"x\" (func)) (export \"y\" (func 0)))".as_ref(),
        )?)?;
        let ai = graph.instantiate(a)?;
        let bi = graph.instantiate(b)?;
        graph.connect(bi, Some(0), ai, 0)?;

        assert!(graph.get_component(a).is_some());
        assert!(graph.get_component(b).is_some());
        assert_eq!(graph.components.len(), 2);
        assert_eq!(graph.instances.len(), 2);
        assert_eq!(graph.graph.node_count(), 2);
        assert_eq!(graph.graph.edge_count(), 1);

        graph.remove_component(b);

        assert!(graph.get_component(a).is_some());
        assert!(graph.get_component(b).is_none());
        assert_eq!(graph.components.len(), 1);
        assert_eq!(graph.instances.len(), 1);
        assert_eq!(graph.graph.node_count(), 1);
        assert_eq!(graph.graph.edge_count(), 0);
        Ok(())
    }

    #[test]
    fn it_removes_a_connection() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let a = graph.add_component(Component::from_bytes(
            "a",
            b"(component (import \"x\" (func)))".as_ref(),
        )?)?;
        let b = graph.add_component(Component::from_bytes(
            "b",
            b"(component (import \"x\" (func)) (export \"y\" (func 0)))".as_ref(),
        )?)?;
        let ai = graph.instantiate(a)?;
        let bi = graph.instantiate(b)?;
        graph.connect(bi, Some(0), ai, 0)?;

        assert_eq!(graph.graph.node_count(), 2);
        assert_eq!(graph.graph.edge_count(), 1);

        graph.disconnect(bi, ai, 0)?;

        assert_eq!(graph.graph.node_count(), 2);
        assert_eq!(graph.graph.edge_count(), 0);
        Ok(())
    }

    #[test]
    fn it_requires_source_to_disconnect() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let a = graph.add_component(Component::from_bytes(
            "a",
            b"(component (import \"x\" (func)))".as_ref(),
        )?)?;
        let b = graph.add_component(Component::from_bytes(
            "b",
            b"(component (import \"x\" (func)) (export \"y\" (func 0)))".as_ref(),
        )?)?;
        let ai = graph.instantiate(a)?;
        let bi = graph.instantiate(b)?;
        graph.connect(bi, Some(0), ai, 0)?;

        match graph.disconnect(101, ai, 0) {
            Ok(_) => panic!("expected a failure to disconnect"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "the source instance does not exist in the graph"
            ),
        }

        Ok(())
    }

    #[test]
    fn it_requires_a_target_to_disconnect() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let a = graph.add_component(Component::from_bytes(
            "a",
            b"(component (import \"x\" (func)))".as_ref(),
        )?)?;
        let b = graph.add_component(Component::from_bytes(
            "b",
            b"(component (import \"x\" (func)) (export \"y\" (func 0)))".as_ref(),
        )?)?;
        let ai = graph.instantiate(a)?;
        let bi = graph.instantiate(b)?;
        graph.connect(bi, Some(0), ai, 0)?;

        match graph.disconnect(bi, 101, 0) {
            Ok(_) => panic!("expected a failure to disconnect"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "the target instance does not exist in the graph"
            ),
        }

        Ok(())
    }

    #[test]
    fn it_validates_connections() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let a = graph.add_component(Component::from_bytes(
            "a",
            b"(component (import \"i1\" (func)) (import \"i2\" (instance (export \"no\" (func)))))"
                .as_ref(),
        )?)?;
        let b = graph.add_component(Component::from_bytes(
            "b",
            b"(component (import \"i1\" (func)) (import \"i2\" (core module)) (export \"e1\" (func 0)) (export \"e2\" (core module 0)))".as_ref(),
        )?)?;
        let ai = graph.instantiate(a)?;
        let bi = graph.instantiate(b)?;

        match graph.connect(ai, None::<ExportIndex>, ai, 0) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "an instance cannot be connected to itself"
            ),
        }

        match graph.connect(ai, Some(0), bi, 0) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(format!("{e:#}"), "the source export index is invalid"),
        }

        match graph.connect(101, Some(0), ai, 0) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "the source instance does not exist in the graph"
            ),
        }

        match graph.connect(bi, Some(0), 101, 0) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "the target instance does not exist in the graph"
            ),
        }

        match graph.connect(bi, Some(101), ai, 0) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(format!("{e:#}"), "the source export index is invalid"),
        }

        match graph.connect(bi, Some(0), ai, 101) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(format!("{e:#}"), "the target import index is invalid"),
        }

        match graph.connect(bi, Some(1), ai, 0) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "source module export `e2` is not compatible with target function import `i1`"
            ),
        }

        match graph.connect(bi, None::<ExportIndex>, ai, 0) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "source instance is not compatible with target function import `i1`"
            ),
        }

        match graph.connect(bi, None::<ExportIndex>, ai, 1) {
            Ok(_) => panic!("expected a failure to connect"),
            Err(e) => assert_eq!(
                format!("{e:#}"),
                "source instance is not compatible with target instance import `i2`"
            ),
        }

        Ok(())
    }

    #[test]
    fn it_cannot_encode_a_cycle() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let a = graph.add_component(Component::from_bytes(
            "a",
            b"(component (import \"i1\" (func)) (export \"e1\" (func 0)))".as_ref(),
        )?)?;
        let b = graph.add_component(Component::from_bytes(
            "b",
            b"(component (import \"i1\" (func)) (export \"e1\" (func 0)))".as_ref(),
        )?)?;
        let ai = graph.instantiate(a)?;
        let bi = graph.instantiate(b)?;

        graph.connect(ai, Some(0), bi, 0)?;
        graph.connect(bi, Some(0), ai, 0)?;

        match graph.encode(EncodeOptions {
            define_components: false,
            export: None,
            validate: true,
        }) {
            Ok(_) => panic!("graph should not encode"),
            Err(e) => assert_eq!(format!("{e:#}"), "an instantiation of component `b` and its dependencies form a cycle in the instantiation graph"),
        }

        Ok(())
    }

    #[test]
    fn it_encodes_an_empty_component() -> Result<()> {
        let mut graph = CompositionGraph::new();
        graph.add_component(Component::from_bytes("a", b"(component)".as_ref())?)?;
        graph.add_component(Component::from_bytes("b", b"(component)".as_ref())?)?;

        let encoded = graph.encode(EncodeOptions {
            define_components: false,
            export: None,
            validate: true,
        })?;

        let wat = wasmprinter::print_bytes(encoded)?;
        assert_eq!(r#"(component)"#, wat);

        Ok(())
    }

    #[test]
    fn it_encodes_component_imports() -> Result<()> {
        let mut graph = CompositionGraph::new();
        // Add a component that doesn't get instantiated (shouldn't be imported)
        graph.add_component(Component::from_bytes("a", b"(component)".as_ref())?)?;
        let b = graph.add_component(Component::from_bytes("b", b"(component)".as_ref())?)?;
        graph.instantiate(b)?;

        let encoded = graph.encode(EncodeOptions {
            define_components: false,
            export: None,
            validate: true,
        })?;

        let wat = wasmprinter::print_bytes(encoded)?.replace("\r\n", "\n");
        assert_eq!(
            r#"(component
  (type (;0;)
    (component)
  )
  (import "b" (component (;0;) (type 0)))
  (instance (;0;) (instantiate 0))
)"#,
            wat
        );

        Ok(())
    }

    #[test]
    fn it_encodes_defined_components() -> Result<()> {
        let mut graph = CompositionGraph::new();
        // Add a component that doesn't get instantiated (shouldn't be imported)
        graph.add_component(Component::from_bytes("a", b"(component)".as_ref())?)?;
        let b = graph.add_component(Component::from_bytes("b", b"(component)".as_ref())?)?;
        graph.instantiate(b)?;

        let encoded = graph.encode(EncodeOptions {
            define_components: true,
            export: None,
            validate: true,
        })?;

        let wat = wasmprinter::print_bytes(encoded)?.replace("\r\n", "\n");
        assert_eq!(
            r#"(component
  (component (;0;))
  (instance (;0;) (instantiate 0))
)"#,
            wat
        );

        Ok(())
    }

    #[test]
    fn it_encodes_a_simple_composition() -> Result<()> {
        let mut graph = CompositionGraph::new();
        let a = graph.add_component(Component::from_bytes(
            "a",
            b"(component
  (type (tuple u32 u32))
  (import \"i1\" (instance (export \"e1\" (func)) (export \"e3\" (func (param \"a\" u32)))))
  (import \"i2\" (func))
  (import \"i3\" (component))
  (import \"i4\" (core module))
  (import \"i5\" (type (eq 0)))
  (export \"e1\" (instance 0))
  (export \"e2\" (func 0))
  (export \"e3\" (component 0))
  (export \"e4\" (core module 0))
  (export \"e5\" (type 1))
)"
            .as_ref(),
        )?)?;
        let b = graph.add_component(Component::from_bytes(
            "b",
            b"(component
  (type (tuple u32 u32))
  (import \"i1\" (instance (export \"e2\" (func)) (export \"e3\" (func (param \"a\" u32)))))
  (import \"i2\" (func))
  (import \"i3\" (component))
  (import \"i4\" (core module))
  (import \"i5\" (type (eq 0)))
)"
            .as_ref(),
        )?)?;

        let ai = graph.instantiate(a)?;
        let bi1 = graph.instantiate(b)?;
        let bi2 = graph.instantiate(b)?;
        let bi3 = graph.instantiate(b)?;

        // Skip the instance arguments so a merged instance is imported
        for i in 1..=3 {
            graph.connect(ai, Some(i), bi1, i)?;
            graph.connect(ai, Some(i), bi2, i)?;
            graph.connect(ai, Some(i), bi3, i)?;
        }

        let encoded = graph.encode(EncodeOptions {
            define_components: true,
            export: None,
            validate: true,
        })?;

        let wat = wasmprinter::print_bytes(encoded)?.replace("\r\n", "\n");
        assert_eq!(
            r#"(component
  (type (;0;)
    (instance
      (type (;0;) (func))
      (export (;0;) "e1" (func (type 0)))
      (type (;1;) (func (param "a" u32)))
      (export (;1;) "e3" (func (type 1)))
      (type (;2;) (func))
      (export (;2;) "e2" (func (type 2)))
    )
  )
  (import "i1" (instance (;0;) (type 0)))
  (type (;1;) (func))
  (import "i2" (func (;0;) (type 1)))
  (type (;2;)
    (component)
  )
  (import "i3" (component (;0;) (type 2)))
  (core type (;0;)
    (module)
  )
  (import "i4" (core module (;0;) (type 0)))
  (type (;3;) (tuple u32 u32))
  (import "i5" (type (;4;) (eq 3)))
  (component (;1;)
    (type (;0;) (tuple u32 u32))
    (type (;1;)
      (instance
        (type (;0;) (func))
        (export (;0;) "e1" (func (type 0)))
        (type (;1;) (func (param "a" u32)))
        (export (;1;) "e3" (func (type 1)))
      )
    )
    (import "i1" (instance (;0;) (type 1)))
    (type (;2;) (func))
    (import "i2" (func (;0;) (type 2)))
    (type (;3;)
      (component)
    )
    (import "i3" (component (;0;) (type 3)))
    (core type (;0;)
      (module)
    )
    (import "i4" (core module (;0;) (type 0)))
    (import "i5" (type (;4;) (eq 0)))
    (export (;1;) "e1" (instance 0))
    (export (;1;) "e2" (func 0))
    (export (;1;) "e3" (component 0))
    (export (;1;) "e4" (core module 0))
    (export (;5;) "e5" (type 1))
  )
  (component (;2;)
    (type (;0;) (tuple u32 u32))
    (type (;1;)
      (instance
        (type (;0;) (func))
        (export (;0;) "e2" (func (type 0)))
        (type (;1;) (func (param "a" u32)))
        (export (;1;) "e3" (func (type 1)))
      )
    )
    (import "i1" (instance (;0;) (type 1)))
    (type (;2;) (func))
    (import "i2" (func (;0;) (type 2)))
    (type (;3;)
      (component)
    )
    (import "i3" (component (;0;) (type 3)))
    (core type (;0;)
      (module)
    )
    (import "i4" (core module (;0;) (type 0)))
    (import "i5" (type (;4;) (eq 0)))
  )
  (instance (;1;) (instantiate 1
      (with "i1" (instance 0))
      (with "i2" (func 0))
      (with "i3" (component 0))
      (with "i4" (core module 0))
      (with "i5" (type 4))
    )
  )
  (alias export 1 "e2" (func (;1;)))
  (alias export 1 "e3" (component (;3;)))
  (alias export 1 "e4" (core module (;1;)))
  (instance (;2;) (instantiate 2
      (with "i2" (func 1))
      (with "i3" (component 3))
      (with "i4" (core module 1))
      (with "i1" (instance 0))
      (with "i5" (type 4))
    )
  )
  (instance (;3;) (instantiate 2
      (with "i2" (func 1))
      (with "i3" (component 3))
      (with "i4" (core module 1))
      (with "i1" (instance 0))
      (with "i5" (type 4))
    )
  )
  (instance (;4;) (instantiate 2
      (with "i2" (func 1))
      (with "i3" (component 3))
      (with "i4" (core module 1))
      (with "i1" (instance 0))
      (with "i5" (type 4))
    )
  )
)"#,
            wat
        );

        Ok(())
    }
}
