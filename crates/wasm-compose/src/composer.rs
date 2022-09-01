//! Module for composing WebAssembly components.

use crate::{
    config::Config,
    encoding::{InstantiationGraphEncoder, TypeEncoder},
};
use anyhow::{anyhow, bail, Context, Result};
use indexmap::{IndexMap, IndexSet};
use petgraph::{algo::toposort, dot::Dot, graph::NodeIndex, visit::EdgeRef, EdgeDirection, Graph};
use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
};
use wasm_encoder::ComponentExportKind;
use wasmparser::{
    types::{ComponentEntityType, ComponentInstanceType, Types, TypesRef},
    Chunk, ComponentExport, ComponentExternalKind, ComponentImport, ComponentTypeRef, Encoding,
    Parser, Payload, ValidPayload, Validator, WasmFeatures,
};

/// The root component name used in configuration.
pub const ROOT_COMPONENT_NAME: &str = "$component";

pub(crate) struct Component {
    /// The index of the component in the instantiation graph.
    index: ComponentIndex,
    /// The path to the component file.
    path: PathBuf,
    /// The raw bytes of the component.
    bytes: Vec<u8>,
    /// The type information of the component.
    types: Types,
    /// The name to use to import this component in the composed component.
    /// If this is `None`, the component will be defined in the composed component.
    import_name: Option<String>,
    /// The import map of the component.
    imports: IndexMap<String, ComponentTypeRef>,
    /// The export map of the component.
    exports: IndexMap<String, (ComponentExternalKind, u32)>,
}

impl Component {
    fn new(
        index: ComponentIndex,
        path: impl Into<PathBuf>,
        import_name: Option<String>,
    ) -> Result<Self> {
        let path = path.into();
        log::info!("parsing WebAssembly component `{}`", path.display());

        let bytes = wat::parse_file(&path).with_context(|| {
            format!("failed to parse component `{path}`", path = path.display())
        })?;

        let mut parser = Parser::new(0);
        let mut parsers = Vec::new();
        let mut validator = Validator::new_with_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });
        let mut imports = IndexMap::new();
        let mut exports = IndexMap::new();

        let mut cur = bytes.as_slice();
        loop {
            match parser.parse(cur, true).with_context(|| {
                format!("failed to parse component `{path}`", path = path.display())
            })? {
                Chunk::Parsed { payload, consumed } => {
                    cur = &cur[consumed..];

                    match validator.payload(&payload).with_context(|| {
                        format!(
                            "failed to validate WebAssembly component `{}`",
                            path.display()
                        )
                    })? {
                        ValidPayload::Ok => {
                            // Don't parse any sub-components or sub-modules
                            if !parsers.is_empty() {
                                continue;
                            }

                            match payload {
                                Payload::Version { encoding, .. } => {
                                    if encoding != Encoding::Component {
                                        bail!(
                                            "file `{path}` is not a WebAssembly component",
                                            path = path.display()
                                        );
                                    }
                                }
                                Payload::ComponentImportSection(s) => {
                                    for import in s {
                                        let import = import?;
                                        imports.insert(import.name.to_string(), import.ty);
                                    }
                                }
                                Payload::ComponentExportSection(s) => {
                                    for export in s {
                                        let export = export?;
                                        exports.insert(
                                            export.name.to_string(),
                                            (export.kind, export.index),
                                        );
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
                                let component = Component {
                                    index,
                                    path,
                                    bytes,
                                    types,
                                    import_name,
                                    imports,
                                    exports,
                                };
                                log::debug!(
                                    "WebAssembly component `{path}` parsed:\n{component:#?}",
                                    path = component.path.display()
                                );
                                return Ok(component);
                            }
                        },
                    }
                }
                Chunk::NeedMoreData(_) => unreachable!(),
            }
        }
    }

    pub(crate) fn index(&self) -> ComponentIndex {
        self.index
    }

    pub(crate) fn path(&self) -> &Path {
        &self.path
    }

    pub(crate) fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub(crate) fn types(&self) -> TypesRef {
        self.types.as_ref()
    }

    pub(crate) fn import_name(&self) -> Option<&str> {
        self.import_name.as_deref()
    }

    pub(crate) fn ty(&self) -> wasm_encoder::ComponentType {
        let encoder = TypeEncoder::new(self.types.as_ref());

        encoder.component(
            self.imports.iter().map(|(name, ty)| {
                (
                    name.as_str(),
                    self.types
                        .component_entity_type_from_import(&ComponentImport {
                            name: name.as_str(),
                            ty: *ty,
                        })
                        .unwrap(),
                )
            }),
            self.exports.iter().map(|(name, (kind, index))| {
                (
                    name.as_str(),
                    self.types
                        .component_entity_type_from_export(&ComponentExport {
                            name: name.as_str(),
                            kind: *kind,
                            index: *index,
                        })
                        .unwrap(),
                )
            }),
        )
    }

    /// Gets an export from the component for the given export index.
    pub(crate) fn export(&self, index: ExportIndex) -> (&str, ComponentExternalKind, u32) {
        let (name, (kind, index)) = self
            .exports
            .get_index(index.0)
            .expect("invalid export index");
        (name.as_str(), *kind, *index)
    }

    /// Gets an iterator over the component's exports.
    pub(crate) fn exports(&self) -> impl Iterator<Item = (&str, ComponentExternalKind, u32)> {
        self.exports
            .iter()
            .map(|(name, (kind, index))| (name.as_str(), *kind, *index))
    }

    /// Gets an exported instance index and type with the given export name.
    fn export_instance(&self, name: &str) -> Option<(ExportIndex, &ComponentInstanceType)> {
        self.exports
            .get_full(name)
            .and_then(|(i, _, (kind, index))| match kind {
                ComponentExternalKind::Instance => Some((
                    ExportIndex(i),
                    self.types.component_instance_at(*index).unwrap(),
                )),
                _ => None,
            })
    }

    /// Finds a compatible instance export on the component for the given instance type.
    fn find_compatible_export(
        &self,
        ty: &ComponentInstanceType,
        types: TypesRef,
    ) -> Option<ExportIndex> {
        self.exports
            .iter()
            .position(|(_, (kind, index))| {
                if *kind != ComponentExternalKind::Instance {
                    return false;
                }
                ComponentInstanceType::is_subtype_of(
                    self.types.component_instance_at(*index).unwrap(),
                    self.types.as_ref(),
                    ty,
                    types,
                )
            })
            .map(ExportIndex)
    }

    /// Checks to see if an instance of this component would be a
    /// subtype of the given instance type.
    fn is_subtype_of(&self, ty: &ComponentInstanceType, types: TypesRef) -> bool {
        let exports = ty.exports(types);

        // This checks if this import's instance type is a subtype of the given instance type.
        for (k, b) in exports {
            match self.exports.get(k.as_str()) {
                Some((ak, ai)) => {
                    let a = self
                        .types
                        .component_entity_type_from_export(&ComponentExport {
                            name: k.as_str(),
                            kind: *ak,
                            index: *ai,
                        })
                        .unwrap();
                    if !ComponentEntityType::is_subtype_of(&a, self.types.as_ref(), b, types) {
                        return false;
                    }
                }
                None => return false,
            }
        }

        true
    }
}

impl std::fmt::Debug for Component {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Component")
            .field("path", &self.path)
            .field("imports", &self.imports)
            .field("exports", &self.exports)
            .finish_non_exhaustive()
    }
}

/// Represents an index into an instantiation graph's `components` collection.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub(crate) struct ComponentIndex(usize);

/// An instance index into an instantiation graph.
pub(crate) type InstanceIndex = NodeIndex;

/// Represents an index into a component's imports collection.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub(crate) struct ImportIndex(usize);

/// Represents an index into a component's exports collection.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub(crate) struct ExportIndex(usize);

/// A reference to an import on a component
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub(crate) struct ImportRef {
    /// The index of the component with the import.
    pub(crate) component: ComponentIndex,
    /// The index of the import on the component.
    pub(crate) import: ImportIndex,
}

/// An instance (node) in the instantiation graph.
#[derive(Debug)]
enum Instance {
    /// The instance will be imported in the composed component.
    ///
    /// The value is the set of components that will import the instance.
    Import(IndexSet<ImportRef>),
    /// The instance will be from an instantiation in the composed component.
    Instantiation {
        /// The index of the component being instantiated.
        component: ComponentIndex,
    },
}

/// An instantiation argument (edge) in the instantiation graph.
#[derive(Debug, Copy, Clone)]
struct InstantiationArg {
    /// The import index on the component being instantiated.
    import: ImportIndex,
    /// The export index of the instantiation argument.
    ///
    /// A value of `None` indicates that the instance is itself the argument to use.
    export: Option<ExportIndex>,
}

pub(crate) struct InstantiationGraph {
    /// The parsed components in the graph.
    components: IndexMap<String, Component>,
    /// The actual instantiation graph.
    ///
    /// Each node is an instance and each edge is an argument to the instance.
    instances: Graph<Instance, InstantiationArg>,
    /// Map from instantiation name to instance index.
    names: IndexMap<String, InstanceIndex>,
    /// True if at least one dependency was found and instantiated.
    ///
    /// This is used to determine if no dependencies were found.
    instantiated: bool,
}

impl InstantiationGraph {
    /// Gets the component index for the given instance.
    ///
    /// Returns `None` for imported instances.
    pub(crate) fn component(&self, instance: InstanceIndex) -> Option<&Component> {
        match &self.instances[instance] {
            Instance::Import(_) => None,
            Instance::Instantiation { component } => Some(&self.components[component.0]),
        }
    }

    /// Gets the set of import references for an imported instance.
    ///
    /// Returns `None` for instantiated instances.
    pub(crate) fn import_refs(&self, instance: InstanceIndex) -> Option<&IndexSet<ImportRef>> {
        match &self.instances[instance] {
            Instance::Import(imports) => Some(imports),
            Instance::Instantiation { .. } => None,
        }
    }

    /// Gets the name of the given instance index.
    pub(crate) fn instance_name(&self, instance: InstanceIndex) -> &str {
        self.names
            .get_index(instance.index())
            .map(|(n, _)| n.as_str())
            .expect("invalid instance index")
    }

    /// Gets the topological instantiation order based on the instantiation graph
    pub(crate) fn instantiation_order(&self) -> Result<Vec<InstanceIndex>> {
        toposort(&self.instances, None).map_err(|e| {
            anyhow!(
                "instantiation `{name}` and its dependencies form a cycle in the instantiation graph",
                name = self.names.get_index(e.node_id().index()).unwrap().0
            )
        })
    }

    /// Gets the instantiation arguments for the given instance index.
    ///
    /// The given function is used to resolve an encoded instance index
    /// for a given graph instance index.
    pub(crate) fn instantiation_args<T>(
        &self,
        instance: InstanceIndex,
        mut index_of_instance: T,
    ) -> Vec<(&str, ComponentExportKind, u32)>
    where
        T: FnMut(InstanceIndex, Option<ExportIndex>) -> u32,
    {
        match self.instances[instance] {
            Instance::Import { .. } => Vec::new(),
            Instance::Instantiation { component } => {
                let imports = &self.components[component.0].imports;

                self.instances
                    .edges_directed(instance, EdgeDirection::Incoming)
                    .map(|e| {
                        let arg = e.weight();
                        (
                            imports.get_index(arg.import.0).unwrap().0.as_str(),
                            ComponentExportKind::Instance,
                            index_of_instance(e.source(), arg.export),
                        )
                    })
                    .collect()
            }
        }
    }

    /// Resolves an import reference to its originating component, import name, and instance type.
    pub(crate) fn resolve_import(
        &self,
        r: ImportRef,
    ) -> (&Component, &str, &ComponentInstanceType) {
        let component = &self.components[r.component.0];
        let (name, ty) = component.imports.get_index(r.import.0).unwrap();
        match ty {
            ComponentTypeRef::Instance(index) => (
                component,
                name.as_str(),
                component
                    .types
                    .type_at(*index, false)
                    .unwrap()
                    .as_component_instance_type()
                    .unwrap(),
            ),
            _ => unreachable!("should not have an import ref to a non-instance import"),
        }
    }
}

/// An instance dependency to process in the instantiation graph.
struct Dependency {
    /// The index of the dependent instance.
    dependent: InstanceIndex,
    /// The import reference on the dependent instance.
    import: ImportRef,
    /// The name of the instance from the instantiation argument.
    instance: String,
    /// The name of the export on the instance to use as the instantiation argument.
    export: Option<String>,
}

struct InstantiationGraphBuilder<'a> {
    /// The associated composition configuration.
    config: &'a Config,
    /// The graph being built.
    graph: InstantiationGraph,
}

impl<'a> InstantiationGraphBuilder<'a> {
    fn new(component: &Path, config: &'a Config) -> Result<Self> {
        // The root component is always first in the map
        let mut components = IndexMap::new();
        components.insert(
            ROOT_COMPONENT_NAME.to_string(),
            Component::new(ComponentIndex(0), component, None)?,
        );

        Ok(Self {
            config,
            graph: InstantiationGraph {
                components,
                instances: Default::default(),
                names: Default::default(),
                instantiated: false,
            },
        })
    }

    /// Adds a component of the given name to the graph.
    ///
    /// If a component with the given name already exists, its index is returned.
    fn add_component(&mut self, name: &str) -> Result<Option<ComponentIndex>> {
        if let Some(index) = self.graph.components.get_index_of(name) {
            return Ok(Some(ComponentIndex(index)));
        }

        match self.find_component(ComponentIndex(self.graph.components.len()), name)? {
            Some(component) => {
                let index = component.index;
                assert_eq!(index.0, self.graph.components.len());
                self.graph.components.insert(name.to_string(), component);
                log::debug!(
                    "adding component `{name}` (component index {index})",
                    index = index.0
                );
                Ok(Some(index))
            }
            None => Ok(None),
        }
    }

    /// Finds the component with the given name on disk.
    ///
    /// The given index is the index at which the component would be
    /// inserted into the graph.
    fn find_component(&self, index: ComponentIndex, name: &str) -> Result<Option<Component>> {
        // Check the config for an explicit path (must be a valid component)
        if let Some(dep) = self.config.dependencies.get(name) {
            log::debug!(
                "component with name `{name}` has an explicit path of `{path}`",
                path = dep.path.display()
            );
            return Ok(Some(Component::new(
                index,
                self.config.dir.join(&dep.path),
                dep.import.clone(),
            )?));
        }

        // Otherwise, search the paths for a valid component with the same name
        log::info!("searching for a component with name `{name}`");
        for dir in std::iter::once(&self.config.dir).chain(self.config.search_paths.iter()) {
            if let Some(component) = Self::parse_component(index, dir, name)? {
                return Ok(Some(component));
            }
        }

        Ok(None)
    }

    /// Parses a component from the given directory, if it exists.
    ///
    /// Returns `Ok(None)` if the component does not exist.
    fn parse_component(index: ComponentIndex, dir: &Path, name: &str) -> Result<Option<Component>> {
        let mut path = dir.join(name);

        for ext in ["wasm", "wat"] {
            path.set_extension(ext);
            if !path.is_file() {
                log::info!("component `{path}` does not exist", path = path.display());
                continue;
            }

            return Ok(Some(Component::new(index, &path, None)?));
        }

        Ok(None)
    }

    /// Instantiates an instance with the given name into the graph.
    ///
    /// `import` is expected to be `None` only for the root instantiation.
    fn instantiate(
        &mut self,
        name: &str,
        component_name: &str,
        import: Option<ImportRef>,
    ) -> Result<(InstanceIndex, bool)> {
        if let Some(index) = self.graph.names.get(name) {
            if let Instance::Import(refs) = &mut self.graph.instances[*index] {
                refs.insert(import.unwrap());
            }
            return Ok((*index, true));
        }

        let instance = match self.add_component(component_name)? {
            Some(component) => {
                // If a dependency component was instantiated, mark it in the graph
                if component.0 != 0 {
                    self.graph.instantiated = true;
                }
                Instance::Instantiation { component }
            }
            None => {
                log::warn!("instance `{name}` will be imported because a dependency named `{component_name}` could not be found");
                Instance::Import([import.unwrap()].into())
            }
        };

        let index = self.graph.instances.add_node(instance);
        log::debug!(
            "adding instance `{name}` to the graph (instance index {index})",
            index = index.index()
        );
        assert_eq!(index.index(), self.graph.names.len());
        self.graph.names.insert(name.to_string(), index);
        Ok((index, false))
    }

    /// Finds a compatible instance for the given instance type.
    ///
    /// Returns `Ok(None)` if the given instance itself is compatible.
    /// Returns `Ok(Some(index))` if a compatible instance export from the instance was found.
    /// Returns `Err(_)` if no compatible instance was found.
    fn find_compatible_instance(
        &self,
        instance: InstanceIndex,
        dependent: InstanceIndex,
        arg_name: &str,
        ty: &ComponentInstanceType,
        types: TypesRef,
    ) -> Result<Option<ExportIndex>> {
        match self.graph.component(instance) {
            Some(component) => {
                let instance_name = self.graph.instance_name(instance);
                let dependent_name = self.graph.instance_name(dependent);

                // Check if the instance or one of its exports is compatible with the expected import type
                if component.is_subtype_of(ty, types) {
                    // The instance itself can be used
                    log::debug!(
                        "instance `{instance_name}` can be used for argument `{arg_name}` of instance `{dependent_name}`",
                    );
                    return Ok(None);
                }

                log::debug!(
                    "searching for compatible export from instance `{instance_name}` for argument `{arg_name}` of instance `{dependent_name}`",
                );

                let export = component.find_compatible_export(ty, types) .ok_or_else(|| {
                    anyhow!(
                        "component `{path}` is not compatible with import `{arg_name}` of component `{dependent_path}`",
                        path = component.path.display(),
                        dependent_path = self.graph.component(dependent).unwrap().path.display(),
                    )
                })?;

                log::debug!(
                    "export `{export_name}` (export index {export}) from instance `{instance_name}` can be used for argument `{arg_name}` of instance `{dependent_name}`",
                    export = export.0,
                    export_name = component.exports.get_index(export.0).unwrap().0,
                );

                Ok(Some(export))
            }
            None => {
                // An imported instance should be directly compatible
                Ok(None)
            }
        }
    }

    /// Resolves an explicitly specified export to its index.
    ///
    /// Returns an error if the export is not found or if it is not compatible with the given type.
    fn resolve_export_index(
        &self,
        export: &str,
        instance: InstanceIndex,
        dependent: InstanceIndex,
        arg_name: &str,
        ty: &ComponentInstanceType,
        types: TypesRef,
    ) -> Result<ExportIndex> {
        let instance_name = self.graph.instance_name(instance);

        match self.graph.component(instance) {
            Some(component) => match component.export_instance(export) {
                Some((index, export_ty)) => {
                    if !ComponentInstanceType::is_subtype_of(
                        export_ty,
                        component.types.as_ref(),
                        ty,
                        types,
                    ) {
                        bail!("component `{path}` exports an instance named `{export}` but it is not compatible with import `{arg_name}` of component `{dependent_path}`",
                            path = component.path.display(),
                            dependent_path = self.graph.component(dependent).unwrap().path.display(),
                        )
                    }

                    Ok(index)
                }
                None => bail!("component `{path}` does not export an instance named `{export}`",
                    path = component.path.display(),
                ),
            },
            None => bail!("an explicit export `{export}` cannot be specified for imported instance `{instance_name}`"),
        }
    }

    /// Processes a dependency in the graph.
    fn process_dependency(&mut self, dependency: Dependency) -> Result<(InstanceIndex, bool)> {
        let name = self.config.dependency_name(&dependency.instance);

        log::info!(
            "processing dependency `{name}` from instance `{dependent}` to instance `{instance}`",
            dependent = self
                .graph
                .names
                .get_index(dependency.dependent.index())
                .unwrap()
                .0,
            instance = dependency.instance
        );

        let (instance, existing) =
            self.instantiate(&dependency.instance, name, Some(dependency.import))?;

        let (dependent, import_name, import_type) = self.graph.resolve_import(dependency.import);

        let export = match &dependency.export {
            Some(export) => Some(self.resolve_export_index(
                export,
                instance,
                dependency.dependent,
                import_name,
                import_type,
                dependent.types.as_ref(),
            )?),
            None => self.find_compatible_instance(
                instance,
                dependency.dependent,
                import_name,
                import_type,
                dependent.types.as_ref(),
            )?,
        };

        // Add the edge from this instance to the dependent instance
        self.graph.instances.add_edge(
            instance,
            dependency.dependent,
            InstantiationArg {
                import: dependency.import.import,
                export,
            },
        );

        Ok((instance, existing))
    }

    /// Push dependencies of the given instance to the dependency queue.
    fn push_dependencies(
        &self,
        instance: InstanceIndex,
        queue: &mut VecDeque<Option<Dependency>>,
    ) -> Result<()> {
        match self.graph.instances[instance] {
            Instance::Import { .. } => {
                // Imported instances don't have dependencies
            }
            Instance::Instantiation { component } => {
                let instance_name = self.graph.instance_name(instance);
                let config = self.config.instantiations.get(instance_name);
                let comp = &self.graph.components[component.0];
                let count = queue.len();

                // Push a dependency for every import
                for (import, (name, ty)) in comp.imports.iter().enumerate() {
                    match ty {
                        ComponentTypeRef::Instance(_) => {}
                        _ => bail!(
                            "component `{path}` has a non-instance import named `{name}`",
                            path = comp.path.display()
                        ),
                    }

                    log::debug!("adding dependency for argument `{name}` (import index {import}) from instance `{instance_name}` to the queue");

                    let arg = config.and_then(|c| c.arguments.get(name));
                    queue.push_back(Some(Dependency {
                        dependent: instance,
                        import: ImportRef {
                            component,
                            import: ImportIndex(import),
                        },
                        instance: arg.map(|arg| &arg.instance).unwrap_or(name).clone(),
                        export: arg.and_then(|arg| arg.export.clone()),
                    }));
                }

                // Ensure every explicit argument is a valid import name
                if let Some(config) = config {
                    for arg in config.arguments.keys() {
                        if !comp.imports.contains_key(arg) {
                            bail!(
                                "component `{path}` has no import named `{arg}`",
                                path = comp.path.display()
                            );
                        }
                    }
                }

                // It is an error if the root component has no instance imports
                if count == queue.len() && component.0 == 0 {
                    bail!(
                        "component `{path}` does not import any instances",
                        path = comp.path.display()
                    );
                }
            }
        }

        Ok(())
    }

    /// Build the instantiation graph.
    fn build(mut self) -> Result<InstantiationGraph> {
        let mut queue: VecDeque<Option<Dependency>> = VecDeque::new();
        queue.push_back(None);

        while let Some(dependency) = queue.pop_front() {
            let (instance, existing) = dependency
                .map(|dep| self.process_dependency(dep))
                .unwrap_or_else(|| {
                    self.instantiate(ROOT_COMPONENT_NAME, ROOT_COMPONENT_NAME, None)
                })?;

            // Add dependencies only for new instances in the graph
            if !existing {
                self.push_dependencies(instance, &mut queue)?;
            }
        }

        Ok(self.graph)
    }
}

/// Used to compose a WebAssembly component from other components.
pub struct ComponentComposer<'a> {
    component: &'a Path,
    config: &'a Config,
}

impl<'a> ComponentComposer<'a> {
    /// Constructs a new WebAssembly component composer.
    ///
    /// ## Arguments
    /// * `component` - The path to the component to compose.
    /// * `config` - The configuration to use for the composition.
    pub fn new(component: &'a Path, config: &'a Config) -> Self {
        Self { component, config }
    }

    /// Composes a WebAssembly component based on the composer's configuration.
    ///
    /// ## Returns
    /// Returns the bytes of the composed component.
    pub fn compose(&self) -> Result<Vec<u8>> {
        let graph = InstantiationGraphBuilder::new(self.component, self.config)?.build()?;

        log::debug!(
            "components:\n{components:#?}\ninstantiation graph:\n{graph:?}",
            components = graph.components,
            graph = Dot::new(&graph.instances)
        );

        // If not a single dependency was instantiated, error out
        if !graph.instantiated {
            bail!(
                "no dependencies of component `{path}` were found",
                path = self.component.display()
            );
        }

        InstantiationGraphEncoder::new(self.config, &graph).encode()
    }
}
