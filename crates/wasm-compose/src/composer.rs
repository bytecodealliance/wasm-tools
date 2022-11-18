//! Module for composing WebAssembly components.

use crate::{
    config::Config,
    encoding::CompositionGraphEncoder,
    graph::{
        Component, ComponentId, CompositionGraph, EncodeOptions, ExportIndex, ImportIndex,
        InstanceId,
    },
};
use anyhow::{anyhow, bail, Result};
use indexmap::IndexMap;
use std::{collections::VecDeque, path::Path};
use wasmparser::{
    types::{ComponentInstanceType, TypesRef},
    ComponentExternalKind, ComponentTypeRef,
};

/// The root component name used in configuration.
pub const ROOT_COMPONENT_NAME: &str = "$input";

/// A reference to an instance import on a component.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub(crate) struct InstanceImportRef {
    /// The id of the component with the instance import.
    pub(crate) component: ComponentId,
    /// The index of the import on the component.
    pub(crate) import: ImportIndex,
}

/// An instance dependency to process in the composer.
struct Dependency {
    /// The index into `instances` for the dependent instance.
    dependent: usize,
    /// The instance import reference on the dependent instance.
    import: InstanceImportRef,
    /// The name of the instance from the instantiation argument.
    instance: String,
    /// The name of the export on the instance to use as the instantiation argument.
    export: Option<String>,
}

/// A composition graph builder that wires up instances from components
/// resolved from the file system.
struct CompositionGraphBuilder<'a> {
    /// The associated composition configuration.
    config: &'a Config,
    /// The graph being built.
    graph: CompositionGraph<'a>,
    /// A map from instance name to graph instance id.
    instances: IndexMap<String, InstanceId>,
}

impl<'a> CompositionGraphBuilder<'a> {
    fn new(root_path: &Path, config: &'a Config) -> Result<Self> {
        let mut graph = CompositionGraph::new();
        graph.add_component(Component::from_file(ROOT_COMPONENT_NAME, root_path)?)?;

        Ok(Self {
            config,
            graph,
            instances: Default::default(),
        })
    }

    /// Adds a component of the given name to the graph.
    ///
    /// If a component with the given name already exists, its id is returned.
    /// Returns `Ok(None)` if a matching component cannot be found.
    fn add_component(&mut self, name: &str) -> Result<Option<ComponentId>> {
        if let Some((id, _)) = self.graph.get_component_by_name(name) {
            return Ok(Some(id));
        }

        match self.find_component(name)? {
            Some(component) => Ok(Some(self.graph.add_component(component)?)),
            None => Ok(None),
        }
    }

    /// Finds the component with the given name on disk.
    fn find_component(&self, name: &str) -> Result<Option<Component<'a>>> {
        // Check the config for an explicit path (must be a valid component)
        if let Some(dep) = self.config.dependencies.get(name) {
            log::debug!(
                "component with name `{name}` has an explicit path of `{path}`",
                path = dep.path.display()
            );
            return Ok(Some(Component::from_file(
                name,
                self.config.dir.join(&dep.path),
            )?));
        }

        // Otherwise, search the paths for a valid component with the same name
        log::info!("searching for a component with name `{name}`");
        for dir in std::iter::once(&self.config.dir).chain(self.config.search_paths.iter()) {
            if let Some(component) = Self::parse_component(dir, name)? {
                return Ok(Some(component));
            }
        }

        Ok(None)
    }

    /// Parses a component from the given directory, if it exists.
    ///
    /// Returns `Ok(None)` if the component does not exist.
    fn parse_component(dir: &Path, name: &str) -> Result<Option<Component<'a>>> {
        let mut path = dir.join(name);

        for ext in ["wasm", "wat"] {
            path.set_extension(ext);
            if !path.is_file() {
                log::info!("component `{path}` does not exist", path = path.display());
                continue;
            }

            return Ok(Some(Component::from_file(name, &path)?));
        }

        Ok(None)
    }

    /// Instantiates an instance with the given name into the graph.
    ///
    /// Returns an index into `dependencies` for the instance being instantiated.
    ///
    /// Returns `Ok(None)` if a component to instantiate cannot be found.
    fn instantiate(&mut self, name: &str, component_name: &str) -> Result<Option<(usize, bool)>> {
        if let Some(index) = self.instances.get_index_of(name) {
            return Ok(Some((index, true)));
        }

        match self.add_component(component_name)? {
            Some(component_id) => {
                let (index, prev) = self
                    .instances
                    .insert_full(name.to_string(), self.graph.instantiate(component_id)?);
                assert!(prev.is_none());
                Ok(Some((index, false)))
            }
            None => {
                if self.config.disallow_imports {
                    bail!("a dependency named `{component_name}` could not be found and instance imports are not allowed");
                }

                log::warn!("instance `{name}` will be imported because a dependency named `{component_name}` could not be found");
                Ok(None)
            }
        }
    }

    /// Finds a compatible instance for the given instance type.
    ///
    /// Returns `Ok(None)` if the given instance itself is compatible.
    /// Returns `Ok(Some(index))` if a compatible instance export from the instance was found.
    /// Returns `Err(_)` if no compatible instance was found.
    fn find_compatible_instance(
        &self,
        instance: usize,
        dependent: usize,
        arg_name: &str,
        ty: &ComponentInstanceType,
        types: TypesRef,
    ) -> Result<Option<ExportIndex>> {
        let (instance_name, instance_id) = self.instances.get_index(instance).unwrap();
        let (_, component) = self.graph.get_component_of_instance(*instance_id).unwrap();

        let (dependent_name, dependent_instance_id) = self.instances.get_index(dependent).unwrap();

        // Check if the instance or one of its exports is compatible with the expected import type
        if component.is_instance_subtype_of(ty, types) {
            // The instance itself can be used
            log::debug!("instance `{instance_name}` can be used for argument `{arg_name}` of instance `{dependent_name}`");
            return Ok(None);
        }

        log::debug!("searching for compatible export from instance `{instance_name}` for argument `{arg_name}` of instance `{dependent_name}`");

        let export = component.find_compatible_export(ty, types).ok_or_else(|| {
            anyhow!(
                "component `{path}` is not compatible with import `{arg_name}` of component `{dependent_path}`",
                path = component.path().unwrap().display(),
                dependent_path = self.graph.get_component_of_instance(*dependent_instance_id).unwrap().1.path().unwrap().display(),
            )
        })?;

        log::debug!(
            "export `{export_name}` (export index {export}) from instance `{instance_name}` can be used for argument `{arg_name}` of instance `{dependent_name}`",
            export = export.0,
            export_name = component.exports.get_index(export.0).unwrap().0,
        );

        Ok(Some(export))
    }

    /// Resolves an explicitly specified export to its index.
    ///
    /// Returns an error if the export is not found or if it is not compatible with the given type.
    fn resolve_export_index(
        &self,
        export: &str,
        instance: usize,
        dependent_path: &Path,
        arg_name: &str,
        ty: &ComponentInstanceType,
        types: TypesRef,
    ) -> Result<ExportIndex> {
        let (_, instance_id) = self.instances.get_index(instance).unwrap();
        let (_, component) = self.graph.get_component_of_instance(*instance_id).unwrap();
        match component.export_by_name(export) {
            Some((export_index, _, kind, index)) if kind == ComponentExternalKind::Instance => {
                let export_ty = component.types.component_instance_at(index).unwrap();
                if !ComponentInstanceType::is_subtype_of(export_ty, component.types(), ty, types) {
                    bail!("component `{path}` exports an instance named `{export}` but it is not compatible with import `{arg_name}` of component `{dependent_path}`",
                            path = component.path().unwrap().display(),
                            dependent_path = dependent_path.display(),
                        )
                }

                Ok(export_index)
            }
            _ => bail!(
                "component `{path}` does not export an instance named `{export}`",
                path = component.path().unwrap().display(),
            ),
        }
    }

    /// Resolves an import instance reference.
    fn resolve_import_ref(
        &self,
        r: InstanceImportRef,
    ) -> (&Component, &str, &ComponentInstanceType) {
        let component = self.graph.get_component(r.component).unwrap();
        let (name, _, ty) = component.import(r.import).unwrap();
        match ty {
            ComponentTypeRef::Instance(index) => (
                component,
                name,
                component
                    .types
                    .type_at(index, false)
                    .unwrap()
                    .as_component_instance_type()
                    .unwrap(),
            ),
            _ => unreachable!("should not have an instance import ref to a non-instance import"),
        }
    }

    /// Processes a dependency in the graph.
    fn process_dependency(&mut self, dependency: Dependency) -> Result<Option<(usize, bool)>> {
        let name = self.config.dependency_name(&dependency.instance);

        log::info!(
            "processing dependency `{name}` from instance `{dependent_name}` to instance `{instance}`",
            dependent_name = self.instances.get_index(dependency.dependent).unwrap().0,
            instance = dependency.instance
        );

        match self.instantiate(&dependency.instance, name)? {
            Some((instance, existing)) => {
                let (dependent, import_name, import_type) =
                    self.resolve_import_ref(dependency.import);

                let export = match &dependency.export {
                    Some(export) => Some(self.resolve_export_index(
                        export,
                        instance,
                        dependent.path().unwrap(),
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

                // Connect the new instance to the dependent
                self.graph.connect(
                    self.instances[instance],
                    export,
                    self.instances[dependency.dependent],
                    dependency.import.import,
                )?;

                Ok(Some((instance, existing)))
            }
            None => {
                if let Some(export) = &dependency.export {
                    bail!("an explicit export `{export}` cannot be specified for imported instance `{name}`");
                }
                Ok(None)
            }
        }
    }

    /// Push dependencies of the given instance to the dependency queue.
    fn push_dependencies(&self, instance: usize, queue: &mut VecDeque<Dependency>) -> Result<()> {
        let (instance_name, instance_id) = self.instances.get_index(instance).unwrap();
        let config = self.config.instantiations.get(instance_name);
        let (component_id, component) = self.graph.get_component_of_instance(*instance_id).unwrap();
        let count = queue.len();

        // Push a dependency for every instance import
        for (import, name, _, ty) in component.imports() {
            match ty {
                ComponentTypeRef::Instance(_) => {}
                _ => bail!(
                    "component `{path}` has a non-instance import named `{name}`",
                    path = component.path().unwrap().display()
                ),
            }

            log::debug!("adding dependency for argument `{name}` (import index {import}) from instance `{instance_name}` to the queue", import = import.0);

            let arg = config.and_then(|c| c.arguments.get(name));
            queue.push_back(Dependency {
                dependent: instance,
                import: InstanceImportRef {
                    component: component_id,
                    import,
                },
                instance: arg
                    .map(|arg| arg.instance.clone())
                    .unwrap_or_else(|| name.to_string()),
                export: arg.and_then(|arg| arg.export.clone()),
            });
        }

        // Ensure every explicit argument is a valid import name
        if let Some(config) = config {
            for arg in config.arguments.keys() {
                if !component.imports.contains_key(arg) {
                    bail!(
                        "component `{path}` has no import named `{arg}`",
                        path = component.path().unwrap().display()
                    );
                }
            }
        }

        // It is an error if the root component has no instance imports
        if count == queue.len() && instance == 0 {
            bail!(
                "component `{path}` does not import any instances",
                path = component.path().unwrap().display()
            );
        }

        Ok(())
    }

    /// Build the instantiation graph.
    fn build(mut self) -> Result<(InstanceId, CompositionGraph<'a>)> {
        let mut queue: VecDeque<Dependency> = VecDeque::new();

        // Instantiate the root and push its dependencies to the queue
        let (root_instance, existing) = self
            .instantiate(ROOT_COMPONENT_NAME, ROOT_COMPONENT_NAME)?
            .unwrap();

        assert!(!existing);

        self.push_dependencies(0, &mut queue)?;

        // Process all remaining dependencies in the queue
        while let Some(dependency) = queue.pop_front() {
            if let Some((instance, existing)) = self.process_dependency(dependency)? {
                // Add dependencies only for new instances in the graph
                if !existing {
                    self.push_dependencies(instance, &mut queue)?;
                }
            }
        }

        Ok((self.instances[root_instance], self.graph))
    }
}

/// Used to compose a WebAssembly component from other components.
///
/// The component composer resolves the dependencies of a root component
/// from components of matching names in the file system.
///
/// The exports of the root component are then exported from the composed
/// component.
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
        let (root_instance, graph) =
            CompositionGraphBuilder::new(self.component, self.config)?.build()?;

        // If only the root component was instantiated, then there are no resolved dependencies
        if graph.instances.len() == 1 {
            bail!(
                "no dependencies of component `{path}` were found",
                path = self.component.display()
            );
        }

        CompositionGraphEncoder::new(
            EncodeOptions {
                define_components: !self.config.import_components,
                export: Some(root_instance),
                validate: false,
            },
            &graph,
        )
        .encode()
    }
}
