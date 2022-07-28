//! Module for encoding of composed WebAssembly components.

use crate::{config::Config, encoding::TypeEncoder};
use anyhow::{anyhow, bail, Context, Result};
use indexmap::{IndexMap, IndexSet};
use petgraph::{algo::toposort, dot::Dot, graph::NodeIndex, visit::EdgeRef, EdgeDirection, Graph};
use std::{
    collections::{hash_map::Entry, HashMap},
    path::{Path, PathBuf},
};
use wasm_encoder::{
    Component, ComponentAliasSection, ComponentExportKind, ComponentExportSection,
    ComponentImportSection, ComponentInstanceSection, ComponentSectionId, ComponentTypeSection,
    RawSection,
};
use wasmparser::{
    types::{ComponentEntityType, ComponentInstanceType, Types, TypesRef},
    Chunk, ComponentExport, ComponentImport, ComponentTypeRef, Encoding, Parser, Payload,
    ValidPayload, Validator, WasmFeatures,
};

struct Import<'a> {
    path: &'a Path,
    bytes: &'a [u8],
    types: Types,
    imports: IndexMap<&'a str, ComponentImport<'a>>,
    exports: IndexMap<&'a str, ComponentExport<'a>>,
}

impl<'a> Import<'a> {
    fn new(path: &'a Path, bytes: &'a [u8]) -> Result<Self> {
        let mut parser = Parser::new(0);
        let mut parsers = Vec::new();
        let mut validator = Validator::new_with_features(WasmFeatures {
            component_model: true,
            ..Default::default()
        });
        let mut imports = IndexMap::new();
        let mut exports = IndexMap::new();

        log::debug!("parsing WebAssembly component `{}`", path.display());

        let mut cur = bytes;
        loop {
            match parser.parse(cur, true)? {
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
                                            "import `{path}` is not a WebAssembly component",
                                            path = path.display()
                                        );
                                    }
                                }
                                Payload::ComponentImportSection(s) => {
                                    for import in s {
                                        let import = import?;
                                        imports.insert(import.name, import);
                                    }
                                }
                                Payload::ComponentExportSection(s) => {
                                    for export in s {
                                        let export = export?;
                                        exports.insert(export.name, export);
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
                                let import = Import {
                                    path,
                                    bytes,
                                    types,
                                    imports,
                                    exports,
                                };
                                log::debug!(
                                    "WebAssembly component `{path}` parsed:\n{import:#?}",
                                    path = path.display()
                                );
                                return Ok(import);
                            }
                        },
                    }
                }
                Chunk::NeedMoreData(_) => unreachable!(),
            }
        }
    }

    fn ty(&self) -> wasm_encoder::ComponentType {
        let encoder = TypeEncoder::new(&self.types);

        encoder.component(
            self.imports
                .iter()
                .map(|(n, i)| (*n, self.types.component_entity_type_from_import(i).unwrap())),
            self.exports
                .iter()
                .map(|(n, e)| (*n, self.types.component_entity_type_from_export(e).unwrap())),
        )
    }

    /// Requires that this import is importable as the given instance type.
    fn require_importable_as(&self, ty: &ComponentInstanceType, types: TypesRef) -> Result<()> {
        let exports = ty.exports(types);

        // This checks if this import's instance type is a subtype of the given instance type.
        for (k, b) in exports {
            match self.exports.get(k.as_str()) {
                Some(a) => {
                    let a = self.types.component_entity_type_from_export(a).unwrap();
                    if !ComponentEntityType::is_subtype_of(&a, self.types.as_ref(), b, types) {
                        bail!("incompatible type for export `{k}`")
                    }
                }
                None => bail!("missing export `{k}`"),
            }
        }

        Ok(())
    }
}

impl std::fmt::Debug for Import<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Import")
            .field("path", &self.path)
            .field("imports", &self.imports)
            .field("exports", &self.exports)
            .finish_non_exhaustive()
    }
}

#[derive(Clone, Copy)]
struct Instantiation<'a> {
    name: &'a str,
    index: usize,
    import: &'a Import<'a>,
}

impl std::fmt::Debug for Instantiation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Instantiation")
            .field("name", &self.name)
            .field("index", &self.index)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, Copy)]
struct InstantiationArg<'a> {
    name: &'a str,
}

type InstantiationGraph<'a> = Graph<Instantiation<'a>, InstantiationArg<'a>>;

#[derive(Default)]
struct Indexes {
    modules: u32,
    funcs: u32,
    values: u32,
    types: u32,
    instances: u32,
    components: u32,
}

#[derive(Default)]
struct State<'a> {
    imports: &'a [Import<'a>],
    graph: InstantiationGraph<'a>,
    /// Map from instantiation name to graph node index.
    nodes: HashMap<&'a str, NodeIndex>,
    /// Map from graph node index to encoded instantiation index.
    instances: HashMap<NodeIndex, u32>,
    /// The set of instantiated imports.
    instantiated: IndexSet<usize>,
    indexes: Indexes,
}

impl<'a> State<'a> {
    // Get the topological instantiation order based on the instantiation graph
    fn instantiation_order(&self) -> Result<Vec<NodeIndex>> {
        toposort(&self.graph, None).map_err(|e| {
            anyhow!(
                "instantiation `{}` and its dependencies form a cycle in the instantiation graph",
                self.graph[e.node_id()].name
            )
        })
    }

    // Get the instantiation arguments for the given node index.
    fn instantiation_args(&self, index: NodeIndex) -> Vec<(&'a str, ComponentExportKind, u32)> {
        self.graph
            .edges_directed(index, EdgeDirection::Incoming)
            .map(|e| {
                (
                    e.weight().name,
                    ComponentExportKind::Instance,
                    *self.instances.get(&e.source()).unwrap(),
                )
            })
            .collect::<Vec<_>>()
    }
}

struct StateBuilder<'a> {
    config: &'a Config,
    state: State<'a>,
}

impl<'a> StateBuilder<'a> {
    fn new(config: &'a Config, imports: &'a [Import<'a>]) -> Self {
        Self {
            config,
            state: State {
                imports,
                ..Default::default()
            },
        }
    }

    fn build(mut self) -> Result<State<'a>> {
        // Add a node to the graph for each explicit instantiation
        for (name, instantiation) in &self.config.instantiations {
            let import = instantiation.import.as_deref().unwrap_or(name);
            let import_index = self.config.imports.get_index_of(import).ok_or_else(|| {
                anyhow!("failed to find an import named `{import}` for instantiation `{name}`")
            })?;

            let node_index = self.state.graph.add_node(Instantiation {
                name,
                index: import_index,
                import: &self.state.imports[import_index],
            });

            log::debug!(
                "inserting explicit instantiation `{name}` (import {import_index}) to graph"
            );

            self.state.nodes.insert(name, node_index);
        }

        // With the nodes added, now add the dependency edges
        // Missing nodes will be explicitly added to the graph if the import can be
        // "default" instantiated (i.e. it has no imports).
        let mut edges = IndexMap::new();
        for (name, instantiation) in &self.config.instantiations {
            edges.clear();

            let index = self.state.nodes[name.as_str()];
            let import = self.state.graph[index].import;

            self.add_explicit_edges(name, import, &instantiation.arguments, &mut edges)?;
            self.add_implicit_edges(name, import, &instantiation.dependencies, &mut edges)?;

            assert_eq!(edges.len(), import.imports.len());

            for (name, (_, dep_index)) in &edges {
                self.state
                    .graph
                    .add_edge(*dep_index, index, InstantiationArg { name });
            }
        }

        // Add the default export to the instantiation graph if it is not already
        if let Some(name) = &self.config.exports.default {
            self.get_or_instantiate(name)
                .with_context(|| "failed to resolve instance reference for default export")?;
        }

        Ok(self.state)
    }

    fn get_or_instantiate(&mut self, name: &'a str) -> Result<NodeIndex> {
        Ok(match self.state.nodes.entry(name) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                // The node isn't in the graph yet
                // Check to see if it can be default instantiated
                let import_index = self
                    .config
                    .imports
                    .get_index_of(name)
                    .ok_or_else(|| anyhow!("an import with name `{name}` does not exist"))?;

                let import = &self.state.imports[import_index];

                if !import.imports.is_empty() {
                    bail!("import `{name}` cannot be implicitly instantiated");
                }

                log::debug!(
                    "inserting implicit instantiation `{name}` (import {import_index}) to graph"
                );

                *e.insert(self.state.graph.add_node(Instantiation {
                    name,
                    index: import_index,
                    import,
                }))
            }
        })
    }

    fn add_explicit_edges(
        &mut self,
        name: &'a str,
        import: &'a Import,
        args: &'a IndexMap<String, String>,
        edges: &mut IndexMap<&'a str, (&'a str, NodeIndex)>,
    ) -> Result<()> {
        for (arg_name, dep_name) in args {
            let arg = import.imports.get(arg_name.as_str()).ok_or_else(|| {
                anyhow!("instantiation `{name}` does not have an argument named `{arg_name}`")
            })?;

            let dep_index = self.get_or_instantiate(dep_name).with_context(|| {
                format!("failed to find an instance named `{dep_name}` specified as argument `{arg_name}` for instantiation `{name}`")
            })?;

            let ty = match arg.ty {
                ComponentTypeRef::Instance(idx) => import
                    .types
                    .type_at(idx, false)
                    .unwrap()
                    .as_component_instance_type()
                    .unwrap(),
                _ => bail!(
                    "import `{path}` has a non-instance import `{name}`",
                    name = arg.name,
                    path = import.path.display()
                ),
            };

            self.state.graph[dep_index]
                .import
                .require_importable_as(ty, import.types.as_ref())
                .with_context(|| format!("instantiation argument `{arg_name}` for instantiation `{name}` is incompatible with dependency `{dep_name}`"))?;

            log::debug!(
                "adding explicit dependency edge `{name}` -> `{dep_name}` (arg `{arg_name}`)"
            );

            // At this point, there should be no edge with this name because there
            // cannot be duplicate names in the argument map.
            assert!(edges.insert(arg_name, (dep_name, dep_index)).is_none());
        }

        Ok(())
    }

    fn add_implicit_edges(
        &mut self,
        name: &'a str,
        import: &'a Import,
        deps: &'a [String],
        edges: &mut IndexMap<&'a str, (&'a str, NodeIndex)>,
    ) -> Result<()> {
        // For the remaining imports that don't already have edges...
        for (n, i) in &import.imports {
            if edges.contains_key(n) {
                continue;
            }

            match i.ty {
                ComponentTypeRef::Instance(idx) => {
                    let ty = import
                        .types
                        .type_at(idx, false)
                        .unwrap()
                        .as_component_instance_type()
                        .unwrap();

                    let mut resolved = false;
                    for dep in deps {
                        let dep_index = self.get_or_instantiate(dep).with_context(|| {
                            format!(
                                "failed to find an instance named `{dep}` specified as a dependency of instantiation `{name}`"
                            )
                        })?;

                        let dep_import = self.state.graph[dep_index].import;

                        match dep_import.require_importable_as(ty, import.types.as_ref()) {
                            Ok(()) => {
                                log::debug!(
                                    "adding implicit dependency edge `{name}` -> `{dep}` (arg `{n}`)"
                                );

                                resolved = true;

                                if let Some(previous) = edges.insert(n, (dep, dep_index)) {
                                    bail!(
                                        "conflicting dependencies for instantiation `{name}`: instantiation argument `{n}` can be satisfied by both `{previous}` and `{dep}`",
                                        previous = previous.0
                                    );
                                }
                            }
                            Err(e) => log::debug!(
                                "instantiation argument `{n}` for instantiation `{name}` cannot be satisfied by `{dep}`: {:?}", e
                            )
                        }
                    }

                    if !resolved {
                        bail!("instantiation argument `{n}` for instantiation `{name}` could not be satisfied by any of the specified dependencies");
                    }
                }
                // For now, only instance imports are supported.
                _ => bail!(
                    "import `{path}` has a non-instance import `{n}`",
                    path = import.path.display()
                ),
            }
        }

        Ok(())
    }
}

/// Used to compose a WebAssembly component from other components.
pub struct ComponentComposer<'a> {
    config: &'a Config,
}

impl<'a> ComponentComposer<'a> {
    /// Constructs a new WebAssembly component composer.
    ///
    /// ## Arguments
    /// * `config` - The configuration to use for the composition.
    pub fn new(config: &'a Config) -> Self {
        Self { config }
    }

    /// Composes a WebAssembly component based on the composer's configuration.
    ///
    /// ## Arguments
    /// * `embed` - Whether to embed all imports in the composed component.
    ///
    /// ## Returns
    /// Returns the bytes of the composed component.
    pub fn compose(&self, embed: bool) -> Result<Vec<u8>> {
        let contents = self.read_import_contents()?;

        let imports = contents
            .iter()
            .map(|(path, bytes)| {
                Import::new(path, bytes)
                    .with_context(|| format!("failed to parse import `{}`", path.display()))
            })
            .collect::<Result<Vec<_>>>()?;

        let mut state = StateBuilder::new(self.config, &imports).build()?;
        log::debug!(
            "calculated instantiation graph:\n{graph:?}",
            graph = Dot::new(&state.graph)
        );

        self.encode(embed, &mut state)
    }

    fn read_import_contents(&self) -> Result<Vec<(PathBuf, Vec<u8>)>> {
        self.config
            .imports
            .iter()
            .map(|(_, import)| {
                let path = self.config.path.join(&import.path);
                let contents = wat::parse_file(&path)?;
                Ok((path, contents))
            })
            .collect()
    }

    fn encode(&self, embed: bool, state: &mut State) -> Result<Vec<u8>> {
        let instances = self.create_instance_section(state)?;

        let mut component = Component::new();
        self.encode_imports(embed, state, &mut component);

        if !instances.is_empty() {
            component.section(&instances);
        }

        self.encode_exports(state, &mut component)?;

        Ok(component.finish())
    }

    fn create_instance_section(&self, state: &mut State) -> Result<ComponentInstanceSection> {
        let ordering = state.instantiation_order()?;
        log::debug!("instantiation order (topological):\n{ordering:#?}");

        let mut instances = ComponentInstanceSection::new();
        for (instance_index, node_index) in ordering.iter().enumerate() {
            let instantiation = &state.graph[*node_index];
            let (component_index, _) = state.instantiated.insert_full(instantiation.index);
            let args = state.instantiation_args(*node_index);

            log::debug!(
                "instantiating import {import_index} (component index {component_index}) as instance {instance_index} with {args:?}",
                import_index = instantiation.index
            );

            instances.instantiate(component_index as u32, args);
            state.instances.insert(*node_index, instance_index as u32);
        }

        if state.instantiated.len() != state.imports.len() {
            for name in (0..state.imports.len())
                .filter(|i| !state.instantiated.contains(&*i))
                .map(|i| self.config.imports.get_index(i).unwrap().0)
            {
                log::warn!(
                    "import `{name}` was not instantiated or referenced from any instantiation"
                );
            }
        }

        Ok(instances)
    }

    fn encode_imports(&self, embed: bool, state: &mut State, component: &mut Component) {
        for import_index in state.instantiated.iter() {
            let (name, config) = self.config.imports.get_index(*import_index).unwrap();
            let import = &state.imports[*import_index];

            if embed || config.embed {
                log::debug!("embedding import `{name}` in composed output",);

                component.section(&RawSection {
                    id: ComponentSectionId::Component.into(),
                    data: import.bytes,
                });

                state.indexes.components += 1;
                continue;
            }

            // TODO: create some sort of canonical import naming scheme?
            let import_name = config.name.as_ref().unwrap_or(name);

            log::debug!("importing `{import_name}` in composed output",);

            let mut types = ComponentTypeSection::new();
            types.component(&import.ty());
            component.section(&types);

            let mut imports = ComponentImportSection::new();
            imports.import(
                import_name,
                wasm_encoder::ComponentTypeRef::Component(state.indexes.types),
            );
            component.section(&imports);

            state.indexes.types += 1;
        }
    }

    fn encode_exports(&self, state: &mut State, component: &mut Component) -> Result<()> {
        let mut exports = ComponentExportSection::new();

        if let Some(name) = &self.config.exports.default {
            // The node should always exist in the graph
            let node_index = state.nodes.get(name.as_str()).unwrap();
            let import = state.graph[*node_index].import;
            let instance_index = state.instances[node_index];

            // Alias all exports from the instance
            let mut aliases = ComponentAliasSection::new();
            for export in import.exports.values() {
                Self::encode_alias_and_export(
                    instance_index,
                    export,
                    state,
                    &mut aliases,
                    &mut exports,
                );
            }

            if !aliases.is_empty() {
                component.section(&aliases);
            }
        }

        if !exports.is_empty() {
            component.section(&exports);
        }

        Ok(())
    }

    fn encode_alias_and_export(
        instance_index: u32,
        export: &ComponentExport,
        state: &mut State,
        aliases: &mut ComponentAliasSection,
        exports: &mut ComponentExportSection,
    ) {
        let (indexes, kind) = match export.kind {
            wasmparser::ComponentExternalKind::Module => {
                (&mut state.indexes.modules, ComponentExportKind::Module)
            }
            wasmparser::ComponentExternalKind::Func => {
                (&mut state.indexes.funcs, ComponentExportKind::Func)
            }
            wasmparser::ComponentExternalKind::Value => {
                (&mut state.indexes.values, ComponentExportKind::Value)
            }
            wasmparser::ComponentExternalKind::Type => {
                (&mut state.indexes.types, ComponentExportKind::Type)
            }
            wasmparser::ComponentExternalKind::Instance => {
                (&mut state.indexes.instances, ComponentExportKind::Instance)
            }
            wasmparser::ComponentExternalKind::Component => (
                &mut state.indexes.components,
                ComponentExportKind::Component,
            ),
        };

        let index = *indexes;
        aliases.instance_export(instance_index, kind, export.name);
        *indexes += 1;
        exports.export(export.name, kind, index);
    }
}
