//! Generation of Wasm
//! [components](https://github.com/WebAssembly/component-model).

#![allow(unused_variables, dead_code)] // TODO FITZGEN

use crate::{arbitrary_loop, Config, DefaultConfig};
use arbitrary::{Arbitrary, Result, Unstructured};
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::{
    collections::{HashMap, HashSet},
    marker,
    rc::Rc,
};
use wasm_encoder::{PrimitiveInterfaceType, ValType};

mod encode;

/// A pseudo-random WebAssembly [component].
///
/// Construct instances of this type with [the `Arbitrary`
/// trait](https://docs.rs/arbitrary/*/arbitrary/trait.Arbitrary.html).
///
/// [component]: https://github.com/WebAssembly/component-model/blob/ast-and-binary/design/MVP/Explainer.md
///
/// ## Configured Generated Components
///
/// This uses the [`DefaultConfig`][crate::DefaultConfig] configuration. If you
/// want to customize the shape of generated components, define your own
/// configuration type, implement the [`Config`][crate::Config] trait for it,
/// and use [`ConfiguredComponent<YourConfigType>`][crate::ConfiguredComponent]
/// instead of plain `Component`.
#[derive(Debug)]
pub struct Component {
    sections: Vec<Section>,
}

/// A builder to create a component (and possibly a whole tree of nested
/// components).
///
/// Maintains a stack of components we are currently building, as well as
/// metadata about them. The split between `Component` and `ComponentBuilder` is
/// that the builder contains metadata that is purely used when generating
/// components and is unnecessary after we are done generating the structure of
/// the components and only need to encode an already-generated component to
/// bytes.
#[derive(Debug)]
struct ComponentBuilder {
    config: Rc<dyn Config>,

    // The set of core `valtype`s that we are configured to generate.
    core_valtypes: Vec<ValType>,

    // Stack of types scopes that are currently available.
    //
    // There is an entry in this stack for each component, but there can also be
    // additional entries for module/component/instance types, each of which
    // have their own scope.
    //
    // This stack is always non-empty and the last entry is always the current
    // scope.
    //
    // When a particular scope can alias outer types, it can alias from any
    // scope that is older than it (i.e. `types_scope[i]` can alias from
    // `types_scope[j]` when `j <= i`).
    types: Vec<TypesScope>,

    // The set of components we are currently building and their associated
    // metadata.
    components: Vec<ComponentContext>,

    // Whether we are in the final bits of generating this component and we just
    // need to ensure that the minimum number of entities configured have all
    // been generated. This changes the behavior of various
    // `arbitrary_<section>` methods to always fill in their minimums.
    fill_minimums: bool,

    // Our maximums for these entities are applied across the whole component
    // tree, not per-component.
    total_components: usize,
    total_modules: usize,
    total_instances: usize,
    total_values: usize,
}

#[derive(Debug, Clone)]
enum ComponentOrCoreFuncType {
    Component(Rc<FuncType>),
    Core(Rc<crate::core::FuncType>),
}

impl ComponentOrCoreFuncType {
    fn unwrap_core(self) -> Rc<crate::core::FuncType> {
        match self {
            ComponentOrCoreFuncType::Core(f) => f,
            ComponentOrCoreFuncType::Component(_) => panic!("not a core func type"),
        }
    }
}

#[derive(Debug, Clone)]
enum ComponentOrCoreInstanceType {
    Component(Rc<InstanceType>),
    Core(BTreeMap<String, crate::core::EntityType>),
}

/// Metadata (e.g. contents of various index spaces) we keep track of on a
/// per-component basis.
#[derive(Debug)]
struct ComponentContext {
    // The actual component itself.
    component: Component,

    // The number of imports we have generated thus far.
    num_imports: usize,

    // The set of names of imports we've generated thus far.
    import_names: HashSet<String>,

    // This component's function index space.
    funcs: Vec<ComponentOrCoreFuncType>,

    // Which entries in `funcs` are interface functions?
    interface_funcs: Vec<u32>,

    // Which entries in `funcs` are interface functions that only use scalar
    // interface types?
    scalar_interface_funcs: Vec<u32>,

    // Which entries in `funcs` are core Wasm functions?
    //
    // Note that a component can't import core functions, so these entries will
    // never point to a `Section::Import`.
    core_funcs: Vec<u32>,

    // This component's component index space.
    //
    // An indirect list of all directly-nested (not transitive) components
    // inside this component.
    //
    // Each entry is of the form `(i, j)` where `component.sections[i]` is
    // guaranteed to be either
    //
    // * a `Section::Component` and we are referencing the component defined in
    //   that section (in this case `j` must also be `0`, since a component
    //   section can only contain a single nested component), or
    //
    // * a `Section::Import` and we are referenceing the `j`th import in that
    //   section, which is guaranteed to be a component import.
    components: Vec<(usize, usize)>,

    // This component's module index space.
    //
    // An indirect list of all directly-nested (not transitive) modules
    // inside this component.
    //
    // Each entry is of the form `(i, j)` where `component.sections[i]` is
    // guaranteed to be either
    //
    // * a `Section::Core` and we are referencing the module defined in that
    //   section (in this case `j` must also be `0`, since a core section can
    //   only contain a single nested module), or
    //
    // * a `Section::Import` and we are referenceing the `j`th import in that
    //   section, which is guaranteed to be a module import.
    modules: Vec<(usize, usize)>,

    // This component's instance index space.
    instances: Vec<ComponentOrCoreInstanceType>,

    // This component's value index space.
    values: Vec<ValueType>,
}

impl ComponentContext {
    fn empty() -> Self {
        ComponentContext {
            component: Component::empty(),
            num_imports: 0,
            import_names: HashSet::default(),
            funcs: vec![],
            interface_funcs: vec![],
            scalar_interface_funcs: vec![],
            core_funcs: vec![],
            components: vec![],
            modules: vec![],
            instances: vec![],
            values: vec![],
        }
    }

    fn num_modules(&self) -> usize {
        self.modules.len()
    }

    fn num_components(&self) -> usize {
        self.components.len()
    }

    fn num_instances(&self) -> usize {
        self.instances.len()
    }

    fn num_funcs(&self) -> usize {
        self.funcs.len()
    }

    fn num_values(&self) -> usize {
        self.values.len()
    }
}

#[derive(Debug, Default)]
struct TypesScope {
    // All types in this index space, regardless of kind.
    types: Vec<Rc<Type>>,

    // The indices of all the entries in `types` that describe things that can
    // be imported or exported at instantiation time.
    def_types: Vec<u32>,

    // The indices of all the entries in `types` that are module types.
    module_types: Vec<u32>,

    // The indices of all the entries in `types` that are component types.
    component_types: Vec<u32>,

    // The indices of all the entries in `types` that are instance types.
    instance_types: Vec<u32>,

    // The indices of all the entries in `types` that are func types.
    func_types: Vec<u32>,

    // A map from function types to their indices in the types space.
    func_type_to_indices: HashMap<Rc<FuncType>, Vec<u32>>,

    // The indices of all the entries in `types` that are value types.
    value_types: Vec<u32>,

    // The indices of all the entries in `types` that are interface types.
    interface_types: Vec<u32>,
}

impl TypesScope {
    fn push(&mut self, ty: Rc<Type>) -> u32 {
        let ty_idx = u32::try_from(self.types.len()).unwrap();

        let (is_def_type, kind_list) = match &*ty {
            Type::Module(_) => (true, &mut self.module_types),
            Type::Component(_) => (true, &mut self.component_types),
            Type::Instance(_) => (true, &mut self.instance_types),
            Type::Func(func_ty) => {
                self.func_type_to_indices
                    .entry(func_ty.clone())
                    .or_default()
                    .push(ty_idx);
                (true, &mut self.func_types)
            }
            Type::Value(_) => (true, &mut self.value_types),
            Type::Interface(_) => (false, &mut self.interface_types),
        };
        kind_list.push(ty_idx);
        if is_def_type {
            self.def_types.push(ty_idx);
        }

        self.types.push(ty);
        ty_idx
    }

    fn get(&self, index: u32) -> &Rc<Type> {
        &self.types[index as usize]
    }

    fn get_func(&self, index: u32) -> &Rc<FuncType> {
        match &**self.get(index) {
            Type::Func(f) => f,
            _ => panic!("get_func on non-function type"),
        }
    }
}

impl<'a> Arbitrary<'a> for Component {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ConfiguredComponent::<DefaultConfig>::arbitrary(u)?.component)
    }
}

/// A pseudo-random generated Wasm component with custom configuration.
///
/// If you don't care about custom configuration, use
/// [`Component`][crate::Component] instead.
///
/// For details on configuring, see the [`Config`][crate::Config] trait.
#[derive(Debug)]
pub struct ConfiguredComponent<C> {
    /// The generated component, controlled by the configuration of `C` in the
    /// `Arbitrary` implementation.
    pub component: Component,
    _marker: marker::PhantomData<C>,
}

impl<'a, C> Arbitrary<'a> for ConfiguredComponent<C>
where
    C: Config + Arbitrary<'a>,
{
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let config = C::arbitrary(u)?;
        let component = Component::new(config, u)?;
        Ok(ConfiguredComponent {
            component,
            _marker: marker::PhantomData,
        })
    }
}

#[derive(Default)]
struct EntityCounts {
    globals: usize,
    tables: usize,
    memories: usize,
    tags: usize,
    funcs: usize,
}

impl Component {
    /// Construct a new `Component` using the given configuration.
    pub fn new(config: impl Config, u: &mut Unstructured) -> Result<Self> {
        let mut builder = ComponentBuilder::new(Rc::new(config));
        Ok(builder.build(u)?)
    }

    fn empty() -> Self {
        Component { sections: vec![] }
    }
}

#[must_use]
enum Step {
    Finished(Component),
    StillBuilding,
}

impl Step {
    fn unwrap_still_building(self) {
        match self {
            Step::Finished(_) => panic!(
                "`Step::unwrap_still_building` called on a `Step` that is not `StillBuilding`"
            ),
            Step::StillBuilding => {}
        }
    }
}

impl ComponentBuilder {
    fn new(config: Rc<dyn Config>) -> Self {
        ComponentBuilder {
            config,
            core_valtypes: vec![],
            types: vec![Default::default()],
            components: vec![ComponentContext::empty()],
            fill_minimums: false,
            total_components: 0,
            total_modules: 0,
            total_instances: 0,
            total_values: 0,
        }
    }

    fn build(&mut self, u: &mut Unstructured) -> Result<Component> {
        self.core_valtypes = crate::core::configured_valtypes(&*self.config);

        let mut choices: Vec<fn(&mut ComponentBuilder, &mut Unstructured) -> Result<Step>> = vec![];

        loop {
            choices.clear();
            choices.push(Self::finish_component);

            // Only add any choice other than "finish what we've generated thus
            // far" when there is more arbitrary fuzzer data for us to consume.
            if u.len() > 0 {
                choices.push(Self::arbitrary_custom_section);

                // NB: we add each section as a choice even if we've already
                // generated our maximum number of entities in that section so that
                // we can exercise adding empty sections to the end of the module.
                choices.push(Self::arbitrary_type_section);
                choices.push(Self::arbitrary_import_section);
                choices.push(Self::arbitrary_func_section);

                if self.total_modules < self.config.max_modules() {
                    choices.push(Self::arbitrary_core_section);
                }

                if self.components.len() < self.config.max_nesting_depth()
                    && self.total_components < self.config.max_components()
                {
                    choices.push(Self::arbitrary_component_section);
                }

                // TODO FITZGEN
                //
                // choices.push(Self::arbitrary_instance_section);
                // choices.push(Self::arbitrary_export_section);
                // choices.push(Self::arbitrary_start_section);
                // choices.push(Self::arbitrary_alias_section);
            }

            let f = u.choose(&choices)?;
            match f(self, u)? {
                Step::StillBuilding => {}
                Step::Finished(component) => {
                    if self.components.is_empty() {
                        // If we just finished the root component, then return it.
                        return Ok(component);
                    } else {
                        // Otherwise, add it as a nested component in the parent.
                        self.push_section(Section::Component(component));
                    }
                }
            }
        }
    }

    fn finish_component(&mut self, u: &mut Unstructured) -> Result<Step> {
        // Ensure we've generated all of our minimums.
        self.fill_minimums = true;
        {
            if self.current_type_scope().types.len() < self.config.min_types() {
                self.arbitrary_type_section(u)?.unwrap_still_building();
            }
            if self.component().num_imports < self.config.min_imports() {
                self.arbitrary_import_section(u)?.unwrap_still_building();
            }
            if self.component().funcs.len() < self.config.min_funcs() {
                self.arbitrary_func_section(u)?.unwrap_still_building();
            }
        }
        self.fill_minimums = false;

        self.types
            .pop()
            .expect("should have a types scope for the component we are finishing");
        Ok(Step::Finished(self.components.pop().unwrap().component))
    }

    fn config(&self) -> &dyn Config {
        &*self.config
    }

    fn component(&self) -> &ComponentContext {
        self.components.last().unwrap()
    }

    fn component_mut(&mut self) -> &mut ComponentContext {
        self.components.last_mut().unwrap()
    }

    fn last_section(&self) -> Option<&Section> {
        self.component().component.sections.last()
    }

    fn last_section_mut(&mut self) -> Option<&mut Section> {
        self.component_mut().component.sections.last_mut()
    }

    fn push_section(&mut self, section: Section) {
        self.component_mut().component.sections.push(section);
    }

    fn ensure_section(
        &mut self,
        mut predicate: impl FnMut(&Section) -> bool,
        mut make_section: impl FnMut() -> Section,
    ) -> &mut Section {
        match self.last_section() {
            Some(sec) if predicate(sec) => {}
            _ => self.push_section(make_section()),
        }
        self.last_section_mut().unwrap()
    }

    fn arbitrary_custom_section(&mut self, u: &mut Unstructured) -> Result<Step> {
        self.push_section(Section::Custom(u.arbitrary()?));
        Ok(Step::StillBuilding)
    }

    fn push_type(&mut self, ty: Rc<Type>) -> u32 {
        match self.ensure_section(
            |s| matches!(s, Section::Type(_)),
            || Section::Type(TypeSection { types: vec![] }),
        ) {
            Section::Type(TypeSection { types }) => {
                types.push(ty.clone());
                self.current_type_scope_mut().push(ty)
            }
            _ => unreachable!(),
        }
    }

    fn arbitrary_type_section(&mut self, u: &mut Unstructured) -> Result<Step> {
        self.push_section(Section::Type(TypeSection { types: vec![] }));

        let min = if self.fill_minimums {
            self.config
                .min_types()
                .saturating_sub(self.current_type_scope().types.len())
        } else {
            0
        };

        let max = self.config.max_types() - self.current_type_scope().types.len();

        arbitrary_loop(u, min, max, |u| {
            let mut type_fuel = self.config.max_type_size();
            let ty = self.arbitrary_type(u, &mut type_fuel)?;
            self.push_type(ty);
            Ok(true)
        })?;

        Ok(Step::StillBuilding)
    }

    fn arbitrary_type(&mut self, u: &mut Unstructured, type_fuel: &mut u32) -> Result<Rc<Type>> {
        *type_fuel = type_fuel.saturating_sub(1);
        if *type_fuel == 0 {
            return Ok(Rc::new(Type::Value(self.arbitrary_value_type(u)?)));
        }

        let ty = match u.int_in_range::<u8>(0..=5)? {
            0 => Type::Module(self.arbitrary_module_type(u, type_fuel)?),
            1 => Type::Component(self.arbitrary_component_type(u, type_fuel)?),
            2 => Type::Instance(self.arbitrary_instance_type(u, type_fuel)?),
            3 => Type::Func(self.arbitrary_func_type(u, type_fuel)?),
            4 => Type::Value(self.arbitrary_value_type(u)?),
            5 => Type::Interface(self.arbitrary_interface_type(u, type_fuel)?),
            _ => unreachable!(),
        };
        Ok(Rc::new(ty))
    }

    fn arbitrary_module_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<Rc<ModuleType>> {
        let mut defs = vec![];
        let mut has_memory = false;
        let mut has_canonical_abi_realloc = false;
        let mut has_canonical_abi_free = false;
        let mut types: Vec<Rc<crate::core::FuncType>> = vec![];
        let mut imports = HashMap::new();
        let mut exports = HashSet::new();
        let mut counts = EntityCounts::default();

        // Special case the canonical ABI functions since certain types can only
        // be passed across the interface types boundary if they exist and
        // randomly generating them is extremely unlikely.

        // `memory`
        if counts.memories < self.config.max_memories() && u.ratio::<u8>(99, 100)? {
            defs.push(ModuleTypeDef::Export(
                "memory".into(),
                crate::core::EntityType::Memory(self.arbitrary_core_memory_type(u)?),
            ));
            counts.memories += 1;
            has_memory = true;
        }

        // `canonical_abi_realloc`
        if counts.funcs < self.config.max_funcs()
            && types.len() < self.config.max_types()
            && u.ratio::<u8>(99, 100)?
        {
            let realloc_ty = Rc::new(crate::core::FuncType {
                params: vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
                results: vec![ValType::I32],
            });
            let ty_idx = u32::try_from(types.len()).unwrap();
            types.push(realloc_ty.clone());
            defs.push(ModuleTypeDef::TypeDef(crate::core::Type::Func(
                realloc_ty.clone(),
            )));
            defs.push(ModuleTypeDef::Export(
                "canonical_abi_realloc".into(),
                crate::core::EntityType::Func(ty_idx, realloc_ty),
            ));
            exports.insert("canonical_abi_realloc".into());
            counts.funcs += 1;
            has_canonical_abi_realloc = true;
        }

        // `canonical_abi_free`
        if counts.funcs < self.config.max_funcs()
            && types.len() < self.config.max_types()
            && u.ratio::<u8>(99, 100)?
        {
            let free_ty = Rc::new(crate::core::FuncType {
                params: vec![ValType::I32, ValType::I32, ValType::I32],
                results: vec![],
            });
            let ty_idx = u32::try_from(types.len()).unwrap();
            types.push(free_ty.clone());
            defs.push(ModuleTypeDef::TypeDef(crate::core::Type::Func(
                free_ty.clone(),
            )));
            defs.push(ModuleTypeDef::Export(
                "canonical_abi_free".into(),
                crate::core::EntityType::Func(ty_idx, free_ty),
            ));
            exports.insert("canonical_abi_free".into());
            counts.funcs += 1;
            has_canonical_abi_free = true;
        }

        let mut entity_choices: Vec<
            fn(
                &mut ComponentBuilder,
                &mut Unstructured,
                &mut EntityCounts,
                &[Rc<crate::core::FuncType>],
            ) -> Result<crate::core::EntityType>,
        > = Vec::with_capacity(5);

        arbitrary_loop(u, 0, 100, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            let max_choice = if types.len() < self.config.max_types() {
                2
            } else {
                1
            };

            match u.int_in_range::<u8>(0..=max_choice)? {
                // Import.
                0 => {
                    let module = crate::limited_string(100, u)?;
                    let existing_module_imports = imports.entry(module.clone()).or_default();
                    let field = crate::unique_string(100, existing_module_imports, u)?;
                    let entity_type = match self.arbitrary_core_entity_type(
                        u,
                        &types,
                        &mut entity_choices,
                        &mut counts,
                    )? {
                        None => return Ok(false),
                        Some(x) => x,
                    };
                    defs.push(ModuleTypeDef::Import(crate::core::Import {
                        module: module.into(),
                        field: field.into(),
                        entity_type,
                    }));
                }

                // Export.
                1 => {
                    let name = crate::unique_string(100, &mut exports, u)?;
                    let entity_ty = match self.arbitrary_core_entity_type(
                        u,
                        &types,
                        &mut entity_choices,
                        &mut counts,
                    )? {
                        None => return Ok(false),
                        Some(x) => x,
                    };
                    defs.push(ModuleTypeDef::Export(name, entity_ty));
                }

                // Type definition.
                _ => {
                    let ty = crate::core::arbitrary_func_type(
                        u,
                        &self.core_valtypes,
                        if self.config.multi_value_enabled() {
                            None
                        } else {
                            Some(1)
                        },
                    )?;
                    types.push(ty.clone());
                    defs.push(ModuleTypeDef::TypeDef(crate::core::Type::Func(ty)));
                }
            }

            Ok(true)
        })?;

        Ok(Rc::new(ModuleType {
            defs,
            has_memory,
            has_canonical_abi_realloc,
            has_canonical_abi_free,
        }))
    }

    fn arbitrary_core_entity_type(
        &mut self,
        u: &mut Unstructured,
        types: &[Rc<crate::core::FuncType>],
        choices: &mut Vec<
            fn(
                &mut ComponentBuilder,
                &mut Unstructured,
                &mut EntityCounts,
                &[Rc<crate::core::FuncType>],
            ) -> Result<crate::core::EntityType>,
        >,
        counts: &mut EntityCounts,
    ) -> Result<Option<crate::core::EntityType>> {
        choices.clear();

        if counts.globals < self.config.max_globals() {
            choices.push(|c, u, counts, _types| {
                counts.globals += 1;
                Ok(crate::core::EntityType::Global(
                    c.arbitrary_core_global_type(u)?,
                ))
            });
        }

        if counts.tables < self.config.max_tables() {
            choices.push(|c, u, counts, _types| {
                counts.tables += 1;
                Ok(crate::core::EntityType::Table(
                    c.arbitrary_core_table_type(u)?,
                ))
            });
        }

        if counts.memories < self.config.max_memories() {
            choices.push(|c, u, counts, _types| {
                counts.memories += 1;
                Ok(crate::core::EntityType::Memory(
                    c.arbitrary_core_memory_type(u)?,
                ))
            });
        }

        if types.iter().any(|ty| ty.results.is_empty())
            && self.config.exceptions_enabled()
            && counts.tags < self.config.max_tags()
        {
            choices.push(|c, u, counts, types| {
                counts.tags += 1;
                let tag_func_types = types
                    .iter()
                    .enumerate()
                    .filter(|(_, ty)| ty.results.is_empty())
                    .map(|(i, _)| u32::try_from(i).unwrap())
                    .collect::<Vec<_>>();
                Ok(crate::core::EntityType::Tag(
                    crate::core::arbitrary_tag_type(u, &tag_func_types, |idx| {
                        types[usize::try_from(idx).unwrap()].clone()
                    })?,
                ))
            });
        }

        if !types.is_empty() && counts.funcs < self.config.max_funcs() {
            choices.push(|c, u, counts, types| {
                counts.funcs += 1;
                let ty_idx = u.int_in_range(0..=u32::try_from(types.len() - 1).unwrap())?;
                let ty = types[ty_idx as usize].clone();
                Ok(crate::core::EntityType::Func(ty_idx, ty))
            });
        }

        if choices.is_empty() {
            return Ok(None);
        }

        let f = u.choose(&choices)?;
        let ty = f(self, u, counts, types)?;
        Ok(Some(ty))
    }

    fn arbitrary_core_valtype(&self, u: &mut Unstructured) -> Result<ValType> {
        Ok(*u.choose(&self.core_valtypes)?)
    }

    fn arbitrary_core_global_type(
        &mut self,
        u: &mut Unstructured,
    ) -> Result<crate::core::GlobalType> {
        Ok(crate::core::GlobalType {
            val_type: self.arbitrary_core_valtype(u)?,
            mutable: u.arbitrary()?,
        })
    }

    fn arbitrary_core_table_type(
        &mut self,
        u: &mut Unstructured,
    ) -> Result<crate::core::TableType> {
        crate::core::arbitrary_table_type(u, self.config())
    }

    fn arbitrary_core_memory_type(
        &mut self,
        u: &mut Unstructured,
    ) -> Result<crate::core::MemoryType> {
        crate::core::arbitrary_memtype(u, self.config())
    }

    fn with_types_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        self.types.push(Default::default());
        let result = f(self);
        self.types.pop();
        result
    }

    fn current_type_scope(&self) -> &TypesScope {
        self.types.last().unwrap()
    }

    fn current_type_scope_mut(&mut self) -> &mut TypesScope {
        self.types.last_mut().unwrap()
    }

    fn outer_types_scope(&self, count: u32) -> &TypesScope {
        &self.types[self.types.len() - 1 - usize::try_from(count).unwrap()]
    }

    fn outer_type(&self, count: u32, i: u32) -> &Rc<Type> {
        &self.outer_types_scope(count).types[usize::try_from(i).unwrap()]
    }

    fn arbitrary_component_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<Rc<ComponentType>> {
        let mut defs = vec![];
        let mut imports = HashSet::new();
        let mut exports = HashSet::new();

        self.with_types_scope(|me| {
            arbitrary_loop(u, 0, 100, |u| {
                *type_fuel = type_fuel.saturating_sub(1);
                if *type_fuel == 0 {
                    return Ok(false);
                }

                if !me.current_type_scope().def_types.is_empty()
                    && u.int_in_range::<u8>(0..=3)? == 0
                {
                    // Imports.
                    let name = crate::unique_string(100, &mut imports, u)?;
                    let max_def_ty_idx = me.current_type_scope().def_types.len() - 1;
                    let def_ty_idx = u.int_in_range(0..=max_def_ty_idx)?;
                    let index = me.current_type_scope().def_types[def_ty_idx];
                    let index = u32::try_from(index).unwrap();
                    let ty = Rc::clone(me.current_type_scope().get(index));
                    defs.push(ComponentTypeDef::Import(Import {
                        name,
                        ty: TypeIndex { index, ty },
                    }));
                } else {
                    // Type definitions, exports, and aliases.
                    let def = me.arbitrary_instance_type_def(u, &mut exports, type_fuel)?;
                    defs.push(def.into());
                }
                Ok(true)
            })
        })?;

        Ok(Rc::new(ComponentType { defs }))
    }

    fn arbitrary_instance_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<Rc<InstanceType>> {
        let mut defs = vec![];
        let mut exports = HashSet::new();

        self.with_types_scope(|me| {
            arbitrary_loop(u, 0, 100, |u| {
                *type_fuel = type_fuel.saturating_sub(1);
                if *type_fuel == 0 {
                    return Ok(false);
                }

                defs.push(me.arbitrary_instance_type_def(u, &mut exports, type_fuel)?);
                Ok(true)
            })
        })?;

        Ok(Rc::new(InstanceType { defs }))
    }

    fn arbitrary_instance_type_def(
        &mut self,
        u: &mut Unstructured,
        exports: &mut HashSet<String>,
        type_fuel: &mut u32,
    ) -> Result<InstanceTypeDef> {
        let mut choices: Vec<
            fn(
                &mut ComponentBuilder,
                &mut HashSet<String>,
                &mut Unstructured,
                &mut u32,
            ) -> Result<InstanceTypeDef>,
        > = Vec::with_capacity(3);

        // Export.
        if !self.current_type_scope().types.is_empty() {
            choices.push(|me, exports, u, _type_fuel| {
                let index = u.int_in_range(
                    0..=u32::try_from(me.current_type_scope().types.len()).unwrap() - 1,
                )?;
                let ty = Rc::clone(me.current_type_scope().get(index));
                Ok(InstanceTypeDef::Export {
                    name: crate::unique_string(100, exports, u)?,
                    ty: TypeIndex { index, ty },
                })
            });
        }

        // Outer type alias.
        if self.types.iter().any(|scope| !scope.types.is_empty()) {
            choices.push(|me, _exports, u, _type_fuel| {
                let alias = me.arbitrary_outer_type_alias(u)?;
                let (count, i) = match alias {
                    Alias::Outer {
                        count,
                        i,
                        kind: OuterAliasKind::Type,
                    } => (count, i),
                    _ => unreachable!(),
                };
                let ty = me.outer_type(count, i).clone();
                me.current_type_scope_mut().push(ty);
                Ok(InstanceTypeDef::Alias(alias))
            });
        }

        // Type definition.
        choices.push(|me, _exports, u, type_fuel| {
            let ty = me.arbitrary_type(u, type_fuel)?;
            me.current_type_scope_mut().push(ty.clone());
            Ok(InstanceTypeDef::Type(ty))
        });

        let f = u.choose(&choices)?;
        f(self, exports, u, type_fuel)
    }

    fn arbitrary_outer_type_alias(&mut self, u: &mut Unstructured) -> Result<Alias> {
        let non_empty_types_scopes: Vec<_> = self
            .types
            .iter()
            .rev()
            .enumerate()
            .filter(|(_, scope)| !scope.types.is_empty())
            .collect();
        assert!(
            !non_empty_types_scopes.is_empty(),
            "precondition: there are non-empty types scopes"
        );

        let (count, scope) = u.choose(&non_empty_types_scopes)?;
        let count = u32::try_from(*count).unwrap();
        assert!(!scope.types.is_empty());

        let max_type_in_scope = u32::try_from(scope.types.len() - 1).unwrap();
        let i = u.int_in_range(0..=max_type_in_scope)?;

        Ok(Alias::Outer {
            count,
            i,
            kind: OuterAliasKind::Type,
        })
    }

    fn arbitrary_func_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<Rc<FuncType>> {
        let mut params = vec![];
        let mut param_names = HashSet::new();
        arbitrary_loop(u, 0, 20, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            params.push(self.arbitrary_optional_named_type(u, &mut param_names)?);
            Ok(true)
        })?;

        let result = self.arbitrary_interface_type_ref(u)?;

        Ok(Rc::new(FuncType { params, result }))
    }

    fn arbitrary_value_type(&mut self, u: &mut Unstructured) -> Result<ValueType> {
        Ok(ValueType(self.arbitrary_interface_type_ref(u)?))
    }

    fn arbitrary_interface_type_ref(&mut self, u: &mut Unstructured) -> Result<InterfaceTypeRef> {
        let max_choices = if self.current_type_scope().interface_types.is_empty() {
            0
        } else {
            1
        };
        match u.int_in_range(0..=max_choices)? {
            0 => Ok(InterfaceTypeRef::Primitive(
                self.arbitrary_primitive_interface_type(u)?,
            )),
            1 => {
                let index = *u.choose(&self.current_type_scope().interface_types)?;
                let ty = Rc::clone(self.current_type_scope().get(index));
                Ok(InterfaceTypeRef::Type(TypeIndex { index, ty }))
            }
            _ => unreachable!(),
        }
    }

    fn arbitrary_primitive_interface_type(
        &mut self,
        u: &mut Unstructured,
    ) -> Result<PrimitiveInterfaceType> {
        match u.int_in_range(0..=13)? {
            0 => Ok(PrimitiveInterfaceType::Unit),
            1 => Ok(PrimitiveInterfaceType::Bool),
            2 => Ok(PrimitiveInterfaceType::S8),
            3 => Ok(PrimitiveInterfaceType::U8),
            4 => Ok(PrimitiveInterfaceType::S16),
            5 => Ok(PrimitiveInterfaceType::U16),
            6 => Ok(PrimitiveInterfaceType::S32),
            7 => Ok(PrimitiveInterfaceType::U32),
            8 => Ok(PrimitiveInterfaceType::S64),
            9 => Ok(PrimitiveInterfaceType::U64),
            10 => Ok(PrimitiveInterfaceType::Float32),
            11 => Ok(PrimitiveInterfaceType::Float64),
            12 => Ok(PrimitiveInterfaceType::Char),
            13 => Ok(PrimitiveInterfaceType::String),
            _ => unreachable!(),
        }
    }

    fn arbitrary_named_type(
        &mut self,
        u: &mut Unstructured,
        names: &mut HashSet<String>,
    ) -> Result<NamedType> {
        let name = crate::unique_non_empty_string(100, names, u)?;
        let ty = self.arbitrary_interface_type_ref(u)?;
        Ok(NamedType { name, ty })
    }

    fn arbitrary_optional_named_type(
        &mut self,
        u: &mut Unstructured,
        names: &mut HashSet<String>,
    ) -> Result<OptionalNamedType> {
        let name = if u.arbitrary()? {
            Some(crate::unique_non_empty_string(100, names, u)?)
        } else {
            None
        };
        let ty = self.arbitrary_interface_type_ref(u)?;
        Ok(OptionalNamedType { name, ty })
    }

    fn arbitrary_record_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<RecordType> {
        let mut fields = vec![];
        let mut field_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            fields.push(self.arbitrary_named_type(u, &mut field_names)?);
            Ok(true)
        })?;
        Ok(RecordType { fields })
    }

    fn arbitrary_variant_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<VariantType> {
        let mut cases = vec![];
        let mut case_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            let default = if !cases.is_empty() && u.arbitrary()? {
                let max_cases = u32::try_from(cases.len() - 1).unwrap();
                Some(u.int_in_range(0..=max_cases)?)
            } else {
                None
            };

            cases.push((self.arbitrary_named_type(u, &mut case_names)?, default));
            Ok(true)
        })?;

        Ok(VariantType { cases })
    }

    fn arbitrary_list_type(&mut self, u: &mut Unstructured) -> Result<ListType> {
        Ok(ListType {
            elem_ty: self.arbitrary_interface_type_ref(u)?,
        })
    }

    fn arbitrary_tuple_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<TupleType> {
        let mut fields = vec![];
        arbitrary_loop(u, 0, 100, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            fields.push(self.arbitrary_interface_type_ref(u)?);
            Ok(true)
        })?;
        Ok(TupleType { fields })
    }

    fn arbitrary_flags_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<FlagsType> {
        let mut fields = vec![];
        let mut field_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            fields.push(crate::unique_non_empty_string(100, &mut field_names, u)?);
            Ok(true)
        })?;
        Ok(FlagsType { fields })
    }

    fn arbitrary_enum_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<EnumType> {
        let mut variants = vec![];
        let mut variant_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            variants.push(crate::unique_non_empty_string(100, &mut variant_names, u)?);
            Ok(true)
        })?;
        Ok(EnumType { variants })
    }

    fn arbitrary_union_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<UnionType> {
        let mut variants = vec![];
        arbitrary_loop(u, 0, 100, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            variants.push(self.arbitrary_interface_type_ref(u)?);
            Ok(true)
        })?;
        Ok(UnionType { variants })
    }

    fn arbitrary_option_type(&mut self, u: &mut Unstructured) -> Result<OptionType> {
        Ok(OptionType {
            inner_ty: self.arbitrary_interface_type_ref(u)?,
        })
    }

    fn arbitrary_expected_type(&mut self, u: &mut Unstructured) -> Result<ExpectedType> {
        Ok(ExpectedType {
            ok_ty: self.arbitrary_interface_type_ref(u)?,
            err_ty: self.arbitrary_interface_type_ref(u)?,
        })
    }

    fn arbitrary_interface_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<InterfaceType> {
        match u.int_in_range(0..=9)? {
            0 => Ok(InterfaceType::Primitive(
                self.arbitrary_primitive_interface_type(u)?,
            )),
            1 => Ok(InterfaceType::Record(
                self.arbitrary_record_type(u, type_fuel)?,
            )),
            2 => Ok(InterfaceType::Variant(
                self.arbitrary_variant_type(u, type_fuel)?,
            )),
            3 => Ok(InterfaceType::List(self.arbitrary_list_type(u)?)),
            4 => Ok(InterfaceType::Tuple(
                self.arbitrary_tuple_type(u, type_fuel)?,
            )),
            5 => Ok(InterfaceType::Flags(
                self.arbitrary_flags_type(u, type_fuel)?,
            )),
            6 => Ok(InterfaceType::Enum(self.arbitrary_enum_type(u, type_fuel)?)),
            7 => Ok(InterfaceType::Union(
                self.arbitrary_union_type(u, type_fuel)?,
            )),
            8 => Ok(InterfaceType::Option(self.arbitrary_option_type(u)?)),
            9 => Ok(InterfaceType::Expected(self.arbitrary_expected_type(u)?)),
            _ => unreachable!(),
        }
    }

    fn push_import(&mut self, name: String, ty: TypeIndex) {
        let nth = match self.ensure_section(
            |sec| matches!(sec, Section::Import(_)),
            || Section::Import(ImportSection { imports: vec![] }),
        ) {
            Section::Import(sec) => {
                sec.imports.push(Import {
                    name,
                    ty: ty.clone(),
                });
                sec.imports.len() - 1
            }
            _ => unreachable!(),
        };
        let section_index = self.component().component.sections.len() - 1;

        match &*ty.ty {
            Type::Func(func_ty) => {
                let func_index = u32::try_from(self.component().funcs.len()).unwrap();
                self.component_mut()
                    .funcs
                    .push(ComponentOrCoreFuncType::Component(Rc::clone(func_ty)));

                self.component_mut().interface_funcs.push(func_index);
                if func_ty.is_scalar() {
                    self.component_mut().scalar_interface_funcs.push(func_index);
                }
            }

            Type::Module(_) => {
                self.total_modules += 1;
                self.component_mut().modules.push((section_index, nth));
            }
            Type::Component(_) => {
                self.total_components += 1;
                self.component_mut().components.push((section_index, nth));
            }
            Type::Instance(ty) => {
                self.total_instances += 1;
                self.component_mut()
                    .instances
                    .push(ComponentOrCoreInstanceType::Component(Rc::clone(ty)));
            }
            Type::Value(ty) => {
                self.total_values += 1;
                self.component_mut().values.push(ty.clone());
            }
            Type::Interface(_) => unreachable!("cannot import interface types"),
        }
    }

    fn interface_function_type(&self, inter_func: u32) -> &Rc<FuncType> {
        match &self.component().funcs[inter_func as usize] {
            ComponentOrCoreFuncType::Component(ty) => ty,
            ComponentOrCoreFuncType::Core(_) => panic!("not an interface function"),
        }
    }

    fn push_func(&mut self, func: Func) {
        let nth = match self.component_mut().component.sections.last_mut() {
            Some(Section::Func(FuncSection { funcs })) => funcs.len(),
            _ => {
                self.push_section(Section::Func(FuncSection { funcs: vec![] }));
                0
            }
        };
        let section_index = self.component().component.sections.len() - 1;

        let func_index = u32::try_from(self.component().funcs.len()).unwrap();

        let ty = match &func {
            Func::CanonLift { func_ty, .. } => {
                self.component_mut().interface_funcs.push(func_index);
                let ty = Rc::clone(self.current_type_scope().get_func(*func_ty));
                if ty.is_scalar() {
                    let interface_func_index = self.component().interface_funcs.len();
                    self.component_mut().scalar_interface_funcs.push(func_index);
                }
                ComponentOrCoreFuncType::Component(ty)
            }
            Func::CanonLower { inter_func, .. } => {
                let inter_func_ty = self.interface_function_type(*inter_func);
                let core_func_ty = canonical_abi_for(inter_func_ty);
                self.component_mut().core_funcs.push(func_index);
                ComponentOrCoreFuncType::Core(core_func_ty)
            }
        };

        self.component_mut().funcs.push(ty);

        match self.component_mut().component.sections.last_mut() {
            Some(Section::Func(FuncSection { funcs })) => funcs.push(func),
            _ => unreachable!(),
        }
    }

    fn arbitrary_import_section(&mut self, u: &mut Unstructured) -> Result<Step> {
        self.push_section(Section::Import(ImportSection { imports: vec![] }));

        let min = if self.fill_minimums {
            self.config
                .min_imports()
                .saturating_sub(self.component().num_imports)
        } else {
            // Allow generating empty sections. We can always fill in the required
            // minimum later.
            0
        };
        let max = self.config.max_imports() - self.component().num_imports;

        if !self.current_type_scope().def_types.is_empty() {
            crate::arbitrary_loop(u, min, max, |u| {
                let name = crate::unique_string(100, &mut self.component_mut().import_names, u)?;

                let mut choices: Vec<fn(&mut Unstructured, &mut ComponentBuilder) -> Result<u32>> =
                    vec![];

                if !self.current_type_scope().module_types.is_empty()
                    && self.total_modules < self.config.max_modules()
                {
                    choices.push(|u, c| u.choose(&c.current_type_scope().module_types).copied());
                }

                if !self.current_type_scope().component_types.is_empty()
                    && self.total_components < self.config.max_components()
                {
                    choices.push(|u, c| u.choose(&c.current_type_scope().component_types).copied());
                }

                if !self.current_type_scope().instance_types.is_empty()
                    && self.total_instances < self.config.max_instances()
                {
                    choices.push(|u, c| u.choose(&c.current_type_scope().instance_types).copied());
                }

                if !self.current_type_scope().func_types.is_empty()
                    && self.component().num_funcs() < self.config.max_funcs()
                {
                    choices.push(|u, c| u.choose(&c.current_type_scope().func_types).copied());
                }

                if !self.current_type_scope().value_types.is_empty()
                    && self.total_values < self.config.max_values()
                {
                    choices.push(|u, c| u.choose(&c.current_type_scope().value_types).copied());
                }

                if choices.is_empty() {
                    return Ok(false);
                }

                let f = u.choose(&choices)?;
                let index = f(u, self)?;
                let ty = Rc::clone(self.current_type_scope().get(index));
                self.push_import(name, TypeIndex { index, ty });
                Ok(true)
            })?;
        }

        Ok(Step::StillBuilding)
    }

    fn arbitrary_func_section(&mut self, u: &mut Unstructured) -> Result<Step> {
        self.push_section(Section::Func(FuncSection { funcs: vec![] }));

        let min = if self.fill_minimums {
            self.config
                .min_funcs()
                .saturating_sub(self.component().funcs.len())
        } else {
            // Allow generating empty sections. We can always fill in the
            // required minimum later.
            0
        };
        let max = self.config.max_funcs() - self.component().funcs.len();

        let mut choices: Vec<fn(&mut Unstructured, &mut ComponentBuilder) -> Result<Option<Func>>> =
            Vec::with_capacity(2);

        crate::arbitrary_loop(u, min, max, |u| {
            choices.clear();

            // NB: We only lift/lower scalar interface functions.
            //
            // If we generated lifting and lowering of compound interface types,
            // the probability of generating a corresponding Wasm module that
            // generates valid instances of the compound interface types would
            // be vanishingly tiny (e.g. for `list<string>` we would have to
            // generate a core Wasm module that correctly produces a pointer and
            // length for a memory region that itself is a series of pointers
            // and lengths of valid strings, as well as `canonical_abi_realloc`
            // and `canonical_abi_free` functions that do the right thing).
            //
            // This is a pretty serious limitation of `wasm-smith`'s interface
            // types support, but it is one we are intentionally
            // accepting. `wasm-smith` will focus on generating arbitrary
            // component sections, structures, and import/export topologies; not
            // interface functions and core Wasm implementations of interface
            // functions. In the future, we intend to build a new, distinct test
            // case generator specifically for exercising interface functions
            // and the canonical ABI. This new generator won't emit arbitrary
            // component sections, structures, or import/export topologies, and
            // will instead leave that to `wasm-smith`.

            if !self.component().scalar_interface_funcs.is_empty() {
                choices.push(|u, c| {
                    let inter_func = *u.choose(&c.component().scalar_interface_funcs)?;
                    Ok(Some(Func::CanonLower {
                        // Scalar interface functions don't use any canonical options.
                        options: vec![],
                        inter_func,
                    }))
                });
            }

            if !self.component().core_funcs.is_empty() {
                choices.push(|u, c| {
                    let core_func = *u.choose(&c.component().core_funcs)?;
                    let core_func_ty = c.component().funcs[core_func as usize]
                        .clone()
                        .unwrap_core();
                    let inter_func_ty = inverse_scalar_canonical_abi_for(u, &core_func_ty)?;

                    let func_ty = if let Some(indices) = c
                        .current_type_scope()
                        .func_type_to_indices
                        .get(&inter_func_ty)
                    {
                        // If we've already defined this interface function type
                        // one or more times, then choose one of those
                        // definitions arbitrarily.
                        debug_assert!(!indices.is_empty());
                        *u.choose(indices)?
                    } else if c.current_type_scope().types.len() < c.config.max_types() {
                        // If we haven't already defined this interface function
                        // type, and we haven't defined the configured maximum
                        // amount of types yet, then just define this type.
                        let ty = Rc::new(Type::Func(Rc::new(inter_func_ty)));
                        c.push_type(ty)
                    } else {
                        // Otherwise, give up on lifting this function.
                        return Ok(None);
                    };

                    Ok(Some(Func::CanonLift {
                        func_ty,
                        // Scalar functions don't use any canonical options.
                        options: vec![],
                        core_func,
                    }))
                });
            }

            if choices.is_empty() {
                return Ok(false);
            }

            let f = u.choose(&choices)?;
            if let Some(func) = f(u, self)? {
                self.push_func(func);
            }

            Ok(true)
        })?;

        Ok(Step::StillBuilding)
    }

    fn arbitrary_core_section(&mut self, u: &mut Unstructured) -> Result<Step> {
        let config: Rc<dyn Config> = Rc::clone(&self.config);
        let module = crate::core::Module::new_internal(
            config,
            u,
            crate::core::DuplicateImportsBehavior::Disallowed,
        )?;
        self.push_section(Section::Core(module));
        self.total_modules += 1;
        Ok(Step::StillBuilding)
    }

    fn arbitrary_component_section(&mut self, u: &mut Unstructured) -> Result<Step> {
        self.types.push(TypesScope::default());
        self.components.push(ComponentContext::empty());
        self.total_components += 1;
        Ok(Step::StillBuilding)
    }

    fn arbitrary_instance_section(&mut self, u: &mut Unstructured) -> Result<()> {
        todo!()
    }

    fn arbitrary_export_section(&mut self, u: &mut Unstructured) -> Result<()> {
        todo!()
    }

    fn arbitrary_start_section(&mut self, u: &mut Unstructured) -> Result<()> {
        todo!()
    }

    fn arbitrary_alias_section(&mut self, u: &mut Unstructured) -> Result<()> {
        todo!()
    }
}

fn canonical_abi_for(inter_func_ty: &FuncType) -> Rc<crate::core::FuncType> {
    let to_core_ty = |ty| match ty {
        InterfaceTypeRef::Primitive(prim_ty) => match prim_ty {
            PrimitiveInterfaceType::Unit => None,
            PrimitiveInterfaceType::Char
            | PrimitiveInterfaceType::Bool
            | PrimitiveInterfaceType::S8
            | PrimitiveInterfaceType::U8
            | PrimitiveInterfaceType::S16
            | PrimitiveInterfaceType::U16
            | PrimitiveInterfaceType::S32
            | PrimitiveInterfaceType::U32 => Some(ValType::I32),
            PrimitiveInterfaceType::S64 | PrimitiveInterfaceType::U64 => Some(ValType::I64),
            PrimitiveInterfaceType::Float32 => Some(ValType::F32),
            PrimitiveInterfaceType::Float64 => Some(ValType::F64),
            PrimitiveInterfaceType::String => {
                unimplemented!("non-scalar types are not supported yet")
            }
        },
        InterfaceTypeRef::Type(_) => unimplemented!("non-scalar types are not supported yet"),
    };

    Rc::new(crate::core::FuncType {
        params: inter_func_ty
            .params
            .iter()
            .flat_map(|ty| to_core_ty(ty.ty.clone()))
            .collect(),
        results: to_core_ty(inter_func_ty.result.clone())
            .into_iter()
            .collect(),
    })
}

fn inverse_scalar_canonical_abi_for(
    u: &mut Unstructured,
    core_func_ty: &crate::core::FuncType,
) -> Result<FuncType> {
    let from_core_ty = |u: &mut Unstructured, core_ty| match core_ty {
        ValType::I32 => u
            .choose(&[
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Char),
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Bool),
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S8),
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U8),
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S16),
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U16),
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S32),
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U32),
            ])
            .cloned(),
        ValType::I64 => u
            .choose(&[
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S64),
                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U64),
            ])
            .cloned(),
        ValType::F32 => Ok(InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Float32)),
        ValType::F64 => Ok(InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Float64)),
        ValType::V128 | ValType::FuncRef | ValType::ExternRef => {
            unreachable!("not used in canonical ABI")
        }
    };

    let mut param_names = HashSet::default();
    let mut params = vec![];
    if u.ratio::<u8>(1, 25)? {
        params.push(OptionalNamedType {
            name: if u.arbitrary()? {
                Some(crate::unique_non_empty_string(100, &mut param_names, u)?)
            } else {
                None
            },
            ty: InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit),
        });
    }
    for core_ty in &core_func_ty.params {
        params.push(OptionalNamedType {
            name: if u.arbitrary()? {
                Some(crate::unique_non_empty_string(100, &mut param_names, u)?)
            } else {
                None
            },
            ty: from_core_ty(u, *core_ty)?,
        });
        if u.ratio::<u8>(1, 25)? {
            params.push(OptionalNamedType {
                name: if u.arbitrary()? {
                    Some(crate::unique_non_empty_string(100, &mut param_names, u)?)
                } else {
                    None
                },
                ty: InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit),
            });
        }
    }

    let result = match core_func_ty.results.len() {
        0 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit),
        1 => from_core_ty(u, core_func_ty.results[0])?,
        _ => unimplemented!("non-scalar types are not supported yet"),
    };

    Ok(FuncType { params, result })
}

#[derive(Debug)]
enum Section {
    Custom(CustomSection),
    Type(TypeSection),
    Import(ImportSection),
    Func(FuncSection),
    Core(crate::Module),
    Component(Component),
    Instance(InstanceSection),
    Export(ExportSection),
    Start(StartSection),
    Alias(AliasSection),
}

#[derive(Debug)]
struct CustomSection {
    name: String,
    data: Vec<u8>,
}

impl<'a> Arbitrary<'a> for CustomSection {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let name = crate::limited_string(1_000, u)?;
        let data = u.arbitrary()?;
        Ok(CustomSection { name, data })
    }
}

#[derive(Debug)]
struct TypeSection {
    types: Vec<Rc<Type>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Type {
    Module(Rc<ModuleType>),
    Component(Rc<ComponentType>),
    Instance(Rc<InstanceType>),
    Func(Rc<FuncType>),
    Value(ValueType),
    Interface(InterfaceType),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ModuleType {
    defs: Vec<ModuleTypeDef>,
    has_memory: bool,
    has_canonical_abi_realloc: bool,
    has_canonical_abi_free: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ModuleTypeDef {
    TypeDef(crate::core::Type),
    Import(crate::core::Import),
    Export(String, crate::core::EntityType),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Alias {
    InstanceExport {
        instance: u32,
        name: String,
        kind: InstanceExportAliasKind,
    },
    Outer {
        count: u32,
        i: u32,
        kind: OuterAliasKind,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum InstanceExportAliasKind {
    Module,
    Component,
    Instance,
    Func,
    Value,
    Table,
    Memory,
    Global,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum OuterAliasKind {
    Module,
    Component,
    Type,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ComponentType {
    defs: Vec<ComponentTypeDef>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ComponentTypeDef {
    Import(Import),
    Type(Rc<Type>),
    Export { name: String, ty: TypeIndex },
    Alias(Alias),
}

impl From<InstanceTypeDef> for ComponentTypeDef {
    fn from(def: InstanceTypeDef) -> Self {
        match def {
            InstanceTypeDef::Type(t) => ComponentTypeDef::Type(t),
            InstanceTypeDef::Export { name, ty } => ComponentTypeDef::Export { name, ty },
            InstanceTypeDef::Alias(a) => ComponentTypeDef::Alias(a),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct InstanceType {
    defs: Vec<InstanceTypeDef>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum InstanceTypeDef {
    Type(Rc<Type>),
    Export { name: String, ty: TypeIndex },
    Alias(Alias),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FuncType {
    params: Vec<OptionalNamedType>,
    result: InterfaceTypeRef,
}

impl FuncType {
    fn is_scalar(&self) -> bool {
        self.params.iter().all(|p| p.is_scalar()) && is_scalar(&self.result)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct OptionalNamedType {
    name: Option<String>,
    ty: InterfaceTypeRef,
}

impl OptionalNamedType {
    fn is_scalar(&self) -> bool {
        is_scalar(&self.ty)
    }
}

fn is_scalar(ty: &InterfaceTypeRef) -> bool {
    match ty {
        InterfaceTypeRef::Primitive(prim) => match prim {
            PrimitiveInterfaceType::Unit
            | PrimitiveInterfaceType::Bool
            | PrimitiveInterfaceType::S8
            | PrimitiveInterfaceType::U8
            | PrimitiveInterfaceType::S16
            | PrimitiveInterfaceType::U16
            | PrimitiveInterfaceType::S32
            | PrimitiveInterfaceType::U32
            | PrimitiveInterfaceType::S64
            | PrimitiveInterfaceType::U64
            | PrimitiveInterfaceType::Float32
            | PrimitiveInterfaceType::Float64
            | PrimitiveInterfaceType::Char => true,
            PrimitiveInterfaceType::String => false,
        },
        InterfaceTypeRef::Type(_) => false,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct NamedType {
    name: String,
    ty: InterfaceTypeRef,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ValueType(InterfaceTypeRef);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum InterfaceType {
    Primitive(PrimitiveInterfaceType),
    Record(RecordType),
    Variant(VariantType),
    List(ListType),
    Tuple(TupleType),
    Flags(FlagsType),
    Enum(EnumType),
    Union(UnionType),
    Option(OptionType),
    Expected(ExpectedType),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct RecordType {
    fields: Vec<NamedType>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct VariantType {
    cases: Vec<(NamedType, Option<u32>)>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ListType {
    elem_ty: InterfaceTypeRef,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TupleType {
    fields: Vec<InterfaceTypeRef>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FlagsType {
    fields: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct EnumType {
    variants: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct UnionType {
    variants: Vec<InterfaceTypeRef>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct OptionType {
    inner_ty: InterfaceTypeRef,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ExpectedType {
    ok_ty: InterfaceTypeRef,
    err_ty: InterfaceTypeRef,
}

#[derive(Debug)]
struct ImportSection {
    imports: Vec<Import>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Import {
    name: String,
    ty: TypeIndex,
}

#[derive(Debug)]
struct FuncSection {
    funcs: Vec<Func>,
}

#[derive(Debug)]
enum Func {
    CanonLift {
        func_ty: u32,
        options: Vec<CanonOpt>,
        core_func: u32,
    },
    CanonLower {
        options: Vec<CanonOpt>,
        inter_func: u32,
    },
}

#[derive(Debug)]
enum CanonOpt {
    StringUtf8,
    StringUtf16,
    StringLatin1Utf16,
    Into { instance: u32 },
}

#[derive(Debug)]
struct InstanceSection {}

#[derive(Debug)]
struct ExportSection {}

#[derive(Debug)]
struct StartSection {}

#[derive(Debug)]
struct AliasSection {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum InterfaceTypeRef {
    Primitive(PrimitiveInterfaceType),
    Type(TypeIndex),
}

impl From<InterfaceTypeRef> for wasm_encoder::InterfaceTypeRef {
    fn from(r: InterfaceTypeRef) -> Self {
        match r {
            InterfaceTypeRef::Primitive(p) => wasm_encoder::InterfaceTypeRef::Primitive(p),
            InterfaceTypeRef::Type(t) => wasm_encoder::InterfaceTypeRef::Type(t.index),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TypeIndex {
    index: u32,
    ty: Rc<Type>,
}
