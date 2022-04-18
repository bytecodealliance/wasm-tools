//! Generation of Wasm
//! [components](https://github.com/WebAssembly/component-model).

#![allow(unused_variables, dead_code)] // TODO FITZGEN

use crate::{arbitrary_loop, Config, DefaultConfig};
use arbitrary::{Arbitrary, Result, Unstructured};
use std::convert::TryFrom;
use std::{
    collections::{HashMap, HashSet},
    marker,
    rc::Rc,
};
use wasm_encoder::{InterfaceTypeRef, PrimitiveInterfaceType, ValType};

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
}

/// Metadata (e.g. contents of various index spaces) we keep track of on a
/// per-component basis.
struct ComponentContext {
    // The actual component itself.
    component: Component,

    // The number of imports we have generated thus far.
    num_imports: usize,

    // The set of names of imports we've generated thus far.
    import_names: HashSet<String>,
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

    // The indices of all the entries in `types` that are value types.
    value_types: Vec<u32>,

    // The indices of all the entries in `types` that are interface types.
    interface_types: Vec<u32>,
}

impl TypesScope {
    fn push(&mut self, ty: Rc<Type>) {
        let (is_def_type, kind_list) = match &*ty {
            Type::Module(_) => (true, &mut self.module_types),
            Type::Component(_) => (true, &mut self.component_types),
            Type::Instance(_) => (true, &mut self.instance_types),
            Type::Func(_) => (true, &mut self.func_types),
            Type::Value(_) => (true, &mut self.value_types),
            Type::Interface(_) => (false, &mut self.interface_types),
        };
        let ty_idx = u32::try_from(self.types.len()).unwrap();
        if is_def_type {
            self.def_types.push(ty_idx);
        }
        kind_list.push(ty_idx);
        self.types.push(ty);
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

impl ComponentContext {
    fn empty() -> Self {
        ComponentContext {
            num_imports: 0,
            import_names: HashSet::default(),
            component: Component::empty(),
        }
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

                // TODO FITZGEN
                //
                // choices.push(Self::arbitrary_func_section);
                // choices.push(Self::arbitrary_core_section);
                // choices.push(Self::arbitrary_component_section);
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
                        self.append_nested_component(component);
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
        }
        self.fill_minimums = false;

        self.types
            .pop()
            .expect("should have a types scope for the component we are finishing");
        Ok(Step::Finished(self.components.pop().unwrap().component))
    }

    fn append_nested_component(&mut self, component: Component) {
        match self.last_section_mut() {
            Some(Section::Component(sec)) => sec.components.push(component),
            _ => self.push_section(Section::Component(ComponentSection {
                components: vec![component],
            })),
        }
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

    fn push_section(&mut self, section: Section) {
        self.component_mut().component.sections.push(section);
    }

    fn last_section_mut(&mut self) -> Option<&mut Section> {
        self.component_mut().component.sections.last_mut()
    }

    fn arbitrary_custom_section(&mut self, u: &mut Unstructured) -> Result<Step> {
        self.push_section(Section::Custom(u.arbitrary()?));
        Ok(Step::StillBuilding)
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

            let section_idx = self.component().component.sections.len() - 1;
            let section = match self.last_section_mut() {
                Some(Section::Type(section)) => section,
                _ => unreachable!(),
            };
            section.types.push(ty.clone());

            self.current_type_scope_mut().push(ty);
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
    ) -> Result<ModuleType> {
        let mut defs = vec![];
        let mut types: Vec<Rc<crate::core::FuncType>> = vec![];
        let mut imports = HashMap::new();
        let mut exports = HashSet::new();
        let mut counts = EntityCounts::default();

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
                    let ty = crate::core::arbitrary_func_type(u, &self.core_valtypes, None)?;
                    types.push(ty.clone());
                    defs.push(ModuleTypeDef::TypeDef(crate::core::Type::Func(ty)));
                }
            }

            Ok(true)
        })?;

        Ok(ModuleType { defs })
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
                    .filter(|ty| ty.results.is_empty())
                    .enumerate()
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
    ) -> Result<ComponentType> {
        let mut defs = vec![];
        let mut imports = HashSet::new();
        let mut exports = HashSet::new();

        self.with_types_scope(|me| {
            arbitrary_loop(u, 0, 100, |u| {
                *type_fuel = type_fuel.saturating_sub(1);
                if *type_fuel == 0 {
                    return Ok(false);
                }

                if !me.current_type_scope().types.is_empty() && u.int_in_range::<u8>(0..=3)? == 0 {
                    // Imports.
                    let name = crate::unique_string(100, &mut imports, u)?;
                    let ty = u.int_in_range(0..=me.current_type_scope().types.len() - 1)?;
                    let ty = u32::try_from(ty).unwrap();
                    defs.push(ComponentTypeDef::Import(Import { name, ty }));
                } else {
                    // Type definitions, exports, and aliases.
                    let def = me.arbitrary_instance_type_def(u, &mut exports, type_fuel)?;
                    defs.push(def.into());
                }
                Ok(true)
            })
        })?;

        Ok(ComponentType { defs })
    }

    fn arbitrary_instance_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<InstanceType> {
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

        Ok(InstanceType { defs })
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
                Ok(InstanceTypeDef::Export {
                    name: crate::unique_string(100, exports, u)?,
                    ty: u.int_in_range(
                        0..=u32::try_from(me.current_type_scope().types.len()).unwrap() - 1,
                    )?,
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
    ) -> Result<FuncType> {
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

        Ok(FuncType { params, result })
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
            1 => Ok(InterfaceTypeRef::Type(
                *u.choose(&self.current_type_scope().interface_types)?,
            )),
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
        let name = crate::unique_string(100, names, u)?;
        let ty = self.arbitrary_interface_type_ref(u)?;
        Ok(NamedType { name, ty })
    }

    fn arbitrary_optional_named_type(
        &mut self,
        u: &mut Unstructured,
        names: &mut HashSet<String>,
    ) -> Result<OptionalNamedType> {
        let name = if u.arbitrary()? {
            Some(crate::unique_string(100, names, u)?)
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

            fields.push(crate::unique_string(100, &mut field_names, u)?);
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

            variants.push(crate::unique_string(100, &mut variant_names, u)?);
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

    fn arbitrary_import_section(&mut self, u: &mut Unstructured) -> Result<Step> {
        let mut imports = vec![];

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
                let max_def_ty_idx = self.current_type_scope().def_types.len() - 1;
                let def_ty_idx = u.int_in_range(0..=max_def_ty_idx)?;
                let ty = self.current_type_scope().def_types[def_ty_idx];
                imports.push(Import { name, ty });
                Ok(true)
            })?;
        }

        self.push_section(Section::Import(ImportSection { imports }));
        Ok(Step::StillBuilding)
    }

    fn arbitrary_func_section(&mut self, u: &mut Unstructured) -> Result<()> {
        todo!()
    }

    fn arbitrary_core_section(&mut self, u: &mut Unstructured) -> Result<()> {
        todo!()
    }

    fn arbitrary_component_section(&mut self, u: &mut Unstructured) -> Result<()> {
        todo!()
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

#[derive(Debug)]
enum Section {
    Custom(CustomSection),
    Type(TypeSection),
    Import(ImportSection),
    Func(FuncSection),
    Core(CoreSection),
    Component(ComponentSection),
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

#[derive(Clone, Debug)]
enum Type {
    Module(ModuleType),
    Component(ComponentType),
    Instance(InstanceType),
    Func(FuncType),
    Value(ValueType),
    Interface(InterfaceType),
}

#[derive(Clone, Debug)]
struct ModuleType {
    defs: Vec<ModuleTypeDef>,
}

#[derive(Clone, Debug)]
enum ModuleTypeDef {
    TypeDef(crate::core::Type),
    Import(crate::core::Import),
    Export(String, crate::core::EntityType),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
enum OuterAliasKind {
    Module,
    Component,
    Type,
}

#[derive(Clone, Debug)]
struct ComponentType {
    defs: Vec<ComponentTypeDef>,
}

#[derive(Clone, Debug)]
enum ComponentTypeDef {
    Import(Import),
    Type(Rc<Type>),
    Export { name: String, ty: u32 },
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

#[derive(Clone, Debug)]
struct InstanceType {
    defs: Vec<InstanceTypeDef>,
}

#[derive(Clone, Debug)]
enum InstanceTypeDef {
    Type(Rc<Type>),
    Export { name: String, ty: u32 },
    Alias(Alias),
}

#[derive(Clone, Debug)]
struct FuncType {
    params: Vec<OptionalNamedType>,
    result: InterfaceTypeRef,
}

#[derive(Clone, Debug)]
struct OptionalNamedType {
    name: Option<String>,
    ty: InterfaceTypeRef,
}

#[derive(Clone, Debug)]
struct NamedType {
    name: String,
    ty: InterfaceTypeRef,
}

#[derive(Clone, Debug)]
struct ValueType(InterfaceTypeRef);

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
struct RecordType {
    fields: Vec<NamedType>,
}

#[derive(Clone, Debug)]
struct VariantType {
    cases: Vec<(NamedType, Option<u32>)>,
}

#[derive(Clone, Debug)]
struct ListType {
    elem_ty: InterfaceTypeRef,
}

#[derive(Clone, Debug)]
struct TupleType {
    fields: Vec<InterfaceTypeRef>,
}

#[derive(Clone, Debug)]
struct FlagsType {
    fields: Vec<String>,
}

#[derive(Clone, Debug)]
struct EnumType {
    variants: Vec<String>,
}

#[derive(Clone, Debug)]
struct UnionType {
    variants: Vec<InterfaceTypeRef>,
}

#[derive(Clone, Debug)]
struct OptionType {
    inner_ty: InterfaceTypeRef,
}

#[derive(Clone, Debug)]
struct ExpectedType {
    ok_ty: InterfaceTypeRef,
    err_ty: InterfaceTypeRef,
}

#[derive(Debug)]
struct ImportSection {
    imports: Vec<Import>,
}

#[derive(Clone, Debug)]
struct Import {
    name: String,
    ty: u32,
}

#[derive(Debug)]
struct FuncSection {}

#[derive(Debug)]
struct CoreSection {}

#[derive(Debug)]
struct ComponentSection {
    components: Vec<Component>,
}

#[derive(Debug)]
struct InstanceSection {}

#[derive(Debug)]
struct ExportSection {}

#[derive(Debug)]
struct StartSection {}

#[derive(Debug)]
struct AliasSection {}
