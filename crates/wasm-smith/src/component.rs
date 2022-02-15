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
use wasm_encoder::{InterfaceType, ValType};

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
    config: Rc<dyn Config>,

    // The set of core `valtype`s that we are configured to generate.
    core_valtypes: Vec<ValType>,

    // Stack of types scopes that are currently available. The last entry is the
    // current scope. We can add aliases to anything in the current scope or
    // parent scopes.
    types_scopes: Vec<TypesScope>,

    // All the sections we've generated thus far for the component.
    sections: Vec<Section>,

    // An indirect list of all types generated in this component. Each entry is
    // of the form `(i, j)` where `sections[i]` is guaranteed to be a
    // `Section::Type` and we are referencing the `j`th type in that section.
    types: Vec<(usize, usize)>,

    // Whether we are in the final bits of generating this component and we just
    // need to ensure that the minimum number of entities configured have all
    // been generated. This changes the behavior of various
    // `arbitrary_<section>` methods to always fill in their minimums.
    fill_minimums: bool,
}

#[derive(Debug, Default)]
struct TypesScope {
    // All types in this index space, regardless of kind.
    types: Vec<Rc<Type>>,

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

    // The indices of all the entries in `types` that are compound types.
    compound_types: Vec<u32>,
}

impl TypesScope {
    fn push(&mut self, ty: Rc<Type>) {
        let kind_list = match &*ty {
            Type::Module(_) => &mut self.module_types,
            Type::Component(_) => &mut self.component_types,
            Type::Instance(_) => &mut self.instance_types,
            Type::Func(_) => &mut self.func_types,
            Type::Value(_) => &mut self.value_types,
            Type::Compound(_) => &mut self.compound_types,
        };
        let ty_idx = u32::try_from(self.types.len()).unwrap();
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
        Ok(ConfiguredComponent {
            component: Component::new(C::arbitrary(u)?, u)?,
            _marker: marker::PhantomData,
        })
    }
}

impl Component {
    /// Construct a new `Component` using the given configuration.
    pub fn new(config: impl Config, u: &mut Unstructured) -> Result<Self> {
        let mut component = Component::empty(Rc::new(config));
        component.build(u)?;
        Ok(component)
    }

    fn empty(config: Rc<dyn Config>) -> Self {
        Component {
            config,
            core_valtypes: vec![],
            types_scopes: vec![Default::default()],
            sections: vec![],
            types: vec![],
            fill_minimums: false,
        }
    }

    fn build(&mut self, u: &mut Unstructured) -> Result<()> {
        self.core_valtypes = crate::core::configured_valtypes(&*self.config);

        let mut choices: Vec<fn(&mut Component, &mut Unstructured) -> Result<()>> = vec![];
        loop {
            // Keep going while the fuzzer tells us to / has more data for us.
            if !u.arbitrary()? {
                break;
            }

            choices.clear();
            choices.push(Self::arbitrary_custom_section);

            // NB: we add this as a choice even if we've already generated our
            // maximum number of types so that we can exercise adding empty type
            // sections to the end of the module.
            choices.push(Self::arbitrary_type_section);

            // TODO FITZGEN
            //
            // choices.push(Self::arbitrary_import_section);
            // choices.push(Self::arbitrary_func_section);
            // choices.push(Self::arbitrary_core_section);
            // choices.push(Self::arbitrary_component_section);
            // choices.push(Self::arbitrary_instance_section);
            // choices.push(Self::arbitrary_export_section);
            // choices.push(Self::arbitrary_start_section);
            // choices.push(Self::arbitrary_alias_section);

            let f = u.choose(&choices)?;
            f(self, u)?;
        }

        // Ensure we've generated all of our minimums.
        self.fill_minimums = true;
        if self.types.len() < self.config.min_types() {
            self.arbitrary_type_section(u)?;
        }

        Ok(())
    }

    fn arbitrary_custom_section(&mut self, u: &mut Unstructured) -> Result<()> {
        self.sections.push(Section::Custom(u.arbitrary()?));
        Ok(())
    }

    fn arbitrary_type_section(&mut self, u: &mut Unstructured) -> Result<()> {
        self.sections
            .push(Section::Type(TypeSection { types: vec![] }));

        let min = if self.fill_minimums {
            self.config.min_types().saturating_sub(self.types.len())
        } else {
            0
        };

        let max = self.config.max_types() - self.types.len();

        arbitrary_loop(u, min, max, |u| {
            let mut type_fuel = self.config.max_type_size();
            let ty = self.arbitrary_type(u, &mut type_fuel)?;

            let section_idx = self.sections.len() - 1;
            let section = match self.sections.last_mut().unwrap() {
                Section::Type(section) => section,
                _ => unreachable!(),
            };
            self.types.push((section_idx, section.types.len()));
            section.types.push(ty.clone());

            self.current_type_scope_mut().push(ty);
            Ok(true)
        })?;

        Ok(())
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
            5 => Type::Compound(self.arbitrary_compound_type(u, type_fuel)?),
            _ => unreachable!(),
        };
        Ok(Rc::new(ty))
    }

    fn arbitrary_module_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<ModuleType> {
        // TODO: this currently only supports function type definitions,
        // function imports, and function exports.

        let mut defs = vec![];
        let mut types: Vec<Rc<crate::core::FuncType>> = vec![];
        let mut imports = HashMap::new();
        let mut exports = HashSet::new();

        arbitrary_loop(u, 0, 100, |u| {
            *type_fuel = type_fuel.saturating_sub(1);
            if *type_fuel == 0 {
                return Ok(false);
            }

            match u.int_in_range::<u8>(0..=2)? {
                // Import.
                0 if !types.is_empty() => {
                    let module = crate::limited_string(100, u)?;
                    let existing_module_imports = imports.entry(module.clone()).or_default();
                    let field = crate::unique_string(100, existing_module_imports, u)?;
                    let ty_idx = u.int_in_range(0..=types.len() - 1)?;
                    let ty = types[ty_idx].clone();
                    defs.push(ModuleTypeDef::Import(crate::core::Import(
                        module,
                        field,
                        crate::core::EntityType::Func(u32::try_from(ty_idx).unwrap(), ty),
                    )));
                }

                // Export.
                1 if !types.is_empty() => {
                    let name = crate::unique_string(100, &mut exports, u)?;
                    let ty_idx = u.int_in_range(0..=types.len() - 1)?;
                    let ty = types[ty_idx].clone();
                    defs.push(ModuleTypeDef::Export(
                        name,
                        crate::core::EntityType::Func(u32::try_from(ty_idx).unwrap(), ty),
                    ));
                }

                // Type definition.
                _ => {
                    let ty = crate::core::arbitrary_func_type(u, &self.core_valtypes)?;
                    types.push(ty.clone());
                    defs.push(ModuleTypeDef::TypeDef(crate::core::Type::Func(ty)));
                }
            }
            Ok(true)
        })?;

        Ok(ModuleType { defs })
    }

    fn with_types_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        self.types_scopes.push(Default::default());
        let result = f(self);
        self.types_scopes.pop();
        result
    }

    fn current_type_scope(&self) -> &TypesScope {
        self.types_scopes.last().unwrap()
    }

    fn current_type_scope_mut(&mut self) -> &mut TypesScope {
        self.types_scopes.last_mut().unwrap()
    }

    fn outer_types_scope(&self, count: u32) -> &TypesScope {
        &self.types_scopes[self.types_scopes.len() - 1 - usize::try_from(count).unwrap()]
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
                &mut Component,
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
        if self
            .types_scopes
            .iter()
            .any(|scope| !scope.types.is_empty())
        {
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
            .types_scopes
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

            params.push(self.arbitrary_named_type(u, &mut param_names)?);
            Ok(true)
        })?;

        let result = self.arbitrary_interface_type(u)?;

        Ok(FuncType { params, result })
    }

    fn arbitrary_value_type(&mut self, u: &mut Unstructured) -> Result<ValueType> {
        Ok(ValueType(self.arbitrary_interface_type(u)?))
    }

    fn arbitrary_interface_type(&mut self, u: &mut Unstructured) -> Result<InterfaceType> {
        let num_choices = if self.current_type_scope().compound_types.is_empty() {
            13
        } else {
            14
        };
        match u.int_in_range(0..=num_choices)? {
            0 => Ok(InterfaceType::Unit),
            1 => Ok(InterfaceType::Bool),
            2 => Ok(InterfaceType::S8),
            3 => Ok(InterfaceType::U8),
            4 => Ok(InterfaceType::S16),
            5 => Ok(InterfaceType::U16),
            6 => Ok(InterfaceType::S32),
            7 => Ok(InterfaceType::U32),
            8 => Ok(InterfaceType::S64),
            9 => Ok(InterfaceType::U64),
            10 => Ok(InterfaceType::F32),
            11 => Ok(InterfaceType::F64),
            12 => Ok(InterfaceType::Char),
            13 => Ok(InterfaceType::String),
            14 => Ok(InterfaceType::Compound(
                *u.choose(&self.current_type_scope().compound_types)?,
            )),
            _ => unreachable!(),
        }
    }

    fn arbitrary_named_type(
        &mut self,
        u: &mut Unstructured,
        names: &mut HashSet<String>,
    ) -> Result<NamedType> {
        let name = crate::unique_string(100, names, u)?;
        let ty = self.arbitrary_interface_type(u)?;
        Ok(NamedType { name, ty })
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

            cases.push(self.arbitrary_named_type(u, &mut case_names)?);
            Ok(true)
        })?;

        let default = if !cases.is_empty() && u.arbitrary()? {
            let max_cases = u32::try_from(cases.len() - 1).unwrap();
            Some(u.int_in_range(0..=max_cases)?)
        } else {
            None
        };

        Ok(VariantType { cases, default })
    }

    fn arbitrary_list_type(&mut self, u: &mut Unstructured) -> Result<ListType> {
        Ok(ListType {
            elem_ty: self.arbitrary_interface_type(u)?,
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

            fields.push(self.arbitrary_interface_type(u)?);
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

            variants.push(self.arbitrary_interface_type(u)?);
            Ok(true)
        })?;
        Ok(UnionType { variants })
    }

    fn arbitrary_optional_type(&mut self, u: &mut Unstructured) -> Result<OptionalType> {
        Ok(OptionalType {
            inner_ty: self.arbitrary_interface_type(u)?,
        })
    }

    fn arbitrary_expected_type(&mut self, u: &mut Unstructured) -> Result<ExpectedType> {
        Ok(ExpectedType {
            ok_ty: self.arbitrary_interface_type(u)?,
            err_ty: self.arbitrary_interface_type(u)?,
        })
    }

    fn arbitrary_compound_type(
        &mut self,
        u: &mut Unstructured,
        type_fuel: &mut u32,
    ) -> Result<CompoundType> {
        match u.int_in_range(0..=8)? {
            0 => Ok(CompoundType::Record(
                self.arbitrary_record_type(u, type_fuel)?,
            )),
            1 => Ok(CompoundType::Variant(
                self.arbitrary_variant_type(u, type_fuel)?,
            )),
            2 => Ok(CompoundType::List(self.arbitrary_list_type(u)?)),
            3 => Ok(CompoundType::Tuple(
                self.arbitrary_tuple_type(u, type_fuel)?,
            )),
            4 => Ok(CompoundType::Flags(
                self.arbitrary_flags_type(u, type_fuel)?,
            )),
            5 => Ok(CompoundType::Enum(self.arbitrary_enum_type(u, type_fuel)?)),
            6 => Ok(CompoundType::Union(
                self.arbitrary_union_type(u, type_fuel)?,
            )),
            7 => Ok(CompoundType::Optional(self.arbitrary_optional_type(u)?)),
            8 => Ok(CompoundType::Expected(self.arbitrary_expected_type(u)?)),
            _ => unreachable!(),
        }
    }

    fn arbitrary_import_section(&mut self, u: &mut Unstructured) -> Result<()> {
        todo!()
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
    Compound(CompoundType),
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
    params: Vec<NamedType>,
    result: InterfaceType,
}

#[derive(Clone, Debug)]
struct NamedType {
    name: String,
    ty: InterfaceType,
}

#[derive(Clone, Debug)]
struct ValueType(InterfaceType);

#[derive(Clone, Debug)]
enum CompoundType {
    Record(RecordType),
    Variant(VariantType),
    List(ListType),
    Tuple(TupleType),
    Flags(FlagsType),
    Enum(EnumType),
    Union(UnionType),
    Optional(OptionalType),
    Expected(ExpectedType),
}

#[derive(Clone, Debug)]
struct RecordType {
    fields: Vec<NamedType>,
}

#[derive(Clone, Debug)]
struct VariantType {
    cases: Vec<NamedType>,
    default: Option<u32>,
}

#[derive(Clone, Debug)]
struct ListType {
    elem_ty: InterfaceType,
}

#[derive(Clone, Debug)]
struct TupleType {
    fields: Vec<InterfaceType>,
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
    variants: Vec<InterfaceType>,
}

#[derive(Clone, Debug)]
struct OptionalType {
    inner_ty: InterfaceType,
}

#[derive(Clone, Debug)]
struct ExpectedType {
    ok_ty: InterfaceType,
    err_ty: InterfaceType,
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
struct ComponentSection {}

#[derive(Debug)]
struct InstanceSection {}

#[derive(Debug)]
struct ExportSection {}

#[derive(Debug)]
struct StartSection {}

#[derive(Debug)]
struct AliasSection {}
