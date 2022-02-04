//! Generation of Wasm
//! [components](https://github.com/WebAssembly/component-model).

#![allow(unused_variables, dead_code)] // TODO FITZGEN

use crate::{arbitrary_loop, limited_str, Config, DefaultConfig};
use arbitrary::{Arbitrary, Result, Unstructured};
use std::convert::TryFrom;
use std::{
    collections::{HashMap, HashSet},
    marker,
    rc::Rc,
};
use wasm_encoder::ValType;

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
pub struct Component<'a> {
    config: Rc<dyn Config>,

    // The set of core `valtype`s that we are configured to generate.
    core_valtypes: Vec<ValType>,

    // Stack of types scopes that are currently available. The last entry is the
    // current scope. We can add aliases to anything in the current scope or
    // parent scopes.
    types_scopes: Vec<Vec<Rc<Type>>>,

    // All the sections we've generated thus far for the component.
    sections: Vec<Section<'a>>,

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

impl<'a> Arbitrary<'a> for Component<'a> {
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
pub struct ConfiguredComponent<'a, C> {
    /// The generated component, controlled by the configuration of `C` in the
    /// `Arbitrary` implementation.
    pub component: Component<'a>,
    _marker: marker::PhantomData<C>,
}

impl<'a, C> Arbitrary<'a> for ConfiguredComponent<'a, C>
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

impl<'a> Component<'a> {
    /// Construct a new `Component` using the given configuration.
    pub fn new(config: impl Config, u: &mut Unstructured<'a>) -> Result<Self> {
        let mut component = Component::empty(Rc::new(config));
        component.build(u)?;
        Ok(component)
    }

    fn empty(config: Rc<dyn Config>) -> Self {
        Component {
            config,
            core_valtypes: vec![],
            types_scopes: vec![vec![]],
            sections: vec![],
            types: vec![],
            fill_minimums: false,
        }
    }

    fn build(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        self.core_valtypes.push(ValType::I32);
        self.core_valtypes.push(ValType::I64);
        self.core_valtypes.push(ValType::F32);
        self.core_valtypes.push(ValType::F64);
        if self.config.simd_enabled() {
            self.core_valtypes.push(ValType::V128);
        }
        if self.config.reference_types_enabled() {
            self.core_valtypes.push(ValType::ExternRef);
            self.core_valtypes.push(ValType::FuncRef);
        }

        let mut choices: Vec<fn(&mut Component<'a>, &mut Unstructured<'a>) -> Result<()>> = vec![];

        loop {
            // Keep going while the fuzzer tells us to / has more data for us.
            if !u.arbitrary()? {
                break;
            }

            choices.clear();
            choices.push(Self::arbitrary_custom_section);
            if self.types.len() < self.config.max_types() {
                choices.push(Self::arbitrary_type_section);
            }

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

    fn arbitrary_custom_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        self.sections.push(Section::Custom(u.arbitrary()?));
        Ok(())
    }

    fn arbitrary_type_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        self.sections
            .push(Section::Type(TypeSection { types: vec![] }));

        let min = if self.fill_minimums {
            self.config.min_types().saturating_sub(self.types.len())
        } else {
            0
        };

        let max = self.config.max_types() - self.types.len();

        arbitrary_loop(u, min, max, |u| {
            let ty = self.arbitrary_type(u)?;
            let section_idx = self.sections.len() - 1;
            let section = match self.sections.last_mut().unwrap() {
                Section::Type(section) => section,
                _ => unreachable!(),
            };
            self.types.push((section_idx, section.types.len()));
            section.types.push(ty.clone());
            self.current_type_scope_mut().push(Rc::new(ty));
            Ok(true)
        })?;

        Ok(())
    }

    fn arbitrary_type(&mut self, u: &mut Unstructured<'a>) -> Result<Type> {
        match u.int_in_range::<u8>(0..=5)? {
            0 => Ok(Type::Module(self.arbitrary_module_type(u)?)),
            1 => Ok(Type::Component(self.arbitrary_component_type(u)?)),
            2 => Ok(Type::Instance(self.arbitrary_instance_type(u)?)),
            3 => Ok(Type::Func(self.arbitrary_func_type(u)?)),
            4 => Ok(Type::Value(self.arbitrary_value_type(u)?)),
            5 => Ok(Type::Compound(self.arbitrary_compound_type(u)?)),
            _ => unreachable!(),
        }
    }

    fn arbitrary_module_type(&mut self, u: &mut Unstructured<'a>) -> Result<ModuleType> {
        // TODO: this currently only supports function type definitions,
        // function imports, and function exports.

        let mut defs = vec![];
        let mut types: Vec<Rc<crate::core::FuncType>> = vec![];
        let mut imports = HashMap::new();
        let mut exports = HashSet::new();

        arbitrary_loop(u, 0, 100, |u| {
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
        self.types_scopes.push(vec![]);
        let result = f(self);
        self.types_scopes.pop();
        result
    }

    fn current_type_scope(&self) -> &[Rc<Type>] {
        self.types_scopes.last().unwrap()
    }

    fn current_type_scope_mut(&mut self) -> &mut Vec<Rc<Type>> {
        self.types_scopes.last_mut().unwrap()
    }

    fn outer_types_scope(&self, count: u32) -> &[Rc<Type>] {
        &self.types_scopes[self.types_scopes.len() - 1 - usize::try_from(count).unwrap()]
    }

    fn outer_type(&self, count: u32, i: u32) -> &Rc<Type> {
        &self.outer_types_scope(count)[usize::try_from(i).unwrap()]
    }

    fn arbitrary_component_type(&mut self, u: &mut Unstructured<'a>) -> Result<ComponentType> {
        let mut defs = vec![];
        let mut imports = HashSet::new();
        let mut exports = HashSet::new();

        self.with_types_scope(|me| {
            arbitrary_loop(u, 0, 100, |u| {
                if !me.current_type_scope().is_empty() && u.arbitrary()? {
                    // Imports.
                    let name = crate::unique_string(100, &mut imports, u)?;
                    let ty = u.int_in_range(0..=me.current_type_scope().len() - 1)?;
                    let ty = u32::try_from(ty).unwrap();
                    defs.push(ComponentTypeDef::Import(Import { name, ty }));
                } else {
                    // Instance type definitions.
                    let def = me.arbitrary_instance_type_def(u, &mut exports)?;
                    defs.push(ComponentTypeDef::Instance(def));
                }
                Ok(true)
            })
        })?;

        Ok(ComponentType { defs })
    }

    fn arbitrary_instance_type(&mut self, u: &mut Unstructured<'a>) -> Result<InstanceType> {
        let mut defs = vec![];
        let mut exports = HashSet::new();

        self.with_types_scope(|me| {
            arbitrary_loop(u, 0, 100, |u| {
                defs.push(me.arbitrary_instance_type_def(u, &mut exports)?);
                Ok(true)
            })
        })?;

        Ok(InstanceType { defs })
    }

    fn arbitrary_instance_type_def(
        &mut self,
        u: &mut Unstructured<'a>,
        exports: &mut HashSet<String>,
    ) -> Result<InstanceTypeDef> {
        match u.int_in_range(0..=2)? {
            // Export.
            0 if !self.current_type_scope().is_empty() => Ok(InstanceTypeDef::Export {
                name: crate::unique_string(100, exports, u)?,
                ty: u.int_in_range(
                    0..=u32::try_from(self.current_type_scope().len()).unwrap() - 1,
                )?,
            }),

            // Outer type alias.
            1 if self.types_scopes.iter().any(|scope| !scope.is_empty()) => {
                let alias = self.arbitrary_outer_type_alias(u)?;
                let (count, i) = match alias {
                    Alias::Outer {
                        count,
                        i,
                        kind: OuterAliasKind::Type,
                    } => (count, i),
                    _ => unreachable!(),
                };
                let ty = self.outer_type(count, i).clone();
                self.current_type_scope_mut().push(ty);
                Ok(InstanceTypeDef::Alias(alias))
            }

            // Type definition.
            _ => {
                let ty = self.arbitrary_type(u)?;
                Ok(InstanceTypeDef::Type(ty))
            }
        }
    }

    fn arbitrary_outer_type_alias(&mut self, u: &mut Unstructured<'a>) -> Result<Alias> {
        let non_empty_types_scopes: Vec<_> = self
            .types_scopes
            .iter()
            .rev()
            .enumerate()
            .filter(|(_, scope)| !scope.is_empty())
            .collect();
        assert!(
            !non_empty_types_scopes.is_empty(),
            "precondition: there are non-empty types scopes"
        );

        let (count, scope) = u.choose(&non_empty_types_scopes)?;
        let count = u32::try_from(*count).unwrap();
        assert!(!scope.is_empty());

        let max_type_in_scope = u32::try_from(scope.len() - 1).unwrap();
        let i = u.int_in_range(0..=max_type_in_scope)?;

        Ok(Alias::Outer {
            count,
            i,
            kind: OuterAliasKind::Type,
        })
    }

    fn arbitrary_func_type(&mut self, u: &mut Unstructured<'a>) -> Result<FuncType> {
        let mut params = vec![];
        let mut param_names = HashSet::new();
        arbitrary_loop(u, 0, 20, |u| {
            params.push(self.arbitrary_named_type(u, &mut param_names)?);
            Ok(true)
        })?;

        let result = self.arbitrary_value_type(u)?;

        Ok(FuncType { params, result })
    }

    fn arbitrary_value_type(&mut self, u: &mut Unstructured<'a>) -> Result<ValueType> {
        let num_choices = if self.current_type_scope().is_empty() {
            13
        } else {
            14
        };
        match u.int_in_range(0..=num_choices)? {
            0 => Ok(ValueType::Unit),
            1 => Ok(ValueType::Bool),
            2 => Ok(ValueType::S8),
            3 => Ok(ValueType::U8),
            4 => Ok(ValueType::S16),
            5 => Ok(ValueType::U16),
            6 => Ok(ValueType::S32),
            7 => Ok(ValueType::U32),
            8 => Ok(ValueType::S64),
            9 => Ok(ValueType::U64),
            10 => Ok(ValueType::F32),
            11 => Ok(ValueType::F64),
            12 => Ok(ValueType::Char),
            13 => Ok(ValueType::String),
            14 => {
                let max_type = u32::try_from(self.current_type_scope().len() - 1).unwrap();
                Ok(ValueType::Indexed(u.int_in_range(0..=max_type)?))
            }
            _ => unreachable!(),
        }
    }

    fn arbitrary_named_type(
        &mut self,
        u: &mut Unstructured<'a>,
        names: &mut HashSet<String>,
    ) -> Result<NamedType> {
        let name = crate::unique_string(100, names, u)?;
        let ty = self.arbitrary_value_type(u)?;
        Ok(NamedType { name, ty })
    }

    fn arbitrary_record_type(&mut self, u: &mut Unstructured<'a>) -> Result<RecordType> {
        let mut fields = vec![];
        let mut field_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
            fields.push(self.arbitrary_named_type(u, &mut field_names)?);
            Ok(true)
        })?;
        Ok(RecordType { fields })
    }

    fn arbitrary_variant_type(&mut self, u: &mut Unstructured<'a>) -> Result<VariantType> {
        let mut cases = vec![];
        let mut case_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
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

    fn arbitrary_list_type(&mut self, u: &mut Unstructured<'a>) -> Result<ListType> {
        Ok(ListType {
            elem_ty: self.arbitrary_value_type(u)?,
        })
    }

    fn arbitrary_tuple_type(&mut self, u: &mut Unstructured<'a>) -> Result<TupleType> {
        let mut fields = vec![];
        arbitrary_loop(u, 0, 100, |u| {
            fields.push(self.arbitrary_value_type(u)?);
            Ok(true)
        })?;
        Ok(TupleType { fields })
    }

    fn arbitrary_flags_type(&mut self, u: &mut Unstructured<'a>) -> Result<FlagsType> {
        let mut fields = vec![];
        let mut field_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
            fields.push(crate::unique_string(100, &mut field_names, u)?);
            Ok(true)
        })?;
        Ok(FlagsType { fields })
    }

    fn arbitrary_enum_type(&mut self, u: &mut Unstructured<'a>) -> Result<EnumType> {
        let mut variants = vec![];
        let mut variant_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
            variants.push(crate::unique_string(100, &mut variant_names, u)?);
            Ok(true)
        })?;
        Ok(EnumType { variants })
    }

    fn arbitrary_union_type(&mut self, u: &mut Unstructured<'a>) -> Result<UnionType> {
        let mut variants = vec![];
        let mut variant_names = HashSet::new();
        arbitrary_loop(u, 0, 100, |u| {
            variants.push(crate::unique_string(100, &mut variant_names, u)?);
            Ok(true)
        })?;
        Ok(UnionType { variants })
    }

    fn arbitrary_optional_type(&mut self, u: &mut Unstructured<'a>) -> Result<OptionalType> {
        Ok(OptionalType {
            inner_ty: self.arbitrary_value_type(u)?,
        })
    }

    fn arbitrary_expected_type(&mut self, u: &mut Unstructured<'a>) -> Result<ExpectedType> {
        Ok(ExpectedType {
            ok_ty: self.arbitrary_value_type(u)?,
            err_ty: self.arbitrary_value_type(u)?,
        })
    }

    fn arbitrary_compound_type(&mut self, u: &mut Unstructured<'a>) -> Result<CompoundType> {
        match u.int_in_range(0..=9)? {
            0 => Ok(CompoundType::Record(self.arbitrary_record_type(u)?)),
            1 => Ok(CompoundType::Variant(self.arbitrary_variant_type(u)?)),
            2 => Ok(CompoundType::List(self.arbitrary_list_type(u)?)),
            3 => Ok(CompoundType::Tuple(self.arbitrary_tuple_type(u)?)),
            4 => Ok(CompoundType::Flags(self.arbitrary_flags_type(u)?)),
            5 => Ok(CompoundType::Enum(self.arbitrary_enum_type(u)?)),
            6 => Ok(CompoundType::Union(self.arbitrary_union_type(u)?)),
            7 => Ok(CompoundType::Optional(self.arbitrary_optional_type(u)?)),
            8 => Ok(CompoundType::Expected(self.arbitrary_expected_type(u)?)),
            9 => Ok(CompoundType::Named(
                self.arbitrary_named_type(u, &mut HashSet::new())?,
            )),
            _ => unreachable!(),
        }
    }

    fn arbitrary_import_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        todo!()
    }

    fn arbitrary_func_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        todo!()
    }

    fn arbitrary_core_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        todo!()
    }

    fn arbitrary_component_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        todo!()
    }

    fn arbitrary_instance_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        todo!()
    }

    fn arbitrary_export_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        todo!()
    }

    fn arbitrary_start_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        todo!()
    }

    fn arbitrary_alias_section(&mut self, u: &mut Unstructured<'a>) -> Result<()> {
        todo!()
    }
}

#[derive(Debug)]
enum Section<'a> {
    Custom(CustomSection<'a>),
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
struct CustomSection<'a> {
    name: &'a str,
    data: &'a [u8],
}

impl<'a> Arbitrary<'a> for CustomSection<'a> {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let name = limited_str(1_000, u)?;
        let data = u.arbitrary()?;
        Ok(CustomSection { name, data })
    }
}

#[derive(Debug)]
struct TypeSection {
    types: Vec<Type>,
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
    Instance(InstanceTypeDef),
    Import(Import),
}

#[derive(Clone, Debug)]
struct InstanceType {
    defs: Vec<InstanceTypeDef>,
}

#[derive(Clone, Debug)]
enum InstanceTypeDef {
    Type(Type),
    Export { name: String, ty: u32 },
    Alias(Alias),
}

#[derive(Clone, Debug)]
struct FuncType {
    params: Vec<NamedType>,
    result: ValueType,
}

#[derive(Clone, Debug)]
struct NamedType {
    name: String,
    ty: ValueType,
}

#[derive(Clone, Debug)]
enum ValueType {
    Indexed(u32),
    Unit,
    Bool,
    S8,
    U8,
    S16,
    U16,
    S32,
    U32,
    S64,
    U64,
    F32,
    F64,
    Char,
    String,
}

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
    Named(NamedType),
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
    elem_ty: ValueType,
}

#[derive(Clone, Debug)]
struct TupleType {
    fields: Vec<ValueType>,
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
    variants: Vec<String>,
}

#[derive(Clone, Debug)]
struct OptionalType {
    inner_ty: ValueType,
}

#[derive(Clone, Debug)]
struct ExpectedType {
    ok_ty: ValueType,
    err_ty: ValueType,
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
