use crate::component::*;
use crate::core;
use crate::kw;
use crate::names::Namespace;
use crate::token::{Id, Index, ItemRef};
use crate::Error;
use std::convert::TryInto;
use std::mem::replace;

/// Resolve the fields of a component and everything nested within it, changing
/// `Index::Id` to `Index::Num` and expanding alias syntax sugar.
pub fn resolve(component: &mut Component<'_>) -> Result<(), Error> {
    let fields = match &mut component.kind {
        ComponentKind::Text(fields) => fields,
        ComponentKind::Binary(_) => return Ok(()),
    };
    let mut resolve_stack = Vec::new();
    resolve_fields(component.id, fields, &mut resolve_stack)
}

fn resolve_fields<'a, 'b>(
    id: Option<Id<'a>>,
    fields: &mut Vec<ComponentField<'a>>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    resolve_stack.push(ComponentResolver::new(id));

    // Iterate through the fields of the component. We use an index
    // instead of an iterator because we'll be inserting aliases
    // as we go.
    let mut i = 0;
    while i < fields.len() {
        // Resolve names within the field.
        resolve_field(&mut fields[i], resolve_stack)?;

        // Name resolution may have emitted some aliases. Insert them before
        // the current definition.
        let resolver = resolve_stack.last_mut().unwrap();
        let aliases_to_insert = replace(&mut resolver.aliases_to_insert, Vec::new());
        for alias in aliases_to_insert {
            fields.insert(i, ComponentField::Alias(alias));
            i += 1;
        }

        // Definitions can't refer to themselves or to definitions that appear
        // later in the format. Now that we're done resolving this field,
        // assign it an index for later defintions to refer to.
        register(&mut fields[i], resolver)?;

        i += 1;
    }

    resolve_stack.pop();
    Ok(())
}

/// Assign an index to the given field.
fn register<'a, 'b>(
    item: &ComponentField<'a>,
    resolver: &'b mut ComponentResolver<'a>,
) -> Result<(), Error> {
    match item {
        ComponentField::Import(i) => match &i.type_ {
            ComponentTypeUse::Inline(inline) => match inline {
                DefType::Func(f) => {
                    resolver.funcs.register(f.id, "component func")?;
                }
                DefType::Module(m) => {
                    resolver.modules.register(m.id, "module")?;
                }
                DefType::Component(c) => {
                    resolver.components.register(c.id, "component")?;
                }
                DefType::Instance(i) => {
                    resolver.instances.register(i.id, "instance")?;
                }
                DefType::Value(v) => {
                    resolver.values.register(v.id, "value")?;
                }
            },
            ComponentTypeUse::Ref(index) => match index.idx {
                Index::Num(_, _) => {}
                Index::Id(name) => {
                    return Err(Error::new(
                        name.span(),
                        "TODO: import with a name".to_string(),
                    ))
                }
            },
        },

        ComponentField::Func(i) => {
            resolver.funcs.register(i.id, "func")?;
        }
        ComponentField::Type(i) => {
            resolver.types.register(i.id, "type")?;
        }
        ComponentField::Instance(i) => {
            resolver.instances.register(i.id, "instance")?;
        }
        ComponentField::Module(m) => {
            resolver.modules.register(m.id, "nested module")?;
        }
        ComponentField::Component(c) => {
            resolver.components.register(c.id, "nested component")?;
        }
        ComponentField::Alias(a) => {
            register_alias(a, resolver)?;
        }

        // These fields don't define any items in any index space.
        ComponentField::Export(_) | ComponentField::Start(_) => return Ok(()),
    };

    Ok(())
}

fn resolve_field<'a, 'b>(
    field: &mut ComponentField<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match field {
        ComponentField::Import(i) => {
            match &mut i.type_ {
                ComponentTypeUse::Inline(i) => resolve_deftype(i, resolve_stack)?,
                ComponentTypeUse::Ref(r) => resolve_item_ref(r, resolve_stack)?,
            }
            Ok(())
        }

        ComponentField::Type(t) => resolve_type_def(&mut t.def, resolve_stack),

        ComponentField::Func(_f) => Ok(()),

        ComponentField::Instance(i) => match &mut i.kind {
            InstanceKind::Module { module, args } => {
                resolve_item_ref(module, resolve_stack)?;
                for arg in args {
                    match &mut arg.arg {
                        ModuleArg::Def(def) => {
                            resolve_item_ref(def, resolve_stack)?;
                        }
                        ModuleArg::BundleOfExports(exports) => {
                            for export in exports {
                                resolve_item_ref(&mut export.index, resolve_stack)?;
                            }
                        }
                    }
                }
                Ok(())
            }
            InstanceKind::Component { component, args } => {
                resolve_item_ref(component, resolve_stack)?;
                for arg in args {
                    match &mut arg.arg {
                        ComponentArg::Def(def) => {
                            resolve_item_ref(def, resolve_stack)?;
                        }
                        ComponentArg::Type(ty) => {
                            resolve_item_ref(ty, resolve_stack)?;
                        }
                        ComponentArg::BundleOfExports(exports) => {
                            for export in exports {
                                resolve_arg(&mut export.arg, resolve_stack)?;
                            }
                        }
                    }
                }
                Ok(())
            }
            InstanceKind::BundleOfExports { args } => {
                for arg in args {
                    resolve_item_ref(&mut arg.index, resolve_stack)?;
                }
                Ok(())
            }
            InstanceKind::BundleOfComponentExports { args } => {
                for arg in args {
                    resolve_arg(&mut arg.arg, resolve_stack)?;
                }
                Ok(())
            }
        },
        ComponentField::Module(m) => {
            match &mut m.kind {
                ModuleKind::Inline { fields } => {
                    crate::core::resolve::resolve(fields)?;
                }

                // For `Import`, just resolve the type.
                ModuleKind::Import { import: _, ty } => {
                    resolve_moduletype_use(ty, resolve_stack)?;
                }
            }

            Ok(())
        }
        ComponentField::Component(c) => match &mut c.kind {
            ComponentKind::Binary(_) => Ok(()),
            ComponentKind::Text(fields) => resolve_fields(c.id, fields, resolve_stack),
        },
        ComponentField::Alias(a) => resolve_alias(a, resolve_stack),

        ComponentField::Start(_i) => Ok(()),

        ComponentField::Export(e) => {
            resolve_arg(&mut e.arg, resolve_stack)?;
            Ok(())
        }
    }
}

fn resolve_alias<'a, 'b>(
    alias: &'b mut Alias<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match &mut alias.target {
        AliasTarget::Export {
            instance,
            export: _,
        } => resolve_ns(instance, Ns::Instance, resolve_stack),
        AliasTarget::Outer { outer, index } => {
            // Resolve `outer`, and compute the depth at which to look up
            // `index`.
            let depth = match outer {
                Index::Id(id) => {
                    let mut depth = 0;
                    for resolver in resolve_stack.iter_mut().rev() {
                        if resolver.id == Some(*id) {
                            break;
                        }
                        depth += 1;
                    }
                    if depth as usize == resolve_stack.len() {
                        return Err(Error::new(
                            alias.span,
                            format!("outer component '{:?}' not found", id),
                        ));
                    }
                    depth
                }
                Index::Num(n, _span) => *n,
            };
            *outer = Index::Num(depth, alias.span);

            // Resolve `index` within the computed scope depth.
            let ns = match alias.kind {
                AliasKind::Module => Ns::Module,
                AliasKind::Component => Ns::Component,
                AliasKind::Instance => Ns::Instance,
                AliasKind::Value => Ns::Value,
                AliasKind::ExportKind(kind) => kind.into(),
            };
            let computed = resolve_stack.len() - 1 - depth as usize;
            resolve_stack[computed].resolve(ns, index)?;

            Ok(())
        }
    }
}

fn resolve_arg<'a, 'b>(
    arg: &'b mut ComponentArg<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match arg {
        ComponentArg::Def(item_ref) => {
            resolve_item_ref(item_ref, resolve_stack)?;
        }
        ComponentArg::Type(item_ref) => {
            resolve_item_ref(item_ref, resolve_stack)?;
        }
        ComponentArg::BundleOfExports(exports) => {
            for export in exports {
                resolve_arg(&mut export.arg, resolve_stack)?;
            }
        }
    }
    Ok(())
}

fn resolve_deftype_use<'a, 'b>(
    ty: &'b mut ComponentTypeUse<'a, DefType<'a>>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match ty {
        ComponentTypeUse::Ref(r) => resolve_item_ref(r, resolve_stack),
        ComponentTypeUse::Inline(i) => resolve_deftype(i, resolve_stack),
    }
}

fn resolve_deftype<'a, 'b>(
    ty: &'b mut DefType<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match ty {
        DefType::Func(f) => {
            for param in f.params.iter_mut() {
                resolve_intertype_use(&mut param.type_, resolve_stack)?;
            }
            resolve_intertype_use(&mut f.result, resolve_stack)
        }
        DefType::Module(m) => Err(Error::new(
            m.span,
            "TODO: resolve for module types".to_string(),
        )),
        DefType::Component(c) => resolve_nested_component_type(c, resolve_stack),
        DefType::Instance(i) => resolve_instance_type(i, resolve_stack),
        DefType::Value(v) => resolve_intertype_use(&mut v.value_type, resolve_stack),
    }
}

fn resolve_moduletype_use<'a, 'b>(
    ty: &'b mut ComponentTypeUse<'a, ModuleType<'a>>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match ty {
        ComponentTypeUse::Ref(r) => resolve_item_ref(r, resolve_stack),
        ComponentTypeUse::Inline(_) => Ok(()),
    }
}

fn resolve_intertype_use<'a, 'b>(
    ty: &'b mut ComponentTypeUse<'a, InterType<'a>>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match ty {
        ComponentTypeUse::Ref(r) => resolve_item_ref(r, resolve_stack),
        ComponentTypeUse::Inline(i) => resolve_intertype(i, resolve_stack),
    }
}

fn resolve_intertype<'a, 'b>(
    ty: &'b mut InterType<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match ty {
        InterType::Unit => {}
        InterType::Bool => {}
        InterType::U8 => {}
        InterType::S8 => {}
        InterType::U16 => {}
        InterType::S16 => {}
        InterType::U32 => {}
        InterType::S32 => {}
        InterType::U64 => {}
        InterType::S64 => {}
        InterType::Float32 => {}
        InterType::Float64 => {}
        InterType::String => {}
        InterType::Char => {}
        InterType::Flags(_) => {}
        InterType::Enum(_) => {}
        InterType::Record(r) => {
            for field in r.fields.iter_mut() {
                resolve_intertype_use(&mut field.type_, resolve_stack)?;
            }
        }
        InterType::Variant(v) => {
            for case in v.cases.iter_mut() {
                resolve_intertype_use(&mut case.type_, resolve_stack)?;
            }

            // Resolve the `defaults-to` field. Use indices instead
            // of iterators to avoid borrowing `v.cases`, as we
            // also need to iterate through it to do the
            // resolution.
            for i in 0..v.cases.len() {
                if let Some(index) = v.cases[i].defaults_to {
                    if let Index::Id(id) = index {
                        for other_i in 0..v.cases.len() {
                            let other = &v.cases[other_i];
                            if other.name == id {
                                v.cases[i].defaults_to = Some(Index::Num(
                                    i.try_into().map_err(|_| {
                                        Error::new(v.cases[i].span, format!("too many cases"))
                                    })?,
                                    other.span,
                                ));
                            }
                        }
                    }
                }
            }
        }
        InterType::List(l) => {
            resolve_intertype_use(&mut *l.element, resolve_stack)?;
        }
        InterType::Tuple(t) => {
            for field in t.fields.iter_mut() {
                resolve_intertype_use(field, resolve_stack)?;
            }
        }
        InterType::Union(t) => {
            for arm in t.arms.iter_mut() {
                resolve_intertype_use(arm, resolve_stack)?;
            }
        }
        InterType::Option(o) => {
            resolve_intertype_use(&mut *o.element, resolve_stack)?;
        }
        InterType::Expected(r) => {
            resolve_intertype_use(&mut *r.ok, resolve_stack)?;
            resolve_intertype_use(&mut *r.err, resolve_stack)?;
        }
    }
    Ok(())
}

fn resolve_type_def<'a, 'b>(
    def: &'b mut ComponentTypeDef<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match def {
        ComponentTypeDef::DefType(d) => resolve_deftype(d, resolve_stack),
        ComponentTypeDef::InterType(i) => resolve_intertype(i, resolve_stack),
    }
}

fn resolve_nested_component_type<'a, 'b>(
    c: &'b mut ComponentType<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    resolve_stack.push(ComponentResolver::new(c.id));

    // Iterate through the fields of the component type. We use an index
    // instead of an iterator because we'll be inserting aliases
    // as we go.
    let mut i = 0;
    while i < c.fields.len() {
        // Resolve names within the field.
        match &mut c.fields[i] {
            ComponentTypeField::Alias(alias) => {
                resolve_alias(alias, resolve_stack)?;
            }
            ComponentTypeField::Type(ty) => {
                resolve_type_def(&mut ty.def, resolve_stack)?;
            }
            ComponentTypeField::Import(import) => {
                resolve_deftype_use(&mut import.type_, resolve_stack)?;
            }
            ComponentTypeField::Export(export) => {
                resolve_deftype_use(&mut export.item, resolve_stack)?;
            }
        }

        // Name resolution may have emitted some aliases. Insert them before
        // the current definition.
        let resolver = resolve_stack.last_mut().unwrap();
        let aliases_to_insert = replace(&mut resolver.aliases_to_insert, Vec::new());
        for alias in aliases_to_insert {
            c.fields.insert(i, ComponentTypeField::Alias(alias));
            i += 1;
        }

        // Definitions can't refer to themselves or to definitions that appear
        // later in the format. Now that we're done resolving this field,
        // assign it an index for later defintions to refer to.
        match &mut c.fields[i] {
            ComponentTypeField::Alias(alias) => {
                register_alias(alias, resolver)?;
            }
            ComponentTypeField::Type(ty) => {
                resolver.types.register(ty.id, "type")?;
            }
            ComponentTypeField::Import(import) => match &import.type_ {
                ComponentTypeUse::Inline(inline) => match inline {
                    DefType::Func(f) => {
                        resolver.funcs.register(f.id, "component func")?;
                    }
                    DefType::Module(m) => {
                        resolver.modules.register(m.id, "module")?;
                    }
                    DefType::Component(c) => {
                        resolver.components.register(c.id, "component")?;
                    }
                    DefType::Instance(i) => {
                        resolver.instances.register(i.id, "instance")?;
                    }
                    DefType::Value(v) => {
                        resolver.values.register(v.id, "value")?;
                    }
                },
                ComponentTypeUse::Ref(_index) => {}
            },
            ComponentTypeField::Export(_export) => {}
        }
        i += 1;
    }

    resolve_stack.pop();
    Ok(())
}

fn resolve_instance_type<'a, 'b>(
    c: &'b mut InstanceType<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    resolve_stack.push(ComponentResolver::new(c.id));

    // Iterate through the fields of the component type. We use an index
    // instead of an iterator because we'll be inserting aliases
    // as we go.
    let mut i = 0;
    while i < c.fields.len() {
        // Resolve names within the field.
        match &mut c.fields[i] {
            InstanceTypeField::Alias(alias) => {
                resolve_alias(alias, resolve_stack)?;
            }
            InstanceTypeField::Type(ty) => {
                resolve_type_def(&mut ty.def, resolve_stack)?;
            }
            InstanceTypeField::Export(export) => {
                resolve_deftype_use(&mut export.item, resolve_stack)?;
            }
        }

        // Name resolution may have emitted some aliases. Insert them before
        // the current definition.
        let resolver = resolve_stack.last_mut().unwrap();
        let aliases_to_insert = replace(&mut resolver.aliases_to_insert, Vec::new());
        for alias in aliases_to_insert {
            c.fields.insert(i, InstanceTypeField::Alias(alias));
            i += 1;
        }

        // Definitions can't refer to themselves or to definitions that appear
        // later in the format. Now that we're done resolving this field,
        // assign it an index for later defintions to refer to.
        match &mut c.fields[i] {
            InstanceTypeField::Alias(alias) => {
                register_alias(alias, resolver)?;
            }
            InstanceTypeField::Type(ty) => {
                resolver.types.register(ty.id, "type")?;
            }
            InstanceTypeField::Export(_export) => {}
        }
        i += 1;
    }

    resolve_stack.pop();
    Ok(())
}

fn register_alias<'a, 'b>(
    alias: &'b Alias<'a>,
    resolver: &'b mut ComponentResolver<'a>,
) -> Result<u32, Error> {
    match alias.kind {
        AliasKind::Module => resolver.modules.register(alias.id, "module"),
        AliasKind::Component => resolver.components.register(alias.id, "component"),
        AliasKind::Instance => resolver.instances.register(alias.id, "instance"),
        AliasKind::Value => resolver.values.register(alias.id, "value"),
        AliasKind::ExportKind(core::ExportKind::Func) => resolver.funcs.register(alias.id, "func"),
        AliasKind::ExportKind(core::ExportKind::Table) => {
            resolver.tables.register(alias.id, "table")
        }
        AliasKind::ExportKind(core::ExportKind::Memory) => {
            resolver.memories.register(alias.id, "memory")
        }
        AliasKind::ExportKind(core::ExportKind::Global) => {
            resolver.globals.register(alias.id, "global")
        }
        AliasKind::ExportKind(core::ExportKind::Tag) => resolver.tags.register(alias.id, "tag"),
        AliasKind::ExportKind(core::ExportKind::Type) => resolver.types.register(alias.id, "type"),
    }
}

fn resolve_item_ref<'a, 'b, K>(
    item: &'b mut ItemRef<'a, K>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error>
where
    K: Into<Ns> + Copy,
{
    // TODO: `item` shoudl have an `extra_names` field which are resolved here
    // which uncomments a bunch of code below.
    // #[cfg(wast_check_exhaustive)]
    // {
    //     if !item.visited {
    //         return Err(Error::new(
    //             item.idx.span(),
    //             format!("BUG: this index wasn't visited"),
    //         ));
    //     }
    // }

    let last_ns = item.kind.into();
    // if item.extra_names.is_empty() {
    resolve_ns(&mut item.idx, last_ns, resolve_stack)?;
    Ok(())
    // return Ok(());
    // }

    // // We have extra export names. This is syntax sugar for inserting export
    // // aliases.

    // // The index is an instance index, rather than the namespace of the
    // // reference.
    // resolve_ns(&mut item.idx, Ns::Instance, resolve_stack)?;

    // // The extra names are export names. Copy them into export aliases.
    // let span = item.idx.span();
    // let mut index = item.idx;
    // for (pos, export_name) in item.extra_names.iter().enumerate() {
    // // The last name is in the namespace of the reference. All others are
    // // instances.
    // let ns = if pos == item.extra_names.len() - 1 {
    //     last_ns
    // } else {
    //     Ns::Instance
    // };

    // // Record an outer alias to be inserted in front of the current
    // // definition.
    // let mut alias = Alias {
    //     span,
    //     id: None,
    //     name: None,
    //     target: AliasTarget::Export {
    //         instance: index,
    //         export: export_name,
    //     },
    //     kind: match ns {
    //         Ns::Module => AliasKind::Module,
    //         Ns::Component => AliasKind::Component,
    //         Ns::Instance => AliasKind::Instance,
    //         Ns::Value => AliasKind::Value,
    //         Ns::Func => AliasKind::ExportKind(core::ExportKind::Func),
    //         Ns::Table => AliasKind::ExportKind(core::ExportKind::Table),
    //         Ns::Global => AliasKind::ExportKind(core::ExportKind::Global),
    //         Ns::Memory => AliasKind::ExportKind(core::ExportKind::Memory),
    //         Ns::Tag => AliasKind::ExportKind(core::ExportKind::Tag),
    //         Ns::Type => AliasKind::ExportKind(core::ExportKind::Type),
    //     },
    // };

    // let resolver = resolve_stack.last_mut().unwrap();
    // index = Index::Num(register_alias(&mut alias, resolver)?, span);
    // resolver.aliases_to_insert.push(alias);
    // }

    // Ok(())
}

fn resolve_ns<'a, 'b>(
    idx: &mut Index<'a>,
    ns: Ns,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    let resolver = resolve_stack.last_mut().unwrap();

    // First look in the current top scope.
    let result = resolver.resolve(ns, idx);

    // If we found it there, we're good.
    if let Ok(_idx) = result {
        return Ok(());
    }

    // If not, this might be syntax sugar for a reference to an outer scope.
    let mut depth = 0;
    let mut found = 0;
    for resolver in resolve_stack.iter_mut().rev() {
        if let Ok(resolved) = resolver.resolve(ns, idx) {
            found = resolved;
            break;
        }
        depth += 1;
    }
    if depth as usize == resolve_stack.len() {
        return Err(Error::new(
            idx.span(),
            "TODO: name resolution failed".to_string(),
        ));
    }

    // Record an outer alias to be inserted in front of the current definition.
    let span = idx.span();
    let mut alias = Alias {
        span,
        id: None,
        name: None,
        target: AliasTarget::Outer {
            outer: Index::Num(depth, span),
            index: Index::Num(found, span),
        },
        kind: match ns {
            Ns::Module => AliasKind::Module,
            Ns::Component => AliasKind::Component,
            Ns::Instance => AliasKind::Instance,
            Ns::Value => AliasKind::Value,
            Ns::Func => AliasKind::ExportKind(core::ExportKind::Func),
            Ns::Table => AliasKind::ExportKind(core::ExportKind::Table),
            Ns::Global => AliasKind::ExportKind(core::ExportKind::Global),
            Ns::Memory => AliasKind::ExportKind(core::ExportKind::Memory),
            Ns::Tag => AliasKind::ExportKind(core::ExportKind::Tag),
            Ns::Type => AliasKind::ExportKind(core::ExportKind::Type),
        },
    };

    let resolver = resolve_stack.last_mut().unwrap();
    register_alias(&mut alias, resolver)?;
    resolver.aliases_to_insert.push(alias);

    Ok(())
}

/// Context structure used to perform name resolution.
#[derive(Default)]
struct ComponentResolver<'a> {
    id: Option<Id<'a>>,

    // Namespaces within each componnet. Note that each namespace carries
    // with it information about the signature of the item in that namespace.
    // The signature is later used to synthesize the type of a component and
    // inject type annotations if necessary.
    funcs: Namespace<'a>,
    globals: Namespace<'a>,
    tables: Namespace<'a>,
    memories: Namespace<'a>,
    types: Namespace<'a>,
    tags: Namespace<'a>,
    instances: Namespace<'a>,
    modules: Namespace<'a>,
    components: Namespace<'a>,
    values: Namespace<'a>,

    // When a name refers to a definition in an outer scope, we'll need to
    // insert an outer alias before it. This collects the aliases to be
    // inserted during resolution.
    aliases_to_insert: Vec<Alias<'a>>,
}

impl<'a> ComponentResolver<'a> {
    fn new(id: Option<Id<'a>>) -> Self {
        Self {
            id,
            funcs: Default::default(),
            globals: Default::default(),
            tables: Default::default(),
            memories: Default::default(),
            types: Default::default(),
            tags: Default::default(),
            instances: Default::default(),
            modules: Default::default(),
            components: Default::default(),
            values: Default::default(),
            aliases_to_insert: Vec::new(),
        }
    }
}

impl<'a> ComponentResolver<'a> {
    fn resolve(&mut self, ns: Ns, idx: &mut Index<'a>) -> Result<u32, Error> {
        match ns {
            Ns::Func => self.funcs.resolve(idx, "func"),
            Ns::Table => self.tables.resolve(idx, "table"),
            Ns::Global => self.globals.resolve(idx, "global"),
            Ns::Memory => self.memories.resolve(idx, "memory"),
            Ns::Tag => self.tags.resolve(idx, "tag"),
            Ns::Type => self.types.resolve(idx, "type"),
            Ns::Component => self.components.resolve(idx, "component"),
            Ns::Module => self.modules.resolve(idx, "module"),
            Ns::Instance => self.instances.resolve(idx, "instance"),
            Ns::Value => self.values.resolve(idx, "instance"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
enum Ns {
    Func,
    Table,
    Global,
    Memory,
    Tag,
    Type,
    Component,
    Module,
    Instance,
    Value,
}

macro_rules! component_kw_conversions {
    ($($kw:ident => $kind:ident)*) => ($(
        impl From<kw::$kw> for Ns {
            fn from(_: kw::$kw) -> Ns {
                Ns::$kind
            }
        }
    )*);
}

component_kw_conversions! {
    func => Func
    module => Module
    component => Component
    instance => Instance
    value => Value
    table => Table
    memory => Memory
    global => Global
    tag => Tag
    r#type => Type
}

impl From<DefTypeKind> for Ns {
    fn from(kind: DefTypeKind) -> Self {
        match kind {
            DefTypeKind::Module => Ns::Module,
            DefTypeKind::Component => Ns::Component,
            DefTypeKind::Instance => Ns::Instance,
            DefTypeKind::Value => Ns::Value,
            DefTypeKind::Func => Ns::Func,
        }
    }
}

impl From<core::ExportKind> for Ns {
    fn from(kind: core::ExportKind) -> Self {
        match kind {
            core::ExportKind::Func => Ns::Func,
            core::ExportKind::Table => Ns::Table,
            core::ExportKind::Global => Ns::Global,
            core::ExportKind::Memory => Ns::Memory,
            core::ExportKind::Tag => Ns::Tag,
            core::ExportKind::Type => Ns::Type,
        }
    }
}
