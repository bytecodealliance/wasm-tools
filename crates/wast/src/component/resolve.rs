use crate::component::*;
use crate::core;
use crate::kw;
use crate::names::Namespace;
use crate::token::{Id, Index};
use crate::Error;
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
        ComponentField::Import(i) => match &i.item.kind {
            ItemKind::Module(_) => resolver.modules.register(i.item.id, "module")?,
            ItemKind::Component(_) => resolver.components.register(i.item.id, "component")?,
            ItemKind::Instance(_) => resolver.instances.register(i.item.id, "instance")?,
            ItemKind::Value(_) => resolver.values.register(i.item.id, "value")?,
            ItemKind::Func(_) => resolver.funcs.register(i.item.id, "func")?,
        },

        ComponentField::Func(i) => resolver.funcs.register(i.id, "func")?,
        ComponentField::Type(i) => resolver.types.register(i.id, "type")?,
        ComponentField::Instance(i) => resolver.instances.register(i.id, "instance")?,
        ComponentField::Module(m) => resolver.modules.register(m.id, "nested module")?,
        ComponentField::Component(c) => resolver.components.register(c.id, "nested component")?,
        ComponentField::Alias(a) => register_alias(a, resolver)?,
        ComponentField::Start(s) => resolver.values.register(s.result, "value")?,

        // These fields don't define any items in any index space.
        ComponentField::Export(_) => return Ok(()),
    };

    Ok(())
}

fn resolve_field<'a, 'b>(
    field: &mut ComponentField<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match field {
        ComponentField::Import(i) => resolve_item_sig(&mut i.item, resolve_stack),

        ComponentField::Type(t) => resolve_type_field(t, resolve_stack),

        ComponentField::Func(f) => {
            let body = match &mut f.kind {
                ComponentFuncKind::Import { .. } => return Ok(()),
                ComponentFuncKind::Inline { body } => body,
            };
            let opts = match body {
                ComponentFuncBody::CanonLift(lift) => {
                    resolve_type_use(&mut lift.type_, resolve_stack)?;
                    resolve_item_ref(&mut lift.func, resolve_stack)?;
                    &mut lift.opts
                }
                ComponentFuncBody::CanonLower(lower) => {
                    resolve_item_ref(&mut lower.func, resolve_stack)?;
                    &mut lower.opts
                }
            };
            for opt in opts {
                match opt {
                    CanonOpt::StringUtf8 | CanonOpt::StringUtf16 | CanonOpt::StringLatin1Utf16 => {}
                    CanonOpt::Into(instance) => {
                        resolve_item_ref(instance, resolve_stack)?;
                    }
                }
            }
            Ok(())
        }

        ComponentField::Instance(i) => match &mut i.kind {
            InstanceKind::Module { module, args } => {
                resolve_item_ref(module, resolve_stack)?;
                for arg in args {
                    match &mut arg.arg {
                        ModuleArg::Def(def) => {
                            resolve_item_ref(def, resolve_stack)?;
                        }
                        ModuleArg::BundleOfExports(..) => {
                            unreachable!("should be expanded already");
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
                        ComponentArg::BundleOfExports(..) => {
                            unreachable!("should be expanded already")
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
            InstanceKind::Import { .. } => {
                unreachable!("should be removed by expansion")
            }
        },
        ComponentField::Module(m) => {
            match &mut m.kind {
                ModuleKind::Inline { fields } => {
                    crate::core::resolve::resolve(fields)?;
                }

                ModuleKind::Import { .. } => {
                    unreachable!("should be expanded already")
                }
            }

            Ok(())
        }
        ComponentField::Component(c) => match &mut c.kind {
            NestedComponentKind::Import { .. } => {
                unreachable!("should be expanded already")
            }
            NestedComponentKind::Inline(fields) => resolve_fields(c.id, fields, resolve_stack),
        },
        ComponentField::Alias(a) => resolve_alias(a, resolve_stack),

        ComponentField::Start(s) => {
            resolve_item_ref(&mut s.func, resolve_stack)?;
            for arg in s.args.iter_mut() {
                resolve_item_ref(arg, resolve_stack)?;
            }
            Ok(())
        }

        ComponentField::Export(e) => {
            resolve_arg(&mut e.arg, resolve_stack)?;
            Ok(())
        }
    }
}

fn resolve_item_sig<'a>(
    sig: &mut ItemSig<'a>,
    resolve_stack: &mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match &mut sig.kind {
        ItemKind::Component(t) => resolve_type_use(t, resolve_stack),
        ItemKind::Module(t) => resolve_type_use(t, resolve_stack),
        ItemKind::Instance(t) => resolve_type_use(t, resolve_stack),
        ItemKind::Func(t) => resolve_type_use(t, resolve_stack),
        ItemKind::Value(t) => resolve_type_use(t, resolve_stack),
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
            // Short-circuit when both indices are already resolved as this
            // helps to write tests for invalid modules where wasmparser should
            // be the one returning the error.
            if let Index::Num(..) = outer {
                if let Index::Num(..) = index {
                    return Ok(());
                }
            }

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
                            format!("outer component `{}` not found", id.name()),
                        ));
                    }
                    depth
                }
                Index::Num(n, _span) => *n,
            };
            *outer = Index::Num(depth, alias.span);
            if depth as usize >= resolve_stack.len() {
                return Err(Error::new(
                    alias.span,
                    format!("component depth of `{}` is too large", depth),
                ));
            }

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
        ComponentArg::BundleOfExports(..) => unreachable!("should be expanded already"),
    }
    Ok(())
}

fn resolve_type_use<'a, T>(
    ty: &mut ComponentTypeUse<'a, T>,
    resolve_stack: &mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    let item = match ty {
        ComponentTypeUse::Ref(r) => r,
        ComponentTypeUse::Inline(_) => unreachable!("inline type-use should be expanded by now"),
    };
    resolve_item_ref(item, resolve_stack)
}

fn resolve_intertype<'a, 'b>(
    ty: &'b mut InterType<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match ty {
        InterType::Primitive(_) => {}
        InterType::Flags(_) => {}
        InterType::Enum(_) => {}
        InterType::Record(r) => {
            for field in r.fields.iter_mut() {
                resolve_intertype_ref(&mut field.type_, resolve_stack)?;
            }
        }
        InterType::Variant(v) => {
            for case in v.cases.iter_mut() {
                resolve_intertype_ref(&mut case.type_, resolve_stack)?;
            }
        }
        InterType::List(l) => {
            resolve_intertype_ref(&mut *l.element, resolve_stack)?;
        }
        InterType::Tuple(t) => {
            for field in t.fields.iter_mut() {
                resolve_intertype_ref(field, resolve_stack)?;
            }
        }
        InterType::Union(t) => {
            for arm in t.arms.iter_mut() {
                resolve_intertype_ref(arm, resolve_stack)?;
            }
        }
        InterType::Option(o) => {
            resolve_intertype_ref(&mut *o.element, resolve_stack)?;
        }
        InterType::Expected(r) => {
            resolve_intertype_ref(&mut *r.ok, resolve_stack)?;
            resolve_intertype_ref(&mut *r.err, resolve_stack)?;
        }
    }
    Ok(())
}

fn resolve_intertype_ref<'a, 'b>(
    ty: &'b mut InterTypeRef<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match ty {
        InterTypeRef::Primitive(_) => Ok(()),
        InterTypeRef::Ref(idx) => resolve_ns(idx, Ns::Type, resolve_stack),
        InterTypeRef::Inline(_) => unreachable!("should be expanded by now"),
    }
}

fn resolve_type_field<'a>(
    field: &mut TypeField<'a>,
    resolve_stack: &mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match &mut field.def {
        ComponentTypeDef::DefType(DefType::Func(f)) => {
            for param in f.params.iter_mut() {
                resolve_intertype_ref(&mut param.type_, resolve_stack)?;
            }
            resolve_intertype_ref(&mut f.result, resolve_stack)?;
        }
        ComponentTypeDef::DefType(DefType::Module(m)) => {
            resolve_stack.push(ComponentResolver::new(field.id));
            resolve_moduletype(m)?;
            resolve_stack.pop();
        }
        ComponentTypeDef::DefType(DefType::Component(c)) => {
            resolve_stack.push(ComponentResolver::new(field.id));
            resolve_nested_component_type(c, resolve_stack)?;
            resolve_stack.pop();
        }
        ComponentTypeDef::DefType(DefType::Instance(i)) => {
            resolve_stack.push(ComponentResolver::new(field.id));
            resolve_instance_type(i, resolve_stack)?;
            resolve_stack.pop();
        }
        ComponentTypeDef::DefType(DefType::Value(v)) => {
            resolve_intertype_ref(&mut v.value_type, resolve_stack)?
        }
        ComponentTypeDef::InterType(i) => resolve_intertype(i, resolve_stack)?,
    }
    Ok(())
}

fn resolve_nested_component_type<'a, 'b>(
    c: &'b mut ComponentType<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
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
                resolve_type_field(ty, resolve_stack)?;
            }
            ComponentTypeField::Import(import) => {
                resolve_item_sig(&mut import.item, resolve_stack)?;
            }
            ComponentTypeField::Export(export) => {
                resolve_item_sig(&mut export.item, resolve_stack)?;
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

            // Only the type namespace is populated within the component type
            // namespace so these are ignored here.
            ComponentTypeField::Import(_) | ComponentTypeField::Export(_) => {}
        }
        i += 1;
    }
    Ok(())
}

fn resolve_instance_type<'a, 'b>(
    c: &'b mut InstanceType<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
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
                resolve_type_field(ty, resolve_stack)?;
            }
            InstanceTypeField::Export(export) => {
                resolve_item_sig(&mut export.item, resolve_stack)?;
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
    let last_ns = item.kind.into();

    // If there are no extra `export_names` listed then this is a reference to
    // something defined within this component's index space, so resolve as
    // necessary.
    if item.export_names.is_empty() {
        resolve_ns(&mut item.idx, last_ns, resolve_stack)?;
        return Ok(());
    }

    // ... otherwise the `index` of `item` refers to an intance and the
    // `export_names` refer to recursive exports from this item. Resolve the
    // instance locally and then process the export names one at a time,
    // injecting aliases as necessary.
    let mut index = item.idx.clone();
    resolve_ns(&mut index, Ns::Instance, resolve_stack)?;
    let span = item.idx.span();
    for (pos, export_name) in item.export_names.iter().enumerate() {
        // The last name is in the namespace of the reference. All others are
        // instances.
        let ns = if pos == item.export_names.len() - 1 {
            last_ns
        } else {
            Ns::Instance
        };

        // Record an outer alias to be inserted in front of the current
        // definition.
        let mut alias = Alias {
            span,
            id: None,
            name: None,
            target: AliasTarget::Export {
                instance: index,
                export: export_name,
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
        index = Index::Num(register_alias(&mut alias, resolver)?, span);
        resolver.aliases_to_insert.push(alias);
    }
    item.idx = index;
    item.export_names = Vec::new();

    Ok(())
}

fn resolve_ns<'a, 'b>(
    idx: &mut Index<'a>,
    ns: Ns,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    // Perform resolution on a local clone walking up the stack of components
    // that we have. Note that a local clone is used since we don't want to use
    // the parent's resolved index if a parent matches, instead we want to use
    // the index of the alias that we will automatically insert.
    let mut idx_clone = idx.clone();
    for (depth, resolver) in resolve_stack.iter_mut().rev().enumerate() {
        let depth = depth as u32;
        let found = match resolver.resolve(ns, &mut idx_clone) {
            Ok(idx) => idx,
            // Try the next parent
            Err(_) => continue,
        };

        // If this is the current component then no extra alias is necessary, so
        // return success.
        if depth == 0 {
            *idx = idx_clone;
            return Ok(());
        }
        let id = match idx {
            Index::Id(id) => id.clone(),
            Index::Num(..) => unreachable!(),
        };

        // When resolution succeeds in a parent then an outer alias is
        // automatically inserted here in this component.
        let span = idx.span();
        let mut alias = Alias {
            span,
            id: Some(id),
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
        let local_index = register_alias(&mut alias, resolver)?;
        resolver.aliases_to_insert.push(alias);
        *idx = Index::Num(local_index, span);
        return Ok(());
    }

    // If resolution in any parent failed then simply return the error from our
    // local namespace
    resolve_stack.last_mut().unwrap().resolve(ns, idx)?;
    unreachable!()
}

fn resolve_moduletype(ty: &mut ModuleType<'_>) -> Result<(), Error> {
    let mut types = Namespace::default();
    for def in ty.defs.iter_mut() {
        match def {
            ModuleTypeDef::Type(t) => {
                types.register(t.id, "type")?;
            }
            ModuleTypeDef::Import(t) => resolve_item_sig(&mut t.item, &types)?,
            ModuleTypeDef::Export(_, t) => resolve_item_sig(t, &types)?,
        }
    }
    return Ok(());

    fn resolve_item_sig<'a>(
        sig: &mut core::ItemSig<'a>,
        names: &Namespace<'a>,
    ) -> Result<(), Error> {
        match &mut sig.kind {
            core::ItemKind::Func(ty) | core::ItemKind::Tag(core::TagType::Exception(ty)) => {
                let idx = ty.index.as_mut().expect("index should be filled in");
                names.resolve(idx, "type")?;
            }
            core::ItemKind::Memory(_) | core::ItemKind::Global(_) | core::ItemKind::Table(_) => {}
        }
        Ok(())
    }
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
