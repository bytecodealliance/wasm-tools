use crate::ast::*;
use crate::resolve::names::Namespace;
use crate::Error;
use std::convert::TryInto;
use std::mem::replace;

/// Resolve the fields of a component and everything nested within it, changing
/// `Index::Id` to `Index::Num` and expanding alias syntax sugar.
pub fn resolve<'a>(id: Option<Id<'a>>, fields: &mut Vec<ComponentField<'a>>) -> Result<(), Error> {
    let mut resolve_stack = Vec::new();
    resolve_fields(id, fields, &mut resolve_stack)
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
            register(&mut fields[i], resolver)?;
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
                Index::Id(_name) => todo!("import with a name"),
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
        ComponentField::Alias(a) => match a.kind {
            AliasKind::Module => {
                resolver.modules.register(a.id, "module alias")?;
            }
            AliasKind::Component => {
                resolver.components.register(a.id, "component alias")?;
            }
            AliasKind::Instance => {
                resolver.instances.register(a.id, "instance alias")?;
            }
            AliasKind::Value => {
                resolver.values.register(a.id, "value alias")?;
            }
            AliasKind::ExportKind(ExportKind::Func) => {
                resolver.funcs.register(a.id, "func alias")?;
            }
            AliasKind::ExportKind(ExportKind::Table) => {
                resolver.tables.register(a.id, "table alias")?;
            }
            AliasKind::ExportKind(ExportKind::Global) => {
                resolver.globals.register(a.id, "global alias")?;
            }
            AliasKind::ExportKind(ExportKind::Memory) => {
                resolver.memories.register(a.id, "memory alias")?;
            }
            AliasKind::ExportKind(ExportKind::Tag) => {
                resolver.tags.register(a.id, "tag alias")?;
            }
            AliasKind::ExportKind(ExportKind::Type) => {
                resolver.types.register(a.id, "type alias")?;
            }
        },

        // These fields don't define any items in any index space.
        ComponentField::Export(_) | ComponentField::Start(_) | ComponentField::Custom(_) => {
            return Ok(())
        }
    };

    Ok(())
}

fn resolve_field<'a, 'b>(
    field: &mut ComponentField<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match field {
        ComponentField::Import(i) => {
            match &i.type_ {
                ComponentTypeUse::Inline(_i) => {
                    // The type is already defined, so there's nothing to do.
                }
                ComponentTypeUse::Ref(r) => match r.idx {
                    Index::Num(_, _) => {}
                    Index::Id(_name) => todo!("import with a name"),
                },
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
                        ModuleArg::BundleOfExports(_) => todo!("bundle of exports"),
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
                        ComponentArg::BundleOfExports(_) => todo!("bundle of exports"),
                    }
                }
                Ok(())
            }
            InstanceKind::BundleOfExports { .. } => todo!("bundle of exports"),
            InstanceKind::BundleOfComponentExports { .. } => todo!("bundle of exports"),
        },
        ComponentField::Module(_m) => Ok(()),
        ComponentField::Component(c) => match &mut c.kind {
            ComponentKind::Binary(_) => Ok(()),
            ComponentKind::Text(fields) => resolve_fields(c.id, fields, resolve_stack),
        },
        ComponentField::Alias(a) => match &mut a.target {
            AliasTarget::Export { .. } => todo!("export alias"),
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
                                a.span,
                                format!("outer component '{:?}' not found", id),
                            ));
                        }
                        depth
                    }
                    Index::Num(n, _span) => *n,
                };
                *outer = Index::Num(depth, a.span);

                // Resolve `index` within the computed scope depth.
                let ns = match a.kind {
                    AliasKind::Module => Ns::Module,
                    AliasKind::Component => Ns::Component,
                    AliasKind::Instance => Ns::Instance,
                    AliasKind::Value => Ns::Value,
                    AliasKind::ExportKind(ExportKind::Func) => Ns::Func,
                    AliasKind::ExportKind(ExportKind::Table) => Ns::Table,
                    AliasKind::ExportKind(ExportKind::Global) => Ns::Global,
                    AliasKind::ExportKind(ExportKind::Memory) => Ns::Memory,
                    AliasKind::ExportKind(ExportKind::Tag) => Ns::Tag,
                    AliasKind::ExportKind(ExportKind::Type) => Ns::Type,
                };
                let computed = resolve_stack.len() - 1 - depth as usize;
                resolve_stack[computed].resolve(ns, index)?;

                Ok(())
            }
        },

        ComponentField::Start(_i) => Ok(()),

        ComponentField::Export(_e) => Ok(()),

        ComponentField::Custom(_) => Ok(()),
    }
}

fn resolve_type_use<'a, 'b, T>(
    ty: &'b mut ComponentTypeUse<'a, T>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match ty {
        ComponentTypeUse::Ref(r) => resolve_item_ref(r, resolve_stack),
        ComponentTypeUse::Inline(_i) => {
            // Nothing to do.
            Ok(())
        }
    }
}

fn resolve_type_def<'a, 'b>(
    def: &'b mut ComponentTypeDef<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    match def {
        ComponentTypeDef::DefType(d) => match d {
            DefType::Func(f) => {
                for param in f.params.iter_mut() {
                    resolve_type_use(&mut param.type_, resolve_stack)?;
                }
                resolve_type_use(&mut f.result, resolve_stack)?;
                Ok(())
            }
            DefType::Module(_m) => {
                todo!("resolve for ModuleType")
            }
            DefType::Component(c) => resolve_nested_component_type(c, resolve_stack),
            DefType::Instance(_i) => {
                todo!("resolve for InstanceType")
            }
            DefType::Value(_v) => {
                todo!("resolve for ValueType")
            }
        },
        ComponentTypeDef::InterType(i) => {
            match i {
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
                        resolve_type_use(&mut field.type_, resolve_stack)?;
                    }
                }
                InterType::Variant(v) => {
                    for case in v.cases.iter_mut() {
                        resolve_type_use(&mut case.type_, resolve_stack)?;
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
                                                Error::new(
                                                    v.cases[i].span,
                                                    format!("too many cases"),
                                                )
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
                    resolve_type_use(&mut l.element, resolve_stack)?;
                }
                InterType::Tuple(t) => {
                    for field in t.fields.iter_mut() {
                        resolve_type_use(field, resolve_stack)?;
                    }
                }
                InterType::Union(t) => {
                    for arm in t.arms.iter_mut() {
                        resolve_type_use(arm, resolve_stack)?;
                    }
                }
                InterType::Option(o) => {
                    resolve_type_use(&mut o.element, resolve_stack)?;
                }
                InterType::Expected(r) => {
                    resolve_type_use(&mut r.ok, resolve_stack)?;
                    resolve_type_use(&mut r.err, resolve_stack)?;
                }
            }
            Ok(())
        }
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
            ComponentTypeField::Alias(_alias) => {}
            ComponentTypeField::Type(ty) => {
                resolve_type_def(&mut ty.def, resolve_stack)?;
            }
            ComponentTypeField::Import(import) => {
                resolve_type_use(&mut import.type_, resolve_stack)?;
            }
            ComponentTypeField::Export(export) => {
                resolve_type_use(&mut export.item, resolve_stack)?;
            }
        }

        // Name resolution may have emitted some aliases. Insert them before
        // the current definition.
        let resolver = resolve_stack.last_mut().unwrap();
        let aliases_to_insert = replace(&mut resolver.aliases_to_insert, Vec::new());
        for mut alias in aliases_to_insert {
            register_alias(&mut alias, resolver)?;
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
    }

    resolve_stack.pop();
    Ok(())
}

fn register_alias<'a, 'b>(
    alias: &'b Alias<'a>,
    resolver: &'b mut ComponentResolver<'a>,
) -> Result<(), Error> {
    match alias.target {
        AliasTarget::Outer { .. } => match alias.kind {
            AliasKind::Module => {
                resolver.modules.register(alias.id, "module")?;
            }
            AliasKind::Component => {
                resolver.components.register(alias.id, "component")?;
            }
            AliasKind::Instance => {
                resolver.instances.register(alias.id, "instance")?;
            }
            AliasKind::Value => {
                resolver.values.register(alias.id, "value")?;
            }
            AliasKind::ExportKind(ExportKind::Func) => {
                resolver.funcs.register(alias.id, "func")?;
            }
            AliasKind::ExportKind(ExportKind::Table) => {
                resolver.tables.register(alias.id, "table")?;
            }
            AliasKind::ExportKind(ExportKind::Memory) => {
                resolver.memories.register(alias.id, "memory")?;
            }
            AliasKind::ExportKind(ExportKind::Global) => {
                resolver.globals.register(alias.id, "global")?;
            }
            AliasKind::ExportKind(ExportKind::Tag) => {
                resolver.tags.register(alias.id, "tag")?;
            }
            AliasKind::ExportKind(ExportKind::Type) => {
                resolver.types.register(alias.id, "type")?;
            }
        },
        AliasTarget::Export {
            instance: _,
            export: _,
        } => {
            todo!("export alias");
        }
    }
    Ok(())
}

fn resolve_item_ref<'a, 'b, K>(
    item: &'b mut ItemRef<'a, K>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error>
where
    K: Into<Ns> + Copy,
{
    #[cfg(wast_check_exhaustive)]
    {
        if !item.visited {
            return Err(Error::new(
                item.idx.span(),
                format!("BUG: this index wasn't visited"),
            ));
        }
    }
    resolve_ns(&mut item.idx, item.kind.into(), resolve_stack)?;
    Ok(())
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
        todo!("name resolution failed")
    }

    // Record an outer alias to be inserted in front of the current definition.
    let span = idx.span();
    let alias = Alias {
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
            Ns::Func => AliasKind::ExportKind(ExportKind::Func),
            Ns::Table => AliasKind::ExportKind(ExportKind::Table),
            Ns::Global => AliasKind::ExportKind(ExportKind::Global),
            Ns::Memory => AliasKind::ExportKind(ExportKind::Memory),
            Ns::Tag => AliasKind::ExportKind(ExportKind::Tag),
            Ns::Type => AliasKind::ExportKind(ExportKind::Type),
        },
    };
    resolve_stack
        .last_mut()
        .unwrap()
        .aliases_to_insert
        .push(alias);

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
