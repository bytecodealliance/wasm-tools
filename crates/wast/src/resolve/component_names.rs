use crate::ast::*;
use crate::resolve::names::Namespace;
use crate::Error;
use std::convert::TryInto;

pub fn resolve<'a>(fields: &mut Vec<ComponentField<'a>>) -> Result<(), Error> {
    let mut resolve_stack = Vec::new();
    resolve_fields(fields, &mut resolve_stack)?;
    Ok(())
}

fn resolve_fields<'a, 'b>(
    fields: &mut Vec<ComponentField<'a>>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    resolve_stack.push(ComponentResolver::default());
    process(fields, resolve_stack)?;
    resolve_stack.pop();
    Ok(())
}

/// Context structure used to perform name resolution.
#[derive(Default)]
struct ComponentResolver<'a> {
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
}

fn process<'a, 'b>(
    fields: &mut Vec<ComponentField<'a>>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    // Number everything in the component, recording what names correspond to
    // what indices.
    for field in fields.iter_mut() {
        register(field, resolve_stack)?;
    }

    // Then we can replace all our `Index::Id` instances with `Index::Num`
    // in the AST. Note that this also recurses into nested components.
    for field in fields.iter_mut() {
        resolve_field(field, resolve_stack)?;
    }
    Ok(())
}

fn register<'a, 'b>(
    item: &ComponentField<'a>,
    resolve_stack: &'b mut Vec<ComponentResolver<'a>>,
) -> Result<(), Error> {
    let resolver = resolve_stack.last_mut().unwrap();

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
            resolver.instances.register(m.id, "nested module")?;
        }
        ComponentField::Component(c) => {
            resolver.instances.register(c.id, "nested component")?;
        }
        ComponentField::Alias(_a) => todo!("register for alias"),

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
    let resolver = resolve_stack.last_mut().unwrap();

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

        ComponentField::Type(t) => match &mut t.def {
            ComponentTypeDef::DefType(d) => match d {
                DefType::Func(f) => {
                    for param in f.params.iter_mut() {
                        resolve_type_use(&mut param.type_, resolver)?;
                    }
                    resolve_type_use(&mut f.result, resolver)?;
                    Ok(())
                }
                DefType::Module(_m) => {
                    todo!("resolve for ModuleType")
                }
                DefType::Component(_c) => {
                    todo!("resolve for ComponentType")
                }
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
                            resolve_type_use(&mut field.type_, resolver)?;
                        }
                    }
                    InterType::Variant(v) => {
                        for case in v.cases.iter_mut() {
                            resolve_type_use(&mut case.type_, resolver)?;
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
                        resolve_type_use(&mut l.element, resolver)?;
                    }
                    InterType::Tuple(t) => {
                        for field in t.fields.iter_mut() {
                            resolve_type_use(field, resolver)?;
                        }
                    }
                    InterType::Union(t) => {
                        for arm in t.arms.iter_mut() {
                            resolve_type_use(arm, resolver)?;
                        }
                    }
                    InterType::Option(o) => {
                        resolve_type_use(&mut o.element, resolver)?;
                    }
                    InterType::Expected(r) => {
                        resolve_type_use(&mut r.ok, resolver)?;
                        resolve_type_use(&mut r.err, resolver)?;
                    }
                }
                Ok(())
            }
        },

        ComponentField::Func(_f) => Ok(()),

        ComponentField::Instance(_i) => Ok(()),
        ComponentField::Module(_m) => Ok(()),
        ComponentField::Component(_c) => Ok(()),
        ComponentField::Alias(_a) => todo!("register for alias"),

        ComponentField::Start(_i) => Ok(()),

        ComponentField::Export(_e) => Ok(()),

        ComponentField::Custom(_) => Ok(()),
    }
}

fn resolve_type_use<'a, 'b, T>(
    ty: &'b mut ComponentTypeUse<'a, T>,
    resolver: &'b mut ComponentResolver<'a>,
) -> Result<(), Error> {
    match ty {
        ComponentTypeUse::Ref(r) => resolve_item_ref(r, resolver),
        ComponentTypeUse::Inline(_i) => {
            // Nothing to do.
            Ok(())
        }
    }
}

fn resolve_item_ref<'a, 'b, K>(
    item: &'b mut ItemRef<'a, K>,
    resolver: &'b mut ComponentResolver<'a>,
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
    resolve_ns(&mut item.idx, item.kind.into(), resolver)?;
    Ok(())
}

fn resolve_ns<'a, 'b>(
    idx: &mut Index<'a>,
    ns: Ns,
    resolver: &'b mut ComponentResolver<'a>,
) -> Result<u32, Error> {
    match ns {
        Ns::Func => resolver.funcs.resolve(idx, "func"),
        Ns::Table => resolver.tables.resolve(idx, "table"),
        Ns::Global => resolver.globals.resolve(idx, "global"),
        Ns::Memory => resolver.memories.resolve(idx, "memory"),
        Ns::Tag => resolver.tags.resolve(idx, "tag"),
        Ns::Type => resolver.types.resolve(idx, "type"),
        Ns::Component => resolver.components.resolve(idx, "component"),
        Ns::Module => resolver.modules.resolve(idx, "module"),
        Ns::Instance => resolver.instances.resolve(idx, "instance"),
        Ns::Value => resolver.values.resolve(idx, "instance"),
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
