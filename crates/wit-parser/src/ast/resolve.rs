use super::{Error, Func, InterfaceItem, ParamList, ResultList, Span, Value, ValueKind, WorldItem};
use crate::*;
use anyhow::Result;
use indexmap::IndexMap;
use petgraph::{algo::toposort, graphmap::DiGraphMap};
use std::collections::{HashMap, HashSet};
use std::mem;

#[derive(Default)]
pub struct Resolver {
    // Type names defined in each interface; the position within the vec
    // corresponds to the containing interface's InterfaceId.
    interface_type_names: Vec<IndexMap<String, TypeId>>,

    type_lookup: IndexMap<String, TypeId>,
    types: Arena<TypeDef>,
    anon_types: HashMap<Key, TypeId>,
    functions: Vec<Function>,
    interfaces: Arena<Interface>,
    interface_lookup: IndexMap<String, InterfaceId>,
    worlds: Arena<World>,
}

#[derive(PartialEq, Eq, Hash)]
enum Key {
    Variant(Vec<(String, Option<Type>)>),
    Record(Vec<(String, Type)>),
    Flags(Vec<String>),
    Tuple(Vec<Type>),
    Enum(Vec<String>),
    List(Type),
    Option(Type),
    Result(Option<Type>, Option<Type>),
    Union(Vec<Type>),
    Future(Option<Type>),
    Stream(Option<Type>, Option<Type>),
}

impl Resolver {
    fn resolve_interfaces(&mut self, ast: &ast::Ast<'_>) -> Result<()> {
        let mut graph = DiGraphMap::<&str, usize>::new();
        let mut defs: HashMap<&str, Span> = Default::default();

        // Register all interface names
        for interface in ast.interfaces() {
            let name = &*interface.name.name;
            let span = interface.name.span;
            if defs.insert(name, span).is_some() {
                return Err(Error {
                    span: interface.name.span,
                    msg: format!("interface `{name}` is defined more than once"),
                }
                .into());
            }
            graph.add_node(name);
        }
        // ...then populate use-dependencies
        for interface in ast.interfaces() {
            let name = &*interface.name.name;

            for use_ in interface.uses() {
                let use_name = &*use_.from.name;
                // Ensure the interface being used is known.
                if !defs.contains_key(use_name) {
                    return Err(Error {
                        span: use_.from.span,
                        msg: format!("interface `{use_name}` is undefined"),
                    }
                    .into());
                }
                graph.add_edge(name, use_name, 0);
            }
        }

        match toposort(&graph, None) {
            Ok(ordered) => {
                let sorted = ordered
                    .into_iter()
                    .rev()
                    .map(|name| ast.interfaces().find(|i| &*i.name.name == name).unwrap());

                for interface in sorted {
                    let items = &interface.items;
                    let name = &interface.name.name;
                    let docs = &interface.docs;
                    let instance = self.resolve_interface(name, items, docs)?;
                    assert!(self
                        .interface_lookup
                        .insert(name.to_string(), instance)
                        .is_none());
                }

                Ok(())
            }
            Err(cycle) => {
                let name = cycle.node_id();
                let span = defs[name];
                return Err(Error {
                    span,
                    msg: format!("interface `{name}` contains a use cycle"),
                }
                .into());
            }
        }
    }

    pub(crate) fn resolve(&mut self, ast: &ast::Ast<'_>) -> Result<Document> {
        self.resolve_interfaces(ast)?;

        for w in ast.worlds() {
            let mut world = World {
                name: w.name.name.to_string(),
                docs: self.docs(&w.docs),
                imports: Default::default(),
                exports: Default::default(),
                default: None,
            };

            for item in w.items.iter() {
                match item {
                    WorldItem::Import(import) => {
                        let ast::Import { name, kind } = import;
                        self.insert_extern(name, kind, "import", &mut world.imports)?;
                    }
                    WorldItem::Export(export) => {
                        let ast::Export { name, kind } = export;
                        self.insert_extern(name, kind, "export", &mut world.exports)?;
                    }
                    WorldItem::ExportDefault(iface) => {
                        if world.default.is_some() {
                            return Err(Error {
                                span: iface.span(),
                                msg: "more than one default interface was defined".to_string(),
                            }
                            .into());
                        }

                        let iface = self.resolve_extern(iface)?;
                        world.default = Some(iface);
                    }
                }
            }

            self.worlds.alloc(world);
        }

        Ok(Document {
            worlds: mem::take(&mut self.worlds),
            interfaces: mem::take(&mut self.interfaces),
            types: mem::take(&mut self.types),
        })
    }

    fn insert_extern(
        &mut self,
        id: &ast::Id<'_>,
        kind: &ast::ExternKind<'_>,
        direction: &str,
        resolved: &mut IndexMap<String, InterfaceId>,
    ) -> Result<()> {
        let interface = self.resolve_extern(kind)?;
        if resolved.insert(id.name.to_string(), interface).is_some() {
            return Err(Error {
                span: id.span,
                msg: format!("duplicate {direction} {}", id.name),
            }
            .into());
        }
        Ok(())
    }

    fn resolve_extern(&mut self, kind: &ast::ExternKind<'_>) -> Result<InterfaceId> {
        match kind {
            ast::ExternKind::Interface(_span, items) => {
                self.resolve_interface("", items, &Default::default())
            }
            ast::ExternKind::Id(id) => {
                self.interface_lookup
                    .get(&*id.name)
                    .cloned()
                    .ok_or_else(|| {
                        Error {
                            span: id.span,
                            msg: format!("{} not defined", id.name),
                        }
                        .into()
                    })
            }
        }
    }

    fn process_use(&mut self, fields: &[InterfaceItem<'_>]) -> Result<()> {
        for item in fields.iter() {
            let u = match item {
                InterfaceItem::Use(u) => u,
                _ => continue,
            };
            let dep_id = self.interface_lookup[&*u.from.name];
            let from_name = &u.from.name;
            for name in u.names.iter() {
                let (my_name, span) = match &name.as_ {
                    Some(id) => (&id.name, id.span),
                    None => (&name.name.name, name.name.span),
                };
                let dep_names = &self.interface_type_names[dep_id.index()];

                let use_type_name = name.name.name.to_string();

                match dep_names.get(&use_type_name) {
                    Some(type_id) => {
                        self.define_type(my_name, span, *type_id)?;
                    }
                    None => {
                        return Err(Error {
                            span: name.name.span,
                            msg: format!(
                                "type `{}` not defined in interface `{}`",
                                name.name.name, from_name,
                            ),
                        }
                        .into())
                    }
                }
            }
        }
        Ok(())
    }

    pub(crate) fn resolve_interface(
        &mut self,
        name: &str,
        fields: &[InterfaceItem<'_>],
        docs: &super::Docs<'_>,
    ) -> Result<InterfaceId> {
        // First resolve any names introduced by `use`
        self.process_use(fields)?;
        // ... then register our own names
        self.register_names(fields)?;

        // With all names registered we can now fully expand and translate all
        // types.
        for field in fields {
            let t = match field {
                InterfaceItem::TypeDef(t) => t,
                _ => continue,
            };
            let id = self.type_lookup[&*t.name.name];
            let kind = self.resolve_type_def(&t.ty)?;
            self.types.get_mut(id).unwrap().kind = kind;
        }

        // And finally we can resolve all type references in functions/globals
        // and additionally validate that types thesmelves are not recursive
        let mut valid_types = HashSet::new();
        let mut visiting = HashSet::new();
        for field in fields {
            match field {
                InterfaceItem::Value(v) => self.resolve_value(v)?,
                InterfaceItem::TypeDef(t) => {
                    self.validate_type_not_recursive(
                        t.name.span,
                        self.type_lookup[&*t.name.name],
                        &mut visiting,
                        &mut valid_types,
                    )?;
                }
                InterfaceItem::Use(_) => {}
            }
        }

        let type_names = self.type_lookup.clone();
        self.interface_type_names.push(type_names);

        let iface = Interface {
            name: name.to_string(),
            url: None,
            docs: self.docs(docs),
            types: mem::take(&mut self.type_lookup)
                .into_iter()
                .map(|(_, ty)| ty)
                .collect(),
            functions: mem::take(&mut self.functions),
        };
        Ok(self.interfaces.alloc(iface))
    }

    fn register_names(&mut self, fields: &[InterfaceItem<'_>]) -> Result<()> {
        let mut values = HashSet::new();
        for field in fields {
            match field {
                InterfaceItem::TypeDef(t) => {
                    let docs = self.docs(&t.docs);
                    let id = self.types.alloc(TypeDef {
                        docs,
                        // a dummy kind is used for now which will get filled in
                        // later with the actual desired contents.
                        kind: TypeDefKind::List(Type::U8),
                        name: Some(t.name.name.to_string()),
                        interface: Some(self.interfaces.next_id()),
                    });
                    self.define_type(&t.name.name, t.name.span, id)?;
                }
                InterfaceItem::Value(f) => {
                    if !values.insert(&f.name.name) {
                        return Err(Error {
                            span: f.name.span,
                            msg: format!("`{}` is defined more than once", f.name.name),
                        }
                        .into());
                    }
                }
                InterfaceItem::Use(_) => {}
            }
        }

        Ok(())
    }

    fn define_type(&mut self, name: &str, span: Span, id: TypeId) -> Result<()> {
        if self.type_lookup.insert(name.to_string(), id).is_some() {
            Err(Error {
                span,
                msg: format!("type `{}` is defined more than once", name),
            }
            .into())
        } else {
            Ok(())
        }
    }

    fn resolve_type_def(&mut self, ty: &super::Type<'_>) -> Result<TypeDefKind> {
        Ok(match ty {
            super::Type::Bool => TypeDefKind::Type(Type::Bool),
            super::Type::U8 => TypeDefKind::Type(Type::U8),
            super::Type::U16 => TypeDefKind::Type(Type::U16),
            super::Type::U32 => TypeDefKind::Type(Type::U32),
            super::Type::U64 => TypeDefKind::Type(Type::U64),
            super::Type::S8 => TypeDefKind::Type(Type::S8),
            super::Type::S16 => TypeDefKind::Type(Type::S16),
            super::Type::S32 => TypeDefKind::Type(Type::S32),
            super::Type::S64 => TypeDefKind::Type(Type::S64),
            super::Type::Float32 => TypeDefKind::Type(Type::Float32),
            super::Type::Float64 => TypeDefKind::Type(Type::Float64),
            super::Type::Char => TypeDefKind::Type(Type::Char),
            super::Type::String => TypeDefKind::Type(Type::String),
            super::Type::Name(name) => {
                let id = match self.type_lookup.get(&*name.name) {
                    Some(id) => *id,
                    None => {
                        return Err(Error {
                            span: name.span,
                            msg: format!("no type named `{}`", name.name),
                        }
                        .into())
                    }
                };
                TypeDefKind::Type(Type::Id(id))
            }
            super::Type::List(list) => {
                let ty = self.resolve_type(list)?;
                TypeDefKind::List(ty)
            }
            super::Type::Record(record) => {
                let fields = record
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(Field {
                            docs: self.docs(&field.docs),
                            name: field.name.name.to_string(),
                            ty: self.resolve_type(&field.ty)?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Record(Record { fields })
            }
            super::Type::Flags(flags) => {
                let flags = flags
                    .flags
                    .iter()
                    .map(|flag| Flag {
                        docs: self.docs(&flag.docs),
                        name: flag.name.name.to_string(),
                    })
                    .collect::<Vec<_>>();
                TypeDefKind::Flags(Flags { flags })
            }
            super::Type::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|ty| self.resolve_type(ty))
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Tuple(Tuple { types })
            }
            super::Type::Variant(variant) => {
                if variant.cases.is_empty() {
                    return Err(Error {
                        span: variant.span,
                        msg: "empty variant".to_string(),
                    }
                    .into());
                }
                let cases = variant
                    .cases
                    .iter()
                    .map(|case| {
                        Ok(Case {
                            docs: self.docs(&case.docs),
                            name: case.name.name.to_string(),
                            ty: self.resolve_optional_type(case.ty.as_ref())?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Variant(Variant { cases })
            }
            super::Type::Enum(e) => {
                if e.cases.is_empty() {
                    return Err(Error {
                        span: e.span,
                        msg: "empty enum".to_string(),
                    }
                    .into());
                }
                let cases = e
                    .cases
                    .iter()
                    .map(|case| {
                        Ok(EnumCase {
                            docs: self.docs(&case.docs),
                            name: case.name.name.to_string(),
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Enum(Enum { cases })
            }
            super::Type::Option(ty) => TypeDefKind::Option(self.resolve_type(ty)?),
            super::Type::Result(r) => TypeDefKind::Result(Result_ {
                ok: self.resolve_optional_type(r.ok.as_deref())?,
                err: self.resolve_optional_type(r.err.as_deref())?,
            }),
            super::Type::Union(e) => {
                if e.cases.is_empty() {
                    return Err(Error {
                        span: e.span,
                        msg: "empty union".to_string(),
                    }
                    .into());
                }
                let cases = e
                    .cases
                    .iter()
                    .map(|case| {
                        Ok(UnionCase {
                            docs: self.docs(&case.docs),
                            ty: self.resolve_type(&case.ty)?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeDefKind::Union(Union { cases })
            }
            super::Type::Future(t) => {
                TypeDefKind::Future(self.resolve_optional_type(t.as_deref())?)
            }
            super::Type::Stream(s) => TypeDefKind::Stream(Stream {
                element: self.resolve_optional_type(s.element.as_deref())?,
                end: self.resolve_optional_type(s.end.as_deref())?,
            }),
        })
    }

    fn resolve_type(&mut self, ty: &super::Type<'_>) -> Result<Type> {
        let kind = self.resolve_type_def(ty)?;
        Ok(self.anon_type_def(TypeDef {
            kind,
            name: None,
            docs: Docs::default(),
            interface: None,
        }))
    }

    fn resolve_optional_type(&mut self, ty: Option<&super::Type<'_>>) -> Result<Option<Type>> {
        match ty {
            Some(ty) => Ok(Some(self.resolve_type(ty)?)),
            None => Ok(None),
        }
    }

    fn anon_type_def(&mut self, ty: TypeDef) -> Type {
        let key = match &ty.kind {
            TypeDefKind::Type(t) => return *t,
            TypeDefKind::Variant(v) => Key::Variant(
                v.cases
                    .iter()
                    .map(|case| (case.name.clone(), case.ty))
                    .collect::<Vec<_>>(),
            ),
            TypeDefKind::Record(r) => Key::Record(
                r.fields
                    .iter()
                    .map(|case| (case.name.clone(), case.ty))
                    .collect::<Vec<_>>(),
            ),
            TypeDefKind::Flags(r) => {
                Key::Flags(r.flags.iter().map(|f| f.name.clone()).collect::<Vec<_>>())
            }
            TypeDefKind::Tuple(t) => Key::Tuple(t.types.clone()),
            TypeDefKind::Enum(r) => {
                Key::Enum(r.cases.iter().map(|f| f.name.clone()).collect::<Vec<_>>())
            }
            TypeDefKind::List(ty) => Key::List(*ty),
            TypeDefKind::Option(t) => Key::Option(*t),
            TypeDefKind::Result(r) => Key::Result(r.ok, r.err),
            TypeDefKind::Union(u) => Key::Union(u.cases.iter().map(|c| c.ty).collect()),
            TypeDefKind::Future(ty) => Key::Future(*ty),
            TypeDefKind::Stream(s) => Key::Stream(s.element, s.end),
        };
        let types = &mut self.types;
        let id = self
            .anon_types
            .entry(key)
            .or_insert_with(|| types.alloc(ty));
        Type::Id(*id)
    }

    fn docs(&mut self, doc: &super::Docs<'_>) -> Docs {
        let mut docs = None;
        for doc in doc.docs.iter() {
            // Comments which are not doc-comments are silently ignored
            if let Some(doc) = doc.strip_prefix("///") {
                let docs = docs.get_or_insert_with(String::new);
                docs.push_str(doc.trim_start_matches('/').trim());
                docs.push('\n');
            } else if let Some(doc) = doc.strip_prefix("/*") {
                // We have to strip this before checking if this is a doc
                // comment to avoid breaking on empty block comments, `/**/`.
                let doc = doc.strip_suffix("*/").unwrap();

                if let Some(doc) = doc.strip_prefix('*') {
                    let docs = docs.get_or_insert_with(String::new);
                    for line in doc.lines() {
                        docs.push_str(line);
                        docs.push('\n');
                    }
                }
            }
        }
        Docs { contents: docs }
    }

    fn resolve_value(&mut self, value: &Value<'_>) -> Result<()> {
        let docs = self.docs(&value.docs);
        match &value.kind {
            ValueKind::Func(Func { params, results }) => {
                let params = self.resolve_params(params)?;
                let results = self.resolve_results(results)?;
                self.functions.push(Function {
                    docs,
                    name: value.name.name.to_string(),
                    kind: FunctionKind::Freestanding,
                    params,
                    results,
                });
            }
        }
        Ok(())
    }

    fn resolve_params(&mut self, params: &ParamList<'_>) -> Result<Params> {
        params
            .iter()
            .map(|(name, ty)| Ok((name.name.to_string(), self.resolve_type(ty)?)))
            .collect::<Result<_>>()
    }

    fn resolve_results(&mut self, results: &ResultList<'_>) -> Result<Results> {
        match results {
            ResultList::Named(rs) => Ok(Results::Named(self.resolve_params(rs)?)),
            ResultList::Anon(ty) => Ok(Results::Anon(self.resolve_type(ty)?)),
        }
    }

    fn validate_type_not_recursive(
        &self,
        span: Span,
        ty: TypeId,
        visiting: &mut HashSet<TypeId>,
        valid: &mut HashSet<TypeId>,
    ) -> Result<()> {
        if valid.contains(&ty) {
            return Ok(());
        }
        if !visiting.insert(ty) {
            return Err(Error {
                span,
                msg: "type can recursively refer to itself".to_string(),
            }
            .into());
        }

        match &self.types[ty].kind {
            TypeDefKind::List(Type::Id(id)) | TypeDefKind::Type(Type::Id(id)) => {
                self.validate_type_not_recursive(span, *id, visiting, valid)?
            }
            TypeDefKind::Variant(v) => {
                for case in v.cases.iter() {
                    if let Some(Type::Id(id)) = case.ty {
                        self.validate_type_not_recursive(span, id, visiting, valid)?;
                    }
                }
            }
            TypeDefKind::Record(r) => {
                for case in r.fields.iter() {
                    if let Type::Id(id) = case.ty {
                        self.validate_type_not_recursive(span, id, visiting, valid)?;
                    }
                }
            }
            TypeDefKind::Tuple(t) => {
                for ty in t.types.iter() {
                    if let Type::Id(id) = *ty {
                        self.validate_type_not_recursive(span, id, visiting, valid)?;
                    }
                }
            }

            TypeDefKind::Option(t) => {
                if let Type::Id(id) = *t {
                    self.validate_type_not_recursive(span, id, visiting, valid)?
                }
            }
            TypeDefKind::Result(r) => {
                if let Some(Type::Id(id)) = r.ok {
                    self.validate_type_not_recursive(span, id, visiting, valid)?
                }
                if let Some(Type::Id(id)) = r.err {
                    self.validate_type_not_recursive(span, id, visiting, valid)?
                }
            }
            TypeDefKind::Future(t) => {
                if let Some(Type::Id(id)) = *t {
                    self.validate_type_not_recursive(span, id, visiting, valid)?
                }
            }
            TypeDefKind::Stream(s) => {
                if let Some(Type::Id(id)) = s.element {
                    self.validate_type_not_recursive(span, id, visiting, valid)?
                }
                if let Some(Type::Id(id)) = s.end {
                    self.validate_type_not_recursive(span, id, visiting, valid)?
                }
            }
            TypeDefKind::Union(u) => {
                for c in u.cases.iter() {
                    if let Type::Id(id) = c.ty {
                        self.validate_type_not_recursive(span, id, visiting, valid)?
                    }
                }
            }

            TypeDefKind::Flags(_)
            | TypeDefKind::List(_)
            | TypeDefKind::Type(_)
            | TypeDefKind::Enum(_) => {}
        }

        valid.insert(ty);
        visiting.remove(&ty);
        Ok(())
    }
}
