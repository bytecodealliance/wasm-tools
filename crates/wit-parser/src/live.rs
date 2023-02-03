use crate::{
    DocumentId, Function, InterfaceId, Resolve, Type, TypeDefKind, TypeId, WorldId, WorldItem,
};
use indexmap::IndexSet;

#[derive(Default)]
pub struct LiveTypes {
    set: IndexSet<TypeId>,
}

impl LiveTypes {
    pub fn iter(&self) -> impl Iterator<Item = TypeId> + '_ {
        // Note the reverse iteration order to ensure that everything is visited
        // in a topological order.
        self.set.iter().rev().copied()
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn add_document(&mut self, resolve: &Resolve, doc: DocumentId) {
        let doc = &resolve.documents[doc];
        for (_, id) in doc.interfaces.iter() {
            self.add_interface(resolve, *id);
        }
        for (_, id) in doc.worlds.iter() {
            self.add_world(resolve, *id);
        }
    }

    pub fn add_interface(&mut self, resolve: &Resolve, iface: InterfaceId) {
        let iface = &resolve.interfaces[iface];
        for (_, id) in iface.types.iter() {
            self.add_type_id(resolve, *id);
        }
        for (_, func) in iface.functions.iter() {
            self.add_func(resolve, func);
        }
    }

    pub fn add_world(&mut self, resolve: &Resolve, world: WorldId) {
        let world = &resolve.worlds[world];
        for (_, item) in world.imports.iter().chain(world.exports.iter()) {
            self.add_world_item(resolve, item);
        }
    }

    pub fn add_world_item(&mut self, resolve: &Resolve, item: &WorldItem) {
        match item {
            WorldItem::Interface(id) => self.add_interface(resolve, *id),
            WorldItem::Function(f) => self.add_func(resolve, f),
            WorldItem::Type(t) => self.add_type_id(resolve, *t),
        }
    }

    pub fn add_func(&mut self, resolve: &Resolve, func: &Function) {
        for (_, ty) in func.params.iter() {
            self.add_type(resolve, ty);
        }
        for ty in func.results.iter_types() {
            self.add_type(resolve, ty);
        }
    }

    pub fn add_type_id(&mut self, resolve: &Resolve, ty: TypeId) {
        if !self.set.insert(ty) {
            return;
        }
        match &resolve.types[ty].kind {
            TypeDefKind::Type(t)
            | TypeDefKind::List(t)
            | TypeDefKind::Option(t)
            | TypeDefKind::Future(Some(t)) => self.add_type(resolve, t),
            TypeDefKind::Record(r) => {
                for field in r.fields.iter() {
                    self.add_type(resolve, &field.ty);
                }
            }
            TypeDefKind::Tuple(r) => {
                for ty in r.types.iter() {
                    self.add_type(resolve, ty);
                }
            }
            TypeDefKind::Variant(v) => {
                for case in v.cases.iter() {
                    if let Some(ty) = &case.ty {
                        self.add_type(resolve, ty);
                    }
                }
            }
            TypeDefKind::Union(u) => {
                for case in u.cases.iter() {
                    self.add_type(resolve, &case.ty);
                }
            }
            TypeDefKind::Result(r) => {
                if let Some(ty) = &r.ok {
                    self.add_type(resolve, ty);
                }
                if let Some(ty) = &r.err {
                    self.add_type(resolve, ty);
                }
            }
            TypeDefKind::Stream(s) => {
                if let Some(ty) = &s.element {
                    self.add_type(resolve, ty);
                }
                if let Some(ty) = &s.end {
                    self.add_type(resolve, ty);
                }
            }
            TypeDefKind::Flags(_) | TypeDefKind::Enum(_) | TypeDefKind::Future(None) => {}
            TypeDefKind::Unknown => unreachable!(),
        }
    }

    pub fn add_type(&mut self, resolve: &Resolve, ty: &Type) {
        match ty {
            Type::Id(id) => self.add_type_id(resolve, *id),
            _ => {}
        }
    }
}
