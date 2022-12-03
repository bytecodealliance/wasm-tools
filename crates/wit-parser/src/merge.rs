use super::*;

impl Document {
    /// Merges a different wit document into this one.
    ///
    /// This method is an infallible operation that will append all of the
    /// contents of `other` into `self`. All types, interfaces, and worlds are
    /// moved over en-masse. The return value is a structur which can be used to
    /// remap old within `Document` to new ids.
    //
    // Note that this should eventually not error on duplicate interfaces but
    // rather attempt to perform a deep merge on duplicate interfaces.
    pub fn merge(&mut self, other: Document) -> Merge {
        let mut merge = Merge::default();
        merge.merge(self, other);
        merge
    }

    /// Merges the world `from` into the world `into`.
    ///
    /// This will attempt to merge one world into another, unioning all of its
    /// imports and exports together. This is an operation performed by
    /// `wit-component`, for example where two different worlds from two
    /// different libraries were linked into the same core wasm file and are
    /// producing a singular world that will be the final component's
    /// interface.
    ///
    /// This operation can fail if the imports/exports overlap.
    //
    // TODO: overlap shouldn't be a hard error here, there should be some form
    // of comparing names/urls/deep merging or such to get this working.
    pub fn merge_worlds(&mut self, from: WorldId, into: WorldId) -> Result<()> {
        let mut new_imports = Vec::new();
        let mut new_exports = Vec::new();

        let from_world = &self.worlds[from];
        let into_world = &self.worlds[into];
        for (name, import) in from_world.imports.iter() {
            match into_world.imports.get(name) {
                Some(_) => bail!("duplicate import found for interface `{name}`"),
                None => new_imports.push((name.clone(), *import)),
            }
        }
        for (name, export) in from_world.exports.iter() {
            match into_world.exports.get(name) {
                Some(_) => bail!("duplicate export found for interface `{name}`"),
                None => new_exports.push((name.clone(), *export)),
            }
        }
        let from_world_default = from_world.default;

        let into = &mut self.worlds[into];
        for (name, import) in new_imports {
            let prev = into.imports.insert(name, import);
            assert!(prev.is_none());
        }
        for (name, import) in new_exports {
            let prev = into.exports.insert(name, import);
            assert!(prev.is_none());
        }
        if let Some(i) = from_world_default {
            if into.default.is_some() {
                bail!("duplicate default export found")
            }
            into.default = Some(i);
        }

        Ok(())
    }
}

#[derive(Default)]
#[allow(missing_docs)]
pub struct Merge {
    pub type_map: Vec<Option<TypeId>>,
    pub iface_map: Vec<InterfaceId>,
    pub world_map: Vec<WorldId>,
}

impl Merge {
    fn merge(&mut self, into: &mut Document, other: Document) {
        let mut topo_map = vec![0; other.types.len()];
        for (i, id) in other.topological_types().into_iter().enumerate() {
            topo_map[id.index()] = i;
        }
        let Document {
            worlds,
            interfaces,
            types,
        } = other;
        let mut types = types.into_iter().collect::<Vec<_>>();
        types.sort_by_key(|(id, _)| topo_map[id.index()]);
        self.type_map.extend((0..types.len()).map(|_| None));
        for (id, mut ty) in types {
            self.update_typedef(&mut ty);
            let new_id = into.types.alloc(ty);
            self.type_map[id.index()] = Some(new_id);
        }

        for (id, mut iface) in interfaces {
            self.update_interface(&mut iface);
            let new_id = into.interfaces.alloc(iface);
            assert_eq!(self.iface_map.len(), id.index());
            self.iface_map.push(new_id);
        }

        for (id, mut world) in worlds {
            self.update_world(&mut world);
            let new_id = into.worlds.alloc(world);
            assert_eq!(self.world_map.len(), id.index());
            self.world_map.push(new_id);
        }

        // Update the `interface` field of `TypeDef` now that all interfaces
        // have been processed.
        for ty in self.type_map.iter() {
            let ty = ty.unwrap();
            if let Some(id) = &mut into.types[ty].interface {
                *id = self.iface_map[id.index()];
            }
        }
    }

    fn update_typedef(&self, typedef: &mut TypeDef) {
        self.update_typedef_kind(&mut typedef.kind);
        // Note that the interface isn't updated here, it's updated as a
        // final pass.
    }

    fn update_typedef_kind(&self, typedef: &mut TypeDefKind) {
        use TypeDefKind::*;
        match typedef {
            Record(r) => {
                for field in r.fields.iter_mut() {
                    self.update_ty(&mut field.ty);
                }
            }
            Tuple(t) => {
                for ty in t.types.iter_mut() {
                    self.update_ty(ty);
                }
            }
            Variant(v) => {
                for case in v.cases.iter_mut() {
                    if let Some(t) = &mut case.ty {
                        self.update_ty(t);
                    }
                }
            }
            Option(t) => self.update_ty(t),
            Result(r) => {
                if let Some(ty) = &mut r.ok {
                    self.update_ty(ty);
                }
                if let Some(ty) = &mut r.err {
                    self.update_ty(ty);
                }
            }
            Union(u) => {
                for case in u.cases.iter_mut() {
                    self.update_ty(&mut case.ty);
                }
            }
            List(t) => self.update_ty(t),
            Future(Some(t)) => self.update_ty(t),
            Stream(t) => {
                if let Some(ty) = &mut t.element {
                    self.update_ty(ty);
                }
                if let Some(ty) = &mut t.end {
                    self.update_ty(ty);
                }
            }
            Type(t) => self.update_ty(t),

            // nothing to do for these as they're just names or empty
            Flags(_) | Enum(_) | Future(None) => {}
        }
    }

    fn update_ty(&self, ty: &mut Type) {
        if let Type::Id(id) = ty {
            *id = self.type_map[id.index()].expect("should visit in topo-order");
        }
    }

    fn update_interface(&self, iface: &mut Interface) {
        for (_, ty) in iface.types.iter_mut() {
            *ty = self.type_map[ty.index()].expect("types should all be visited");
        }
        for func in iface.functions.iter_mut() {
            for (_, ty) in func.params.iter_mut() {
                self.update_ty(ty);
            }
            match &mut func.results {
                Results::Named(named) => {
                    for (_, ty) in named.iter_mut() {
                        self.update_ty(ty);
                    }
                }
                Results::Anon(ty) => self.update_ty(ty),
            }
        }
    }

    fn update_world(&self, world: &mut World) {
        if let Some(i) = &mut world.default {
            *i = self.iface_map[i.index()];
        }
        for (_, i) in world.imports.iter_mut().chain(&mut world.exports) {
            *i = self.iface_map[i.index()];
        }
    }
}
