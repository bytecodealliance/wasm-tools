use crate::builder::ComponentBuilder;
use crate::encoding::types::{FunctionKey, ValtypeEncoder};
use anyhow::Result;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::mem;
use url::Url;
use wasm_encoder::*;
use wit_parser::*;

/// TODO
pub fn encode(resolve: &Resolve, package: PackageId) -> Result<Vec<u8>> {
    let mut encoder = Encoder {
        component: ComponentBuilder::default(),
        resolve,
        package,
    };
    encoder.run()?;
    Ok(encoder.component.finish())
}

struct Encoder<'a> {
    component: ComponentBuilder,
    resolve: &'a Resolve,
    package: PackageId,
}

impl Encoder<'_> {
    fn run(&mut self) -> Result<()> {
        for (name, doc) in self.resolve.packages[self.package].documents.iter() {
            let ty = self.encode_document(*doc)?;
            self.component
                .export(name, "", ComponentExportKind::Type, ty);
        }
        Ok(())
    }

    fn encode_document(&mut self, doc: DocumentId) -> Result<u32> {
        let mut set = LiveTypes::default();

        // Note that worlds within `doc` are explicitly excluded here since
        // worlds have their own scope and import everything themselves, this
        // transitive import set is only required for the interfaces defined in
        // the document.
        for (_, id) in self.resolve.documents[doc].interfaces.iter() {
            set.add_interface(self.resolve, *id);
        }

        let mut imported_interfaces = IndexMap::new();
        for ty in set.iter() {
            let owner = match &self.resolve.types[ty].owner {
                TypeOwner::Interface(i) => *i,
                _ => continue,
            };
            if self.resolve.interfaces[owner].document == doc {
                continue;
            }
            imported_interfaces
                .entry(owner)
                .or_insert(Vec::new())
                .push(ty);
        }

        let mut encoder = InterfaceEncoder::new(self.resolve);
        for (owner, ids) in imported_interfaces {
            let owner_iface = &self.resolve.interfaces[owner];
            for id in ids {
                encoder.encode_valtype(self.resolve, &Type::Id(id))?;
            }
            let instance = encoder.take_instance();
            let idx = encoder.outer.type_count();
            encoder.outer.ty().instance(&instance);
            encoder.import_map.insert(owner, encoder.instances);
            encoder.instances += 1;

            let import_name = owner_iface.name.as_ref().unwrap();
            let url = self.url_of(owner);
            encoder
                .outer
                .import(import_name, &url, ComponentTypeRef::Instance(idx));
        }

        let doc = &self.resolve.documents[doc];
        for (name, interface) in doc.interfaces.iter() {
            let idx = encoder.encode_instance(*interface)?;
            encoder
                .outer
                .export(name, "", ComponentTypeRef::Instance(idx));
        }

        for world in doc.worlds.iter() {
            let world = &self.resolve.worlds[*world];
            let mut component = InterfaceEncoder::new(self.resolve);
            for (name, import) in world.imports.iter() {
                let (url, ty) = match import {
                    WorldItem::Interface(i) => {
                        let idx = component.encode_instance(*i)?;
                        (self.url_of(*i), ComponentTypeRef::Instance(idx))
                    }
                    WorldItem::Function(_) => panic!(),
                };
                component.outer.import(name, &url, ty);
            }
            for (name, export) in world.exports.iter() {
                let (url, ty) = match export {
                    WorldItem::Interface(i) => {
                        let idx = component.encode_instance(*i)?;
                        (self.url_of(*i), ComponentTypeRef::Instance(idx))
                    }
                    WorldItem::Function(_) => panic!(),
                };
                component.outer.export(name, &url, ty);
            }
            let idx = encoder.outer.type_count();
            encoder.outer.ty().component(&component.outer);
            encoder
                .outer
                .export(&world.name, "", ComponentTypeRef::Component(idx));
        }

        Ok(self.component.component_type(&encoder.outer))
    }

    fn url_of(&self, interface: InterfaceId) -> String {
        let iface = &self.resolve.interfaces[interface];
        let iface_name = match &iface.name {
            Some(name) => name,
            None => return String::new(),
        };
        let doc = &self.resolve.documents[iface.document];
        let pkg = doc.package.unwrap();
        let mut base = if pkg == self.package {
            Url::parse("pkg:/").unwrap()
        } else {
            let pkg = &self.resolve.packages[pkg];
            Url::parse(pkg.url.as_ref().unwrap()).unwrap()
        };
        let mut segments = base.path_segments_mut().unwrap();
        segments.push(&doc.name);
        segments.push(iface_name);
        drop(segments);
        base.to_string()
    }
}

struct InterfaceEncoder<'a> {
    resolve: &'a Resolve,
    outer: ComponentType,
    ty: InstanceType,
    func_type_map: HashMap<FunctionKey<'a>, u32>,
    type_map: HashMap<TypeId, u32>,
    import_map: HashMap<InterfaceId, u32>,
    outer_type_map: HashMap<TypeId, u32>,
    instances: u32,
}

impl InterfaceEncoder<'_> {
    fn new(resolve: &Resolve) -> InterfaceEncoder<'_> {
        InterfaceEncoder {
            resolve,
            outer: ComponentType::new(),
            ty: InstanceType::new(),
            type_map: Default::default(),
            func_type_map: Default::default(),
            import_map: Default::default(),
            outer_type_map: Default::default(),
            instances: 0,
        }
    }

    fn encode_instance(&mut self, interface: InterfaceId) -> Result<u32> {
        let iface = &self.resolve.interfaces[interface];
        for (_, id) in iface.types.iter() {
            self.encode_valtype(self.resolve, &Type::Id(*id))?;
        }
        for (name, func) in iface.functions.iter() {
            let ty = self.encode_func_type(self.resolve, func)?;
            self.ty.export(name, "", ComponentTypeRef::Func(ty));
        }
        let instance = self.take_instance();
        let idx = self.outer.type_count();
        self.outer.ty().instance(&instance);
        self.import_map.insert(interface, self.instances);
        self.instances += 1;
        Ok(idx)
    }

    fn take_instance(&mut self) -> InstanceType {
        self.func_type_map.clear();
        self.type_map.clear();
        mem::take(&mut self.ty)
    }
}

impl<'a> ValtypeEncoder<'a> for InterfaceEncoder<'a> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        (self.ty.type_count(), self.ty.ty().defined_type())
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        (self.ty.type_count(), self.ty.ty().function())
    }
    fn export_type(&mut self, index: u32, name: &'a str) -> u32 {
        let ret = self.ty.type_count();
        self.ty
            .export(name, "", ComponentTypeRef::Type(TypeBounds::Eq, index));
        ret
    }
    fn type_map(&mut self) -> &mut HashMap<TypeId, u32> {
        &mut self.type_map
    }
    fn maybe_import_type(&mut self, id: TypeId) -> Option<u32> {
        let ty = &self.resolve.types[id];
        let owner = match ty.owner {
            TypeOwner::Interface(i) => i,
            _ => return None,
        };
        let instance = *self.import_map.get(&owner)?;
        let outer_idx = *self.outer_type_map.entry(id).or_insert_with(|| {
            let ret = self.outer.type_count();
            self.outer.alias(Alias::InstanceExport {
                instance,
                name: ty.name.as_ref().unwrap(),
                kind: ComponentExportKind::Type,
            });
            ret
        });
        let ret = self.ty.type_count();
        self.ty.alias(Alias::Outer {
            count: 1,
            index: outer_idx,
            kind: ComponentOuterAliasKind::Type,
        });
        Some(ret)
    }
    fn func_type_map(&mut self) -> &mut HashMap<FunctionKey<'a>, u32> {
        &mut self.func_type_map
    }
}
