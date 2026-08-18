use crate::encoding::types::{TypeEncodingMaps, ValtypeEncoder, extern_name};
use anyhow::Result;
use indexmap::IndexSet;
use std::collections::HashMap;
use std::mem;
use wasm_encoder::*;
use wit_parser::*;

/// Encodes the given `package` within `resolve` to a binary WebAssembly
/// representation.
///
/// This function is the root of the implementation of serializing a WIT package
/// into a WebAssembly representation. The wasm representation serves two
/// purposes:
///
/// * One is to be a binary encoding of a WIT document which is ideally more
///   stable than the WIT textual format itself.
/// * Another is to provide a clear mapping of all WIT features into the
///   component model through use of its binary representation.
///
/// The `resolve` provided is a set of packages and types and such and the
/// `package` argument is an ID within the world provided. The documents within
/// `package` will all be encoded into the binary returned.
///
/// The binary returned can be [`decode`d](crate::decode) to recover the WIT
/// package provided.
pub fn encode(resolve: &Resolve, package: PackageId) -> Result<Vec<u8>> {
    let mut component = encode_component(resolve, package)?;
    component.raw_custom_section(&crate::base_producers().raw_custom_section());
    Ok(component.finish())
}

/// Encodes the given `package` within `resolve` to a binary WebAssembly
/// representation.
///
/// This function is the root of the implementation of serializing a WIT package
/// into a WebAssembly representation. The wasm representation serves two
/// purposes:
///
/// * One is to be a binary encoding of a WIT document which is ideally more
///   stable than the WIT textual format itself.
/// * Another is to provide a clear mapping of all WIT features into the
///   component model through use of its binary representation.
///
/// The `resolve` provided is a set of packages and types and such and the
/// `package` argument is an ID within the world provided. The documents within
/// `package` will all be encoded into the binary returned.
///
/// The binary returned can be [`decode`d](crate::decode) to recover the WIT
/// package provided.
pub fn encode_component(resolve: &Resolve, package: PackageId) -> Result<ComponentBuilder> {
    let mut encoder = Encoder {
        component: ComponentBuilder::default(),
        resolve,
        package,
    };
    encoder.run()?;

    let package_metadata = PackageMetadata::extract(resolve, package);
    encoder.component.custom_section(&CustomSection {
        name: PackageMetadata::SECTION_NAME.into(),
        data: package_metadata.encode()?.into(),
    });

    Ok(encoder.component)
}

/// Encodes a `world` as a component type.
pub fn encode_world(resolve: &Resolve, world_id: WorldId) -> Result<ComponentType> {
    let mut component = InterfaceEncoder::new(resolve);
    let world = &resolve.worlds[world_id];
    log::trace!("encoding world {}", world.name);

    // Encode the imports
    for (key, import) in world.imports.iter() {
        log::trace!("encoding import {}", resolve.name_world_key(key));
        let ty = match import {
            WorldItem::Interface { id, .. } => {
                component.interface = Some(*id);
                let idx = component.encode_instance(*id)?;
                ComponentTypeRef::Instance(idx)
            }
            WorldItem::Function(f) => {
                component.interface = None;
                let idx = component.encode_func_type(resolve, f)?;
                ComponentTypeRef::Func(idx)
            }
            WorldItem::Type { id, .. } => {
                component.interface = None;
                component.import_types = true;
                component.encode_valtype(resolve, &Type::Id(*id))?;
                component.import_types = false;
                continue;
            }
        };
        component
            .outer
            .import(component_extern_name(resolve, key, import), ty);
    }
    // Encode the exports
    for (key, export) in world.exports.iter() {
        log::trace!("encoding export {}", resolve.name_world_key(key));
        let ty = match export {
            WorldItem::Interface { id, .. } => {
                component.interface = Some(*id);
                let idx = component.encode_instance(*id)?;
                ComponentTypeRef::Instance(idx)
            }
            WorldItem::Function(f) => {
                component.interface = None;
                let idx = component.encode_func_type(resolve, f)?;
                ComponentTypeRef::Func(idx)
            }
            WorldItem::Type { .. } => unreachable!(),
        };
        component
            .outer
            .export(component_extern_name(resolve, key, export), ty);
    }

    Ok(component.outer)
}

fn component_extern_name(
    resolve: &Resolve,
    key: &WorldKey,
    item: &WorldItem,
) -> wasm_encoder::ComponentExternName<'static> {
    ComponentExternName {
        name: resolve.name_world_key(key).into(),
        implements: resolve.implements_value(key, item).map(|s| s.into()),
        external_id: resolve.external_id_value(key, item).map(|s| s.into()),
        version_suffix: None,
    }
}

struct Encoder<'a> {
    component: ComponentBuilder,
    resolve: &'a Resolve,
    package: PackageId,
}

impl Encoder<'_> {
    fn run(&mut self) -> Result<()> {
        // Encode package-scope types first so V2 sniffing still sees a Label
        // as the first export name when types are present.
        {
            let mut encoder = PackageTypeEncoder {
                component: &mut self.component,
                package: self.package,
                type_encoding_maps: Default::default(),
                foreign_type_maps: Default::default(),
                foreign_declared: Default::default(),
            };
            for (_, &id) in self.resolve.packages[self.package].types.iter() {
                encoder.encode_valtype(self.resolve, &Type::Id(id))?;
            }
        }

        // Encode all interfaces as component types and then export them.
        for (name, &id) in self.resolve.packages[self.package].interfaces.iter() {
            let component_ty = self.encode_interface(id)?;
            let ty = self.component.type_component(Some(name), &component_ty);
            self.component
                .export(name, ComponentExportKind::Type, ty, None);
        }

        // For each `world` encode it directly as a component and then create a
        // wrapper component that exports that component.
        for (name, &world) in self.resolve.packages[self.package].worlds.iter() {
            let component_ty = encode_world(self.resolve, world)?;

            let world = &self.resolve.worlds[world];
            let mut wrapper = ComponentType::new();
            wrapper.ty().component(&component_ty);
            let pkg = &self.resolve.packages[world.package.unwrap()];
            wrapper.export(pkg.name.interface_id(name), ComponentTypeRef::Component(0));

            let ty = self.component.type_component(Some(name), &wrapper);
            self.component
                .export(name, ComponentExportKind::Type, ty, None);
        }

        Ok(())
    }

    fn encode_interface(&mut self, id: InterfaceId) -> Result<ComponentType> {
        // Build a set of interfaces reachable from this document, including the
        // interfaces in the document itself. This is used to import instances
        // into the component type we're encoding. Note that entire interfaces
        // are imported with all their types as opposed to just the needed types
        // in an interface for this document. That's done to assist with the
        // decoding process where everyone's view of a foreign document agrees
        // notably on the order that types are defined in to assist with
        // roundtripping.
        let mut interfaces = IndexSet::new();
        self.add_live_interfaces(&mut interfaces, id);

        // Seed the set of used names with all exported interfaces to ensure
        // that imported interfaces choose different names as the import names
        // aren't used during decoding.
        let mut used_names = IndexSet::new();
        for id in interfaces.iter() {
            let iface = &self.resolve.interfaces[*id];
            if iface.package == Some(self.package) {
                let first = used_names.insert(iface.name.as_ref().unwrap().clone());
                assert!(first);
            }
        }

        let mut encoder = InterfaceEncoder::new(self.resolve);
        for interface in interfaces {
            encoder.interface = Some(interface);
            let iface = &self.resolve.interfaces[interface];
            let name = self.resolve.id_of(interface).unwrap();
            if interface == id {
                let idx = encoder.encode_instance(interface)?;
                log::trace!("exporting self as {idx}");
                encoder.outer.export(name, ComponentTypeRef::Instance(idx));
            } else {
                encoder.push_instance();
                for (_, id) in iface.types.iter() {
                    encoder.encode_valtype(self.resolve, &Type::Id(*id))?;
                }
                let instance = encoder.pop_instance();
                let idx = encoder.outer.type_count();
                encoder.outer.ty().instance(&instance);
                encoder.import_map.insert(interface, encoder.instances);
                encoder.instances += 1;
                encoder.outer.import(name, ComponentTypeRef::Instance(idx));
            }
        }

        encoder.interface = None;

        Ok(encoder.outer)
    }

    /// Recursively add all live interfaces reachable from `id` into the
    /// `interfaces` set, and then add `id` to the set.
    fn add_live_interfaces(&self, interfaces: &mut IndexSet<InterfaceId>, id: InterfaceId) {
        if interfaces.contains(&id) {
            return;
        }
        for id in self.resolve.interface_direct_deps(id) {
            self.add_live_interfaces(interfaces, id);
        }
        assert!(interfaces.insert(id));
    }
}

struct InterfaceEncoder<'a> {
    resolve: &'a Resolve,
    outer: ComponentType,
    ty: Option<InstanceType>,
    type_encoding_maps: TypeEncodingMaps<'a>,
    saved_maps: Option<TypeEncodingMaps<'a>>,
    /// Encoding maps for the definitions restated in `outer` to bind
    /// package-scope type imports, kept separate from the maps of whichever
    /// scope is currently being encoded.
    package_type_maps: TypeEncodingMaps<'a>,
    import_map: HashMap<InterfaceId, u32>,
    outer_type_map: HashMap<TypeId, u32>,
    instances: u32,
    import_types: bool,
    interface: Option<InterfaceId>,
}

impl<'a> InterfaceEncoder<'a> {
    fn new(resolve: &Resolve) -> InterfaceEncoder<'_> {
        InterfaceEncoder {
            resolve,
            outer: ComponentType::new(),
            ty: None,
            type_encoding_maps: Default::default(),
            package_type_maps: Default::default(),
            import_map: Default::default(),
            outer_type_map: Default::default(),
            instances: 0,
            saved_maps: None,
            import_types: false,
            interface: None,
        }
    }

    /// Declares the package-scope type `id` in `outer`, returning the index of
    /// the `import` that binds it there.
    fn declare_package_type(&mut self, resolve: &'a Resolve, id: TypeId) -> Result<u32> {
        PackageTypeImporter {
            resolve,
            outer: &mut self.outer,
            type_encoding_maps: &mut self.package_type_maps,
            declared: &mut self.outer_type_map,
        }
        .declare(id)
    }

    fn encode_instance(&mut self, interface: InterfaceId) -> Result<u32> {
        self.push_instance();
        let iface = &self.resolve.interfaces[interface];
        let mut type_order = IndexSet::new();
        for (_, id) in iface.types.iter() {
            self.encode_valtype(self.resolve, &Type::Id(*id))?;
            type_order.insert(*id);
        }

        // Sort functions based on whether or not they're associated with
        // resources.
        //
        // This is done here to ensure that when a WIT package is printed as WIT
        // then decoded, or if it's printed as Wasm then decoded, the final
        // result is the same. When printing via WIT resource methods are
        // attached to the resource types themselves meaning that they'll appear
        // intermingled with the rest of the types, namely first before all
        // other functions. The purpose of this sort is to perform a stable sort
        // over all functions by shuffling the resource-related functions first,
        // in order of when their associated resource was encoded, and putting
        // freestanding functions last.
        //
        // Note that this is not actually required for correctness, it's
        // basically here to make fuzzing happy.
        let mut funcs = iface.functions.iter().collect::<Vec<_>>();
        funcs.sort_by_key(|(_name, func)| match func.kind.resource() {
            Some(id) => type_order.get_index_of(&id).unwrap(),
            None => type_order.len(),
        });

        for (name, func) in funcs {
            let ty = self.encode_func_type(self.resolve, func)?;
            self.ty.as_mut().unwrap().export(
                extern_name(name, func.external_id.as_deref()),
                ComponentTypeRef::Func(ty),
            );
        }
        let instance = self.pop_instance();
        let idx = self.outer.type_count();
        self.outer.ty().instance(&instance);
        self.import_map.insert(interface, self.instances);
        self.instances += 1;
        Ok(idx)
    }

    fn push_instance(&mut self) {
        assert!(self.ty.is_none());
        assert!(self.saved_maps.is_none());
        self.saved_maps = Some(mem::take(&mut self.type_encoding_maps));
        self.ty = Some(InstanceType::default());
    }

    fn pop_instance(&mut self) -> InstanceType {
        let maps = self.saved_maps.take().unwrap();
        self.type_encoding_maps = maps;
        mem::take(&mut self.ty).unwrap()
    }
}

impl<'a> ValtypeEncoder<'a> for InterfaceEncoder<'a> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        match &mut self.ty {
            Some(ty) => (ty.type_count(), ty.ty().defined_type()),
            None => (self.outer.type_count(), self.outer.ty().defined_type()),
        }
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        match &mut self.ty {
            Some(ty) => (ty.type_count(), ty.ty().function()),
            None => (self.outer.type_count(), self.outer.ty().function()),
        }
    }
    fn export_type(&mut self, index: u32, name: ComponentExternName<'a>) -> Option<u32> {
        match &mut self.ty {
            Some(ty) => {
                assert!(!self.import_types);
                let ret = ty.type_count();
                ty.export(name, ComponentTypeRef::Type(TypeBounds::Eq(index)));
                Some(ret)
            }
            None => {
                let ret = self.outer.type_count();
                if self.import_types {
                    self.outer
                        .import(name, ComponentTypeRef::Type(TypeBounds::Eq(index)));
                } else {
                    self.outer
                        .export(name, ComponentTypeRef::Type(TypeBounds::Eq(index)));
                }
                Some(ret)
            }
        }
    }
    fn export_resource(&mut self, name: ComponentExternName<'a>) -> u32 {
        let type_ref = ComponentTypeRef::Type(TypeBounds::SubResource);
        match &mut self.ty {
            Some(ty) => {
                assert!(!self.import_types);
                ty.export(name, type_ref);
                ty.type_count() - 1
            }
            None => {
                if self.import_types {
                    self.outer.import(name, type_ref);
                } else {
                    self.outer.export(name, type_ref);
                }
                self.outer.type_count() - 1
            }
        }
    }
    fn type_encoding_maps(&mut self) -> &mut TypeEncodingMaps<'a> {
        &mut self.type_encoding_maps
    }
    fn interface(&self) -> Option<InterfaceId> {
        self.interface
    }
    fn import_type(&mut self, owner: InterfaceId, id: TypeId) -> u32 {
        let ty = &self.resolve.types[id];
        let instance = self.import_map[&owner];
        let outer_idx = *self.outer_type_map.entry(id).or_insert_with(|| {
            let ret = self.outer.type_count();
            self.outer.alias(Alias::InstanceExport {
                instance,
                name: ty.name.as_ref().unwrap(),
                kind: ComponentExportKind::Type,
            });
            ret
        });
        self.alias_outer_type(outer_idx)
    }
    fn import_package_type(&mut self, resolve: &'a Resolve, id: TypeId) -> Result<Option<u32>> {
        let outer_idx = self.declare_package_type(resolve, id)?;
        Ok(Some(self.alias_outer_type(outer_idx)))
    }
}

impl InterfaceEncoder<'_> {
    /// Brings `outer_idx`, an index in the wrapping component-type, into the
    /// instance type currently being built, if any.
    fn alias_outer_type(&mut self, outer_idx: u32) -> u32 {
        match &mut self.ty {
            Some(ty) => {
                let ret = ty.type_count();
                ty.alias(Alias::Outer {
                    count: 1,
                    index: outer_idx,
                    kind: ComponentOuterAliasKind::Type,
                });
                ret
            }
            None => outer_idx,
        }
    }
}

/// An index space that a restated package-scope type definition, and the
/// `import` naming it, can be written into.
trait PackageTypeScope {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>);
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>);
    /// Imports a type bound with `eq` to `index`, returning the index of the
    /// imported type.
    fn import_eq(&mut self, name: ComponentExternName<'_>, index: u32) -> u32;
}

impl PackageTypeScope for ComponentType {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        (self.type_count(), self.ty().defined_type())
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        (self.type_count(), self.ty().function())
    }
    fn import_eq(&mut self, name: ComponentExternName<'_>, index: u32) -> u32 {
        let ret = self.type_count();
        self.import(name, ComponentTypeRef::Type(TypeBounds::Eq(index)));
        ret
    }
}

impl PackageTypeScope for ComponentBuilder {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        self.type_defined(None)
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        self.type_function(None)
    }
    fn import_eq(&mut self, name: ComponentExternName<'_>, index: u32) -> u32 {
        self.import(name, ComponentTypeRef::Type(TypeBounds::Eq(index)))
    }
}

/// Declares package-scope types that something depends on.
///
/// A package-scope type is encoded like a `use` of an `interface`, just without
/// the wrapping `instance` type: the definition is restated locally and then
/// imported under its fully-qualified `namespace:package/name`, bound with `eq`
/// so that nothing has to be supplied to satisfy the import.
struct PackageTypeImporter<'b, 'a, S> {
    resolve: &'a Resolve,
    outer: &'b mut S,
    type_encoding_maps: &'b mut TypeEncodingMaps<'a>,
    declared: &'b mut HashMap<TypeId, u32>,
}

impl<'a, S: PackageTypeScope> PackageTypeImporter<'_, 'a, S> {
    fn declare(&mut self, id: TypeId) -> Result<u32> {
        if let Some(index) = self.declared.get(&id) {
            return Ok(*index);
        }

        let resolve = self.resolve;
        let structure = match self.encode_typedef_structure(resolve, id)? {
            ComponentValType::Type(index) => index,
            // A named primitive still needs an entry in the type section for
            // the import's bound to refer to.
            ComponentValType::Primitive(ty) => {
                let (index, encoder) = self.defined_type();
                encoder.primitive(ty);
                index
            }
        };

        let ty = &resolve.types[id];
        let index = self.outer.import_eq(
            ComponentExternName {
                name: package_type_name(resolve, id).into(),
                implements: None,
                version_suffix: None,
                external_id: ty.external_id.clone().map(Into::into),
            },
            structure,
        );

        self.declared.insert(id, index);
        self.type_encoding_maps.insert(resolve, id, index);
        Ok(index)
    }
}

impl<'a, S: PackageTypeScope> ValtypeEncoder<'a> for PackageTypeImporter<'_, 'a, S> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        self.outer.defined_type()
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        self.outer.define_function_type()
    }
    fn export_type(&mut self, _index: u32, _name: ComponentExternName<'a>) -> Option<u32> {
        // Definitions restated here are named by the `import` that binds them,
        // so the index of the definition itself is what callers want.
        None
    }
    fn export_resource(&mut self, _name: ComponentExternName<'a>) -> u32 {
        unreachable!("package-scope resources are not allowed")
    }
    fn type_encoding_maps(&mut self) -> &mut TypeEncodingMaps<'a> {
        self.type_encoding_maps
    }
    fn interface(&self) -> Option<InterfaceId> {
        None
    }
    fn import_type(&mut self, _owner: InterfaceId, _id: TypeId) -> u32 {
        unreachable!("package-scope types cannot refer to interface types")
    }
    fn import_package_type(&mut self, _resolve: &'a Resolve, id: TypeId) -> Result<Option<u32>> {
        self.declare(id).map(Some)
    }
}

/// The fully-qualified `namespace:package/name` of a package-scope type.
fn package_type_name(resolve: &Resolve, id: TypeId) -> String {
    let ty = &resolve.types[id];
    let package = match ty.owner {
        TypeOwner::Package(package) => package,
        _ => unreachable!("not a package-scope type"),
    };
    let name = ty.name.as_ref().expect("package-scope types are named");
    resolve.packages[package].name.interface_id(name)
}

/// Encodes a package's own package-scope types as type exports of the
/// package's component.
struct PackageTypeEncoder<'b, 'a> {
    component: &'b mut ComponentBuilder,
    package: PackageId,
    type_encoding_maps: TypeEncodingMaps<'a>,
    /// Encoding maps for the definitions restated to bind imports of
    /// package-scope types from other packages, kept separate from the maps of
    /// the definitions this package exports.
    foreign_type_maps: TypeEncodingMaps<'a>,
    foreign_declared: HashMap<TypeId, u32>,
}

impl<'a> ValtypeEncoder<'a> for PackageTypeEncoder<'_, 'a> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        self.component.type_defined(None)
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        self.component.type_function(None)
    }
    fn export_type(&mut self, index: u32, name: ComponentExternName<'a>) -> Option<u32> {
        Some(
            self.component
                .export(name, ComponentExportKind::Type, index, None),
        )
    }
    fn export_resource(&mut self, _name: ComponentExternName<'a>) -> u32 {
        unreachable!("package-scope resources are not allowed")
    }
    fn type_encoding_maps(&mut self) -> &mut TypeEncodingMaps<'a> {
        &mut self.type_encoding_maps
    }
    fn interface(&self) -> Option<InterfaceId> {
        None
    }
    fn import_type(&mut self, _owner: InterfaceId, _id: TypeId) -> u32 {
        unreachable!("package-scope types do not import from interfaces")
    }
    fn import_package_type(&mut self, resolve: &'a Resolve, id: TypeId) -> Result<Option<u32>> {
        // This encoder is defining the package's own types, so those are
        // defined and exported rather than imported.
        if resolve.types[id].owner == TypeOwner::Package(self.package) {
            return Ok(None);
        }

        // A definition from another package has no wrapping component-type to
        // hold the `import` naming it, so it goes on the package's component
        // directly.
        PackageTypeImporter {
            resolve,
            outer: &mut *self.component,
            type_encoding_maps: &mut self.foreign_type_maps,
            declared: &mut self.foreign_declared,
        }
        .declare(id)
        .map(Some)
    }
}
