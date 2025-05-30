use super::EncodingState;
use anyhow::Result;
use std::collections::HashMap;
use wasm_encoder::*;
use wit_parser::{
    Enum, Flags, Function, Handle, InterfaceId, Record, Resolve, Result_, Tuple, Type, TypeDefKind,
    TypeId, TypeOwner, Variant,
};

/// Represents a key type for interface function definitions.
#[derive(Hash, PartialEq, Eq, Clone)]
pub struct FunctionKey<'a> {
    params: &'a [(String, Type)],
    result: &'a Option<Type>,
}

#[derive(Default, Clone)]
pub struct TypeEncodingMaps<'a> {
    pub id_to_index: HashMap<TypeId, u32>,
    pub def_to_index: HashMap<&'a TypeDefKind, u32>,
    pub func_type_map: HashMap<FunctionKey<'a>, u32>,
}

impl<'a> TypeEncodingMaps<'a> {
    /// Returns the index used to encode `id`, if it has been previously
    /// encoded.
    fn lookup(&self, resolve: &'a Resolve, id: TypeId) -> Option<u32> {
        // If this exact id has been encoded before, use that.
        if let Some(index) = self.id_to_index.get(&id) {
            return Some(*index);
        }

        // If this is an anonymous type, or one where the name of the type is
        // `none`, then consult the `def_to_index` map as well.
        let ty = &resolve.types[id];
        if ty.name.is_none() {
            if let Some(index) = self.def_to_index.get(&ty.kind) {
                return Some(*index);
            }
        }

        // Failing all that this hasn't previously been encoded.
        None
    }

    /// Inserts that `id` was encoded as `index`.
    fn insert(&mut self, resolve: &'a Resolve, id: TypeId, index: u32) {
        // Always record the id=>index mapping.
        self.id_to_index.insert(id, index);

        // Conditionally record the kind=>index mapping if the type is anonymous
        // and thus a candidate to use.
        let ty = &resolve.types[id];
        if ty.name.is_none() {
            self.def_to_index.insert(&ty.kind, index);
        }
    }
}

/// Support for encoding a wit-parser type into a component.
///
/// This is a `trait` to enable different implementations which define types
/// slightly differently in different contexts. For example types might be
/// defined within an instance type's index space or might be defined in the
/// component's root index space in a type section. The default trait methods
/// here are intended to assist in multiplexing over this difference.
pub trait ValtypeEncoder<'a> {
    /// Returns a new type encoder used to define a new type in this type
    /// section.
    ///
    /// The `u32` returned is the index of the type being defined in this type
    /// index space and the encoder returned must be used to define a type.
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>);

    /// Returns the index of a new function type and the encoder of where to
    /// place its results.
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>);

    /// Creates an export item for the specified type index.
    fn export_type(&mut self, index: u32, name: &'a str) -> Option<u32>;

    /// Creates a new `(type (sub resource))` export with the given name,
    /// returning the type index that refers to the fresh type created.
    fn export_resource(&mut self, name: &'a str) -> u32;

    /// Returns the encoding maps used to encoding types such as id-to-index
    /// maps.
    fn type_encoding_maps(&mut self) -> &mut TypeEncodingMaps<'a>;

    /// Imports `id` from a different interface, returning the index of the
    /// imported type into this index space.
    fn import_type(&mut self, interface: InterfaceId, id: TypeId) -> u32;

    /// Returns the identifier of the interface that generation is for.
    fn interface(&self) -> Option<InterfaceId>;

    /// Encodes a new function type which is defined within the provided
    /// document.
    fn encode_func_type(&mut self, resolve: &'a Resolve, func: &'a Function) -> Result<u32> {
        let key = FunctionKey {
            params: &func.params,
            result: &func.result,
        };
        if let Some(index) = self.type_encoding_maps().func_type_map.get(&key) {
            return Ok(*index);
        }

        // Encode all referenced parameter types from this function.
        let params: Vec<_> = self.encode_params(resolve, &func.params)?;

        let result = func
            .result
            .map(|ty| self.encode_valtype(resolve, &ty))
            .transpose()?;

        // Encode the function type
        let (index, mut f) = self.define_function_type();
        f.params(params).result(result);
        let prev = self.type_encoding_maps().func_type_map.insert(key, index);
        assert!(prev.is_none());
        Ok(index)
    }

    fn encode_params(
        &mut self,
        resolve: &'a Resolve,
        params: &'a [(String, Type)],
    ) -> Result<Vec<(&'a str, ComponentValType)>> {
        params
            .iter()
            .map(|(name, ty)| Ok((name.as_str(), self.encode_valtype(resolve, ty)?)))
            .collect::<Result<_>>()
    }

    /// Encodes the `ty`, defined within `resolve`, into this encoder and returns
    /// the corresponding `ComponentValType` that it represents.
    ///
    /// This will recursively define the entire structure of `ty` within `self`
    /// if necessary.
    fn encode_valtype(&mut self, resolve: &'a Resolve, ty: &Type) -> Result<ComponentValType> {
        Ok(match *ty {
            Type::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
            Type::U8 => ComponentValType::Primitive(PrimitiveValType::U8),
            Type::U16 => ComponentValType::Primitive(PrimitiveValType::U16),
            Type::U32 => ComponentValType::Primitive(PrimitiveValType::U32),
            Type::U64 => ComponentValType::Primitive(PrimitiveValType::U64),
            Type::S8 => ComponentValType::Primitive(PrimitiveValType::S8),
            Type::S16 => ComponentValType::Primitive(PrimitiveValType::S16),
            Type::S32 => ComponentValType::Primitive(PrimitiveValType::S32),
            Type::S64 => ComponentValType::Primitive(PrimitiveValType::S64),
            Type::F32 => ComponentValType::Primitive(PrimitiveValType::F32),
            Type::F64 => ComponentValType::Primitive(PrimitiveValType::F64),
            Type::Char => ComponentValType::Primitive(PrimitiveValType::Char),
            Type::String => ComponentValType::Primitive(PrimitiveValType::String),
            Type::ErrorContext => ComponentValType::Primitive(PrimitiveValType::ErrorContext),
            Type::Id(id) => {
                // If this id has already been prior defined into this section
                // refer to that definition.
                if let Some(index) = self.type_encoding_maps().lookup(resolve, id) {
                    return Ok(ComponentValType::Type(index));
                }

                let ty = &resolve.types[id];

                // If this type is imported from another interface then return
                // it as it was bound here with an alias.
                log::trace!("encode type name={:?} {:?}", ty.name, &ty.kind);
                if let Some(index) = self.maybe_import_type(resolve, id) {
                    self.type_encoding_maps().insert(resolve, id, index);
                    return Ok(ComponentValType::Type(index));
                }

                // ... and failing all that insert the type export.
                let mut encoded = match &ty.kind {
                    TypeDefKind::Record(r) => self.encode_record(resolve, r)?,
                    TypeDefKind::Tuple(t) => self.encode_tuple(resolve, t)?,
                    TypeDefKind::Flags(r) => self.encode_flags(r)?,
                    TypeDefKind::Variant(v) => self.encode_variant(resolve, v)?,
                    TypeDefKind::Option(t) => self.encode_option(resolve, t)?,
                    TypeDefKind::Result(r) => self.encode_result(resolve, r)?,
                    TypeDefKind::Enum(e) => self.encode_enum(e)?,
                    TypeDefKind::List(ty) => {
                        let ty = self.encode_valtype(resolve, ty)?;
                        let (index, encoder) = self.defined_type();
                        encoder.list(ty);
                        ComponentValType::Type(index)
                    }
                    TypeDefKind::FixedSizeList(ty, elements) => {
                        let ty = self.encode_valtype(resolve, ty)?;
                        let (index, encoder) = self.defined_type();
                        encoder.fixed_size_list(ty, *elements);
                        ComponentValType::Type(index)
                    }
                    TypeDefKind::Type(ty) => self.encode_valtype(resolve, ty)?,
                    TypeDefKind::Future(ty) => self.encode_future(resolve, ty)?,
                    TypeDefKind::Stream(ty) => self.encode_stream(resolve, ty)?,
                    TypeDefKind::Unknown => unreachable!(),
                    TypeDefKind::Resource => {
                        let name = ty.name.as_ref().expect("resources must be named");
                        let index = self.export_resource(name);
                        self.type_encoding_maps().id_to_index.insert(id, index);
                        return Ok(ComponentValType::Type(index));
                    }
                    TypeDefKind::Handle(Handle::Own(id)) => {
                        let ty = match self.encode_valtype(resolve, &Type::Id(*id))? {
                            ComponentValType::Type(index) => index,
                            _ => panic!("must be an indexed type"),
                        };
                        let (index, encoder) = self.defined_type();
                        encoder.own(ty);
                        ComponentValType::Type(index)
                    }
                    TypeDefKind::Handle(Handle::Borrow(id)) => {
                        let ty = match self.encode_valtype(resolve, &Type::Id(*id))? {
                            ComponentValType::Type(index) => index,
                            _ => panic!("must be an indexed type"),
                        };
                        let (index, encoder) = self.defined_type();
                        encoder.borrow(ty);
                        ComponentValType::Type(index)
                    }
                };

                if let Some(name) = &ty.name {
                    let index = match encoded {
                        ComponentValType::Type(index) => index,
                        ComponentValType::Primitive(ty) => {
                            // Named primitive types need entries in the type
                            // section, so convert this to a type reference
                            let (index, encoder) = self.defined_type();
                            encoder.primitive(ty);
                            index
                        }
                    };
                    let index = self.export_type(index, name).unwrap_or(index);

                    encoded = ComponentValType::Type(index);
                }

                if let ComponentValType::Type(index) = encoded {
                    self.type_encoding_maps().insert(resolve, id, index);
                }

                encoded
            }
        })
    }

    /// Optionally imports `id` from a different interface, returning the index
    /// of the imported type into this index space.
    ///
    /// Returns `None` if `id` can't be imported.
    fn maybe_import_type(&mut self, resolve: &Resolve, id: TypeId) -> Option<u32> {
        let ty = &resolve.types[id];
        let owner = match ty.owner {
            TypeOwner::Interface(i) => i,
            _ => return None,
        };
        if Some(owner) == self.interface() {
            return None;
        }
        Some(self.import_type(owner, id))
    }

    fn encode_optional_valtype(
        &mut self,
        resolve: &'a Resolve,
        ty: Option<&Type>,
    ) -> Result<Option<ComponentValType>> {
        match ty {
            Some(ty) => self.encode_valtype(resolve, ty).map(Some),
            None => Ok(None),
        }
    }

    fn encode_record(&mut self, resolve: &'a Resolve, record: &Record) -> Result<ComponentValType> {
        let fields = record
            .fields
            .iter()
            .map(|f| Ok((f.name.as_str(), self.encode_valtype(resolve, &f.ty)?)))
            .collect::<Result<Vec<_>>>()?;

        let (index, encoder) = self.defined_type();
        encoder.record(fields);
        Ok(ComponentValType::Type(index))
    }

    fn encode_tuple(&mut self, resolve: &'a Resolve, tuple: &Tuple) -> Result<ComponentValType> {
        let tys = tuple
            .types
            .iter()
            .map(|ty| self.encode_valtype(resolve, ty))
            .collect::<Result<Vec<_>>>()?;
        let (index, encoder) = self.defined_type();
        encoder.tuple(tys);
        Ok(ComponentValType::Type(index))
    }

    fn encode_flags(&mut self, flags: &Flags) -> Result<ComponentValType> {
        let (index, encoder) = self.defined_type();
        encoder.flags(flags.flags.iter().map(|f| f.name.as_str()));
        Ok(ComponentValType::Type(index))
    }

    fn encode_variant(
        &mut self,
        resolve: &'a Resolve,
        variant: &Variant,
    ) -> Result<ComponentValType> {
        let cases = variant
            .cases
            .iter()
            .map(|c| {
                Ok((
                    c.name.as_str(),
                    self.encode_optional_valtype(resolve, c.ty.as_ref())?,
                    None, // TODO: support defaulting case values in the future
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        let (index, encoder) = self.defined_type();
        encoder.variant(cases);
        Ok(ComponentValType::Type(index))
    }

    fn encode_option(&mut self, resolve: &'a Resolve, payload: &Type) -> Result<ComponentValType> {
        let ty = self.encode_valtype(resolve, payload)?;
        let (index, encoder) = self.defined_type();
        encoder.option(ty);
        Ok(ComponentValType::Type(index))
    }

    fn encode_result(
        &mut self,
        resolve: &'a Resolve,
        result: &Result_,
    ) -> Result<ComponentValType> {
        let ok = self.encode_optional_valtype(resolve, result.ok.as_ref())?;
        let error = self.encode_optional_valtype(resolve, result.err.as_ref())?;
        let (index, encoder) = self.defined_type();
        encoder.result(ok, error);
        Ok(ComponentValType::Type(index))
    }

    fn encode_enum(&mut self, enum_: &Enum) -> Result<ComponentValType> {
        let (index, encoder) = self.defined_type();
        encoder.enum_type(enum_.cases.iter().map(|c| c.name.as_str()));
        Ok(ComponentValType::Type(index))
    }

    fn encode_future(
        &mut self,
        resolve: &'a Resolve,
        payload: &Option<Type>,
    ) -> Result<ComponentValType> {
        let ty = self.encode_optional_valtype(resolve, payload.as_ref())?;
        let (index, encoder) = self.defined_type();
        encoder.future(ty);
        Ok(ComponentValType::Type(index))
    }

    fn encode_stream(
        &mut self,
        resolve: &'a Resolve,
        payload: &Option<Type>,
    ) -> Result<ComponentValType> {
        let ty = self.encode_optional_valtype(resolve, payload.as_ref())?;
        let (index, encoder) = self.defined_type();
        encoder.stream(ty);
        Ok(ComponentValType::Type(index))
    }
}

pub struct RootTypeEncoder<'state, 'a> {
    pub state: &'state mut EncodingState<'a>,
    pub interface: Option<InterfaceId>,
    pub import_types: bool,
}

impl<'a> ValtypeEncoder<'a> for RootTypeEncoder<'_, 'a> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        self.state.component.type_defined()
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        self.state.component.type_function()
    }
    fn interface(&self) -> Option<InterfaceId> {
        self.interface
    }
    fn export_type(&mut self, idx: u32, name: &'a str) -> Option<u32> {
        // When encoding types for the root the root component will export
        // this type, but when encoding types for a targeted interface then we
        // can't export types just yet. Interfaces will be created as an
        // instance with a bag-of-exports construction which can't refer to its
        // own types.
        if self.interface.is_none() {
            Some(if self.import_types {
                self.state
                    .component
                    .import(name, ComponentTypeRef::Type(TypeBounds::Eq(idx)))
            } else {
                self.state
                    .component
                    .export(name, ComponentExportKind::Type, idx, None)
            })
        } else {
            assert!(!self.import_types);
            None
        }
    }
    fn export_resource(&mut self, name: &'a str) -> u32 {
        assert!(self.interface.is_none());
        assert!(self.import_types);
        self.state
            .component
            .import(name, ComponentTypeRef::Type(TypeBounds::SubResource))
    }
    fn import_type(&mut self, interface: InterfaceId, id: TypeId) -> u32 {
        if !self.import_types {
            if let Some(cur) = self.interface {
                let set = &self.state.info.exports_used[&cur];
                if set.contains(&interface) {
                    return self.state.alias_exported_type(interface, id);
                }
            }
        }
        self.state.alias_imported_type(interface, id)
    }
    fn type_encoding_maps(&mut self) -> &mut TypeEncodingMaps<'a> {
        if self.import_types {
            &mut self.state.import_type_encoding_maps
        } else {
            &mut self.state.export_type_encoding_maps
        }
    }
}

pub struct InstanceTypeEncoder<'state, 'a> {
    pub state: &'state mut EncodingState<'a>,
    pub interface: InterfaceId,
    pub type_encoding_maps: TypeEncodingMaps<'a>,
    pub ty: InstanceType,
}

impl<'a> ValtypeEncoder<'a> for InstanceTypeEncoder<'_, 'a> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        (self.ty.type_count(), self.ty.ty().defined_type())
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        (self.ty.type_count(), self.ty.ty().function())
    }
    fn export_type(&mut self, idx: u32, name: &str) -> Option<u32> {
        let ret = self.ty.type_count();
        self.ty
            .export(name, ComponentTypeRef::Type(TypeBounds::Eq(idx)));
        Some(ret)
    }
    fn export_resource(&mut self, name: &str) -> u32 {
        let ret = self.ty.type_count();
        self.ty
            .export(name, ComponentTypeRef::Type(TypeBounds::SubResource));
        ret
    }
    fn type_encoding_maps(&mut self) -> &mut TypeEncodingMaps<'a> {
        &mut self.type_encoding_maps
    }
    fn interface(&self) -> Option<InterfaceId> {
        Some(self.interface)
    }
    fn import_type(&mut self, interface: InterfaceId, id: TypeId) -> u32 {
        self.ty.alias(Alias::Outer {
            count: 1,
            index: self.state.alias_imported_type(interface, id),
            kind: ComponentOuterAliasKind::Type,
        });
        self.ty.type_count() - 1
    }
}
