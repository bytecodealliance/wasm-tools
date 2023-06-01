use super::EncodingState;
use anyhow::Result;
use std::collections::HashMap;
use wasm_encoder::*;
use wit_parser::{
    Enum, Flags, Function, InterfaceId, Params, Record, Resolve, Result_, Results, Tuple, Type,
    TypeDefKind, TypeId, TypeOwner, Union, Variant,
};

/// Represents a key type for interface function definitions.
#[derive(Hash, PartialEq, Eq)]
pub struct FunctionKey<'a> {
    params: &'a Params,
    results: &'a Results,
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

    /// Returns a map of all types previously defined in this type index space.
    fn type_map(&mut self) -> &mut HashMap<TypeId, u32>;

    /// Imports `id` from a different interface, returning the index of the
    /// imported type into this index space.
    fn import_type(&mut self, interface: InterfaceId, id: TypeId) -> u32;

    /// Returns the identifier of the interface that generation is for.
    fn interface(&self) -> Option<InterfaceId>;

    /// Returns the map of already-defined function types in this type index
    /// space.
    fn func_type_map(&mut self) -> &mut HashMap<FunctionKey<'a>, u32>;

    /// Encodes a new function type which is defined within the provided
    /// document.
    fn encode_func_type(&mut self, resolve: &'a Resolve, func: &'a Function) -> Result<u32> {
        let key = FunctionKey {
            params: &func.params,
            results: &func.results,
        };
        if let Some(index) = self.func_type_map().get(&key) {
            return Ok(*index);
        }

        // Encode all referenced parameter types from this function.
        let params: Vec<_> = self.encode_params(resolve, &func.params)?;

        enum EncodedResults<'a> {
            Named(Vec<(&'a str, ComponentValType)>),
            Anon(ComponentValType),
        }

        let results = match &func.results {
            Results::Named(rs) => EncodedResults::Named(self.encode_params(resolve, rs)?),
            Results::Anon(ty) => EncodedResults::Anon(self.encode_valtype(resolve, ty)?),
        };

        // Encode the function type
        let (index, mut f) = self.define_function_type();
        f.params(params);
        match results {
            EncodedResults::Named(rs) => f.results(rs),
            EncodedResults::Anon(ty) => f.result(ty),
        };
        let prev = self.func_type_map().insert(key, index);
        assert!(prev.is_none());
        Ok(index)
    }

    fn encode_params(
        &mut self,
        resolve: &'a Resolve,
        params: &'a Params,
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
            Type::Float32 => ComponentValType::Primitive(PrimitiveValType::Float32),
            Type::Float64 => ComponentValType::Primitive(PrimitiveValType::Float64),
            Type::Char => ComponentValType::Primitive(PrimitiveValType::Char),
            Type::String => ComponentValType::Primitive(PrimitiveValType::String),
            Type::Id(id) => {
                // If this id has already been prior defined into this section
                // refer to that definition.
                if let Some(index) = self.type_map().get(&id) {
                    return Ok(ComponentValType::Type(*index));
                }

                // If this type is imported from another interface then return
                // it as it was bound here with an alias.
                let ty = &resolve.types[id];
                log::trace!("encode type name={:?}", ty.name);
                if let Some(index) = self.maybe_import_type(resolve, id) {
                    self.type_map().insert(id, index);
                    return Ok(ComponentValType::Type(index));
                }

                // ... and failing all that insert the type export.
                let mut encoded = match &ty.kind {
                    TypeDefKind::Record(r) => self.encode_record(resolve, r)?,
                    TypeDefKind::Tuple(t) => self.encode_tuple(resolve, t)?,
                    TypeDefKind::Flags(r) => self.encode_flags(r)?,
                    TypeDefKind::Variant(v) => self.encode_variant(resolve, v)?,
                    TypeDefKind::Union(u) => self.encode_union(resolve, u)?,
                    TypeDefKind::Option(t) => self.encode_option(resolve, t)?,
                    TypeDefKind::Result(r) => self.encode_result(resolve, r)?,
                    TypeDefKind::Enum(e) => self.encode_enum(e)?,
                    TypeDefKind::List(ty) => {
                        let ty = self.encode_valtype(resolve, ty)?;
                        let (index, encoder) = self.defined_type();
                        encoder.list(ty);
                        ComponentValType::Type(index)
                    }
                    TypeDefKind::Type(ty) => self.encode_valtype(resolve, ty)?,
                    TypeDefKind::Future(_) => todo!("encoding for future type"),
                    TypeDefKind::Stream(_) => todo!("encoding for stream type"),
                    TypeDefKind::Unknown => unreachable!(),
                    TypeDefKind::Resource(_) => todo!(),
                    TypeDefKind::Handle(_) => todo!(),
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
                    self.type_map().insert(id, index);
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

    fn encode_union(&mut self, resolve: &'a Resolve, union: &Union) -> Result<ComponentValType> {
        let tys = union
            .cases
            .iter()
            .map(|c| self.encode_valtype(resolve, &c.ty))
            .collect::<Result<Vec<_>>>()?;

        let (index, encoder) = self.defined_type();
        encoder.union(tys);
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
}

pub struct RootTypeEncoder<'state, 'a> {
    pub state: &'state mut EncodingState<'a>,
    pub interface: Option<InterfaceId>,
    pub import_types: bool,

    // These maps are used when `interface` is set to `Some` as all the type
    // information will be per-interface.
    pub type_map: HashMap<TypeId, u32>,
    pub func_type_map: HashMap<FunctionKey<'a>, u32>,
}

impl<'a> ValtypeEncoder<'a> for RootTypeEncoder<'_, 'a> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        self.state.component.defined_type()
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        self.state.component.function_type()
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
    fn import_type(&mut self, _: InterfaceId, id: TypeId) -> u32 {
        self.state.index_of_type_export(id)
    }
    fn type_map(&mut self) -> &mut HashMap<TypeId, u32> {
        if self.interface.is_some() {
            &mut self.type_map
        } else {
            &mut self.state.type_map
        }
    }
    fn func_type_map(&mut self) -> &mut HashMap<FunctionKey<'a>, u32> {
        if self.interface.is_some() {
            &mut self.func_type_map
        } else {
            &mut self.state.func_type_map
        }
    }
}

pub struct InstanceTypeEncoder<'state, 'a> {
    pub state: &'state mut EncodingState<'a>,
    pub interface: InterfaceId,
    pub type_map: HashMap<TypeId, u32>,
    pub func_type_map: HashMap<FunctionKey<'a>, u32>,
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
    fn type_map(&mut self) -> &mut HashMap<TypeId, u32> {
        &mut self.type_map
    }
    fn interface(&self) -> Option<InterfaceId> {
        Some(self.interface)
    }
    fn import_type(&mut self, _: InterfaceId, id: TypeId) -> u32 {
        self.ty.alias(Alias::Outer {
            count: 1,
            index: self.state.index_of_type_export(id),
            kind: ComponentOuterAliasKind::Type,
        });
        self.ty.type_count() - 1
    }
    fn func_type_map(&mut self) -> &mut HashMap<FunctionKey<'a>, u32> {
        &mut self.func_type_map
    }
}
