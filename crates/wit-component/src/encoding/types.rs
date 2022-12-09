use super::EncodingState;
use anyhow::Result;
use std::collections::HashMap;
use wasm_encoder::*;
use wit_parser::{
    Document, Enum, Flags, Function, InterfaceId, Params, Record, Result_, Results, Tuple, Type,
    TypeDefKind, TypeId, Union, Variant,
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

    /// Aliases the provided type as a new type using an outer depth of 0.
    fn define_type_alias_self(&mut self, ty: u32) -> u32;

    /// Creates an export item for the specified type index.
    fn export_type(&mut self, index: u32, name: &'a str);

    /// Returns a map of all types previously defined in this type index space.
    fn type_map(&mut self) -> &mut HashMap<TypeId, u32>;

    /// Optionally imports `id` from a different interface, returning the index
    /// of the imported type into this index space.
    ///
    /// Returns `None` if `id can't be imported.
    fn maybe_import_type(&mut self, id: TypeId) -> Option<u32>;

    /// Returns the map of already-defined function types in this type index
    /// space.
    fn func_type_map(&mut self) -> &mut HashMap<FunctionKey<'a>, u32>;

    /// Encodes a new function type which is defined within the provided
    /// document.
    fn encode_func_type(&mut self, doc: &'a Document, func: &'a Function) -> Result<u32> {
        let key = FunctionKey {
            params: &func.params,
            results: &func.results,
        };
        if let Some(index) = self.func_type_map().get(&key) {
            return Ok(*index);
        }

        // Encode all referenced parameter types from this function.
        let params: Vec<_> = self.encode_params(doc, &func.params)?;

        enum EncodedResults<'a> {
            Named(Vec<(&'a str, ComponentValType)>),
            Anon(ComponentValType),
        }

        let results = match &func.results {
            Results::Named(rs) => EncodedResults::Named(self.encode_params(doc, rs)?),
            Results::Anon(ty) => EncodedResults::Anon(self.encode_valtype(doc, ty)?),
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
        doc: &'a Document,
        params: &'a Params,
    ) -> Result<Vec<(&'a str, ComponentValType)>> {
        params
            .iter()
            .map(|(name, ty)| Ok((name.as_str(), self.encode_valtype(doc, ty)?)))
            .collect::<Result<_>>()
    }

    /// Encodes the `ty`, defined within `doc`, into this encoder and returns
    /// the corresponding `ComponentValType` that it represents.
    ///
    /// This will recursively define the entire structure of `ty` within `self`
    /// if necessary.
    fn encode_valtype(&mut self, doc: &'a Document, ty: &Type) -> Result<ComponentValType> {
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
                if let Some(index) = self.maybe_import_type(id) {
                    self.type_map().insert(id, index);
                    return Ok(ComponentValType::Type(index));
                }

                // ... and failing all that insert the type export.
                let ty = &doc.types[id];
                let mut encoded = match &ty.kind {
                    TypeDefKind::Record(r) => self.encode_record(doc, r)?,
                    TypeDefKind::Tuple(t) => self.encode_tuple(doc, t)?,
                    TypeDefKind::Flags(r) => self.encode_flags(r)?,
                    TypeDefKind::Variant(v) => self.encode_variant(doc, v)?,
                    TypeDefKind::Union(u) => self.encode_union(doc, u)?,
                    TypeDefKind::Option(t) => self.encode_option(doc, t)?,
                    TypeDefKind::Result(r) => self.encode_result(doc, r)?,
                    TypeDefKind::Enum(e) => self.encode_enum(e)?,
                    TypeDefKind::List(ty) => {
                        let ty = self.encode_valtype(doc, ty)?;
                        let (index, encoder) = self.defined_type();
                        encoder.list(ty);
                        ComponentValType::Type(index)
                    }
                    TypeDefKind::Type(ty) => {
                        match self.encode_valtype(doc, ty)? {
                            // This is `type a = b` which is encoded as an
                            // `outer` alias of depth 0
                            ComponentValType::Type(index) => {
                                ComponentValType::Type(self.define_type_alias_self(index))
                            }
                            t @ ComponentValType::Primitive(_) => t,
                        }
                    }
                    TypeDefKind::Future(_) => todo!("encoding for future type"),
                    TypeDefKind::Stream(_) => todo!("encoding for stream type"),
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
                    self.export_type(index, name);

                    encoded = ComponentValType::Type(index);
                }

                if let ComponentValType::Type(index) = encoded {
                    self.type_map().insert(id, index);
                }

                encoded
            }
        })
    }

    fn encode_optional_valtype(
        &mut self,
        doc: &'a Document,
        ty: Option<&Type>,
    ) -> Result<Option<ComponentValType>> {
        match ty {
            Some(ty) => self.encode_valtype(doc, ty).map(Some),
            None => Ok(None),
        }
    }

    fn encode_record(&mut self, doc: &'a Document, record: &Record) -> Result<ComponentValType> {
        let fields = record
            .fields
            .iter()
            .map(|f| Ok((f.name.as_str(), self.encode_valtype(doc, &f.ty)?)))
            .collect::<Result<Vec<_>>>()?;

        let (index, encoder) = self.defined_type();
        encoder.record(fields);
        Ok(ComponentValType::Type(index))
    }

    fn encode_tuple(&mut self, doc: &'a Document, tuple: &Tuple) -> Result<ComponentValType> {
        let tys = tuple
            .types
            .iter()
            .map(|ty| self.encode_valtype(doc, ty))
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

    fn encode_variant(&mut self, doc: &'a Document, variant: &Variant) -> Result<ComponentValType> {
        let cases = variant
            .cases
            .iter()
            .map(|c| {
                Ok((
                    c.name.as_str(),
                    self.encode_optional_valtype(doc, c.ty.as_ref())?,
                    None, // TODO: support defaulting case values in the future
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        let (index, encoder) = self.defined_type();
        encoder.variant(cases);
        Ok(ComponentValType::Type(index))
    }

    fn encode_union(&mut self, doc: &'a Document, union: &Union) -> Result<ComponentValType> {
        let tys = union
            .cases
            .iter()
            .map(|c| self.encode_valtype(doc, &c.ty))
            .collect::<Result<Vec<_>>>()?;

        let (index, encoder) = self.defined_type();
        encoder.union(tys);
        Ok(ComponentValType::Type(index))
    }

    fn encode_option(&mut self, doc: &'a Document, payload: &Type) -> Result<ComponentValType> {
        let ty = self.encode_valtype(doc, payload)?;
        let (index, encoder) = self.defined_type();
        encoder.option(ty);
        Ok(ComponentValType::Type(index))
    }

    fn encode_result(&mut self, doc: &'a Document, result: &Result_) -> Result<ComponentValType> {
        let ok = self.encode_optional_valtype(doc, result.ok.as_ref())?;
        let error = self.encode_optional_valtype(doc, result.err.as_ref())?;
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
    pub type_exports: Vec<(u32, &'a str)>,
    pub interface: InterfaceId,
}

impl<'a> ValtypeEncoder<'a> for RootTypeEncoder<'_, 'a> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        self.state.component.defined_type()
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        self.state.component.function_type()
    }
    fn define_type_alias_self(&mut self, idx: u32) -> u32 {
        self.state.component.alias_outer_type(0, idx)
    }
    fn export_type(&mut self, idx: u32, name: &'a str) {
        self.type_exports.push((idx, name));
    }
    fn type_map(&mut self) -> &mut HashMap<TypeId, u32> {
        &mut self.state.type_map
    }
    fn maybe_import_type(&mut self, id: TypeId) -> Option<u32> {
        // If this `id` is anonymous or belongs to this interface there's
        // nothing to import, it needs defining. Otherwise alias the type from
        // an import into this component's root namespace.
        let other = self.state.info.encoder.metadata.doc.types[id].interface?;
        if other == self.interface {
            return None;
        }
        // TODO: this doesn't work for importing types from other exports. That
        // just trips an assertion here.
        Some(self.state.index_of_type_export(id))
    }
    fn func_type_map(&mut self) -> &mut HashMap<FunctionKey<'a>, u32> {
        &mut self.state.func_type_map
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
    fn define_type_alias_self(&mut self, idx: u32) -> u32 {
        let ret = self.ty.type_count();
        self.ty.alias_outer_type(0, idx);
        ret
    }
    fn export_type(&mut self, idx: u32, name: &str) {
        self.ty
            .export(name, "", ComponentTypeRef::Type(TypeBounds::Eq, idx));
    }
    fn type_map(&mut self) -> &mut HashMap<TypeId, u32> {
        &mut self.type_map
    }
    fn maybe_import_type(&mut self, id: TypeId) -> Option<u32> {
        // If this `id` is anonymous or belongs to this interface
        // there's nothing to import, it needs defining. Otherwise
        // perform the importing process with an outer alias to the
        // parent component.
        let other = self.state.info.encoder.metadata.doc.types[id].interface?;
        if other == self.interface {
            return None;
        }

        let outer_idx = self.state.index_of_type_export(id);
        let ret = self.ty.type_count();
        self.type_map.insert(id, ret);
        self.ty.alias_outer_type(1, outer_idx);
        Some(ret)
    }
    fn func_type_map(&mut self) -> &mut HashMap<FunctionKey<'a>, u32> {
        &mut self.func_type_map
    }
}
