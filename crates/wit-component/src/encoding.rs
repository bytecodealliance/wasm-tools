use anyhow::{bail, Result};
use std::{
    collections::{hash_map::Entry, HashMap},
    hash::{Hash, Hasher},
};
use wasm_encoder::{
    ComponentExport, ComponentExportSection, ComponentTypeSection, InstanceType, InterfaceTypeRef,
    PrimitiveInterfaceType,
};
use wit_parser::{
    abi::Abi, Docs, Field, Function, FunctionKind, Interface, Record, RecordKind, Type, TypeDef,
    TypeDefKind, Variant,
};

struct TypeKey<'a> {
    interface: &'a Interface,
    ty: Type,
}

impl Hash for TypeKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.ty {
            Type::Id(id) => {
                TypeDefKey::borrow(self.interface, &self.interface.types[id]).hash(state)
            }
            _ => self.ty.hash(state),
        }
    }
}

impl PartialEq for TypeKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self.ty, other.ty) {
            (Type::Id(id), Type::Id(other_id)) => {
                TypeDefKey::borrow(self.interface, &self.interface.types[id])
                    == TypeDefKey::borrow(other.interface, &other.interface.types[other_id])
            }
            _ => self.ty.eq(&other.ty),
        }
    }
}

impl Eq for TypeKey<'_> {}

/// Represents a key type for interface type definitions.
pub struct TypeDefKey<'a> {
    interface: &'a Interface,
    borrow: Option<&'a TypeDef>,
    owned: Option<TypeDef>,
}

impl<'a> TypeDefKey<'a> {
    fn borrow(interface: &'a Interface, def: &'a TypeDef) -> Self {
        Self {
            interface,
            borrow: Some(def),
            owned: None,
        }
    }

    fn owned(interface: &'a Interface, def: TypeDef) -> Self {
        Self {
            interface,
            borrow: None,
            owned: Some(def),
        }
    }

    fn def(&self) -> &TypeDef {
        if let Some(kind) = self.borrow {
            kind
        } else {
            self.owned.as_ref().unwrap()
        }
    }
}

impl PartialEq for TypeDefKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        let def = self.def();
        let other_def = other.def();
        def.name == other_def.name
            && match (&def.kind, &other_def.kind) {
                (TypeDefKind::Record(r1), TypeDefKind::Record(r2)) => {
                    if r1.fields.len() != r2.fields.len() {
                        return false;
                    }

                    r1.fields.iter().zip(r2.fields.iter()).all(|(f1, f2)| {
                        f1.name == f2.name
                            && TypeKey {
                                interface: self.interface,
                                ty: f1.ty,
                            }
                            .eq(&TypeKey {
                                interface: other.interface,
                                ty: f2.ty,
                            })
                    })
                }
                (TypeDefKind::Variant(v1), TypeDefKind::Variant(v2)) => {
                    if v1.cases.len() != v2.cases.len() {
                        return false;
                    }

                    v1.cases.iter().zip(v2.cases.iter()).all(|(c1, c2)| {
                        c1.name == c2.name
                            && c1
                                .ty
                                .map(|ty| TypeKey {
                                    interface: self.interface,
                                    ty,
                                })
                                .eq(&c2.ty.map(|ty| TypeKey {
                                    interface: other.interface,
                                    ty,
                                }))
                    })
                }
                (TypeDefKind::List(t1), TypeDefKind::List(t2))
                | (TypeDefKind::Type(t1), TypeDefKind::Type(t2)) => TypeKey {
                    interface: self.interface,
                    ty: *t1,
                }
                .eq(&TypeKey {
                    interface: other.interface,
                    ty: *t2,
                }),
                _ => false,
            }
    }
}

impl Eq for TypeDefKey<'_> {}

impl Hash for TypeDefKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let def = self.def();
        def.name.hash(state);
        match &def.kind {
            TypeDefKind::Record(r) => {
                state.write_u8(0);
                for f in &r.fields {
                    f.name.hash(state);
                    TypeKey {
                        interface: self.interface,
                        ty: f.ty,
                    }
                    .hash(state);
                }
            }
            TypeDefKind::Variant(v) => {
                state.write_u8(1);
                for c in &v.cases {
                    c.name.hash(state);
                    c.ty.map(|ty| TypeKey {
                        interface: self.interface,
                        ty,
                    })
                    .hash(state);
                }
            }
            TypeDefKind::List(ty) => {
                state.write_u8(2);
                TypeKey {
                    interface: self.interface,
                    ty: *ty,
                }
                .hash(state);
            }
            TypeDefKind::Type(ty) => {
                state.write_u8(3);
                TypeKey {
                    interface: self.interface,
                    ty: *ty,
                }
                .hash(state);
            }
            TypeDefKind::Pointer(_)
            | TypeDefKind::ConstPointer(_)
            | TypeDefKind::PushBuffer(_)
            | TypeDefKind::PullBuffer(_) => unreachable!(),
        }
    }
}

/// Represents a key type for interface function definitions.
pub struct FunctionKey<'a> {
    interface: &'a Interface,
    func: &'a Function,
}

impl PartialEq for FunctionKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.func.params.len() != other.func.params.len()
            || self.func.results.len() != other.func.results.len()
        {
            return false;
        }

        self.func
            .params
            .iter()
            .zip(other.func.params.iter())
            .all(|((n1, t1), (n2, t2))| {
                n1 == n2
                    && TypeKey {
                        interface: self.interface,
                        ty: *t1,
                    }
                    .eq(&TypeKey {
                        interface: other.interface,
                        ty: *t2,
                    })
            })
            && self
                .func
                .results
                .iter()
                .zip(other.func.results.iter())
                .all(|((_, t1), (_, t2))| {
                    // Return value names aren't encoded, so ignore them
                    TypeKey {
                        interface: self.interface,
                        ty: *t1,
                    }
                    .eq(&TypeKey {
                        interface: other.interface,
                        ty: *t2,
                    })
                })
    }
}

impl Eq for FunctionKey<'_> {}

impl Hash for FunctionKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.func.params.len().hash(state);
        for (name, ty) in &self.func.params {
            name.hash(state);
            TypeKey {
                interface: self.interface,
                ty: *ty,
            }
            .hash(state);
        }
        self.func.results.len().hash(state);
        for (_, ty) in &self.func.results {
            // Result names aren't encoded, so ignore them
            TypeKey {
                interface: self.interface,
                ty: *ty,
            }
            .hash(state);
        }
    }
}

#[derive(Default)]
struct InterfaceEncoderState<'a> {
    ty: InstanceType,
    aliases: HashMap<u32, u32>,
    exports: HashMap<&'a str, u32>,
}

/// An encoder of WebAssembly interfaces AST representations.
pub struct InterfaceEncoder<'a> {
    interface: &'a Interface,
    name: &'a str,
    types: &'a mut ComponentTypeSection,
    exports: &'a mut ComponentExportSection,
    type_map: &'a mut HashMap<TypeDefKey<'a>, u32>,
    func_type_map: &'a mut HashMap<FunctionKey<'a>, u32>,
    state: InterfaceEncoderState<'a>,
}

impl<'a> InterfaceEncoder<'a> {
    /// Creates a new `InterfaceEncoder` for the given interface.
    ///
    /// The interface will be encoded in the given sections.
    pub fn new(
        interface: &'a Interface,
        name: &'a str,
        types: &'a mut ComponentTypeSection,
        exports: &'a mut ComponentExportSection,
        type_map: &'a mut HashMap<TypeDefKey<'a>, u32>,
        func_type_map: &'a mut HashMap<FunctionKey<'a>, u32>,
    ) -> Result<Self> {
        if interface.resources.len() != 0 {
            bail!("the use of resources in interfaces is not currently not supported");
        }

        Ok(InterfaceEncoder {
            interface,
            name,
            types,
            exports,
            type_map,
            func_type_map,
            state: InterfaceEncoderState::default(),
        })
    }

    /// Encodes the interface into the internal sections.
    pub fn encode(&mut self) -> Result<()> {
        for func in &self.interface.functions {
            assert!(!func.name.is_empty());

            if !matches!(func.kind, FunctionKind::Freestanding) {
                bail!(
                    "unsupported function `{}`: only free-standing functions are currently supported",
                    func.name
                );
            }

            if !matches!(func.abi, Abi::Canonical) {
                bail!(
                    "unsupported function `{}`: only canonical functions are supported",
                    func.name
                );
            }

            if func.is_async {
                bail!(
                    "unsupported function `{}`: only synchronous functions are currently supported",
                    func.name
                );
            }

            let index = match self.func_type_map.get(&FunctionKey {
                interface: self.interface,
                func,
            }) {
                Some(index) => *index,
                None => {
                    let index = self.encode_func_type(func)?;
                    self.func_type_map.insert(
                        FunctionKey {
                            interface: self.interface,
                            func,
                        },
                        index,
                    );
                    index
                }
            };

            self.export_type(&func.name, index)?;

            // TODO: stick interface documentation in a custom section?
        }

        let state = std::mem::take(&mut self.state);
        let index = self.types.len();
        self.types.instance(&state.ty);
        self.exports.export(self.name, ComponentExport::Type(index));
        Ok(())
    }

    fn export_type(&mut self, name: &'a str, index: u32) -> Result<()> {
        match self.state.exports.entry(name) {
            Entry::Occupied(e) => {
                if *e.get() != index {
                    bail!("duplicate export `{}`", name)
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(index);
                let index = self.alias_type(index);
                self.state.ty.export(name, index);
            }
        }

        Ok(())
    }

    fn alias_type(&mut self, index: u32) -> u32 {
        match self.state.aliases.entry(index) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let alias = self.state.ty.type_count();
                e.insert(alias);
                self.state.ty.alias_outer_type(1, index);
                alias
            }
        }
    }

    fn encode_record(&mut self, record: &Record) -> Result<InterfaceTypeRef> {
        Ok(match record.kind {
            RecordKind::Other => {
                let fields = record
                    .fields
                    .iter()
                    .map(|f| Ok((f.name.as_str(), self.encode_type(&f.ty)?)))
                    .collect::<Result<Vec<_>>>()?;

                let index = self.types.len();
                let encoder = self.types.interface_type();
                encoder.record(fields);
                InterfaceTypeRef::Type(index)
            }
            RecordKind::Flags(_) => {
                let index = self.types.len();
                let encoder = self.types.interface_type();
                encoder.flags(record.fields.iter().map(|f| f.name.as_str()));
                InterfaceTypeRef::Type(index)
            }
            RecordKind::Tuple => {
                let tys = record
                    .fields
                    .iter()
                    .map(|f| self.encode_type(&f.ty))
                    .collect::<Result<Vec<_>>>()?;
                let index = self.types.len();
                let encoder = self.types.interface_type();
                encoder.tuple(tys);
                InterfaceTypeRef::Type(index)
            }
        })
    }

    fn encode_variant(&mut self, variant: &Variant) -> Result<InterfaceTypeRef> {
        if variant.is_bool() {
            return Ok(InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Bool));
        }

        if let Some(ty) = variant.as_option() {
            let ty = self.encode_type(ty)?;
            let index = self.types.len();
            let encoder = self.types.interface_type();
            encoder.option(ty);
            return Ok(InterfaceTypeRef::Type(index));
        }

        if let Some((ok, error)) = variant.as_expected() {
            let ok = ok
                .map(|ty| self.encode_type(ty))
                .transpose()?
                .unwrap_or(InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit));
            let error = error
                .map(|ty| self.encode_type(ty))
                .transpose()?
                .unwrap_or(InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit));
            let index = self.types.len();
            let encoder = self.types.interface_type();
            encoder.expected(ok, error);
            return Ok(InterfaceTypeRef::Type(index));
        }

        if variant.is_enum() {
            let index = self.types.len();
            let encoder = self.types.interface_type();
            encoder.enum_type(variant.cases.iter().map(|c| c.name.as_str()));
            return Ok(InterfaceTypeRef::Type(index));
        }

        if variant.is_union() {
            let tys = variant
                .cases
                .iter()
                .map(|c| self.encode_type(c.ty.as_ref().unwrap()))
                .collect::<Result<Vec<_>>>()?;

            let index = self.types.len();
            let encoder = self.types.interface_type();
            encoder.union(tys);
            return Ok(InterfaceTypeRef::Type(index));
        }

        let cases = variant
            .cases
            .iter()
            .map(|c| {
                Ok((
                    c.name.as_str(),
                    match c.ty.as_ref() {
                        Some(ty) => self.encode_type(ty)?,
                        None => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit),
                    },
                    None, // TODO: support defaulting case values in the future
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        let index = self.types.len();
        let encoder = self.types.interface_type();
        encoder.variant(cases);
        Ok(InterfaceTypeRef::Type(index))
    }

    fn encode_type(&mut self, ty: &Type) -> Result<InterfaceTypeRef> {
        Ok(match ty {
            Type::U8 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U8),
            Type::U16 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U16),
            Type::U32 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U32),
            Type::U64 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::U64),
            Type::S8 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S8),
            Type::S16 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S16),
            Type::S32 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S32),
            Type::S64 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::S64),
            Type::F32 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Float32),
            Type::F64 => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Float64),
            Type::Char => InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Char),
            Type::Id(id) => {
                let ty = &self.interface.types[*id];
                let key = TypeDefKey::borrow(self.interface, &self.interface.types[*id]);
                let encoded = if let Some(index) = self.type_map.get(&key) {
                    InterfaceTypeRef::Type(*index)
                } else {
                    let mut encoded = match &ty.kind {
                        TypeDefKind::Record(r) => self.encode_record(r)?,
                        TypeDefKind::Variant(v) => self.encode_variant(v)?,
                        TypeDefKind::List(ty) => {
                            if matches!(ty, Type::Char) {
                                InterfaceTypeRef::Primitive(PrimitiveInterfaceType::String)
                            } else {
                                let ty = self.encode_type(ty)?;
                                let index = self.types.len();
                                let encoder = self.types.interface_type();
                                encoder.list(ty);
                                InterfaceTypeRef::Type(index)
                            }
                        }
                        TypeDefKind::Type(ty) => self.encode_type(ty)?,
                        TypeDefKind::Pointer(_) | TypeDefKind::ConstPointer(_) => {
                            bail!("the use of pointers in interfaces is not supported")
                        }
                        TypeDefKind::PushBuffer(_) | TypeDefKind::PullBuffer(_) => {
                            bail!("the use of buffers in interfaces is not currently supported")
                        }
                    };

                    if ty.name.is_some() {
                        if let InterfaceTypeRef::Primitive(ty) = encoded {
                            // Named primitive types need entries in the type section
                            let index = self.types.len();
                            self.types.interface_type().primitive(ty);
                            encoded = InterfaceTypeRef::Type(index);
                        }
                    }

                    if let InterfaceTypeRef::Type(index) = encoded {
                        self.type_map.insert(key, index);
                    }

                    encoded
                };

                if let Some(name) = ty.name.as_deref() {
                    if let InterfaceTypeRef::Type(index) = encoded {
                        self.export_type(name, index)?;
                    }
                }

                encoded
            }
            Type::Handle(_) => {
                bail!("the use of handle types in interfaces is not currently supported")
            }
            Type::CChar => {
                bail!("the use of C characters in interfaces is not currently supported")
            }
            Type::Usize => bail!("the use of usize in interfaces is not currently supported"),
        })
    }

    fn encode_func_type(&mut self, func: &Function) -> Result<u32> {
        // Encode all referenced parameter types from this function.
        let params: Vec<_> = func
            .params
            .iter()
            .map(|(name, ty)| Ok((Some(name.as_str()), self.encode_type(ty)?)))
            .collect::<Result<_>>()?;

        let result = if func.results.is_empty() {
            InterfaceTypeRef::Primitive(PrimitiveInterfaceType::Unit)
        } else if func.results.len() == 1 {
            let (name, ty) = &func.results[0];
            if !name.is_empty() {
                bail!(
                    "unsupported function `{}`: a single return value cannot be named",
                    func.name
                );
            }
            self.encode_type(ty)?
        } else {
            // Encode a tuple for the return value
            let fields = func
                .results
                .iter()
                .enumerate()
                .map(|(i, (_, ty))| {
                    Ok(Field {
                        docs: Docs::default(),
                        name: i.to_string(),
                        ty: *ty,
                    })
                })
                .collect::<Result<Vec<_>>>()?;

            let def = TypeDef {
                docs: Docs::default(),
                name: None,
                kind: TypeDefKind::Record(Record {
                    fields,
                    kind: RecordKind::Tuple,
                }),
                foreign_module: None,
            };

            InterfaceTypeRef::Type(
                match self.type_map.get(&TypeDefKey::borrow(self.interface, &def)) {
                    Some(index) => *index,
                    None => match &def.kind {
                        TypeDefKind::Record(r) => match self.encode_record(r)? {
                            InterfaceTypeRef::Type(ty) => {
                                self.type_map
                                    .insert(TypeDefKey::owned(self.interface, def), ty);
                                ty
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    },
                },
            )
        };

        // Encode the function type
        let index = self.types.len();
        self.types.function(params, result);
        Ok(index)
    }
}
