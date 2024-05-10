use crate::prelude::*;
use crate::types::{ComponentDefinedType, ComponentValType, TypesRef};
use crate::{
    BinaryReader, BinaryReaderError, FromReader, Ieee32, Ieee64, PrimitiveValType, Result,
    SectionLimited,
};

/// A component value with its type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ComponentValue<'a> {
    /// The type of this value.
    pub ty: crate::ComponentValType,
    bytes: &'a [u8],
    original_offset: usize,
}

impl<'a> ComponentValue<'a> {
    /// A component model value.
    /// This takes the types from the current components type section
    /// in the same order as they where read from there.
    pub fn val<V>(&self, types: &TypesRef, visitor: V) -> Result<()>
    where
        V: Val,
    {
        let ty = match self.ty {
            crate::ComponentValType::Primitive(prim_ty) => ComponentValType::Primitive(prim_ty),
            crate::ComponentValType::Type(idx) => {
                ComponentValType::Type(types.component_defined_type_at(idx))
            }
        };
        read_val(
            &mut BinaryReader::new_with_offset(self.bytes, self.original_offset),
            ty,
            types,
            visitor,
        )
    }
}

/// A primitive value.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PrimitiveValue {
    /// A boolean value.
    Bool(bool),
    /// A signed 8-bit integer.
    S8(i8),
    /// An unsigned 8-bit integer.
    U8(u8),
    /// An signed 16-bit integer.
    S16(i16),
    /// An unsigned 16-bit integer.
    U16(u16),
    /// A signed 32-bit integer.
    S32(i32),
    /// An unsigned 32-bit integer.
    U32(u32),
    /// A signed 64-bit integer.
    S64(i64),
    /// An unsigned 64-bit integer.
    U64(u64),
    /// A 32-bit floating point number.
    F32(Ieee32),
    /// A 64-bit floating point number.
    F64(Ieee64),
    /// A Unicode scalar value.
    Char(char),
    /// A Unicode string.
    String(String),
}

/// A value visitor.
pub trait Val: Sized {
    /// A record visitor.
    type R: Record<Self>;
    /// A list visitor.
    type L: List<Self>;
    /// A tuple visitor.
    type T: Tuple<Self>;
    /// A flags visitor.
    type F: Flags<Self>;
    /// A primitive value.
    fn primitive(self, v: PrimitiveValue);
    /// A record.
    fn record(self, length: u32) -> Self::R;
    /// A variant case with a given value.
    fn variant_case(self, label_index: u32, name: &str) -> Self;
    /// A variant case without a value.
    fn variant_case_empty(self, index: u32, name: &str);
    /// A list.
    fn list(self, length: u32) -> Self::L;
    /// A tuple.
    fn tuple(self, length: u32) -> Self::T;
    /// A flags value.
    fn flags(self, length: u32) -> Self::F;
    /// An enum case.
    fn enum_case(self, label_index: u32, name: &str);
    /// A none case of an option.
    fn none(self);
    /// A some case of an option with a given value.
    fn some(self) -> Self;
    /// An ok case of a result with a given value.
    fn ok(self) -> Self;
    /// An ok case of a result without a value.
    fn ok_empty(self);
    /// An error case of a result with a given value.
    fn error(self) -> Self;
    /// An error case of a result without a given value.
    fn error_empty(self);
}

/// A visitor for record fields.
pub trait Record<V: Val>: Sized {
    /// Visitor for the next record field.
    fn field(&mut self, name: &str) -> V;
    /// No more fields.
    fn end(self);
}

/// A visitor for list elements.
pub trait List<V: Val>: Sized {
    /// Visitor for the next list element.
    fn element(&mut self) -> V;
    /// No more elements.
    fn end(self);
}

/// A visitor for tuple fields.
pub trait Tuple<V: Val>: Sized {
    /// Visitor for the next tuple field.
    fn field(&mut self) -> V;
    /// No more fields.
    fn end(self);
}

/// A visitor for flags fields.
pub trait Flags<V: Val>: Sized {
    /// Visitor for the next flags field.
    fn field(&mut self, name: &str, val: bool);
    /// No more fields.
    fn end(self);
}

/// A reader for the value section of a WebAssembly component.
pub type ComponentValueSectionReader<'a> = SectionLimited<'a, ComponentValue<'a>>;

impl<'a> FromReader<'a> for ComponentValue<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let ty = crate::ComponentValType::from_reader(reader)?;
        let len = reader.read_var_u32()?;
        let original_offset = reader.original_position();
        let bytes = reader.read_bytes(len as usize)?;
        Ok(ComponentValue {
            ty,
            bytes,
            original_offset,
        })
    }
}

fn read_val<V>(
    reader: &mut BinaryReader,
    ty: ComponentValType,
    types: &TypesRef,
    visitor: V,
) -> Result<()>
where
    V: Val,
{
    let ty = get_defined_type(ty, types, reader.original_position())?;
    match ty {
        ComponentDefinedType::Primitive(prim_ty) => {
            visitor.primitive(read_primitive_value(reader, prim_ty)?);
        }
        ComponentDefinedType::Record(record_ty) => {
            let mut record = visitor.record(record_ty.fields.len() as u32);
            for field in record_ty.fields.iter() {
                read_val(reader, *field.1, types, record.field(field.0))?;
            }
            record.end();
        }
        ComponentDefinedType::Variant(variant_ty) => {
            let label = reader.read_var_u32()?;
            if label as usize >= variant_ty.cases.len() {
                bail!(
                    reader.original_position(),
                    "invalid variant case label: {label}"
                );
            }
            let case_ty = variant_ty.cases[label as usize].ty;
            if let Some(case_ty) = case_ty {
                read_val(reader, case_ty, types, visitor.some())?;
            } else {
                visitor.none();
            };
        }
        ComponentDefinedType::List(element_ty) => {
            let len = reader.read_var_u32()?;
            let mut list = visitor.list(len);
            for _ in 0..len {
                read_val(reader, element_ty, types, list.element())?;
            }
            list.end();
        }
        ComponentDefinedType::Tuple(tuple_ty) => {
            let mut tuple = visitor.tuple(tuple_ty.types.len() as u32);
            for field_ty in tuple_ty.types.iter() {
                read_val(reader, *field_ty, types, tuple.field())?;
            }
            tuple.end();
        }
        ComponentDefinedType::Flags(flags_ty) => {
            let n = reader.read_var_u64()?;
            let mut flags = visitor.flags(flags_ty.len() as u32);
            for i in 0..flags_ty.len() {
                let v = if ((n >> (i as u64)) & 1) == 1 {
                    true
                } else {
                    false
                };
                flags.field(flags_ty.get_index(i).unwrap(), v);
            }
            flags.end();
        }
        ComponentDefinedType::Enum(enum_ty) => {
            let label = reader.read_var_u32()?;
            if label as usize >= enum_ty.len() {
                bail!(
                    reader.original_position(),
                    "invalid enum case label: {label}"
                );
            }
            visitor.enum_case(label, enum_ty.get_index(label as usize).unwrap());
        }
        ComponentDefinedType::Option(option_ty) => match reader.read_u8()? {
            0x0 => {
                visitor.none();
            }
            0x1 => {
                read_val(reader, option_ty, types, visitor.some())?;
            }
            x => return reader.invalid_leading_byte(x, "invalid option label"),
        },
        ComponentDefinedType::Result {
            ok: ok_ty,
            err: err_ty,
        } => {
            let label = reader.read_u8()?;
            match label {
                0x0 => {
                    if let Some(ok_ty) = ok_ty {
                        read_val(reader, ok_ty, types, visitor.ok())?;
                    } else {
                        visitor.ok_empty();
                    }
                }
                0x1 => {
                    if let Some(err_ty) = err_ty {
                        read_val(reader, err_ty, types, visitor.error())?;
                    } else {
                        visitor.error_empty();
                    }
                }
                x => return reader.invalid_leading_byte(x, "invalid result label"),
            }
        }
        ComponentDefinedType::Own(_) | ComponentDefinedType::Borrow(_) => {
            bail!(
                reader.original_position(),
                "resource handles not supported in value section"
            )
        }
    }
    Ok(())
}

fn read_primitive_value(reader: &mut BinaryReader, ty: PrimitiveValType) -> Result<PrimitiveValue> {
    Ok(match ty {
        PrimitiveValType::Bool => PrimitiveValue::Bool(match reader.read_u8()? {
            0x0 => false,
            0x1 => true,
            x => return reader.invalid_leading_byte(x, "invalid bool value: {n}"),
        }),
        PrimitiveValType::S8 => PrimitiveValue::S8(reader.read_u8()? as i8),
        PrimitiveValType::U8 => PrimitiveValue::U8(reader.read_u8()?),
        PrimitiveValType::S16 => PrimitiveValue::S16(reader.read_var_i16()?),
        PrimitiveValType::U16 => PrimitiveValue::U16(reader.read_var_u16()?),
        PrimitiveValType::S32 => PrimitiveValue::S32(reader.read_var_i32()?),
        PrimitiveValType::U32 => PrimitiveValue::U32(reader.read_var_u32()?),
        PrimitiveValType::S64 => PrimitiveValue::S64(reader.read_var_i64()?),
        PrimitiveValType::U64 => PrimitiveValue::U64(reader.read_var_u64()?),
        PrimitiveValType::F32 => PrimitiveValue::F32({
            let value = reader.read_f32()?;
            if f32::from_bits(value.0).is_nan() && value.0 != 0x7f_c0_00_00 {
                bail!(reader.original_position(), "invalid f32: non canonical NaN");
            }
            value
        }),
        PrimitiveValType::F64 => PrimitiveValue::F64({
            let value = reader.read_f64()?;
            if f64::from_bits(value.0).is_nan() && value.0 != 0x7f_f8_00_00_00_00_00_00 {
                bail!(reader.original_position(), "invalid f64: non canonical NaN");
            }
            value
        }),
        PrimitiveValType::Char => {
            PrimitiveValue::Char(char::from_u32(reader.read_var_u32()?).ok_or(
                BinaryReaderError::new("invalid Unicode scalar value", reader.original_position()),
            )?)
        }
        PrimitiveValType::String => PrimitiveValue::String(reader.read_string()?.into()),
    })
}

fn get_defined_type(
    ty: ComponentValType,
    types: &TypesRef,
    offset: usize,
) -> Result<ComponentDefinedType> {
    Ok(match ty {
        ComponentValType::Primitive(prim_ty) => ComponentDefinedType::Primitive(prim_ty),
        ComponentValType::Type(id) => {
            if let Some(def_ty) = types.get(id) {
                def_ty.clone()
            } else {
                bail!(offset, "invalid type");
            }
        }
    })
}
