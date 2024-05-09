use crate::prelude::*;
use crate::{
    BinaryReader, BinaryReaderError, ComponentDefinedType, ComponentType, ComponentValType,
    FromReader, Ieee32, Ieee64, PrimitiveValType, Result, SectionLimited,
};

/// A component value with its type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ComponentValue<'a> {
    /// The type of this value.
    pub ty: ComponentValType,
    bytes: &'a [u8],
    original_offset: usize,
}

impl<'a> ComponentValue<'a> {
    /// A component model value.
    /// This takes the types from the current components type section
    /// in the same order as they where read from there.
    pub fn val(&self, types: &[ComponentType]) -> Result<Val> {
        read_val(
            &mut BinaryReader::new_with_offset(self.bytes, self.original_offset),
            self.ty,
            types,
        )
    }
}

/// A component value.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
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
    /// A record.
    Record(Vec<Val>),
    /// A variant case.
    VariantCase {
        /// The label of the variant case.
        label: u32,
        /// The value of the variant case.
        v: Option<Box<Val>>,
    },
    /// A list.
    List(Vec<Val>),
    /// A tuple.
    Tuple(Vec<Val>),
    /// A flags value.
    Flags(Vec<bool>),
    /// An enum case.
    EnumCase(u32),
    /// A none case of an option.
    None,
    /// A some case of an option with a given value.
    Some(Box<Val>),
    /// An ok case of a result with an optional value.
    Ok(Option<Box<Val>>),
    /// An error case of a result with an optional value.
    Error(Option<Box<Val>>),
}

/// A reader for the value section of a WebAssembly component.
pub type ComponentValueSectionReader<'a> = SectionLimited<'a, ComponentValue<'a>>;

impl<'a> FromReader<'a> for ComponentValue<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let ty = ComponentValType::from_reader(reader)?;
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

fn read_val(
    reader: &mut BinaryReader,
    ty: ComponentValType,
    types: &[ComponentType],
) -> Result<Val> {
    let ty = get_defined_type(ty, types, reader.original_position())?;
    match ty {
        ComponentDefinedType::Primitive(prim_ty) => read_primitive_value(reader, prim_ty),
        ComponentDefinedType::Record(record_ty) => {
            let mut fields = Vec::with_capacity(record_ty.len());
            for field in record_ty.iter() {
                fields.push(read_val(reader, field.1, types)?);
            }
            Ok(Val::Record(fields))
        }
        ComponentDefinedType::Variant(variant_ty) => {
            let label = reader.read_var_u32()?;
            if label as usize >= variant_ty.len() {
                bail!(
                    reader.original_position(),
                    "invalid variant case label: {label}"
                );
            }
            let case_ty = variant_ty[label as usize].ty;
            let v = if let Some(case_ty) = case_ty {
                Some(Box::new(read_val(reader, case_ty, types)?))
            } else {
                None
            };
            Ok(Val::VariantCase { label, v })
        }
        ComponentDefinedType::List(element_ty) => {
            let len = reader.read_var_u32()?;
            let mut elements = Vec::with_capacity(len as usize);
            for _ in 0..len {
                elements.push(read_val(reader, element_ty, types)?);
            }
            Ok(Val::List(elements))
        }
        ComponentDefinedType::Tuple(tuple_ty) => {
            let mut fields = Vec::with_capacity(tuple_ty.len());
            for field_ty in tuple_ty.iter() {
                fields.push(read_val(reader, *field_ty, types)?);
            }
            Ok(Val::Tuple(fields))
        }
        ComponentDefinedType::Flags(flags_ty) => Ok(Val::Flags({
            let mut value = vec![false; flags_ty.len()];
            let n = reader.read_var_u64()?;
            for (i, field) in value.iter_mut().enumerate() {
                if ((n >> (i as u64)) & 1) == 1 {
                    *field = true;
                }
            }
            value
        })),
        ComponentDefinedType::Enum(enum_ty) => Ok(Val::EnumCase({
            let label = reader.read_var_u32()?;
            if label as usize >= enum_ty.len() {
                bail!(
                    reader.original_position(),
                    "invalid enum case label: {label}"
                );
            }
            label
        })),
        ComponentDefinedType::Option(option_ty) => Ok(match reader.read_u8()? {
            0x0 => Val::None,
            0x1 => Val::Some(Box::new(read_val(reader, option_ty, types)?)),
            x => return reader.invalid_leading_byte(x, "invalid option label"),
        }),
        ComponentDefinedType::Result {
            ok: ok_ty,
            err: err_ty,
        } => {
            let label = reader.read_u8()?;
            Ok(match label {
                0x0 => Val::Ok(if let Some(ok_ty) = ok_ty {
                    Some(Box::new(read_val(reader, ok_ty, types)?))
                } else {
                    None
                }),
                0x1 => Val::Error(if let Some(err_ty) = err_ty {
                    Some(Box::new(read_val(reader, err_ty, types)?))
                } else {
                    None
                }),
                x => return reader.invalid_leading_byte(x, "invalid result label"),
            })
        }
        ComponentDefinedType::Own(_) | ComponentDefinedType::Borrow(_) => {
            Err(BinaryReaderError::new(
                "resource handles not supported in value section",
                reader.original_position(),
            ))
        }
    }
}

fn read_primitive_value(reader: &mut BinaryReader, ty: PrimitiveValType) -> Result<Val> {
    Ok(match ty {
        PrimitiveValType::Bool => Val::Bool(match reader.read_u8()? {
            0 => false,
            1 => true,
            n => bail!(reader.original_position(), "invalid bool value: {n}"),
        }),
        PrimitiveValType::S8 => Val::S8(reader.read_u8()? as i8),
        PrimitiveValType::U8 => Val::U8(reader.read_u8()?),
        PrimitiveValType::S16 => Val::S16(reader.read_var_i16()?),
        PrimitiveValType::U16 => Val::U16(reader.read_var_u16()?),
        PrimitiveValType::S32 => Val::S32(reader.read_var_i32()?),
        PrimitiveValType::U32 => Val::U32(reader.read_var_u32()?),
        PrimitiveValType::S64 => Val::S64(reader.read_var_i64()?),
        PrimitiveValType::U64 => Val::U64(reader.read_var_u64()?),
        PrimitiveValType::F32 => Val::F32({
            let value = reader.read_f32()?;
            if f32::from_bits(value.0).is_nan() && value.0 != 0x7f_c0_00_00 {
                bail!(reader.original_position(), "invalid f32: non canonical NaN");
            }
            value
        }),
        PrimitiveValType::F64 => Val::F64({
            let value = reader.read_f64()?;
            if f64::from_bits(value.0).is_nan() && value.0 != 0x7f_f8_00_00_00_00_00_00 {
                bail!(reader.original_position(), "invalid f64: non canonical NaN");
            }
            value
        }),
        PrimitiveValType::Char => Val::Char(char::from_u32(reader.read_var_u32()?).ok_or(
            BinaryReaderError::new("invalid Unicode scalar value", reader.original_position()),
        )?),
        PrimitiveValType::String => Val::String(reader.read_string()?.into()),
    })
}

fn get_defined_type<'a>(
    ty: ComponentValType,
    types: &[ComponentType<'a>],
    offset: usize,
) -> Result<ComponentDefinedType<'a>> {
    match ty {
        ComponentValType::Primitive(prim_ty) => Ok(ComponentDefinedType::Primitive(prim_ty)),
        ComponentValType::Type(idx) => match types.get(idx as usize) {
            Some(ComponentType::Defined(cdt)) => Ok(cdt.clone()),
            Some(_) => bail!(offset, "not a component defined type at index {idx}"),
            None => bail!(offset, "type index out of bounds: {idx}"),
        },
    }
}
