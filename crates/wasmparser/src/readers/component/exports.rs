use crate::{BinaryReader, ComponentTypeRef, FromReader, Result, SectionLimited};

/// Represents the kind of an external items of a WebAssembly component.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ComponentExternalKind {
    /// The external kind is a core module.
    Module,
    /// The external kind is a function.
    Func,
    /// The external kind is a value.
    Value,
    /// The external kind is a type.
    Type,
    /// The external kind is an instance.
    Instance,
    /// The external kind is a component.
    Component,
}

impl ComponentExternalKind {
    pub(crate) fn from_bytes(
        byte1: u8,
        byte2: Option<u8>,
        offset: usize,
    ) -> Result<ComponentExternalKind> {
        Ok(match byte1 {
            0x00 => match byte2.unwrap() {
                0x11 => ComponentExternalKind::Module,
                x => {
                    return Err(BinaryReader::invalid_leading_byte_error(
                        x,
                        "component external kind",
                        offset + 1,
                    ))
                }
            },
            0x01 => ComponentExternalKind::Func,
            0x02 => ComponentExternalKind::Value,
            0x03 => ComponentExternalKind::Type,
            0x04 => ComponentExternalKind::Component,
            0x05 => ComponentExternalKind::Instance,
            x => {
                return Err(BinaryReader::invalid_leading_byte_error(
                    x,
                    "component external kind",
                    offset,
                ))
            }
        })
    }
}

/// Represents an export in a WebAssembly component.
#[derive(Debug, Clone)]
pub struct ComponentExport<'a> {
    /// The name of the exported item.
    pub name: &'a str,
    /// The optional URL of the exported item.
    pub url: &'a str,
    /// The kind of the export.
    pub kind: ComponentExternalKind,
    /// The index of the exported item.
    pub index: u32,
    /// An optionally specified type ascribed to this export.
    pub ty: Option<ComponentTypeRef>,
}

/// A reader for the export section of a WebAssembly component.
pub type ComponentExportSectionReader<'a> = SectionLimited<'a, ComponentExport<'a>>;

impl<'a> FromReader<'a> for ComponentExport<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(ComponentExport {
            name: reader.read()?,
            url: reader.read()?,
            kind: reader.read()?,
            index: reader.read()?,
            ty: match reader.read_u8()? {
                0x00 => None,
                0x01 => Some(reader.read()?),
                other => {
                    return Err(BinaryReader::invalid_leading_byte_error(
                        other,
                        "optional component export type",
                        reader.original_position() - 1,
                    ))
                }
            },
        })
    }
}

impl<'a> FromReader<'a> for ComponentExternalKind {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let offset = reader.original_position();
        let byte1 = reader.read_u8()?;
        let byte2 = if byte1 == 0x00 {
            Some(reader.read_u8()?)
        } else {
            None
        };

        ComponentExternalKind::from_bytes(byte1, byte2, offset)
    }
}
