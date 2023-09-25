use crate::{
    BinaryReader, ComponentExternalKind, ComponentValType, FromReader, Result, SectionLimited,
};

/// Represents the type bounds for imports and exports.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TypeBounds {
    /// The type is bounded by equality.
    Eq(u32),
    /// A fresh resource type,
    SubResource,
}

impl<'a> FromReader<'a> for TypeBounds {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read_u8()? {
            0x00 => TypeBounds::Eq(reader.read()?),
            0x01 => TypeBounds::SubResource,
            x => return reader.invalid_leading_byte(x, "type bound"),
        })
    }
}

/// Represents a reference to a component type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ComponentTypeRef {
    /// The reference is to a core module type.
    ///
    /// The index is expected to be core type index to a core module type.
    Module(u32),
    /// The reference is to a function type.
    ///
    /// The index is expected to be a type index to a function type.
    Func(u32),
    /// The reference is to a value type.
    Value(ComponentValType),
    /// The reference is to a bounded type.
    ///
    /// The index is expected to be a type index.
    Type(TypeBounds),
    /// The reference is to an instance type.
    ///
    /// The index is a type index to an instance type.
    Instance(u32),
    /// The reference is to a component type.
    ///
    /// The index is a type index to a component type.
    Component(u32),
}

impl ComponentTypeRef {
    /// Returns the corresponding [`ComponentExternalKind`] for this reference.
    pub fn kind(&self) -> ComponentExternalKind {
        match self {
            ComponentTypeRef::Module(_) => ComponentExternalKind::Module,
            ComponentTypeRef::Func(_) => ComponentExternalKind::Func,
            ComponentTypeRef::Value(_) => ComponentExternalKind::Value,
            ComponentTypeRef::Type(..) => ComponentExternalKind::Type,
            ComponentTypeRef::Instance(_) => ComponentExternalKind::Instance,
            ComponentTypeRef::Component(_) => ComponentExternalKind::Component,
        }
    }
}

impl<'a> FromReader<'a> for ComponentTypeRef {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(match reader.read()? {
            ComponentExternalKind::Module => ComponentTypeRef::Module(reader.read()?),
            ComponentExternalKind::Func => ComponentTypeRef::Func(reader.read()?),
            ComponentExternalKind::Value => ComponentTypeRef::Value(reader.read()?),
            ComponentExternalKind::Type => ComponentTypeRef::Type(reader.read()?),
            ComponentExternalKind::Instance => ComponentTypeRef::Instance(reader.read()?),
            ComponentExternalKind::Component => ComponentTypeRef::Component(reader.read()?),
        })
    }
}

/// Represents an import in a WebAssembly component
#[derive(Debug, Copy, Clone)]
pub struct ComponentImport<'a> {
    /// The name of the imported item.
    pub name: ComponentImportName<'a>,
    /// The type reference for the import.
    pub ty: ComponentTypeRef,
}

impl<'a> FromReader<'a> for ComponentImport<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        Ok(ComponentImport {
            name: reader.read()?,
            ty: reader.read()?,
        })
    }
}

/// A reader for the import section of a WebAssembly component.
///
/// # Examples
///
/// ```
/// use wasmparser::ComponentImportSectionReader;
/// let data: &[u8] = &[0x01, 0x00, 0x01, 0x41, 0x01, 0x66];
/// let reader = ComponentImportSectionReader::new(data, 0).unwrap();
/// for import in reader {
///     let import = import.expect("import");
///     println!("Import: {:?}", import);
/// }
/// ```
pub type ComponentImportSectionReader<'a> = SectionLimited<'a, ComponentImport<'a>>;

/// Represents the name of a component import.
#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum ComponentImportName<'a> {
    Kebab(&'a str),
    Interface(&'a str),
    Url(&'a str, &'a str, Option<&'a str>),
    Relative(&'a str, &'a str, Option<&'a str>),
    Naked(&'a str, &'a str),
    Locked(&'a str, &'a str),
    Unlocked(&'a str),
}

/// Represents the name of a component export.
#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum ComponentExportName<'a> {
    Kebab(&'a str),
    Interface(&'a str),
}

impl<'a> ComponentImportName<'a> {
    /// Returns the underlying string representing this name.
    pub fn as_str(&self) -> &'a str {
        match self {
            ComponentImportName::Kebab(name) => name,
            ComponentImportName::Interface(name) => name,
            ComponentImportName::Url(name, _, _) => name,
            ComponentImportName::Relative(name, _, _) => name,
            ComponentImportName::Naked(name, _) => name,
            ComponentImportName::Locked(name, _) => name,
            ComponentImportName::Unlocked(name) => name,
        }
    }
}

impl<'a> ComponentExportName<'a> {
    /// Returns the underlying string representing this name.
    pub fn as_str(&self) -> &'a str {
        match self {
            ComponentExportName::Kebab(name) => name,
            ComponentExportName::Interface(name) => name,
        }
    }
}
impl<'a> FromReader<'a> for ComponentImportName<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let byte1 = reader.read_u8()?;
        Ok(match byte1 {
            0x00 => ComponentImportName::Kebab(reader.read()?),
            0x01 => ComponentImportName::Interface(reader.read()?),
            0x02 => {
                let name = reader.read()?;
                let location = reader.read()?;
                let integrity = read_opt_integrity(reader)?;
                ComponentImportName::Url(name, location, integrity)
            }
            0x03 => {
                let name = reader.read()?;
                let location = reader.read()?;
                let integrity = read_opt_integrity(reader)?;
                ComponentImportName::Relative(name, location, integrity)
            }
            0x04 => {
                let name = reader.read()?;
                let integrity = reader.read()?;
                ComponentImportName::Naked(name, integrity)
            }
            0x05 => {
                let name = reader.read()?;
                let integrity = reader.read()?;
                ComponentImportName::Locked(name, integrity)
            }
            0x06 => {
                let name = reader.read()?;
                ComponentImportName::Unlocked(name)
            }
            x => return reader.invalid_leading_byte(x, "import name"),
        })
    }
}

fn read_opt_integrity<'a>(reader: &mut BinaryReader<'a>) -> Result<Option<&'a str>> {
    match reader.read_u8()? {
        0x00 => Ok(None),
        0x01 => Ok(Some(reader.read()?)),
        x => reader.invalid_leading_byte(x, "optional integrity"),
    }
}

impl<'a> FromReader<'a> for ComponentExportName<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let byte1 = reader.read_u8()?;
        Ok(match byte1 {
            0x00 => ComponentExportName::Kebab(reader.read()?),
            0x01 => ComponentExportName::Interface(reader.read()?),
            x => return reader.invalid_leading_byte(x, "import name"),
        })
    }
}

impl<'a: 'b, 'b> From<ComponentExportName<'a>> for ComponentImportName<'b> {
    fn from(import: ComponentExportName<'a>) -> ComponentImportName<'b> {
        match import {
            ComponentExportName::Kebab(name) => ComponentImportName::Kebab(name),
            ComponentExportName::Interface(name) => ComponentImportName::Interface(name),
        }
    }
}
