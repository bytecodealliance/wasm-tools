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
    pub name: ComponentExternName<'a>,
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
pub enum ComponentExternName<'a> {
    Kebab(&'a str),
    Interface(&'a str),
    Implementation(ImplementationImport<'a>),
}
/// Various types of implementation imports
#[derive(Debug, Copy, Clone)]
pub enum ImplementationImport<'a> {
    /// External Url
    Url(ImportMetadata<'a>),
    /// Relative path
    Relative(ImportMetadata<'a>),
    /// Locked Registry Import
    Locked(ImportMetadata<'a>),
    /// Unlocked Registry Import
    Unlocked(ImportMetadata<'a>),
}

/// Metadata For Import
#[derive(Debug, Copy, Clone)]
pub struct ImportMetadata<'a> {
    /// Import Name
    pub name: &'a str,
    /// Import Location
    pub location: &'a str,
    /// Import Integrity
    pub integrity: &'a str,
    /// Semver Range
    pub range: &'a str,
}

impl<'a> ImportMetadata<'a> {
    /// Returns the underlying string representing this name.
    pub fn as_str(&self) -> &'a str {
        self.name
    }
}
impl<'a> ImplementationImport<'a> {
    /// Returns the underlying string representing this name.
    pub fn as_str(&self) -> &'a str {
        match self {
            Self::Relative(metadata) => metadata.as_str(),
            Self::Url(metadata) => metadata.as_str(),
            Self::Locked(metadata) => metadata.as_str(),
            Self::Unlocked(metadata) => metadata.as_str(),
        }
    }
}

impl<'a> ComponentExternName<'a> {
    /// Returns the underlying string representing this name.
    pub fn as_str(&self) -> &'a str {
        match self {
            ComponentExternName::Kebab(name) => name,
            ComponentExternName::Interface(name) => name,
            ComponentExternName::Implementation(impl_import) => match impl_import {
                ImplementationImport::Url(metadata) => metadata.name,
                ImplementationImport::Relative(metadata) => metadata.name,
                ImplementationImport::Locked(metadata) => metadata.name,
                ImplementationImport::Unlocked(metadata) => metadata.name,
            },
        }
    }
}

impl<'a> FromReader<'a> for ComponentExternName<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> Result<Self> {
        let byte1 = reader.read_u8()?;
        dbg!("THIS RIGHT HERE");
        Ok(match byte1 {
            0x00 => ComponentExternName::Kebab(reader.read()?),
            0x01 => ComponentExternName::Interface(reader.read()?),
            0x02 | 0x03 | 0x04 | 0x05 => {
                ComponentExternName::Implementation(read_impl_import(byte1, reader)?)
            }
            x => return reader.invalid_leading_byte(x, "import name"),
        })
    }
}

fn read_impl_import<'a>(
    byte1: u8,
    reader: &mut BinaryReader<'a>,
) -> Result<ImplementationImport<'a>> {
    let name = reader.read()?;
    let location = reader.read()?;
    let integrity_or_range = if reader.peek()? != 0x01 {
        reader.read()?
    } else {
        ""
    };

    Ok(match byte1 {
        0x02 => ImplementationImport::Url(ImportMetadata {
            name,
            location,
            integrity: integrity_or_range,
            range: ""
        }),
        0x03 => ImplementationImport::Relative(ImportMetadata {
            name,
            location,
            integrity: integrity_or_range,
            range: ""
        }),
        0x04 => ImplementationImport::Locked(ImportMetadata {
            name,
            location,
            integrity: integrity_or_range,
            range: ""
        }),
        0x05 => ImplementationImport::Unlocked(ImportMetadata {
            name,
            location,
            integrity: "",
            range: integrity_or_range
        }),
        x => reader.invalid_leading_byte(x, "implementation import")?,
    })
}
