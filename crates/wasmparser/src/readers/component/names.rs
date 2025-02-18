use crate::{BinaryReader, BinaryReaderError, NameMap, Result, Subsection, Subsections};
use core::ops::Range;
use wasm_types::{
    ComponentFuncIdx, ComponentIdx, ComponentInstanceIdx, ComponentTypeIdx, ComponentValueIdx,
    CoreInstanceIdx, CoreModuleIdx, FuncIdx, GlobalIdx, MemIdx, TableIdx, TypeIdx,
};

/// Type used to iterate and parse the contents of the `component-name` custom
/// section in compnents, similar to the `name` section of core modules.
pub type ComponentNameSectionReader<'a> = Subsections<'a, ComponentName<'a>>;

/// Represents a name read from the names custom section.
#[derive(Clone)]
#[allow(missing_docs)]
pub enum ComponentName<'a> {
    Component {
        name: &'a str,
        name_range: Range<usize>,
    },
    CoreFuncs(NameMap<'a, FuncIdx>),
    CoreGlobals(NameMap<'a, GlobalIdx>),
    CoreMemories(NameMap<'a, MemIdx>),
    CoreTables(NameMap<'a, TableIdx>),
    CoreModules(NameMap<'a, CoreModuleIdx>),
    CoreInstances(NameMap<'a, CoreInstanceIdx>),
    CoreTypes(NameMap<'a, TypeIdx>),
    Types(NameMap<'a, ComponentTypeIdx>),
    Instances(NameMap<'a, ComponentInstanceIdx>),
    Components(NameMap<'a, ComponentIdx>),
    Funcs(NameMap<'a, ComponentFuncIdx>),
    Values(NameMap<'a, ComponentValueIdx>),

    /// An unknown [name subsection](https://webassembly.github.io/spec/core/appendix/custom.html#subsections).
    Unknown {
        /// The identifier for this subsection.
        ty: u8,
        /// The contents of this subsection.
        data: &'a [u8],
        /// The range of bytes, relative to the start of the original data
        /// stream, that the contents of this subsection reside in.
        range: Range<usize>,
    },
}

fn name_map<'a, I>(reader: &BinaryReader<'a>) -> Result<NameMap<'a, I>> {
    NameMap::new(reader.shrink())
}

impl<'a> Subsection<'a> for ComponentName<'a> {
    fn from_reader(id: u8, mut reader: BinaryReader<'a>) -> Result<Self> {
        let data = reader.remaining_buffer();
        let offset = reader.original_position();
        Ok(match id {
            0 => {
                let name = reader.read_string()?;
                if !reader.eof() {
                    return Err(BinaryReaderError::new(
                        "trailing data at the end of a name",
                        reader.original_position(),
                    ));
                }
                ComponentName::Component {
                    name,
                    name_range: offset..reader.original_position(),
                }
            }
            1 => match reader.read_u8()? {
                0x00 => match reader.read_u8()? {
                    0x00 => ComponentName::CoreFuncs(name_map(&reader)?),
                    0x01 => ComponentName::CoreTables(name_map(&reader)?),
                    0x02 => ComponentName::CoreMemories(name_map(&reader)?),
                    0x03 => ComponentName::CoreGlobals(name_map(&reader)?),
                    0x10 => ComponentName::CoreTypes(name_map(&reader)?),
                    0x11 => ComponentName::CoreModules(name_map(&reader)?),
                    0x12 => ComponentName::CoreInstances(name_map(&reader)?),
                    _ => {
                        return Ok(ComponentName::Unknown {
                            ty: 1,
                            data,
                            range: offset..offset + data.len(),
                        });
                    }
                },
                0x01 => ComponentName::Funcs(name_map(&reader)?),
                0x02 => ComponentName::Values(name_map(&reader)?),
                0x03 => ComponentName::Types(name_map(&reader)?),
                0x04 => ComponentName::Components(name_map(&reader)?),
                0x05 => ComponentName::Instances(name_map(&reader)?),
                _ => {
                    return Ok(ComponentName::Unknown {
                        ty: 1,
                        data,
                        range: offset..offset + data.len(),
                    });
                }
            },
            ty => ComponentName::Unknown {
                ty,
                data,
                range: offset..offset + data.len(),
            },
        })
    }
}
