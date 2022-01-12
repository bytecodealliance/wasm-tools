use crate::{
    BinaryReader, BinaryReaderError, Range, Result, SectionIteratorLimited, SectionReader,
    SectionWithLimitedItems,
};

pub(crate) const CANONICAL_OPTION_UTF8: u32 = 0x00;
pub(crate) const CANONICAL_OPTION_UTF16: u32 = 0x01;
pub(crate) const CANONICAL_OPTION_COMPACT_UTF16: u32 = 0x02;
pub(crate) const CANONICAL_OPTION_MEMORY: u32 = 0x03;
pub(crate) const CANONICAL_OPTION_REALLOC: u32 = 0x04;
pub(crate) const CANONICAL_OPTION_FREE: u32 = 0x05;

/// Represents options for functions and adapter functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalOption {
    /// The string types in the function signature are UTF-8 encoded.
    UTF8,
    /// The string types in the function signature are UTF-16 encoded.
    UTF16,
    /// The string types in the function signature are compact UTF-16 encoded.
    CompactUTF16,
    /// Specifies the memory to use.
    Memory(u32),
    /// Specifies the function to use to reallocate memory.
    Realloc(u32),
    /// Specifies the function to use to free memory.
    Free(u32),
}

impl CanonicalOption {
    pub(crate) fn new(reader: &mut BinaryReader) -> Result<Self> {
        Ok(match reader.read_u8()? {
            CANONICAL_OPTION_UTF8 => Self::UTF8,
            CANONICAL_OPTION_UTF16 => Self::UTF16,
            CANONICAL_OPTION_COMPACT_UTF16 => Self::CompactUTF16,
            CANONICAL_OPTION_MEMORY => Self::Memory(reader.read_var_u32()?),
            CANONICAL_OPTION_REALLOC => Self::Realloc(reader.read_var_u32()?),
            CANONICAL_OPTION_FREE => Self::Free(reader.read_var_u32()?),
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid byte (0x{:x}) in canonical option", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// Represents a function in the component function section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// The index of the function's type.
    pub ty: u32,
    /// The index of the adapter function being lowered.
    pub adapter: u32,
    /// The options for the function.
    pub options: Box<[CanonicalOption]>,
}

impl Function {
    fn new(reader: &mut BinaryReader) -> Result<Self> {
        Ok(Self {
            ty: reader.read_var_u32()?,
            adapter: reader.read_var_u32()?,
            options: (0..reader.read_var_u32()?)
                .map(|_| CanonicalOption::new(reader))
                .collect::<Result<_>>()?,
        })
    }
}

/// The function section reader for a WebAssembly component.
#[derive(Clone)]
pub struct FunctionSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> FunctionSectionReader<'a> {
    /// Creates a new function section reader for the given data and initial offset.
    pub fn new(data: &'a [u8], offset: usize) -> Result<Self> {
        let mut reader = BinaryReader::new_with_offset(data, offset);
        let count = reader.read_var_u32()?;
        Ok(Self { reader, count })
    }

    /// Gets the original position of the reader.
    pub fn original_position(&self) -> usize {
        self.reader.original_position()
    }

    /// Gets the number of functions in the section.
    pub fn len(&self) -> u32 {
        self.count
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Reads functions from the function section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::component::FunctionSectionReader;
    /// # let data: &[u8] = &[0x1, 0x0, 0x1, 0x0];
    /// let functions = FunctionSectionReader::new(data, 0).unwrap();
    /// for function in functions {
    ///     let function = function.expect("function");
    ///     println!("{:?}", function);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<Function> {
        Function::new(&mut self.reader)
    }
}

impl<'a> SectionReader for FunctionSectionReader<'a> {
    type Item = Function;

    fn read(&mut self) -> Result<Self::Item> {
        FunctionSectionReader::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        FunctionSectionReader::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for FunctionSectionReader<'a> {
    fn get_count(&self) -> u32 {
        FunctionSectionReader::len(self)
    }
}

impl<'a> IntoIterator for FunctionSectionReader<'a> {
    type Item = Result<Function>;
    type IntoIter = SectionIteratorLimited<FunctionSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;

    #[test]
    fn it_parses_an_empty_section() -> Result<()> {
        let data: &[u8] = &[0];
        let reader = FunctionSectionReader::new(data, 0)?;
        assert!(reader.eof());
        assert_eq!(reader.len(), 0);
        Ok(())
    }

    #[test]
    fn it_parses_functions() -> Result<()> {
        let data: &[u8] = &[
            1,
            0,
            1,
            6,
            CANONICAL_OPTION_UTF8 as u8,
            CANONICAL_OPTION_UTF16 as u8,
            CANONICAL_OPTION_COMPACT_UTF16 as u8,
            CANONICAL_OPTION_MEMORY as u8,
            2,
            CANONICAL_OPTION_REALLOC as u8,
            3,
            CANONICAL_OPTION_FREE as u8,
            4,
        ];
        let reader = FunctionSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        let functions: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;
        assert_eq!(
            functions,
            [Function {
                ty: 0,
                adapter: 1,
                options: [
                    CanonicalOption::UTF8,
                    CanonicalOption::UTF16,
                    CanonicalOption::CompactUTF16,
                    CanonicalOption::Memory(2),
                    CanonicalOption::Realloc(3),
                    CanonicalOption::Free(4)
                ]
                .into(),
            },]
        );
        Ok(())
    }
}
