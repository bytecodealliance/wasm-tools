use super::CanonicalOption;
use crate::{
    BinaryReader, Range, Result, SectionIteratorLimited, SectionReader, SectionWithLimitedItems,
};

/// Represents an adapter function in the component adapter function section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdapterFunction {
    /// The index of the adapter function's type.
    pub ty: u32,
    /// The index of the function being lifted.
    pub function: u32,
    /// The options for the adapter function.
    pub options: Box<[CanonicalOption]>,
}

impl AdapterFunction {
    fn new(reader: &mut BinaryReader) -> Result<Self> {
        Ok(Self {
            ty: reader.read_var_u32()?,
            function: reader.read_var_u32()?,
            options: (0..reader.read_var_u32()?)
                .map(|_| CanonicalOption::new(reader))
                .collect::<Result<_>>()?,
        })
    }
}

/// The function section reader for a WebAssembly component.
#[derive(Clone)]
pub struct AdapterFunctionSectionReader<'a> {
    reader: BinaryReader<'a>,
    count: u32,
}

impl<'a> AdapterFunctionSectionReader<'a> {
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

    /// Gets the number of adapter functions in the section.
    pub fn len(&self) -> u32 {
        self.count
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Reads adapter functions from the adapter function section.
    ///
    /// # Examples
    /// ```
    /// use wasmparser::component::AdapterFunctionSectionReader;
    /// # let data: &[u8] = &[0x1, 0x0, 0x1, 0x0];
    /// let functions = AdapterFunctionSectionReader::new(data, 0).unwrap();
    /// for function in functions {
    ///     let function = function.expect("function");
    ///     println!("{:?}", function);
    /// }
    /// ```
    pub fn read(&mut self) -> Result<AdapterFunction> {
        AdapterFunction::new(&mut self.reader)
    }
}

impl<'a> SectionReader for AdapterFunctionSectionReader<'a> {
    type Item = AdapterFunction;

    fn read(&mut self) -> Result<Self::Item> {
        AdapterFunctionSectionReader::read(self)
    }

    fn eof(&self) -> bool {
        self.reader.eof()
    }

    fn original_position(&self) -> usize {
        AdapterFunctionSectionReader::original_position(self)
    }

    fn range(&self) -> Range {
        self.reader.range()
    }
}

impl<'a> SectionWithLimitedItems for AdapterFunctionSectionReader<'a> {
    fn get_count(&self) -> u32 {
        AdapterFunctionSectionReader::len(self)
    }
}

impl<'a> IntoIterator for AdapterFunctionSectionReader<'a> {
    type Item = Result<AdapterFunction>;
    type IntoIter = SectionIteratorLimited<AdapterFunctionSectionReader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        SectionIteratorLimited::new(self)
    }
}

#[cfg(test)]
mod test {
    use crate::component::{
        CANONICAL_OPTION_COMPACT_UTF16, CANONICAL_OPTION_FREE, CANONICAL_OPTION_MEMORY,
        CANONICAL_OPTION_REALLOC, CANONICAL_OPTION_UTF16, CANONICAL_OPTION_UTF8,
    };

    use super::*;
    use anyhow::Result;

    #[test]
    fn it_parses_an_empty_section() -> Result<()> {
        let data: &[u8] = &[0];
        let reader = AdapterFunctionSectionReader::new(data, 0)?;
        assert!(reader.eof());
        assert_eq!(reader.len(), 0);
        Ok(())
    }

    #[test]
    fn it_parses_adapter_functions() -> Result<()> {
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
        let reader = AdapterFunctionSectionReader::new(data, 0)?;
        assert!(!reader.eof());
        assert_eq!(reader.len(), 1);

        let functions: Vec<_> = reader.into_iter().collect::<crate::Result<_>>()?;
        assert_eq!(
            functions,
            [AdapterFunction {
                ty: 0,
                function: 1,
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
