//! Module for parsing WebAssembly components.

use crate::{
    parser::{delimited, subreader, usize_to_u64},
    BinaryReader, BinaryReaderError, Range, Result, WASM_MODULE_VERSION,
};
use std::fmt;
use std::iter;

mod adapters;
mod aliases;
mod exports;
mod functions;
mod imports;
mod instances;
mod types;

pub use self::adapters::*;
pub use self::aliases::*;
pub use self::exports::*;
pub use self::functions::*;
pub use self::imports::*;
pub use self::instances::*;
pub use self::types::*;

// The currently supported version in a component's header.
const WASM_COMPONENT_VERSION: u32 = 0x0002000a;

const SECTION_CUSTOM: u8 = 0x00;
const SECTION_TYPE: u8 = 0x01;
const SECTION_IMPORT: u8 = 0x02;
const SECTION_MODULE: u8 = 0x03;
const SECTION_INSTANCE: u8 = 0x04;
const SECTION_ALIAS: u8 = 0x05;
const SECTION_EXPORT: u8 = 0x06;
const SECTION_FUNCTION: u8 = 0x07;
const SECTION_ADAPTER_FUNCTION: u8 = 0x08;

const INDEX_REF_INSTANCE: u32 = 0x00;
const INDEX_REF_MODULE: u32 = 0x01;
const INDEX_REF_FUNCTION: u32 = 0x02;
const INDEX_REF_TABLE: u32 = 0x03;
const INDEX_REF_MEMORY: u32 = 0x04;
const INDEX_REF_GLOBAL: u32 = 0x05;
const INDEX_REF_ADAPTER_FUNCTION: u32 = 0x06;

/// Parses an entire section resident in memory into a `Payload`.
///
/// Requires that `len` bytes are resident in `reader` and uses `ctor`/`variant`
/// to construct the section to return.
fn section<'a, T>(
    reader: &mut BinaryReader<'a>,
    len: u32,
    ctor: fn(&'a [u8], usize) -> Result<T>,
    variant: fn(T) -> Payload<'a>,
) -> Result<Payload<'a>> {
    let offset = reader.original_position();
    let payload = reader.read_bytes(len as usize)?;
    // clear the hint for "need this many more bytes" here because we already
    // read all the bytes, so it's not possible to read more bytes if this
    // fails.
    let reader = ctor(payload, offset).map_err(BinaryReaderError::clear_hint)?;
    Ok(variant(reader))
}

/// Represents a reference to an index in a WebAssembly component section.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum IndexRef {
    /// The reference is to an instance in the instance section.
    Instance(u32),
    /// The reference is to a module in the module section.
    Module(u32),
    /// The reference is to a function in the function section.
    Function(u32),
    /// The reference is to a table in the table section.
    Table(u32),
    /// The reference is to a memory in the memory section.
    Memory(u32),
    /// The reference is to a global in the global section.
    Global(u32),
    /// The reference is to an adapter function in the adapter function section.
    AdapterFunction(u32),
}

impl IndexRef {
    fn new(reader: &mut BinaryReader) -> Result<Self> {
        Ok(match reader.read_u8()? {
            INDEX_REF_INSTANCE => IndexRef::Instance(reader.read_var_u32()?),
            INDEX_REF_MODULE => IndexRef::Module(reader.read_var_u32()?),
            INDEX_REF_FUNCTION => IndexRef::Function(reader.read_var_u32()?),
            INDEX_REF_TABLE => IndexRef::Table(reader.read_var_u32()?),
            INDEX_REF_MEMORY => IndexRef::Memory(reader.read_var_u32()?),
            INDEX_REF_GLOBAL => IndexRef::Global(reader.read_var_u32()?),
            INDEX_REF_ADAPTER_FUNCTION => IndexRef::AdapterFunction(reader.read_var_u32()?),
            x => {
                return Err(BinaryReaderError::new(
                    format!("invalid byte (0x{:x}) in index reference", x),
                    reader.original_position() - 1,
                ))
            }
        })
    }
}

/// An incremental parser of a binary WebAssembly component.
///
/// This type is intended to be used to incrementally parse a WebAssembly component
/// as bytes become available for the component. This can also be used to parse
/// components that are already entirely resident within memory.
///
/// This primary function for a parser is the [`Parser::parse`] function which
/// will incrementally consume input. You can also use the [`Parser::parse_all`]
/// function to parse a component that is entirely resident in memory.
#[derive(Debug, Clone)]
pub struct Parser {
    state: State,
    offset: u64,
    max_size: u64,
}

impl Parser {
    /// Creates a new component parser.
    ///
    /// Reports errors and ranges relative to `offset` provided, where `offset`
    /// is some logical offset within the input stream that we're parsing.
    pub fn new(offset: u64) -> Self {
        Self::new_with_max_size(offset, u64::max_value())
    }

    pub(crate) fn new_with_max_size(offset: u64, max_size: u64) -> Self {
        Self {
            state: State::Header,
            offset,
            max_size,
        }
    }

    /// Attempts to parse a chunk of data.
    ///
    /// This method will attempt to parse the next incremental portion of a
    /// WebAssembly binary. Data available for the component is provided as `data`,
    /// and the data can be incomplete if more data has yet to arrive for the
    /// component. The `eof` flag indicates whether `data` represents all possible
    /// data for the component and no more data will ever be received.
    ///
    /// There are two ways parsing can succeed with this method:
    ///
    /// * `Chunk::NeedMoreData` - this indicates that there is not enough bytes
    ///   in `data` to parse a chunk of this component. The caller needs to wait
    ///   for more data to be available in this situation before calling this
    ///   method again. It is guaranteed that this is only returned if `eof` is
    ///   `false`.
    ///
    /// * `Chunk::Parsed` - this indicates that a chunk of the input was
    ///   successfully parsed. The payload is available in this variant of what
    ///   was parsed, and this also indicates how many bytes of `data` was
    ///   consumed. It's expected that the caller will not provide these bytes
    ///   back to the [`Parser`] again.
    ///
    /// Note that all `Chunk` return values are connected, with a lifetime, to
    /// the input buffer. Each parsed chunk borrows the input buffer and is a
    /// view into it for successfully parsed chunks.
    ///
    /// It is expected that you'll call this method until `Payload::End` is
    /// reached, at which point you're guaranteed that the component has completely
    /// parsed. Note that complete parsing, for the top-level WebAssembly component,
    /// implies that `data` is empty and `eof` is `true`.
    ///
    /// # Errors
    ///
    /// Parse errors are returned as an `Err`. Errors can happen when the
    /// structure of the component is unexpected, or if sections are too large for
    /// example. Note that errors are not returned for malformed *contents* of
    /// sections here. Sections are generally not individually parsed and each
    /// returned [`Payload`] needs to be iterated over further to detect all
    /// errors.
    ///
    /// # Examples
    ///
    /// An example of reading a WebAssembly component file from a stream (`std::io::Read`)
    /// and incrementally parsing it.
    ///
    /// ```
    /// use std::io::Read;
    /// use anyhow::Result;
    /// use wasmparser::component::{Parser, SubParser, Chunk, Payload};
    ///
    /// fn parse(mut reader: impl Read) -> Result<()> {
    ///     let mut buf = Vec::new();
    ///     // Start with a component parser at the top-level
    ///     let mut parser = SubParser::Component(Parser::new(0));
    ///     let mut eof = false;
    ///     let mut stack = Vec::new();
    ///
    ///     loop {
    ///         let (hint, payload) = match &mut parser {
    ///             SubParser::Module(parser) => match parser.parse(&buf, eof)? {
    ///                 wasmparser::Chunk::NeedMoreData(hint) => (Some(hint), None),
    ///                 wasmparser::Chunk::Parsed { payload, consumed } => (None, Some((Payload::SubmodulePayload(payload), consumed))),
    ///             },
    ///             SubParser::Component(parser) => match parser.parse(&buf, eof)? {
    ///                 Chunk::NeedMoreData(hint) => (Some(hint), None),
    ///                 Chunk::Parsed { payload, consumed } => (None, Some((payload, consumed))),
    ///             },
    ///         };
    ///
    ///         if let Some(hint) = hint {
    ///             assert!(!eof); // otherwise an error would be returned
    ///
    ///             // Use the hint to preallocate more space, then read
    ///             // some more data into our buffer.
    ///             //
    ///             // Note that the buffer management here is not ideal,
    ///             // but it's compact enough to fit in an example!
    ///             let len = buf.len();
    ///             buf.extend((0..hint).map(|_| 0u8));
    ///             let n = reader.read(&mut buf[len..])?;
    ///             buf.truncate(len + n);
    ///             eof = n == 0;
    ///             continue;
    ///         }
    ///         let (payload, consumed) = payload.unwrap();
    ///         match payload {
    ///             // Each of these would be handled individually as necessary
    ///             Payload::Version { .. } => { /* ... */ }
    ///             Payload::TypeSection(_) => { /* ... */ }
    ///             Payload::ImportSection(_) => { /* ... */ }
    ///             Payload::InstanceSection(_) => { /* ... */ }
    ///             Payload::AliasSection(_) => { /* ... */ }
    ///             Payload::ExportSection(_) => { /* ... */ }
    ///             Payload::FunctionSection(_) => { /* ... */ }
    ///             Payload::AdapterFunctionSection(_) => { /* ... */ }
    ///             // When parsing nested components/modules we need to switch which
    ///             // `Parser` we're using.
    ///             Payload::ModuleSectionStart { .. } => { /* ... */ }
    ///             Payload::ModuleSectionEntry { subparser, .. } => {
    ///                 stack.push(parser);
    ///                 parser = subparser;
    ///             }
    ///             Payload::SubmodulePayload(payload) => {
    ///                // This is a payload from a submodule being parsed.
    ///             }
    ///             Payload::CustomSection { name, .. } => { /* ... */ }
    ///             // most likely you'd return an error here
    ///             Payload::UnknownSection { id, .. } => { /* ... */ }
    ///             // Once we've reached the end of a component we either resume
    ///             // at the parent component or we break out of the loop because
    ///             // we're done.
    ///             Payload::End => {
    ///                 if let Some(parent_parser) = stack.pop() {
    ///                     parser = parent_parser;
    ///                 } else {
    ///                     break;
    ///                 }
    ///             }
    ///         }
    ///
    ///         // once we're done processing the payload we can forget the
    ///         // original.
    ///         buf.drain(..consumed);
    ///     }
    ///
    ///     Ok(())
    /// }
    ///
    /// # parse(&b"\0asm\x0a\0\x02\0"[..]).unwrap();
    /// ```
    pub fn parse<'a>(&mut self, data: &'a [u8], eof: bool) -> Result<Chunk<'a>> {
        let (data, eof) = if usize_to_u64(data.len()) > self.max_size {
            (&data[..(self.max_size as usize)], true)
        } else {
            (data, eof)
        };
        let mut reader = BinaryReader::new_with_offset(data, self.offset as usize);
        match self.parse_reader(&mut reader, eof) {
            Ok(payload) => {
                // Be sure to update our offset with how far we got in the
                // reader
                self.offset += usize_to_u64(reader.position);
                self.max_size -= usize_to_u64(reader.position);
                Ok(Chunk::Parsed {
                    consumed: reader.position,
                    payload,
                })
            }
            Err(e) => {
                // If we're at EOF then there's no way we can recover from any
                // error, so continue to propagate it.
                if eof {
                    return Err(e);
                }

                // If our error doesn't look like it can be resolved with more
                // data being pulled down, then propagate it, otherwise switch
                // the error to "feed me please"
                match e.inner.needed_hint {
                    Some(hint) => Ok(Chunk::NeedMoreData(usize_to_u64(hint))),
                    None => Err(e),
                }
            }
        }
    }

    fn parse_reader<'a>(
        &mut self,
        reader: &mut BinaryReader<'a>,
        eof: bool,
    ) -> Result<Payload<'a>> {
        match self.state {
            State::Header => {
                let start = reader.original_position();
                let version = reader.read_module_version()?;
                if version != WASM_COMPONENT_VERSION {
                    return Err(BinaryReaderError::new(
                        format!(
                            "WebAssembly component version `{}` is not supported",
                            version
                        ),
                        reader.original_position() - 4,
                    ));
                }
                self.state = State::SectionStart;
                Ok(Payload::Version {
                    version,
                    range: Range {
                        start,
                        end: reader.original_position(),
                    },
                })
            }
            State::SectionStart => {
                // If we're at eof and there are no bytes in our buffer, then
                // that means we reached the end of the component file since it's
                // just a bunch of sections concatenated after the component's
                // header.
                if eof && reader.bytes_remaining() == 0 {
                    return Ok(Payload::End);
                }

                let id = reader.read_var_u7()? as u8;
                let len_pos = reader.position;
                let mut len = reader.read_var_u32()?;

                // Test to make sure that this section actually fits within
                // `Parser::max_size`. This doesn't matter for top-level components
                // but it is required for nested component/modules to correctly ensure
                // that all sections live entirely within their section of the
                // original file.
                let section_overflow = self
                    .max_size
                    .checked_sub(usize_to_u64(reader.position))
                    .and_then(|s| s.checked_sub(len.into()))
                    .is_none();
                if section_overflow {
                    return Err(BinaryReaderError::new("section too large", len_pos));
                }

                match id {
                    SECTION_CUSTOM => {
                        let start = reader.original_position();
                        let range = Range {
                            start,
                            end: reader.original_position() + len as usize,
                        };
                        let mut content = subreader(reader, len)?;
                        // Note that if this fails we can't read any more bytes,
                        // so clear the "we'd succeed if we got this many more
                        // bytes" because we can't recover from "eof" at this point.
                        let name = content
                            .read_string()
                            .map_err(BinaryReaderError::clear_hint)?;
                        Ok(Payload::CustomSection {
                            name,
                            data_offset: content.original_position(),
                            data: content.remaining_buffer(),
                            range,
                        })
                    }
                    SECTION_TYPE => {
                        section(reader, len, TypeSectionReader::new, Payload::TypeSection)
                    }
                    SECTION_IMPORT => section(
                        reader,
                        len,
                        ImportSectionReader::new,
                        Payload::ImportSection,
                    ),
                    SECTION_MODULE => {
                        let start = reader.original_position();
                        let count = delimited(reader, &mut len, |r| r.read_var_u32())?;
                        let range = Range {
                            start,
                            end: reader.original_position() + len as usize,
                        };
                        self.state = State::ModuleSection {
                            remaining: count,
                            len,
                        };
                        Ok(Payload::ModuleSectionStart {
                            count,
                            range,
                            size: len,
                        })
                    }
                    SECTION_INSTANCE => section(
                        reader,
                        len,
                        InstanceSectionReader::new,
                        Payload::InstanceSection,
                    ),
                    SECTION_ALIAS => {
                        section(reader, len, AliasSectionReader::new, Payload::AliasSection)
                    }
                    SECTION_EXPORT => section(
                        reader,
                        len,
                        ExportSectionReader::new,
                        Payload::ExportSection,
                    ),
                    SECTION_FUNCTION => section(
                        reader,
                        len,
                        FunctionSectionReader::new,
                        Payload::FunctionSection,
                    ),
                    SECTION_ADAPTER_FUNCTION => section(
                        reader,
                        len,
                        AdapterFunctionSectionReader::new,
                        Payload::AdapterFunctionSection,
                    ),
                    id => {
                        let offset = reader.original_position();
                        let contents = reader.read_bytes(len as usize)?;
                        let range = Range {
                            start: offset,
                            end: offset + len as usize,
                        };
                        Ok(Payload::UnknownSection {
                            id,
                            contents,
                            range,
                        })
                    }
                }
            }
            State::ModuleSection {
                remaining: 0,
                len: 0,
            } => {
                self.state = State::SectionStart;
                self.parse_reader(reader, eof)
            }
            // ... otherwise trailing bytes with no remaining entries in these
            // sections indicates an error.
            State::ModuleSection { remaining: 0, len } => {
                debug_assert!(len > 0);
                Err(BinaryReaderError::new(
                    "trailing bytes at end of section",
                    reader.original_position(),
                ))
            }
            State::ModuleSection { remaining, mut len } => {
                let size = delimited(reader, &mut len, |r| r.read_var_u32())?;
                match len.checked_sub(size) {
                    Some(i) => len = i,
                    None => {
                        return Err(BinaryReaderError::new(
                            "Unexpected EOF",
                            reader.original_position(),
                        ));
                    }
                }
                let subparser = {
                    // We need to peek at the entry's header to see if it is a component or module
                    let mut reader = reader.clone();
                    match reader.read_module_version()? {
                        WASM_MODULE_VERSION => SubParser::Module(crate::Parser::new_with_max_size(
                            usize_to_u64(reader.original_position()),
                            size.into(),
                        )),
                        WASM_COMPONENT_VERSION => SubParser::Component(Parser::new_with_max_size(
                            usize_to_u64(reader.original_position()),
                            size.into(),
                        )),
                        version => {
                            return Err(BinaryReaderError::new(
                                format!(
                                    "Module section entry has unsupported version `{}`",
                                    version
                                ),
                                reader.original_position() - 4,
                            ));
                        }
                    }
                };

                self.state = State::ModuleSection {
                    remaining: remaining - 1,
                    len,
                };
                let range = Range {
                    start: reader.original_position(),
                    end: reader.original_position() + size as usize,
                };
                self.max_size -= u64::from(size);
                self.offset += u64::from(size);
                Ok(Payload::ModuleSectionEntry { subparser, range })
            }
        }
    }

    /// Convenience function that can be used to parse a component that is entirely
    /// resident in memory.
    ///
    /// This function will parse the `data` provided as a WebAssembly component,
    /// assuming that `data` represents the entire WebAssembly component.
    ///
    /// Note that when this function yields `ModuleSectionEntry`
    /// no action needs to be taken with the returned sub-parser. The sub-parser will be
    /// automatically switched to internally and more payloads will continue to
    /// be returned.
    pub fn parse_all(self, mut data: &[u8]) -> impl Iterator<Item = Result<Payload>> {
        let mut stack = Vec::new();
        let mut cur = SubParser::Component(self);
        let mut done = false;
        iter::from_fn(move || {
            if done {
                return None;
            }

            let payload = match &mut cur {
                SubParser::Module(parser) => match parser.parse(data, true) {
                    Ok(crate::Chunk::NeedMoreData(_)) => unreachable!(),
                    Ok(crate::Chunk::Parsed { payload, consumed }) => {
                        data = &data[consumed..];

                        // Eventually core WebAssembly modules won't support submodules
                        // Until then, just return an error when encountered
                        if let crate::Payload::ModuleSectionStart { range, .. } = &payload {
                            return Some(Err(BinaryReaderError::new(
                                "WebAssembly modules may not contain submodules",
                                range.start,
                            )));
                        }

                        Payload::SubmodulePayload(payload)
                    }
                    Err(e) => return Some(Err(e)),
                },
                SubParser::Component(parser) => match parser.parse(data, true) {
                    Ok(Chunk::NeedMoreData(_)) => unreachable!(),
                    Ok(Chunk::Parsed { payload, consumed }) => {
                        data = &data[consumed..];
                        payload
                    }
                    Err(e) => return Some(Err(e)),
                },
            };

            match &payload {
                // If a module ends then we either finished the current
                // module or, if there's a parent, we switch back to
                // resuming parsing the parent.
                Payload::End => match stack.pop() {
                    Some(p) => cur = p,
                    None => done = true,
                },

                // When we enter a nested component/module then we need to update our
                // current parser, saving off the previous state.
                //
                // Afterwards we turn the loop again to recurse in parsing the
                // nested component/module.
                Payload::ModuleSectionEntry {
                    subparser,
                    range: _,
                } => {
                    stack.push(cur.clone());
                    cur = subparser.clone();
                }

                _ => {}
            }

            Some(Ok(payload))
        })
    }

    /// Skip parsing the component's module section entirely.
    ///
    /// This function can be used to indicate, after receiving
    /// `ModuleSectionStart`, that the section will not be parsed.
    ///
    /// The caller will be responsible for skipping `size` bytes (found in the
    /// `ModuleSectionStart` payload). Bytes should only be fed into `parse`
    /// after the `size` bytes have been skipped.
    ///
    /// # Panics
    ///
    /// This function will panic if the parser is not in a state where it's
    /// parsing the module section.
    ///
    /// # Examples
    ///
    /// ```
    /// use wasmparser::{Result, Range, SectionReader, component::{Parser, Chunk, Payload}};
    ///
    /// fn objdump_headers(mut bytes: &[u8]) -> Result<()> {
    ///     let mut parser = Parser::new(0);
    ///     loop {
    ///         let payload = match parser.parse(bytes, true)? {
    ///             Chunk::Parsed { consumed, payload } => {
    ///                 bytes = &bytes[consumed..];
    ///                 payload
    ///             }
    ///             // this state isn't possible with `eof = true`
    ///             Chunk::NeedMoreData(_) => unreachable!(),
    ///         };
    ///         match payload {
    ///             Payload::TypeSection(s) => print_range("type section", &s.range()),
    ///             // .. other sections
    ///
    ///             // Print the range of the module section we see, but don't
    ///             // actually iterate over each individual module.
    ///             Payload::ModuleSectionStart { range, size, .. } => {
    ///                 print_range("module section", &range);
    ///                 parser.skip_section();
    ///                 bytes = &bytes[size as usize..];
    ///             }
    ///             Payload::End => break,
    ///             _ => {}
    ///         }
    ///     }
    ///     Ok(())
    /// }
    ///
    /// fn print_range(section: &str, range: &Range) {
    ///     println!("{:>40}: {:#010x} - {:#010x}", section, range.start, range.end);
    /// }
    /// ```
    pub fn skip_section(&mut self) {
        let skip = match self.state {
            State::ModuleSection { remaining: _, len } => len,
            _ => panic!("wrong state to call `skip_section`"),
        };
        self.offset += u64::from(skip);
        self.max_size -= u64::from(skip);
        self.state = State::SectionStart;
    }
}

#[derive(Debug, Clone)]
enum State {
    /// The header is being parsed.
    Header,
    /// The start of a section is being parsed.
    SectionStart,
    /// The module section is being incrementally parsed.
    ModuleSection { remaining: u32, len: u32 },
}

/// A successful return payload from [`Parser::parse`].
///
/// On success one of two possible values can be returned, either that more data
/// is needed to continue parsing or a chunk of the input was parsed, indicating
/// how much of it was parsed.
#[derive(Debug)]
pub enum Chunk<'a> {
    /// This can be returned at any time and indicates that more data is needed
    /// to proceed with parsing. Zero bytes were consumed from the input to
    /// [`Parser::parse`]. The `usize` value here is a hint as to how many more
    /// bytes are needed to continue parsing.
    NeedMoreData(u64),

    /// A chunk was successfully parsed.
    Parsed {
        /// This many bytes of the `data` input to [`Parser::parse`] were
        /// consumed to produce `payload`.
        consumed: usize,
        /// The value that we actually parsed.
        payload: Payload<'a>,
    },
}

/// Values that can be parsed from a WebAssembly component.
///
/// This enumeration is all possible chunks of pieces that can be parsed by a
/// [`Parser`] from a binary WebAssembly component. Note that for many sections the
/// entire section is parsed all at once, whereas other functions, like the module
/// section, are parsed incrementally.
///
/// Note that payloads, when returned, do not indicate that the component is
/// valid. For example when you receive a `Payload::TypeSection` the type
/// section itself has not yet actually been parsed. The reader returned will be
/// able to parse it, but you'll have to actually iterate the reader to do the
/// full parse. Each payload returned is intended to be a *window* into the
/// original `data` passed to [`Parser::parse`] which can be further processed
/// if necessary.
pub enum Payload<'a> {
    /// Indicates the header of a WebAssembly component.
    ///
    /// This header also indicates the version number that was parsed, which is
    /// currently always 2.
    Version {
        /// The version number found
        version: u32,
        /// The range of bytes that were parsed to consume the header of the
        /// component. Note that this range is relative to the start of the byte
        /// stream.
        range: Range,
    },

    /// A custom section was found.
    CustomSection {
        /// The name of the custom section.
        name: &'a str,
        /// The offset, relative to the start of the original component, that the
        /// `data` payload for this custom section starts at.
        data_offset: usize,
        /// The actual contents of the custom section.
        data: &'a [u8],
        /// The range of bytes that specify this whole custom section (including
        /// both the name of this custom section and its data) specified in
        /// offsets relative to the start of the byte stream.
        range: Range,
    },

    /// A type section was received, and the provided reader can be used to
    /// parse the contents of the type section.
    TypeSection(TypeSectionReader<'a>),

    /// An import section was received, and the provided reader can be used to
    /// parse the contents of the import section.
    ImportSection(ImportSectionReader<'a>),

    /// Indicator of the start of a component's modules section.
    ///
    /// It is guaranteed that the next `count` payloads will be of type
    /// `ModuleSectionEntry`.
    ModuleSectionStart {
        /// The number of inline component or modules in this section.
        count: u32,
        /// The range of bytes that represent this section, specified in
        /// offsets relative to the start of the byte stream.
        range: Range,
        /// The size, in bytes, of the remaining contents of this section.
        size: u32,
    },

    /// An entry of the module section was parsed.
    ///
    /// This variant is special in that it returns a `SubParser`. Upon
    /// receiving a `ModuleSectionEntry` it is expected that the returned
    /// `SubParser` will be used instead of the parent `Parser` until the parse has
    /// finished. You'll need to feed data into the `SubParser` returned until it
    /// returns `Payload::End`. After that you'll switch back to the parent
    /// parser to resume parsing the rest of the module section.
    ModuleSectionEntry {
        /// The subparser to use for parsing the module section entry.
        subparser: SubParser,
        /// The range of bytes, relative to the start of the input stream, of
        /// the bytes containing this module entry.
        range: Range,
    },

    /// A submodule payload was parsed when using `Parse::parse_all`.
    ///
    /// Submodule payloads are returned when a submodule is being parsed
    /// from a component's module section.
    ///
    /// This variant is only ever returned from calls to `Parse::parse_all`.
    SubmodulePayload(crate::Payload<'a>),

    /// An instance section was received, and the provided reader can be used to
    /// parse the contents of the instance section.
    InstanceSection(InstanceSectionReader<'a>),

    /// An alias section was received, and the provided reader can be used to
    /// parse the contents of the alias section.
    AliasSection(AliasSectionReader<'a>),

    /// An export section was received, and the provided reader can be used to
    /// parse the contents of the export section.
    ExportSection(ExportSectionReader<'a>),

    /// A function section was received, and the provided reader can be used to
    /// parse the contents of the function section.
    FunctionSection(FunctionSectionReader<'a>),

    /// An adapter function section was received, and the provided reader can be used to
    /// parse the contents of the adapter function section.
    AdapterFunctionSection(AdapterFunctionSectionReader<'a>),

    /// An unknown section was found.
    ///
    /// This variant is returned for all unknown sections in a component file. This
    /// likely wants to be interpreted as an error by consumers of the parser,
    /// but this can also be used to parse sections unknown to wasmparser at
    /// this time.
    UnknownSection {
        /// The 8-bit identifier for this section.
        id: u8,
        /// The contents of this section.
        contents: &'a [u8],
        /// The range of bytes, relative to the start of the original data
        /// stream, that the contents of this section reside in.
        range: Range,
    },

    /// The end of the WebAssembly component was reached.
    End,
}

impl fmt::Debug for Payload<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Payload::Version { version, range } => f
                .debug_struct("Version")
                .field("version", version)
                .field("range", range)
                .finish(),
            Payload::CustomSection {
                name,
                data_offset,
                data: _,
                range,
            } => f
                .debug_struct("CustomSection")
                .field("name", name)
                .field("data_offset", data_offset)
                .field("range", range)
                .field("data", &"...")
                .finish(),
            Payload::TypeSection(_) => f.debug_tuple("TypeSection").field(&"...").finish(),
            Payload::ImportSection(_) => f.debug_tuple("ImportSection").field(&"...").finish(),
            Payload::ModuleSectionStart { count, range, size } => f
                .debug_struct("ModuleSectionStart")
                .field("count", count)
                .field("range", range)
                .field("size", size)
                .finish(),
            Payload::ModuleSectionEntry {
                subparser: _,
                range,
            } => f
                .debug_struct("ModuleSectionEntry")
                .field("range", range)
                .finish(),
            Payload::SubmodulePayload(p) => p.fmt(f),
            Payload::InstanceSection(_) => f.debug_tuple("InstanceSection").field(&"...").finish(),
            Payload::AliasSection(_) => f.debug_tuple("AliasSection").field(&"...").finish(),
            Payload::ExportSection(_) => f.debug_tuple("ExportSection").field(&"...").finish(),
            Payload::FunctionSection(_) => f.debug_tuple("FunctionSection").field(&"...").finish(),
            Payload::AdapterFunctionSection(_) => f
                .debug_tuple("AdapterFunctionSection")
                .field(&"...")
                .finish(),
            Payload::UnknownSection { id, range, .. } => f
                .debug_struct("UnknownSection")
                .field("id", id)
                .field("range", range)
                .finish(),
            Payload::End => f.write_str("End"),
        }
    }
}

/// Represents the possible sub-parsers that can be returned while
/// parsing a WebAssembly component's module section.
#[derive(Debug, Clone)]
pub enum SubParser {
    /// The subparser for a WebAssembly module.
    Module(crate::Parser),
    /// The subparser for a WebAssembly component.
    Component(Parser),
}
