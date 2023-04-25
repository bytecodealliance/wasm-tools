use std::borrow::Cow;

use crate::{CustomSection, Encode, Section};

/// The "core" custom section for coredumps, as described in the
/// [tool-conventions
/// repository](https://github.com/WebAssembly/tool-conventions/blob/main/Coredump.md)
///
/// # Example
///
/// ```
/// use wasm_encoder::{CoreDumpSection, Module};
/// let core = CoreDumpSection::new("MyModule.wasm");
/// let mut module = Module::new();
/// module.section(&core);
/// ```
#[derive(Clone, Debug, Default)]
pub struct CoreDumpSection {
    name: String,
}

impl CoreDumpSection {
    /// Create a new core dump section encoder
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        CoreDumpSection { name }
    }
}

impl Encode for CoreDumpSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        let mut data = vec![0];
        self.name.encode(&mut data);
        CustomSection {
            name: "core".into(),
            data: Cow::Owned(data),
        }
        .encode(sink);
    }
}

impl Section for CoreDumpSection {
    fn id(&self) -> u8 {
        crate::core::SectionId::Custom as u8
    }
}

/// A "corestack" custom section as described in the [tool-conventions
/// repository](https://github.com/WebAssembly/tool-conventions/blob/main/Coredump.md)
///
/// # Example
///
/// ```
/// use wasm_encoder::{CoreDumpStackSection, Module, CoreDumpValue};
/// let mut thread = CoreDumpStackSection::new("main");
///
/// let func_index = 42;
/// let code_offset = 0x1234;
/// let locals = vec![CoreDumpValue::I32(1)];
/// let stack = vec![CoreDumpValue::I32(2)];
/// thread.frame(func_index, code_offset, locals, stack);
///
/// let mut module = Module::new();
/// module.section(&thread);
/// ```
#[derive(Clone, Debug, Default)]
pub struct CoreDumpStackSection {
    frame_bytes: Vec<u8>,
    count: u32,
    name: String,
}

impl CoreDumpStackSection {
    /// Create a new core dump stack section encoder.
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        CoreDumpStackSection {
            frame_bytes: Vec::new(),
            count: 0,
            name,
        }
    }

    /// Add a stack frame to this coredump stack section.
    pub fn frame<L, S>(&mut self, funcidx: u32, codeoffset: u32, locals: L, stack: S) -> &mut Self
    where
        L: IntoIterator<Item = CoreDumpValue>,
        <L as IntoIterator>::IntoIter: ExactSizeIterator,
        S: IntoIterator<Item = CoreDumpValue>,
        <S as IntoIterator>::IntoIter: ExactSizeIterator,
    {
        self.count += 1;
        self.frame_bytes.push(0);
        funcidx.encode(&mut self.frame_bytes);
        codeoffset.encode(&mut self.frame_bytes);
        crate::encode_vec(locals, &mut self.frame_bytes);
        crate::encode_vec(stack, &mut self.frame_bytes);
        self
    }
}

impl Encode for CoreDumpStackSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        let mut data = vec![0];
        self.name.encode(&mut data);
        self.count.encode(&mut data);
        data.extend(&self.frame_bytes);
        CustomSection {
            name: "corestack".into(),
            data: Cow::Owned(data),
        }
        .encode(sink);
    }
}

impl Section for CoreDumpStackSection {
    fn id(&self) -> u8 {
        crate::core::SectionId::Custom as u8
    }
}

/// Local and stack values are encoded using one byte for the type (similar to
/// Wasm's Number Types) followed by bytes representing the actual value
/// See the tool-conventions repo for more details.
#[derive(Clone, Debug)]
pub enum CoreDumpValue {
    /// a missing value (usually missing because it was optimized out)
    Missing,
    /// An i32 value
    I32(i32),
    /// An i64 value
    I64(i64),
    /// An f32 value
    F32(f32),
    /// An f64 value
    F64(f64),
}

impl Encode for CoreDumpValue {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            CoreDumpValue::Missing => sink.push(0x01),
            CoreDumpValue::I32(x) => {
                sink.push(0x7F);
                x.encode(sink);
            }
            CoreDumpValue::I64(x) => {
                sink.push(0x7E);
                x.encode(sink);
            }
            CoreDumpValue::F32(x) => {
                sink.push(0x7D);
                x.encode(sink);
            }
            CoreDumpValue::F64(x) => {
                sink.push(0x7C);
                x.encode(sink);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_coredump_section() {
        let core = CoreDumpSection::new("test");

        let mut encoded = vec![];
        core.encode(&mut encoded);

        #[rustfmt::skip]
      assert_eq!(encoded, vec![
          // section length
          11,
          // name length
          4,
          // section name (core)
          b'c',b'o',b'r',b'e',
          // process-info (0, data length, data)
          0, 4, b't', b'e', b's', b't',
      ]);
    }

    #[test]
    fn test_encode_corestack_section() {
        let mut thread = CoreDumpStackSection::new("main");
        thread.frame(
            42,
            51,
            vec![CoreDumpValue::I32(1)],
            vec![CoreDumpValue::I32(2)],
        );

        let mut encoded = vec![];
        thread.encode(&mut encoded);

        #[rustfmt::skip]
        assert_eq!(
            encoded,
            vec![
                // section length
                26, 
                // length of name.
                9,
                // section name (corestack)
                b'c',b'o',b'r',b'e',b's',b't',b'a',b'c',b'k',
                // 0x0, thread name length
                0, 4,
                // thread name (main)
                b'm',b'a',b'i',b'n',
                // frame count
                1,
                // 0x0, funcidx, codeoffset
                0, 42, 51,
                // local count
                1,
                // local value type
                0x7F,
                // local value
                1,
                // stack count
                1,
                // stack value type
                0x7F,
                // stack value
                2

            ]
        );
    }
}
