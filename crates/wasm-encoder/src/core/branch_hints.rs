use crate::{CustomSection, Encode, Section, SectionId};
use std::borrow::Cow;

/// Helper structure to encode the `metadata.code.branch_hint` custom section.
///
/// This section was defined in the branch-hinting proposal for WebAssembly:
/// <https://github.com/WebAssembly/branch-hinting>.
#[derive(Default, Debug)]
pub struct BranchHints {
    bytes: Vec<u8>,
    num_hints: u32,
}

/// A single branch hint within a function.
#[derive(Debug, Clone, Copy)]
pub struct BranchHint {
    /// The offset, in bytes from the beginning of the function, to the `if`
    /// instruction that this is hinting.
    pub branch_func_offset: u32,
    /// The value of the hint, 0 for not taken and 1 for taken.
    pub branch_hint_value: u32,
}

impl BranchHints {
    /// Construct an empty encoder for the branch hints custom section.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a new set of function hints for the `func` specified.
    pub fn function_hints(&mut self, func: u32, hints: impl ExactSizeIterator<Item = BranchHint>) {
        self.num_hints += 1;
        func.encode(&mut self.bytes);
        hints.len().encode(&mut self.bytes);
        for hint in hints {
            hint.branch_func_offset.encode(&mut self.bytes);
            1u32.encode(&mut self.bytes);
            hint.branch_hint_value.encode(&mut self.bytes);
        }
    }

    /// Returns if this is an empty section.
    pub fn is_empty(&self) -> bool {
        self.num_hints == 0
    }

    /// Returns the number of functions that have hints registered in this
    /// sectino.
    pub fn len(&self) -> u32 {
        self.num_hints
    }
}

impl Encode for BranchHints {
    fn encode(&self, sink: &mut Vec<u8>) {
        let mut data = Vec::new();
        self.num_hints.encode(&mut data);
        data.extend(&self.bytes);

        CustomSection {
            name: "metadata.code.branch_hint".into(),
            data: Cow::Borrowed(&data),
        }
        .encode(sink);
    }
}

impl Section for BranchHints {
    fn id(&self) -> u8 {
        SectionId::Custom.into()
    }
}
