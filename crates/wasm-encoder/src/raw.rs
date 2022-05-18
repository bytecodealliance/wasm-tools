use crate::{ComponentSection, Encode, Section};

/// A section made up of uninterpreted, raw bytes.
///
/// Allows you to splat any data into module or component.
#[derive(Clone, Copy, Debug)]
pub struct RawSection<'a> {
    /// The id for this section.
    pub id: u8,
    /// The raw data for this section.
    pub data: &'a [u8],
}

impl Encode for RawSection<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(self.id);
        self.data.encode(sink);
    }
}

impl Section for RawSection<'_> {}
impl ComponentSection for RawSection<'_> {}
