use crate::{encoders, ComponentSection, Section};

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

impl Section for RawSection<'_> {
    fn id(&self) -> u8 {
        self.id
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        sink.extend(
            encoders::u32(u32::try_from(self.data.len()).unwrap()).chain(self.data.iter().copied()),
        );
    }
}

impl ComponentSection for RawSection<'_> {
    fn id(&self) -> u8 {
        self.id
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        sink.extend(
            encoders::u32(u32::try_from(self.data.len()).unwrap()).chain(self.data.iter().copied()),
        );
    }
}
