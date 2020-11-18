use super::*;

/// A custom section holding arbitrary data.
pub struct CustomSection<'a> {
    /// The name of this custom section.
    pub name: &'a str,
    /// This custom section's data.
    pub data: &'a [u8],
}

impl Section for CustomSection<'_> {
    fn id(&self) -> u8 {
        SectionId::Custom as u8
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let name_len = encoders::u32(u32::try_from(self.name.len()).unwrap());
        let n = name_len.len();

        let data_len = encoders::u32(u32::try_from(self.data.len()).unwrap());
        let m = data_len.len();

        sink.extend(
            encoders::u32(u32::try_from(n + self.name.len() + m + self.data.len()).unwrap())
                .chain(name_len)
                .chain(self.name.as_bytes().iter().copied())
                .chain(data_len)
                .chain(self.data.iter().copied()),
        );
    }
}
