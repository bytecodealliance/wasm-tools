use super::{ComponentSection, SectionId};
use crate::encoders;

/// An encoder for the component custom section.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::component::{Component, CustomSection};
///
/// let mut component = Component::new();
/// component.section(&CustomSection {
///    name: "my_custom_section",
///    data: &[0x01, 0x02, 0x03]
/// });
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct CustomSection<'a> {
    /// The name of the custom section.
    pub name: &'a str,
    /// The custom section's data.
    pub data: &'a [u8],
}

impl ComponentSection for CustomSection<'_> {
    fn id(&self) -> u8 {
        SectionId::Custom.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let name_len = encoders::u32(u32::try_from(self.name.len()).unwrap());
        let n = name_len.len();

        sink.extend(
            encoders::u32(u32::try_from(n + self.name.len() + self.data.len()).unwrap())
                .chain(name_len)
                .chain(self.name.as_bytes().iter().copied())
                .chain(self.data.iter().copied()),
        );
    }
}
