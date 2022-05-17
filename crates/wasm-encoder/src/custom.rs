use crate::{encoders, ComponentSection, Encode, Section, SectionId};

/// A custom section holding arbitrary data.
#[derive(Clone, Debug)]
pub struct CustomSection<'a> {
    /// The name of this custom section.
    pub name: &'a str,
    /// This custom section's data.
    pub data: &'a [u8],
}

impl Encode for CustomSection<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        let name_len = encoders::u32(u32::try_from(self.name.len()).unwrap());

        // Note: the custom section id is the same for both modules and components
        sink.push(SectionId::Custom.into());
        sink.extend(encoders::u32(
            u32::try_from(name_len.len() + self.name.len() + self.data.len()).unwrap(),
        ));
        sink.extend(name_len);
        sink.extend(self.name.as_bytes());
        sink.extend(self.data);
    }
}

impl Section for CustomSection<'_> {}
impl ComponentSection for CustomSection<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_custom_section() {
        let custom = CustomSection {
            name: "test",
            data: &[11, 22, 33, 44],
        };

        let mut encoded = vec![];
        custom.encode(&mut encoded);

        #[rustfmt::skip]
        assert_eq!(encoded, vec![
            // Section ID
            0,
            // LEB128 length of section.
            9,
            // LEB128 length of name.
            4,
            // Name.
            b't', b'e', b's', b't',
            // Data.
            11, 22, 33, 44,
        ]);
    }
}
