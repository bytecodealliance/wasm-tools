use std::borrow::Cow;
use std::fmt::{self, Display};

use anyhow::{ensure, Result};
use wasm_encoder::{ComponentSection, CustomSection, Encode};
use wasmparser::CustomSectionReader;

/// Contact details of the people or organization responsible for the image
/// encoded as a freeform string.
#[derive(Debug)]
pub struct Author(CustomSection<'static>);

impl Display for Author {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE: this will never panic since we always guarantee the data is
        // encoded as utf8, even if we internally store it as [u8].
        let data = String::from_utf8(self.0.data.to_vec()).unwrap();
        write!(f, "{data}")
    }
}

impl Author {
    /// Create a new instance of `Author`.
    pub fn new<S: Into<Cow<'static, str>>>(s: S) -> Self {
        Self(CustomSection {
            name: "author".into(),
            data: match s.into() {
                Cow::Borrowed(s) => Cow::Borrowed(s.as_bytes()),
                Cow::Owned(s) => Cow::Owned(s.into()),
            },
        })
    }

    /// Parse an `author` custom section from a wasm binary.
    pub fn parse_wasm(reader: CustomSectionReader<'_>) -> Result<Self> {
        ensure!(
            dbg!(reader.name()) == "author",
            "The `author` custom section should have a name of 'author'"
        );
        let data = String::from_utf8(reader.data().to_owned())?;
        Ok(Self::new(data))
    }
}

impl ComponentSection for Author {
    fn id(&self) -> u8 {
        self.0.id()
    }
}

impl Encode for Author {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.0.encode(sink);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use wasm_encoder::Component;
    use wasmparser::Payload;

    #[test]
    fn roundtrip() {
        let mut component = Component::new();
        component.section(&Author::new("Nori Cat"));
        let component = component.finish();

        let mut parsed = false;
        for section in wasmparser::Parser::new(0).parse_all(&component) {
            if let Payload::CustomSection(reader) = section.unwrap() {
                let author = Author::parse_wasm(reader).unwrap();
                assert_eq!(author.to_string(), "Nori Cat");
                parsed = true;
            }
        }
        assert!(parsed);
    }
}
