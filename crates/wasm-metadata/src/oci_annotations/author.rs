use std::borrow::Cow;
use std::fmt::{self, Display};
use std::str::FromStr;

use anyhow::{ensure, Error, Result};
use serde::Serialize;
use wasm_encoder::{ComponentSection, CustomSection, Encode, Section};
use wasmparser::CustomSectionReader;

/// Contact details of the people or organization responsible,
/// encoded as a freeform string.
#[derive(Debug, Clone, PartialEq)]
pub struct Author(CustomSection<'static>);

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
    pub(crate) fn parse_custom_section(reader: &CustomSectionReader<'_>) -> Result<Self> {
        ensure!(
            reader.name() == "author",
            "The `author` custom section should have a name of 'author'"
        );
        let data = String::from_utf8(reader.data().to_owned())?;
        Ok(Self::new(data))
    }
}

impl FromStr for Author {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(s.to_owned()))
    }
}

impl Serialize for Author {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Display for Author {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE: this will never panic since we always guarantee the data is
        // encoded as utf8, even if we internally store it as [u8].
        let data = String::from_utf8(self.0.data.to_vec()).unwrap();
        write!(f, "{data}")
    }
}

impl ComponentSection for Author {
    fn id(&self) -> u8 {
        ComponentSection::id(&self.0)
    }
}

impl Section for Author {
    fn id(&self) -> u8 {
        Section::id(&self.0)
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
                let author = Author::parse_custom_section(&reader).unwrap();
                assert_eq!(author.to_string(), "Nori Cat");
                parsed = true;
            }
        }
        assert!(parsed);
    }

    #[test]
    fn serialize() {
        let author = Author::new("Chashu Cat");
        let json = serde_json::to_string(&author).unwrap();
        assert_eq!(r#""Chashu Cat""#, json);
    }
}
