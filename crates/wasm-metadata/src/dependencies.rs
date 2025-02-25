use std::fmt::{self, Display};
use std::str::FromStr;

use anyhow::{ensure, Error, Result};
use auditable_serde::VersionInfo;
// use auditable_serde::Auditable;
use flate2::read::GzDecoder;
// use miniz_oxide::inflate::decompress_to_vec_zlib;
use serde::Serialize;
use wasm_encoder::{ComponentSection, CustomSection, Encode, Section};
use wasmparser::CustomSectionReader;

/// Human-readable description of the binary
#[derive(Debug, Clone, PartialEq)]
pub struct Dependencies(VersionInfo);

impl Dependencies {
    /// Parse an `description` custom section from a wasm binary.
    pub(crate) fn parse_custom_section(reader: &CustomSectionReader<'_>) -> Result<Self> {
        ensure!(
            reader.name() == ".dep-v0",
            "The `dependencies` custom section should have a name of '.dep-v0'"
        );
        // let decompressed_data = decompress_to_vec_zlib(reader.data())?;
        let decompressed_data = GzDecoder::new(reader.data());
        let decompressed_data = std::io::read_to_string(decompressed_data)?;
        let dependency_tree = auditable_serde::VersionInfo::from_str(&decompressed_data)?;

        Ok(Self::new(dependency_tree))
    }

    fn new(dependency_tree: auditable_serde::VersionInfo) -> Self {
        Self(dependency_tree)
    }
}

// impl FromStr for Dependencies {
//     type Err = Error;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         Ok(Self::new(s.to_owned()))
//     }
// }

impl Serialize for Dependencies {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Display for Dependencies {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE: this will never panic since we always guarantee the data is
        // encoded as utf8, even if we internally store it as [u8].
        // let data = String::from_utf8(self.0.data.to_vec()).unwrap();
        write!(f, "")
    }
}

impl ComponentSection for Dependencies {
    fn id(&self) -> u8 {
        ComponentSection::id(&self.0)
    }
}

impl Section for Dependencies {
    fn id(&self) -> u8 {
        Section::id(&self.0)
    }
}

impl Encode for Dependencies {
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
        component.section(&Dependencies::new("Nori likes chicken"));
        let component = component.finish();

        let mut parsed = false;
        for section in wasmparser::Parser::new(0).parse_all(&component) {
            if let Payload::CustomSection(reader) = section.unwrap() {
                let description = Dependencies::parse_custom_section(&reader).unwrap();
                assert_eq!(description.to_string(), "Nori likes chicken");
                parsed = true;
            }
        }
        assert!(parsed);
    }

    #[test]
    fn serialize() {
        let description = Dependencies::new("Chashu likes tuna");
        let json = serde_json::to_string(&description).unwrap();
        assert_eq!(r#""Chashu likes tuna""#, json);
    }
}
