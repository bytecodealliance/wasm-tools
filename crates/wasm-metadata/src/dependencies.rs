use std::fmt::{self, Display};
use std::io::Read;
use std::str::FromStr;

use anyhow::{Result, ensure};
use auditable_serde::VersionInfo;
use flate2::Compression;
use flate2::read::{ZlibDecoder, ZlibEncoder};
use serde::Serialize;
use wasm_encoder::{ComponentSection, CustomSection, Encode, Section};
use wasmparser::CustomSectionReader;

/// This section is currently zlib-compressed so have a hard limit to avoid
/// decompressing undue amounts of data from a wasm module.
const MAX_DECOMPRESSED_SIZE: u64 = 2 * 1024 * 1024;

/// Human-readable description of the binary
#[derive(Debug, Clone, PartialEq)]
pub struct Dependencies {
    version_info: VersionInfo,
    custom_section: CustomSection<'static>,
}

impl Dependencies {
    /// Parse an `description` custom section from a wasm binary.
    pub(crate) fn parse_custom_section(reader: &CustomSectionReader<'_>) -> Result<Self> {
        ensure!(
            reader.name() == ".dep-v0",
            "The `dependencies` custom section should have a name of '.dep-v0'"
        );
        let mut decompressed_data = Vec::new();
        ZlibDecoder::new(reader.data())
            .take(MAX_DECOMPRESSED_SIZE + 1)
            .read_to_end(&mut decompressed_data)?;
        ensure!(
            decompressed_data.len() as u64 <= MAX_DECOMPRESSED_SIZE,
            "`.dep-v0` custom section decompresses to more than the {MAX_DECOMPRESSED_SIZE} byte limit"
        );
        let decompressed_data = String::from_utf8(decompressed_data)?;
        let dependency_tree = auditable_serde::VersionInfo::from_str(&decompressed_data)?;

        Ok(Self {
            version_info: dependency_tree,
            custom_section: CustomSection {
                name: ".dep-v0".into(),
                data: reader.data().to_owned().into(),
            },
        })
    }

    /// Create a new instance of `Dependencies`.
    pub fn new(dependency_tree: auditable_serde::VersionInfo) -> Result<Self> {
        let data = serde_json::to_string(&dependency_tree)?;
        ensure!(
            data.len() as u64 <= MAX_DECOMPRESSED_SIZE,
            "`.dep-v0` custom section would decompress to more than the {MAX_DECOMPRESSED_SIZE} byte limit"
        );

        let mut ret_vec = Vec::new();
        let mut encoder = ZlibEncoder::new(data.as_bytes(), Compression::fast());
        encoder.read_to_end(&mut ret_vec)?;

        Ok(Self {
            version_info: dependency_tree,
            custom_section: CustomSection {
                name: ".dep-v0".into(),
                data: ret_vec.into(),
            },
        })
    }

    /// Provides access to the version information stored in the object
    pub fn version_info(&self) -> &VersionInfo {
        &self.version_info
    }
}

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
        let data = serde_json::to_string(&self.version_info).unwrap();
        write!(f, "{data}")
    }
}

impl ComponentSection for Dependencies {
    fn id(&self) -> u8 {
        ComponentSection::id(&self.custom_section)
    }
}

impl Section for Dependencies {
    fn id(&self) -> u8 {
        Section::id(&self.custom_section)
    }
}

impl Encode for Dependencies {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.custom_section.encode(sink);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use auditable_serde::{Source, VersionInfo};
    use std::str::FromStr;
    use wasm_encoder::Component;
    use wasmparser::Payload;

    #[test]
    fn roundtrip() {
        let json_str = r#"{"packages":[{"name":"adler","version":"0.2.3","source":"registry"}]}"#;
        let info = VersionInfo::from_str(json_str).unwrap();
        assert_eq!(&info.packages[0].name, "adler");
        let mut component = Component::new();
        component.section(&Dependencies::new(info).unwrap());
        let component = component.finish();

        let mut parsed = false;
        for section in wasmparser::Parser::new(0).parse_all(&component) {
            if let Payload::CustomSection(reader) = section.unwrap() {
                let dependencies = Dependencies::parse_custom_section(&reader).unwrap();
                assert_eq!(dependencies.to_string(), json_str);
                parsed = true;
            }
        }
        assert!(parsed);
    }

    #[test]
    fn rejects_decompression_too_big() {
        let plain = vec![b' '; (MAX_DECOMPRESSED_SIZE * 2) as usize];
        let mut compressed = Vec::new();
        ZlibEncoder::new(&plain[..], Compression::best())
            .read_to_end(&mut compressed)
            .unwrap();
        assert!(
            compressed.len() < plain.len(),
            "the section body should be much smaller than its decompressed form"
        );

        let mut component = Component::new();
        component.section(&wasm_encoder::CustomSection {
            name: ".dep-v0".into(),
            data: compressed.into(),
        });
        let component = component.finish();

        let mut saw_section = false;
        for section in wasmparser::Parser::new(0).parse_all(&component) {
            if let Payload::CustomSection(reader) = section.unwrap() {
                saw_section = true;
                let err = Dependencies::parse_custom_section(&reader).unwrap_err();
                assert!(
                    err.to_string().contains("byte limit"),
                    "unexpected error: {err}"
                );
            }
        }
        assert!(saw_section);
    }

    #[test]
    fn serialize() {
        let json_str = r#"{"packages":[{"name":"adler","version":"0.2.3","source":"registry"}]}"#;
        let info = VersionInfo::from_str(json_str).unwrap();
        let dependencies = Dependencies::new(info).unwrap();
        assert_eq!(dependencies.version_info().packages[0].name, "adler");
        assert_eq!(
            dependencies.version_info().packages[0].version.to_string(),
            "0.2.3"
        );
        assert_eq!(
            dependencies.version_info().packages[0].source,
            Source::Registry,
        );
    }
}
