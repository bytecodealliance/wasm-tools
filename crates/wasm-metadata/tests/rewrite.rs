#![cfg(feature = "oci")]

use wasm_encoder::{Component, ComponentSectionId, Module, RawSection};
use wasm_metadata::*;
use wasmparser::{Parser, Payload};

fn metadata(value: &str) -> AddMetadata {
    let mut add = AddMetadata::default();
    add.authors = AddMetadataField::Set(Authors::new(value.to_owned()));
    add.description = AddMetadataField::Set(Description::new(value.to_owned()));
    add.licenses = AddMetadataField::Set(Licenses::new(value).unwrap());
    add.source =
        AddMetadataField::Set(Source::new(&format!("https://example.com/{value}")).unwrap());
    add.homepage =
        AddMetadataField::Set(Homepage::new(&format!("https://example.com/{value}")).unwrap());
    add.revision = AddMetadataField::Set(Revision::new(value.to_owned()));
    add.version = AddMetadataField::Set(Version::new(value.to_owned()));
    add
}

fn custom_sections(wasm: &[u8]) -> Vec<(String, Vec<u8>)> {
    Parser::new(0)
        .parse_all(wasm)
        .filter_map(|payload| match payload.unwrap() {
            Payload::CustomSection(section) => {
                Some((section.name().to_owned(), section.data().to_vec()))
            }
            _ => None,
        })
        .collect()
}

#[test]
fn replace_existing_oci_metadata() {
    for empty in [Module::new().finish(), Component::new().finish()] {
        let old = metadata("MIT").to_wasm(&empty).unwrap();
        let add = metadata("Apache-2.0");
        let expected = custom_sections(&add.to_wasm(&empty).unwrap());
        assert_eq!(expected.len(), 7);
        let replaced = add.to_wasm(&old).unwrap();
        assert_eq!(custom_sections(&replaced), expected);
        assert_eq!(custom_sections(&add.to_wasm(&replaced).unwrap()), expected);
        assert_eq!(AddMetadata::default().to_wasm(&old).unwrap(), old);

        let mut clear = AddMetadata::default();
        clear.authors = AddMetadataField::Clear;
        clear.description = AddMetadataField::Clear;
        clear.licenses = AddMetadataField::Clear;
        clear.source = AddMetadataField::Clear;
        clear.homepage = AddMetadataField::Clear;
        clear.revision = AddMetadataField::Clear;
        clear.version = AddMetadataField::Clear;
        assert_eq!(clear.to_wasm(&old).unwrap(), empty);
    }
}

#[test]
fn replace_oci_metadata_preserves_nested_sections() {
    let module = metadata("MIT").to_wasm(&Module::new().finish()).unwrap();
    let mut component = Component::new();
    component.section(&RawSection {
        id: ComponentSectionId::CoreModule as u8,
        data: &module,
    });
    let input = metadata("MIT").to_wasm(&component.finish()).unwrap();
    let output = metadata("Apache-2.0").to_wasm(&input).unwrap();
    let sections = custom_sections(&output);
    assert_eq!(&sections[..7], custom_sections(&module));
    assert_eq!(sections.len(), 14);
}
