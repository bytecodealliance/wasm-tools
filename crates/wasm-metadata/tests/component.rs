use std::vec;

use wasm_encoder::{Component, Module};
use wasm_metadata::*;

#[test]
fn add_to_empty_component() {
    let component = Component::new().finish();
    let add = AddMetadata {
        name: Some("foo".to_owned()),
        language: vec![("bar".to_owned(), "1.0".to_owned())],
        processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
        sdk: vec![],
        author: Some(Author::new("Chashu Cat")),
        description: Some(Description::new("Chashu likes tuna")),
        licenses: Some(
            Licenses::new("Apache-2.0 WITH LLVM-exception OR Apache-2.0 OR MIT").unwrap(),
        ),
        source: Some(Source::new("https://github.com/bytecodealliance/wasm-tools").unwrap()),
        homepage: Some(Homepage::new("https://github.com/bytecodealliance/wasm-tools").unwrap()),
        revision: Some(Revision::new("de978e17a80c1118f606fce919ba9b7d5a04a5ad")),
        version: Some(Version::new("1.0.0")),
    };
    let component = add.to_wasm(&component).unwrap();

    match Payload::from_binary(&component).unwrap() {
        Payload::Component {
            children,
            metadata:
                Metadata {
                    name,
                    producers,
                    author,
                    description,
                    licenses,
                    source,
                    range,
                    homepage,
                    revision,
                    version,
                },
        } => {
            assert!(children.is_empty());
            assert_eq!(name, Some("foo".to_owned()));
            let producers = producers.expect("some producers");
            assert_eq!(
                producers.get("language").unwrap().get("bar").unwrap(),
                "1.0"
            );
            assert_eq!(
                producers.get("processed-by").unwrap().get("baz").unwrap(),
                "1.0"
            );

            assert_eq!(author.unwrap(), Author::new("Chashu Cat"));
            assert_eq!(description.unwrap(), Description::new("Chashu likes tuna"));
            assert_eq!(
                licenses.unwrap(),
                Licenses::new("Apache-2.0 WITH LLVM-exception OR Apache-2.0 OR MIT").unwrap()
            );
            assert_eq!(
                source.unwrap(),
                Source::new("https://github.com/bytecodealliance/wasm-tools").unwrap(),
            );
            assert_eq!(
                homepage.unwrap(),
                Homepage::new("https://github.com/bytecodealliance/wasm-tools").unwrap(),
            );
            assert_eq!(
                revision.unwrap(),
                Revision::new("de978e17a80c1118f606fce919ba9b7d5a04a5ad")
            );
            assert_eq!(version.unwrap(), Version::new("1.0.0"));

            assert_eq!(range.start, 0);
            assert_eq!(range.end, 374);
        }
        _ => panic!("metadata should be component"),
    }
}

#[test]
fn add_to_nested_component() {
    // Create the same old module, stick some metadata into it
    let module = Module::new().finish();
    let add = AddMetadata {
        name: Some("foo".to_owned()),
        language: vec![("bar".to_owned(), "1.0".to_owned())],
        processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
        sdk: vec![],
        author: Some(Author::new("Chashu Cat")),
        description: Some(Description::new("Chashu likes tuna")),
        licenses: Some(
            Licenses::new("Apache-2.0 WITH LLVM-exception OR Apache-2.0 OR MIT").unwrap(),
        ),
        source: Some(Source::new("https://github.com/bytecodealliance/wasm-tools").unwrap()),
        homepage: Some(Homepage::new("https://github.com/bytecodealliance/wasm-tools").unwrap()),
        revision: Some(Revision::new("de978e17a80c1118f606fce919ba9b7d5a04a5ad")),
        version: Some(Version::new("1.0.0")),
    };
    let module = add.to_wasm(&module).unwrap();

    // Stick that module inside a component.
    let mut component = wasm_encoder::Component::new();
    component.section(&wasm_encoder::RawSection {
        id: wasm_encoder::ComponentSectionId::CoreModule.into(),
        data: &module,
    });
    let component = component.finish();

    // Add some different metadata to the component.
    let add = AddMetadata {
        name: Some("gussie".to_owned()),
        sdk: vec![("willa".to_owned(), "sparky".to_owned())],
        ..Default::default()
    };
    let component = add.to_wasm(&component).unwrap();

    match Payload::from_binary(&component).unwrap() {
        Payload::Component {
            children,
            metadata: Metadata {
                name, producers, ..
            },
        } => {
            // Check that the component metadata is in the component
            assert_eq!(name, Some("gussie".to_owned()));
            let producers = producers.as_ref().expect("some producers");
            assert_eq!(
                producers.get("sdk").unwrap().get("willa").unwrap(),
                &"sparky".to_owned()
            );
            // Check that there is a single child with the metadata set for the module
            assert_eq!(children.len(), 1);

            match children.get(0).unwrap() {
                Payload::Module(Metadata {
                    name,
                    producers,
                    author,
                    licenses,
                    source,
                    range,
                    description,
                    homepage,
                    revision,
                    version,
                }) => {
                    assert_eq!(name, &Some("foo".to_owned()));
                    let producers = producers.as_ref().expect("some producers");
                    assert_eq!(
                        producers.get("language").unwrap().get("bar").unwrap(),
                        "1.0"
                    );
                    assert_eq!(
                        producers.get("processed-by").unwrap().get("baz").unwrap(),
                        "1.0"
                    );

                    assert_eq!(author, &Some(Author::new("Chashu Cat")));
                    assert_eq!(description, &Some(Description::new("Chashu likes tuna")));
                    assert_eq!(
                        licenses,
                        &Some(
                            Licenses::new("Apache-2.0 WITH LLVM-exception OR Apache-2.0 OR MIT")
                                .unwrap()
                        )
                    );
                    assert_eq!(
                        source,
                        &Some(
                            Source::new("https://github.com/bytecodealliance/wasm-tools").unwrap()
                        ),
                    );
                    assert_eq!(
                        homepage,
                        &Some(
                            Homepage::new("https://github.com/bytecodealliance/wasm-tools")
                                .unwrap()
                        ),
                    );
                    assert_eq!(
                        revision,
                        &Some(Revision::new("de978e17a80c1118f606fce919ba9b7d5a04a5ad"))
                    );
                    assert_eq!(version, &Some(Version::new("1.0.0")));

                    assert_eq!(range.start, 11);
                    assert_eq!(range.end, 375);
                }
                _ => panic!("child is a module"),
            }
        }
        _ => panic!("root should be component"),
    }
}
