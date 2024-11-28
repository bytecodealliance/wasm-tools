use std::vec;

use wasm_metadata::*;

#[test]
fn add_to_empty_module() {
    let wat = "(module)";
    let module = wat::parse_str(wat).unwrap();
    let add = AddMetadata {
        name: Some("foo".to_owned()),
        language: vec!["bar".to_owned()],
        processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
        sdk: vec![],
        registry_metadata: Some(RegistryMetadata {
            authors: Some(vec!["foo".to_owned()]),
            description: Some("foo bar baz".to_owned()),
            license: Some("MIT OR LicenseRef-FOO".to_owned()),
            custom_licenses: Some(vec![CustomLicense {
                id: "FOO".to_owned(),
                name: "Foo".to_owned(),
                text: "Foo License".to_owned(),
                reference: Some("https://exaple.com/license/foo".to_owned()),
            }]),
            links: Some(vec![
                Link {
                    ty: LinkType::Custom("CustomFoo".to_owned()),
                    value: "https://example.com/custom".to_owned(),
                },
                Link {
                    ty: LinkType::Homepage,
                    value: "https://example.com".to_owned(),
                },
            ]),
            categories: Some(vec!["Tools".to_owned()]),
        }),
    };
    let module = add.to_wasm(&module).unwrap();

    let metadata = Metadata::from_binary(&module).unwrap();
    match metadata {
        Metadata::Module {
            name,
            producers,
            registry_metadata,
            range,
        } => {
            assert_eq!(name, Some("foo".to_owned()));
            let producers = producers.expect("some producers");
            assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
            assert_eq!(
                producers.get("processed-by").unwrap().get("baz").unwrap(),
                "1.0"
            );

            let registry_metadata = registry_metadata.unwrap();

            assert!(registry_metadata.validate().is_ok());

            assert_eq!(registry_metadata.authors.unwrap(), vec!["foo".to_owned()]);
            assert_eq!(
                registry_metadata.description.unwrap(),
                "foo bar baz".to_owned()
            );

            assert_eq!(
                registry_metadata.license.unwrap(),
                "MIT OR LicenseRef-FOO".to_owned()
            );
            assert_eq!(
                registry_metadata.custom_licenses.unwrap(),
                vec![CustomLicense {
                    id: "FOO".to_owned(),
                    name: "Foo".to_owned(),
                    text: "Foo License".to_owned(),
                    reference: Some("https://exaple.com/license/foo".to_owned()),
                }]
            );
            assert_eq!(
                registry_metadata.links.unwrap(),
                vec![
                    Link {
                        ty: LinkType::Custom("CustomFoo".to_owned()),
                        value: "https://example.com/custom".to_owned(),
                    },
                    Link {
                        ty: LinkType::Homepage,
                        value: "https://example.com".to_owned(),
                    },
                ]
            );
            assert_eq!(
                registry_metadata.categories.unwrap(),
                vec!["Tools".to_owned()]
            );

            assert_eq!(range.start, 0);
            assert_eq!(range.end, 422);
        }
        _ => panic!("metadata should be module"),
    }
}

#[test]
fn add_to_empty_component() {
    let wat = "(component)";
    let component = wat::parse_str(wat).unwrap();
    let add = AddMetadata {
        name: Some("foo".to_owned()),
        language: vec!["bar".to_owned()],
        processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
        sdk: vec![],
        registry_metadata: Some(RegistryMetadata {
            authors: Some(vec!["foo".to_owned()]),
            description: Some("foo bar baz".to_owned()),
            license: Some("MIT OR LicenseRef-FOO".to_owned()),
            custom_licenses: Some(vec![CustomLicense {
                id: "FOO".to_owned(),
                name: "Foo".to_owned(),
                text: "Foo License".to_owned(),
                reference: Some("https://exaple.com/license/foo".to_owned()),
            }]),
            links: Some(vec![
                Link {
                    ty: LinkType::Custom("CustomFoo".to_owned()),
                    value: "https://example.com/custom".to_owned(),
                },
                Link {
                    ty: LinkType::Homepage,
                    value: "https://example.com".to_owned(),
                },
            ]),
            categories: Some(vec!["Tools".to_owned()]),
        }),
    };
    let component = add.to_wasm(&component).unwrap();

    let metadata = Metadata::from_binary(&component).unwrap();
    match metadata {
        Metadata::Component {
            name,
            producers,
            registry_metadata,
            children,
            range,
        } => {
            assert!(children.is_empty());
            assert_eq!(name, Some("foo".to_owned()));
            let producers = producers.expect("some producers");
            assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
            assert_eq!(
                producers.get("processed-by").unwrap().get("baz").unwrap(),
                "1.0"
            );

            let registry_metadata = registry_metadata.unwrap();

            assert!(registry_metadata.validate().is_ok());

            assert_eq!(registry_metadata.authors.unwrap(), vec!["foo".to_owned()]);
            assert_eq!(
                registry_metadata.description.unwrap(),
                "foo bar baz".to_owned()
            );

            assert_eq!(
                registry_metadata.license.unwrap(),
                "MIT OR LicenseRef-FOO".to_owned()
            );
            assert_eq!(
                registry_metadata.custom_licenses.unwrap(),
                vec![CustomLicense {
                    id: "FOO".to_owned(),
                    name: "Foo".to_owned(),
                    text: "Foo License".to_owned(),
                    reference: Some("https://exaple.com/license/foo".to_owned()),
                }]
            );
            assert_eq!(
                registry_metadata.links.unwrap(),
                vec![
                    Link {
                        ty: LinkType::Custom("CustomFoo".to_owned()),
                        value: "https://example.com/custom".to_owned(),
                    },
                    Link {
                        ty: LinkType::Homepage,
                        value: "https://example.com".to_owned(),
                    },
                ]
            );
            assert_eq!(
                registry_metadata.categories.unwrap(),
                vec!["Tools".to_owned()]
            );

            assert_eq!(range.start, 0);
            assert_eq!(range.end, 432);
        }
        _ => panic!("metadata should be component"),
    }
}

#[test]
fn add_to_nested_component() {
    // Create the same old module, stick some metadata into it
    let wat = "(module)";
    let module = wat::parse_str(wat).unwrap();
    let add = AddMetadata {
        name: Some("foo".to_owned()),
        language: vec!["bar".to_owned()],
        processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
        sdk: vec![],
        registry_metadata: Some(RegistryMetadata {
            authors: Some(vec!["Foo".to_owned()]),
            ..Default::default()
        }),
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

    let metadata = Metadata::from_binary(&component).unwrap();
    match metadata {
        Metadata::Component {
            name,
            producers,
            children,
            ..
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
            let child = children.get(0).unwrap();
            match &**child {
                Metadata::Module {
                    name,
                    producers,
                    registry_metadata,
                    range,
                } => {
                    assert_eq!(name, &Some("foo".to_owned()));
                    let producers = producers.as_ref().expect("some producers");
                    assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
                    assert_eq!(
                        producers.get("processed-by").unwrap().get("baz").unwrap(),
                        "1.0"
                    );

                    let registry_metadata = registry_metadata.as_ref().unwrap();
                    assert_eq!(
                        registry_metadata.authors.as_ref().unwrap(),
                        &["Foo".to_owned()]
                    );

                    assert_eq!(range.start, 10);
                    assert_eq!(range.end, 120);
                }
                _ => panic!("child is a module"),
            }
        }
        _ => panic!("root should be component"),
    }
}

#[test]
fn producers_empty_module() {
    let wat = "(module)";
    let module = wat::parse_str(wat).unwrap();
    let mut producers = Producers::empty();
    producers.add("language", "bar", "");
    producers.add("processed-by", "baz", "1.0");

    let module = producers.add_to_wasm(&module).unwrap();

    let metadata = Metadata::from_binary(&module).unwrap();
    match metadata {
        Metadata::Module {
            name, producers, ..
        } => {
            assert_eq!(name, None);
            let producers = producers.expect("some producers");
            assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
            assert_eq!(
                producers.get("processed-by").unwrap().get("baz").unwrap(),
                "1.0"
            );
        }
        _ => panic!("metadata should be module"),
    }
}

#[test]
fn producers_add_another_field() {
    let wat = "(module)";
    let module = wat::parse_str(wat).unwrap();
    let mut producers = Producers::empty();
    producers.add("language", "bar", "");
    producers.add("processed-by", "baz", "1.0");
    let module = producers.add_to_wasm(&module).unwrap();

    let mut producers = Producers::empty();
    producers.add("language", "waaat", "");
    let module = producers.add_to_wasm(&module).unwrap();

    let metadata = Metadata::from_binary(&module).unwrap();
    match metadata {
        Metadata::Module {
            name, producers, ..
        } => {
            assert_eq!(name, None);
            let producers = producers.expect("some producers");
            assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
            assert_eq!(producers.get("language").unwrap().get("waaat").unwrap(), "");
            assert_eq!(
                producers.get("processed-by").unwrap().get("baz").unwrap(),
                "1.0"
            );
        }
        _ => panic!("metadata should be module"),
    }
}

#[test]
fn producers_overwrite_field() {
    let wat = "(module)";
    let module = wat::parse_str(wat).unwrap();
    let mut producers = Producers::empty();
    producers.add("processed-by", "baz", "1.0");
    let module = producers.add_to_wasm(&module).unwrap();

    let mut producers = Producers::empty();
    producers.add("processed-by", "baz", "420");
    let module = producers.add_to_wasm(&module).unwrap();

    let metadata = Metadata::from_binary(&module).unwrap();
    match metadata {
        Metadata::Module { producers, .. } => {
            let producers = producers.expect("some producers");
            assert_eq!(
                producers.get("processed-by").unwrap().get("baz").unwrap(),
                "420"
            );
        }
        _ => panic!("metadata should be module"),
    }
}

#[test]
fn overwrite_registry_metadata() {
    let wat = "(module)";
    let module = wat::parse_str(wat).unwrap();
    let registry_metadata = RegistryMetadata {
        authors: Some(vec!["Foo".to_owned()]),
        ..Default::default()
    };
    let module = registry_metadata.add_to_wasm(&module).unwrap();

    let registry_metadata = RegistryMetadata {
        authors: Some(vec!["Bar".to_owned()]),
        ..Default::default()
    };
    let module = registry_metadata.add_to_wasm(&module).unwrap();

    let metadata = Metadata::from_binary(&module).unwrap();
    match metadata {
        Metadata::Module {
            registry_metadata, ..
        } => {
            let registry_metadata = registry_metadata.expect("some registry_metadata");
            assert_eq!(registry_metadata.authors.unwrap(), vec!["Bar".to_owned()]);
        }
        _ => panic!("metadata should be module"),
    }
}
