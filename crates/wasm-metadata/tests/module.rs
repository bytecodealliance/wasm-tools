use std::vec;

use wasm_encoder::Module;
use wasm_metadata::*;

#[test]
fn add_to_empty_module() {
    let module = Module::new().finish();
    let add = AddMetadata {
        name: Some("foo".to_owned()),
        language: vec![("bar".to_owned(), "1.0".to_owned())],
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
            assert_eq!(
                producers.get("language").unwrap().get("bar").unwrap(),
                "1.0"
            );
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
            assert_eq!(range.end, 425);
        }
        _ => panic!("metadata should be module"),
    }
}
