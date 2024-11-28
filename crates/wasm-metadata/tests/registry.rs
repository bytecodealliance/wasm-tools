use std::vec;

use wasm_metadata::*;

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
