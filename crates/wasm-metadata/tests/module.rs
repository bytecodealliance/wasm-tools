use std::{str::FromStr, vec};

use auditable_serde::VersionInfo;
use wasm_encoder::Module;
use wasm_metadata::*;

#[test]
fn add_to_empty_module() {
    let add = AddMetadata {
        name: Some("foo".to_owned()),
        language: vec![("bar".to_owned(), "1.0".to_owned())],
        processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
        sdk: vec![],
        authors: Some(Authors::new("Chashu Cat")),
        description: Some(Description::new("Chashu likes tuna")),
        licenses: Some(
            Licenses::new("Apache-2.0 WITH LLVM-exception OR Apache-2.0 OR MIT").unwrap(),
        ),
        source: Some(Source::new("https://github.com/bytecodealliance/wasm-tools").unwrap()),
        homepage: Some(Homepage::new("https://github.com/bytecodealliance/wasm-tools").unwrap()),
        revision: Some(Revision::new("de978e17a80c1118f606fce919ba9b7d5a04a5ad")),
        version: Some(Version::new("1.0.0")),
    };

    let json_str = r#"{"packages":[{"name":"adler","version":"0.2.3","source":"registry"}]}"#;
    let info = VersionInfo::from_str(json_str).unwrap();
    let mut component = Module::new();
    component.section(&Dependencies::new(info.clone()));
    let module = component.finish();
    let module = add.to_wasm(&module).unwrap();

    match Payload::from_binary(&module).unwrap() {
        Payload::Module(Metadata {
            name,
            producers,
            authors: author,
            licenses,
            source,
            range,
            description,
            homepage,
            revision,
            version,
            dependencies,
        }) => {
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

            assert_eq!(author.unwrap(), Authors::new("Chashu Cat"));
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
            assert_eq!(dependencies.unwrap().version_info(), &info,);

            assert_eq!(range.start, 0);
            assert_eq!(range.end, 455);
        }
        _ => panic!("metadata should be module"),
    }
}
