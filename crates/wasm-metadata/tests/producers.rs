use wasm_metadata::*;

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
