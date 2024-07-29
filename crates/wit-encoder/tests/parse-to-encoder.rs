use pretty_assertions::assert_eq;
use wit_encoder::packages_from_parsed;

#[test]
fn round_trip() {
    let mut resolve = wit_parser::Resolve::new();
    resolve.push_file("./tests/parse-to-encoder.wit").unwrap();
    let packages = packages_from_parsed(&resolve);

    assert!(packages.len() == 1, "Should create exactly one package");
    let package = &packages[0];
    let input = include_str!("./parse-to-encoder.wit");
    assert_eq!(input, package.to_string());
}
