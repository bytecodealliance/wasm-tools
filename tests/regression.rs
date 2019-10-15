#[test]
fn empty_string_fails() {
    assert!(wast::parse_str("").is_err());
}
