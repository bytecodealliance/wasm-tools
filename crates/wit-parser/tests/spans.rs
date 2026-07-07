//! Tests for `SourceMap::resolve_span`, which resolves a global `Span` to a
//! file path and file-local byte range.

use wit_parser::{ParseError, Resolve, SourceMap, UnresolvedPackageGroup};

#[test]
fn resolve_span_in_single_file() {
    let contents = "package foo:foo;\nworld w { not-a-keyword }";
    let (map, err) =
        UnresolvedPackageGroup::parse("test.wit", contents).expect_err("expected parse to fail");
    let loc = map
        .resolve_span(err.kind().span())
        .expect("span should resolve");
    assert_eq!(loc.path, "test.wit");
    assert_eq!(&contents[loc.range.clone()], "not-a-keyword");
}

#[test]
fn resolve_span_in_second_file() {
    let mut map = SourceMap::default();
    map.push_str("a.wit", "package foo:foo;\ninterface a {}\n");
    let b = "interface b { type t = nonexistent; }";
    map.push_str("b.wit", b);
    let (map, err) = map.parse().expect_err("expected parse to fail");
    let loc = map
        .resolve_span(err.kind().span())
        .expect("span should resolve");
    assert_eq!(loc.path, "b.wit");
    assert_eq!(&b[loc.range.clone()], "nonexistent");
}

#[test]
fn resolve_span_at_eof() {
    let contents = "package foo:foo;\nworld w {";
    let (map, err) =
        UnresolvedPackageGroup::parse("test.wit", contents).expect_err("expected parse to fail");
    let loc = map
        .resolve_span(err.kind().span())
        .expect("span should resolve");
    assert_eq!(loc.path, "test.wit");
    // The source map appends a synthetic newline internally and eof spans
    // point at it; they must resolve to an empty range at the real length.
    assert_eq!(loc.range, contents.len()..contents.len());
}

#[test]
fn resolve_span_not_in_map() {
    let contents = "package foo:foo;\nworld w { not-a-keyword }";
    let (_, err) =
        UnresolvedPackageGroup::parse("test.wit", contents).expect_err("expected parse to fail");
    let other = SourceMap::default();
    assert!(other.resolve_span(err.kind().span()).is_none());
}

#[test]
fn resolve_span_against_resolve_source_map() {
    let mut resolve = Resolve::new();
    let contents = "package foo:foo;\nworld w { not-a-keyword }";
    let err = resolve
        .push_str("test.wit", contents)
        .expect_err("expected parse to fail");
    let pe = err
        .chain()
        .find_map(|layer| layer.downcast_ref::<ParseError>())
        .expect("expected a ParseError in the chain");
    // The failure-path merge keeps spans valid against `resolve.source_map`,
    // so resolution against it must find the right file and range.
    let loc = resolve
        .source_map
        .resolve_span(pe.kind().span())
        .expect("span should resolve");
    assert_eq!(loc.path, "test.wit");
    assert_eq!(&contents[loc.range.clone()], "not-a-keyword");
}
