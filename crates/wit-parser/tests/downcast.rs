//! Verifies that `ResolveError` and `ParseError` are downcastable from the
//! `anyhow::Error` returned by `Resolve`'s public `push_*` methods.

use wit_parser::{ParseError, ParseErrorKind, Resolve, ResolveError, ResolveErrorKind};

fn outer_resolve_error(err: &anyhow::Error) -> &ResolveError {
    err.downcast_ref::<ResolveError>()
        .expect("expected the outermost error to be a ResolveError")
}

fn parse_error_in_chain(err: &anyhow::Error) -> &ParseError {
    err.chain()
        .find_map(|layer| layer.downcast_ref::<ParseError>())
        .expect("expected a ParseError somewhere in the error chain")
}

#[test]
fn push_str_returns_downcastable_error() {
    let mut resolve = Resolve::new();
    let err = resolve
        .push_str(
            "test.wit",
            "package foo:foo;\nworld w { import some:dependency/iface; }",
        )
        .expect_err("expected resolve to fail");
    let re = outer_resolve_error(&err);
    assert!(
        matches!(re.kind(), ResolveErrorKind::PackageNotFound { .. }),
        "expected PackageNotFound, got: {:?}",
        re.kind()
    );
}

#[test]
fn push_file_returns_downcastable_error() {
    let mut resolve = Resolve::new();
    let err = resolve
        .push_file("tests/ui/parse-fail/unresolved-interface4.wit")
        .expect_err("expected resolve to fail");
    let re = outer_resolve_error(&err);
    assert!(
        matches!(re.kind(), ResolveErrorKind::PackageNotFound { .. }),
        "expected PackageNotFound, got: {:?}",
        re.kind()
    );
}

#[test]
fn push_dir_returns_downcastable_error() {
    let mut resolve = Resolve::new();
    let err = resolve
        .push_dir("tests/ui/parse-fail/bad-pkg6")
        .expect_err("expected resolve to fail");
    let re = outer_resolve_error(&err);
    assert!(
        matches!(re.kind(), ResolveErrorKind::PackageNotFound { .. }),
        "expected PackageNotFound, got: {:?}",
        re.kind()
    );
}

#[test]
fn push_str_parse_error_is_downcastable() {
    let mut resolve = Resolve::new();
    let err = resolve
        .push_str("test.wit", "package foo:foo;\nworld w { not-a-keyword }")
        .expect_err("expected parse to fail");
    let pe = parse_error_in_chain(&err);
    // Spans must be valid against `resolve.source_map` after the failure-path
    // merge, so highlighting against it should not panic and should include
    // the file and line.
    let rendered = pe.highlight(&resolve.source_map);
    assert!(
        rendered.contains("test.wit:2:"),
        "expected highlighted error to reference test.wit:2:..., got:\n{rendered}"
    );
    assert!(
        matches!(pe.kind(), ParseErrorKind::Syntax { .. }),
        "expected Syntax, got: {:?}",
        pe.kind()
    );
}

#[test]
fn push_file_parse_error_is_downcastable() {
    let mut resolve = Resolve::new();
    let err = resolve
        .push_file("tests/ui/parse-fail/bad-function.wit")
        .expect_err("expected parse to fail");
    parse_error_in_chain(&err);
}
