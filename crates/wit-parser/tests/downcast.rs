//! Verifies that `ResolveError` is downcastable from the `anyhow::Error`
//! returned by `Resolve`'s public `push_*` methods.
//!
//! This is the contract behind the structured-errors refactor: consumers
//! (LSPs, build tools, custom embedders) can introspect error kinds
//! programmatically instead of pattern-matching rendered strings. Each test
//! pins one of the call sites in `fs.rs` / `mod.rs` that previously stringified
//! errors at the boundary; if any of them regresses, exactly one test fails.

use wit_parser::{Resolve, ResolveError, ResolveErrorKind};

fn find_resolve_error(err: &anyhow::Error) -> &ResolveError {
    err.chain()
        .find_map(|e| e.downcast_ref::<ResolveError>())
        .expect("expected a ResolveError in the error chain")
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
    let re = find_resolve_error(&err);
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
    let re = find_resolve_error(&err);
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
    let re = find_resolve_error(&err);
    assert!(
        matches!(re.kind(), ResolveErrorKind::PackageNotFound { .. }),
        "expected PackageNotFound, got: {:?}",
        re.kind()
    );
}
