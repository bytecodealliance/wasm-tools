//! Helper program to generate files in `tests/cli/spec/*` which correspond to
//! running spec tests in `tests/testsuite/*`.

use std::fs;
use std::path::Path;

fn main() {
    let _ = fs::remove_dir_all("./tests/cli/spec");
    copy_tests("tests/testsuite".as_ref(), "tests/cli/spec".as_ref());
}

/// Recursively visit `src` and, for all test files, create a file in `dst` to
/// run the test.
fn copy_tests(src: &Path, dst: &Path) {
    fs::create_dir(&dst).unwrap();
    for entry in src.read_dir().unwrap() {
        let entry = entry.unwrap();

        let src = entry.path();
        let dst = dst.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tests(&src, &dst);
            continue;
        }

        if src.extension().and_then(|s| s.to_str()) != Some("wast") {
            continue;
        }

        copy_test(&src, &dst);
    }
}

/// Creates `dst` as a file to run `src` as a test.
fn copy_test(src: &Path, dst: &Path) {
    // The legacy exception-handling proposal is not currently supported because
    // it uses the folded form of s-expressions which are not implemented here.
    // Regardless just skip these spec tests.
    if src.iter().any(|p| p == "legacy") {
        return;
    }

    let mut contents = format!(";; RUN: wast \\\n");
    contents.push_str(";;      --assert default \\\n");
    contents.push_str(";;      --snapshot tests/snapshots \\\n");

    // This test specifically tests various forms of unicode which are
    // default-disallowed, so explicitly allow it here.
    if src.ends_with("names.wast") {
        contents.push_str(";;      --allow-confusing-unicode \\\n");
    }

    // Historically wasm-tools tried to match the upstream error message. This
    // generally led to a large sequence of matches here which is not easy to
    // maintain and is particularly difficult when test suites and proposals
    // conflict with each other (e.g. one asserts one error message and another
    // asserts a different error message). Overall we didn't benefit a whole lot
    // from trying to match errors so just assume the error is roughly the same
    // and otherwise don't try to match it.
    contents.push_str(";;      --ignore-error-messages \\\n");

    // Push a `--features=..` flag for the spec tests. Spec tests often need a
    // precise set of features different from the defaults of `wasm-tools` so
    // it's always overridden here.
    let features = match find_proposal(src) {
        None => "wasm2",
        Some("annotations") => "wasm2",
        Some("threads") => "wasm1,threads",
        Some("function-references") => "wasm2,function-references,tail-call",
        Some("wasm-3.0") => "wasm3",
        Some("gc") => "wasm2,function-references,gc,tail-call",
        Some("multi-memory") => "wasm2,multi-memory",
        Some("extended-const") => "wasm2,extended-const",
        Some("exception-handling") => "wasm2,exceptions,tail-call",
        Some("custom-page-sizes") => "wasm3,custom-page-sizes",
        Some("wide-arithmetic") => "wasm2,wide-arithmetic",
        Some("tail-call") => "wasm2,tail-call",
        Some("relaxed-simd") => "wasm2,relaxed-simd",
        Some("memory64") => "wasm3",
        Some(proposal) => panic!("unsupported proposal: {}", proposal),
    };
    contents.push_str(&format!(";;      --features={features} \\\n"));

    // And finally push a path to the test itself.
    contents.push_str(&format!(";;      {}\n", src.display()));

    fs::write(dst, contents).unwrap();
}

/// Finds the wasm proposal, if present, within `src`.
fn find_proposal(src: &Path) -> Option<&str> {
    // Look for `foo` in `.../proposals/foo/...`
    let mut parts = src.iter();
    while let Some(next) = parts.next() {
        if next.to_str() == Some("proposals") {
            return parts.next()?.to_str();
        }
    }
    None
}
