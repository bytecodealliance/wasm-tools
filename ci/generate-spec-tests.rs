//! Helper program to generate files in `tests/cli/spec/*` which correspond to
//! running spec tests in `tests/testsuite/*`.

use std::fs;
use std::path::Path;

fn main() {
    let _ = fs::remove_dir_all("./tests/cli/spec");
    copy_tests(
        "tests/testsuite".as_ref(),
        "tests/cli/spec".as_ref(),
        spec_features,
    );
    copy_tests(
        "tests/component-model/test".as_ref(),
        "tests/cli/spec/components".as_ref(),
        component_features,
    );
}

fn spec_features(path: &Path) -> &str {
    return match find_proposal(path) {
        None => "wasm3",
        Some("threads") => "wasm1,threads",
        Some("custom-page-sizes") => "wasm3,custom-page-sizes",
        Some("wide-arithmetic") => "wasm3,wide-arithmetic",
        Some("custom-descriptors") => "wasm3,custom-descriptors",
        Some(proposal) => panic!("unsupported proposal: {}", proposal),
    };

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
}

fn component_features(_path: &Path) -> &str {
    "wasm3,\
        component-model,\
        cm-map,\
        cm-async,\
        cm-implements,\
        cm-async-stackful,\
        cm-threading,\
        cm-more-async-builtins,\
        cm-fixed-length-lists"
}

/// Recursively visit `src` and, for all test files, create a file in `dst` to
/// run the test.
fn copy_tests(src: &Path, dst: &Path, features: fn(&Path) -> &str) {
    fs::create_dir(&dst).unwrap();
    for entry in src.read_dir().unwrap() {
        let entry = entry.unwrap();

        let src = entry.path();
        let dst = dst.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tests(&src, &dst, features);
            continue;
        }

        if src.extension().and_then(|s| s.to_str()) != Some("wast") {
            continue;
        }

        copy_test(&src, &dst, features);
    }
}

/// Creates `dst` as a file to run `src` as a test.
fn copy_test(src: &Path, dst: &Path, features: fn(&Path) -> &str) {
    // The legacy exception-handling proposal is not currently supported because
    // it uses the folded form of s-expressions which are not implemented here.
    // Regardless just skip these spec tests.
    if src.iter().any(|p| p == "legacy") {
        return;
    }

    let directive = match dst.file_name().and_then(|s| s.to_str()) {
        // Disable tests by doing something like:
        // Some("exact-func-import.wast") => "FAIL",

        // Temporary exception until WebAssembly/component-model#704 lands
        Some("kebab.wast") => "FAIL",

        Some(_) | None => "RUN",
    };

    let mut contents = format!(";; {directive}: wast \\\n");
    contents.push_str(";;      --assert default \\\n");

    // Allow certain assert_malformed tests to be interpreted as assert_invalid
    if src.ends_with("binary.wast") {
        contents.push_str(";;      --assert permissive \\\n");
    }

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
    let features = features(src);
    contents.push_str(&format!(";;      --features={features} \\\n"));

    // And finally push a path to the test itself.
    contents.push_str(&format!(";;      {}\n", src.display()));

    fs::write(dst, contents).unwrap();
}
