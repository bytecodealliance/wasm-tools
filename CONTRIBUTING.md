# Contributing

This project welcomes contributions for bug fixes, documentation updates, new
features, or whatever you might like. Development is done through GitHub pull
requests. Feel free to reach out on the [Bytecode Alliance
Zulip](https://bytecodealliance.zulipchat.com/) as well if you'd like assistance
in contributing or would just like to say hi.

### Building From Source

To create a debug build of this project from source, execute this command at the
root of the repository:

```
$ cargo build
```

And the resulting binary is located at `./target/debug/wasm-tools` for the
current platform.

An optimized build can be produced with:

```
$ cargo build --release
```

### Testing

Many crates in this repository (located in `crates/*`) both have unit tests
(`#[test]` functions throughout the source) and integration tests
(`crates/*/tests/*.rs`). Testing an individual crate can be done with:

```
$ cargo test -p wasmparser
```

Running all tests can be done by fist ensuring that the spec test suite is
checked out:

```
$ git submodule update --init
```

and then using Cargo to execute all tests:

```
$ cargo test --workspace
```

Running the spec test suite can be done with:

```
$ cargo test --test roundtrip
```

and running a single spec test can be done with an argument to this command as a
string filter on the filename.

```
$ cargo test --test roundtrip binary-leb128.wast
```

Many tests are also located in the top-level `tests/*` folder. This is organized
into a few suites:

* `tests/cli/*` - these files are run by the `tests/cli.rs` test file and are
  intended to be tests for the CLI itself. They start with `;; RUN: ...` headers
  to indicate what commands should run and adjacent files indicate the expected
  output.

* `tests/local/*` - these are handwritten `*.wat` and `*.wast` tests. The
  `*.wat` files must all validate as valid modules and `*.wast` files run their
  directives in the same manner as the spec test suite. This folder additional
  subfolders for specific classes of tests, for example `missing-features` has
  all optional wasm features disabled to test what happens when a feature is
  implemented but disabled at runtime. The `component-model` folder contains all
  tests related to enabling the component model feature.

* `tests/testsuite` - this is a git submodule pointing to the [upstream test
  suite repository](https://github.com/WebAssembly/testsuite/) and is where spec
  tests come from.

* `tests/roundtrip.rs` - this is the main driver for the `local` and `testsuite`
  folders. This will crawl over all files in those folders and execute what
  tests it can. This means running `*.wast` directives such as `assert_invalid`.
  Additionally all valid wasm modules are printed with `wasmprinter` and then
  parsed again with `wat` to ensure that they can be round-tripped through the
  crates.

* `tests/snapshots` - this contains golden output files which correspond to the
  `wasmprinter`-printed version of binaries of all tests. These files are used
  to view the impact of changes to `wasmprinter`.

Many tests throughout the repository have automatically generated files
associated with them which reflect the expected output of an operation. This is
done to view, during code review, the impact of changes made. It's not expected
that these files need to be edited by hand, but instead setting the environment
variable `BLESS=1` when running tests will update all of these test
expectations.

### Continuous Integration

All changes to `wasm-tools` are required to pass the CI suite powered by GitHub
Actions. Pull requests will automatically have checks performed and can only be
merged once all tests are passing. CI checks currently include:

* Code is all formatted correctly (use `cargo fmt` locally to pass this)
* Tests pass on Rust stable, beta, and Nightly.
* Tests pass on Linux, macOS, and Windows.
* This tool can be compiled to WebAssembly using the `wasm32-wasi` target.
* Fuzzers can be built.
* Various miscellaneous checks such as building the tool with various
  combinations of Cargo features.

### Fuzzing

This repository uses LLVM's libFuzzer through the [`cargo
fuzz`](https://github.com/rust-fuzz/cargo-fuzz) tool. Building fuzzers requires
a Nightly Rust toolchain which can be acquired with Rustup-based installations
of Rust by executing:

```
$ rustup update nightly
```

Next the `cargo-fuzz` runner should be installed:

```
$ cargo install cargo-fuzz
```

Fuzzers are then built with:

```
$ cargo +nightly fuzz build
```

Useful options to this can include:

* `--dev` - build fuzzers in debug mode instead of release mode (default is
  release)
* `--sanitizer none` - Rust doesn't benefit much from AddressSanitizer for
  example so disabling sanitizers can improve fuzzing performance and build more
  quickly too.

The fuzzing binary for this project is located at
`target/$host_target/release/run`. Due to limitations on OSS-Fuzz all fuzzers
are combined into a single binary at this time. This binary can be run with:

```
$ cargo +nightly fuzz run run
```

The main driver for fuzzing is located at `fuzz/fuzz_targets/run.rs`. This
driver dispatches, based on the input, to a number of other fuzzers. Each
individual fuzzer lives in `fuzz/src/*.rs`.

Running a single fuzzer can be done by configuring the `FUZZER` environment
variable:

```
$ FUZZER=roundtrip cargo +nightly fuzz run run
```

More documentation of `cargo fuzz` can [be found
online](https://rust-fuzz.github.io/book/cargo-fuzz.html).

# License

This project is licensed under the Apache 2.0 license with the LLVM exception.
See [LICENSE](LICENSE) for more details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be licensed as above, without any additional terms or conditions.
