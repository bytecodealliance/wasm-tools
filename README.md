<div align="center">
  <h1><code>wasm-tools</code></h1>

<strong>A <a href="https://bytecodealliance.org/">Bytecode Alliance</a> project</strong>

  <p>
    <strong>CLI and Rust libraries for low-level manipulation of WebAssembly modules</strong>
  </p>
</div>

# Installation

[Precompiled artifacts built on CI][artifacts] are available for download for
each release.

If you'd prefer to build from source then first [install Rust for your
platform](https://www.rust-lang.org/tools/install) and then use the included
Cargo package manager to install:

```
$ cargo install wasm-tools
```

[artifacts]: https://github.com/bytecodealliance/wasm-tools/releases

Installation can be confirmed with:

```
$ wasm-tools --version
```

Subcommands can be explored with:

```
$ wasm-tools help
```

# Examples

Basic validation/printing:

```sh
# Validate a WebAssembly file
$ wasm-tools validate foo.wasm

# Validate a WebAssembly module in the text format, automatically converting to
# binary.
$ wasm-tools validate foo.wat

# Validate a WebAssembly file enabling an off-by-default feature
$ wasm-tools validate foo.wasm --features=exception-handling

# Validate a WebAssembly file with a default-enabled feature disabled
$ wasm-tools validate foo.wasm --features=-simd

# Print the text format of a module to stdout
$ wasm-tools print foo.wasm

# Convert a binary module to text
$ wasm-tools print foo.wasm -o foo.wat
```

Simple mutation as well as piping commands together:

```sh
# Mutate a WebAssembly module and print its text representation to stdout
$ wasm-tools mutate foo.wasm -t

# Mutate a WebAssembly module with a non-default seed and validate that the
# output is a valid module.
$ wasm-tools mutate foo.wasm --seed 192 | wasm-tools validate

# Demangle Rust/C++ symbol names in the `name` section, strip all other custom
# sections, and then print out what binary sections remain.
$ wasm-tools demangle foo.wasm | wasm-tools strip | wasm-tools objdump
```

Working with components:

```sh
# Print the WIT interface of a component
$ wasm-tools component wit component.wasm

# Convert WIT text files to a binary-encoded WIT package, printing the result to
# stdout
$ wasm-tools component wit ./wit -t

# Convert a WIT document to JSON
$ wasm-tools component wit ./wit --json

# Round trip WIT through the binary-encoded format to stdout.
$ wasm-tools component wit ./wit --wasm | wasm-tools component wit

# Convert a core WebAssembly binary into a component. Note that this requires
# WIT metadata having previously been embedded in the core wasm module.
$ wasm-tools component new my-core.wasm -o my-component.wasm

# Convert a core WebAssembly binary which uses WASI to a component.
$ wasm-tools component new my-core.wasm -o my-component.wasm --adapt wasi_snapshot_preview1.reactor.wasm
```

### CLI Conventions

There are a few conventions that all CLI commands adhere to:

* Input is by default read from stdin if no file input is specified (when
  applicable).
* Output is by default sent to stdout if a `-o` or `--output` flag is not
  provided. Binary WebAssembly is not printed to a tty by default, however.
* Commands which output WebAssembly binaries all support a `-t` or `--wat` flag
  to generate the WebAssembly text format instead.
* A `-v` or `--verbose` flag can be passed to enable log messages throughout the
  tooling. Verbosity can be turned up by passing the flag multiple times such as
  `-vvv`.
* Color in error messages and console output is enabled by default for TTY based
  outputs and can be configured with a `--color` argument.

# Tools included

The `wasm-tools` binary internally contains a number of subcommands for working
with wasm modules and component. Many subcommands also come with Rust crates
that can be use programmatically as well:

| CLI | Rust Crate | Description |
|------|------|------------|
| `wasm-tools validate` | [wasmparser] | Validate a WebAssembly file |
| `wasm-tools parse` | [wat] and [wast] | Translate the WebAssembly text format to binary |
| `wasm-tools print` | [wasmprinter] | Translate the WebAssembly binary format to text |
| `wasm-tools smith` | [wasm-smith] | Generate a valid WebAssembly module from an input seed |
| `wasm-tools mutate` | [wasm-mutate] | Mutate an input wasm file into a new valid wasm file |
| `wasm-tools shrink` | [wasm-shrink] | Shrink a wasm file while preserving a predicate |
| `wasm-tools dump` |   | Print debugging information about the binary format |
| `wasm-tools objdump` |   | Print debugging information about section headers |
| `wasm-tools strip` |   | Remove custom sections from a WebAssembly file |
| `wasm-tools demangle` |   | Demangle Rust and C++ symbol names in the `name` section |
| `wasm-tools compose` | [wasm-compose] | Compose wasm components together |
| `wasm-tools component new` | [wit-component] | Create a component from a core wasm binary |
| `wasm-tools component wit` |  | Extract a `*.wit` interface from a component |
| `wasm-tools component embed` |  | Embed a `component-type` custom section in a core wasm binary |
| `wasm-tools metadata show` |  [wasm-metadata] | Show name and producer metadata in a component or module |
| `wasm-tools metadata add` |  | Add name or producer metadata to a component or module |
| `wasm-tools addr2line` |  | Translate wasm offsets to filename/line numbers with DWARF |
| `wasm-tools completion` |  | Generate shell completion scripts for `wasm-tools` |

[wasmparser]: https://crates.io/crates/wasmparser
[wat]: https://crates.io/crates/wat
[wast]: https://crates.io/crates/wast
[wasmprinter]: https://crates.io/crates/wasmprinter
[wasm-smith]: https://crates.io/crates/wasm-smith
[wasm-mutate]: https://crates.io/crates/wasm-mutate
[wasm-shrink]: https://crates.io/crates/wasm-shrink
[wit-component]: https://crates.io/crates/wit-component
[wasm-compose]: https://crates.io/crates/wasm-compose
[wasm-metadata]: https://crates.io/crates/wasm-metadata

The `wasm-tools` CLI contains useful tools for debugging WebAssembly modules and
components. The various subcommands all have `--help` explainer texts to
describe more about their functionality as well.

# Libraries

As mentioned above many of the tools of the `wasm-tools` CLI have libraries
implemented in this repository as well. These libraries are:

* [**`wasmparser`**](crates/wasmparser) - a library to parse WebAssembly binaries
* [**`wat`**](crates/wat) - a library to parse the WebAssembly text format
* [**`wast`**](crates/wast) - like `wat`, except provides an AST
* [**`wasmprinter`**](crates/wasmprinter) - prints WebAssembly binaries in their
  string form
* [**`wasm-mutate`**](crates/wasm-mutate) - a WebAssembly test case mutator
* [**`wasm-shrink`**](crates/wasm-shrink) - a WebAssembly test case shrinker
* [**`wasm-smith`**](crates/wasm-smith) - a WebAssembly test case generator
* [**`wasm-encoder`**](crates/wasm-encoder) - a crate to generate a binary
  WebAssembly module
* [**`wit-parser`**](crates/wit-parser) - a crate to parse and manage `*.wit`
  files and interfaces.
* [**`wit-component`**](crates/wit-component) - a crate to create components
  from core wasm modules.
* [**`wasm-metadata`**](crates/wasm-metadata) - a crate to manipulate name and
  producer metadata (custom sections) in a wasm module or component.

It's recommended to use the libraries directly rather than the CLI tooling when
embedding into a separate project.

# C/C++ bindings

Using the `CMakeLists.txt` in `crates/c-api`, `wasm-tools` can be used from the
[`wasm-tools.h` header](crates/c-api/include/wasm-tools.h). Note that these
bindings do not comprehensively cover all the functionality of this repository
at this time, but please feel free to contribute more if you find functions
useful!

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
