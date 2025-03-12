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

[artifacts]: https://github.com/bytecodealliance/wasm-tools/releases

To build from source first [install Rust for your
platform](https://www.rust-lang.org/tools/install) and then use the included
Cargo package manager to install:

```
$ cargo install --locked wasm-tools
```

Alternatively if you use [`cargo
binstall`](https://github.com/cargo-bins/cargo-binstall) then that can be used
to install [the precompiled artifacts][artifacts] instead:

```
$ cargo binstall wasm-tools
```

Installation can be confirmed with:

```
$ wasm-tools --version
```

Subcommands can be explored with:

```
$ wasm-tools help
```

# Examples

Basic validation/printing/parsing:

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

# Convert a text module to binary
$ wasm-tools parse foo.wat -o foo.wasm
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

* All subcommands print "short help" with `-h` and "long help" with `--help`.
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

| CLI | Rust Crate | Playground | Description |
|------|------|--------|------------|
| `wasm-tools validate` | [wasmparser] |  | Validate a WebAssembly file |
| `wasm-tools parse` | [wat] and [wast] | [parse](https://bytecodealliance.github.io/wasm-tools/parse) | Translate the WebAssembly text format to binary |
| `wasm-tools print` | [wasmprinter] | [print](https://bytecodealliance.github.io/wasm-tools/print) | Translate the WebAssembly binary format to text |
| `wasm-tools smith` | [wasm-smith] |  | Generate a valid WebAssembly module from an input seed |
| `wasm-tools mutate` | [wasm-mutate] |  | Mutate an input wasm file into a new valid wasm file |
| `wasm-tools shrink` | [wasm-shrink] |  | Shrink a wasm file while preserving a predicate |
| `wasm-tools dump` |   |  | Print debugging information about the binary format |
| `wasm-tools objdump` |   |  | Print debugging information about section headers |
| `wasm-tools strip` |   |  | Remove custom sections from a WebAssembly file |
| `wasm-tools demangle` |   |  | Demangle Rust and C++ symbol names in the `name` section |
| `wasm-tools compose` | [wasm-compose] |  | Compose wasm components together (*deprecated*) |
| `wasm-tools component new` | [wit-component] |  | Create a component from a core wasm binary |
| `wasm-tools component wit` |  |  | Extract a `*.wit` interface from a component |
| `wasm-tools component embed` |  |  | Embed a `component-type` custom section in a core wasm binary |
| `wasm-tools component unbundle` |  |  | Extract core wasm modules from a component |
| `wasm-tools metadata show` |  [wasm-metadata] |  | Show name and producer metadata in a component or module |
| `wasm-tools metadata add` |  |  | Add name or producer metadata to a component or module |
| `wasm-tools addr2line` |  |  | Translate wasm offsets to filename/line numbers with DWARF |
| `wasm-tools completion` |  |  | Generate shell completion scripts for `wasm-tools` |
| `wasm-tools json-from-wast` |  |  | Convert a `*.wast` file into JSON commands |
| `wasm-tools wast` |  |  | Validate the structure of a `*.wast` file |

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

# WebAssembly proposals

This repository strives to implement any standardized proposal to WebAssembly.
All proposals that are [Stage 4+](https://github.com/WebAssembly/proposals) are
enabled by default in validation. Support in the text format is always enabled
at all times for all proposals.

Currently implemented proposals in this repository that are stage 4+ are:

* [x] [annotations](https://github.com/WebAssembly/annotations)
* [x] [branch-hinting](https://github.com/WebAssembly/branch-hinting)
* [x] [bulk-memory](https://github.com/WebAssembly/bulk-memory-operations)
* [x] [component-model](https://github.com/WebAssembly/component-model) (note
  this is an exception, it's not stage4+ but is on-by-default)
* [x] [exceptions](https://github.com/WebAssembly/exceptions)
* [x] [extended-const](https://github.com/WebAssembly/extended-const)
* [x] [extended-name-section](https://github.com/WebAssembly/extended-name-section)
  (note this is not phase 4 but `wast` does not have the concept of features)
* [x] [function-references](https://github.com/WebAssembly/function-references)
* [x] [gc](https://github.com/WebAssembly/gc)
* [x] [memory64](https://github.com/WebAssembly/memory64)
* [x] [multi-memory](https://github.com/WebAssembly/multi-memory)
* [x] [multi-value](https://github.com/WebAssembly/multi-value)
* [x] [mutable-global](https://github.com/WebAssembly/mutable-global)
* [x] [reference-types](https://github.com/WebAssembly/reference-types)
* [x] [relaxed-simd](https://github.com/WebAssembly/relaxed-simd)
* [x] [saturating-float-to-int](https://github.com/WebAssembly/nontrapping-float-to-int-conversions)
* [x] [sign-extension-ops](https://github.com/WebAssembly/sign-extension-ops)
* [x] [simd](https://github.com/WebAssembly/simd)
* [x] [tail-call](https://github.com/WebAssembly/tail-call)
* [x] [threads](https://github.com/WebAssembly/threads)
* [x] [wat-numeric-values](https://github.com/WebAssembly/wat-numeric-values)
  (note this is not phase 4 but `wast` does not have the concept of features)

These proposals are implemented but have not yet reached stage 4. This means
they are disabled by default in validation and their implementation may change
over time as the proposal evolves. Additionally the proposal may also have
changed since these proposals were implemented, so there may be a mismatch too.

* [x] [custom-page-sizes](https://github.com/WebAssembly/custom-page-sizes)
* [x] [memory-control](https://github.com/WebAssembly/memory-control)
* [x] [shared-everything-threads](https://github.com/WebAssembly/shared-everything-threads)
* [x] [stack-switching](https://github.com/WebAssembly/stack-switching)
* [x] [wide-arithmetic](https://github.com/WebAssembly/wide-arithmetic)

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
* [**`wit-encoder`**](crates/wit-encoder) - a crate to generate `*.wit` files and
  interfaces.
* [**`wit-component`**](crates/wit-component) - a crate to create components
  from core wasm modules.
* [**`wit-smith`**](crates/wit-smith) - a `*.wit` test case generator.
* [**`wasm-metadata`**](crates/wasm-metadata) - a crate to read and manipulate
  WebAssembly metadata

It's recommended to use the libraries directly rather than the CLI tooling when
embedding into a separate project.

# C/C++ bindings

Using the `CMakeLists.txt` in `crates/c-api`, `wasm-tools` can be used from the
[`wasm-tools.h` header](crates/c-api/include/wasm-tools.h). Note that these
bindings do not comprehensively cover all the functionality of this repository
at this time, but please feel free to contribute more if you find functions
useful!

# Versioning and Releases

This repository has both a CLI and a suite of crates that is published to
crates.io (Rust's package manager). The versioning scheme used by this
repository looks like:

* `wasm-tools` - the CLI follows the versioning pattern of `1.X.Y`. Frequently
  `Y` is 0 and `X` is bumped as part of a release for this repository.
* `wat` - this Rust crate is versioned at `1.X.Y` as well and matches the
  `wasm-tools` version.
* `wast` - this Rust crate is versioned as `X.0.Y`. The `X` here matches the `X`
  in `1.X.Y` of `wasm-tools`.
* All other crates - all other crates in this repository are versioned at
  `0.X.Y` where `X` matches the `1.X.Y` of `wasm-tools`.

Note that the `Y` of all the versions above will also match for any release of
this repository. This versioning scheme is intended to reflect the stable nature
of the CLI and the `wat` crate in terms of API stability. Other crates, however,
all receive a major version bump that are not automatically considered API
compatible on all releases. This reflects how WebAssembly itself is an evolving
standard which is not an unchanging foundation. All of the crates in this
repository are suitable for "production use" but be aware that API stability is
not guaranteed over time. If you have difficulty upgrading versions please feel
free to file an issue and we can help out.

Also, this repository does not currently have a strict release cadence. Releases
are done on an as-needed basis. If you'd like a release done please feel free to
reach out on [Zulip], file an issue, leave a comment on a PR, or otherwise
contact a maintainer.

[Zulip]: https://bytecodealliance.zulipchat.com/

For maintainers, the release process looks like:

* Go to [this link](https://github.com/bytecodealliance/wasm-tools/actions/workflows/release-process.yml)
* Click on "Run workflow" in the UI.
* Use the default `bump` argument and hit "Run workflow"
* Wait for a PR to be created by CI. You can watch the "Actions" tab for if
  things go wrong.
* When the PR opens, close it then reopen it. Don't ask questions.
* Review the PR, approve it, then queue it for merge.

That should be it, but be sure to keep an eye on CI in case anything goes wrong.

## Patch Release Process

Patch releases aren't too frequent so the process is a bit more involved at this
time. The basic idea is the same though as a normal release:

* Determine that you're going to make a patch release for the release of
  `wasm-tools` version `1.N.0`.
* If `N` is the latest version on `main`, or basically if a major release hasn't
  happened since `N`, then land changes on `main` first. Then go to [this
  link](https://github.com/bytecodealliance/wasm-tools/actions/workflows/release-process.yml)
  and run the workflow against the `main` branch with `bump-patch` as an
  argument. Double-check all changes from the `v1.N.0` tag to ensure that no
  breaking changes in any crate have landed.
* If `N` is not the latest version on `main`, then create a new branch in the
  repository named `release-N` starting from the `v1.N.0` tag. Next land changes
  as PRs against this branch, and note that CI may not have been run in awhile
  so it may need some fixes. Then run the same workflow as above against the
  `release-N` branch with the `bump-patch` argument. Again double-check that no
  breaking changes have been made.

Basically a `release-N` may need to be manually created if the `main` branch has
moved beyond `N`. After that though it should work basically the same where a
workflow is run and a PR is merged and that's all that's necessary.

# Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md) for more information about contributing
to this repository.

# License

This project is triple licenced under the Apache 2/ Apache 2 with LLVM exceptions/ MIT licences. The reasoning for this is:
- Apache 2/ MIT is common in the rust ecosystem.
- Apache 2/ MIT is used in the rust compiler, and some of this code may be migrated there.
- Some of this code may be used in compiler output, and the Apache 2 with LLVM exceptions licence is useful for this.

For more details see
- [Apache 2 Licence](LICENSE-APACHE)
- [Apache 2 Licence with LLVM exceptions](LICENSE-Apache-2.0_WITH_LLVM-exception)
- [MIT Licence](LICENSE-MIT)

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache 2/ Apache 2 with LLVM exceptions/ MIT licenses,
shall be licensed as above, without any additional terms or conditions.
