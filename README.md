<div align="center">
  <h1><code>wasm-tools</code></h1>

<strong>A <a href="https://bytecodealliance.org/">Bytecode Alliance</a> project</strong>

  <p>
    <strong>Rust tooling for low-level manipulation of WebAssembly modules</strong>
  </p>
</div>

# Installation

This project can be installed and compiled from source with this Cargo command:

```
$ cargo install wasm-tools
```

Additionally there are [precompiled artifacts built on CI][artifacts] which are
available for download as well.

[artifacts]: https://github.com/bytecodealliance/wasm-tools/releases

Installation can be confirmed with:

```
$ wasm-tools --version
```

Subcommands can be explored with:

```
$ wasm-tools help
```

# Tools included

The `wasm-tools` binary internally contains a number of subcommands for working
with wasm modules. Many subcommands also come with Rust crates that can be use
programmatically as well:

| Tool | Crate | Description |
|------|------|------------|
| `wasm-tools validate` | [wasmparser] | Validate a WebAssembly file |
| `wasm-tools parse` | [wat] and [wast] | Translate the WebAssembly text format to binary |
| `wasm-tools print` | [wasmprinter] | Translate the WebAssembly binary format to text |
| `wasm-tools smith` | [wasm-smith] | Generate a "random" valid WebAssembly module |
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

The `wasm-tools` CLI is primarily intended to be a debugging aid. The various
subcommands all have `--help` explainer texts to describe more about their
functionality as well.

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

Using the `CMakeLists.txt` in `crates/c-api`, `wasm-tools` can be used from the [`wasm-tools.h` header](crates/c-api/include/wasm-tools.h).

# License

This project is licensed under the Apache 2.0 license with the LLVM exception.
See [LICENSE](LICENSE) for more details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be licensed as above, without any additional terms or conditions.
