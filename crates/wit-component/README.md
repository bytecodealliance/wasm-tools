# `wit-component`

`wit-component` is a crate for creating and interacting with WebAssembly
components based on the [component model proposal][model].

## CLI usage

The `wit-component` crate is available through the `wasm-tools` CLI suite under
two subcommands:

```
# Create a component from the input core wasm module
$ wasm-tools component new core.wasm -o component.wasm

# Extract a `*.wit` interface from a component
$ wasm-tools component wit component.wasm
```

## Features

* Creates WebAssembly [component binaries][model] from input core WebAssembly
  modules. Input modules communicate with the [canonical ABI] to imported and
  exported interfaces described with [`*.wit` files][wit]. The wit interface is
  either embedded directly in the core wasm binary if it was generated with
  [`wit-bindgen`] or it can be manually specified with a `--wit` flag.

* Supports "adapters" which can be used to bridge legacy core WebAssembly
  imported functions into [component model][model] functions. Adapters are
  themselves core wasm binaries which will be embedded into the final component.
  An adapter's exports can be imported by the main core wasm binary and the
  adapter can then call component model imports.

* A [`*.wit`][wit] interface can be extracted from an existing component to
  see the interface that it exports and intends to import.

[model]: https://github.com/webassembly/component-model
[canonical ABI]: https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md
[wit]: https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md
[`wit-bindgen`]: https://github.com/bytecodealliance/wit-bindgen

## Usage

Note that this crate is intended to be a low-level detail of tooling for
components. Developers will not necessarily interact with this tooling
day-to-day, instead using wrappers such as
[`cargo-component`](https://github.com/bytecodealliance/cargo-component) which
will automatically execute `wit-component` to produce component binaries.

First `wit-component` supports a "types only" mode where a core wasm input is
not necessary. This mode is used when embedding type information in a core wasm
binary, for example:

```sh
$ cat demo.wit
interface host {
  hello: func()
}
world demo {
  import host: host
}

$ wasm-tools component new --types-only --wit demo.wit -o demo.wasm

# The output `demo.wasm` is a valid component binary
$ wasm-tools validate --features component-model demo.wasm
$ wasm-tools print demo.wasm

# The `*.wit` file can be recovered from the `demo.wasm` as well
$ wasm-tools component wit demo.wasm
```

Toolchain authors can use `wit-component` to embed this "types only" section
into a core wasm binary. For a small demo here a raw `*.wat` wasm text file will
be used where the `demo.wit` argument is specified manually, however.

```sh
$ cat demo.core.wat
(module
  (import "host" "hello" (func))
)

$ wasm-tools component new --wit demo.wit demo.core.wat -o demo.wasm

# Like before the output `demo.wasm` is a valid component binary
$ wasm-tools validate --features component-model demo.wasm
$ wasm-tools print demo.wasm

# Additionally like before the `*.wit` interface can still be extracted
$ wasm-tools component wit demo.wasm
```

Here the `demo.wasm` can now be shipped to a component runtime or embedded into
hosts.
