<div align="center">
  <h1><code>wasm-compose</code></h1>

<strong>A <a href="https://bytecodealliance.org/">Bytecode Alliance</a> project</strong>

  <p>
    <strong>A WebAssembly component composing library.</strong>
  </p>

  <p>
    <a href="https://crates.io/crates/wasm-compose"><img src="https://img.shields.io/crates/v/wasm-compose.svg?style=flat-square" alt="Crates.io version" /></a>
    <a href="https://crates.io/crates/wasm-compose"><img src="https://img.shields.io/crates/d/wasm-compose.svg?style=flat-square" alt="Download" /></a>
    <a href="https://docs.rs/wasm-compose/"><img src="https://img.shields.io/static/v1?label=docs&message=wasm-compose&color=blue&style=flat-square" alt="docs.rs docs" /></a>
  </p>
</div>

## Overview

`wasm-compose` is a library for composing [WebAssembly components](https://github.com/webassembly/component-model)
from other WebAssembly components.

It is made available as the `compose` subcommand of `wasm-tools`.

## Implementation status

A quick note on the implementation status of the component model
proposal:

__At this time of this writing, no WebAssembly runtimes have fully
implemented the component model proposal.__

__[Wasmtime](https://github.com/bytecodealliance/wasmtime)
has implementation efforts underway to support it, but it's still a
_work-in-progress_.__

## Usage

With a configuration file present, run the `compose` command without
any options to compose a component:

```sh
wasm-tools compose
```

## Configuration

See [configuring `wasm-compose`](CONFIG.md) for more information on authoring configuration files.

## Example

See the [example](example/README.md) directory for a complete example
of composing WebAssembly components together.

## License

This project is licensed under the Apache 2.0 license with the LLVM exception.
See [LICENSE](LICENSE) for more details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be licensed as above, without any additional terms or conditions.
