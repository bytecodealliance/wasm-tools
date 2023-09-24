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

At this time of this writing, no WebAssembly runtimes have fully
implemented the component model proposal.

__[Wasmtime](https://github.com/bytecodealliance/wasmtime)
has implementation efforts underway to support it.__

## Usage

To composed a component, run the `compose` command:

```sh
wasm-tools compose -o composed.wasm component.wasm
```

This will automatically search for dependencies at the same location
as the input component, `component.wasm`, and create a composed
component named `composed.wasm`.

Any unresolved dependencies will remain as imports in the composed
component.

## Configuration

See [configuring `wasm-compose`](CONFIG.md) for more information on authoring configuration files.

## How it works

`wasm-compose` starts with the input component and then processes each of the component's instance imports.

For each instance import, `wasm-compose` will consult its configuration to determine how to locate a dependency with the same name as the import.

If the dependency is not specified in the configuration, `wasm-compose` will search the configured search paths for a matching component file.

If a component to satisfy the dependency cannot be found, it will remain as an instance import in the composed component; at least one dependency must be satisfied for the component to be composed.

`wasm-compose` then repeats this process for all of the transitive imports of dependent components that have been found.

The composed component will, by default, define the transitive component dependencies directly in the composed component; it will then instantiate the dependencies in a topological order.

Finally the input component is instantiated and all of its exports are then exported from the composed component.

## Example

See the [example](example/README.md) directory for a complete example
of composing WebAssembly components together.

## License

This project is licensed under the Apache 2.0 license with the LLVM exception.
See [LICENSE](../../LICENSE) for more details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be licensed as above, without any additional terms or conditions.
