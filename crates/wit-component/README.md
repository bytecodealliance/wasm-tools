<div align="center">
  <h1><code>wit-component</code></h1>

  <p>
    <strong>WebAssembly component tooling based on the component model proposal and <em>wit-bindgen</em>.</strong>
  </p>

  <strong>A <a href="https://bytecodealliance.org/">Bytecode Alliance</a> project</strong>

  <p>
    <a href="https://github.com/bytecodealliance/wit-bindgen/actions?query=workflow%3ACI"><img src="https://github.com/bytecodealliance/wit-bindgen/workflows/CI/badge.svg" alt="build status" /></a>
    <img src="https://img.shields.io/badge/rustc-stable+-green.svg" alt="supported rustc stable" />
  </p>
</div>

# `wit-component`

`wit-component` is a crate and a set of CLI tools for creating and interacting with WebAssembly components based on the [component model proposal](https://github.com/WebAssembly/component-model/).

## Tools

* `wit-component` - creates a WebAssembly component from a core WebAssembly module and a set of
  `.wit` files representing the component's imported and exported interfaces.

* `wit2wasm` - encodes an interface definition (in `wit`) as an "interface-only" WebAssembly component.
  A `.wasm` component file will be generated that stores a full description of the original interface.

* `wasm2wit` - decodes an "interface-only" WebAssembly component to an interface definition (in `wit`).
  A `.wit` file will be generated that represents the interface described by the component.
