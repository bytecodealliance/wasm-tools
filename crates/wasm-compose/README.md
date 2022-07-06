<div align="center">
  <h1><code>wasm-compose</code></h1>

<strong>A <a href="https://bytecodealliance.org/">Bytecode Alliance</a> project</strong>

  <p>
    <strong>A WebAssembly component composing tool.</strong>
  </p>

  <p>
    <a href="https://crates.io/crates/wasm-compose"><img src="https://img.shields.io/crates/v/wasm-compose.svg?style=flat-square" alt="Crates.io version" /></a>
    <a href="https://crates.io/crates/wasm-compose"><img src="https://img.shields.io/crates/d/wasm-compose.svg?style=flat-square" alt="Download" /></a>
    <a href="https://docs.rs/wasm-compose/"><img src="https://img.shields.io/static/v1?label=docs&message=wasm-compose&color=blue&style=flat-square" alt="docs.rs docs" /></a>
  </p>
</div>

## Overview

`wasm-compose` is a tool for composing [WebAssembly components](https://github.com/webassembly/component-model)
from other WebAssembly components.

## Implementation status

A quick note on the implementation status of the component model
proposal:

__At this time of this writing, no WebAssembly runtimes have fully
implemented the component model proposal.__

__[Wasmtime](https://github.com/bytecodealliance/wasmtime)
has implementation efforts underway to support it, but it's still a
_work-in-progress_.__

## Example

See the [example](example/README.md) directory for a complete example
of composing WebAssembly components together.

## Usage

With a configuration file present, run `wasm-compose` without any options to
compose a component:

```sh
cargo run -p wasm-compose
```

## Configuration

By default, `wasm-compose` looks for a configuration file named
`wasm-compose.toml` or `wasm-compose.yml` in the current directory.

The composition configuration currently has three main sections:

* `imports` - specifies the component imports for the composed component.
* `instantiations` - specifies the instantiations of an imported component.
* `exports` - specifies the exports of the composed component.

### Imports section

Each import is given a name to refer to that import from the `instantiations`
section.

In this example, an import named `a` is defined for a component found at `./a.wasm`:

```toml
[imports.a]
path = "a.wasm"
```
By default, the composed component will _import_ its component dependencies
rather than embedding them in the composed component.

To change this, set the `embed` field to `true`:

```toml
[imports.a]
path = "a.wasm"
embed = true
```

Additionally, the `--embed` option can be given to `wasm-compose` to embed
_all_ imports into the composed component.

It is currently necessary to embed _all_ imports to run
the composed component in [Wasmtime](https://github.com/bytecodealliance/wasmtime).

### Instantiations section

The `instantiations` section defines how the imported
components are instantiated in the composed component.

By default, an instantiation looks for an import of the same name. To change
what import is instantiated, use the `import` field:

```toml
[instantiations.foo]
import = "bar"
```

Here the `foo` instantiation uses the `bar` import rather than the expected
default of the `foo` import.

To specify a dependency between instantiations, use the `dependencies` field:

```toml
[instantiations.a]
[instantiations.b]
dependencies = ["a"]
```

In the above example, import `a` is _default_ instantiated because it has no
dependencies. Import `b` is instantiated with a dependency on instance `a` to
satisfy _all_ of its instantiation arguments.

Because instance `a` can be _default_ instantiated and it is for an import of
the same name, this can be simplified to the following without having to
explicitly define an `a` instance:

```toml
[instantiations.b]
dependencies = ["a"]
```

The `dependencies` field of an instantiation automatically tries to resolve the
instantiation arguments from the list of dependencies.

It does this by scanning the dependencies for instantiation arguments that can
be satisfied by the dependency. If more than one argument can be satisfied by a
dependency, then `wasm-compose` will error and an explicit instantiation
argument must be used.

To specify an explicit instantiation argument, use the `with` field:

```toml
[instantiations.b.with]
foo = "a"
```

Here instance `a` is explicitly being specified for instantiation argument `foo`.

Explicit instantiation arguments can be combined with the `dependencies` field
too:

```toml
[instantiations.c]
dependencies = ["b"]

[instantiations.c.with]
a = "a"
```

Here the `a` instantiation argument of instantiation `c` is explicitly specified
to instance `a`, whereas any remaining arguments will be automatically resolved
from instance `b`.

Currently only the _default_ (directly) exported interface of each instance is
considered for resolving dependencies; a forthcoming feature will add the ability
to use specify instance exports from instances.

### Exports section

The `exports` section defines the exports of the composed component.

Currently, it only supports exporting the _default_ interface from a single
instance:

```toml
[exports]
default = "a"
```

Here the composed component will export the _default_ interface (i.e. directly
exported functions) of instance `a`.

Upcoming work will make specifying exports from the composed component more
flexible.

## License

This project is licensed under the Apache 2.0 license with the LLVM exception.
See [LICENSE](LICENSE) for more details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be licensed as above, without any additional terms or conditions.
