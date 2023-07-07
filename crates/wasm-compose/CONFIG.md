# Configuring `wasm-compose`

`wasm-compose` can be configured using a YAML configuration file.

To specify the configuration file to use, pass the `--config` option to
`wasm-tools compose`.

The configuration file has the following top-level fields:

* `search-paths` : `list<string>` (optional) - a list of paths to search for
dependencies.
* `skip-validation` : `bool` (optional) - a boolean indicating whether to skip
validation of the resulting composed component.
* `dependencies` : `map<string, dependency>` (optional) - a map specifying the
explicit locations of transitive dependencies.
* `instantiations` : `map<string, instantiation>` (optional) - a map specifying
the explicit instantiations of transitive dependencies.
* `definitions` : `list<string>` (optional) - a list of paths to _definition_
components.

## Dependencies

Each `dependency` specifies a path where `wasm-compose` may locate a
dependency, rather than searching the configured search paths.

A dependency has the following fields:

* `path` : `string` - the path to the WebAssembly component file; the path is
  relative to the configuration file.

* `import` : `string` (optional) - the name to use for importing the component.
  If not present, the component at the given path will be defined directly in
  the composed component.

Dependencies may be specified as a `string` rather than a `map`, in which case
it is treated as having a `path` field with the value of the string.

### Dependencies example

An example of specifying dependencies:

```yaml
dependencies:
  a: a.wasm
  b:
    path: b.wasm
    import: b
```

In the above example, two dependencies are defined in the configuration:

* `a` - the contents of `a.wasm` will be defined directly in the composed
component.
* `b` - a component of the same type as `b.wasm` will be imported in the
composed component with name `b`; the original file will not be embedded in the
composed component.

_Note: importing components from a composed component is not currently
supported in [Wasmtime](https://github.com/bytecodealliance/wasmtime)._

Dependency names should match the expected name of imports from components in
the component graph.

## Instantiations

Each `instantiation` specifies how a particular dependency is to be
instantiated, even allowing a dependency to be instantiated multiple times.

An instantiation has the following fields:

* `dependency` : `string` (optional) - the name of the dependency to
  instantiate.

  If unspecified, the instantiation will be for a dependency of the same name
  as the instantiation itself.

* `arguments` : `map<string, argument>` (optional) - a mapping of argument
  names to arguments.

  Argument names match the names of the imports of the dependency being
  instantiated.

Note that the instantiation name `$input` is special and signifies how the input
component is to be instantiated.

### Instantiation arguments

Each `argument` specifies exactly which instance should be provided as argument
to the instantiation of a dependency.

An argument has the following fields:

* `instance` : `string` - the name of the instance to pass as the argument.

* `export` : `string` (optional) - the name of the exported instance on
  `instance` to use as the argument.

  If not present, the instance specified by `instance` will be passed directly.

Arguments may be specified as a `string` rather than a `map`, in which case
it is treated as having an `instance` field with the value of the string.

## Instantiations example

A slightly complex example of configuring instantiations:

```yaml
instantiations:
  $input:
    arguments:
      a: b
  b:
    arguments:
      c:
        instance: d
        export: e
  d:
    dependency: f
```

In the above example, the `$input` instantiation (i.e. the root instantiation)
has explicitly specified that the argument named `a` is to be provided instance
`b`.

It also defines an instantiation named `b` which is to be passed an instance
export named `e` from instance `d` (of a dependency named `f`) for the
instantiation argument named `c`.

By default, `wasm-compose` will minimize the number of instantiations and
dependencies which share an import of the same name will share the same
instance of that dependency, provided that the instance is compatible with
every import.

Configuring instantiations for `wasm-compose` allows for a custom instantiation
graph to be constructed in the composed component.

## Definition components

A _definition_ component is a component that exports instances that are
automatically used to satisfy any instance import in the composition with a
matching name.

A typical use case for definition components is to provide a _virtualized_
implementation of common interfaces.

For example, a definition component might export a virtualized implementation
of the `wasi:filesystem/types` interface that serves files via inline data
segments instead of by accessing the host filesystem.

By providing such a definition component to the composition, any use of the
WASI filesystem interface by the root component (or its dependencies) will
automatically use the implementation provided by the definition component
instead of importing it from the host environment.
