# Configuring `wasm-compose`

By default, `wasm-tools compose` looks for a composition configuration file
named `wasm-compose.toml` or `wasm-compose.yml` in the current directory.

The composition configuration currently has three mappings:

* `components` - maps a friendly name to a component to be instantiated.
* `instantiations` - maps a friendly name to an instantiation of a component.
* `exports` - specifies what gets exported from the resulting composed component.

## Components

Each component specified in the `components` mapping will result in a component
definition (embedding) or a component import in the resulting composed component.

A component has the following fields:

* `path`: string - the path to the WebAssembly component file.
  The path is relative to the configuration file.

* `import`: string (optional) - the name to use for to import the component.
  If unspecified, the component at the given path will be embedded directly in
  the composed component.

An example:

```yaml
components:
  foo:
    path: foo.wasm
  bar:
    path: bar.wasm
    import: bar
```

In the above example, two components are defined in the configuration:

* `foo` - the contents of `foo.wasm` will be embedded directly in the composed
  component as a defined component.
* `bar` - a component of the same type as `bar.wasm` will be imported in the composed
  component with name `bar`; the original file will not be embedded in the composed
  component.

Each component is referred to by its friendly name (e.g. `foo`) for the remainder of
the configuration file.

_Note: importing components from a composed component is not currently supported in_
_[Wasmtime](https://github.com/bytecodealliance/wasmtime)._

## Instantiations

The `instantiations` mapping defines how the components are instantiated in the
composed component.

An instantiation has the following fields:

* `component`: (optional) string - the friendly name of the component to instantiate.
  If unspecified, the instantiation will be for a component of the same name as the
  instantiation itself.

* `with`: (optional) mapping - a mapping of instantiation argument names to
  instantiation names. This is used to explicitly specify the arguments to an
  instantiation.

* `dependencies`: (optional) list of strings - the friendly names of the instances
  that can be used to automatically resolve instantiation arguments that were not
  specified in `with`.

An example:

```yaml
...

instantiations:
  foo: {}
  bar:
    dependencies: [foo]
```

In the above example, there are two instantiations defined in the configuration:

* `foo` - an instantiation of component `foo` with no arguments or dependencies on other
  instances; this assumes that `foo.wasm` has no imported items.

* `bar` - an instantiation of component `bar` using instance `foo` to resolve all
  arguments for the instantiation.

Each instance resulting from an instantiation is referred to by its friendly name (e.g. `foo`)
for the remainder of the configuration file.

Because component `foo` has no imports, we can omit it from the list of instances to have it
implicitly instantiated when referenced from another instantiation:

```yaml
...

instantiations:
  bar:
    dependencies: [foo]
```

The above example is semantically equivalent to the previous example.

By default, an instantiation looks for a component of the same name in the configuration.
This can be overridden by using the `component` field:

```yaml
...

instantiations:
  bar:
    component: foo
```

With the above example, the instance named `bar` instantiates the component named `foo`
instead of component named `bar`.

### Instantiation arguments

To instantiate a component, it must be instantiated with the imports the component
expects.

Each expected import of a component is treated as an "argument" that gets passed another
instance defined in the configuration.

There are two ways of specifying the arguments of an instantiation: _explicit_ and
_implicit_.

#### Explicit instantiation arguments

Instantiation arguments may be explicitly specified using the `with` field when
defining an instantiation:

```yaml
...

instantiations:
  bar:
    with:
      a: foo
```

The example above defines an instantiation named `bar` that will instantiate
component `bar` with instance `foo` as the argument named `a`.

It is an error if component `bar` does not have an import named `a` or if instance
`foo` is incompatible with what component `bar` expects to import.

#### Implicit instantiation arguments

In addition to (or in lieu of) explicitly specifying the arguments of an instantiation,
instantiation arguments may be automatically resolved by `wasm-compose` from a list of
instance dependencies.

The list of instance dependencies can be specified using the `dependencies` field:

```yaml
...

instantiations:
  bar:
    dependencies: [foo]
```

In the above example, the instantiation named `bar` will resolve all of the arguments
using instance `foo`. As in the previous example, if we assume that component `bar`
has a single import named `a` that is compatible with instance `foo`, then
`wasm-compose` will automatically pass instance `foo` as the argument named `a`.

The resolution of implicit arguments is performed using the following steps:

* For each import of the component that does not have an explicit argument
  (i.e. not used in the `with` mapping):
  * For each instance specified in the `dependencies` list:
    * If the import is compatible with the instance, then the instance is passed
      as the argument and it moves on to the next import.
    * If the import is not compatible with the instance, then it moves on
      to the next instance in the `dependencies` list.

It is an error if more than one instance in the `dependencies` list is compatible
with a particular import of the component being instantiated.

If this occurs, an explicit argument must be specified using the `with` field.

### Aliasing exports for instantiations

Currently `wasm-compose` only looks at the direct exports of an instance to resolve
instantiation arguments.

Support for aliasing of instance exports so they can be used in the `with` field or
automatically resolved by `wasm-compose` via the `dependencies` list is forthcoming.

## Exports

The `exports` mapping defines the exports of the composed component.

Currently, it only supports exporting the _default_ interface from a single
instance:

```yaml
...

exports:
  default: foo
```

Here the composed component will export the _default_ interface (i.e. all functions
directly exported) of instance `foo`.

Support for aliasing and exporting non-default interfaces from an instance is
forthcoming.
