# Playground

This is a simple online playground for `wasm-tools parse` and `print`, available at https://bytecodealliance.github.io/wasm-tools/parse / https://bytecodealliance.github.io/wasm-tools/print respectively.

## Building

In addition to `cargo`, you'll need to have `node` and [`cargo-component`](https://github.com/bytecodealliance/cargo-Component) installed and on your path.

Then

```sh
npm ci
```
to install dependencies and
```sh
npm run build
```
to build.

Output will be placed under `dist`.

## Making changes

The [./component/wit/world.wit](./component/wit/world.wit) file defines the exported functions for use by the playground. After changing it you'll need to `npm run build` to regenerate the bindings file (the command will fail but still generate the bindings) and then implement the functions in [./component/src/lib.rs](./component/src/lib.rs).

After `npm run build`ing again you can make use of those functions from [./src/worker.ts](./src/worker.ts), with full type safety (thanks to [`jco`](https://github.com/bytecodealliance/jco)).

It is also possible to use them outside of a worker, but since they run synchronously and block their thread the playground is designed to do all calls off the main thread.

To preview your changes after building you'll need to run a web server; the worker will not load from a `file:` URL. The simplest way to do this is by running `npx http-server dist`, but you can use whatever means you'd like.
