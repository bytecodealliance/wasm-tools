# `wit-dylib`

**A [dynamic library][dylib] generator for a [WIT world][world].**

## What is this?

The `wit-dylib` crate, and `wasm-tools wit-dylib` subcommand, generate a
WebAssembly [shared-everything dynamic library][dylib] from a [WIT
world][world] which interacts and is implemented with a C header file
[`wit_dylib.h`]. Crucially this translates any possible WIT interface through a
single [`wit_dylib.h`] interface. This enables creating a component that is
suitable for implementing and using an WIT interface.

The primary use case in mind for this crate is turning interpreted languages
into a WebAssembly component. This crate enables decoupling the interpreter and
the end component's WIT world, which yields benefits such as:

* An interpreter is independent of the target WIT world chosen by a user.
* Interpreters can provide builds and users can then take these builds and turn
  them into any component. Users no longer need to build custom interpreters
  themselves.
* Engines can share code between components which use the same interpreter since
  it's the same interpreter binary embedded in each component.
* Interpreters can give content access to WIT worlds that weren't known at
  compile-time for the interpreter. Effectively content can access any arbitrary
  API defined in WIT.

The [`wit_dylib.h`] is the only requirement that interpreters need implement,
and it's expected that this can be done as a "shim" around existing embedding
APIs that interpreters already have.

## Usage

This tool is agnostic to the actual interpreter that it is integrated with, and
this is intended to be a low-level implementation detail of componentizing
interpreters. Additionally this is just one piece of the puzzle to
componentizing interpreters. Nevertheless integration would probably look
something like this:

1. The interpreter in question is built with Clang's `-shared` flag. This
   outputs a `*.so` WebAssembly binary.

2. This crate is used with a WIT world to generate a `world.so`, for example.
   Execution would be `wasm-tools wit-dylib ./wit -o world.so`.

3. The two libraries are linked together with `wasm-tools component link
   interpreter.so world.so -o component.wasm`. This probably also needs:

   * `libc.so` from `$WASI_SDK_PATH/share/wasi-sysroot/lib/wasm32-wasip2/libc.so`
   * `--adapt` with [`wasi_snapshot_preview1.reactor.wasm`]

The output component's world will be a union of the WIT provided in step (2) as
well as the inherent world required by `interpreter.so` and `libc.so` (e.g.
what's needed by `wasi-libc`).

## Example

An example of using this crate is in `crates/wit-dylib/test-programs/src/*.rs`.
This is used for testing in this repository but is written in such a way that it
should be general enough for external use. Specifically if you were implementing
the interpreter in Rust you'd implement the `Interpreter` trait and use the
`export!` macro in your interpreter build.

## Testing

Testing this crate is done with:

```
cargo test -p wit-dylib
```

Make sure you have the `wasmtime` CLI executable installed.

Tests are organized as `crates/wit-dylib/test-programs/src/bin/${name}{.wit,_caller.rs,_callee.rs}`
where the WIT describes an interface between the caller/callee and the two are
composed together to execute the test case.

[dylib]: https://github.com/WebAssembly/tool-conventions/blob/main/DynamicLinking.md
[world]: https://component-model.bytecodealliance.org/design/worlds.html
[`wit_dylib.h`]: ./wit_dylib.h
[`wasi_snapshot_preview1.reactor.wasm`]: https://github.com/bytecodealliance/wasmtime/releases/latest
