# wasmprinter

**A [Bytecode Alliance](https://bytecodealliance.org/) project**

A Rust parser for printing a WebAssembly binary in the [WebAssembly Text format
(WAT)][wat]

[Documentation](https://docs.rs/wasmprinter)

[wat]: http://webassembly.github.io/spec/core/text/index.html

## Usage

This crate is published on crates.io, so you can depend on it with:

```toml
[dependencies]
wasmprinter = "1.0"
```

You can then convert wasm binaries to strings like so:

```rust
fn main() -> Result<()> {
    let foo_wat = wasmprinter::print_file("path/to/foo.wasm")?;

    let binary = /* ... */;
    let wat = wasmprinter::print_bytes(&binary)?;

    // ...
}
```

# License

This project is license under the Apache 2.0 license with the LLVM exception.
See [LICENSE] for more details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.
