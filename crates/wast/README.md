# wast

A Rust parser for the WebAssembly Text format: [WAT][wat] and WAST

[![Documentation](https://docs.rs/wast/badge.svg)](https://docs.rs/wast)

[wat]: http://webassembly.github.io/spec/core/text/index.html

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
wast = "1.0"
```

The intent of this crate is to provide utilities, combinators, and built-in
types to parse anything that looks like a WebAssembly s-expression.

* Need to parse a `*.wat` file?
* Need to parse a `*.wast` file?
* Need to run test suite assertions from the official wasm test suite?
* Want to write an extension do the WebAssembly text format?

If you'd like to do any of the above this crate might be right for you! You may
also want to check out the `wat` crate which provides a much more stable
interface if all you'd like to do is convert `*.wat` to `*.wasm`.

# License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.
