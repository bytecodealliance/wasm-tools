# wast

A Rust parser for the [WebAssembly Text format (WAT)][wat]

* `wast` - [![Documentation (`wast`)](https://docs.rs/wast/badge.svg)](https://docs.rs/wast)
* `wast-parser` - [![Documentation (`wast-parser`)](https://docs.rs/wast-parser/badge.svg)](https://docs.rs/wast-parser)

[wat]: http://webassembly.github.io/spec/core/text/index.html

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
wast = "1.0"
```

And then you can parse WAT to binary WebAssembly via:

```rust
// Parse from a file ...
let binary = wast::parse_file("./foo.wat")?;

// ... or a string
let wat = r#"
    (module
        (func $foo)

        (func (export "bar")
            call $foo
        )
    )
"#;
let binary = wast::parse_str(wat)?;
```

## Low-level parsing

This repository and project also aims to provide low-level parsing support for
the WAT and WAST formats. Effectively, if you've got an s-expression lookalike
that you'd like to parse, you should be able to parse it!

The `wast` crate does not support this because it strives to provide strong
API-level stability guarantees, but the `wast-parser` crate has all the
low-level details and is the implementation of the `wast` crate. Be sure to
[check out its `README.md`](crates/parser/README.md) for more information.

## Stability and WebAssembly Features

Consult the [crate documentation](https://docs.rs/wast) for more information,
but the general idea is this crate will not issue a semver-breaking change for
breaking changes in the WAT format, either for MVP features or post-MVP
features. No opt-in is required to use WebAssembly features, so using them may
break if the upstream spec changes.

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
