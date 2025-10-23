# `wit-dylib-ffi`

A sample Rust crate used to implement `wit_dylib.h` with a Rust trait with the
possibility of an implementation being entirely safe Rust. Currently used in
tests of the `wit-dylib` crate, but written in such a way that this could be
used externally.

## Regenerating `ffi.rs`

If you've made changes to `wit_dylib.h` (or if someone else did, but forgot to
update `ffi.rs`), you can use `rust-bindgen` to regenerate `ffi.rs`.

First, install a recent version of `rust-bindgen` if you don't already have it:

```
cargo install bindgen-cli
```

Then, from the parent of this directory, run:

```
bindgen wit_dylib.h --allowlist-file wit_dylib.h --no-layout-tests --ignore-functions > ./ffi/src/ffi.rs
```
