# `wasm-shrink`

**A WebAssembly test case shrinker.**

[![](https://docs.rs/wasm-shrink/badge.svg)](https://docs.rs/wasm-shrink/)
[![](https://img.shields.io/crates/v/wasm-shrink.svg)](https://crates.io/crates/wasm-shrink)
[![](https://img.shields.io/crates/d/wasm-shrink.svg)](https://crates.io/crates/wasm-shrink)

* [About](#about)
* [Usage](#usage)
  * [Install](#install)
  * [Writing a Predicate Script](#writing-a-predicate-script)
  * [Run](#run)
* [Embed as a Library](#embed-as-a-library)

## About

`wasm-shrink` is a test case shrinker for WebAssembly. It shrinks a Wasm file
while preserving an interesting property (such as triggering a bug in your Wasm
compiler).

## Usage

### Install

```
$ cargo install --git https://github.com/bytecodealliance/wasm-tools.git
```

### Writing a Predicate Script

The predicate script tells `wasm-shrink` whether a Wasm file is "interesting" or
not, i.e. whether the Wasm file triggers the bug you are trying to isolate.

The interface between `wasm-shrink` and a predicate script is simple:

* The predicate script is given a Wasm file as its first and only argument.

* The predicate script must exit with a zero status code if the Wasm file is
  interesting and a non-zero status code otherwise.

The predicate script must not depend on the current working directory nor that
only one predicate script process is running at any given time.

Here is an example predicate script that could be used to track down a panic in
Wasmtime that includes the panic message "assertion failed: invalid stack map":

```bash
#!/usr/bin/env bash

# Exit the script if any subcommand fails.
set -e

# The Wasm file is given as the first and only argument to the script.
WASM=$1

# Run the Wasm in Wasmtime and `grep` for our target bug's panic
# message.
wasmtime run $WASM 2>&1 | grep --quiet 'assertion failed: invalid stack map'
```

Note that passing `grep` the `--quiet` flag makes it avoid its usual
match-printing behavior and exit with status code zero if there is *any* match
and non-zero if there is *not* any match. This is useful for predicate scripts.

### Run

To run `wasm-shrink` pass it the predicate and the initial test case:

```bash
$ wasm-shrink predicate.sh test-case.wasm -o shrunken.wasm
```

The shrunken Wasm file will be written to `shrunken.wasm` in this case, but if
the `-o` flag is not given an output name is generated based on the initial test
case's name.

You can see all options by passing `--help`:

```bash
$ wasm-shrink --help
```

## Embed as a Library

`wasm-shrink` is not only a command-line tool, it also defines a library to
allow programmatic usage.

First, add a dependency to your `Cargo.toml`:

```toml
[dependencies]
wasm-shrink = "0.1.0"
```

Then use the `wasm_shrink::WasmShrink` builder to configure and run a shrinking
task:

```rust
use wasm_shrink::WasmShrink;

// Get the Wasm you want to shrink from somewhere.
let my_input_wasm: Vec<u8> = todo!();

// Configure the shrinking task.
let shrink = WasmShrink::default()
    // Give up on shrinking after 999 failed attempts to shrink a given
    // Wasm test case any further.
    .attempts(999);

// Run the configured shrinking task.
let info = shrink.run(
    my_input_wasm,

    // Predicate.
    &mut |wasm| {
        let is_interesting: bool = todo!(
            "check for whether the given Wasm is interesting"
        );
        Ok(is_interesting)
    },

    // Callback called each time we find a new smallest interesting
    // Wasm.
    &mut |new_smallest| {
        // Optionally do something with the new smallest Wasm.
        Ok(())
    },
)?;

// Get the shrunken Wasm and other information about the completed shrink
// task from the returned `ShrinkInfo`.
let shrunken_wasm = info.output;
```

For more details, see [the documentation on
`docs.rs`](https://docs.rs/wasm-shrink/).
