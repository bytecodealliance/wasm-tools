//! Web Assembly Value Encoding
//!
//! WAVE is a human-oriented text encoding of WebAssembly Component Model values.
//!
//! For more information, see the [README](https://github.com/lann/wasm-wave#readme).
#![deny(missing_docs)]

pub mod ast;
pub mod lex;
pub mod parser;
mod strings;
pub mod untyped;
pub mod value;
pub mod wasm;
pub mod writer;

use parser::Parser;
use wasm::WasmValue;
use writer::Writer;

/// Parses a [`WasmValue`] from the given WAVE-encoded string.
/// ```
/// use wasmtime::component::{Type, Val};
/// # fn main() -> Result<(), wasm_wave::parser::ParserError> {
/// let val: Val = wasm_wave::from_str(&Type::Char, r"'\u{1F44B}'")?;
/// assert_eq!(val, Val::Char('👋'));
/// # Ok(())
/// # }
/// ```
pub fn from_str<V: WasmValue>(ty: &V::Type, s: &str) -> Result<V, parser::ParserError> {
    let mut parser = Parser::new(s);

    let value = parser.parse_value(ty)?;

    // Ensure that we've parsed the entire string.
    parser.finish()?;

    Ok(value)
}

/// Returns the given [`WasmValue`] as a WAVE-encoded string.
/// ```
/// use wasmtime::component::Val;
/// # fn main() -> Result<(), wasm_wave::writer::WriterError> {
/// let wave_str = wasm_wave::to_string(&Val::Char('\u{1F44B}'))?;
/// assert_eq!(wave_str, "'👋'");
/// # Ok(())
/// # }
pub fn to_string(val: &impl WasmValue) -> Result<String, writer::WriterError> {
    let mut buf = vec![];
    Writer::new(&mut buf).write_value(val)?;
    Ok(String::from_utf8(buf).unwrap_or_else(|err| panic!("invalid UTF-8: {err:?}")))
}

fn canonicalize_nan32(val: f32) -> f32 {
    if val.is_nan() {
        f32::from_bits(0x7fc00000)
    } else {
        val
    }
}

fn canonicalize_nan64(val: f64) -> f64 {
    if val.is_nan() {
        f64::from_bits(0x7ff8000000000000)
    } else {
        val
    }
}
