//! The WebAssembly component tooling.

#![deny(missing_docs)]

use anyhow::{bail, Result};
use std::fmt::Display;
use std::str::FromStr;
use wasm_encoder::CanonicalOption;

mod decoding;
mod encoding;
mod gc;
mod printing;
mod validation;

pub use decoding::decode_world;
pub use encoding::*;
pub use printing::*;

pub mod metadata;

/// Supported string encoding formats.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum StringEncoding {
    /// Strings are encoded with UTF-8.
    UTF8,
    /// Strings are encoded with UTF-16.
    UTF16,
    /// Strings are encoded with compact UTF-16 (i.e. Latin1+UTF-16).
    CompactUTF16,
}

impl Default for StringEncoding {
    fn default() -> Self {
        StringEncoding::UTF8
    }
}

impl Display for StringEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringEncoding::UTF8 => write!(f, "utf8"),
            StringEncoding::UTF16 => write!(f, "utf16"),
            StringEncoding::CompactUTF16 => write!(f, "compact-utf16"),
        }
    }
}

impl FromStr for StringEncoding {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "utf8" => Ok(StringEncoding::UTF8),
            "utf16" => Ok(StringEncoding::UTF16),
            "compact-utf16" => Ok(StringEncoding::CompactUTF16),
            _ => bail!("unknown string encoding `{}`", s),
        }
    }
}

impl From<StringEncoding> for wasm_encoder::CanonicalOption {
    fn from(e: StringEncoding) -> wasm_encoder::CanonicalOption {
        match e {
            StringEncoding::UTF8 => CanonicalOption::UTF8,
            StringEncoding::UTF16 => CanonicalOption::UTF16,
            StringEncoding::CompactUTF16 => CanonicalOption::CompactUTF16,
        }
    }
}
