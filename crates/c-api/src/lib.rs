//! This crate is the implementation of Wasm-tools's C API.
//!
//! This crate is not intended to be used from Rust itself and is typically
//! compiled as a cdylib/staticlib. Documentation for this crate largely lives in the header
//! files of the `include` directory for this crate.

#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]

use arbitrary::{Error, Unstructured};
use wasm_smith::{DefaultConfig, Module};

#[repr(C)]
pub struct wasm_tools_byte_vec_t {
    data: *mut u8,
    size: usize,
}

#[repr(i32)]
pub enum wasm_tools_error {
    WASM_TOOLS_SUCCESS = 0,
    WASM_TOOLS_ERROR = -1,
    WASM_TOOLS_INSUFFICIENT_ENTROPY = -2,
}
use wasm_tools_error::*;

#[no_mangle]
pub extern "C" fn wasm_tools_byte_vec_delete(bytes: &mut wasm_tools_byte_vec_t) {
    unsafe {
        Vec::from_raw_parts(bytes.data, bytes.size, bytes.size);
    }
}

/// Create a new WebAssembly module with the given seed.
///
/// # Safety
///
/// `seed` must be a valid pointer to `seed_len` bytes of memory.
#[no_mangle]
pub unsafe extern "C" fn wasm_smith_create(
    seed: *const u8,
    seed_len: usize,
    bytes: &mut wasm_tools_byte_vec_t,
) -> wasm_tools_error {
    // seed == NULL is acceptable as long as seed_len is zero
    let seed_bytes = if seed_len == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(seed, seed_len) }
    };

    bytes.data = std::ptr::null_mut();
    let mut u = Unstructured::new(seed_bytes);
    match Module::new(DefaultConfig::default(), &mut u) {
        Ok(module) => {
            let mut wasm_buffer = module.to_bytes().into_boxed_slice();
            bytes.data = wasm_buffer.as_mut_ptr();
            bytes.size = wasm_buffer.len();
            std::mem::forget(wasm_buffer);
            WASM_TOOLS_SUCCESS
        }
        Err(Error::NotEnoughData) => WASM_TOOLS_INSUFFICIENT_ENTROPY,
        Err(_e) => WASM_TOOLS_ERROR,
    }
}
