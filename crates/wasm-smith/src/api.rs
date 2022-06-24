//! C API to allow usage from other languages

use std::ffi::CStr;
use std::os::raw::c_char;

use crate::{DefaultConfig, Module};

#[repr(C)]
pub struct WasmSmithModule {
    data: *mut u8,
    size: u32,
}

#[no_mangle]
pub extern "C" fn wasm_smith_create(c_seed: *const c_char, c_output: *mut WasmSmithModule) -> i32 {
    if !c_seed.is_null() && !c_output.is_null() {
        let seed = unsafe { CStr::from_ptr(c_seed) };
        let output = unsafe { c_output.as_mut().unwrap() };

        let mut u = arbitrary::Unstructured::new(seed.to_bytes());

        if let Ok(module) = Module::new(DefaultConfig::default(), &mut u) {
            let mut wasm_buffer = std::mem::ManuallyDrop::new(module.to_bytes());
            output.data = wasm_buffer.as_mut_ptr();
            output.size = wasm_buffer.len() as u32;

            return 0;
        } else {
            output.data = std::ptr::null_mut();

            return -2;
        }
    }

    return -1;
}

#[no_mangle]
pub extern "C" fn wasm_smith_free(c_output: *mut WasmSmithModule) -> i32 {
    if !c_output.is_null() {
        let output = unsafe { c_output.as_mut().unwrap() };
        std::mem::drop(output.data);
    }

    return -1;
}
