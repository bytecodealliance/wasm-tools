//! A length in the input must not decide how much the decoder allocates.
#![cfg(feature = "decoding")]
#![allow(unsafe_code)]

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

struct Largest;

static LARGEST: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for Largest {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        LARGEST.fetch_max(layout.size(), Ordering::Relaxed);
        unsafe { System.alloc(layout) }
    }
    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        LARGEST.fetch_max(size, Ordering::Relaxed);
        unsafe { System.realloc(ptr, layout, size) }
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { System.dealloc(ptr, layout) }
    }
}

#[global_allocator]
static ALLOCATOR: Largest = Largest;

#[test]
fn a_section_claiming_4_gib_allocates_little() {
    // A component's header, then a custom section claiming nearly 4 GiB.
    let bytes = [
        0x00, 0x61, 0x73, 0x6d, 0x0d, 0x00, 0x01, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0x0f,
    ];
    assert!(wit_parser::decoding::decode(&bytes).is_err());
    let largest = LARGEST.load(Ordering::Relaxed);
    assert!(largest < 1 << 20, "allocated {largest} bytes");
}
