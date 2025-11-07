//! A helper crate for providing a more ergonomic and safe Rust API over the
//! `wit_dylib.h` interface.
//!
//! The `ffi` module in this crate is the raw `bindgen`-generated Rust bindings
//! for the C header file. The rest of the crate is the built on top of that.
//!
//! A `test_util` module provides a sample implementation of an "interpreter"
//! which is used for test cases here and can also be a possibly-helpful
//! reference to an implementation.

#![allow(unsafe_code)]
#![allow(clippy::allow_attributes_without_reason)]

#[cfg(all(feature = "async-runtime", feature = "async-raw"))]
compile_error!("cannot enable both `async-runtime` and `async-raw` features");

use std::alloc::Layout;
use std::ptr;

/// Macro to specify a struct name which implements the `Interpreter` trait.
///
/// This will export various symbols for interpreter intrinsics which will call
/// through the trait methods here.
#[macro_export]
macro_rules! export {
    ($name:ident) => {
        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_initialize(ptr: *const u8) {
            unsafe { <$name as $crate::RawInterpreter>::raw_initialize(ptr) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_dealloc_bytes(
            cx: *mut u8,
            ptr: *mut u8,
            byte_size: usize,
            align: usize,
            defer: bool,
        ) {
            unsafe {
                <$name as $crate::RawInterpreter>::raw_dealloc_bytes(
                    cx, ptr, byte_size, align, defer,
                )
            }
        }

        #[no_mangle]
        pub extern "C" fn wit_dylib_export_start(which: usize) -> *mut u8 {
            unsafe { <$name as $crate::RawInterpreter>::raw_export_start(which) }
        }

        #[no_mangle]
        pub extern "C" fn wit_dylib_export_call(cx: *mut u8, which: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_export_call(cx, which) }
        }

        #[no_mangle]
        pub extern "C" fn wit_dylib_export_async_call(cx: *mut u8, which: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_export_async_call(cx, which) }
        }

        #[no_mangle]
        pub extern "C" fn wit_dylib_export_async_callback(
            a: u32,
            b: u32,
            c: u32,
            which: usize,
        ) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_export_async_callback(a, b, c, which) }
        }

        #[no_mangle]
        pub extern "C" fn wit_dylib_export_finish(cx: *mut u8, which: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_export_finish(cx, which) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_resource_dtor(ty: usize, handle: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_resource_dtor(ty, handle) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_u8(cx: *mut u8) -> u8 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_u8(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_u8(cx: *mut u8, val: u8) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_u8(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_s8(cx: *mut u8) -> i8 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_s8(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_s8(cx: *mut u8, val: i8) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_s8(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_u16(cx: *mut u8) -> u16 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_u16(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_u16(cx: *mut u8, val: u16) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_u16(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_s16(cx: *mut u8) -> i16 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_s16(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_s16(cx: *mut u8, val: i16) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_s16(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_u32(cx: *mut u8) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_u32(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_u32(cx: *mut u8, val: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_u32(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_s32(cx: *mut u8) -> i32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_s32(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_s32(cx: *mut u8, val: i32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_s32(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_u64(cx: *mut u8) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_u64(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_u64(cx: *mut u8, val: u64) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_u64(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_s64(cx: *mut u8) -> i64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_s64(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_s64(cx: *mut u8, val: i64) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_s64(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_bool(cx: *mut u8) -> bool {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_bool(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_bool(cx: *mut u8, val: bool) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_bool(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_char(cx: *mut u8) -> char {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_char(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_char(cx: *mut u8, val: char) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_char(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_f32(cx: *mut u8) -> f32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_f32(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_f32(cx: *mut u8, val: f32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_f32(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_f64(cx: *mut u8) -> f64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_f64(cx) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_f64(cx: *mut u8, val: f64) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_f64(cx, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_enum(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_enum(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_enum(cx: *mut u8, ty: usize, val: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_enum(cx, ty, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_flags(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_flags(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_flags(cx: *mut u8, ty: usize, val: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_flags(cx, ty, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_borrow(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_borrow(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_borrow(cx: *mut u8, ty: usize, val: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_borrow(cx, ty, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_own(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_own(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_own(cx: *mut u8, ty: usize, val: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_own(cx, ty, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_future(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_future(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_future(cx: *mut u8, ty: usize, val: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_future(cx, ty, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_stream(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_stream(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_stream(cx: *mut u8, ty: usize, val: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_stream(cx, ty, val) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_record(cx: *mut u8, ty: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_record(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_record(cx: *mut u8, ty: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_record(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_tuple(cx: *mut u8, ty: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_tuple(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_tuple(cx: *mut u8, ty: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_tuple(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_option(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_option(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_option(cx: *mut u8, ty: usize, discr: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_option(cx, ty, discr) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_result(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_result(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_result(cx: *mut u8, ty: usize, discr: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_result(cx, ty, discr) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_variant(cx: *mut u8, ty: usize) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_variant(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_variant(cx: *mut u8, ty: usize, discr: u32) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_variant(cx, ty, discr) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_string(cx: *mut u8, ptr: &mut *const u8) -> usize {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_string(cx, ptr) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_string(cx: *mut u8, ptr: *mut u8, len: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_string(cx, ptr, len) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_list(
            cx: *mut u8,
            ty: usize,
            ptr: &mut *const u8,
        ) -> usize {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_list(cx, ty, ptr) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_iter_next(cx: *mut u8, ty: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_iter_next(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_pop_iter(cx: *mut u8, ty: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_pop_iter(cx, ty) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_push_list(
            cx: *mut u8,
            ty: usize,
            ptr: *mut u8,
            len: usize,
        ) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_push_list(cx, ty, ptr, len) }
        }

        #[no_mangle]
        pub unsafe extern "C" fn wit_dylib_list_append(cx: *mut u8, ty: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_list_append(cx, ty) }
        }

        fn main() {
            unreachable!();
        }
    };
}

/// # Safety
///
/// `Self::Borrow` and `Self::Own` must have same in-memory representation and
/// generally represent correct ownership semantics.
pub trait Interpreter: 'static {
    type CallCx<'a>: Call;

    /// Startup hook if necessary.
    fn initialize(wit: Wit) {
        let _ = wit;
    }

    fn export_start<'a>(wit: Wit, func: ExportFunction) -> Box<Self::CallCx<'a>>;

    fn export_call(wit: Wit, func: ExportFunction, cx: &mut Self::CallCx<'_>);

    #[cfg(feature = "async-runtime")]
    fn export_call_async(
        wit: Wit,
        func: ExportFunction,
        cx: Box<Self::CallCx<'static>>,
    ) -> impl std::future::Future<Output = ()>;

    #[cfg(feature = "async-raw")]
    fn export_async_start(wit: Wit, func: ExportFunction, cx: Box<Self::CallCx<'static>>) -> u32;

    #[cfg(feature = "async-raw")]
    fn export_async_callback(event0: u32, event1: u32, event2: u32) -> u32;

    fn export_finish(cx: Box<Self::CallCx<'_>>, func: ExportFunction) {
        let _ = func;
        let _ = cx;
    }

    fn import_call(wit: Wit, interface: Option<&str>, func: &str, cx: &mut Self::CallCx<'_>) {
        let func = wit.unwrap_import(interface, func);
        func.call_import_sync(cx)
    }

    fn resource_dtor(ty: Resource, handle: usize);
}

pub trait Call {
    unsafe fn defer_deallocate(&mut self, ptr: *mut u8, layout: Layout);

    fn pop_u8(&mut self) -> u8;
    fn pop_u16(&mut self) -> u16;
    fn pop_u32(&mut self) -> u32;
    fn pop_u64(&mut self) -> u64;
    fn pop_s8(&mut self) -> i8;
    fn pop_s16(&mut self) -> i16;
    fn pop_s32(&mut self) -> i32;
    fn pop_s64(&mut self) -> i64;
    fn pop_bool(&mut self) -> bool;
    fn pop_char(&mut self) -> char;
    fn pop_f32(&mut self) -> f32;
    fn pop_f64(&mut self) -> f64;
    fn pop_string(&mut self) -> &str;
    fn pop_borrow(&mut self, ty: Resource) -> u32;
    fn pop_own(&mut self, ty: Resource) -> u32;
    fn pop_enum(&mut self, ty: Enum) -> u32;
    fn pop_flags(&mut self, ty: Flags) -> u32;
    fn pop_future(&mut self, ty: Future) -> u32;
    fn pop_stream(&mut self, ty: Stream) -> u32;
    fn pop_option(&mut self, ty: WitOption) -> u32;
    fn pop_result(&mut self, ty: WitResult) -> u32;
    fn pop_variant(&mut self, ty: Variant) -> u32;
    fn pop_record(&mut self, ty: Record);
    fn pop_tuple(&mut self, ty: Tuple);

    unsafe fn maybe_pop_list(&mut self, ty: List) -> Option<(*const u8, usize)> {
        let _ = ty;
        None
    }
    fn pop_list(&mut self, ty: List) -> usize;
    fn pop_iter_next(&mut self, ty: List);
    fn pop_iter(&mut self, ty: List);

    fn push_bool(&mut self, val: bool);
    fn push_char(&mut self, val: char);
    fn push_u8(&mut self, val: u8);
    fn push_s8(&mut self, val: i8);
    fn push_u16(&mut self, val: u16);
    fn push_s16(&mut self, val: i16);
    fn push_u32(&mut self, val: u32);
    fn push_s32(&mut self, val: i32);
    fn push_u64(&mut self, val: u64);
    fn push_s64(&mut self, val: i64);
    fn push_f32(&mut self, val: f32);
    fn push_f64(&mut self, val: f64);
    fn push_string(&mut self, val: String);
    fn push_record(&mut self, ty: Record);
    fn push_tuple(&mut self, ty: Tuple);
    fn push_flags(&mut self, ty: Flags, bits: u32);
    fn push_enum(&mut self, ty: Enum, discr: u32);
    fn push_borrow(&mut self, ty: Resource, handle: u32);
    fn push_own(&mut self, ty: Resource, handle: u32);
    fn push_future(&mut self, ty: Future, handle: u32);
    fn push_stream(&mut self, ty: Stream, handle: u32);
    fn push_variant(&mut self, ty: Variant, discr: u32);
    fn push_option(&mut self, ty: WitOption, is_some: bool);
    fn push_result(&mut self, ty: WitResult, is_err: bool);
    unsafe fn push_raw_list(&mut self, ty: List, ptr: *mut u8, len: usize) -> bool {
        let _ = (ty, ptr, len);
        false
    }
    fn push_list(&mut self, ty: List, capacity: usize);
    fn list_append(&mut self, ty: List);
}

static mut WIT_T: *const ffi::wit_t = ptr::null_mut();

macro_rules! debug_println {
    ($($t:tt)*) => (
        if false {
            eprintln!($($t)*);
        }
    )
}

#[doc(hidden)]
pub trait RawInterpreter: Interpreter {
    unsafe fn raw_initialize(ptr: *const u8) {
        debug_println!("initialize({ptr:?})");
        let ptr = ptr.cast::<ffi::wit_t>();
        unsafe {
            assert!(WIT_T.is_null());
            WIT_T = ptr;
            Self::initialize(Wit::from_raw(ptr));
        }
    }

    unsafe fn raw_dealloc_bytes(
        cx: *mut u8,
        ptr: *mut u8,
        byte_size: usize,
        align: usize,
        defer: bool,
    ) {
        debug_println!("dealloc_bytes({cx:?}, {ptr:?}, {byte_size:#x}, {align:#x}, {defer})");
        unsafe {
            let layout = std::alloc::Layout::from_size_align(byte_size, align).unwrap();
            if defer {
                Self::cx_mut(cx).defer_deallocate(ptr, layout);
            } else {
                std::alloc::dealloc(ptr, layout);
            }
        }
    }

    unsafe fn raw_export_start(which: usize) -> *mut u8 {
        debug_println!("export_start({which})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let func = wit.export_func(which);
            Box::into_raw(Self::export_start(wit, func)).cast()
        }
    }

    unsafe fn raw_export_call(cx: *mut u8, which: usize) {
        debug_println!("export_call({cx:?}, {which})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let func = wit.export_func(which);
            Self::export_call(wit, func, Self::cx_mut(cx))
        }
    }

    unsafe fn raw_export_async_call(cx: *mut u8, which: usize) -> u32 {
        debug_println!("export_async_call({cx:?}, {which})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let func = wit.export_func(which);
            #[cfg(feature = "async-runtime")]
            {
                wit_bindgen::rt::async_support::start_task(Self::export_call_async(
                    wit,
                    func,
                    Box::from_raw(cx.cast()),
                ))
                .cast_unsigned()
            }
            #[cfg(feature = "async-raw")]
            {
                Self::export_async_start(wit, func, Box::from_raw(cx.cast()))
            }
            #[cfg(not(any(feature = "async-runtime", feature = "async-raw")))]
            {
                _ = func;
                panic!("no async features enabled")
            }
        }
    }

    unsafe fn raw_export_async_callback(a: u32, b: u32, c: u32, which: usize) -> u32 {
        debug_println!("export_async_callback({a:#x}, {b:#x}, {c:#x}, {which})");
        #[cfg(feature = "async-runtime")]
        unsafe {
            wit_bindgen::rt::async_support::callback(a, b, c)
        }
        #[cfg(feature = "async-raw")]
        {
            Self::export_async_callback(a, b, c)
        }
        #[cfg(not(any(feature = "async-runtime", feature = "async-raw")))]
        {
            panic!("no async features enabled")
        }
    }

    unsafe fn raw_export_finish(cx: *mut u8, which: usize) {
        debug_println!("export_finish({cx:?}, {which})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let func = wit.export_func(which);
            Self::export_finish(Box::from_raw(cx.cast()), func)
        }
    }

    unsafe fn raw_resource_dtor(ty: usize, handle: usize) {
        debug_println!("resource_dtor({ty}, {handle:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::resource_dtor(wit.resource(ty), handle);
        }
    }

    unsafe fn cx_mut<'a, 'b>(ptr: *mut u8) -> &'a mut Self::CallCx<'b> {
        unsafe { &mut *ptr.cast() }
    }

    unsafe fn raw_pop_u8(cx: *mut u8) -> u8 {
        debug_println!("pop_u8({cx:?})");
        unsafe { Self::cx_mut(cx).pop_u8() }
    }

    unsafe fn raw_pop_u16(cx: *mut u8) -> u16 {
        debug_println!("pop_u16({cx:?})");
        unsafe { Self::cx_mut(cx).pop_u16() }
    }

    unsafe fn raw_pop_u32(cx: *mut u8) -> u32 {
        debug_println!("pop_u32({cx:?})");
        unsafe { Self::cx_mut(cx).pop_u32() }
    }

    unsafe fn raw_pop_u64(cx: *mut u8) -> u64 {
        debug_println!("pop_u64({cx:?})");
        unsafe { Self::cx_mut(cx).pop_u64() }
    }

    unsafe fn raw_pop_s8(cx: *mut u8) -> i8 {
        debug_println!("pop_s8({cx:?})");
        unsafe { Self::cx_mut(cx).pop_s8() }
    }

    unsafe fn raw_pop_s16(cx: *mut u8) -> i16 {
        debug_println!("pop_s16({cx:?})");
        unsafe { Self::cx_mut(cx).pop_s16() }
    }

    unsafe fn raw_pop_s32(cx: *mut u8) -> i32 {
        debug_println!("pop_s32({cx:?})");
        unsafe { Self::cx_mut(cx).pop_s32() }
    }

    unsafe fn raw_pop_s64(cx: *mut u8) -> i64 {
        debug_println!("pop_s64({cx:?})");
        unsafe { Self::cx_mut(cx).pop_s64() }
    }

    unsafe fn raw_pop_f32(cx: *mut u8) -> f32 {
        debug_println!("pop_f32({cx:?})");
        unsafe { Self::cx_mut(cx).pop_f32() }
    }

    unsafe fn raw_pop_f64(cx: *mut u8) -> f64 {
        debug_println!("pop_f64({cx:?})");
        unsafe { Self::cx_mut(cx).pop_f64() }
    }

    unsafe fn raw_pop_bool(cx: *mut u8) -> bool {
        debug_println!("pop_bool({cx:?})");
        unsafe { Self::cx_mut(cx).pop_bool() }
    }

    unsafe fn raw_pop_char(cx: *mut u8) -> char {
        debug_println!("pop_char({cx:?})");
        unsafe { Self::cx_mut(cx).pop_char() }
    }

    unsafe fn raw_pop_borrow(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_borrow({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_borrow(wit.resource(ty))
        }
    }

    unsafe fn raw_pop_own(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_own({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_own(wit.resource(ty))
        }
    }

    unsafe fn raw_pop_flags(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_flags({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_flags(wit.flags(ty))
        }
    }

    unsafe fn raw_pop_enum(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_enum({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_enum(wit.enum_(ty))
        }
    }

    unsafe fn raw_pop_future(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_future({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_future(wit.future(ty))
        }
    }

    unsafe fn raw_pop_stream(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_stream({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_stream(wit.stream(ty))
        }
    }

    unsafe fn raw_pop_record(cx: *mut u8, ty: usize) {
        debug_println!("pop_record({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_record(wit.record(ty))
        }
    }

    unsafe fn raw_pop_tuple(cx: *mut u8, ty: usize) {
        debug_println!("pop_tuple({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_tuple(wit.tuple(ty))
        }
    }

    unsafe fn raw_pop_string(cx: *mut u8, ptr: &mut *const u8) -> usize {
        debug_println!("pop_string({cx:?})");
        let s = unsafe { Self::cx_mut(cx).pop_string() };
        *ptr = s.as_ptr();
        s.len()
    }

    unsafe fn raw_pop_option(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_option({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_option(wit.option(ty))
        }
    }

    unsafe fn raw_pop_result(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_result({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_result(wit.result(ty))
        }
    }

    unsafe fn raw_pop_variant(cx: *mut u8, ty: usize) -> u32 {
        debug_println!("pop_variant({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_variant(wit.variant(ty))
        }
    }

    unsafe fn raw_pop_list(cx: *mut u8, ty: usize, retptr: &mut *const u8) -> usize {
        debug_println!("pop_list({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let cx = Self::cx_mut(cx);
            let ty = wit.list(ty);
            match cx.maybe_pop_list(ty) {
                Some((ptr, len)) => {
                    *retptr = ptr;
                    len
                }
                None => {
                    *retptr = ptr::null();
                    cx.pop_list(ty)
                }
            }
        }
    }

    unsafe fn raw_pop_iter_next(cx: *mut u8, ty: usize) {
        debug_println!("pop_iter_next({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_iter_next(wit.list(ty))
        }
    }

    unsafe fn raw_pop_iter(cx: *mut u8, ty: usize) {
        debug_println!("pop_iter({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).pop_iter(wit.list(ty))
        }
    }

    unsafe fn raw_push_bool(cx: *mut u8, val: bool) {
        debug_println!("push_bool({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_bool(val) }
    }

    unsafe fn raw_push_char(cx: *mut u8, val: char) {
        debug_println!("push_char({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_char(val) }
    }

    unsafe fn raw_push_u8(cx: *mut u8, val: u8) {
        debug_println!("push_u8({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_u8(val) }
    }

    unsafe fn raw_push_s8(cx: *mut u8, val: i8) {
        debug_println!("push_s8({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_s8(val) }
    }

    unsafe fn raw_push_u16(cx: *mut u8, val: u16) {
        debug_println!("push_u16({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_u16(val) }
    }

    unsafe fn raw_push_s16(cx: *mut u8, val: i16) {
        debug_println!("push_s16({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_s16(val) }
    }

    unsafe fn raw_push_u32(cx: *mut u8, val: u32) {
        debug_println!("push_u32({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_u32(val) }
    }

    unsafe fn raw_push_s32(cx: *mut u8, val: i32) {
        debug_println!("push_s32({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_s32(val) }
    }

    unsafe fn raw_push_u64(cx: *mut u8, val: u64) {
        debug_println!("push_u64({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_u64(val) }
    }

    unsafe fn raw_push_s64(cx: *mut u8, val: i64) {
        debug_println!("push_s64({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_s64(val) }
    }

    unsafe fn raw_push_f32(cx: *mut u8, val: f32) {
        debug_println!("push_f32({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_f32(val) }
    }

    unsafe fn raw_push_f64(cx: *mut u8, val: f64) {
        debug_println!("push_f64({cx:?}, {val})");
        unsafe { Self::cx_mut(cx).push_f64(val) }
    }

    unsafe fn raw_push_string(cx: *mut u8, ptr: *mut u8, len: usize) {
        debug_println!("push_string({cx:?}, {ptr:?}, {len})");
        unsafe {
            let bytes = Vec::from_raw_parts(ptr, len, len);
            let string = String::from_utf8_unchecked(bytes);
            Self::cx_mut(cx).push_string(string)
        }
    }

    unsafe fn raw_push_record(cx: *mut u8, ty: usize) {
        debug_println!("push_record({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_record(wit.record(ty))
        }
    }

    unsafe fn raw_push_tuple(cx: *mut u8, ty: usize) {
        debug_println!("push_tuple({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_tuple(wit.tuple(ty))
        }
    }

    unsafe fn raw_push_flags(cx: *mut u8, ty: usize, flags: u32) {
        debug_println!("push_flags({cx:?}, {ty}, {flags:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_flags(wit.flags(ty), flags)
        }
    }

    unsafe fn raw_push_enum(cx: *mut u8, ty: usize, enum_: u32) {
        debug_println!("push_enum({cx:?}, {ty}, {enum_:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_enum(wit.enum_(ty), enum_)
        }
    }

    unsafe fn raw_push_borrow(cx: *mut u8, ty: usize, borrow: u32) {
        debug_println!("push_borrow({cx:?}, {ty}, {borrow:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_borrow(wit.resource(ty), borrow)
        }
    }

    unsafe fn raw_push_own(cx: *mut u8, ty: usize, own: u32) {
        debug_println!("push_own({cx:?}, {ty}, {own:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_own(wit.resource(ty), own)
        }
    }

    unsafe fn raw_push_future(cx: *mut u8, ty: usize, future: u32) {
        debug_println!("push_future({cx:?}, {ty}, {future:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_future(wit.future(ty), future)
        }
    }

    unsafe fn raw_push_stream(cx: *mut u8, ty: usize, stream: u32) {
        debug_println!("push_stream({cx:?}, {ty}, {stream:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_stream(wit.stream(ty), stream)
        }
    }

    unsafe fn raw_push_option(cx: *mut u8, ty: usize, discr: u32) {
        debug_println!("push_option({cx:?}, {ty}, {discr:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_option(wit.option(ty), discr != 0)
        }
    }

    unsafe fn raw_push_result(cx: *mut u8, ty: usize, discr: u32) {
        debug_println!("push_result({cx:?}, {ty}, {discr:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_result(wit.result(ty), discr != 0)
        }
    }

    unsafe fn raw_push_variant(cx: *mut u8, ty: usize, discr: u32) {
        debug_println!("push_variant({cx:?}, {ty}, {discr:#x})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).push_variant(wit.variant(ty), discr)
        }
    }

    unsafe fn raw_push_list(cx: *mut u8, ty: usize, list: *mut u8, len: usize) -> u32 {
        debug_println!("push_list({cx:?}, {ty}, {list:?}, {len})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let cx = Self::cx_mut(cx);
            let ty = wit.list(ty);
            if cx.push_raw_list(ty, list, len) {
                1
            } else {
                cx.push_list(ty, len);
                0
            }
        }
    }

    unsafe fn raw_list_append(cx: *mut u8, ty: usize) {
        debug_println!("list_append({cx:?}, {ty})");
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::cx_mut(cx).list_append(wit.list(ty))
        }
    }
}

impl<T: Interpreter + ?Sized> RawInterpreter for T {}

#[allow(dead_code, non_camel_case_types)]
mod ffi;
mod types;

pub use self::types::*;
