//! A helper crate for providing a more ergonomic and safe Rust API over the
//! `wit_dylib.h` interface.
//!
//! The `ffi` module in this crate is the raw `bindgen`-generated Rust bindings
//! for the C header file. The rest of the crate is the built on top of that.
//!
//! A `test_util` module provides a sample implementation of an "interpreter"
//! which is used for test cases here and can also be a possibly-helpful
//! reference to an implementation.

#![allow(unsafe_code, reason = "not like the rest of wasm-tools")]

use std::mem::ManuallyDrop;
use std::ptr;

/// Macro to specify a struct name which implements the `Interpreter` trait.
///
/// This will export various symbols for interpreter intrinsics which will call
/// through the trait methods here.
#[macro_export]
macro_rules! export {
    ($name:ident) => {
        const _: () = {
            type Borrow = <$name as $crate::Interpreter>::Borrow<'static>;
            type Own = <$name as $crate::Interpreter>::Own;

            // Both `Borrow` and `Own` types should fit in the 64-bits that
            // we're provided for value slots.
            assert!(size_of::<u64>() >= size_of::<Borrow>());
            assert!(align_of::<u64>() >= align_of::<Borrow>());
            assert!(size_of::<u64>() >= size_of::<Own>());
            assert!(align_of::<u64>() >= align_of::<Own>());

            // `Borrow` is not managed in such a way at this time where
            // destructors are properly run, so assert that it doesn't require
            // one.
            assert!(!std::mem::needs_drop::<Borrow>());
        };

        #[unsafe(no_mangle)]
        pub extern "C" fn wit_dylib_call_export(which: usize, ptr: *mut u64) {
            unsafe { <$name as $crate::RawInterpreter>::raw_call_export(which, ptr) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_initialize(ptr: *const u8) {
            unsafe { <$name as $crate::RawInterpreter>::raw_initialize(ptr) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_u8(val: u64) -> u8 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_u8(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_u16(val: u64) -> u16 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_u16(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_u32(val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_u32(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_u64(val: u64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_u64(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_s8(val: u64) -> i8 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_s8(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_s16(val: u64) -> i16 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_s16(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_s32(val: u64) -> i32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_s32(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_s64(val: u64) -> i64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_s64(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_f32(val: u64) -> f32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_f32(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_f64(val: u64) -> f64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_f64(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_bool(val: u64) -> bool {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_bool(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_char(val: u64) -> char {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_char(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_borrow(ty: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_borrow(ty, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_own(ty: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_own(ty, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_enum(ty: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_enum(ty, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_flags(ty: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_flags(ty, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_future(ty: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_future(ty, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_stream(ty: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_stream(ty, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_record(ty: usize, val: u64, fields: *mut u64) {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_record(ty, val, fields) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lower_tuple(ty: usize, val: u64, fields: *mut u64) {
            unsafe { <$name as $crate::RawInterpreter>::raw_lower_tuple(ty, val, fields) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_string_ptr(val: u64) -> *const u8 {
            unsafe { <$name as $crate::RawInterpreter>::raw_string_ptr(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_string_len(val: u64) -> usize {
            unsafe { <$name as $crate::RawInterpreter>::raw_string_len(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_list_len(type_index: usize, val: u64) -> usize {
            unsafe { <$name as $crate::RawInterpreter>::raw_list_len(type_index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_list_ptr(type_index: usize, val: u64) -> *const u8 {
            unsafe { <$name as $crate::RawInterpreter>::raw_list_ptr(type_index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_list_get(
            type_index: usize,
            index: usize,
            val: u64,
        ) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_list_get(type_index, index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_option_is_some(type_index: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_option_is_some(type_index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_option_payload(type_index: usize, val: u64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_option_payload(type_index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_result_is_err(type_index: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_result_is_err(type_index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_result_payload(type_index: usize, val: u64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_result_payload(type_index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_variant_discr(type_index: usize, val: u64) -> u32 {
            unsafe { <$name as $crate::RawInterpreter>::raw_variant_discr(type_index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_variant_payload(type_index: usize, val: u64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_variant_payload(type_index, val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_dealloc_bytes(
            ptr: *mut u8,
            byte_size: usize,
            align: usize,
        ) {
            unsafe { <$name as $crate::RawInterpreter>::raw_dealloc_bytes(ptr, byte_size, align) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_dealloc_val(val: u64) {
            unsafe { <$name as $crate::RawInterpreter>::raw_dealloc_val(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_bool(val: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_bool(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_char(val: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_char(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_u8(val: u8) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_u8(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_s8(val: i8) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_s8(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_u16(val: u16) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_u16(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_s16(val: i16) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_s16(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_u32(val: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_u32(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_s32(val: i32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_s32(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_u64(val: u64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_u64(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_s64(val: i64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_s64(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_f32(val: f32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_f32(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_f64(val: f64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_f64(val) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_string(ptr: *mut u8, len: usize) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_string(ptr, len) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_record(ty: usize, vals: *mut u64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_record(ty, vals) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_tuple(ty: usize, vals: *mut u64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_tuple(ty, vals) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_flags(ty: usize, flags: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_flags(ty, flags) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_enum(ty: usize, enum_: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_enum(ty, enum_) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_borrow(ty: usize, handle: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_borrow(ty, handle) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_own(ty: usize, handle: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_own(ty, handle) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_future(ty: usize, handle: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_future(ty, handle) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_stream(ty: usize, handle: u32) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_stream(ty, handle) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_resource_dtor(ty: usize, handle: usize) {
            unsafe { <$name as $crate::RawInterpreter>::raw_resource_dtor(ty, handle) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_option(
            ty: usize,
            discr: u32,
            opt_payload: u64,
        ) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_option(ty, discr, opt_payload) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_result(
            ty: usize,
            discr: u32,
            opt_payload: u64,
        ) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_result(ty, discr, opt_payload) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_variant(
            ty: usize,
            discr: u32,
            opt_payload: u64,
        ) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_variant(ty, discr, opt_payload) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_lift_list(ty: usize, ptr: *mut u8, len: usize) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_lift_list(ty, ptr, len) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_list_alloc(ty: usize, len: usize) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_list_alloc(ty, len) }
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn wit_dylib_list_push(ty: usize, list: u64, val: u64) -> u64 {
            unsafe { <$name as $crate::RawInterpreter>::raw_list_push(ty, list, val) }
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
pub unsafe trait Interpreter {
    type Borrow<'a>;
    type Own;

    /// Startup hook if necessary.
    fn initialize(wit: Wit) {
        let _ = wit;
    }

    fn call_export(wit: Wit, func: Function, args: OwnVals<'_, Self>) -> Option<Self::Own>;

    fn call_import<'a>(
        wit: Wit,
        interface: Option<&str>,
        func: &str,
        args: impl IntoIterator<Item = Self::Borrow<'a>>,
    ) -> Option<Self::Own> {
        let mut funcs = wit.iter_funcs().filter(|f| {
            f.interface() == interface && f.name() == func && f.import_impl().is_some()
        });
        let func = match funcs.next() {
            Some(func) => func,
            None => match interface {
                Some(i) => panic!("no import function named {func:?} found in {i:?}"),
                None => panic!("no import function named {func:?}"),
            },
        };
        assert!(funcs.next().is_none());
        let nparams = func.params().len();
        let has_result = func.result().is_some();

        // Allocate space for args/results
        let mut storage = vec![0u64; nparams.max(usize::from(has_result))];

        // Store all `args` into `storage`, and also panic if there are too
        // many `args` or too few `args`.
        let mut slots = storage[..nparams].iter_mut();
        for arg in args {
            let slot = slots.next().unwrap();
            unsafe {
                *slot = <Self as RawInterpreter>::borrow_to_val(arg);
            }
        }
        assert!(slots.next().is_none());

        // Invoke the actual import which is provided through metadata.
        let import_impl = func.import_impl().unwrap();
        unsafe {
            import_impl(storage.as_mut_ptr());
        }

        // If the function has a result, it's now located in `storage[0]`.
        // Otherwise storage has a bunch of `Borrow` values that shouldn't need
        // a destructor, so we can exit.
        if has_result {
            Some(unsafe { <Self as RawInterpreter>::to_own(storage[0]) })
        } else {
            None
        }
    }

    fn resource_dtor(ty: Resource, handle: usize);

    fn lower_u8(val: Self::Borrow<'_>) -> u8;
    fn lower_u16(val: Self::Borrow<'_>) -> u16;
    fn lower_u32(val: Self::Borrow<'_>) -> u32;
    fn lower_u64(val: Self::Borrow<'_>) -> u64;
    fn lower_s8(val: Self::Borrow<'_>) -> i8;
    fn lower_s16(val: Self::Borrow<'_>) -> i16;
    fn lower_s32(val: Self::Borrow<'_>) -> i32;
    fn lower_s64(val: Self::Borrow<'_>) -> i64;
    fn lower_bool(val: Self::Borrow<'_>) -> bool;
    fn lower_char(val: Self::Borrow<'_>) -> char;
    fn lower_f32(val: Self::Borrow<'_>) -> f32;
    fn lower_f64(val: Self::Borrow<'_>) -> f64;
    fn lower_string(val: Self::Borrow<'_>) -> &str;
    fn lower_borrow(ty: Resource, val: Self::Borrow<'_>) -> u32;
    fn lower_own(ty: Resource, val: Self::Borrow<'_>) -> u32;
    fn lower_enum(ty: Enum, val: Self::Borrow<'_>) -> u32;
    fn lower_flags(ty: Flags, val: Self::Borrow<'_>) -> u32;
    fn lower_future(ty: Future, val: Self::Borrow<'_>) -> u32;
    fn lower_stream(ty: Stream, val: Self::Borrow<'_>) -> u32;
    fn lower_option(ty: WitOption, val: Self::Borrow<'_>) -> Option<Self::Borrow<'_>>;
    fn lower_result(
        ty: WitResult,
        val: Self::Borrow<'_>,
    ) -> Result<Option<Self::Borrow<'_>>, Option<Self::Borrow<'_>>>;
    fn lower_record(ty: Record, val: Self::Borrow<'_>) -> impl Iterator<Item = Self::Borrow<'_>>;
    fn lower_tuple(ty: Tuple, val: Self::Borrow<'_>) -> impl Iterator<Item = Self::Borrow<'_>>;
    fn list_ptr(ty: List, val: Self::Borrow<'_>) -> *const u8;
    fn list_len(ty: List, val: Self::Borrow<'_>) -> usize;
    fn list_get(ty: List, index: usize, val: Self::Borrow<'_>) -> Self::Borrow<'_>;
    fn variant_discr(ty: Variant, val: Self::Borrow<'_>) -> u32;
    fn variant_payload(ty: Variant, val: Self::Borrow<'_>) -> Option<Self::Borrow<'_>>;

    fn lift_bool(val: bool) -> Self::Own;
    fn lift_char(val: char) -> Self::Own;
    fn lift_u8(val: u8) -> Self::Own;
    fn lift_s8(val: i8) -> Self::Own;
    fn lift_u16(val: u16) -> Self::Own;
    fn lift_s16(val: i16) -> Self::Own;
    fn lift_u32(val: u32) -> Self::Own;
    fn lift_s32(val: i32) -> Self::Own;
    fn lift_u64(val: u64) -> Self::Own;
    fn lift_s64(val: i64) -> Self::Own;
    fn lift_f32(val: f32) -> Self::Own;
    fn lift_f64(val: f64) -> Self::Own;
    fn lift_string(val: String) -> Self::Own;
    fn lift_record(ty: Record, vals: OwnVals<'_, Self>) -> Self::Own;
    fn lift_tuple(ty: Tuple, vals: OwnVals<'_, Self>) -> Self::Own;
    fn lift_flags(ty: Flags, bits: u32) -> Self::Own;
    fn lift_enum(ty: Enum, discr: u32) -> Self::Own;
    fn lift_borrow(ty: Resource, handle: u32) -> Self::Own;
    fn lift_own(ty: Resource, handle: u32) -> Self::Own;
    fn lift_future(ty: Future, handle: u32) -> Self::Own;
    fn lift_stream(ty: Stream, handle: u32) -> Self::Own;
    fn lift_variant(ty: Variant, discr: u32, payload: Option<Self::Own>) -> Self::Own;
    fn lift_option(ty: WitOption, val: Option<Self::Own>) -> Self::Own;
    fn lift_result(ty: WitResult, val: Result<Option<Self::Own>, Option<Self::Own>>) -> Self::Own;
    unsafe fn lift_list(ty: List, ptr: *mut u8, len: usize) -> Option<Self::Own> {
        let _ = (ty, ptr, len);
        None
    }
    fn list_alloc(ty: List, len: usize) -> Self::Own;
    fn list_push(ty: List, list: &mut Self::Own, val: Self::Own);
}

/// List of arguments provided to `Interpreter::call_export`.
pub struct OwnVals<'a, I: Interpreter + ?Sized> {
    iter: std::slice::IterMut<'a, RawVal<'a, I>>,
}

impl<I: Interpreter + ?Sized> OwnVals<'_, I> {
    unsafe fn new(ptr: *mut u64, len: usize) -> Self {
        OwnVals {
            iter: if len == 0 {
                Default::default()
            } else {
                unsafe { std::slice::from_raw_parts_mut(ptr.cast(), len).iter_mut() }
            },
        }
    }
}

impl<I: Interpreter + ?Sized> Iterator for OwnVals<'_, I> {
    type Item = I::Own;

    fn next(&mut self) -> Option<I::Own> {
        let raw = self.iter.next()?;
        Some(unsafe { ManuallyDrop::take(&mut raw.own) })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<I: Interpreter + ?Sized> DoubleEndedIterator for OwnVals<'_, I> {
    fn next_back(&mut self) -> Option<I::Own> {
        let raw = self.iter.next_back()?;
        Some(unsafe { ManuallyDrop::take(&mut raw.own) })
    }
}

impl<I: Interpreter + ?Sized> ExactSizeIterator for OwnVals<'_, I> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

// Drain unused items on drop to ensure all values are properly dropped.
impl<I: Interpreter + ?Sized> Drop for OwnVals<'_, I> {
    fn drop(&mut self) {
        for item in self {
            let _ = item;
        }
    }
}

static mut WIT_T: *const ffi::wit_t = ptr::null_mut();

union RawVal<'a, T: Interpreter + ?Sized> {
    own: ManuallyDrop<T::Own>,
    borrow: ManuallyDrop<T::Borrow<'a>>,
    val: u64,
}

#[doc(hidden)]
pub trait RawInterpreter: Interpreter {
    unsafe fn raw_initialize(ptr: *const u8) {
        let ptr = ptr.cast::<ffi::wit_t>();
        unsafe {
            WIT_T = ptr;
            Self::initialize(Wit::from_raw(ptr));
        }
    }

    unsafe fn raw_call_export(which: usize, ptr: *mut u64) {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let func = wit.func(which);
            let nparams = func.params().len();
            let args = OwnVals::new(ptr, nparams);
            match Self::call_export(wit, func, args) {
                Some(val) => {
                    debug_assert!(func.result().is_some());
                    *ptr = Self::own_to_val(val);
                }
                None => {
                    debug_assert!(func.result().is_none());
                }
            }
        }
    }

    unsafe fn to_borrow<'a>(val: u64) -> Self::Borrow<'a> {
        unsafe { ManuallyDrop::into_inner(RawVal::<'_, Self> { val }.borrow) }
    }

    unsafe fn to_own(val: u64) -> Self::Own {
        unsafe { ManuallyDrop::into_inner(RawVal::<'_, Self> { val }.own) }
    }

    unsafe fn borrow_to_val(borrow: Self::Borrow<'_>) -> u64 {
        unsafe {
            RawVal::<Self> {
                borrow: ManuallyDrop::new(borrow),
            }
            .val
        }
    }

    unsafe fn own_to_val(own: Self::Own) -> u64 {
        unsafe {
            RawVal::<Self> {
                own: ManuallyDrop::new(own),
            }
            .val
        }
    }

    unsafe fn raw_lower_u8(val: u64) -> u8 {
        unsafe { Self::lower_u8(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_u16(val: u64) -> u16 {
        unsafe { Self::lower_u16(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_u32(val: u64) -> u32 {
        unsafe { Self::lower_u32(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_u64(val: u64) -> u64 {
        unsafe { Self::lower_u64(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_s8(val: u64) -> i8 {
        unsafe { Self::lower_s8(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_s16(val: u64) -> i16 {
        unsafe { Self::lower_s16(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_s32(val: u64) -> i32 {
        unsafe { Self::lower_s32(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_s64(val: u64) -> i64 {
        unsafe { Self::lower_s64(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_f32(val: u64) -> f32 {
        unsafe { Self::lower_f32(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_f64(val: u64) -> f64 {
        unsafe { Self::lower_f64(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_bool(val: u64) -> bool {
        unsafe { Self::lower_bool(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_char(val: u64) -> char {
        unsafe { Self::lower_char(Self::to_borrow(val)) }
    }

    unsafe fn raw_lower_borrow(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::lower_borrow(wit.resource(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_lower_own(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::lower_own(wit.resource(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_lower_flags(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::lower_flags(wit.flags(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_lower_enum(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::lower_enum(wit.enum_(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_lower_future(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::lower_future(wit.future(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_lower_stream(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::lower_stream(wit.stream(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_lower_record(ty: usize, val: u64, mut raw_fields: *mut u64) {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let ty = wit.record(ty);
            let fields = Self::lower_record(ty, Self::to_borrow(val));
            for field in fields {
                *raw_fields = Self::borrow_to_val(field);
                raw_fields = raw_fields.add(1);
            }
        }
    }

    unsafe fn raw_lower_tuple(ty: usize, val: u64, mut raw_fields: *mut u64) {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let fields = Self::lower_tuple(wit.tuple(ty), Self::to_borrow(val));
            for field in fields {
                *raw_fields = Self::borrow_to_val(field);
                raw_fields = raw_fields.add(1);
            }
        }
    }

    unsafe fn raw_string_ptr(val: u64) -> *const u8 {
        unsafe { Self::lower_string(Self::to_borrow(val)).as_ptr() }
    }

    unsafe fn raw_string_len(val: u64) -> usize {
        unsafe { Self::lower_string(Self::to_borrow(val)).len() }
    }

    unsafe fn raw_list_len(ty: usize, val: u64) -> usize {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::list_len(wit.list(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_list_ptr(ty: usize, val: u64) -> *const u8 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::list_ptr(wit.list(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_list_get(ty: usize, index: usize, val: u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let element = Self::list_get(wit.list(ty), index, Self::to_borrow(val));
            Self::borrow_to_val(element)
        }
    }

    unsafe fn raw_option_is_some(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::lower_option(wit.option(ty), Self::to_borrow(val))
                .is_some()
                .into()
        }
    }

    unsafe fn raw_option_payload(ty: usize, val: u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let payload = Self::lower_option(wit.option(ty), Self::to_borrow(val)).unwrap();
            Self::borrow_to_val(payload)
        }
    }

    unsafe fn raw_result_is_err(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::lower_result(wit.result(ty), Self::to_borrow(val))
                .is_err()
                .into()
        }
    }

    unsafe fn raw_result_payload(ty: usize, val: u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let payload = Self::lower_result(wit.result(ty), Self::to_borrow(val))
                .unwrap_or_else(|e| e)
                .unwrap();
            Self::borrow_to_val(payload)
        }
    }

    unsafe fn raw_variant_discr(ty: usize, val: u64) -> u32 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::variant_discr(wit.variant(ty), Self::to_borrow(val))
        }
    }

    unsafe fn raw_variant_payload(ty: usize, val: u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let payload = Self::variant_payload(wit.variant(ty), Self::to_borrow(val)).unwrap();
            Self::borrow_to_val(payload)
        }
    }

    unsafe fn raw_dealloc_bytes(ptr: *mut u8, byte_size: usize, align: usize) {
        unsafe {
            let layout = std::alloc::Layout::from_size_align(byte_size, align).unwrap();
            std::alloc::dealloc(ptr, layout);
        }
    }

    unsafe fn raw_dealloc_val(val: u64) {
        unsafe {
            let _ = Self::to_own(val);
        }
    }

    unsafe fn raw_lift_bool(val: u32) -> u64 {
        unsafe { Self::own_to_val(Self::lift_bool(val != 0)) }
    }

    unsafe fn raw_lift_char(val: u32) -> u64 {
        unsafe { Self::own_to_val(Self::lift_char(char::from_u32(val).unwrap())) }
    }

    unsafe fn raw_lift_u8(val: u8) -> u64 {
        unsafe { Self::own_to_val(Self::lift_u8(val)) }
    }

    unsafe fn raw_lift_s8(val: i8) -> u64 {
        unsafe { Self::own_to_val(Self::lift_s8(val)) }
    }

    unsafe fn raw_lift_u16(val: u16) -> u64 {
        unsafe { Self::own_to_val(Self::lift_u16(val)) }
    }

    unsafe fn raw_lift_s16(val: i16) -> u64 {
        unsafe { Self::own_to_val(Self::lift_s16(val)) }
    }

    unsafe fn raw_lift_u32(val: u32) -> u64 {
        unsafe { Self::own_to_val(Self::lift_u32(val)) }
    }

    unsafe fn raw_lift_s32(val: i32) -> u64 {
        unsafe { Self::own_to_val(Self::lift_s32(val)) }
    }

    unsafe fn raw_lift_u64(val: u64) -> u64 {
        unsafe { Self::own_to_val(Self::lift_u64(val)) }
    }

    unsafe fn raw_lift_s64(val: i64) -> u64 {
        unsafe { Self::own_to_val(Self::lift_s64(val)) }
    }

    unsafe fn raw_lift_f32(val: f32) -> u64 {
        unsafe { Self::own_to_val(Self::lift_f32(val)) }
    }

    unsafe fn raw_lift_f64(val: f64) -> u64 {
        unsafe { Self::own_to_val(Self::lift_f64(val)) }
    }

    unsafe fn raw_lift_string(ptr: *mut u8, len: usize) -> u64 {
        unsafe {
            let bytes = Vec::from_raw_parts(ptr, len, len);
            let string = String::from_utf8_unchecked(bytes);
            Self::own_to_val(Self::lift_string(string))
        }
    }

    unsafe fn raw_lift_record(ty: usize, vals: *mut u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let ty = wit.record(ty);
            let nfields = ty.fields().len();
            let record = Self::lift_record(ty, OwnVals::new(vals, nfields));
            Self::own_to_val(record)
        }
    }

    unsafe fn raw_lift_tuple(ty: usize, vals: *mut u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let ty = wit.tuple(ty);
            let nfields = ty.types().len();
            let tuple = Self::lift_tuple(ty, OwnVals::new(vals, nfields));
            Self::own_to_val(tuple)
        }
    }

    unsafe fn raw_lift_flags(ty: usize, flags: u32) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let flags = Self::lift_flags(wit.flags(ty), flags);
            Self::own_to_val(flags)
        }
    }

    unsafe fn raw_lift_enum(ty: usize, enum_: u32) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let enum_ = Self::lift_enum(wit.enum_(ty), enum_);
            Self::own_to_val(enum_)
        }
    }

    unsafe fn raw_lift_borrow(ty: usize, borrow: u32) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let borrow = Self::lift_borrow(wit.resource(ty), borrow);
            Self::own_to_val(borrow)
        }
    }

    unsafe fn raw_lift_own(ty: usize, own: u32) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let own = Self::lift_own(wit.resource(ty), own);
            Self::own_to_val(own)
        }
    }

    unsafe fn raw_lift_future(ty: usize, future: u32) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let future = Self::lift_future(wit.future(ty), future);
            Self::own_to_val(future)
        }
    }

    unsafe fn raw_lift_stream(ty: usize, stream: u32) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let stream = Self::lift_stream(wit.stream(ty), stream);
            Self::own_to_val(stream)
        }
    }

    unsafe fn raw_resource_dtor(ty: usize, handle: usize) {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            Self::resource_dtor(wit.resource(ty), handle);
        }
    }

    unsafe fn raw_lift_option(ty: usize, discr: u32, opt_payload: u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let payload = if opt_payload == 0 {
                assert!(discr == 0);
                None
            } else {
                assert!(discr == 1);
                Some(Self::to_own(opt_payload))
            };
            let val = Self::lift_option(wit.option(ty), payload);
            Self::own_to_val(val)
        }
    }

    unsafe fn raw_lift_result(ty: usize, discr: u32, opt_payload: u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let payload = if opt_payload == 0 {
                None
            } else {
                Some(Self::to_own(opt_payload))
            };
            let val = if discr == 0 {
                Ok(payload)
            } else {
                Err(payload)
            };
            let val = Self::lift_result(wit.result(ty), val);
            Self::own_to_val(val)
        }
    }

    unsafe fn raw_lift_variant(ty: usize, discr: u32, opt_payload: u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let payload = if opt_payload == 0 {
                None
            } else {
                Some(Self::to_own(opt_payload))
            };
            let val = Self::lift_variant(wit.variant(ty), discr, payload);
            Self::own_to_val(val)
        }
    }

    unsafe fn raw_lift_list(ty: usize, list: *mut u8, len: usize) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            match Self::lift_list(wit.list(ty), list, len) {
                Some(list) => Self::own_to_val(list),
                None => 0,
            }
        }
    }

    unsafe fn raw_list_alloc(ty: usize, len: usize) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let list = Self::list_alloc(wit.list(ty), len);
            Self::own_to_val(list)
        }
    }

    unsafe fn raw_list_push(ty: usize, list: u64, val: u64) -> u64 {
        unsafe {
            let wit = Wit::from_raw(WIT_T);
            let mut list = Self::to_own(list);
            let val = Self::to_own(val);
            Self::list_push(wit.list(ty), &mut list, val);
            Self::own_to_val(list)
        }
    }
}

impl<T: Interpreter + ?Sized> RawInterpreter for T {}

#[expect(dead_code, non_camel_case_types, reason = "generated code")]
mod ffi;
pub mod test_util;
mod types;

pub use self::types::*;
