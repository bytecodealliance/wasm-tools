//! An example implementation of the `Interpreter` trait which is used in the
//! test of this crate.
//!
//! Here a `TestCase` trait is used to customize the behavior of each test case,
//! but all tests have the same lifting/lowering behavior so there's a blanket
//! `impl<T: TestCase> Interpreter for T` here as well. The `Val` type
//! represents what a component model value might look like in an interpreted
//! language. Note that it's specialized here for precise testing against WIT
//! semantics.

use crate::{
    Enum, Flags, Function, Future, Interpreter, List, OwnVals, Record, Resource, Stream, Tuple,
    Type, Variant, Wit, WitOption, WitResult,
};
use std::ptr;

pub trait TestCase {
    fn initialize(wit: Wit) {
        let _ = wit;
    }

    fn call_export(wit: Wit, func: Function, args: OwnVals<'_, Self>) -> Option<Box<Val>>;

    fn resource_dtor(ty: Resource, handle: usize) {
        let _ = (ty, handle);
        panic!("should override this");
    }
}

unsafe impl<T: TestCase + ?Sized> Interpreter for T {
    type Borrow<'a> = &'a Val;
    type Own = Box<Val>;

    fn initialize(wit: Wit) {
        T::initialize(wit)
    }

    fn call_export(wit: Wit, func: Function, args: OwnVals<'_, Self>) -> Option<Box<Val>> {
        T::call_export(wit, func, args)
    }

    fn resource_dtor(ty: Resource, handle: usize) {
        T::resource_dtor(ty, handle)
    }

    fn lower_u8(val: &Val) -> u8 {
        match val {
            Val::U8(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_u16(val: &Val) -> u16 {
        match val {
            Val::U16(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_u32(val: &Val) -> u32 {
        match val {
            Val::U32(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_u64(val: &Val) -> u64 {
        match val {
            Val::U64(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_s8(val: &Val) -> i8 {
        match val {
            Val::S8(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_s16(val: &Val) -> i16 {
        match val {
            Val::S16(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_s32(val: &Val) -> i32 {
        match val {
            Val::S32(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_s64(val: &Val) -> i64 {
        match val {
            Val::S64(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_bool(val: &Val) -> bool {
        match val {
            Val::Bool(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_char(val: &Val) -> char {
        match val {
            Val::Char(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_f32(val: &Val) -> f32 {
        match val {
            Val::F32(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_f64(val: &Val) -> f64 {
        match val {
            Val::F64(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_borrow(_ty: Resource, val: &Val) -> u32 {
        match val {
            Val::Borrow(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_own(_ty: Resource, val: &Val) -> u32 {
        match val {
            Val::Own(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_flags(_ty: Flags, val: &Val) -> u32 {
        match val {
            Val::Flags(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_enum(_ty: Enum, val: &Val) -> u32 {
        match val {
            Val::Enum(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_future(_ty: Future, val: &Val) -> u32 {
        match val {
            Val::Future(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_stream(_ty: Stream, val: &Val) -> u32 {
        match val {
            Val::Stream(val) => *val,
            _ => invalid(),
        }
    }

    fn lower_record(_ty: Record, val: Self::Borrow<'_>) -> impl Iterator<Item = Self::Borrow<'_>> {
        match val {
            Val::Record(val) => val.iter().map(|p| &**p),
            _ => invalid(),
        }
    }

    fn lower_tuple(_ty: Tuple, val: Self::Borrow<'_>) -> impl Iterator<Item = Self::Borrow<'_>> {
        match val {
            Val::Tuple(val) => val.iter().map(|p| &**p),
            _ => invalid(),
        }
    }

    fn lower_string(val: Self::Borrow<'_>) -> &str {
        match val {
            Val::String(s) => s,
            _ => invalid(),
        }
    }

    fn list_ptr(ty: List, val: &Val) -> *const u8 {
        if ty.ty() == Type::U8 {
            match val {
                Val::ByteList(s) => s.as_ptr(),
                _ => invalid(),
            }
        } else {
            match val {
                Val::GenericList(_) => ptr::null(),
                _ => invalid(),
            }
        }
    }

    fn list_len(ty: List, val: &Val) -> usize {
        if ty.ty() == Type::U8 {
            match val {
                Val::ByteList(s) => s.len(),
                _ => invalid(),
            }
        } else {
            match val {
                Val::GenericList(s) => s.len(),
                _ => invalid(),
            }
        }
    }

    fn list_get(ty: List, index: usize, val: Self::Borrow<'_>) -> Self::Borrow<'_> {
        if ty.ty() == Type::U8 {
            unreachable!()
        } else {
            match val {
                Val::GenericList(s) => &s[index],
                _ => invalid(),
            }
        }
    }

    fn lower_option(_ty: WitOption, val: Self::Borrow<'_>) -> Option<Self::Borrow<'_>> {
        match val {
            Val::Option(s) => s.as_deref(),
            _ => invalid(),
        }
    }

    fn lower_result(
        _ty: WitResult,
        val: Self::Borrow<'_>,
    ) -> Result<Option<Self::Borrow<'_>>, Option<Self::Borrow<'_>>> {
        match val {
            Val::Result(Ok(s)) => Ok(s.as_deref()),
            Val::Result(Err(s)) => Err(s.as_deref()),
            _ => invalid(),
        }
    }

    fn variant_discr(_ty: Variant, val: Self::Borrow<'_>) -> u32 {
        match val {
            Val::Variant(discr, _payload) => *discr,
            _ => invalid(),
        }
    }

    fn variant_payload(_ty: Variant, val: Self::Borrow<'_>) -> Option<Self::Borrow<'_>> {
        match val {
            Val::Variant(_discr, payload) => payload.as_deref(),
            _ => invalid(),
        }
    }

    fn lift_bool(val: bool) -> Self::Own {
        Box::new(Val::Bool(val))
    }

    fn lift_char(val: char) -> Self::Own {
        Box::new(Val::Char(val))
    }

    fn lift_u8(val: u8) -> Self::Own {
        Box::new(Val::U8(val))
    }

    fn lift_s8(val: i8) -> Self::Own {
        Box::new(Val::S8(val))
    }

    fn lift_u16(val: u16) -> Self::Own {
        Box::new(Val::U16(val))
    }

    fn lift_s16(val: i16) -> Self::Own {
        Box::new(Val::S16(val))
    }

    fn lift_u32(val: u32) -> Self::Own {
        Box::new(Val::U32(val))
    }

    fn lift_s32(val: i32) -> Self::Own {
        Box::new(Val::S32(val))
    }

    fn lift_u64(val: u64) -> Self::Own {
        Box::new(Val::U64(val))
    }

    fn lift_s64(val: i64) -> Self::Own {
        Box::new(Val::S64(val))
    }

    fn lift_f32(val: f32) -> Self::Own {
        Box::new(Val::F32(val))
    }

    fn lift_f64(val: f64) -> Self::Own {
        Box::new(Val::F64(val))
    }

    fn lift_string(val: String) -> Self::Own {
        Box::new(Val::String(val))
    }

    fn lift_record(_ty: Record, vals: OwnVals<'_, Self>) -> Self::Own {
        Box::new(Val::Record(vals.collect()))
    }

    fn lift_tuple(_ty: Tuple, vals: OwnVals<'_, Self>) -> Self::Own {
        Box::new(Val::Tuple(vals.collect()))
    }

    fn lift_flags(_ty: Flags, flags: u32) -> Self::Own {
        Box::new(Val::Flags(flags))
    }

    fn lift_enum(_ty: Enum, enum_: u32) -> Self::Own {
        Box::new(Val::Enum(enum_))
    }

    fn lift_borrow(_ty: Resource, borrow: u32) -> Self::Own {
        Box::new(Val::Borrow(borrow))
    }

    fn lift_own(_ty: Resource, own: u32) -> Self::Own {
        Box::new(Val::Own(own))
    }

    fn lift_future(_ty: Future, future: u32) -> Self::Own {
        Box::new(Val::Future(future))
    }

    fn lift_stream(_ty: Stream, stream: u32) -> Self::Own {
        Box::new(Val::Stream(stream))
    }

    fn lift_variant(_ty: Variant, discr: u32, payload: Option<Self::Own>) -> Self::Own {
        Box::new(Val::Variant(discr, payload))
    }

    fn lift_option(_ty: WitOption, val: Option<Self::Own>) -> Self::Own {
        Box::new(Val::Option(val))
    }

    fn lift_result(_ty: WitResult, val: Result<Option<Self::Own>, Option<Self::Own>>) -> Self::Own {
        Box::new(Val::Result(val))
    }

    unsafe fn lift_list(ty: List, ptr: *mut u8, len: usize) -> Option<Self::Own> {
        if ty.ty() == Type::U8 {
            unsafe { Some(Box::new(Val::ByteList(Vec::from_raw_parts(ptr, len, len)))) }
        } else {
            None
        }
    }

    fn list_alloc(ty: List, len: usize) -> Self::Own {
        assert!(ty.ty() != Type::U8);
        Box::new(Val::GenericList(Vec::with_capacity(len)))
    }

    fn list_push(ty: List, list: &mut Self::Own, val: Self::Own) {
        assert!(ty.ty() != Type::U8);
        match &mut **list {
            Val::GenericList(list) => list.push(val),
            _ => invalid(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    S8(i8),
    S16(i16),
    S32(i32),
    S64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    Borrow(u32),
    Own(u32),
    Flags(u32),
    Enum(u32),
    Future(u32),
    Stream(u32),
    Record(Vec<Box<Val>>),
    Tuple(Vec<Box<Val>>),
    String(String),
    Option(Option<Box<Val>>),
    Result(Result<Option<Box<Val>>, Option<Box<Val>>>),
    Variant(u32, Option<Box<Val>>),

    GenericList(Vec<Box<Val>>),
    ByteList(Vec<u8>),
}

#[cold]
fn invalid() -> ! {
    unreachable!()
}

pub mod alloc {
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

    #[global_allocator]
    static ALLOC: A = A;

    static ALLOC_AMT: AtomicUsize = AtomicUsize::new(0);

    struct A;

    unsafe impl GlobalAlloc for A {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            let ptr = unsafe { System.alloc(layout) };
            if !ptr.is_null() {
                ALLOC_AMT.fetch_add(layout.size(), Relaxed);
            }
            return ptr;
        }

        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            unsafe {
                // Poison all deallocations to try to catch any use-after-free in the
                // bindings as early as possible.
                std::ptr::write_bytes(ptr, 0xde, layout.size());
                ALLOC_AMT.fetch_sub(layout.size(), Relaxed);
                System.dealloc(ptr, layout)
            }
        }
    }

    /// Get the current number of bytes allocated for this program.
    pub fn get() -> usize {
        ALLOC_AMT.load(Relaxed)
    }

    /// Helper type to assert that allocated bytes in an RAII scope remain
    /// constant.
    pub struct Guard {
        me_before: usize,
    }

    impl Guard {
        pub fn new() -> Guard {
            Guard { me_before: get() }
        }
    }

    impl Drop for Guard {
        fn drop(&mut self) {
            assert_eq!(self.me_before, get());
        }
    }
}
