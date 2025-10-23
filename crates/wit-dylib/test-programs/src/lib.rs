//! An example implementation of the `Interpreter` trait which is used in the
//! test of this crate.
//!
//! Here a `TestCase` trait is used to customize the behavior of each test case,
//! but all tests have the same lifting/lowering behavior so there's a blanket
//! `impl<T: TestCase> Interpreter for T` here as well. The `Val` type
//! represents what a component model value might look like in an interpreted
//! language. Note that it's specialized here for precise testing against WIT
//! semantics.

#![allow(unsafe_code, async_fn_in_trait)]
#![allow(clippy::allow_attributes_without_reason)]

use std::alloc::Layout;
use std::borrow::Cow;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering::Relaxed};
pub use wit_dylib_ffi::*;

pub mod generate;

#[macro_export]
macro_rules! export_test {
    (struct $name:ident) => {
        struct $name;
        type _Test = $crate::Test<$name>;
        wit_dylib_ffi::export!(_Test);
    };
}

pub const DEBUG: bool = false;

#[macro_export]
macro_rules! dprintln {
    ($($t:tt)*) => (if $crate::DEBUG {
        println!($($t)*);
    })
}

pub trait TestCase: 'static {
    fn initialize(wit: Wit) {
        let _ = wit;
    }

    fn call_export(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val>;

    async fn call_export_async(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        let _ = (wit, func, args);
        unimplemented!();
    }

    fn call_import(wit: Wit, interface: Option<&str>, func: &str, args: &[Val]) -> Option<Val> {
        Self::call_import_func(wit.unwrap_import(interface, func), args)
    }

    fn call_import_func(func: ImportFunction, args: &[Val]) -> Option<Val> {
        call_import_func(func, args)
    }

    async fn call_import_async(
        wit: Wit,
        interface: Option<&str>,
        func: &str,
        args: &[Val],
    ) -> Option<Val> {
        Self::call_import_func_async(wit.unwrap_import(interface, func), args).await
    }

    async fn call_import_func_async(func: ImportFunction, args: &[Val]) -> Option<Val> {
        let mut cx = Cx::default();
        cx.stack.extend(args.iter().rev().map(Cow::Borrowed));
        func.call_import_async(&mut cx).await;
        cx.stack.pop().map(|i| i.into_owned())
    }

    fn resource_dtor(ty: Resource, handle: usize) {
        let _ = (ty, handle);
        panic!("should override this");
    }
}

fn call_import_func(func: ImportFunction, args: &[Val]) -> Option<Val> {
    let mut cx = Cx::default();
    cx.stack.extend(args.iter().rev().map(Cow::Borrowed));
    func.call_import_sync(&mut cx);
    cx.stack.pop().map(|i| i.into_owned())
}

#[derive(Default)]
pub struct Cx<'a> {
    stack: Vec<Cow<'a, Val>>,
    temp_strings: Vec<String>,
    temp_bytes: Vec<Vec<u8>>,
    iterators: Vec<CowIter<'a>>,
    deferred_deallocs: Vec<(*mut u8, Layout)>,
}

impl Drop for Cx<'_> {
    fn drop(&mut self) {
        for (ptr, layout) in self.deferred_deallocs.drain(..) {
            unsafe {
                std::alloc::dealloc(ptr, layout);
            }
        }
    }
}

enum CowIter<'a> {
    Borrowed(std::slice::Iter<'a, Val>),
    Owned(std::vec::IntoIter<Val>),
}

impl<'a> Iterator for CowIter<'a> {
    type Item = Cow<'a, Val>;

    fn next(&mut self) -> Option<Cow<'a, Val>> {
        match self {
            CowIter::Borrowed(i) => Some(Cow::Borrowed(i.next()?)),
            CowIter::Owned(i) => Some(Cow::Owned(i.next()?)),
        }
    }
}

pub struct Test<T: ?Sized>(T);

impl<T: TestCase + ?Sized> Interpreter for Test<T> {
    type CallCx<'a> = Cx<'a>;

    fn initialize(wit: Wit) {
        T::initialize(wit)
    }

    fn export_start<'a>(_wit: Wit, _func: ExportFunction) -> Box<Self::CallCx<'a>> {
        Box::new(Cx::default())
    }

    fn export_call(wit: Wit, func: ExportFunction, cx: &mut Self::CallCx<'_>) {
        match T::call_export(wit, func, cx.stack.drain(..).map(|v| v.into_owned())) {
            Some(val) => cx.push_own(val),
            None => {}
        }
    }

    async fn export_call_async(wit: Wit, func: ExportFunction, mut cx: Box<Self::CallCx<'static>>) {
        match T::call_export_async(wit, func, cx.stack.drain(..).map(|v| v.into_owned())).await {
            Some(val) => cx.push_own(val),
            None => {}
        }

        unsafe {
            let cx: *mut Self::CallCx<'_> = &mut *cx;
            func.task_return().unwrap()(cx.cast())
        }
    }

    fn resource_dtor(ty: Resource, handle: usize) {
        T::resource_dtor(ty, handle)
    }
}

impl<'a> Cx<'a> {
    fn always_pop(&mut self) -> Cow<'a, Val> {
        match self.stack.pop() {
            Some(val) => val,
            None => invalid(),
        }
    }

    fn push_own(&mut self, val: Val) {
        self.stack.push(Cow::Owned(val));
    }

    fn pop_payload(&mut self, payload_present: bool) -> Option<Box<Val>> {
        if !payload_present {
            return None;
        }
        Some(Box::new(self.always_pop().into_owned()))
    }

    fn drain_top(&mut self, amt: usize) -> Vec<Val> {
        let len = self.stack.len();
        self.stack
            .drain(len - amt..len)
            .map(|i| i.into_owned())
            .collect()
    }
}

impl Call for Cx<'_> {
    unsafe fn defer_deallocate(&mut self, ptr: *mut u8, layout: Layout) {
        self.deferred_deallocs.push((ptr, layout));
    }

    fn pop_u8(&mut self) -> u8 {
        match *self.always_pop() {
            Val::U8(val) => val,
            _ => invalid(),
        }
    }

    fn push_u8(&mut self, val: u8) {
        self.push_own(Val::U8(val));
    }

    fn pop_s8(&mut self) -> i8 {
        match *self.always_pop() {
            Val::S8(val) => val,
            _ => invalid(),
        }
    }

    fn push_s8(&mut self, val: i8) {
        self.push_own(Val::S8(val));
    }

    fn pop_u16(&mut self) -> u16 {
        match *self.always_pop() {
            Val::U16(val) => val,
            _ => invalid(),
        }
    }

    fn push_u16(&mut self, val: u16) {
        self.push_own(Val::U16(val));
    }

    fn pop_s16(&mut self) -> i16 {
        match *self.always_pop() {
            Val::S16(val) => val,
            _ => invalid(),
        }
    }

    fn push_s16(&mut self, val: i16) {
        self.push_own(Val::S16(val));
    }

    fn pop_u32(&mut self) -> u32 {
        match *self.always_pop() {
            Val::U32(val) => val,
            _ => invalid(),
        }
    }

    fn push_u32(&mut self, val: u32) {
        self.push_own(Val::U32(val));
    }

    fn pop_s32(&mut self) -> i32 {
        match *self.always_pop() {
            Val::S32(val) => val,
            _ => invalid(),
        }
    }

    fn push_s32(&mut self, val: i32) {
        self.push_own(Val::S32(val));
    }

    fn pop_u64(&mut self) -> u64 {
        match *self.always_pop() {
            Val::U64(val) => val,
            _ => invalid(),
        }
    }

    fn push_u64(&mut self, val: u64) {
        self.push_own(Val::U64(val));
    }

    fn pop_s64(&mut self) -> i64 {
        match *self.always_pop() {
            Val::S64(val) => val,
            _ => invalid(),
        }
    }

    fn push_s64(&mut self, val: i64) {
        self.push_own(Val::S64(val));
    }

    fn pop_f32(&mut self) -> f32 {
        match *self.always_pop() {
            Val::F32(val) => val,
            _ => invalid(),
        }
    }

    fn push_f32(&mut self, val: f32) {
        self.push_own(Val::F32(val));
    }

    fn pop_f64(&mut self) -> f64 {
        match *self.always_pop() {
            Val::F64(val) => val,
            _ => invalid(),
        }
    }

    fn push_f64(&mut self, val: f64) {
        self.push_own(Val::F64(val));
    }

    fn pop_bool(&mut self) -> bool {
        match *self.always_pop() {
            Val::Bool(val) => val,
            _ => invalid(),
        }
    }

    fn push_bool(&mut self, val: bool) {
        self.push_own(Val::Bool(val));
    }

    fn pop_char(&mut self) -> char {
        match *self.always_pop() {
            Val::Char(val) => val,
            _ => invalid(),
        }
    }

    fn push_char(&mut self, val: char) {
        self.push_own(Val::Char(val));
    }

    fn pop_string(&mut self) -> &str {
        match self.always_pop() {
            Cow::Borrowed(Val::String(val)) => val,
            Cow::Owned(Val::String(val)) => {
                self.temp_strings.push(val);
                self.temp_strings.last().unwrap()
            }
            _ => invalid(),
        }
    }

    fn push_string(&mut self, val: String) {
        self.push_own(Val::String(val));
    }

    fn pop_borrow(&mut self, _ty: Resource) -> u32 {
        match &*self.always_pop() {
            Val::Borrow(val) => val.handle(),
            _ => invalid(),
        }
    }

    fn push_borrow(&mut self, ty: Resource, val: u32) {
        self.push_own(Val::Borrow(if ty.new().is_some() {
            Borrow::Rep(val)
        } else {
            Borrow::Handle(ty, val)
        }))
    }

    fn pop_own(&mut self, _ty: Resource) -> u32 {
        match &*self.always_pop() {
            Val::Own(val) => val.take_handle(),
            _ => invalid(),
        }
    }

    fn push_own(&mut self, ty: Resource, handle: u32) {
        self.push_own(Val::Own(Own {
            inner: Arc::new(OwnInner {
                ty,
                handle: handle.into(),
            }),
        }))
    }

    fn pop_future(&mut self, _ty: Future) -> u32 {
        match *self.always_pop() {
            Val::Future(val) => val,
            _ => invalid(),
        }
    }

    fn push_future(&mut self, _ty: Future, val: u32) {
        self.push_own(Val::Future(val))
    }

    fn pop_stream(&mut self, _ty: Stream) -> u32 {
        match *self.always_pop() {
            Val::Stream(val) => val,
            _ => invalid(),
        }
    }

    fn push_stream(&mut self, _ty: Stream, val: u32) {
        self.push_own(Val::Stream(val))
    }

    fn pop_flags(&mut self, _ty: Flags) -> u32 {
        match *self.always_pop() {
            Val::Flags(val) => val,
            _ => invalid(),
        }
    }

    fn push_flags(&mut self, _ty: Flags, val: u32) {
        self.push_own(Val::Flags(val))
    }

    fn pop_enum(&mut self, _ty: Enum) -> u32 {
        match *self.always_pop() {
            Val::Enum(val) => val,
            _ => invalid(),
        }
    }

    fn push_enum(&mut self, _ty: Enum, val: u32) {
        self.push_own(Val::Enum(val))
    }

    fn pop_option(&mut self, _ty: WitOption) -> u32 {
        match self.always_pop() {
            Cow::Borrowed(Val::Option(None)) => 0,
            Cow::Borrowed(Val::Option(Some(val))) => {
                self.stack.push(Cow::Borrowed(val));
                1
            }
            Cow::Owned(Val::Option(None)) => 0,
            Cow::Owned(Val::Option(Some(val))) => {
                self.stack.push(Cow::Owned(*val));
                1
            }
            Cow::Borrowed(_) | Cow::Owned(_) => invalid(),
        }
    }

    fn push_option(&mut self, _ty: WitOption, is_some: bool) {
        let payload = self.pop_payload(is_some);
        self.push_own(Val::Option(payload));
    }

    fn pop_result(&mut self, _ty: WitResult) -> u32 {
        match self.always_pop() {
            Cow::Borrowed(Val::Result(Ok(val))) => {
                if let Some(val) = val {
                    self.stack.push(Cow::Borrowed(val));
                }
                0
            }
            Cow::Borrowed(Val::Result(Err(val))) => {
                if let Some(val) = val {
                    self.stack.push(Cow::Borrowed(val));
                }
                1
            }
            Cow::Owned(Val::Result(Ok(val))) => {
                if let Some(val) = val {
                    self.stack.push(Cow::Owned(*val));
                }
                0
            }
            Cow::Owned(Val::Result(Err(val))) => {
                if let Some(val) = val {
                    self.stack.push(Cow::Owned(*val));
                }
                1
            }
            Cow::Borrowed(_) | Cow::Owned(_) => invalid(),
        }
    }

    fn push_result(&mut self, ty: WitResult, is_err: bool) {
        let val = if is_err {
            Err(self.pop_payload(ty.err().is_some()))
        } else {
            Ok(self.pop_payload(ty.ok().is_some()))
        };
        self.push_own(Val::Result(val))
    }

    fn pop_variant(&mut self, _ty: Variant) -> u32 {
        match self.always_pop() {
            Cow::Borrowed(Val::Variant(discr, payload)) => {
                if let Some(val) = payload {
                    self.stack.push(Cow::Borrowed(val));
                }
                *discr
            }
            Cow::Owned(Val::Variant(discr, payload)) => {
                if let Some(val) = payload {
                    self.stack.push(Cow::Owned(*val));
                }
                discr
            }
            Cow::Borrowed(_) | Cow::Owned(_) => invalid(),
        }
    }

    fn push_variant(&mut self, ty: Variant, discr: u32) {
        let payload = self.pop_payload(ty.cases().nth(discr as usize).unwrap().1.is_some());
        self.push_own(Val::Variant(discr, payload))
    }

    fn pop_record(&mut self, _ty: Record) {
        match self.always_pop() {
            Cow::Borrowed(Val::Record(fields)) => {
                self.stack.extend(fields.iter().rev().map(Cow::Borrowed));
            }
            Cow::Owned(Val::Record(fields)) => {
                self.stack.extend(fields.into_iter().rev().map(Cow::Owned));
            }
            Cow::Borrowed(_) | Cow::Owned(_) => invalid(),
        }
    }

    fn push_record(&mut self, ty: Record) {
        let fields = self.drain_top(ty.fields().len());
        self.push_own(Val::Record(fields));
    }

    fn pop_tuple(&mut self, _ty: Tuple) {
        match self.always_pop() {
            Cow::Borrowed(Val::Tuple(types)) => {
                self.stack.extend(types.iter().rev().map(Cow::Borrowed));
            }
            Cow::Owned(Val::Tuple(types)) => {
                self.stack.extend(types.into_iter().rev().map(Cow::Owned));
            }
            Cow::Borrowed(_) | Cow::Owned(_) => invalid(),
        }
    }

    fn push_tuple(&mut self, ty: Tuple) {
        let types = self.drain_top(ty.types().len());
        self.push_own(Val::Tuple(types));
    }

    unsafe fn maybe_pop_list(&mut self, ty: List) -> Option<(*const u8, usize)> {
        if ty.ty() != Type::U8 {
            return None;
        }
        let bytes = match self.always_pop() {
            Cow::Borrowed(Val::ByteList(val)) => val,
            Cow::Owned(Val::ByteList(val)) => {
                self.temp_bytes.push(val);
                self.temp_bytes.last().unwrap()
            }
            _ => invalid(),
        };
        Some((bytes.as_ptr(), bytes.len()))
    }

    fn pop_list(&mut self, ty: List) -> usize {
        assert!(ty.ty() != Type::U8);
        match self.always_pop() {
            Cow::Borrowed(Val::GenericList(l)) => {
                self.iterators.push(CowIter::Borrowed(l.iter()));
                l.len()
            }
            Cow::Owned(Val::GenericList(l)) => {
                let ret = l.len();
                self.iterators.push(CowIter::Owned(l.into_iter()));
                ret
            }
            _ => invalid(),
        }
    }

    fn pop_iter_next(&mut self, _ty: List) {
        let value = self.iterators.last_mut().unwrap().next().unwrap();
        self.stack.push(value);
    }

    fn pop_iter(&mut self, _ty: List) {
        self.iterators.pop();
    }

    unsafe fn push_raw_list(&mut self, ty: List, ptr: *mut u8, len: usize) -> bool {
        if ty.ty() == Type::U8 {
            self.push_own(Val::ByteList(unsafe { Vec::from_raw_parts(ptr, len, len) }));
            true
        } else {
            false
        }
    }
    fn push_list(&mut self, ty: List, capacity: usize) {
        assert!(ty.ty() != Type::U8);
        self.push_own(Val::GenericList(Vec::with_capacity(capacity)));
    }

    fn list_append(&mut self, ty: List) {
        assert!(ty.ty() != Type::U8);
        let val = self.always_pop().into_owned();
        match self.stack.last_mut() {
            Some(Cow::Owned(Val::GenericList(list))) => list.push(val),
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
    Borrow(Borrow),
    Own(Own),
    Flags(u32),
    Enum(u32),
    Future(u32),
    Stream(u32),
    Record(Vec<Val>),
    Tuple(Vec<Val>),
    String(String),
    Option(Option<Box<Val>>),
    Result(Result<Option<Box<Val>>, Option<Box<Val>>>),
    Variant(u32, Option<Box<Val>>),
    GenericList(Vec<Val>),
    ByteList(Vec<u8>),
}

#[derive(Clone, Debug)]
pub enum Borrow {
    Rep(u32),
    Handle(Resource, u32),
}

impl Borrow {
    fn handle(&self) -> u32 {
        match self {
            // can't return borrows from functions, meaning that this shouldn't
            // ever get hit.
            Borrow::Rep(_) => unreachable!(),
            Borrow::Handle(_, handle) => *handle,
        }
    }
}

impl PartialEq for Borrow {
    fn eq(&self, other: &Borrow) -> bool {
        match (self, other) {
            (Self::Rep(a), Self::Rep(b)) => a == b,
            (Self::Rep(_), _) => false,
            // just compare types since handles don't have meaning and we
            // otherwise can't generically look into the handle itself
            (Self::Handle(a, _), Self::Handle(b, _)) => a == b,
            (Self::Handle(..), _) => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Own {
    inner: Arc<OwnInner>,
}

impl Own {
    pub fn new(ty: Resource, rep: usize) -> Own {
        Own {
            inner: Arc::new(OwnInner {
                ty,
                handle: unsafe { ty.new().unwrap()(rep).into() },
            }),
        }
    }

    pub fn borrow(&self) -> Borrow {
        Borrow::Handle(self.inner.ty, self.handle())
    }

    pub fn handle(&self) -> u32 {
        self.inner.handle()
    }

    fn take_handle(&self) -> u32 {
        match self.inner.handle.swap(0, Relaxed) {
            0 => panic!("handle already taken"),
            other => other,
        }
    }
}

#[derive(Debug)]
struct OwnInner {
    ty: Resource,
    handle: AtomicU32,
}

impl OwnInner {
    fn handle(&self) -> u32 {
        match self.handle.load(Relaxed) {
            0 => panic!("handle already taken"),
            other => other,
        }
    }
}

impl PartialEq for OwnInner {
    fn eq(&self, other: &OwnInner) -> bool {
        if self.ty != other.ty {
            return false;
        }
        let rep = match self.ty.rep() {
            Some(rep) => rep,
            // just compare types since handles don't have meaning and we
            // otherwise can't generically look into the handle itself
            //
            // TODO: this means that resource equality in the caller component
            // in roundtrip testing isn't testing reps, only the callee
            // component is testing reps.
            None => return true,
        };
        unsafe { rep(self.handle()) == rep(other.handle()) }
    }
}

impl Drop for OwnInner {
    fn drop(&mut self) {
        match *self.handle.get_mut() {
            0 => {}
            n => unsafe { self.ty.drop()(n) },
        }
    }
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
        slop: usize,
    }

    impl Guard {
        pub fn new() -> Guard {
            Guard {
                me_before: get(),
                slop: 0,
            }
        }

        pub fn sloppy(slop: usize) -> Guard {
            Guard {
                me_before: get(),
                slop,
            }
        }
    }

    impl Drop for Guard {
        fn drop(&mut self) {
            if self.slop == 0 {
                assert_eq!(self.me_before, get());
            } else {
                assert!(
                    self.me_before + self.slop > get(),
                    "before: {}, after: {}",
                    self.me_before,
                    get(),
                );
            }
        }
    }
}
