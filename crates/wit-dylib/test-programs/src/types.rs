use crate::ffi;
use std::ffi::{CStr, c_char};
use std::fmt;

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Wit {
    ptr: &'static ffi::wit_t,
}

impl Wit {
    pub(crate) unsafe fn from_raw(ptr: *const ffi::wit_t) -> Wit {
        unsafe {
            assert!((*ptr).version >= ffi::WIT_V0);
            Wit {
                ptr: ptr.as_ref().unwrap(),
            }
        }
    }

    fn raw_funcs(&self) -> &'static [ffi::wit_func_t] {
        unsafe { slice(self.ptr.funcs, self.ptr.num_funcs) }
    }

    pub fn func(&self, index: usize) -> Function {
        Function {
            wit: *self,
            ptr: &self.raw_funcs()[index],
        }
    }

    pub fn iter_funcs(&self) -> impl ExactSizeIterator<Item = Function> + '_ {
        self.raw_funcs().iter().map(|func| Function {
            wit: *self,
            ptr: func,
        })
    }

    fn raw_records(&self) -> &'static [ffi::wit_record_t] {
        unsafe { slice(self.ptr.records, self.ptr.num_records) }
    }

    pub fn record(&self, index: usize) -> Record {
        Record {
            wit: *self,
            ptr: &self.raw_records()[index],
        }
    }

    pub fn iter_records(&self) -> impl ExactSizeIterator<Item = Record> + '_ {
        self.raw_records().iter().map(|record| Record {
            wit: *self,
            ptr: record,
        })
    }

    fn raw_resources(&self) -> &'static [ffi::wit_resource_t] {
        unsafe { slice(self.ptr.resources, self.ptr.num_resources) }
    }

    pub fn resource(&self, index: usize) -> Resource {
        Resource {
            ptr: &self.raw_resources()[index],
        }
    }

    pub fn iter_resources(&self) -> impl ExactSizeIterator<Item = Resource> + '_ {
        self.raw_resources()
            .iter()
            .map(|resource| Resource { ptr: resource })
    }

    fn raw_flags(&self) -> &'static [ffi::wit_flags_t] {
        unsafe { slice(self.ptr.flags, self.ptr.num_flags) }
    }

    pub fn flags(&self, index: usize) -> Flags {
        Flags {
            ptr: &self.raw_flags()[index],
        }
    }

    pub fn iter_flags(&self) -> impl ExactSizeIterator<Item = Flags> + '_ {
        self.raw_flags().iter().map(|flags| Flags { ptr: flags })
    }

    fn raw_tuples(&self) -> &'static [ffi::wit_tuple_t] {
        unsafe { slice(self.ptr.tuples, self.ptr.num_tuples) }
    }

    pub fn tuple(&self, index: usize) -> Tuple {
        Tuple {
            wit: *self,
            ptr: &self.raw_tuples()[index],
        }
    }

    pub fn iter_tuples(&self) -> impl ExactSizeIterator<Item = Tuple> + '_ {
        self.raw_tuples().iter().map(|tuple| Tuple {
            wit: *self,
            ptr: tuple,
        })
    }

    fn raw_variants(&self) -> &'static [ffi::wit_variant_t] {
        unsafe { slice(self.ptr.variants, self.ptr.num_variants) }
    }

    pub fn variant(&self, index: usize) -> Variant {
        Variant {
            wit: *self,
            ptr: &self.raw_variants()[index],
        }
    }

    pub fn iter_variants(&self) -> impl ExactSizeIterator<Item = Variant> + '_ {
        self.raw_variants().iter().map(|variant| Variant {
            wit: *self,
            ptr: variant,
        })
    }

    fn raw_enums(&self) -> &'static [ffi::wit_enum_t] {
        unsafe { slice(self.ptr.enums, self.ptr.num_enums) }
    }

    pub fn enum_(&self, index: usize) -> Enum {
        Enum {
            ptr: &self.raw_enums()[index],
        }
    }

    pub fn iter_enums(&self) -> impl ExactSizeIterator<Item = Enum> + '_ {
        self.raw_enums().iter().map(|e| Enum { ptr: e })
    }

    fn raw_options(&self) -> &'static [ffi::wit_option_t] {
        unsafe { slice(self.ptr.options, self.ptr.num_options) }
    }

    pub fn option(&self, index: usize) -> WitOption {
        WitOption {
            wit: *self,
            ptr: &self.raw_options()[index],
        }
    }

    pub fn iter_options(&self) -> impl ExactSizeIterator<Item = WitOption> + '_ {
        self.raw_options()
            .iter()
            .map(|e| WitOption { wit: *self, ptr: e })
    }

    fn raw_results(&self) -> &'static [ffi::wit_result_t] {
        unsafe { slice(self.ptr.results, self.ptr.num_results) }
    }

    pub fn result(&self, index: usize) -> WitResult {
        WitResult {
            wit: *self,
            ptr: &self.raw_results()[index],
        }
    }

    pub fn iter_results(&self) -> impl ExactSizeIterator<Item = WitResult> + '_ {
        self.raw_results()
            .iter()
            .map(|e| WitResult { wit: *self, ptr: e })
    }

    fn raw_lists(&self) -> &'static [ffi::wit_list_t] {
        unsafe { slice(self.ptr.lists, self.ptr.num_lists) }
    }

    pub fn list(&self, index: usize) -> List {
        List {
            wit: *self,
            ptr: &self.raw_lists()[index],
        }
    }

    pub fn iter_lists(&self) -> impl ExactSizeIterator<Item = List> + '_ {
        self.raw_lists().iter().map(|e| List { wit: *self, ptr: e })
    }

    fn raw_fixed_size_lists(&self) -> &'static [ffi::wit_fixed_size_list_t] {
        unsafe { slice(self.ptr.fixed_size_lists, self.ptr.num_fixed_size_lists) }
    }

    pub fn fixed_size_list(&self, index: usize) -> FixedSizeList {
        FixedSizeList {
            wit: *self,
            ptr: &self.raw_fixed_size_lists()[index],
        }
    }

    pub fn iter_fixed_size_lists(&self) -> impl ExactSizeIterator<Item = FixedSizeList> + '_ {
        self.raw_fixed_size_lists()
            .iter()
            .map(|e| FixedSizeList { wit: *self, ptr: e })
    }

    fn raw_futures(&self) -> &'static [ffi::wit_future_t] {
        unsafe { slice(self.ptr.futures, self.ptr.num_futures) }
    }

    pub fn future(&self, index: usize) -> Future {
        Future {
            wit: *self,
            ptr: &self.raw_futures()[index],
        }
    }

    pub fn iter_futures(&self) -> impl ExactSizeIterator<Item = Future> + '_ {
        self.raw_futures()
            .iter()
            .map(|e| Future { wit: *self, ptr: e })
    }

    fn raw_streams(&self) -> &'static [ffi::wit_stream_t] {
        unsafe { slice(self.ptr.streams, self.ptr.num_streams) }
    }

    pub fn stream(&self, index: usize) -> Stream {
        Stream {
            wit: *self,
            ptr: &self.raw_streams()[index],
        }
    }

    pub fn iter_streams(&self) -> impl ExactSizeIterator<Item = Stream> + '_ {
        self.raw_streams()
            .iter()
            .map(|e| Stream { wit: *self, ptr: e })
    }

    fn raw_aliases(&self) -> &'static [ffi::wit_alias_t] {
        unsafe { slice(self.ptr.aliases, self.ptr.num_aliases) }
    }

    pub fn alias(&self, index: usize) -> Alias {
        Alias {
            wit: *self,
            ptr: &self.raw_aliases()[index],
        }
    }

    pub fn iter_aliases(&self) -> impl ExactSizeIterator<Item = Alias> + '_ {
        self.raw_aliases()
            .iter()
            .map(|e| Alias { wit: *self, ptr: e })
    }
}

unsafe fn slice<T>(ptr: *const T, len: usize) -> &'static [T] {
    if len == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

#[derive(Copy, Clone)]
pub struct Function {
    wit: Wit,
    ptr: &'static ffi::wit_func_t,
}

impl Function {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn import_impl(&self) -> Option<unsafe extern "C" fn(*mut u64)> {
        self.ptr.impl_
    }

    pub fn params(&self) -> impl ExactSizeIterator<Item = Type> + '_ {
        self.raw_params()
            .iter()
            .map(|param| Type::from_raw(self.wit, *param))
    }

    fn raw_params(&self) -> &'static [ffi::wit_type_t] {
        unsafe { slice(self.ptr.params, self.ptr.nparams) }
    }

    pub fn result(&self) -> Option<Type> {
        Type::from_raw_opt(self.wit, self.ptr.result)
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Function")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("params", &self.params().collect::<Vec<_>>())
            .field("result", &self.result())
            .finish_non_exhaustive()
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

unsafe fn opt_str(s: *const c_char) -> Option<&'static str> {
    if s.is_null() {
        None
    } else {
        unsafe { Some(to_str(s)) }
    }
}

unsafe fn to_str(s: *const c_char) -> &'static str {
    unsafe { CStr::from_ptr(s).to_str().unwrap() }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
    Bool,
    Char,
    F32,
    F64,
    String,
    ErrorContext,
    Record(Record),
    Own(Resource),
    Borrow(Resource),
    Tuple(Tuple),
    Variant(Variant),
    Flags(Flags),
    Enum(Enum),
    Option(WitOption),
    Result(WitResult),
    List(List),
    FixedSizeList(FixedSizeList),
    Future(Future),
    Stream(Stream),
    Alias(Alias),
}

impl Type {
    fn from_raw(wit: Wit, raw: ffi::wit_type_t) -> Type {
        let index = (raw >> 8) as usize;
        match raw & 0xff {
            ffi::WIT_TYPE_U8 => Self::U8,
            ffi::WIT_TYPE_U16 => Self::U16,
            ffi::WIT_TYPE_U32 => Self::U32,
            ffi::WIT_TYPE_U64 => Self::U64,
            ffi::WIT_TYPE_S8 => Self::S8,
            ffi::WIT_TYPE_S16 => Self::S16,
            ffi::WIT_TYPE_S32 => Self::S32,
            ffi::WIT_TYPE_S64 => Self::S64,
            ffi::WIT_TYPE_BOOL => Self::Bool,
            ffi::WIT_TYPE_CHAR => Self::Char,
            ffi::WIT_TYPE_F32 => Self::F32,
            ffi::WIT_TYPE_F64 => Self::F64,
            ffi::WIT_TYPE_STRING => Self::String,
            ffi::WIT_TYPE_ERROR_CONTEXT => Self::ErrorContext,
            ffi::WIT_TYPE_RECORD => Self::Record(wit.record(index)),
            ffi::WIT_TYPE_OWN => Self::Own(wit.resource(index)),
            ffi::WIT_TYPE_BORROW => Self::Borrow(wit.resource(index)),
            ffi::WIT_TYPE_FLAGS => Self::Flags(wit.flags(index)),
            ffi::WIT_TYPE_TUPLE => Self::Tuple(wit.tuple(index)),
            ffi::WIT_TYPE_VARIANT => Self::Variant(wit.variant(index)),
            ffi::WIT_TYPE_ENUM => Self::Enum(wit.enum_(index)),
            ffi::WIT_TYPE_OPTION => Self::Option(wit.option(index)),
            ffi::WIT_TYPE_RESULT => Self::Result(wit.result(index)),
            ffi::WIT_TYPE_LIST => Self::List(wit.list(index)),
            ffi::WIT_TYPE_FIXED_SIZE_LIST => Self::FixedSizeList(wit.fixed_size_list(index)),
            ffi::WIT_TYPE_FUTURE => Self::Future(wit.future(index)),
            ffi::WIT_TYPE_STREAM => Self::Stream(wit.stream(index)),
            ffi::WIT_TYPE_ALIAS => Self::Alias(wit.alias(index)),
            other => panic!("unknown type {other:#x}"),
        }
    }

    fn from_raw_opt(wit: Wit, raw: ffi::wit_type_t) -> Option<Type> {
        if raw & 0xff == 0xff {
            None
        } else {
            Some(Self::from_raw(wit, raw))
        }
    }
}

#[derive(Copy, Clone)]
pub struct Record {
    wit: Wit,
    ptr: &'static ffi::wit_record_t,
}

impl Record {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn fields(&self) -> impl ExactSizeIterator<Item = (&'static str, Type)> + '_ {
        unsafe {
            slice(self.ptr.fields, self.ptr.nfields)
                .iter()
                .map(|field| (to_str(field.name), Type::from_raw(self.wit, field.ty)))
        }
    }
}

impl fmt::Debug for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Record")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("fields", &self.fields().collect::<Vec<_>>())
            .finish()
    }
}

impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct Resource {
    ptr: &'static ffi::wit_resource_t,
}

impl Resource {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn drop(&self) -> unsafe extern "C" fn(u32) {
        self.ptr.drop.unwrap()
    }

    pub fn new(&self) -> Option<unsafe extern "C" fn(usize) -> u32> {
        self.ptr.new
    }

    pub fn rep(&self) -> Option<unsafe extern "C" fn(u32) -> usize> {
        self.ptr.rep
    }
}

impl fmt::Debug for Resource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Resource")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .finish_non_exhaustive()
    }
}

impl PartialEq for Resource {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct Flags {
    ptr: &'static ffi::wit_flags_t,
}

impl Flags {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn names(&self) -> impl ExactSizeIterator<Item = &'static str> {
        unsafe {
            slice(self.ptr.names, self.ptr.nnames)
                .iter()
                .map(|name| to_str(*name))
        }
    }
}

impl fmt::Debug for Flags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Flags")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("names", &self.names().collect::<Vec<_>>())
            .finish()
    }
}

impl PartialEq for Flags {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct Tuple {
    wit: Wit,
    ptr: &'static ffi::wit_tuple_t,
}

impl Tuple {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn types(&self) -> impl ExactSizeIterator<Item = Type> + '_ {
        unsafe {
            slice(self.ptr.types, self.ptr.ntypes)
                .iter()
                .map(|name| Type::from_raw(self.wit, *name))
        }
    }
}

impl fmt::Debug for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Tuple")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("types", &self.types().collect::<Vec<_>>())
            .finish()
    }
}

impl PartialEq for Tuple {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct Variant {
    wit: Wit,
    ptr: &'static ffi::wit_variant_t,
}

impl Variant {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn cases(&self) -> impl ExactSizeIterator<Item = (&'static str, Option<Type>)> + '_ {
        unsafe {
            slice(self.ptr.cases, self.ptr.ncases)
                .iter()
                .map(|case| (to_str(case.name), Type::from_raw_opt(self.wit, case.ty)))
        }
    }
}

impl fmt::Debug for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Variant")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("cases", &self.cases().collect::<Vec<_>>())
            .finish()
    }
}

impl PartialEq for Variant {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct Enum {
    ptr: &'static ffi::wit_enum_t,
}

impl Enum {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn names(&self) -> impl ExactSizeIterator<Item = &'static str> + '_ {
        unsafe {
            slice(self.ptr.names, self.ptr.nnames)
                .iter()
                .map(|name| to_str(*name))
        }
    }
}

impl fmt::Debug for Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Enum")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("names", &self.names().collect::<Vec<_>>())
            .finish()
    }
}

impl PartialEq for Enum {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct WitOption {
    wit: Wit,
    ptr: &'static ffi::wit_option_t,
}

impl WitOption {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn ty(&self) -> Type {
        Type::from_raw(self.wit, self.ptr.ty)
    }
}

impl fmt::Debug for WitOption {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WitOption")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("ty", &self.ty())
            .finish()
    }
}

impl PartialEq for WitOption {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct WitResult {
    wit: Wit,
    ptr: &'static ffi::wit_result_t,
}

impl WitResult {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn ok(&self) -> Option<Type> {
        Type::from_raw_opt(self.wit, self.ptr.ok)
    }

    pub fn err(&self) -> Option<Type> {
        Type::from_raw_opt(self.wit, self.ptr.err)
    }
}

impl fmt::Debug for WitResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WitResult")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("ok", &self.ok())
            .field("err", &self.err())
            .finish()
    }
}

impl PartialEq for WitResult {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct List {
    wit: Wit,
    ptr: &'static ffi::wit_list_t,
}

impl List {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn ty(&self) -> Type {
        Type::from_raw(self.wit, self.ptr.ty)
    }
}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("List")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("ty", &self.ty())
            .finish()
    }
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct FixedSizeList {
    wit: Wit,
    ptr: &'static ffi::wit_fixed_size_list_t,
}

impl FixedSizeList {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn len(&self) -> usize {
        self.ptr.size
    }

    pub fn ty(&self) -> Type {
        Type::from_raw(self.wit, self.ptr.ty)
    }
}

impl fmt::Debug for FixedSizeList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FixedSizeList")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("ty", &self.ty())
            .field("len", &self.len())
            .finish()
    }
}

impl PartialEq for FixedSizeList {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct Future {
    wit: Wit,
    ptr: &'static ffi::wit_future_t,
}

impl Future {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn ty(&self) -> Option<Type> {
        Type::from_raw_opt(self.wit, self.ptr.ty)
    }
}

impl fmt::Debug for Future {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Future")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("ty", &self.ty())
            .finish()
    }
}

impl PartialEq for Future {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct Stream {
    wit: Wit,
    ptr: &'static ffi::wit_stream_t,
}

impl Stream {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn ty(&self) -> Option<Type> {
        Type::from_raw_opt(self.wit, self.ptr.ty)
    }
}

impl fmt::Debug for Stream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Stream")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("ty", &self.ty())
            .finish()
    }
}

impl PartialEq for Stream {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

#[derive(Copy, Clone)]
pub struct Alias {
    wit: Wit,
    ptr: &'static ffi::wit_alias_t,
}

impl Alias {
    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn ty(&self) -> Type {
        Type::from_raw(self.wit, self.ptr.ty)
    }
}

impl fmt::Debug for Alias {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Alias")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("ty", &self.ty())
            .finish()
    }
}

impl PartialEq for Alias {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}
