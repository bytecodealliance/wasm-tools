use crate::Call;
use crate::ffi;
use std::ffi::{CStr, c_char, c_void};
use std::fmt;
use std::hash::{Hash, Hasher};

macro_rules! impl_extra_traits {
    ($name:ident) => {
        impl Hash for $name {
            fn hash<H: Hasher>(&self, hasher: &mut H) {
                std::ptr::hash(self.ptr, hasher)
            }
        }
        impl PartialEq for $name {
            fn eq(&self, other: &$name) -> bool {
                std::ptr::eq(self.ptr, other.ptr)
            }
        }
        impl Eq for $name {}

        unsafe impl Send for $name {}
        unsafe impl Sync for $name {}
    };
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Wit {
    ptr: &'static ffi::wit_t,
}

impl_extra_traits!(Wit);

impl Wit {
    pub(crate) unsafe fn from_raw(ptr: *const ffi::wit_t) -> Wit {
        unsafe {
            assert!((*ptr).version >= ffi::WIT_CURRENT_VERSION);
            Wit {
                ptr: ptr.as_ref().unwrap(),
            }
        }
    }

    fn raw_import_funcs(&self) -> &'static [ffi::wit_import_func_t] {
        unsafe { slice(self.ptr.import_funcs, self.ptr.num_import_funcs) }
    }

    pub fn import_func(&self, index: usize) -> ImportFunction {
        ImportFunction {
            wit: *self,
            ptr: &self.raw_import_funcs()[index],
        }
    }

    pub fn iter_import_funcs(&self) -> impl ExactSizeIterator<Item = ImportFunction> + Clone + '_ {
        self.raw_import_funcs().iter().map(|func| ImportFunction {
            wit: *self,
            ptr: func,
        })
    }

    pub fn get_import(&self, interface: Option<&str>, name: &str) -> Option<ImportFunction> {
        let mut funcs = self
            .iter_import_funcs()
            .filter(|f| f.interface() == interface && f.name() == name);
        let func = funcs.next()?;
        assert!(funcs.next().is_none());
        Some(func)
    }

    pub fn unwrap_import(&self, interface: Option<&str>, name: &str) -> ImportFunction {
        match self.get_import(interface, name) {
            Some(func) => func,
            None => match interface {
                Some(i) => panic!("no import function named {name:?} found in {i:?}"),
                None => panic!("no import function named {name:?}"),
            },
        }
    }

    fn raw_export_funcs(&self) -> &'static [ffi::wit_export_func_t] {
        unsafe { slice(self.ptr.export_funcs, self.ptr.num_export_funcs) }
    }

    pub fn export_func(&self, index: usize) -> ExportFunction {
        ExportFunction {
            wit: *self,
            ptr: &self.raw_export_funcs()[index],
        }
    }

    pub fn iter_export_funcs(&self) -> impl ExactSizeIterator<Item = ExportFunction> + Clone + '_ {
        self.raw_export_funcs().iter().map(|func| ExportFunction {
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

    pub fn iter_records(&self) -> impl ExactSizeIterator<Item = Record> + Clone + '_ {
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
            wit: *self,
            ptr: &self.raw_resources()[index],
        }
    }

    pub fn iter_resources(&self) -> impl ExactSizeIterator<Item = Resource> + Clone + '_ {
        self.raw_resources().iter().map(|resource| Resource {
            wit: *self,
            ptr: resource,
        })
    }

    fn raw_flags(&self) -> &'static [ffi::wit_flags_t] {
        unsafe { slice(self.ptr.flags, self.ptr.num_flags) }
    }

    pub fn flags(&self, index: usize) -> Flags {
        Flags {
            wit: *self,
            ptr: &self.raw_flags()[index],
        }
    }

    pub fn iter_flags(&self) -> impl ExactSizeIterator<Item = Flags> + Clone + '_ {
        self.raw_flags().iter().map(|flags| Flags {
            wit: *self,
            ptr: flags,
        })
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

    pub fn iter_tuples(&self) -> impl ExactSizeIterator<Item = Tuple> + Clone + '_ {
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

    pub fn iter_variants(&self) -> impl ExactSizeIterator<Item = Variant> + Clone + '_ {
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
            wit: *self,
            ptr: &self.raw_enums()[index],
        }
    }

    pub fn iter_enums(&self) -> impl ExactSizeIterator<Item = Enum> + Clone + '_ {
        self.raw_enums().iter().map(|e| Enum { wit: *self, ptr: e })
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

    pub fn iter_options(&self) -> impl ExactSizeIterator<Item = WitOption> + Clone + '_ {
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

    pub fn iter_results(&self) -> impl ExactSizeIterator<Item = WitResult> + Clone + '_ {
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

    pub fn iter_lists(&self) -> impl ExactSizeIterator<Item = List> + Clone + '_ {
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

    pub fn iter_fixed_size_lists(
        &self,
    ) -> impl ExactSizeIterator<Item = FixedSizeList> + Clone + '_ {
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

    pub fn iter_futures(&self) -> impl ExactSizeIterator<Item = Future> + Clone + '_ {
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

    pub fn iter_streams(&self) -> impl ExactSizeIterator<Item = Stream> + Clone + '_ {
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

    pub fn iter_aliases(&self) -> impl ExactSizeIterator<Item = Alias> + Clone + '_ {
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

pub struct PendingAsyncImportCall {
    pub subtask: u32,
    pub buffer: *mut u8,
}

#[derive(Copy, Clone)]
pub struct ImportFunction {
    wit: Wit,
    ptr: &'static ffi::wit_import_func_t,
}

impl_extra_traits!(ImportFunction);

impl ImportFunction {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.import_funcs) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn sync_import_impl(&self) -> ffi::wit_import_fn_t {
        self.ptr.impl_
    }

    pub fn async_import_impl(&self) -> ffi::wit_import_async_fn_t {
        self.ptr.async_impl
    }

    pub fn async_import_lift_impl(&self) -> ffi::wit_import_async_lift_fn_t {
        self.ptr.async_lift_impl
    }

    pub fn is_async(&self) -> bool {
        self.ptr.async_impl.is_some()
    }

    pub fn params(&self) -> impl ExactSizeIterator<Item = Type> + DoubleEndedIterator + Clone + '_ {
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

    pub fn call_import_sync(&self, cx: &mut impl Call) {
        let import_impl = self.sync_import_impl().unwrap();
        unsafe {
            let cx: *mut _ = cx;
            import_impl(cx.cast());
        }
    }

    #[cfg(feature = "async-raw")]
    pub unsafe fn call_import_async(&self, cx: &mut impl Call) -> Option<PendingAsyncImportCall> {
        use core::alloc::Layout;

        const STATUS_STARTING: u32 = 0;
        const STATUS_STARTED: u32 = 1;
        const STATUS_RETURNED: u32 = 2;

        let layout =
            Layout::from_size_align(self.ptr.async_abi_area_size, self.ptr.async_abi_area_align)
                .unwrap();
        let (status, buffer) = unsafe {
            let buffer = std::alloc::alloc(layout);
            cx.defer_deallocate(buffer, layout);
            let status = self.ptr.async_impl.unwrap()((&raw mut *cx).cast(), buffer.cast());
            (status, buffer)
        };
        let subtask = status >> 4;
        let status = status & 0xF;
        match status {
            STATUS_STARTING | STATUS_STARTED => Some(PendingAsyncImportCall { subtask, buffer }),
            STATUS_RETURNED => unsafe {
                self.lift_import_async_result(cx, buffer);
                None
            },
            _ => panic!("unexpected status from async import call: {status}"),
        }
    }

    #[cfg(feature = "async-raw")]
    pub unsafe fn lift_import_async_result(&self, cx: &mut impl Call, buffer: *mut u8) {
        unsafe { self.ptr.async_lift_impl.unwrap()((&raw mut *cx).cast(), buffer.cast()) };
    }

    #[cfg(feature = "async-runtime")]
    pub async fn call_import_async(&self, cx: &mut impl Call) {
        use core::alloc::Layout;
        use wit_bindgen::rt::async_support::Subtask;

        return DylibSubtask { ptr: self.ptr, cx }.call(()).await;

        struct DylibSubtask<C> {
            ptr: &'static ffi::wit_import_func_t,
            cx: *mut C,
        }

        unsafe impl<C> Subtask for DylibSubtask<C> {
            type Params = ();
            type ParamsLower = ();
            type Results = ();

            fn abi_layout(&self) -> Layout {
                Layout::from_size_align(self.ptr.async_abi_area_size, self.ptr.async_abi_area_align)
                    .unwrap()
            }

            fn results_offset(&self) -> usize {
                0
            }

            unsafe fn params_lower(&self, (): (), _: *mut u8) {}
            unsafe fn params_dealloc_lists(&self, (): ()) {}
            unsafe fn params_dealloc_lists_and_own(&self, (): ()) {}

            unsafe fn call_import(&self, (): (), ptr: *mut u8) -> u32 {
                unsafe {
                    let cx: *mut _ = self.cx;
                    self.ptr.async_impl.unwrap()(cx.cast(), ptr.cast())
                }
            }

            unsafe fn results_lift(&self, ptr: *mut u8) {
                unsafe {
                    let cx: *mut _ = self.cx;
                    self.ptr.async_lift_impl.unwrap()(cx.cast(), ptr.cast());
                }
            }
        }
    }
}

impl fmt::Debug for ImportFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ImportFunction")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("params", &self.params().collect::<Vec<_>>())
            .field("result", &self.result())
            .finish_non_exhaustive()
    }
}

#[derive(Copy, Clone)]
pub struct ExportFunction {
    wit: Wit,
    ptr: &'static ffi::wit_export_func_t,
}

impl_extra_traits!(ExportFunction);

impl ExportFunction {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.export_funcs) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn task_return(&self) -> ffi::wit_export_task_return_fn_t {
        self.ptr.task_return
    }

    pub fn call_task_return(&self, cx: &mut impl Call) {
        let task_return = self.task_return().unwrap();
        unsafe {
            let cx: *mut _ = cx;
            task_return(cx.cast());
        }
    }

    pub fn params(&self) -> impl ExactSizeIterator<Item = Type> + DoubleEndedIterator + Clone + '_ {
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

impl fmt::Debug for ExportFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExportFunction")
            .field("interface", &self.interface())
            .field("name", &self.name())
            .field("params", &self.params().collect::<Vec<_>>())
            .field("result", &self.result())
            .finish_non_exhaustive()
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

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
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

impl_extra_traits!(Record);

impl Record {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.records) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn fields(
        &self,
    ) -> impl ExactSizeIterator<Item = (&'static str, Type)> + DoubleEndedIterator + Clone + '_
    {
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

#[derive(Copy, Clone)]
pub struct Resource {
    wit: Wit,
    ptr: &'static ffi::wit_resource_t,
}

impl_extra_traits!(Resource);

impl Resource {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.resources) }
    }

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

#[derive(Copy, Clone)]
pub struct Flags {
    wit: Wit,
    ptr: &'static ffi::wit_flags_t,
}

impl_extra_traits!(Flags);

impl Flags {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.flags) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn names(
        &self,
    ) -> impl ExactSizeIterator<Item = &'static str> + DoubleEndedIterator + Clone {
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

#[derive(Copy, Clone)]
pub struct Tuple {
    wit: Wit,
    ptr: &'static ffi::wit_tuple_t,
}

impl_extra_traits!(Tuple);

impl Tuple {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.tuples) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn types(&self) -> impl ExactSizeIterator<Item = Type> + DoubleEndedIterator + Clone + '_ {
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

#[derive(Copy, Clone)]
pub struct Variant {
    wit: Wit,
    ptr: &'static ffi::wit_variant_t,
}

impl_extra_traits!(Variant);

impl Variant {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.variants) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn cases(
        &self,
    ) -> impl ExactSizeIterator<Item = (&'static str, Option<Type>)> + DoubleEndedIterator + Clone + '_
    {
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

#[derive(Copy, Clone)]
pub struct Enum {
    wit: Wit,
    ptr: &'static ffi::wit_enum_t,
}

impl_extra_traits!(Enum);

impl Enum {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.enums) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> &'static str {
        unsafe { to_str(self.ptr.name) }
    }

    pub fn names(
        &self,
    ) -> impl ExactSizeIterator<Item = &'static str> + DoubleEndedIterator + Clone + '_ {
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

#[derive(Copy, Clone)]
pub struct WitOption {
    wit: Wit,
    ptr: &'static ffi::wit_option_t,
}

impl_extra_traits!(WitOption);

impl WitOption {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.options) }
    }

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

#[derive(Copy, Clone)]
pub struct WitResult {
    wit: Wit,
    ptr: &'static ffi::wit_result_t,
}

impl_extra_traits!(WitResult);

impl WitResult {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.results) }
    }

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

#[derive(Copy, Clone)]
pub struct List {
    wit: Wit,
    ptr: &'static ffi::wit_list_t,
}

impl_extra_traits!(List);

impl List {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.lists) }
    }

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

#[derive(Copy, Clone)]
pub struct FixedSizeList {
    wit: Wit,
    ptr: &'static ffi::wit_fixed_size_list_t,
}

impl_extra_traits!(FixedSizeList);

impl FixedSizeList {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.fixed_size_lists) }
    }

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

#[derive(Copy, Clone)]
pub struct Future {
    wit: Wit,
    ptr: &'static ffi::wit_future_t,
}

impl_extra_traits!(Future);

impl Future {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.futures) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn ty(&self) -> Option<Type> {
        Type::from_raw_opt(self.wit, self.ptr.ty)
    }

    pub fn new(&self) -> unsafe extern "C" fn() -> u64 {
        self.ptr.new.unwrap()
    }

    pub fn read(&self) -> unsafe extern "C" fn(u32, *mut c_void) -> u32 {
        self.ptr.read.unwrap()
    }

    pub fn write(&self) -> unsafe extern "C" fn(u32, *const c_void) -> u32 {
        self.ptr.write.unwrap()
    }

    pub fn cancel_read(&self) -> unsafe extern "C" fn(u32) -> u32 {
        self.ptr.cancel_read.unwrap()
    }

    pub fn cancel_write(&self) -> unsafe extern "C" fn(u32) -> u32 {
        self.ptr.cancel_write.unwrap()
    }

    pub fn drop_readable(&self) -> unsafe extern "C" fn(u32) {
        self.ptr.drop_readable.unwrap()
    }

    pub fn drop_writable(&self) -> unsafe extern "C" fn(u32) {
        self.ptr.drop_writable.unwrap()
    }

    pub unsafe fn lift(&self, cx: &mut impl Call, buffer: *mut u8) {
        unsafe { self.ptr.lift.unwrap()((&raw mut *cx).cast(), buffer.cast()) };
    }

    pub unsafe fn lower(&self, cx: &mut impl Call, buffer: *mut u8) {
        unsafe { self.ptr.lower.unwrap()((&raw mut *cx).cast(), buffer.cast()) };
    }

    pub fn abi_payload_size(&self) -> usize {
        self.ptr.abi_payload_size
    }

    pub fn abi_payload_align(&self) -> usize {
        self.ptr.abi_payload_align
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

#[derive(Copy, Clone)]
pub struct Stream {
    wit: Wit,
    ptr: &'static ffi::wit_stream_t,
}

impl_extra_traits!(Stream);

impl Stream {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.streams) }
    }

    pub fn interface(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.interface) }
    }

    pub fn name(&self) -> Option<&'static str> {
        unsafe { opt_str(self.ptr.name) }
    }

    pub fn ty(&self) -> Option<Type> {
        Type::from_raw_opt(self.wit, self.ptr.ty)
    }

    pub fn new(&self) -> unsafe extern "C" fn() -> u64 {
        self.ptr.new.unwrap()
    }

    pub fn read(&self) -> unsafe extern "C" fn(u32, *mut c_void, usize) -> u32 {
        self.ptr.read.unwrap()
    }

    pub fn write(&self) -> unsafe extern "C" fn(u32, *const c_void, usize) -> u32 {
        self.ptr.write.unwrap()
    }

    pub fn cancel_read(&self) -> unsafe extern "C" fn(u32) -> u32 {
        self.ptr.cancel_read.unwrap()
    }

    pub fn cancel_write(&self) -> unsafe extern "C" fn(u32) -> u32 {
        self.ptr.cancel_write.unwrap()
    }

    pub fn drop_readable(&self) -> unsafe extern "C" fn(u32) {
        self.ptr.drop_readable.unwrap()
    }

    pub fn drop_writable(&self) -> unsafe extern "C" fn(u32) {
        self.ptr.drop_writable.unwrap()
    }

    pub unsafe fn lift(&self, cx: &mut impl Call, buffer: *mut u8) {
        unsafe { self.ptr.lift.unwrap()((&raw mut *cx).cast(), buffer.cast()) };
    }

    pub unsafe fn lower(&self, cx: &mut impl Call, buffer: *mut u8) {
        unsafe { self.ptr.lower.unwrap()((&raw mut *cx).cast(), buffer.cast()) };
    }

    pub fn abi_payload_size(&self) -> usize {
        self.ptr.abi_payload_size
    }

    pub fn abi_payload_align(&self) -> usize {
        self.ptr.abi_payload_align
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

#[derive(Copy, Clone)]
pub struct Alias {
    wit: Wit,
    ptr: &'static ffi::wit_alias_t,
}

impl_extra_traits!(Alias);

impl Alias {
    pub fn index(&self) -> usize {
        unsafe { (&raw const *self.ptr).offset_from_unsigned(self.wit.ptr.aliases) }
    }

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
