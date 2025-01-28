use crate::{encode_section, ComponentSection, ComponentSectionId, ComponentValType, Encode};
use alloc::vec::Vec;

/// Represents options for canonical function definitions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalOption {
    /// The string types in the function signature are UTF-8 encoded.
    UTF8,
    /// The string types in the function signature are UTF-16 encoded.
    UTF16,
    /// The string types in the function signature are compact UTF-16 encoded.
    CompactUTF16,
    /// The memory to use if the lifting or lowering of a function requires memory access.
    ///
    /// The value is an index to a core memory.
    Memory(u32),
    /// The realloc function to use if the lifting or lowering of a function requires memory
    /// allocation.
    ///
    /// The value is an index to a core function of type `(func (param i32 i32 i32 i32) (result i32))`.
    Realloc(u32),
    /// The post-return function to use if the lifting of a function requires
    /// cleanup after the function returns.
    PostReturn(u32),
    /// Indicates that specified function should be lifted or lowered using the `async` ABI.
    Async,
    /// The function to use if the async lifting of a function should receive task/stream/future progress events
    /// using a callback.
    Callback(u32),
}

impl Encode for CanonicalOption {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::UTF8 => sink.push(0x00),
            Self::UTF16 => sink.push(0x01),
            Self::CompactUTF16 => sink.push(0x02),
            Self::Memory(idx) => {
                sink.push(0x03);
                idx.encode(sink);
            }
            Self::Realloc(idx) => {
                sink.push(0x04);
                idx.encode(sink);
            }
            Self::PostReturn(idx) => {
                sink.push(0x05);
                idx.encode(sink);
            }
            Self::Async => {
                sink.push(0x06);
            }
            Self::Callback(idx) => {
                sink.push(0x07);
                idx.encode(sink);
            }
        }
    }
}

/// An encoder for the canonical function section of WebAssembly components.
///
/// # Example
///
/// ```
/// use wasm_encoder::{Component, CanonicalFunctionSection, CanonicalOption};
///
/// let mut functions = CanonicalFunctionSection::new();
/// functions.lift(0, 0, [CanonicalOption::UTF8]);
///
/// let mut component = Component::new();
/// component.section(&functions);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct CanonicalFunctionSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl CanonicalFunctionSection {
    /// Construct a new component function section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of functions in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define a function that will lift a core WebAssembly function to the canonical ABI.
    pub fn lift<O>(&mut self, core_func_index: u32, type_index: u32, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        let options = options.into_iter();
        self.bytes.push(0x00);
        self.bytes.push(0x00);
        core_func_index.encode(&mut self.bytes);
        options.len().encode(&mut self.bytes);
        for option in options {
            option.encode(&mut self.bytes);
        }
        type_index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Define a function that will lower a canonical ABI function to a core WebAssembly function.
    pub fn lower<O>(&mut self, func_index: u32, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        let options = options.into_iter();
        self.bytes.push(0x01);
        self.bytes.push(0x00);
        func_index.encode(&mut self.bytes);
        options.len().encode(&mut self.bytes);
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Defines a function which will create an owned handle to the resource
    /// specified by `ty_index`.
    pub fn resource_new(&mut self, ty_index: u32) -> &mut Self {
        self.bytes.push(0x02);
        ty_index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function which will drop the specified type of handle.
    pub fn resource_drop(&mut self, ty_index: u32) -> &mut Self {
        self.bytes.push(0x03);
        ty_index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function which will return the representation of the specified
    /// resource type.
    pub fn resource_rep(&mut self, ty_index: u32) -> &mut Self {
        self.bytes.push(0x04);
        ty_index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function which will spawns a new thread by invoking a shared
    /// function of type `ty_index`.
    pub fn thread_spawn(&mut self, ty_index: u32) -> &mut Self {
        self.bytes.push(0x05);
        ty_index.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function which will return the number of threads that can be
    /// expected to execute concurrently.
    pub fn thread_hw_concurrency(&mut self) -> &mut Self {
        self.bytes.push(0x06);
        self.num_added += 1;
        self
    }

    /// Defines a function which tells the host to enable or disable
    /// backpressure for the caller's instance.  When backpressure is enabled,
    /// the host must not start any new calls to that instance until
    /// backpressure is disabled.
    pub fn task_backpressure(&mut self) -> &mut Self {
        self.bytes.push(0x08);
        self.num_added += 1;
        self
    }

    /// Defines a function which returns a result to the caller of a lifted
    /// export function.  This allows the callee to continue executing after
    /// returning a result.
    pub fn task_return(&mut self, ty: Option<impl Into<ComponentValType>>) -> &mut Self {
        self.bytes.push(0x09);
        if let Some(ty) = ty {
            self.bytes.push(0x00);
            ty.into().encode(&mut self.bytes);
        } else {
            self.bytes.push(0x01);
            0_usize.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Defines a function which waits for at least one outstanding async
    /// task/stream/future to make progress, returning the first such event.
    ///
    /// If `async_` is true, the caller instance may be reentered.
    pub fn task_wait(&mut self, async_: bool, memory: u32) -> &mut Self {
        self.bytes.push(0x0a);
        self.bytes.push(if async_ { 1 } else { 0 });
        memory.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function which checks whether any outstanding async
    /// task/stream/future has made progress.  Unlike `task.wait`, this does not
    /// block and may return nothing if no such event has occurred.
    ///
    /// If `async_` is true, the caller instance may be reentered.
    pub fn task_poll(&mut self, async_: bool, memory: u32) -> &mut Self {
        self.bytes.push(0x0b);
        self.bytes.push(if async_ { 1 } else { 0 });
        memory.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function which yields control to the host so that other tasks
    /// are able to make progress, if any.
    ///
    /// If `async_` is true, the caller instance may be reentered.
    pub fn task_yield(&mut self, async_: bool) -> &mut Self {
        self.bytes.push(0x0c);
        self.bytes.push(if async_ { 1 } else { 0 });
        self.num_added += 1;
        self
    }

    /// Defines a function to drop a specified task which has completed.
    pub fn subtask_drop(&mut self) -> &mut Self {
        self.bytes.push(0x0d);
        self.num_added += 1;
        self
    }

    /// Defines a function to create a new `stream` handle of the specified
    /// type.
    pub fn stream_new(&mut self, ty: u32) -> &mut Self {
        self.bytes.push(0x0e);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function to read from a `stream` of the specified type.
    pub fn stream_read<O>(&mut self, ty: u32, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        self.bytes.push(0x0f);
        ty.encode(&mut self.bytes);
        let options = options.into_iter();
        options.len().encode(&mut self.bytes);
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Defines a function to write to a `stream` of the specified type.
    pub fn stream_write<O>(&mut self, ty: u32, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        self.bytes.push(0x10);
        ty.encode(&mut self.bytes);
        let options = options.into_iter();
        options.len().encode(&mut self.bytes);
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Defines a function to cancel an in-progress read from a `stream` of the
    /// specified type.
    pub fn stream_cancel_read(&mut self, ty: u32, async_: bool) -> &mut Self {
        self.bytes.push(0x11);
        ty.encode(&mut self.bytes);
        self.bytes.push(if async_ { 1 } else { 0 });
        self.num_added += 1;
        self
    }

    /// Defines a function to cancel an in-progress write to a `stream` of the
    /// specified type.
    pub fn stream_cancel_write(&mut self, ty: u32, async_: bool) -> &mut Self {
        self.bytes.push(0x12);
        ty.encode(&mut self.bytes);
        self.bytes.push(if async_ { 1 } else { 0 });
        self.num_added += 1;
        self
    }

    /// Defines a function to close the readable end of a `stream` of the
    /// specified type.
    pub fn stream_close_readable(&mut self, ty: u32) -> &mut Self {
        self.bytes.push(0x13);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function to close the writable end of a `stream` of the
    /// specified type.
    pub fn stream_close_writable(&mut self, ty: u32) -> &mut Self {
        self.bytes.push(0x14);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function to create a new `future` handle of the specified
    /// type.
    pub fn future_new(&mut self, ty: u32) -> &mut Self {
        self.bytes.push(0x15);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function to read from a `future` of the specified type.
    pub fn future_read<O>(&mut self, ty: u32, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        self.bytes.push(0x16);
        ty.encode(&mut self.bytes);
        let options = options.into_iter();
        options.len().encode(&mut self.bytes);
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Defines a function to write to a `future` of the specified type.
    pub fn future_write<O>(&mut self, ty: u32, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        self.bytes.push(0x17);
        ty.encode(&mut self.bytes);
        let options = options.into_iter();
        options.len().encode(&mut self.bytes);
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Defines a function to cancel an in-progress read from a `future` of the
    /// specified type.
    pub fn future_cancel_read(&mut self, ty: u32, async_: bool) -> &mut Self {
        self.bytes.push(0x18);
        ty.encode(&mut self.bytes);
        self.bytes.push(if async_ { 1 } else { 0 });
        self.num_added += 1;
        self
    }

    /// Defines a function to cancel an in-progress write to a `future` of the
    /// specified type.
    pub fn future_cancel_write(&mut self, ty: u32, async_: bool) -> &mut Self {
        self.bytes.push(0x19);
        ty.encode(&mut self.bytes);
        self.bytes.push(if async_ { 1 } else { 0 });
        self.num_added += 1;
        self
    }

    /// Defines a function to close the readable end of a `future` of the
    /// specified type.
    pub fn future_close_readable(&mut self, ty: u32) -> &mut Self {
        self.bytes.push(0x1a);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function to close the writable end of a `future` of the
    /// specified type.
    pub fn future_close_writable(&mut self, ty: u32) -> &mut Self {
        self.bytes.push(0x1b);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines a function to create a new `error-context` with a specified
    /// debug message.
    pub fn error_context_new<O>(&mut self, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        self.bytes.push(0x1c);
        let options = options.into_iter();
        options.len().encode(&mut self.bytes);
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Defines a function to get the debug message for a specified
    /// `error-context`.
    ///
    /// Note that the debug message might not necessarily match what was passed
    /// to `error-context.new`.
    pub fn error_context_debug_message<O>(&mut self, options: O) -> &mut Self
    where
        O: IntoIterator<Item = CanonicalOption>,
        O::IntoIter: ExactSizeIterator,
    {
        self.bytes.push(0x1d);
        let options = options.into_iter();
        options.len().encode(&mut self.bytes);
        for option in options {
            option.encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Defines a function to drop a specified `error-context`.
    pub fn error_context_drop(&mut self) -> &mut Self {
        self.bytes.push(0x1e);
        self.num_added += 1;
        self
    }
}

impl Encode for CanonicalFunctionSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl ComponentSection for CanonicalFunctionSection {
    fn id(&self) -> u8 {
        ComponentSectionId::CanonicalFunction.into()
    }
}
