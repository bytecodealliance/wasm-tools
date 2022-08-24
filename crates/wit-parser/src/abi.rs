use crate::sizealign::align_to;
use crate::{
    Enum, Expected, Flags, FlagsRepr, Function, Int, Interface, Record, ResourceId, Tuple, Type,
    TypeDefKind, TypeId, Union, Variant,
};
use std::mem;

/// A raw WebAssembly signature with params and results.
#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct WasmSignature {
    /// The WebAssembly parameters of this function.
    pub params: Vec<WasmType>,

    /// The WebAssembly results of this function.
    pub results: Vec<WasmType>,

    /// Whether or not this signature is passing all of its parameters
    /// indirectly through a pointer within `params`.
    ///
    /// Note that `params` still reflects the true wasm paramters of this
    /// function, this is auxiliary information for code generators if
    /// necessary.
    pub indirect_params: bool,

    /// Whether or not this signature is using a return pointer to store the
    /// result of the function, which is reflected either in `params` or
    /// `results` depending on the context this function is used (e.g. an import
    /// or an export).
    pub retptr: bool,
}

/// Enumerates wasm types used by interface types when lowering/lifting.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WasmType {
    I32,
    I64,
    F32,
    F64,
    // NOTE: we don't lower interface types to any other Wasm type,
    // e.g. externref, so we don't need to define them here.
}

fn join(a: WasmType, b: WasmType) -> WasmType {
    use WasmType::*;

    match (a, b) {
        (I32, I32) | (I64, I64) | (F32, F32) | (F64, F64) => a,

        (I32, F32) | (F32, I32) => I32,

        (_, I64 | F64) | (I64 | F64, _) => I64,
    }
}

impl From<Int> for WasmType {
    fn from(i: Int) -> WasmType {
        match i {
            Int::U8 | Int::U16 | Int::U32 => WasmType::I32,
            Int::U64 => WasmType::I64,
        }
    }
}

// Helper macro for defining instructions without having to have tons of
// exhaustive `match` statements to update
macro_rules! def_instruction {
    (
        $( #[$enum_attr:meta] )*
        pub enum $name:ident<'a> {
            $(
                $( #[$attr:meta] )*
                $variant:ident $( {
                    $($field:ident : $field_ty:ty $(,)* )*
                } )?
                    :
                [$num_popped:expr] => [$num_pushed:expr],
            )*
        }
    ) => {
        $( #[$enum_attr] )*
        pub enum $name<'a> {
            $(
                $( #[$attr] )*
                $variant $( {
                    $(
                        $field : $field_ty,
                    )*
                } )? ,
            )*
        }

        impl $name<'_> {
            /// How many operands does this instruction pop from the stack?
            #[allow(unused_variables)]
            pub fn operands_len(&self) -> usize {
                match self {
                    $(
                        Self::$variant $( {
                            $(
                                $field,
                            )*
                        } )? => $num_popped,
                    )*
                }
            }

            /// How many results does this instruction push onto the stack?
            #[allow(unused_variables)]
            pub fn results_len(&self) -> usize {
                match self {
                    $(
                        Self::$variant $( {
                            $(
                                $field,
                            )*
                        } )? => $num_pushed,
                    )*
                }
            }
        }
    };
}

def_instruction! {
    #[derive(Debug)]
    pub enum Instruction<'a> {
        /// Acquires the specified parameter and places it on the stack.
        /// Depending on the context this may refer to wasm parameters or
        /// interface types parameters.
        GetArg { nth: usize } : [0] => [1],

        // Integer const/manipulation instructions

        /// Pushes the constant `val` onto the stack.
        I32Const { val: i32 } : [0] => [1],
        /// Casts the top N items on the stack using the `Bitcast` enum
        /// provided. Consumes the same number of operands that this produces.
        Bitcasts { casts: &'a [Bitcast] } : [casts.len()] => [casts.len()],
        /// Pushes a number of constant zeros for each wasm type on the stack.
        ConstZero { tys: &'a [WasmType] } : [0] => [tys.len()],

        // Memory load/store instructions

        /// Pops an `i32` from the stack and loads a little-endian `i32` from
        /// it, using the specified constant offset.
        I32Load { offset: i32 } : [1] => [1],
        /// Pops an `i32` from the stack and loads a little-endian `i8` from
        /// it, using the specified constant offset. The value loaded is the
        /// zero-extended to 32-bits
        I32Load8U { offset: i32 } : [1] => [1],
        /// Pops an `i32` from the stack and loads a little-endian `i8` from
        /// it, using the specified constant offset. The value loaded is the
        /// sign-extended to 32-bits
        I32Load8S { offset: i32 } : [1] => [1],
        /// Pops an `i32` from the stack and loads a little-endian `i16` from
        /// it, using the specified constant offset. The value loaded is the
        /// zero-extended to 32-bits
        I32Load16U { offset: i32 } : [1] => [1],
        /// Pops an `i32` from the stack and loads a little-endian `i16` from
        /// it, using the specified constant offset. The value loaded is the
        /// sign-extended to 32-bits
        I32Load16S { offset: i32 } : [1] => [1],
        /// Pops an `i32` from the stack and loads a little-endian `i64` from
        /// it, using the specified constant offset.
        I64Load { offset: i32 } : [1] => [1],
        /// Pops an `i32` from the stack and loads a little-endian `f32` from
        /// it, using the specified constant offset.
        F32Load { offset: i32 } : [1] => [1],
        /// Pops an `i32` from the stack and loads a little-endian `f64` from
        /// it, using the specified constant offset.
        F64Load { offset: i32 } : [1] => [1],

        /// Pops an `i32` address from the stack and then an `i32` value.
        /// Stores the value in little-endian at the pointer specified plus the
        /// constant `offset`.
        I32Store { offset: i32 } : [2] => [0],
        /// Pops an `i32` address from the stack and then an `i32` value.
        /// Stores the low 8 bits of the value in little-endian at the pointer
        /// specified plus the constant `offset`.
        I32Store8 { offset: i32 } : [2] => [0],
        /// Pops an `i32` address from the stack and then an `i32` value.
        /// Stores the low 16 bits of the value in little-endian at the pointer
        /// specified plus the constant `offset`.
        I32Store16 { offset: i32 } : [2] => [0],
        /// Pops an `i32` address from the stack and then an `i64` value.
        /// Stores the value in little-endian at the pointer specified plus the
        /// constant `offset`.
        I64Store { offset: i32 } : [2] => [0],
        /// Pops an `i32` address from the stack and then an `f32` value.
        /// Stores the value in little-endian at the pointer specified plus the
        /// constant `offset`.
        F32Store { offset: i32 } : [2] => [0],
        /// Pops an `i32` address from the stack and then an `f64` value.
        /// Stores the value in little-endian at the pointer specified plus the
        /// constant `offset`.
        F64Store { offset: i32 } : [2] => [0],

        // Scalar lifting/lowering

        /// Converts an interface type `char` value to a 32-bit integer
        /// representing the unicode scalar value.
        I32FromChar : [1] => [1],
        /// Converts an interface type `u64` value to a wasm `i64`.
        I64FromU64 : [1] => [1],
        /// Converts an interface type `s64` value to a wasm `i64`.
        I64FromS64 : [1] => [1],
        /// Converts an interface type `u32` value to a wasm `i32`.
        I32FromU32 : [1] => [1],
        /// Converts an interface type `s32` value to a wasm `i32`.
        I32FromS32 : [1] => [1],
        /// Converts an interface type `u16` value to a wasm `i32`.
        I32FromU16 : [1] => [1],
        /// Converts an interface type `s16` value to a wasm `i32`.
        I32FromS16 : [1] => [1],
        /// Converts an interface type `u8` value to a wasm `i32`.
        I32FromU8 : [1] => [1],
        /// Converts an interface type `s8` value to a wasm `i32`.
        I32FromS8 : [1] => [1],
        /// Conversion an interface type `f32` value to a wasm `f32`.
        ///
        /// This may be a noop for some implementations, but it's here in case the
        /// native language representation of `f32` is different than the wasm
        /// representation of `f32`.
        F32FromFloat32 : [1] => [1],
        /// Conversion an interface type `f64` value to a wasm `f64`.
        ///
        /// This may be a noop for some implementations, but it's here in case the
        /// native language representation of `f64` is different than the wasm
        /// representation of `f64`.
        F64FromFloat64 : [1] => [1],

        /// Converts a native wasm `i32` to an interface type `s8`.
        ///
        /// This will truncate the upper bits of the `i32`.
        S8FromI32 : [1] => [1],
        /// Converts a native wasm `i32` to an interface type `u8`.
        ///
        /// This will truncate the upper bits of the `i32`.
        U8FromI32 : [1] => [1],
        /// Converts a native wasm `i32` to an interface type `s16`.
        ///
        /// This will truncate the upper bits of the `i32`.
        S16FromI32 : [1] => [1],
        /// Converts a native wasm `i32` to an interface type `u16`.
        ///
        /// This will truncate the upper bits of the `i32`.
        U16FromI32 : [1] => [1],
        /// Converts a native wasm `i32` to an interface type `s32`.
        S32FromI32 : [1] => [1],
        /// Converts a native wasm `i32` to an interface type `u32`.
        U32FromI32 : [1] => [1],
        /// Converts a native wasm `i64` to an interface type `s64`.
        S64FromI64 : [1] => [1],
        /// Converts a native wasm `i64` to an interface type `u64`.
        U64FromI64 : [1] => [1],
        /// Converts a native wasm `i32` to an interface type `char`.
        ///
        /// It's safe to assume that the `i32` is indeed a valid unicode code point.
        CharFromI32 : [1] => [1],
        /// Converts a native wasm `f32` to an interface type `f32`.
        Float32FromF32 : [1] => [1],
        /// Converts a native wasm `f64` to an interface type `f64`.
        Float64FromF64 : [1] => [1],

        /// Creates a `bool` from an `i32` input, trapping if the `i32` isn't
        /// zero or one.
        BoolFromI32 : [1] => [1],
        /// Creates an `i32` from a `bool` input, must return 0 or 1.
        I32FromBool : [1] => [1],

        /// Creates a "unit" value from nothing.
        UnitLift : [0] => [1],
        /// Consumes a "unit" value and returns nothing.
        UnitLower : [1] => [0],

        // Handles

        /// Converts a "borrowed" handle into a wasm `i32` value.
        ///
        /// > **Note**: this documentation is outdated and does not reflect the
        /// > current implementation of the canonical ABI. This needs to be
        /// > updated.
        ///
        /// A "borrowed" handle in this case means one where ownership is not
        /// being relinquished. This is only used for lowering interface types
        /// parameters.
        ///
        /// Situations that this is used are:
        ///
        /// * A wasm exported function receives, as a parameter, handles defined
        ///   by the wasm module itself. This is effectively proof of ownership
        ///   by an external caller (be it host or wasm module) and the
        ///   ownership of the handle still lies with the caller. The wasm
        ///   module is only receiving a reference to the resource.
        ///
        /// * A wasm module is calling an import with a handle defined by the
        ///   import's module. Sort of the converse of the previous case this
        ///   means that the wasm module is handing out a reference to a
        ///   resource that it owns. The type in the wasm module, for example,
        ///   needs to reflect this.
        ///
        /// This instruction is not used for return values in either
        /// export/import positions.
        I32FromBorrowedHandle { ty: ResourceId } : [1] => [1],

        /// Converts an "owned" handle into a wasm `i32` value.
        ///
        /// > **Note**: this documentation is outdated and does not reflect the
        /// > current implementation of the canonical ABI. This needs to be
        /// > updated.
        ///
        /// This conversion is used for handle values which are crossing a
        /// module boundary for perhaps the first time. Some example cases of
        /// when this conversion is used are:
        ///
        /// * When a host defines a function to be imported, returned handles
        ///   use this instruction. Handles being returned to wasm a granting a
        ///   capability, which means that this new capability is typically
        ///   wrapped up in a new integer descriptor.
        ///
        /// * When a wasm module calls an imported function with a type defined
        ///   by itself, then it's granting a capability to the callee. This
        ///   means that the wasm module's type is being granted for the first
        ///   time, possibly, so it needs to be an owned value that's consumed.
        ///   Note that this doesn't actually happen with `*.witx` today due to
        ///   the lack of handle type imports.
        ///
        /// * When a wasm module export returns a handle defined within the
        ///   module, then it's similar to calling an imported function with
        ///   that handle. The capability is being granted to the caller of the
        ///   export, so the owned value is wrapped up in an `i32`.
        ///
        /// * When a host is calling a wasm module with a capability defined by
        ///   the host, its' similar to the host import returning a capability.
        ///   This would be granting the wasm module with the capability so an
        ///   owned version with a fresh handle is passed to the wasm module.
        ///   Note that this doesn't happen today with `*.witx` due to the lack
        ///   of handle type imports.
        ///
        /// Basically this instruction is used for handle->wasm conversions
        /// depending on the calling context and where the handle type in
        /// question was defined.
        I32FromOwnedHandle { ty: ResourceId } : [1] => [1],

        /// Converts a native wasm `i32` into an owned handle value.
        ///
        /// > **Note**: this documentation is outdated and does not reflect the
        /// > current implementation of the canonical ABI. This needs to be
        /// > updated.
        ///
        /// This is the converse of `I32FromOwnedHandle` and is used in similar
        /// situations:
        ///
        /// * A host definition of an import receives a handle defined in the
        ///   module itself.
        /// * A wasm module calling an import receives a handle defined by the
        ///   import.
        /// * A wasm module's export receives a handle defined by an external
        ///   module.
        /// * A host calling a wasm export receives a handle defined in the
        ///   module.
        ///
        /// Note that like `I32FromOwnedHandle` the first and third bullets
        /// above don't happen today because witx can't express type imports
        /// just yet.
        HandleOwnedFromI32 { ty: ResourceId } : [1] => [1],

        /// Converts a native wasm `i32` into a borrowedhandle value.
        ///
        /// > **Note**: this documentation is outdated and does not reflect the
        /// > current implementation of the canonical ABI. This needs to be
        /// > updated.
        ///
        /// This is the converse of `I32FromBorrowedHandle` and is used in similar
        /// situations:
        ///
        /// * An exported wasm function receives, as a parameter, a handle that
        ///   is defined by the wasm module.
        /// * An host-defined imported function is receiving a handle, as a
        ///   parameter, that is defined by the host itself.
        HandleBorrowedFromI32 { ty: ResourceId } : [1] => [1],

        // lists

        /// Lowers a list where the element's layout in the native language is
        /// expected to match the canonical ABI definition of interface types.
        ///
        /// Pops a list value from the stack and pushes the pointer/length onto
        /// the stack. If `realloc` is set to `Some` then this is expected to
        /// *consume* the list which means that the data needs to be copied. An
        /// allocation/copy is expected when:
        ///
        /// * A host is calling a wasm export with a list (it needs to copy the
        ///   list in to the callee's module, allocating space with `realloc`)
        /// * A wasm export is returning a list (it's expected to use `realloc`
        ///   to give ownership of the list to the caller.
        /// * A host is returning a list in a import definition, meaning that
        ///   space needs to be allocated in the caller with `realloc`).
        ///
        /// A copy does not happen (e.g. `realloc` is `None`) when:
        ///
        /// * A wasm module calls an import with the list. In this situation
        ///   it's expected the caller will know how to access this module's
        ///   memory (e.g. the host has raw access or wasm-to-wasm communication
        ///   would copy the list).
        ///
        /// If `realloc` is `Some` then the adapter is not responsible for
        /// cleaning up this list because the other end is receiving the
        /// allocation. If `realloc` is `None` then the adapter is responsible
        /// for cleaning up any temporary allocation it created, if any.
        ListCanonLower {
            element: &'a Type,
            realloc: Option<&'a str>,
        } : [1] => [2],

        /// Same as `ListCanonLower`, but used for strings
        StringLower {
            realloc: Option<&'a str>,
        } : [1] => [2],

        /// Lowers a list where the element's layout in the native language is
        /// not expected to match the canonical ABI definition of interface
        /// types.
        ///
        /// Pops a list value from the stack and pushes the pointer/length onto
        /// the stack. This operation also pops a block from the block stack
        /// which is used as the iteration body of writing each element of the
        /// list consumed.
        ///
        /// The `realloc` field here behaves the same way as `ListCanonLower`.
        /// It's only set to `None` when a wasm module calls a declared import.
        /// Otherwise lowering in other contexts requires allocating memory for
        /// the receiver to own.
        ListLower {
            element: &'a Type,
            realloc: Option<&'a str>,
        } : [1] => [2],

        /// Lifts a list which has a canonical representation into an interface
        /// types value.
        ///
        /// The term "canonical" representation here means that the
        /// representation of the interface types value in the native language
        /// exactly matches the canonical ABI definition of the type.
        ///
        /// This will consume two `i32` values from the stack, a pointer and a
        /// length, and then produces an interface value list. If the `free`
        /// field is set to `Some` then the pointer/length should be considered
        /// an owned allocation and need to be deallocated by the receiver. If
        /// it is set to `None` then a view is provided but it does not need to
        /// be deallocated.
        ///
        /// The `free` field is set to `Some` in similar situations as described
        /// by `ListCanonLower`. If `free` is `Some` then the memory must be
        /// deallocated after the lifted list is done being consumed. If it is
        /// `None` then the receiver of the lifted list does not own the memory
        /// and must leave the memory as-is.
        ListCanonLift {
            element: &'a Type,
            free: Option<&'a str>,
            ty: TypeId,
        } : [2] => [1],

        /// Same as `ListCanonLift`, but used for strings
        StringLift {
            free: Option<&'a str>,
        } : [2] => [1],

        /// Lifts a list which into an interface types value.
        ///
        /// This will consume two `i32` values from the stack, a pointer and a
        /// length, and then produces an interface value list. Note that the
        /// pointer/length popped are **owned** and need to be deallocated with
        /// the wasm `free` function when the list is no longer needed.
        ///
        /// This will also pop a block from the block stack which is how to
        /// read each individual element from the list.
        ListLift {
            element: &'a Type,
            free: Option<&'a str>,
            ty: TypeId,
        } : [2] => [1],

        /// Pushes an operand onto the stack representing the list item from
        /// each iteration of the list.
        ///
        /// This is only used inside of blocks related to lowering lists.
        IterElem { element: &'a Type } : [0] => [1],

        /// Pushes an operand onto the stack representing the base pointer of
        /// the next element in a list.
        ///
        /// This is used for both lifting and lowering lists.
        IterBasePointer : [0] => [1],

        // records

        /// Pops a record value off the stack, decomposes the record to all of
        /// its fields, and then pushes the fields onto the stack.
        RecordLower {
            record: &'a Record,
            name: &'a str,
            ty: TypeId,
        } : [1] => [record.fields.len()],

        /// Pops all fields for a record off the stack and then composes them
        /// into a record.
        RecordLift {
            record: &'a Record,
            name: &'a str,
            ty: TypeId,
        } : [record.fields.len()] => [1],

        /// Pops a tuple value off the stack, decomposes the tuple to all of
        /// its fields, and then pushes the fields onto the stack.
        TupleLower {
            tuple: &'a Tuple,
            ty: TypeId,
        } : [1] => [tuple.types.len()],

        /// Pops all fields for a tuple off the stack and then composes them
        /// into a tuple.
        TupleLift {
            tuple: &'a Tuple,
            ty: TypeId,
        } : [tuple.types.len()] => [1],

        /// Converts a language-specific record-of-bools to a list of `i32`.
        FlagsLower {
            flags: &'a Flags,
            name: &'a str,
            ty: TypeId,
        } : [1] => [flags.repr().count()],
        /// Converts a list of native wasm `i32` to a language-specific
        /// record-of-bools.
        FlagsLift {
            flags: &'a Flags,
            name: &'a str,
            ty: TypeId,
        } : [flags.repr().count()] => [1],

        // variants

        /// This is a special instruction used for `VariantLower`
        /// instruction to determine the name of the payload, if present, to use
        /// within each block.
        ///
        /// Each sub-block will have this be the first instruction, and if it
        /// lowers a payload it will expect something bound to this name.
        VariantPayloadName : [0] => [1],

        /// Pops a variant off the stack as well as `ty.cases.len()` blocks
        /// from the code generator. Uses each of those blocks and the value
        /// from the stack to produce `nresults` of items.
        VariantLower {
            variant: &'a Variant,
            name: &'a str,
            ty: TypeId,
            results: &'a [WasmType],
        } : [1] => [results.len()],

        /// Pops an `i32` off the stack as well as `ty.cases.len()` blocks
        /// from the code generator. Uses each of those blocks and the value
        /// from the stack to produce a final variant.
        VariantLift {
            variant: &'a Variant,
            name: &'a str,
            ty: TypeId,
        } : [1] => [1],

        /// Same as `VariantLower`, except used for unions.
        UnionLower {
            union: &'a Union,
            name: &'a str,
            ty: TypeId,
            results: &'a [WasmType],
        } : [1] => [results.len()],

        /// Same as `VariantLift`, except used for unions.
        UnionLift {
            union: &'a Union,
            name: &'a str,
            ty: TypeId,
        } : [1] => [1],

        /// Pops an enum off the stack and pushes the `i32` representation.
        EnumLower {
            enum_: &'a Enum,
            name: &'a str,
            ty: TypeId,
        } : [1] => [1],

        /// Pops an `i32` off the stack and lifts it into the `enum` specified.
        EnumLift {
            enum_: &'a Enum,
            name: &'a str,
            ty: TypeId,
        } : [1] => [1],

        /// Specialization of `VariantLower` for specifically `option<T>` types,
        /// otherwise behaves the same as `VariantLower` (e.g. two blocks for
        /// the two cases.
        OptionLower {
            payload: &'a Type,
            ty: TypeId,
            results: &'a [WasmType],
        } : [1] => [results.len()],

        /// Specialization of `VariantLift` for specifically the `option<T>`
        /// type. Otherwise behaves the same as the `VariantLift` instruction
        /// with two blocks for the lift.
        OptionLift {
            payload: &'a Type,
            ty: TypeId,
        } : [1] => [1],

        /// Specialization of `VariantLower` for specifically `expected<T, E>`
        /// types, otherwise behaves the same as `VariantLower` (e.g. two blocks
        /// for the two cases.
        ExpectedLower {
            expected: &'a Expected,
            ty: TypeId,
            results: &'a [WasmType],
        } : [1] => [results.len()],

        /// Specialization of `VariantLift` for specifically the `expected<T,
        /// E>` type. Otherwise behaves the same as the `VariantLift`
        /// instruction with two blocks for the lift.
        ExpectedLift {
            expected: &'a Expected,
            ty: TypeId,
        } : [1] => [1],

        // calling/control flow

        /// Represents a call to a raw WebAssembly API. The module/name are
        /// provided inline as well as the types if necessary.
        ///
        /// Note that this instruction is not currently used for async
        /// functions, instead `CallWasmAsyncImport` and `CallWasmAsyncExport`
        /// are used.
        CallWasm {
            iface: &'a Interface,
            name: &'a str,
            sig: &'a WasmSignature,
        } : [sig.params.len()] => [sig.results.len()],

        /// Represents a call to an asynchronous wasm import.
        ///
        /// This currently only happens when a compiled-to-wasm module calls as
        /// async import. This instruction is used to indicate that the
        /// specified import function should be called. The specified import
        /// function has `params` as its types, but the final two parameters
        /// must be synthesized by this instruction which are the
        /// callback/callback state. The actual imported function does not
        /// return anything but the callback will be called with the `i32` state
        /// as the first parameter and `results` as the rest of the parameters.
        /// The callback function should return nothing.
        ///
        /// It's up to the bindings generator to figure out how to make this
        /// look synchronous despite it being callback-based in the middle.
        CallWasmAsyncImport {
            iface: &'a Interface,
            name: &'a str,
            params: &'a [WasmType],
            results: &'a [WasmType],
        } : [params.len() - 2] => [results.len()],

        /// Represents a call to an asynchronous wasm export.
        ///
        /// This currently only happens when a host module calls an async
        /// function on a wasm module. The specified function will take `params`
        /// as its argument plus one more argument of an `i32` state that the
        /// host needs to synthesize. The function being called doesn't actually
        /// return anything. Instead wasm will call an `async_export_done`
        /// intrinsic in the `canonical_abi` module. This intrinsic receives a
        /// context value and a pointer into linear memory. The context value
        /// lines up with the final `i32` parameter of this function call (which
        /// the bindings generator must synthesize) and the pointer into linear
        /// memory contains the `results`, stored at 8-byte offsets in the same
        /// manner that multiple results are transferred.
        ///
        /// It's up to the bindings generator to figure out how to make this
        /// look synchronous despite it being callback-based in the middle.
        CallWasmAsyncExport {
            module: &'a str,
            name: &'a str,
            params: &'a [WasmType],
            results: &'a [WasmType],
        } : [params.len() - 1] => [results.len()],

        /// Same as `CallWasm`, except the dual where an interface is being
        /// called rather than a raw wasm function.
        ///
        /// Note that this will be used for async functions.
        CallInterface {
            module: &'a str,
            func: &'a Function,
        } : [func.params.len()] => [1],

        /// Returns `amt` values on the stack. This is always the last
        /// instruction.
        ///
        /// Note that this instruction is used for asynchronous functions where
        /// the results are *lifted*, not when they're *lowered*, though. For
        /// those modes the `ReturnAsyncExport` and `ReturnAsyncImport`
        /// functions are used.
        Return { amt: usize, func: &'a Function } : [*amt] => [0],

        /// "Returns" from an asynchronous export.
        ///
        /// This is only used for compiled-to-wasm modules at this time, and
        /// only for the exports of async functions in those modules. This
        /// instruction receives two parameters, the first of which is the
        /// original context from the start of the function which was provided
        /// when the export was first called (its last parameter). The second
        /// argument is a pointer into linear memory with the results of the
        /// asynchronous call already encoded. This instruction should then call
        /// the `async_export_done` intrinsic in the `canonical_abi` module.
        ReturnAsyncExport { func: &'a Function } : [2] => [0],

        /// "Returns" from an asynchronous import.
        ///
        /// This is only used for host modules at this time, and
        /// only for the import of async functions in those modules. This
        /// instruction receives the operands used to call the completion
        /// function in the wasm module. The first parameter to this instruction
        /// is the index into the function table of the function to call, and
        /// the remaining parameters are the parameters to invoke the function
        /// with.
        ReturnAsyncImport {
            func: &'a Function,
            params: usize,
        } : [*params + 2] => [0],

        /// Calls the `realloc` function specified in a malloc-like fashion
        /// allocating `size` bytes with alignment `align`.
        ///
        /// Pushes the returned pointer onto the stack.
        Malloc {
            realloc: &'static str,
            size: usize,
            align: usize,
        } : [0] => [1],

        /// Calls the `free` function specified to deallocate the pointer on the
        /// stack which has `size` bytes with alignment `align`.
        Free {
            free: &'static str,
            size: usize,
            align: usize,
        } : [1] => [0],
    }
}

#[derive(Debug, PartialEq)]
pub enum Bitcast {
    // Upcasts
    F32ToI32,
    F64ToI64,
    I32ToI64,
    F32ToI64,

    // Downcasts
    I32ToF32,
    I64ToF64,
    I64ToI32,
    I64ToF32,

    None,
}

/// Whether the glue code surrounding a call is lifting arguments and lowering
/// results or vice versa.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LiftLower {
    /// When the glue code lifts arguments and lowers results.
    ///
    /// ```text
    /// Wasm --lift-args--> SourceLanguage; call; SourceLanguage --lower-results--> Wasm
    /// ```
    LiftArgsLowerResults,
    /// When the glue code lowers arguments and lifts results.
    ///
    /// ```text
    /// SourceLanguage --lower-args--> Wasm; call; Wasm --lift-results--> SourceLanguage
    /// ```
    LowerArgsLiftResults,
}

/// We use a different ABI for wasm importing functions exported by the host
/// than for wasm exporting functions imported by the host.
///
/// Note that this reflects the flavor of ABI we generate, and not necessarily
/// the way the resulting bindings will be used by end users. See the comments
/// on the `Direction` enum in gen-core for details.
///
/// The bindings ABI has a concept of a "guest" and a "host". There are two
/// variants of the ABI, one specialized for the "guest" importing and calling
/// a function defined and exported in the "host", and the other specialized for
/// the "host" importing and calling a function defined and exported in the "guest".
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AbiVariant {
    /// The guest is importing and calling the function.
    GuestImport,
    /// The guest is defining and exporting the function.
    GuestExport,
}

/// Trait for language implementors to use to generate glue code between native
/// WebAssembly signatures and interface types signatures.
///
/// This is used as an implementation detail in interpreting the ABI between
/// interface types and wasm types. Eventually this will be driven by interface
/// types adapters themselves, but for now the ABI of a function dictates what
/// instructions are fed in.
///
/// Types implementing `Bindgen` are incrementally fed `Instruction` values to
/// generate code for. Instructions operate like a stack machine where each
/// instruction has a list of inputs and a list of outputs (provided by the
/// `emit` function).
pub trait Bindgen {
    /// The intermediate type for fragments of code for this type.
    ///
    /// For most languages `String` is a suitable intermediate type.
    type Operand: Clone;

    /// Emit code to implement the given instruction.
    ///
    /// Each operand is given in `operands` and can be popped off if ownership
    /// is required. It's guaranteed that `operands` has the appropriate length
    /// for the `inst` given, as specified with [`Instruction`].
    ///
    /// Each result variable should be pushed onto `results`. This function must
    /// push the appropriate number of results or binding generation will panic.
    fn emit(
        &mut self,
        iface: &Interface,
        inst: &Instruction<'_>,
        operands: &mut Vec<Self::Operand>,
        results: &mut Vec<Self::Operand>,
    );

    /// Gets a operand reference to the return pointer area.
    ///
    /// The provided size and alignment is for the function's return type.
    fn return_pointer(&mut self, iface: &Interface, size: usize, align: usize) -> Self::Operand;

    /// Enters a new block of code to generate code for.
    ///
    /// This is currently exclusively used for constructing variants. When a
    /// variant is constructed a block here will be pushed for each case of a
    /// variant, generating the code necessary to translate a variant case.
    ///
    /// Blocks are completed with `finish_block` below. It's expected that `emit`
    /// will always push code (if necessary) into the "current block", which is
    /// updated by calling this method and `finish_block` below.
    fn push_block(&mut self);

    /// Indicates to the code generator that a block is completed, and the
    /// `operand` specified was the resulting value of the block.
    ///
    /// This method will be used to compute the value of each arm of lifting a
    /// variant. The `operand` will be `None` if the variant case didn't
    /// actually have any type associated with it. Otherwise it will be `Some`
    /// as the last value remaining on the stack representing the value
    /// associated with a variant's `case`.
    ///
    /// It's expected that this will resume code generation in the previous
    /// block before `push_block` was called. This must also save the results
    /// of the current block internally for instructions like `ResultLift` to
    /// use later.
    fn finish_block(&mut self, operand: &mut Vec<Self::Operand>);

    /// Returns size information that was previously calculated for all types.
    fn sizes(&self) -> &crate::sizealign::SizeAlign;

    /// Returns whether or not the specified element type is represented in a
    /// "canonical" form for lists. This dictates whether the `ListCanonLower`
    /// and `ListCanonLift` instructions are used or not.
    fn is_list_canonical(&self, iface: &Interface, element: &Type) -> bool;
}

impl Interface {
    /// Get the WebAssembly type signature for this interface function
    ///
    /// The first entry returned is the list of parameters and the second entry
    /// is the list of results for the wasm function signature.
    pub fn wasm_signature(&self, variant: AbiVariant, func: &Function) -> WasmSignature {
        const MAX_FLAT_PARAMS: usize = 16;
        const MAX_FLAT_RESULTS: usize = 1;

        let mut params = Vec::new();
        let mut indirect_params = false;
        for (_, param) in func.params.iter() {
            self.push_wasm(variant, param, &mut params);
        }

        if params.len() > MAX_FLAT_PARAMS {
            params.truncate(0);
            params.push(WasmType::I32);
            indirect_params = true;
        }

        let mut results = Vec::new();
        self.push_wasm(variant, &func.result, &mut results);

        let mut retptr = false;
        if func.is_async {
            // Asynchronous functions never actually return anything since
            // they're all callback-based, meaning that we always put all the
            // results into a return pointer.
            //
            // Asynchronous exports take one extra parameter which is the
            // context used to pass to the `async_export_done` intrinsic, and
            // asynchronous imports take two extra parameters where the first is
            // a pointer into the function table and the second is a context
            // argument to pass to this function.
            match variant {
                AbiVariant::GuestExport => {
                    retptr = true;
                    results.truncate(0);
                    params.push(WasmType::I32);
                }
                AbiVariant::GuestImport => {
                    retptr = true;
                    results.truncate(0);
                    params.push(WasmType::I32);
                    params.push(WasmType::I32);
                }
            }
        } else {
            // Rust/C don't support multi-value well right now, so if a function
            // would have multiple results then instead truncate it. Imports take a
            // return pointer to write into and exports return a pointer they wrote
            // into.
            if results.len() > MAX_FLAT_RESULTS {
                retptr = true;
                results.truncate(0);
                match variant {
                    AbiVariant::GuestImport => {
                        params.push(WasmType::I32);
                    }
                    AbiVariant::GuestExport => {
                        results.push(WasmType::I32);
                    }
                }
            }
        }

        WasmSignature {
            params,
            indirect_params,
            results,
            retptr,
        }
    }

    fn push_wasm(&self, variant: AbiVariant, ty: &Type, result: &mut Vec<WasmType>) {
        match ty {
            Type::Unit => {}

            Type::Bool
            | Type::S8
            | Type::U8
            | Type::S16
            | Type::U16
            | Type::S32
            | Type::U32
            | Type::Char
            | Type::Handle(_) => result.push(WasmType::I32),

            Type::U64 | Type::S64 => result.push(WasmType::I64),
            Type::Float32 => result.push(WasmType::F32),
            Type::Float64 => result.push(WasmType::F64),
            Type::String => {
                result.push(WasmType::I32);
                result.push(WasmType::I32);
            }

            Type::Id(id) => match &self.types[*id].kind {
                TypeDefKind::Type(t) => self.push_wasm(variant, t, result),

                TypeDefKind::Record(r) => {
                    for field in r.fields.iter() {
                        self.push_wasm(variant, &field.ty, result);
                    }
                }

                TypeDefKind::Tuple(t) => {
                    for ty in t.types.iter() {
                        self.push_wasm(variant, ty, result);
                    }
                }

                TypeDefKind::Flags(r) => {
                    for _ in 0..r.repr().count() {
                        result.push(WasmType::I32);
                    }
                }

                TypeDefKind::List(_) => {
                    result.push(WasmType::I32);
                    result.push(WasmType::I32);
                }

                TypeDefKind::Variant(v) => {
                    result.push(v.tag().into());
                    self.push_wasm_variants(variant, v.cases.iter().map(|c| &c.ty), result);
                }

                TypeDefKind::Enum(e) => result.push(e.tag().into()),

                TypeDefKind::Option(t) => {
                    result.push(WasmType::I32);
                    self.push_wasm_variants(variant, [&Type::Unit, t], result);
                }

                TypeDefKind::Expected(e) => {
                    result.push(WasmType::I32);
                    self.push_wasm_variants(variant, [&e.ok, &e.err], result);
                }

                TypeDefKind::Union(u) => {
                    result.push(WasmType::I32);
                    self.push_wasm_variants(variant, u.cases.iter().map(|c| &c.ty), result);
                }

                TypeDefKind::Future(_) => {
                    result.push(WasmType::I32);
                }

                TypeDefKind::Stream(_) => {
                    result.push(WasmType::I32);
                }
            },
        }
    }

    fn push_wasm_variants<'a>(
        &self,
        variant: AbiVariant,
        tys: impl IntoIterator<Item = &'a Type>,
        result: &mut Vec<WasmType>,
    ) {
        let mut temp = Vec::new();
        let start = result.len();

        // Push each case's type onto a temporary vector, and then
        // merge that vector into our final list starting at
        // `start`. Note that this requires some degree of
        // "unification" so we can handle things like `Result<i32,
        // f32>` where that turns into `[i32 i32]` where the second
        // `i32` might be the `f32` bitcasted.
        for ty in tys {
            self.push_wasm(variant, ty, &mut temp);

            for (i, ty) in temp.drain(..).enumerate() {
                match result.get_mut(start + i) {
                    Some(prev) => *prev = join(*prev, ty),
                    None => result.push(ty),
                }
            }
        }
    }

    /// Generates an abstract sequence of instructions which represents this
    /// function being adapted as an imported function.
    ///
    /// The instructions here, when executed, will emulate a language with
    /// interface types calling the concrete wasm implementation. The parameters
    /// for the returned instruction sequence are the language's own
    /// interface-types parameters. One instruction in the instruction stream
    /// will be a `Call` which represents calling the actual raw wasm function
    /// signature.
    ///
    /// This function is useful, for example, if you're building a language
    /// generator for WASI bindings. This will document how to translate
    /// language-specific values into the wasm types to call a WASI function,
    /// and it will also automatically convert the results of the WASI function
    /// back to a language-specific value.
    pub fn call(
        &self,
        variant: AbiVariant,
        lift_lower: LiftLower,
        func: &Function,
        bindgen: &mut impl Bindgen,
    ) {
        Generator::new(self, variant, lift_lower, bindgen).call(func);
    }
}

struct Generator<'a, B: Bindgen> {
    variant: AbiVariant,
    lift_lower: LiftLower,
    bindgen: &'a mut B,
    iface: &'a Interface,
    operands: Vec<B::Operand>,
    results: Vec<B::Operand>,
    stack: Vec<B::Operand>,
    return_pointer: Option<B::Operand>,
}

impl<'a, B: Bindgen> Generator<'a, B> {
    fn new(
        iface: &'a Interface,
        variant: AbiVariant,
        lift_lower: LiftLower,
        bindgen: &'a mut B,
    ) -> Generator<'a, B> {
        Generator {
            iface,
            variant,
            lift_lower,
            bindgen,
            operands: Vec::new(),
            results: Vec::new(),
            stack: Vec::new(),
            return_pointer: None,
        }
    }

    fn call(&mut self, func: &Function) {
        let sig = self.iface.wasm_signature(self.variant, func);

        match self.lift_lower {
            LiftLower::LowerArgsLiftResults => {
                if !sig.indirect_params {
                    // If the parameters for this function aren't indirect
                    // (there aren't too many) then we simply do a normal lower
                    // operation for them all.
                    for (nth, (_, ty)) in func.params.iter().enumerate() {
                        self.emit(&Instruction::GetArg { nth });
                        self.lower(ty);
                    }
                } else {
                    // ... otherwise if parameters are indirect space is
                    // allocated from them and each argument is lowered
                    // individually into memory.
                    let (size, align) = self
                        .bindgen
                        .sizes()
                        .record(func.params.iter().map(|t| &t.1));
                    let ptr = match self.variant {
                        // When a wasm module calls an import it will provide
                        // static space that isn't dynamically allocated.
                        AbiVariant::GuestImport => {
                            self.bindgen.return_pointer(self.iface, size, align)
                        }
                        // When calling a wasm module from the outside, though,
                        // malloc needs to be called.
                        AbiVariant::GuestExport => {
                            self.emit(&Instruction::Malloc {
                                realloc: "canonical_abi_realloc",
                                size,
                                align,
                            });
                            self.stack.pop().unwrap()
                        }
                    };
                    let mut offset = 0usize;
                    for (nth, (_, ty)) in func.params.iter().enumerate() {
                        self.emit(&Instruction::GetArg { nth });
                        offset = align_to(offset, self.bindgen.sizes().align(ty));
                        self.write_to_memory(ty, ptr.clone(), offset as i32);
                        offset += self.bindgen.sizes().size(ty);
                    }

                    self.stack.push(ptr);
                }

                if func.is_async {
                    // We emit custom instructions for async calls since they
                    // have different parameters synthesized by the bindings
                    // generator depending on what kind of call is being made.
                    //
                    // Note that no return pointer goop happens here because
                    // that's all done through parameters of callbacks instead.
                    let mut results = Vec::new();
                    self.iface
                        .push_wasm(self.variant, &func.result, &mut results);
                    match self.variant {
                        AbiVariant::GuestImport => {
                            assert_eq!(self.stack.len(), sig.params.len() - 2);
                            self.emit(&Instruction::CallWasmAsyncImport {
                                iface: self.iface,
                                name: &func.name,
                                params: &sig.params,
                                results: &results,
                            });
                        }
                        AbiVariant::GuestExport => {
                            assert_eq!(self.stack.len(), sig.params.len() - 1);
                            self.emit(&Instruction::CallWasmAsyncExport {
                                module: &self.iface.name,
                                name: &func.name,
                                params: &sig.params,
                                results: &results,
                            });
                        }
                    }

                    self.lift(&func.result);
                } else {
                    // If necessary we may need to prepare a return pointer for
                    // this ABI.
                    if self.variant == AbiVariant::GuestImport && sig.retptr {
                        let size = self.bindgen.sizes().size(&func.result);
                        let align = self.bindgen.sizes().align(&func.result);
                        let ptr = self.bindgen.return_pointer(self.iface, size, align);
                        self.return_pointer = Some(ptr.clone());
                        self.stack.push(ptr);
                    }

                    // Now that all the wasm args are prepared we can call the
                    // actual wasm function.
                    assert_eq!(self.stack.len(), sig.params.len());
                    self.emit(&Instruction::CallWasm {
                        iface: self.iface,
                        name: &func.name,
                        sig: &sig,
                    });

                    if !sig.retptr {
                        // With no return pointer in use we can simply lift the
                        // result of the function from the result of the core
                        // wasm function.
                        self.lift(&func.result);
                    } else {
                        let ptr = match self.variant {
                            // imports into guests means it's a wasm module
                            // calling an imported function. We supplied the
                            // return poitner as the last argument (saved in
                            // `self.return_pointer`) so we use that to read
                            // the result of the function from memory.
                            AbiVariant::GuestImport => {
                                assert!(sig.results.len() == 0);
                                self.return_pointer.take().unwrap()
                            }

                            // guest exports means that this is a host
                            // calling wasm so wasm returned a pointer to where
                            // the result is stored
                            AbiVariant::GuestExport => self.stack.pop().unwrap(),
                        };

                        self.read_from_memory(&func.result, ptr, 0);
                    }
                }

                self.emit(&Instruction::Return { func, amt: 1 });
            }
            LiftLower::LiftArgsLowerResults => {
                if !sig.indirect_params {
                    // If parameters are not passed indirectly then we lift each
                    // argument in succession from the component wasm types that
                    // make-up the type.
                    let mut offset = 0;
                    let mut temp = Vec::new();
                    for (_, ty) in func.params.iter() {
                        temp.truncate(0);
                        self.iface.push_wasm(self.variant, ty, &mut temp);
                        for _ in 0..temp.len() {
                            self.emit(&Instruction::GetArg { nth: offset });
                            offset += 1;
                        }
                        self.lift(ty);
                    }
                } else {
                    // ... otherwise argument is read in succession from memory
                    // where the pointer to the arguments is the first argument
                    // to the function.
                    let mut offset = 0usize;
                    self.emit(&Instruction::GetArg { nth: 0 });
                    let ptr = self.stack.pop().unwrap();
                    for (_, ty) in func.params.iter() {
                        offset = align_to(offset, self.bindgen.sizes().align(ty));
                        self.read_from_memory(ty, ptr.clone(), offset as i32);
                        offset += self.bindgen.sizes().size(ty);
                    }
                }

                // ... and that allows us to call the interface types function
                self.emit(&Instruction::CallInterface {
                    module: &self.iface.name,
                    func,
                });

                // This was dynamically allocated by the caller so after
                // it's been read by the guest we need to deallocate it.
                if let AbiVariant::GuestExport = self.variant {
                    if sig.indirect_params {
                        let (size, align) = self
                            .bindgen
                            .sizes()
                            .record(func.params.iter().map(|t| &t.1));
                        self.emit(&Instruction::GetArg { nth: 0 });
                        self.emit(&Instruction::Free {
                            free: "canonical_abi_free",
                            size,
                            align,
                        });
                    }
                }

                if func.is_async {
                    match self.variant {
                        // Returning from a guest import means that the
                        // completion callback needs to be called which is
                        // currently given the lowered representation of the
                        // result.
                        AbiVariant::GuestImport => {
                            self.lower(&func.result);

                            let mut tys = Vec::new();
                            self.iface.push_wasm(self.variant, &func.result, &mut tys);
                            assert_eq!(self.stack.len(), tys.len());
                            let operands = mem::take(&mut self.stack);
                            // function index to call
                            self.emit(&Instruction::GetArg {
                                nth: sig.params.len() - 2,
                            });
                            // environment for the function
                            self.emit(&Instruction::GetArg {
                                nth: sig.params.len() - 1,
                            });
                            self.stack.extend(operands);
                            self.emit(&Instruction::ReturnAsyncImport {
                                func,
                                params: tys.len(),
                            });
                        }

                        // Returning from a guest export means that we need to
                        // invoke the completion intrinsics with where the
                        // result is stored in linear memory.
                        AbiVariant::GuestExport => {
                            let size = self.bindgen.sizes().size(&func.result);
                            let align = self.bindgen.sizes().align(&func.result);
                            let ptr = self.bindgen.return_pointer(self.iface, size, align);
                            self.write_to_memory(&func.result, ptr.clone(), 0);

                            // Get the caller's context index.
                            self.emit(&Instruction::GetArg {
                                nth: sig.params.len() - 1,
                            });
                            self.stack.push(ptr);

                            // This will call the "done" function with the
                            // context/pointer argument
                            self.emit(&Instruction::ReturnAsyncExport { func });
                        }
                    }
                } else {
                    if !sig.retptr {
                        // With no return pointer in use we simply lower the
                        // result and return that directly from the function.
                        self.lower(&func.result);
                    } else {
                        match self.variant {
                            // When a function is imported to a guest this means
                            // it's a host providing the implementation of the
                            // import. The result is stored in the pointer
                            // specified in the last argument, so we get the
                            // pointer here and then write the return value into
                            // it.
                            AbiVariant::GuestImport => {
                                self.emit(&Instruction::GetArg {
                                    nth: sig.params.len() - 1,
                                });
                                let ptr = self.stack.pop().unwrap();
                                self.write_to_memory(&func.result, ptr, 0);
                            }

                            // For a guest import this is a function defined in
                            // wasm, so we're returning a pointer where the
                            // value was stored at. Allocate some space here
                            // (statically) and then write the result into that
                            // memory, returning the pointer at the end.
                            AbiVariant::GuestExport => {
                                let size = self.bindgen.sizes().size(&func.result);
                                let align = self.bindgen.sizes().align(&func.result);
                                let ptr = self.bindgen.return_pointer(self.iface, size, align);
                                self.write_to_memory(&func.result, ptr.clone(), 0);
                                self.stack.push(ptr);
                            }
                        }
                    }

                    self.emit(&Instruction::Return {
                        func,
                        amt: sig.results.len(),
                    });
                }
            }
        }

        assert!(
            self.stack.is_empty(),
            "stack has {} items remaining",
            self.stack.len()
        );
    }

    fn emit(&mut self, inst: &Instruction<'_>) {
        self.operands.clear();
        self.results.clear();

        let operands_len = inst.operands_len();
        assert!(
            self.stack.len() >= operands_len,
            "not enough operands on stack for {:?}",
            inst
        );
        self.operands
            .extend(self.stack.drain((self.stack.len() - operands_len)..));
        self.results.reserve(inst.results_len());

        self.bindgen
            .emit(self.iface, inst, &mut self.operands, &mut self.results);

        assert_eq!(
            self.results.len(),
            inst.results_len(),
            "{:?} expected {} results, got {}",
            inst,
            inst.results_len(),
            self.results.len()
        );
        self.stack.append(&mut self.results);
    }

    fn push_block(&mut self) {
        self.bindgen.push_block();
    }

    fn finish_block(&mut self, size: usize) {
        self.operands.clear();
        assert!(
            size <= self.stack.len(),
            "not enough operands on stack for finishing block",
        );
        self.operands
            .extend(self.stack.drain((self.stack.len() - size)..));
        self.bindgen.finish_block(&mut self.operands);
    }

    fn lower(&mut self, ty: &Type) {
        use Instruction::*;

        match *ty {
            Type::Unit => self.emit(&UnitLower),
            Type::Bool => self.emit(&I32FromBool),
            Type::S8 => self.emit(&I32FromS8),
            Type::U8 => self.emit(&I32FromU8),
            Type::S16 => self.emit(&I32FromS16),
            Type::U16 => self.emit(&I32FromU16),
            Type::S32 => self.emit(&I32FromS32),
            Type::U32 => self.emit(&I32FromU32),
            Type::S64 => self.emit(&I64FromS64),
            Type::U64 => self.emit(&I64FromU64),
            Type::Char => self.emit(&I32FromChar),
            Type::Float32 => self.emit(&F32FromFloat32),
            Type::Float64 => self.emit(&F64FromFloat64),
            Type::Handle(ty) => {
                let borrowed = match self.lift_lower {
                    // This means that a return value is being lowered, which is
                    // never borrowed.
                    LiftLower::LiftArgsLowerResults => false,
                    // There's one of three possible situations we're in:
                    //
                    // * The handle is defined by the wasm module itself. This
                    //   is the only actual possible scenario today due to how
                    //   witx is defined. In this situation the handle is owned
                    //   by the host and "proof of ownership" is being offered
                    //   and there's no need to relinquish ownership.
                    //
                    // * The handle is defined by the host, and it's passing it
                    //   to a wasm module. This should use an owned conversion.
                    //   This isn't expressible in today's `*.witx` format.
                    //
                    // * The handle is defined by neither the host or the wasm
                    //   mdoule. This means that the host is passing a
                    //   capability from another wasm module into this one,
                    //   meaning it's doing so by reference since the host is
                    //   retaining access to its own
                    //
                    // Note, again, only the first bullet here is possible
                    // today, hence the hardcoded `true` value. We'll need to
                    // refactor `witx` to expose the other possibilities.
                    LiftLower::LowerArgsLiftResults => true,
                };
                if borrowed {
                    self.emit(&I32FromBorrowedHandle { ty });
                } else {
                    self.emit(&I32FromOwnedHandle { ty });
                }
            }
            Type::String => {
                let realloc = self.list_realloc();
                self.emit(&StringLower { realloc });
            }
            Type::Id(id) => match &self.iface.types[id].kind {
                TypeDefKind::Type(t) => self.lower(t),
                TypeDefKind::List(element) => {
                    let realloc = self.list_realloc();
                    if self.bindgen.is_list_canonical(self.iface, element) {
                        self.emit(&ListCanonLower { element, realloc });
                    } else {
                        self.push_block();
                        self.emit(&IterElem { element });
                        self.emit(&IterBasePointer);
                        let addr = self.stack.pop().unwrap();
                        self.write_to_memory(element, addr, 0);
                        self.finish_block(0);
                        self.emit(&ListLower { element, realloc });
                    }
                }
                TypeDefKind::Record(record) => {
                    self.emit(&RecordLower {
                        record,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                    let values = self
                        .stack
                        .drain(self.stack.len() - record.fields.len()..)
                        .collect::<Vec<_>>();
                    for (field, value) in record.fields.iter().zip(values) {
                        self.stack.push(value);
                        self.lower(&field.ty);
                    }
                }
                TypeDefKind::Tuple(tuple) => {
                    self.emit(&TupleLower { tuple, ty: id });
                    let values = self
                        .stack
                        .drain(self.stack.len() - tuple.types.len()..)
                        .collect::<Vec<_>>();
                    for (ty, value) in tuple.types.iter().zip(values) {
                        self.stack.push(value);
                        self.lower(ty);
                    }
                }

                TypeDefKind::Flags(flags) => {
                    self.emit(&FlagsLower {
                        flags,
                        ty: id,
                        name: self.iface.types[id].name.as_ref().unwrap(),
                    });
                }

                TypeDefKind::Variant(v) => {
                    let results = self.lower_variant_arms(ty, v.cases.iter().map(|c| &c.ty));
                    self.emit(&VariantLower {
                        variant: v,
                        ty: id,
                        results: &results,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }
                TypeDefKind::Enum(enum_) => {
                    self.emit(&EnumLower {
                        enum_,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }
                TypeDefKind::Option(t) => {
                    let results = self.lower_variant_arms(ty, [&Type::Unit, t]);
                    self.emit(&OptionLower {
                        payload: t,
                        ty: id,
                        results: &results,
                    });
                }
                TypeDefKind::Expected(e) => {
                    let results = self.lower_variant_arms(ty, [&e.ok, &e.err]);
                    self.emit(&ExpectedLower {
                        expected: e,
                        ty: id,
                        results: &results,
                    });
                }
                TypeDefKind::Union(union) => {
                    let results = self.lower_variant_arms(ty, union.cases.iter().map(|c| &c.ty));
                    self.emit(&UnionLower {
                        union,
                        ty: id,
                        results: &results,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }
                TypeDefKind::Future(_) => todo!("lower future"),
                TypeDefKind::Stream(_) => todo!("lower stream"),
            },
        }
    }

    fn lower_variant_arms<'b>(
        &mut self,
        ty: &Type,
        cases: impl IntoIterator<Item = &'b Type>,
    ) -> Vec<WasmType> {
        use Instruction::*;
        let mut results = Vec::new();
        let mut temp = Vec::new();
        let mut casts = Vec::new();
        self.iface.push_wasm(self.variant, ty, &mut results);
        for (i, ty) in cases.into_iter().enumerate() {
            self.push_block();
            self.emit(&VariantPayloadName);
            let payload_name = self.stack.pop().unwrap();
            self.emit(&I32Const { val: i as i32 });
            let mut pushed = 1;
            // Using the payload of this block we lower the type to
            // raw wasm values.
            self.stack.push(payload_name.clone());
            self.lower(ty);

            // Determine the types of all the wasm values we just
            // pushed, and record how many. If we pushed too few
            // then we'll need to push some zeros after this.
            temp.truncate(0);
            self.iface.push_wasm(self.variant, ty, &mut temp);
            pushed += temp.len();

            // For all the types pushed we may need to insert some
            // bitcasts. This will go through and cast everything
            // to the right type to ensure all blocks produce the
            // same set of results.
            casts.truncate(0);
            for (actual, expected) in temp.iter().zip(&results[1..]) {
                casts.push(cast(*actual, *expected));
            }
            if casts.iter().any(|c| *c != Bitcast::None) {
                self.emit(&Bitcasts { casts: &casts });
            }

            // If we haven't pushed enough items in this block to match
            // what other variants are pushing then we need to push
            // some zeros.
            if pushed < results.len() {
                self.emit(&ConstZero {
                    tys: &results[pushed..],
                });
            }
            self.finish_block(results.len());
        }
        results
    }

    fn list_realloc(&self) -> Option<&'static str> {
        // Lowering parameters calling a wasm import means
        // we don't need to pass ownership, but we pass
        // ownership in all other cases.
        match (self.variant, self.lift_lower) {
            (AbiVariant::GuestImport, LiftLower::LowerArgsLiftResults) => None,
            _ => Some("canonical_abi_realloc"),
        }
    }

    /// Note that in general everything in this function is the opposite of the
    /// `lower` function above. This is intentional and should be kept this way!
    fn lift(&mut self, ty: &Type) {
        use Instruction::*;

        match *ty {
            Type::Unit => self.emit(&UnitLift),
            Type::Bool => self.emit(&BoolFromI32),
            Type::S8 => self.emit(&S8FromI32),
            Type::U8 => self.emit(&U8FromI32),
            Type::S16 => self.emit(&S16FromI32),
            Type::U16 => self.emit(&U16FromI32),
            Type::S32 => self.emit(&S32FromI32),
            Type::U32 => self.emit(&U32FromI32),
            Type::S64 => self.emit(&S64FromI64),
            Type::U64 => self.emit(&U64FromI64),
            Type::Char => self.emit(&CharFromI32),
            Type::Float32 => self.emit(&Float32FromF32),
            Type::Float64 => self.emit(&Float64FromF64),
            Type::Handle(ty) => {
                // For more information on these values see the comments in
                // `lower` above.
                let borrowed = match self.lift_lower {
                    LiftLower::LiftArgsLowerResults => true,
                    LiftLower::LowerArgsLiftResults => false,
                };
                if borrowed {
                    self.emit(&HandleBorrowedFromI32 { ty });
                } else {
                    self.emit(&HandleOwnedFromI32 { ty });
                }
            }
            Type::String => {
                let free = self.list_free();
                self.emit(&StringLift { free });
            }
            Type::Id(id) => match &self.iface.types[id].kind {
                TypeDefKind::Type(t) => self.lift(t),
                TypeDefKind::List(element) => {
                    let free = self.list_free();
                    if self.is_char(element) || self.bindgen.is_list_canonical(self.iface, element)
                    {
                        self.emit(&ListCanonLift {
                            element,
                            free,
                            ty: id,
                        });
                    } else {
                        self.push_block();
                        self.emit(&IterBasePointer);
                        let addr = self.stack.pop().unwrap();
                        self.read_from_memory(element, addr, 0);
                        self.finish_block(1);
                        self.emit(&ListLift {
                            element,
                            free,
                            ty: id,
                        });
                    }
                }
                TypeDefKind::Record(record) => {
                    let mut temp = Vec::new();
                    self.iface.push_wasm(self.variant, ty, &mut temp);
                    let mut args = self
                        .stack
                        .drain(self.stack.len() - temp.len()..)
                        .collect::<Vec<_>>();
                    for field in record.fields.iter() {
                        temp.truncate(0);
                        self.iface.push_wasm(self.variant, &field.ty, &mut temp);
                        self.stack.extend(args.drain(..temp.len()));
                        self.lift(&field.ty);
                    }
                    self.emit(&RecordLift {
                        record,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }
                TypeDefKind::Tuple(tuple) => {
                    let mut temp = Vec::new();
                    self.iface.push_wasm(self.variant, ty, &mut temp);
                    let mut args = self
                        .stack
                        .drain(self.stack.len() - temp.len()..)
                        .collect::<Vec<_>>();
                    for ty in tuple.types.iter() {
                        temp.truncate(0);
                        self.iface.push_wasm(self.variant, ty, &mut temp);
                        self.stack.extend(args.drain(..temp.len()));
                        self.lift(ty);
                    }
                    self.emit(&TupleLift { tuple, ty: id });
                }
                TypeDefKind::Flags(flags) => {
                    self.emit(&FlagsLift {
                        flags,
                        ty: id,
                        name: self.iface.types[id].name.as_ref().unwrap(),
                    });
                }

                TypeDefKind::Variant(v) => {
                    self.lift_variant_arms(ty, v.cases.iter().map(|c| &c.ty));
                    self.emit(&VariantLift {
                        variant: v,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }

                TypeDefKind::Enum(enum_) => {
                    self.emit(&EnumLift {
                        enum_,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }

                TypeDefKind::Option(t) => {
                    self.lift_variant_arms(ty, [&Type::Unit, t]);
                    self.emit(&OptionLift { payload: t, ty: id });
                }

                TypeDefKind::Expected(e) => {
                    self.lift_variant_arms(ty, [&e.ok, &e.err]);
                    self.emit(&ExpectedLift {
                        expected: e,
                        ty: id,
                    });
                }

                TypeDefKind::Union(union) => {
                    self.lift_variant_arms(ty, union.cases.iter().map(|c| &c.ty));
                    self.emit(&UnionLift {
                        union,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }

                TypeDefKind::Future(_) => todo!("lift future"),
                TypeDefKind::Stream(_) => todo!("lift stream"),
            },
        }
    }

    fn lift_variant_arms<'b>(&mut self, ty: &Type, cases: impl IntoIterator<Item = &'b Type>) {
        let mut params = Vec::new();
        let mut temp = Vec::new();
        let mut casts = Vec::new();
        self.iface.push_wasm(self.variant, ty, &mut params);
        let block_inputs = self
            .stack
            .drain(self.stack.len() + 1 - params.len()..)
            .collect::<Vec<_>>();
        for ty in cases {
            self.push_block();
            // Push only the values we need for this variant onto
            // the stack.
            temp.truncate(0);
            self.iface.push_wasm(self.variant, ty, &mut temp);
            self.stack
                .extend(block_inputs[..temp.len()].iter().cloned());

            // Cast all the types we have on the stack to the actual
            // types needed for this variant, if necessary.
            casts.truncate(0);
            for (actual, expected) in temp.iter().zip(&params[1..]) {
                casts.push(cast(*expected, *actual));
            }
            if casts.iter().any(|c| *c != Bitcast::None) {
                self.emit(&Instruction::Bitcasts { casts: &casts });
            }

            // Then recursively lift this variant's payload.
            self.lift(ty);
            self.finish_block(1);
        }
    }

    fn list_free(&self) -> Option<&'static str> {
        // Lifting the arguments of a defined import means that, if
        // possible, the caller still retains ownership and we don't
        // free anything.
        match (self.variant, self.lift_lower) {
            (AbiVariant::GuestImport, LiftLower::LiftArgsLowerResults) => None,
            _ => Some("canonical_abi_free"),
        }
    }

    fn write_to_memory(&mut self, ty: &Type, addr: B::Operand, offset: i32) {
        use Instruction::*;

        match *ty {
            Type::Unit => self.lower(ty),
            // Builtin types need different flavors of storage instructions
            // depending on the size of the value written.
            Type::Bool | Type::U8 | Type::S8 => {
                self.lower_and_emit(ty, addr, &I32Store8 { offset })
            }
            Type::U16 | Type::S16 => self.lower_and_emit(ty, addr, &I32Store16 { offset }),
            Type::U32 | Type::S32 | Type::Handle(_) | Type::Char => {
                self.lower_and_emit(ty, addr, &I32Store { offset })
            }
            Type::U64 | Type::S64 => self.lower_and_emit(ty, addr, &I64Store { offset }),
            Type::Float32 => self.lower_and_emit(ty, addr, &F32Store { offset }),
            Type::Float64 => self.lower_and_emit(ty, addr, &F64Store { offset }),
            Type::String => self.write_list_to_memory(ty, addr, offset),

            Type::Id(id) => match &self.iface.types[id].kind {
                TypeDefKind::Type(t) => self.write_to_memory(t, addr, offset),
                TypeDefKind::List(_) => self.write_list_to_memory(ty, addr, offset),

                // Decompose the record into its components and then write all
                // the components into memory one-by-one.
                TypeDefKind::Record(record) => {
                    self.emit(&RecordLower {
                        record,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                    self.write_fields_to_memory(
                        &record.fields.iter().map(|f| f.ty).collect::<Vec<_>>(),
                        addr,
                        offset,
                    );
                }
                TypeDefKind::Tuple(tuple) => {
                    self.emit(&TupleLower { tuple, ty: id });
                    self.write_fields_to_memory(&tuple.types, addr, offset);
                }

                TypeDefKind::Flags(f) => {
                    self.lower(ty);
                    match f.repr() {
                        FlagsRepr::U8 => {
                            self.stack.push(addr);
                            self.store_intrepr(offset, Int::U8);
                        }
                        FlagsRepr::U16 => {
                            self.stack.push(addr);
                            self.store_intrepr(offset, Int::U16);
                        }
                        FlagsRepr::U32(n) => {
                            for i in (0..n).rev() {
                                self.stack.push(addr.clone());
                                self.emit(&I32Store {
                                    offset: offset + (i as i32) * 4,
                                });
                            }
                        }
                    }
                }

                // Each case will get its own block, and the first item in each
                // case is writing the discriminant. After that if we have a
                // payload we write the payload after the discriminant, aligned up
                // to the type's alignment.
                TypeDefKind::Variant(v) => {
                    self.write_variant_arms_to_memory(
                        offset,
                        addr,
                        v.tag(),
                        v.cases.iter().map(|c| &c.ty),
                    );
                    self.emit(&VariantLower {
                        variant: v,
                        ty: id,
                        results: &[],
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }

                TypeDefKind::Option(t) => {
                    self.write_variant_arms_to_memory(offset, addr, Int::U8, [&Type::Unit, t]);
                    self.emit(&OptionLower {
                        payload: t,
                        ty: id,
                        results: &[],
                    });
                }

                TypeDefKind::Expected(e) => {
                    self.write_variant_arms_to_memory(offset, addr, Int::U8, [&e.ok, &e.err]);
                    self.emit(&ExpectedLower {
                        expected: e,
                        ty: id,
                        results: &[],
                    });
                }

                TypeDefKind::Enum(e) => {
                    self.lower(ty);
                    self.stack.push(addr);
                    self.store_intrepr(offset, e.tag());
                }

                TypeDefKind::Union(union) => {
                    self.write_variant_arms_to_memory(
                        offset,
                        addr,
                        union.tag(),
                        union.cases.iter().map(|c| &c.ty),
                    );
                    self.emit(&UnionLower {
                        union,
                        ty: id,
                        results: &[],
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }

                TypeDefKind::Future(_) => todo!("write future to memory"),
                TypeDefKind::Stream(_) => todo!("write stream to memory"),
            },
        }
    }

    fn write_variant_arms_to_memory<'b>(
        &mut self,
        offset: i32,
        addr: B::Operand,
        tag: Int,
        cases: impl IntoIterator<Item = &'b Type> + Clone,
    ) {
        let payload_offset =
            offset + (self.bindgen.sizes().payload_offset(tag, cases.clone()) as i32);
        for (i, ty) in cases.into_iter().enumerate() {
            self.push_block();
            self.emit(&Instruction::VariantPayloadName);
            let payload_name = self.stack.pop().unwrap();
            self.emit(&Instruction::I32Const { val: i as i32 });
            self.stack.push(addr.clone());
            self.store_intrepr(offset, tag);
            self.stack.push(payload_name.clone());
            self.write_to_memory(ty, addr.clone(), payload_offset);
            self.finish_block(0);
        }
    }

    fn write_list_to_memory(&mut self, ty: &Type, addr: B::Operand, offset: i32) {
        // After lowering the list there's two i32 values on the stack
        // which we write into memory, writing the pointer into the low address
        // and the length into the high address.
        self.lower(ty);
        self.stack.push(addr.clone());
        self.emit(&Instruction::I32Store { offset: offset + 4 });
        self.stack.push(addr);
        self.emit(&Instruction::I32Store { offset });
    }

    fn write_fields_to_memory(&mut self, tys: &[Type], addr: B::Operand, offset: i32) {
        let fields = self
            .stack
            .drain(self.stack.len() - tys.len()..)
            .collect::<Vec<_>>();
        for ((field_offset, op), ty) in self
            .bindgen
            .sizes()
            .field_offsets(tys.iter())
            .into_iter()
            .zip(fields)
            .zip(tys)
        {
            self.stack.push(op);
            self.write_to_memory(ty, addr.clone(), offset + (field_offset as i32));
        }
    }

    fn lower_and_emit(&mut self, ty: &Type, addr: B::Operand, instr: &Instruction) {
        self.lower(ty);
        self.stack.push(addr);
        self.emit(instr);
    }

    fn read_from_memory(&mut self, ty: &Type, addr: B::Operand, offset: i32) {
        use Instruction::*;

        match *ty {
            Type::Unit => self.emit(&UnitLift),
            Type::Bool => self.emit_and_lift(ty, addr, &I32Load8U { offset }),
            Type::U8 => self.emit_and_lift(ty, addr, &I32Load8U { offset }),
            Type::S8 => self.emit_and_lift(ty, addr, &I32Load8S { offset }),
            Type::U16 => self.emit_and_lift(ty, addr, &I32Load16U { offset }),
            Type::S16 => self.emit_and_lift(ty, addr, &I32Load16S { offset }),
            Type::U32 | Type::S32 | Type::Char | Type::Handle(_) => {
                self.emit_and_lift(ty, addr, &I32Load { offset })
            }
            Type::U64 | Type::S64 => self.emit_and_lift(ty, addr, &I64Load { offset }),
            Type::Float32 => self.emit_and_lift(ty, addr, &F32Load { offset }),
            Type::Float64 => self.emit_and_lift(ty, addr, &F64Load { offset }),
            Type::String => self.read_list_from_memory(ty, addr, offset),

            Type::Id(id) => match &self.iface.types[id].kind {
                TypeDefKind::Type(t) => self.read_from_memory(t, addr, offset),

                TypeDefKind::List(_) => self.read_list_from_memory(ty, addr, offset),

                // Read and lift each field individually, adjusting the offset
                // as we go along, then aggregate all the fields into the
                // record.
                TypeDefKind::Record(record) => {
                    self.read_fields_from_memory(
                        &record.fields.iter().map(|f| f.ty).collect::<Vec<_>>(),
                        addr,
                        offset,
                    );
                    self.emit(&RecordLift {
                        record,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }
                TypeDefKind::Tuple(tuple) => {
                    self.read_fields_from_memory(&tuple.types, addr, offset);
                    self.emit(&TupleLift { tuple, ty: id });
                }

                TypeDefKind::Flags(f) => {
                    match f.repr() {
                        FlagsRepr::U8 => {
                            self.stack.push(addr);
                            self.load_intrepr(offset, Int::U8);
                        }
                        FlagsRepr::U16 => {
                            self.stack.push(addr);
                            self.load_intrepr(offset, Int::U16);
                        }
                        FlagsRepr::U32(n) => {
                            for i in 0..n {
                                self.stack.push(addr.clone());
                                self.emit(&I32Load {
                                    offset: offset + (i as i32) * 4,
                                });
                            }
                        }
                    }
                    self.lift(ty);
                }

                // Each case will get its own block, and we'll dispatch to the
                // right block based on the `i32.load` we initially perform. Each
                // individual block is pretty simple and just reads the payload type
                // from the corresponding offset if one is available.
                TypeDefKind::Variant(variant) => {
                    self.read_variant_arms_from_memory(
                        offset,
                        addr,
                        variant.tag(),
                        variant.cases.iter().map(|c| &c.ty),
                    );
                    self.emit(&VariantLift {
                        variant,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }

                TypeDefKind::Option(t) => {
                    self.read_variant_arms_from_memory(offset, addr, Int::U8, [&Type::Unit, t]);
                    self.emit(&OptionLift { payload: t, ty: id });
                }

                TypeDefKind::Expected(e) => {
                    self.read_variant_arms_from_memory(offset, addr, Int::U8, [&e.ok, &e.err]);
                    self.emit(&ExpectedLift {
                        expected: e,
                        ty: id,
                    });
                }

                TypeDefKind::Enum(e) => {
                    self.stack.push(addr.clone());
                    self.load_intrepr(offset, e.tag());
                    self.lift(ty);
                }

                TypeDefKind::Union(union) => {
                    self.read_variant_arms_from_memory(
                        offset,
                        addr,
                        union.tag(),
                        union.cases.iter().map(|c| &c.ty),
                    );
                    self.emit(&UnionLift {
                        union,
                        ty: id,
                        name: self.iface.types[id].name.as_deref().unwrap(),
                    });
                }

                TypeDefKind::Future(_) => todo!("read future from memory"),
                TypeDefKind::Stream(_) => todo!("read stream from memory"),
            },
        }
    }

    fn read_variant_arms_from_memory<'b>(
        &mut self,
        offset: i32,
        addr: B::Operand,
        tag: Int,
        cases: impl IntoIterator<Item = &'b Type> + Clone,
    ) {
        self.stack.push(addr.clone());
        self.load_intrepr(offset, tag);
        let payload_offset =
            offset + (self.bindgen.sizes().payload_offset(tag, cases.clone()) as i32);
        for ty in cases {
            self.push_block();
            self.read_from_memory(ty, addr.clone(), payload_offset);
            self.finish_block(1);
        }
    }

    fn read_list_from_memory(&mut self, ty: &Type, addr: B::Operand, offset: i32) {
        // Read the pointer/len and then perform the standard lifting
        // proceses.
        self.stack.push(addr.clone());
        self.emit(&Instruction::I32Load { offset });
        self.stack.push(addr);
        self.emit(&Instruction::I32Load { offset: offset + 4 });
        self.lift(ty);
    }

    fn read_fields_from_memory(&mut self, tys: &[Type], addr: B::Operand, offset: i32) {
        for (field_offset, ty) in self.bindgen.sizes().field_offsets(tys).into_iter().zip(tys) {
            self.read_from_memory(ty, addr.clone(), offset + (field_offset as i32));
        }
    }

    fn emit_and_lift(&mut self, ty: &Type, addr: B::Operand, instr: &Instruction) {
        self.stack.push(addr);
        self.emit(instr);
        self.lift(ty);
    }

    fn load_intrepr(&mut self, offset: i32, repr: Int) {
        self.emit(&match repr {
            Int::U64 => Instruction::I64Load { offset },
            Int::U32 => Instruction::I32Load { offset },
            Int::U16 => Instruction::I32Load16U { offset },
            Int::U8 => Instruction::I32Load8U { offset },
        });
    }

    fn store_intrepr(&mut self, offset: i32, repr: Int) {
        self.emit(&match repr {
            Int::U64 => Instruction::I64Store { offset },
            Int::U32 => Instruction::I32Store { offset },
            Int::U16 => Instruction::I32Store16 { offset },
            Int::U8 => Instruction::I32Store8 { offset },
        });
    }

    fn is_char(&self, ty: &Type) -> bool {
        match ty {
            Type::Char => true,
            Type::Id(id) => match &self.iface.types[*id].kind {
                TypeDefKind::Type(t) => self.is_char(t),
                _ => false,
            },
            _ => false,
        }
    }
}

fn cast(from: WasmType, to: WasmType) -> Bitcast {
    use WasmType::*;

    match (from, to) {
        (I32, I32) | (I64, I64) | (F32, F32) | (F64, F64) => Bitcast::None,

        (I32, I64) => Bitcast::I32ToI64,
        (F32, I32) => Bitcast::F32ToI32,
        (F64, I64) => Bitcast::F64ToI64,

        (I64, I32) => Bitcast::I64ToI32,
        (I32, F32) => Bitcast::I32ToF32,
        (I64, F64) => Bitcast::I64ToF64,

        (F32, I64) => Bitcast::F32ToI64,
        (I64, F32) => Bitcast::I64ToF32,

        (F32, F64) | (F64, F32) | (F64, I32) | (I32, F64) => unreachable!(),
    }
}
