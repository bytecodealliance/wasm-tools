/* Copyright 2017 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! A simple event-driven library for parsing WebAssembly binary files
//! (or streams).
//!
//! The parser library reports events as they happen and only stores
//! parsing information for a brief period of time, making it very fast
//! and memory-efficient. The event-driven model, however, has some drawbacks.
//! If you need random access to the entire WebAssembly data-structure,
//! this is not the right library for you. You could however, build such
//! a data-structure using this library.

#![deny(missing_docs)]

/// A helper macro to conveniently iterate over all opcodes recognized by this
/// crate. This can be used to work with either the [`Operator`] enumeration or
/// the [`VisitOperator`] trait if your use case uniformly handles all operators
/// the same way.
///
/// This is an "iterator macro" where this macro is invoked with the name of
/// another macro, and then that macro is invoked with the list of all
/// operators. An example invocation of this looks like:
///
/// ```
/// // These names are referred to by the types of each payload and must
/// // be imported into the module using the `for_each_operator!` macro.
/// use wasmparser::{HeapType, V128, MemArg, BlockType, ValType, BrTable, Ieee32, Ieee64};
///
/// macro_rules! define_visit_operator {
///     // The outer layer of repetition represents how all operators are
///     // provided to the macro at the same time.
///     //
///     // The `$op` name is bound to the `Operator` variant name. The
///     // payload of the operator is optionally specified (the `$(...)?`
///     // clause) since not all instructions have payloads. Within the payload
///     // each argument is named and has its type specified.
///     //
///     // The `$visit` name is bound to the corresponding name in the
///     // `VisitOperator` trait that this corresponds to.
///     ($( $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
///         $(
///             fn $visit(&mut self, _offset: usize $($(,$arg: $argty)*)?) {
///                 // do nothing for this example
///             }
///         )*
///     }
/// }
///
/// pub struct VisitAndDoNothing;
///
/// impl<'a> wasmparser::VisitOperator<'a> for VisitAndDoNothing {
///     type Output = ();
///
///     wasmparser::for_each_operator!(define_visit_operator);
/// }
/// ```
#[macro_export]
macro_rules! for_each_operator {
    ($mac:ident) => {
        $mac! {
            Unreachable => visit_unreachable
            Nop => visit_nop
            Block { ty: BlockType } => visit_block
            Loop { ty: BlockType } => visit_loop
            If { ty: BlockType } => visit_if
            Else => visit_else
            Try { ty: BlockType } => visit_try
            Catch { index: u32 } => visit_catch
            Throw { index: u32 } => visit_throw
            Rethrow { relative_depth: u32 } => visit_rethrow
            End => visit_end
            Br { relative_depth: u32 } => visit_br
            BrIf { relative_depth: u32 } => visit_br_if
            BrTable { table: BrTable<'a> } => visit_br_table
            Return => visit_return
            Call { function_index: u32 } => visit_call
            CallIndirect { index: u32, table_index: u32, table_byte: u8 } => visit_call_indirect
            ReturnCall { function_index: u32 } => visit_return_call
            ReturnCallIndirect { index: u32, table_index: u32 } => visit_return_call_indirect
            CallRef => visit_call_ref
            ReturnCallRef => visit_return_call_ref
            Delegate { relative_depth: u32 } => visit_delegate
            CatchAll => visit_catch_all
            Drop => visit_drop
            Select => visit_select
            TypedSelect { ty: ValType } => visit_typed_select
            LocalGet { local_index: u32 } => visit_local_get
            LocalSet { local_index: u32 } => visit_local_set
            LocalTee { local_index: u32 } => visit_local_tee
            GlobalGet { global_index: u32 } => visit_global_get
            GlobalSet { global_index: u32 } => visit_global_set
            I32Load { memarg: MemArg } => visit_i32_load
            I64Load { memarg: MemArg } => visit_i64_load
            F32Load { memarg: MemArg } => visit_f32_load
            F64Load { memarg: MemArg } => visit_f64_load
            I32Load8S { memarg: MemArg } => visit_i32_load8_s
            I32Load8U { memarg: MemArg } => visit_i32_load8_u
            I32Load16S { memarg: MemArg } => visit_i32_load16_s
            I32Load16U { memarg: MemArg } => visit_i32_load16_u
            I64Load8S { memarg: MemArg } => visit_i64_load8_s
            I64Load8U { memarg: MemArg } => visit_i64_load8_u
            I64Load16S { memarg: MemArg } => visit_i64_load16_s
            I64Load16U { memarg: MemArg } => visit_i64_load16_u
            I64Load32S { memarg: MemArg } => visit_i64_load32_s
            I64Load32U { memarg: MemArg } => visit_i64_load32_u
            I32Store { memarg: MemArg } => visit_i32_store
            I64Store { memarg: MemArg } => visit_i64_store
            F32Store { memarg: MemArg } => visit_f32_store
            F64Store { memarg: MemArg } => visit_f64_store
            I32Store8 { memarg: MemArg } => visit_i32_store8
            I32Store16 { memarg: MemArg } => visit_i32_store16
            I64Store8 { memarg: MemArg } => visit_i64_store8
            I64Store16 { memarg: MemArg } => visit_i64_store16
            I64Store32 { memarg: MemArg } => visit_i64_store32
            MemorySize { mem: u32, mem_byte: u8 } => visit_memory_size
            MemoryGrow { mem: u32, mem_byte: u8 } => visit_memory_grow
            I32Const { value: i32 } => visit_i32_const
            I64Const { value: i64 } => visit_i64_const
            F32Const { value: Ieee32 } => visit_f32_const
            F64Const { value: Ieee64 } => visit_f64_const
            RefNull { ty: HeapType } => visit_ref_null
            RefAsNonNull => visit_ref_as_non_null
            BrOnNull { relative_depth: u32 } => visit_br_on_null
            BrOnNonNull { relative_depth: u32 } => visit_br_on_non_null
            RefIsNull => visit_ref_is_null
            RefFunc { function_index: u32 } => visit_ref_func
            I32Eqz => visit_i32_eqz
            I32Eq => visit_i32_eq
            I32Ne => visit_i32_ne
            I32LtS => visit_i32_lt_s
            I32LtU => visit_i32_lt_u
            I32GtS => visit_i32_gt_s
            I32GtU => visit_i32_gt_u
            I32LeS => visit_i32_le_s
            I32LeU => visit_i32_le_u
            I32GeS => visit_i32_ge_s
            I32GeU => visit_i32_ge_u
            I64Eqz => visit_i64_eqz
            I64Eq => visit_i64_eq
            I64Ne => visit_i64_ne
            I64LtS => visit_i64_lt_s
            I64LtU => visit_i64_lt_u
            I64GtS => visit_i64_gt_s
            I64GtU => visit_i64_gt_u
            I64LeS => visit_i64_le_s
            I64LeU => visit_i64_le_u
            I64GeS => visit_i64_ge_s
            I64GeU => visit_i64_ge_u
            F32Eq => visit_f32_eq
            F32Ne => visit_f32_ne
            F32Lt => visit_f32_lt
            F32Gt => visit_f32_gt
            F32Le => visit_f32_le
            F32Ge => visit_f32_ge
            F64Eq => visit_f64_eq
            F64Ne => visit_f64_ne
            F64Lt => visit_f64_lt
            F64Gt => visit_f64_gt
            F64Le => visit_f64_le
            F64Ge => visit_f64_ge
            I32Clz => visit_i32_clz
            I32Ctz => visit_i32_ctz
            I32Popcnt => visit_i32_popcnt
            I32Add => visit_i32_add
            I32Sub => visit_i32_sub
            I32Mul => visit_i32_mul
            I32DivS => visit_i32_div_s
            I32DivU => visit_i32_div_u
            I32RemS => visit_i32_rem_s
            I32RemU => visit_i32_rem_u
            I32And => visit_i32_and
            I32Or => visit_i32_or
            I32Xor => visit_i32_xor
            I32Shl => visit_i32_shl
            I32ShrS => visit_i32_shr_s
            I32ShrU => visit_i32_shr_u
            I32Rotl => visit_i32_rotl
            I32Rotr => visit_i32_rotr
            I64Clz => visit_i64_clz
            I64Ctz => visit_i64_ctz
            I64Popcnt => visit_i64_popcnt
            I64Add => visit_i64_add
            I64Sub => visit_i64_sub
            I64Mul => visit_i64_mul
            I64DivS => visit_i64_div_s
            I64DivU => visit_i64_div_u
            I64RemS => visit_i64_rem_s
            I64RemU => visit_i64_rem_u
            I64And => visit_i64_and
            I64Or => visit_i64_or
            I64Xor => visit_i64_xor
            I64Shl => visit_i64_shl
            I64ShrS => visit_i64_shr_s
            I64ShrU => visit_i64_shr_u
            I64Rotl => visit_i64_rotl
            I64Rotr => visit_i64_rotr
            F32Abs => visit_f32_abs
            F32Neg => visit_f32_neg
            F32Ceil => visit_f32_ceil
            F32Floor => visit_f32_floor
            F32Trunc => visit_f32_trunc
            F32Nearest => visit_f32_nearest
            F32Sqrt => visit_f32_sqrt
            F32Add => visit_f32_add
            F32Sub => visit_f32_sub
            F32Mul => visit_f32_mul
            F32Div => visit_f32_div
            F32Min => visit_f32_min
            F32Max => visit_f32_max
            F32Copysign => visit_f32_copysign
            F64Abs => visit_f64_abs
            F64Neg => visit_f64_neg
            F64Ceil => visit_f64_ceil
            F64Floor => visit_f64_floor
            F64Trunc => visit_f64_trunc
            F64Nearest => visit_f64_nearest
            F64Sqrt => visit_f64_sqrt
            F64Add => visit_f64_add
            F64Sub => visit_f64_sub
            F64Mul => visit_f64_mul
            F64Div => visit_f64_div
            F64Min => visit_f64_min
            F64Max => visit_f64_max
            F64Copysign => visit_f64_copysign
            I32WrapI64 => visit_i32_wrap_i64
            I32TruncF32S => visit_i32_trunc_f32s
            I32TruncF32U => visit_i32_trunc_f32u
            I32TruncF64S => visit_i32_trunc_f64s
            I32TruncF64U => visit_i32_trunc_f64u
            I64ExtendI32S => visit_i64_extend_i32s
            I64ExtendI32U => visit_i64_extend_i32u
            I64TruncF32S => visit_i64_trunc_f32s
            I64TruncF32U => visit_i64_trunc_f32u
            I64TruncF64S => visit_i64_trunc_f64s
            I64TruncF64U => visit_i64_trunc_f64u
            F32ConvertI32S => visit_f32_convert_i32s
            F32ConvertI32U => visit_f32_convert_i32u
            F32ConvertI64S => visit_f32_convert_i64s
            F32ConvertI64U => visit_f32_convert_i64u
            F32DemoteF64 => visit_f32_demote_f64
            F64ConvertI32S => visit_f64_convert_i32_s
            F64ConvertI32U => visit_f64_convert_i32_u
            F64ConvertI64S => visit_f64_convert_i64_s
            F64ConvertI64U => visit_f64_convert_i64_u
            F64PromoteF32 => visit_f64_promote_f32
            I32ReinterpretF32 => visit_i32_reinterpret_f32
            I64ReinterpretF64 => visit_i64_reinterpret_f64
            F32ReinterpretI32 => visit_f32_reinterpret_i32
            F64ReinterpretI64 => visit_f64_reinterpret_i64
            I32Extend8S => visit_i32_extend8_s
            I32Extend16S => visit_i32_extend16_s
            I64Extend8S => visit_i64_extend8_s
            I64Extend16S => visit_i64_extend16_s
            I64Extend32S => visit_i64_extend32_s

            // 0xFC operators
            // Non-trapping Float-to-int Conversions
            I32TruncSatF32S => visit_i32_trunc_sat_f32_s
            I32TruncSatF32U => visit_i32_trunc_sat_f32_u
            I32TruncSatF64S => visit_i32_trunc_sat_f64_s
            I32TruncSatF64U => visit_i32_trunc_sat_f64_u
            I64TruncSatF32S => visit_i64_trunc_sat_f32_s
            I64TruncSatF32U => visit_i64_trunc_sat_f32_u
            I64TruncSatF64S => visit_i64_trunc_sat_f64_s
            I64TruncSatF64U => visit_i64_trunc_sat_f64_u

            // 0xFC operators
            // bulk memory https://github.com/WebAssembly/bulk-memory-operations/blob/master/proposals/bulk-memory-operations/Overview.md
            MemoryInit { segment: u32, mem: u32 } => visit_memory_init
            DataDrop { segment: u32 } => visit_data_drop
            MemoryCopy { dst: u32, src: u32 } => visit_memory_copy
            MemoryFill { mem: u32 } => visit_memory_fill
            TableInit { segment: u32, table: u32 } => visit_table_init
            ElemDrop { segment: u32 } => visit_elem_drop
            TableCopy { dst_table: u32, src_table: u32 } => visit_table_copy
            TableFill { table: u32 } => visit_table_fill
            TableGet { table: u32 } => visit_table_get
            TableSet { table: u32 } => visit_table_set
            TableGrow { table: u32 } => visit_table_grow
            TableSize { table: u32 } => visit_table_size

            // 0xFE operators
            // https://github.com/WebAssembly/threads/blob/master/proposals/threads/Overview.md
            MemoryAtomicNotify { memarg: MemArg } => visit_memory_atomic_notify
            MemoryAtomicWait32 { memarg: MemArg } => visit_memory_atomic_wait32
            MemoryAtomicWait64 { memarg: MemArg } => visit_memory_atomic_wait64
            AtomicFence { flags: u8 } => visit_atomic_fence
            I32AtomicLoad { memarg: MemArg } => visit_i32_atomic_load
            I64AtomicLoad { memarg: MemArg } => visit_i64_atomic_load
            I32AtomicLoad8U { memarg: MemArg } => visit_i32_atomic_load8_u
            I32AtomicLoad16U { memarg: MemArg } => visit_i32_atomic_load16_u
            I64AtomicLoad8U { memarg: MemArg } => visit_i64_atomic_load8_u
            I64AtomicLoad16U { memarg: MemArg } => visit_i64_atomic_load16_u
            I64AtomicLoad32U { memarg: MemArg } => visit_i64_atomic_load32_u
            I32AtomicStore { memarg: MemArg } => visit_i32_atomic_store
            I64AtomicStore { memarg: MemArg } => visit_i64_atomic_store
            I32AtomicStore8 { memarg: MemArg } => visit_i32_atomic_store8
            I32AtomicStore16 { memarg: MemArg } => visit_i32_atomic_store16
            I64AtomicStore8 { memarg: MemArg } => visit_i64_atomic_store8
            I64AtomicStore16 { memarg: MemArg } => visit_i64_atomic_store16
            I64AtomicStore32 { memarg: MemArg } => visit_i64_atomic_store32
            I32AtomicRmwAdd { memarg: MemArg } => visit_i32_atomic_rmw_add
            I64AtomicRmwAdd { memarg: MemArg } => visit_i64_atomic_rmw_add
            I32AtomicRmw8AddU { memarg: MemArg } => visit_i32_atomic_rmw8_add_u
            I32AtomicRmw16AddU { memarg: MemArg } => visit_i32_atomic_rmw16_add_u
            I64AtomicRmw8AddU { memarg: MemArg } => visit_i64_atomic_rmw8_add_u
            I64AtomicRmw16AddU { memarg: MemArg } => visit_i64_atomic_rmw16_add_u
            I64AtomicRmw32AddU { memarg: MemArg } => visit_i64_atomic_rmw32_add_u
            I32AtomicRmwSub { memarg: MemArg } => visit_i32_atomic_rmw_sub
            I64AtomicRmwSub { memarg: MemArg } => visit_i64_atomic_rmw_sub
            I32AtomicRmw8SubU { memarg: MemArg } => visit_i32_atomic_rmw8_sub_u
            I32AtomicRmw16SubU { memarg: MemArg } => visit_i32_atomic_rmw16_sub_u
            I64AtomicRmw8SubU { memarg: MemArg } => visit_i64_atomic_rmw8_sub_u
            I64AtomicRmw16SubU { memarg: MemArg } => visit_i64_atomic_rmw16_sub_u
            I64AtomicRmw32SubU { memarg: MemArg } => visit_i64_atomic_rmw32_sub_u
            I32AtomicRmwAnd { memarg: MemArg } => visit_i32_atomic_rmw_and
            I64AtomicRmwAnd { memarg: MemArg } => visit_i64_atomic_rmw_and
            I32AtomicRmw8AndU { memarg: MemArg } => visit_i32_atomic_rmw8_and_u
            I32AtomicRmw16AndU { memarg: MemArg } => visit_i32_atomic_rmw16_and_u
            I64AtomicRmw8AndU { memarg: MemArg } => visit_i64_atomic_rmw8_and_u
            I64AtomicRmw16AndU { memarg: MemArg } => visit_i64_atomic_rmw16_and_u
            I64AtomicRmw32AndU { memarg: MemArg } => visit_i64_atomic_rmw32_and_u
            I32AtomicRmwOr { memarg: MemArg } => visit_i32_atomic_rmw_or
            I64AtomicRmwOr { memarg: MemArg } => visit_i64_atomic_rmw_or
            I32AtomicRmw8OrU { memarg: MemArg } => visit_i32_atomic_rmw8_or_u
            I32AtomicRmw16OrU { memarg: MemArg } => visit_i32_atomic_rmw16_or_u
            I64AtomicRmw8OrU { memarg: MemArg } => visit_i64_atomic_rmw8_or_u
            I64AtomicRmw16OrU { memarg: MemArg } => visit_i64_atomic_rmw16_or_u
            I64AtomicRmw32OrU { memarg: MemArg } => visit_i64_atomic_rmw32_or_u
            I32AtomicRmwXor { memarg: MemArg } => visit_i32_atomic_rmw_xor
            I64AtomicRmwXor { memarg: MemArg } => visit_i64_atomic_rmw_xor
            I32AtomicRmw8XorU { memarg: MemArg } => visit_i32_atomic_rmw8_xor_u
            I32AtomicRmw16XorU { memarg: MemArg } => visit_i32_atomic_rmw16_xor_u
            I64AtomicRmw8XorU { memarg: MemArg } => visit_i64_atomic_rmw8_xor_u
            I64AtomicRmw16XorU { memarg: MemArg } => visit_i64_atomic_rmw16_xor_u
            I64AtomicRmw32XorU { memarg: MemArg } => visit_i64_atomic_rmw32_xor_u
            I32AtomicRmwXchg { memarg: MemArg } => visit_i32_atomic_rmw_xchg
            I64AtomicRmwXchg { memarg: MemArg } => visit_i64_atomic_rmw_xchg
            I32AtomicRmw8XchgU { memarg: MemArg } => visit_i32_atomic_rmw8_xchg_u
            I32AtomicRmw16XchgU { memarg: MemArg } => visit_i32_atomic_rmw16_xchg_u
            I64AtomicRmw8XchgU { memarg: MemArg } => visit_i64_atomic_rmw8_xchg_u
            I64AtomicRmw16XchgU { memarg: MemArg } => visit_i64_atomic_rmw16_xchg_u
            I64AtomicRmw32XchgU { memarg: MemArg } => visit_i64_atomic_rmw32_xchg_u
            I32AtomicRmwCmpxchg { memarg: MemArg } => visit_i32_atomic_rmw_cmpxchg
            I64AtomicRmwCmpxchg { memarg: MemArg } => visit_i64_atomic_rmw_cmpxchg
            I32AtomicRmw8CmpxchgU { memarg: MemArg } => visit_i32_atomic_rmw8_cmpxchg_u
            I32AtomicRmw16CmpxchgU { memarg: MemArg } => visit_i32_atomic_rmw16_cmpxchg_u
            I64AtomicRmw8CmpxchgU { memarg: MemArg } => visit_i64_atomic_rmw8_cmpxchg_u
            I64AtomicRmw16CmpxchgU { memarg: MemArg } => visit_i64_atomic_rmw16_cmpxchg_u
            I64AtomicRmw32CmpxchgU { memarg: MemArg } => visit_i64_atomic_rmw32_cmpxchg_u

            // 0xFD operators
            // SIMD https://webassembly.github.io/simd/core/binary/instructions.html
            V128Load { memarg: MemArg } => visit_v128_load
            V128Load8x8S { memarg: MemArg } => visit_v128_load8x8_s
            V128Load8x8U { memarg: MemArg } => visit_v128_load8x8_u
            V128Load16x4S { memarg: MemArg } => visit_v128_load16x4_s
            V128Load16x4U { memarg: MemArg } => visit_v128_load16x4_u
            V128Load32x2S { memarg: MemArg } => visit_v128_load32x2_s
            V128Load32x2U { memarg: MemArg } => visit_v128_load32x2_u
            V128Load8Splat { memarg: MemArg } => visit_v128_load8_splat
            V128Load16Splat { memarg: MemArg } => visit_v128_load16_splat
            V128Load32Splat { memarg: MemArg } => visit_v128_load32_splat
            V128Load64Splat { memarg: MemArg } => visit_v128_load64_splat
            V128Load32Zero { memarg: MemArg } => visit_v128_load32_zero
            V128Load64Zero { memarg: MemArg } => visit_v128_load64_zero
            V128Store { memarg: MemArg } => visit_v128_store
            V128Load8Lane { memarg: MemArg, lane: u8 } => visit_v128_load8_lane
            V128Load16Lane { memarg: MemArg, lane: u8 } => visit_v128_load16_lane
            V128Load32Lane { memarg: MemArg, lane: u8 } => visit_v128_load32_lane
            V128Load64Lane { memarg: MemArg, lane: u8 } => visit_v128_load64_lane
            V128Store8Lane { memarg: MemArg, lane: u8 } => visit_v128_store8_lane
            V128Store16Lane { memarg: MemArg, lane: u8 } => visit_v128_store16_lane
            V128Store32Lane { memarg: MemArg, lane: u8 } => visit_v128_store32_lane
            V128Store64Lane { memarg: MemArg, lane: u8 } => visit_v128_store64_lane
            V128Const { value: V128 } => visit_v128_const
            I8x16Shuffle { lanes: [u8; 16] } => visit_i8x16_shuffle
            I8x16ExtractLaneS { lane: u8 } => visit_i8x16_extract_lane_s
            I8x16ExtractLaneU { lane: u8 } => visit_i8x16_extract_lane_u
            I8x16ReplaceLane { lane: u8 } => visit_i8x16_replace_lane
            I16x8ExtractLaneS { lane: u8 } => visit_i16x8_extract_lane_s
            I16x8ExtractLaneU { lane: u8 } => visit_i16x8_extract_lane_u
            I16x8ReplaceLane { lane: u8 } => visit_i16x8_replace_lane
            I32x4ExtractLane { lane: u8 } => visit_i32x4_extract_lane
            I32x4ReplaceLane { lane: u8 } => visit_i32x4_replace_lane
            I64x2ExtractLane { lane: u8 } => visit_i64x2_extract_lane
            I64x2ReplaceLane { lane: u8 } => visit_i64x2_replace_lane
            F32x4ExtractLane { lane: u8 } => visit_f32x4_extract_lane
            F32x4ReplaceLane { lane: u8 } => visit_f32x4_replace_lane
            F64x2ExtractLane { lane: u8 } => visit_f64x2_extract_lane
            F64x2ReplaceLane { lane: u8 } => visit_f64x2_replace_lane
            I8x16Swizzle => visit_i8x16_swizzle
            I8x16Splat => visit_i8x16_splat
            I16x8Splat => visit_i16x8_splat
            I32x4Splat => visit_i32x4_splat
            I64x2Splat => visit_i64x2_splat
            F32x4Splat => visit_f32x4_splat
            F64x2Splat => visit_f64x2_splat
            I8x16Eq => visit_i8x16_eq
            I8x16Ne => visit_i8x16_ne
            I8x16LtS => visit_i8x16_lt_s
            I8x16LtU => visit_i8x16_lt_u
            I8x16GtS => visit_i8x16_gt_s
            I8x16GtU => visit_i8x16_gt_u
            I8x16LeS => visit_i8x16_le_s
            I8x16LeU => visit_i8x16_le_u
            I8x16GeS => visit_i8x16_ge_s
            I8x16GeU => visit_i8x16_ge_u
            I16x8Eq => visit_i16x8_eq
            I16x8Ne => visit_i16x8_ne
            I16x8LtS => visit_i16x8_lt_s
            I16x8LtU => visit_i16x8_lt_u
            I16x8GtS => visit_i16x8_gt_s
            I16x8GtU => visit_i16x8_gt_u
            I16x8LeS => visit_i16x8_le_s
            I16x8LeU => visit_i16x8_le_u
            I16x8GeS => visit_i16x8_ge_s
            I16x8GeU => visit_i16x8_ge_u
            I32x4Eq => visit_i32x4_eq
            I32x4Ne => visit_i32x4_ne
            I32x4LtS => visit_i32x4_lt_s
            I32x4LtU => visit_i32x4_lt_u
            I32x4GtS => visit_i32x4_gt_s
            I32x4GtU => visit_i32x4_gt_u
            I32x4LeS => visit_i32x4_le_s
            I32x4LeU => visit_i32x4_le_u
            I32x4GeS => visit_i32x4_ge_s
            I32x4GeU => visit_i32x4_ge_u
            I64x2Eq => visit_i64x2_eq
            I64x2Ne => visit_i64x2_ne
            I64x2LtS => visit_i64x2_lt_s
            I64x2GtS => visit_i64x2_gt_s
            I64x2LeS => visit_i64x2_le_s
            I64x2GeS => visit_i64x2_ge_s
            F32x4Eq => visit_f32x4_eq
            F32x4Ne => visit_f32x4_ne
            F32x4Lt => visit_f32x4_lt
            F32x4Gt => visit_f32x4_gt
            F32x4Le => visit_f32x4_le
            F32x4Ge => visit_f32x4_ge
            F64x2Eq => visit_f64x2_eq
            F64x2Ne => visit_f64x2_ne
            F64x2Lt => visit_f64x2_lt
            F64x2Gt => visit_f64x2_gt
            F64x2Le => visit_f64x2_le
            F64x2Ge => visit_f64x2_ge
            V128Not => visit_v128_not
            V128And => visit_v128_and
            V128AndNot => visit_v128_andnot
            V128Or => visit_v128_or
            V128Xor => visit_v128_xor
            V128Bitselect => visit_v128_bitselect
            V128AnyTrue => visit_v128_any_true
            I8x16Abs => visit_i8x16_abs
            I8x16Neg => visit_i8x16_neg
            I8x16Popcnt => visit_i8x16_popcnt
            I8x16AllTrue => visit_i8x16_all_true
            I8x16Bitmask => visit_i8x16_bitmask
            I8x16NarrowI16x8S => visit_i8x16_narrow_i16x8_s
            I8x16NarrowI16x8U => visit_i8x16_narrow_i16x8_u
            I8x16Shl => visit_i8x16_shl
            I8x16ShrS => visit_i8x16_shr_s
            I8x16ShrU => visit_i8x16_shr_u
            I8x16Add => visit_i8x16_add
            I8x16AddSatS => visit_i8x16_add_sat_s
            I8x16AddSatU => visit_i8x16_add_sat_u
            I8x16Sub => visit_i8x16_sub
            I8x16SubSatS => visit_i8x16_sub_sat_s
            I8x16SubSatU => visit_i8x16_sub_sat_u
            I8x16MinS => visit_i8x16_min_s
            I8x16MinU => visit_i8x16_min_u
            I8x16MaxS => visit_i8x16_max_s
            I8x16MaxU => visit_i8x16_max_u
            I8x16RoundingAverageU => visit_i8x16_avgr_u
            I16x8ExtAddPairwiseI8x16S => visit_i16x8_extadd_pairwise_i8x16_s
            I16x8ExtAddPairwiseI8x16U => visit_i16x8_extadd_pairwise_i8x16_u
            I16x8Abs => visit_i16x8_abs
            I16x8Neg => visit_i16x8_neg
            I16x8Q15MulrSatS => visit_i16x8_q15mulr_sat_s
            I16x8AllTrue => visit_i16x8_all_true
            I16x8Bitmask => visit_i16x8_bitmask
            I16x8NarrowI32x4S => visit_i16x8_narrow_i32x4_s
            I16x8NarrowI32x4U => visit_i16x8_narrow_i32x4_u
            I16x8ExtendLowI8x16S => visit_i16x8_extend_low_i8x16_s
            I16x8ExtendHighI8x16S => visit_i16x8_extend_high_i8x16_s
            I16x8ExtendLowI8x16U => visit_i16x8_extend_low_i8x16_u
            I16x8ExtendHighI8x16U => visit_i16x8_extend_high_i8x16_u
            I16x8Shl => visit_i16x8_shl
            I16x8ShrS => visit_i16x8_shr_s
            I16x8ShrU => visit_i16x8_shr_u
            I16x8Add => visit_i16x8_add
            I16x8AddSatS => visit_i16x8_add_sat_s
            I16x8AddSatU => visit_i16x8_add_sat_u
            I16x8Sub => visit_i16x8_sub
            I16x8SubSatS => visit_i16x8_sub_sat_s
            I16x8SubSatU => visit_i16x8_sub_sat_u
            I16x8Mul => visit_i16x8_mul
            I16x8MinS => visit_i16x8_min_s
            I16x8MinU => visit_i16x8_min_u
            I16x8MaxS => visit_i16x8_max_s
            I16x8MaxU => visit_i16x8_max_u
            I16x8RoundingAverageU => visit_i16x8_avgr_u
            I16x8ExtMulLowI8x16S => visit_i16x8_extmul_low_i8x16_s
            I16x8ExtMulHighI8x16S => visit_i16x8_extmul_high_i8x16_s
            I16x8ExtMulLowI8x16U => visit_i16x8_extmul_low_i8x16_u
            I16x8ExtMulHighI8x16U => visit_i16x8_extmul_high_i8x16_u
            I32x4ExtAddPairwiseI16x8S => visit_i32x4_extadd_pairwise_i16x8_s
            I32x4ExtAddPairwiseI16x8U => visit_i32x4_extadd_pairwise_i16x8_u
            I32x4Abs => visit_i32x4_abs
            I32x4Neg => visit_i32x4_neg
            I32x4AllTrue => visit_i32x4_all_true
            I32x4Bitmask => visit_i32x4_bitmask
            I32x4ExtendLowI16x8S => visit_i32x4_extend_low_i16x8_s
            I32x4ExtendHighI16x8S => visit_i32x4_extend_high_i16x8_s
            I32x4ExtendLowI16x8U => visit_i32x4_extend_low_i16x8_u
            I32x4ExtendHighI16x8U => visit_i32x4_extend_high_i16x8_u
            I32x4Shl => visit_i32x4_shl
            I32x4ShrS => visit_i32x4_shr_s
            I32x4ShrU => visit_i32x4_shr_u
            I32x4Add => visit_i32x4_add
            I32x4Sub => visit_i32x4_sub
            I32x4Mul => visit_i32x4_mul
            I32x4MinS => visit_i32x4_min_s
            I32x4MinU => visit_i32x4_min_u
            I32x4MaxS => visit_i32x4_max_s
            I32x4MaxU => visit_i32x4_max_u
            I32x4DotI16x8S => visit_i32x4_dot_i16x8_s
            I32x4ExtMulLowI16x8S => visit_i32x4_extmul_low_i16x8_s
            I32x4ExtMulHighI16x8S => visit_i32x4_extmul_high_i16x8_s
            I32x4ExtMulLowI16x8U => visit_i32x4_extmul_low_i16x8_u
            I32x4ExtMulHighI16x8U => visit_i32x4_extmul_high_i16x8_u
            I64x2Abs => visit_i64x2_abs
            I64x2Neg => visit_i64x2_neg
            I64x2AllTrue => visit_i64x2_all_true
            I64x2Bitmask => visit_i64x2_bitmask
            I64x2ExtendLowI32x4S => visit_i64x2_extend_low_i32x4_s
            I64x2ExtendHighI32x4S => visit_i64x2_extend_high_i32x4_s
            I64x2ExtendLowI32x4U => visit_i64x2_extend_low_i32x4_u
            I64x2ExtendHighI32x4U => visit_i64x2_extend_high_i32x4_u
            I64x2Shl => visit_i64x2_shl
            I64x2ShrS => visit_i64x2_shr_s
            I64x2ShrU => visit_i64x2_shr_u
            I64x2Add => visit_i64x2_add
            I64x2Sub => visit_i64x2_sub
            I64x2Mul => visit_i64x2_mul
            I64x2ExtMulLowI32x4S => visit_i64x2_extmul_low_i32x4_s
            I64x2ExtMulHighI32x4S => visit_i64x2_extmul_high_i32x4_s
            I64x2ExtMulLowI32x4U => visit_i64x2_extmul_low_i32x4_u
            I64x2ExtMulHighI32x4U => visit_i64x2_extmul_high_i32x4_u
            F32x4Ceil => visit_f32x4_ceil
            F32x4Floor => visit_f32x4_floor
            F32x4Trunc => visit_f32x4_trunc
            F32x4Nearest => visit_f32x4_nearest
            F32x4Abs => visit_f32x4_abs
            F32x4Neg => visit_f32x4_neg
            F32x4Sqrt => visit_f32x4_sqrt
            F32x4Add => visit_f32x4_add
            F32x4Sub => visit_f32x4_sub
            F32x4Mul => visit_f32x4_mul
            F32x4Div => visit_f32x4_div
            F32x4Min => visit_f32x4_min
            F32x4Max => visit_f32x4_max
            F32x4PMin => visit_f32x4_pmin
            F32x4PMax => visit_f32x4_pmax
            F64x2Ceil => visit_f64x2_ceil
            F64x2Floor => visit_f64x2_floor
            F64x2Trunc => visit_f64x2_trunc
            F64x2Nearest => visit_f64x2_nearest
            F64x2Abs => visit_f64x2_abs
            F64x2Neg => visit_f64x2_neg
            F64x2Sqrt => visit_f64x2_sqrt
            F64x2Add => visit_f64x2_add
            F64x2Sub => visit_f64x2_sub
            F64x2Mul => visit_f64x2_mul
            F64x2Div => visit_f64x2_div
            F64x2Min => visit_f64x2_min
            F64x2Max => visit_f64x2_max
            F64x2PMin => visit_f64x2_pmin
            F64x2PMax => visit_f64x2_pmax
            I32x4TruncSatF32x4S => visit_i32x4_trunc_sat_f32x4_s
            I32x4TruncSatF32x4U => visit_i32x4_trunc_sat_f32x4_u
            F32x4ConvertI32x4S => visit_f32x4_convert_i32x4_s
            F32x4ConvertI32x4U => visit_f32x4_convert_i32x4_u
            I32x4TruncSatF64x2SZero => visit_i32x4_trunc_sat_f64x2_s_zero
            I32x4TruncSatF64x2UZero => visit_i32x4_trunc_sat_f64x2_u_zero
            F64x2ConvertLowI32x4S => visit_f64x2_convert_low_i32x4_s
            F64x2ConvertLowI32x4U => visit_f64x2_convert_low_i32x4_u
            F32x4DemoteF64x2Zero => visit_f32x4_demote_f64x2_zero
            F64x2PromoteLowF32x4 => visit_f64x2_promote_low_f32x4

            // Relaxed SIMD operators
            I8x16RelaxedSwizzle => visit_i8x16_relaxed_swizzle
            I32x4RelaxedTruncSatF32x4S => visit_i32x4_relaxed_trunc_sat_f32x4_s
            I32x4RelaxedTruncSatF32x4U => visit_i32x4_relaxed_trunc_sat_f32x4_u
            I32x4RelaxedTruncSatF64x2SZero => visit_i32x4_relaxed_trunc_sat_f64x2_s_zero
            I32x4RelaxedTruncSatF64x2UZero => visit_i32x4_relaxed_trunc_sat_f64x2_u_zero
            F32x4Fma => visit_f32x4_fma
            F32x4Fms => visit_f32x4_fms
            F64x2Fma => visit_f64x2_fma
            F64x2Fms => visit_f64x2_fms
            I8x16LaneSelect => visit_i8x16_laneselect
            I16x8LaneSelect => visit_i16x8_laneselect
            I32x4LaneSelect => visit_i32x4_laneselect
            I64x2LaneSelect => visit_i64x2_laneselect
            F32x4RelaxedMin => visit_f32x4_relaxed_min
            F32x4RelaxedMax => visit_f32x4_relaxed_max
            F64x2RelaxedMin => visit_f64x2_relaxed_min
            F64x2RelaxedMax => visit_f64x2_relaxed_max
        }
    };
}

pub use crate::binary_reader::{BinaryReader, BinaryReaderError, Result};
pub use crate::parser::*;
pub use crate::readers::*;
pub use crate::resources::*;
pub use crate::validator::*;

mod binary_reader;
mod limits;
mod parser;
mod readers;
mod resources;
mod validator;
