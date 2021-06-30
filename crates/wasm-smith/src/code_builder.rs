use super::{
    BlockType, Config, ConfiguredModule, Elements, FuncType, Instruction, MemArg, ValType,
};
use arbitrary::{Result, Unstructured};
use std::collections::{BTreeMap, BTreeSet};

macro_rules! instructions {
	(
        $(
            ($predicate:expr, $generator_fn:ident $(, $cost:tt)?),
        )*
    ) => {
        static NUM_OPTIONS: usize = instructions!(
            @count;
            $( $generator_fn )*
        );

        fn choose_instruction<C>(
            u: &mut Unstructured<'_>,
            module: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        ) -> Option<
            fn(&mut Unstructured<'_>, &ConfiguredModule<C>, &mut CodeBuilder<C>) -> Result<Instruction>
        >
        where
            C: Config
        {
            builder.allocs.options.clear();
            let mut cost = 0;
            // Unroll the loop that checks whether each instruction is valid in
            // the current context and, if it is valid, pushes it onto our
            // options. Unrolling this loops lets us avoid dynamic calls through
            // function pointers and, furthermore, each call site can be branch
            // predicted and even inlined. This saved us about 30% of time in
            // the `corpus` benchmark.
            $(
                let predicate: Option<fn(&ConfiguredModule<C>, &mut CodeBuilder<C>) -> bool> = $predicate;
                if predicate.map_or(true, |f| f(module, builder)) {

                    builder.allocs.options.push(($generator_fn, cost));
                    cost += 1000 $(- $cost)?;
                }
            )*

            debug_assert!(cost > 0);

            let i = u.int_in_range(0..=cost).ok()?;
            let idx = builder
                .allocs
                .options
                .binary_search_by_key(&i,|p| p.1)
                .unwrap_or_else(|i| i - 1);
            Some(builder.allocs.options[idx].0)
        }
	};

    ( @count; ) => {
        0
    };
    ( @count; $x:ident $( $xs:ident )* ) => {
        1 + instructions!( @count; $( $xs )* )
    };
}

// The static set of options of instruction to generate that could be valid at
// some given time. One entry per Wasm instruction.
//
// Each entry is made up of up to three parts:
//
// 1. A predicate for whether this is a valid choice, if any. `None` means that
//    the choice is always applicable.
//
// 2. The function to generate the instruction, given that we've made this
//    choice.
//
// 3. An optional number used to weight how often this instruction is chosen.
//    Higher numbers are less likely to be chosen, and number specified must be
//    less than 1000.
instructions! {
    // Control instructions.
    (None, unreachable, 990),
    (None, nop, 800),
    (None, block),
    (None, r#loop),
    (Some(if_valid), r#if),
    (Some(else_valid), r#else),
    (Some(end_valid), end),
    (Some(br_valid), br),
    (Some(br_if_valid), br_if),
    (Some(br_table_valid), br_table),
    (Some(return_valid), r#return, 900),
    (Some(call_valid), call),
    (Some(call_indirect_valid), call_indirect),
    // Parametric instructions.
    (Some(drop_valid), drop),
    (Some(select_valid), select),
    // Variable instructions.
    (Some(local_get_valid), local_get),
    (Some(local_set_valid), local_set),
    (Some(local_set_valid), local_tee),
    (Some(global_get_valid), global_get),
    (Some(global_set_valid), global_set),
    // Memory instructions.
    (Some(have_memory_and_offset), i32_load),
    (Some(have_memory_and_offset), i64_load),
    (Some(have_memory_and_offset), f32_load),
    (Some(have_memory_and_offset), f64_load),
    (Some(have_memory_and_offset), i32_load_8_s),
    (Some(have_memory_and_offset), i32_load_8_u),
    (Some(have_memory_and_offset), i32_load_16_s),
    (Some(have_memory_and_offset), i32_load_16_u),
    (Some(have_memory_and_offset), i64_load_8_s),
    (Some(have_memory_and_offset), i64_load_16_s),
    (Some(have_memory_and_offset), i64_load_32_s),
    (Some(have_memory_and_offset), i64_load_8_u),
    (Some(have_memory_and_offset), i64_load_16_u),
    (Some(have_memory_and_offset), i64_load_32_u),
    (Some(i32_store_valid), i32_store),
    (Some(i64_store_valid), i64_store),
    (Some(f32_store_valid), f32_store),
    (Some(f64_store_valid), f64_store),
    (Some(i32_store_valid), i32_store_8),
    (Some(i32_store_valid), i32_store_16),
    (Some(i64_store_valid), i64_store_8),
    (Some(i64_store_valid), i64_store_16),
    (Some(i64_store_valid), i64_store_32),
    (Some(have_memory), memory_size),
    (Some(memory_grow_valid), memory_grow),
    (Some(memory_init_valid), memory_init),
    (Some(data_drop_valid), data_drop),
    (Some(memory_copy_valid), memory_copy),
    (Some(memory_fill_valid), memory_fill),
    // Numeric instructions.
    (None, i32_const),
    (None, i64_const),
    (None, f32_const),
    (None, f64_const),
    (Some(i32_on_stack), i32_eqz),
    (Some(i32_i32_on_stack), i32_eq),
    (Some(i32_i32_on_stack), i32_neq),
    (Some(i32_i32_on_stack), i32_lt_s),
    (Some(i32_i32_on_stack), i32_lt_u),
    (Some(i32_i32_on_stack), i32_gt_s),
    (Some(i32_i32_on_stack), i32_gt_u),
    (Some(i32_i32_on_stack), i32_le_s),
    (Some(i32_i32_on_stack), i32_le_u),
    (Some(i32_i32_on_stack), i32_ge_s),
    (Some(i32_i32_on_stack), i32_ge_u),
    (Some(i64_on_stack), i64_eqz),
    (Some(i64_i64_on_stack), i64_eq),
    (Some(i64_i64_on_stack), i64_neq),
    (Some(i64_i64_on_stack), i64_lt_s),
    (Some(i64_i64_on_stack), i64_lt_u),
    (Some(i64_i64_on_stack), i64_gt_s),
    (Some(i64_i64_on_stack), i64_gt_u),
    (Some(i64_i64_on_stack), i64_le_s),
    (Some(i64_i64_on_stack), i64_le_u),
    (Some(i64_i64_on_stack), i64_ge_s),
    (Some(i64_i64_on_stack), i64_ge_u),
    (Some(f32_f32_on_stack), f32_eq),
    (Some(f32_f32_on_stack), f32_neq),
    (Some(f32_f32_on_stack), f32_lt),
    (Some(f32_f32_on_stack), f32_gt),
    (Some(f32_f32_on_stack), f32_le),
    (Some(f32_f32_on_stack), f32_ge),
    (Some(f64_f64_on_stack), f64_eq),
    (Some(f64_f64_on_stack), f64_neq),
    (Some(f64_f64_on_stack), f64_lt),
    (Some(f64_f64_on_stack), f64_gt),
    (Some(f64_f64_on_stack), f64_le),
    (Some(f64_f64_on_stack), f64_ge),
    (Some(i32_on_stack), i32_clz),
    (Some(i32_on_stack), i32_ctz),
    (Some(i32_on_stack), i32_popcnt),
    (Some(i32_i32_on_stack), i32_add),
    (Some(i32_i32_on_stack), i32_sub),
    (Some(i32_i32_on_stack), i32_mul),
    (Some(i32_i32_on_stack), i32_div_s),
    (Some(i32_i32_on_stack), i32_div_u),
    (Some(i32_i32_on_stack), i32_rem_s),
    (Some(i32_i32_on_stack), i32_rem_u),
    (Some(i32_i32_on_stack), i32_and),
    (Some(i32_i32_on_stack), i32_or),
    (Some(i32_i32_on_stack), i32_xor),
    (Some(i32_i32_on_stack), i32_shl),
    (Some(i32_i32_on_stack), i32_shr_s),
    (Some(i32_i32_on_stack), i32_shr_u),
    (Some(i32_i32_on_stack), i32_rotl),
    (Some(i32_i32_on_stack), i32_rotr),
    (Some(i64_on_stack), i64_clz),
    (Some(i64_on_stack), i64_ctz),
    (Some(i64_on_stack), i64_popcnt),
    (Some(i64_i64_on_stack), i64_add),
    (Some(i64_i64_on_stack), i64_sub),
    (Some(i64_i64_on_stack), i64_mul),
    (Some(i64_i64_on_stack), i64_div_s),
    (Some(i64_i64_on_stack), i64_div_u),
    (Some(i64_i64_on_stack), i64_rem_s),
    (Some(i64_i64_on_stack), i64_rem_u),
    (Some(i64_i64_on_stack), i64_and),
    (Some(i64_i64_on_stack), i64_or),
    (Some(i64_i64_on_stack), i64_xor),
    (Some(i64_i64_on_stack), i64_shl),
    (Some(i64_i64_on_stack), i64_shr_s),
    (Some(i64_i64_on_stack), i64_shr_u),
    (Some(i64_i64_on_stack), i64_rotl),
    (Some(i64_i64_on_stack), i64_rotr),
    (Some(f32_on_stack), f32_abs),
    (Some(f32_on_stack), f32_neg),
    (Some(f32_on_stack), f32_ceil),
    (Some(f32_on_stack), f32_floor),
    (Some(f32_on_stack), f32_trunc),
    (Some(f32_on_stack), f32_nearest),
    (Some(f32_on_stack), f32_sqrt),
    (Some(f32_f32_on_stack), f32_add),
    (Some(f32_f32_on_stack), f32_sub),
    (Some(f32_f32_on_stack), f32_mul),
    (Some(f32_f32_on_stack), f32_div),
    (Some(f32_f32_on_stack), f32_min),
    (Some(f32_f32_on_stack), f32_max),
    (Some(f32_f32_on_stack), f32_copysign),
    (Some(f64_on_stack), f64_abs),
    (Some(f64_on_stack), f64_neg),
    (Some(f64_on_stack), f64_ceil),
    (Some(f64_on_stack), f64_floor),
    (Some(f64_on_stack), f64_trunc),
    (Some(f64_on_stack), f64_nearest),
    (Some(f64_on_stack), f64_sqrt),
    (Some(f64_f64_on_stack), f64_add),
    (Some(f64_f64_on_stack), f64_sub),
    (Some(f64_f64_on_stack), f64_mul),
    (Some(f64_f64_on_stack), f64_div),
    (Some(f64_f64_on_stack), f64_min),
    (Some(f64_f64_on_stack), f64_max),
    (Some(f64_f64_on_stack), f64_copysign),
    (Some(i64_on_stack), i32_wrap_i64),
    (Some(f32_on_stack), i32_trunc_f32_s),
    (Some(f32_on_stack), i32_trunc_f32_u),
    (Some(f64_on_stack), i32_trunc_f64_s),
    (Some(f64_on_stack), i32_trunc_f64_u),
    (Some(i32_on_stack), i64_extend_i32_s),
    (Some(i32_on_stack), i64_extend_i32_u),
    (Some(f32_on_stack), i64_trunc_f32_s),
    (Some(f32_on_stack), i64_trunc_f32_u),
    (Some(f64_on_stack), i64_trunc_f64_s),
    (Some(f64_on_stack), i64_trunc_f64_u),
    (Some(i32_on_stack), f32_convert_i32_s),
    (Some(i32_on_stack), f32_convert_i32_u),
    (Some(i64_on_stack), f32_convert_i64_s),
    (Some(i64_on_stack), f32_convert_i64_u),
    (Some(f64_on_stack), f32_demote_f64),
    (Some(i32_on_stack), f64_convert_i32_s),
    (Some(i32_on_stack), f64_convert_i32_u),
    (Some(i64_on_stack), f64_convert_i64_s),
    (Some(i64_on_stack), f64_convert_i64_u),
    (Some(f32_on_stack), f64_promote_f32),
    (Some(f32_on_stack), i32_reinterpret_f32),
    (Some(f64_on_stack), i64_reinterpret_f64),
    (Some(i32_on_stack), f32_reinterpret_i32),
    (Some(i64_on_stack), f64_reinterpret_i64),
    (Some(i32_on_stack), i32_extend_8_s),
    (Some(i32_on_stack), i32_extend_16_s),
    (Some(i64_on_stack), i64_extend_8_s),
    (Some(i64_on_stack), i64_extend_16_s),
    (Some(i64_on_stack), i64_extend_32_s),
    (Some(f32_on_stack), i32_trunc_sat_f32_s),
    (Some(f32_on_stack), i32_trunc_sat_f32_u),
    (Some(f64_on_stack), i32_trunc_sat_f64_s),
    (Some(f64_on_stack), i32_trunc_sat_f64_u),
    (Some(f32_on_stack), i64_trunc_sat_f32_s),
    (Some(f32_on_stack), i64_trunc_sat_f32_u),
    (Some(f64_on_stack), i64_trunc_sat_f64_s),
    (Some(f64_on_stack), i64_trunc_sat_f64_u),
    // reference types proposal
    (Some(ref_null_valid), ref_null),
    (Some(ref_func_valid), ref_func),
    (Some(ref_is_null_valid), ref_is_null),
    (Some(table_fill_valid), table_fill),
    (Some(table_set_valid), table_set),
    (Some(table_get_valid), table_get),
    (Some(table_size_valid), table_size),
    (Some(table_grow_valid), table_grow),
    (Some(table_copy_valid), table_copy),
    (Some(table_init_valid), table_init),
    (Some(elem_drop_valid), elem_drop),
    // SIMD instructions.
    (Some(simd_have_memory_and_offset), v128_load),
    (Some(simd_have_memory_and_offset), v128_load8x8s),
    (Some(simd_have_memory_and_offset), v128_load8x8u),
    (Some(simd_have_memory_and_offset), v128_load16x4s),
    (Some(simd_have_memory_and_offset), v128_load16x4u),
    (Some(simd_have_memory_and_offset), v128_load32x2s),
    (Some(simd_have_memory_and_offset), v128_load32x2u),
    (Some(simd_have_memory_and_offset), v128_load8_splat),
    (Some(simd_have_memory_and_offset), v128_load16_splat),
    (Some(simd_have_memory_and_offset), v128_load32_splat),
    (Some(simd_have_memory_and_offset), v128_load64_splat),
    (Some(simd_have_memory_and_offset), v128_load32_zero),
    (Some(simd_have_memory_and_offset), v128_load64_zero),
    (Some(simd_v128_store_valid), v128_store),
    (Some(simd_have_memory_and_offset_and_v128), v128_load8_lane),
    (Some(simd_have_memory_and_offset_and_v128), v128_load16_lane),
    (Some(simd_have_memory_and_offset_and_v128), v128_load32_lane),
    (Some(simd_have_memory_and_offset_and_v128), v128_load64_lane),
    (Some(simd_v128_store_valid), v128_store8_lane),
    (Some(simd_v128_store_valid), v128_store16_lane),
    (Some(simd_v128_store_valid), v128_store32_lane),
    (Some(simd_v128_store_valid), v128_store64_lane),
    (Some(simd_enabled), v128_const),
    (Some(simd_v128_v128_on_stack), i8x16_shuffle),
    (Some(simd_v128_on_stack), i8x16_extract_lane_s),
    (Some(simd_v128_on_stack), i8x16_extract_lane_u),
    (Some(simd_v128_i32_on_stack), i8x16_replace_lane),
    (Some(simd_v128_on_stack), i16x8_extract_lane_s),
    (Some(simd_v128_on_stack), i16x8_extract_lane_u),
    (Some(simd_v128_i32_on_stack), i16x8_replace_lane),
    (Some(simd_v128_on_stack), i32x4_extract_lane),
    (Some(simd_v128_i32_on_stack), i32x4_replace_lane),
    (Some(simd_v128_on_stack), i64x2_extract_lane),
    (Some(simd_v128_i64_on_stack), i64x2_replace_lane),
    (Some(simd_v128_on_stack), f32x4_extract_lane),
    (Some(simd_v128_f32_on_stack), f32x4_replace_lane),
    (Some(simd_v128_on_stack), f64x2_extract_lane),
    (Some(simd_v128_f64_on_stack), f64x2_replace_lane),
    (Some(simd_v128_v128_on_stack), i8x16_swizzle),
    (Some(simd_i32_on_stack), i8x16_splat),
    (Some(simd_i32_on_stack), i16x8_splat),
    (Some(simd_i32_on_stack), i32x4_splat),
    (Some(simd_i64_on_stack), i64x2_splat),
    (Some(simd_f32_on_stack), f32x4_splat),
    (Some(simd_f64_on_stack), f64x2_splat),
    (Some(simd_v128_v128_on_stack), i8x16_swizzle),
    (Some(simd_v128_v128_v128_on_stack), v128_bitselect),
    (Some(simd_v128_v128_on_stack), i8x16_eq),
    (Some(simd_v128_v128_on_stack), i8x16_ne),
    (Some(simd_v128_v128_on_stack), i8x16_lt_s),
    (Some(simd_v128_v128_on_stack), i8x16_lt_u),
    (Some(simd_v128_v128_on_stack), i8x16_gt_s),
    (Some(simd_v128_v128_on_stack), i8x16_gt_u),
    (Some(simd_v128_v128_on_stack), i8x16_le_s),
    (Some(simd_v128_v128_on_stack), i8x16_le_u),
    (Some(simd_v128_v128_on_stack), i8x16_ge_s),
    (Some(simd_v128_v128_on_stack), i8x16_ge_u),
    (Some(simd_v128_v128_on_stack), i16x8_eq),
    (Some(simd_v128_v128_on_stack), i16x8_ne),
    (Some(simd_v128_v128_on_stack), i16x8_lt_s),
    (Some(simd_v128_v128_on_stack), i16x8_lt_u),
    (Some(simd_v128_v128_on_stack), i16x8_gt_s),
    (Some(simd_v128_v128_on_stack), i16x8_gt_u),
    (Some(simd_v128_v128_on_stack), i16x8_le_s),
    (Some(simd_v128_v128_on_stack), i16x8_le_u),
    (Some(simd_v128_v128_on_stack), i16x8_ge_s),
    (Some(simd_v128_v128_on_stack), i16x8_ge_u),
    (Some(simd_v128_v128_on_stack), i32x4_eq),
    (Some(simd_v128_v128_on_stack), i32x4_ne),
    (Some(simd_v128_v128_on_stack), i32x4_lt_s),
    (Some(simd_v128_v128_on_stack), i32x4_lt_u),
    (Some(simd_v128_v128_on_stack), i32x4_gt_s),
    (Some(simd_v128_v128_on_stack), i32x4_gt_u),
    (Some(simd_v128_v128_on_stack), i32x4_le_s),
    (Some(simd_v128_v128_on_stack), i32x4_le_u),
    (Some(simd_v128_v128_on_stack), i32x4_ge_s),
    (Some(simd_v128_v128_on_stack), i32x4_ge_u),
    (Some(simd_v128_v128_on_stack), i64x2_eq),
    (Some(simd_v128_v128_on_stack), i64x2_ne),
    (Some(simd_v128_v128_on_stack), i64x2_lt_s),
    (Some(simd_v128_v128_on_stack), i64x2_gt_s),
    (Some(simd_v128_v128_on_stack), i64x2_le_s),
    (Some(simd_v128_v128_on_stack), i64x2_ge_s),
    (Some(simd_v128_v128_on_stack), f32x4_eq),
    (Some(simd_v128_v128_on_stack), f32x4_ne),
    (Some(simd_v128_v128_on_stack), f32x4_lt),
    (Some(simd_v128_v128_on_stack), f32x4_gt),
    (Some(simd_v128_v128_on_stack), f32x4_le),
    (Some(simd_v128_v128_on_stack), f32x4_ge),
    (Some(simd_v128_v128_on_stack), f64x2_eq),
    (Some(simd_v128_v128_on_stack), f64x2_ne),
    (Some(simd_v128_v128_on_stack), f64x2_lt),
    (Some(simd_v128_v128_on_stack), f64x2_gt),
    (Some(simd_v128_v128_on_stack), f64x2_le),
    (Some(simd_v128_v128_on_stack), f64x2_ge),
    (Some(simd_v128_on_stack), v128_not),
    (Some(simd_v128_v128_on_stack), v128_and),
    (Some(simd_v128_v128_on_stack), v128_and_not),
    (Some(simd_v128_v128_on_stack), v128_or),
    (Some(simd_v128_v128_on_stack), v128_xor),
    (Some(simd_v128_v128_on_stack), v128_any_true),
    (Some(simd_v128_on_stack), i8x16_abs),
    (Some(simd_v128_on_stack), i8x16_neg),
    (Some(simd_v128_on_stack), i8x16_popcnt),
    (Some(simd_v128_on_stack), i8x16_all_true),
    (Some(simd_v128_on_stack), i8x16_bitmask),
    (Some(simd_v128_v128_on_stack), i8x16_narrow_i16x8s),
    (Some(simd_v128_v128_on_stack), i8x16_narrow_i16x8u),
    (Some(simd_v128_i32_on_stack), i8x16_shl),
    (Some(simd_v128_i32_on_stack), i8x16_shr_s),
    (Some(simd_v128_i32_on_stack), i8x16_shr_u),
    (Some(simd_v128_v128_on_stack), i8x16_add),
    (Some(simd_v128_v128_on_stack), i8x16_add_sat_s),
    (Some(simd_v128_v128_on_stack), i8x16_add_sat_u),
    (Some(simd_v128_v128_on_stack), i8x16_sub),
    (Some(simd_v128_v128_on_stack), i8x16_sub_sat_s),
    (Some(simd_v128_v128_on_stack), i8x16_sub_sat_u),
    (Some(simd_v128_v128_on_stack), i8x16_min_s),
    (Some(simd_v128_v128_on_stack), i8x16_min_u),
    (Some(simd_v128_v128_on_stack), i8x16_max_s),
    (Some(simd_v128_v128_on_stack), i8x16_max_u),
    (Some(simd_v128_v128_on_stack), i8x16_rounding_average_u),
    (Some(simd_v128_on_stack), i16x8_ext_add_pairwise_i8x16s),
    (Some(simd_v128_on_stack), i16x8_ext_add_pairwise_i8x16u),
    (Some(simd_v128_on_stack), i16x8_abs),
    (Some(simd_v128_on_stack), i16x8_neg),
    (Some(simd_v128_v128_on_stack), i16x8q15_mulr_sat_s),
    (Some(simd_v128_on_stack), i16x8_all_true),
    (Some(simd_v128_on_stack), i16x8_bitmask),
    (Some(simd_v128_v128_on_stack), i16x8_narrow_i32x4s),
    (Some(simd_v128_v128_on_stack), i16x8_narrow_i32x4u),
    (Some(simd_v128_on_stack), i16x8_extend_low_i8x16s),
    (Some(simd_v128_on_stack), i16x8_extend_high_i8x16s),
    (Some(simd_v128_on_stack), i16x8_extend_low_i8x16u),
    (Some(simd_v128_on_stack), i16x8_extend_high_i8x16u),
    (Some(simd_v128_i32_on_stack), i16x8_shl),
    (Some(simd_v128_i32_on_stack), i16x8_shr_s),
    (Some(simd_v128_i32_on_stack), i16x8_shr_u),
    (Some(simd_v128_v128_on_stack), i16x8_add),
    (Some(simd_v128_v128_on_stack), i16x8_add_sat_s),
    (Some(simd_v128_v128_on_stack), i16x8_add_sat_u),
    (Some(simd_v128_v128_on_stack), i16x8_sub),
    (Some(simd_v128_v128_on_stack), i16x8_sub_sat_s),
    (Some(simd_v128_v128_on_stack), i16x8_sub_sat_u),
    (Some(simd_v128_v128_on_stack), i16x8_mul),
    (Some(simd_v128_v128_on_stack), i16x8_min_s),
    (Some(simd_v128_v128_on_stack), i16x8_min_u),
    (Some(simd_v128_v128_on_stack), i16x8_max_s),
    (Some(simd_v128_v128_on_stack), i16x8_max_u),
    (Some(simd_v128_v128_on_stack), i16x8_rounding_average_u),
    (Some(simd_v128_v128_on_stack), i16x8_ext_mul_low_i8x16s),
    (Some(simd_v128_v128_on_stack), i16x8_ext_mul_high_i8x16s),
    (Some(simd_v128_v128_on_stack), i16x8_ext_mul_low_i8x16u),
    (Some(simd_v128_v128_on_stack), i16x8_ext_mul_high_i8x16u),
    (Some(simd_v128_on_stack), i32x4_ext_add_pairwise_i16x8s),
    (Some(simd_v128_on_stack), i32x4_ext_add_pairwise_i16x8u),
    (Some(simd_v128_on_stack), i32x4_abs),
    (Some(simd_v128_on_stack), i32x4_neg),
    (Some(simd_v128_on_stack), i32x4_all_true),
    (Some(simd_v128_on_stack), i32x4_bitmask),
    (Some(simd_v128_on_stack), i32x4_extend_low_i16x8s),
    (Some(simd_v128_on_stack), i32x4_extend_high_i16x8s),
    (Some(simd_v128_on_stack), i32x4_extend_low_i16x8u),
    (Some(simd_v128_on_stack), i32x4_extend_high_i16x8u),
    (Some(simd_v128_i32_on_stack), i32x4_shl),
    (Some(simd_v128_i32_on_stack), i32x4_shr_s),
    (Some(simd_v128_i32_on_stack), i32x4_shr_u),
    (Some(simd_v128_v128_on_stack), i32x4_add),
    (Some(simd_v128_v128_on_stack), i32x4_sub),
    (Some(simd_v128_v128_on_stack), i32x4_mul),
    (Some(simd_v128_v128_on_stack), i32x4_min_s),
    (Some(simd_v128_v128_on_stack), i32x4_min_u),
    (Some(simd_v128_v128_on_stack), i32x4_max_s),
    (Some(simd_v128_v128_on_stack), i32x4_max_u),
    (Some(simd_v128_v128_on_stack), i32x4_dot_i16x8s),
    (Some(simd_v128_v128_on_stack), i32x4_ext_mul_low_i16x8s),
    (Some(simd_v128_v128_on_stack), i32x4_ext_mul_high_i16x8s),
    (Some(simd_v128_v128_on_stack), i32x4_ext_mul_low_i16x8u),
    (Some(simd_v128_v128_on_stack), i32x4_ext_mul_high_i16x8u),
    (Some(simd_v128_on_stack), i64x2_abs),
    (Some(simd_v128_on_stack), i64x2_neg),
    (Some(simd_v128_on_stack), i64x2_all_true),
    (Some(simd_v128_on_stack), i64x2_bitmask),
    (Some(simd_v128_on_stack), i64x2_extend_low_i32x4s),
    (Some(simd_v128_on_stack), i64x2_extend_high_i32x4s),
    (Some(simd_v128_on_stack), i64x2_extend_low_i32x4u),
    (Some(simd_v128_on_stack), i64x2_extend_high_i32x4u),
    (Some(simd_v128_i32_on_stack), i64x2_shl),
    (Some(simd_v128_i32_on_stack), i64x2_shr_s),
    (Some(simd_v128_i32_on_stack), i64x2_shr_u),
    (Some(simd_v128_v128_on_stack), i64x2_add),
    (Some(simd_v128_v128_on_stack), i64x2_sub),
    (Some(simd_v128_v128_on_stack), i64x2_mul),
    (Some(simd_v128_v128_on_stack), i64x2_ext_mul_low_i32x4s),
    (Some(simd_v128_v128_on_stack), i64x2_ext_mul_high_i32x4s),
    (Some(simd_v128_v128_on_stack), i64x2_ext_mul_low_i32x4u),
    (Some(simd_v128_v128_on_stack), i64x2_ext_mul_high_i32x4u),
    (Some(simd_v128_on_stack), f32x4_ceil),
    (Some(simd_v128_on_stack), f32x4_floor),
    (Some(simd_v128_on_stack), f32x4_trunc),
    (Some(simd_v128_on_stack), f32x4_nearest),
    (Some(simd_v128_on_stack), f32x4_abs),
    (Some(simd_v128_on_stack), f32x4_neg),
    (Some(simd_v128_on_stack), f32x4_sqrt),
    (Some(simd_v128_v128_on_stack), f32x4_add),
    (Some(simd_v128_v128_on_stack), f32x4_sub),
    (Some(simd_v128_v128_on_stack), f32x4_mul),
    (Some(simd_v128_v128_on_stack), f32x4_div),
    (Some(simd_v128_v128_on_stack), f32x4_min),
    (Some(simd_v128_v128_on_stack), f32x4_max),
    (Some(simd_v128_v128_on_stack), f32x4p_min),
    (Some(simd_v128_v128_on_stack), f32x4p_max),
    (Some(simd_v128_on_stack), f64x2_ceil),
    (Some(simd_v128_on_stack), f64x2_floor),
    (Some(simd_v128_on_stack), f64x2_trunc),
    (Some(simd_v128_on_stack), f64x2_nearest),
    (Some(simd_v128_on_stack), f64x2_abs),
    (Some(simd_v128_on_stack), f64x2_neg),
    (Some(simd_v128_on_stack), f64x2_sqrt),
    (Some(simd_v128_v128_on_stack), f64x2_add),
    (Some(simd_v128_v128_on_stack), f64x2_sub),
    (Some(simd_v128_v128_on_stack), f64x2_mul),
    (Some(simd_v128_v128_on_stack), f64x2_div),
    (Some(simd_v128_v128_on_stack), f64x2_min),
    (Some(simd_v128_v128_on_stack), f64x2_max),
    (Some(simd_v128_v128_on_stack), f64x2p_min),
    (Some(simd_v128_v128_on_stack), f64x2p_max),
    (Some(simd_v128_on_stack), i32x4_trunc_sat_f32x4s),
    (Some(simd_v128_on_stack), i32x4_trunc_sat_f32x4u),
    (Some(simd_v128_on_stack), f32x4_convert_i32x4s),
    (Some(simd_v128_on_stack), f32x4_convert_i32x4u),
    (Some(simd_v128_on_stack), i32x4_trunc_sat_f64x2s_zero),
    (Some(simd_v128_on_stack), i32x4_trunc_sat_f64x2u_zero),
    (Some(simd_v128_on_stack), f64x2_convert_low_i32x4s),
    (Some(simd_v128_on_stack), f64x2_convert_low_i32x4u),
    (Some(simd_v128_on_stack), f32x4_demote_f64x2_zero),
    (Some(simd_v128_on_stack), f64x2_promote_low_f32x4),
}

pub(crate) struct CodeBuilderAllocations<C>
where
    C: Config,
{
    // The control labels in scope right now.
    controls: Vec<Control>,

    // The types on the operand stack right now.
    operands: Vec<Option<ValType>>,

    // Dynamic set of options of instruction we can generate that are known to
    // be valid right now.
    options: Vec<(
        fn(&mut Unstructured, &ConfiguredModule<C>, &mut CodeBuilder<C>) -> Result<Instruction>,
        u32,
    )>,

    // Cached information about the module that we're generating functions for,
    // used to speed up validity checks. The mutable globals map is a map of the
    // type of global to the global indices which have that type (and they're
    // all mutable).
    mutable_globals: BTreeMap<ValType, Vec<u32>>,

    // Like mutable globals above this is a map from function types to the list
    // of functions that have that function type.
    functions: BTreeMap<Vec<ValType>, Vec<u32>>,

    // Tables in this module which have a funcref element type.
    funcref_tables: Vec<u32>,

    // Functions that are referenced in the module through globals and segments.
    referenced_functions: Vec<u32>,

    // Flag that indicates if any element segments have the same type as any
    // table
    table_init_possible: bool,
}

pub(crate) struct CodeBuilder<'a, C>
where
    C: Config,
{
    func_ty: &'a FuncType,
    locals: &'a Vec<ValType>,
    allocs: &'a mut CodeBuilderAllocations<C>,
}

/// A control frame.
#[derive(Debug, Clone)]
struct Control {
    kind: ControlKind,
    /// Value types that must be on the stack when entering this control frame.
    params: Vec<ValType>,
    /// Value types that are left on the stack when exiting this control frame.
    results: Vec<ValType>,
    /// How far down the operand stack instructions inside this control frame
    /// can reach.
    height: usize,
}

impl Control {
    fn label_types(&self) -> &[ValType] {
        if self.kind == ControlKind::Loop {
            &self.params
        } else {
            &self.results
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ControlKind {
    Block,
    If,
    Loop,
}

impl<C> CodeBuilderAllocations<C>
where
    C: Config,
{
    pub(crate) fn new(module: &ConfiguredModule<C>) -> Self {
        let mut mutable_globals = BTreeMap::new();
        for (i, global) in module.globals.iter().enumerate() {
            if global.mutable {
                mutable_globals
                    .entry(global.val_type)
                    .or_insert(Vec::new())
                    .push(i as u32);
            }
        }

        let mut functions = BTreeMap::new();
        for (idx, func) in module.funcs() {
            functions
                .entry(func.params.clone())
                .or_insert(Vec::new())
                .push(idx);
        }

        let mut funcref_tables = Vec::new();
        let mut table_tys = Vec::new();
        for (i, table) in module.tables.iter().enumerate() {
            table_tys.push(table.elem_ty);
            if table.elem_ty == ValType::FuncRef {
                funcref_tables.push(i as u32);
            }
        }

        let mut referenced_functions = BTreeSet::new();
        for (_, expr) in module.defined_globals.iter() {
            if let Instruction::RefFunc(i) = *expr {
                referenced_functions.insert(i);
            }
        }
        for g in module.elems.iter() {
            match &g.items {
                Elements::Expressions(e) => {
                    let iter = e.iter().filter_map(|i| *i);
                    referenced_functions.extend(iter);
                }
                Elements::Functions(e) => {
                    referenced_functions.extend(e.iter().cloned());
                }
            }
        }

        let table_init_possible = module.elems.iter().any(|e| table_tys.contains(&e.ty));

        CodeBuilderAllocations {
            controls: Vec::with_capacity(4),
            operands: Vec::with_capacity(16),
            options: Vec::with_capacity(NUM_OPTIONS),
            functions,
            mutable_globals,
            funcref_tables,
            referenced_functions: referenced_functions.into_iter().collect(),
            table_init_possible,
        }
    }

    pub(crate) fn builder<'a>(
        &'a mut self,
        func_ty: &'a FuncType,
        locals: &'a Vec<ValType>,
    ) -> CodeBuilder<'a, C> {
        self.controls.clear();
        self.controls.push(Control {
            kind: ControlKind::Block,
            params: vec![],
            results: func_ty.results.clone(),
            height: 0,
        });

        self.operands.clear();
        self.options.clear();

        CodeBuilder {
            func_ty,
            locals,
            allocs: self,
        }
    }
}

impl<'a, C> CodeBuilder<'a, C>
where
    C: Config,
{
    /// Get the operands that are in-scope within the current control frame.
    fn operands(&self) -> &[Option<ValType>] {
        let height = self.allocs.controls.last().map_or(0, |c| c.height);
        &self.allocs.operands[height..]
    }

    fn pop_operands(&mut self, to_pop: &[ValType]) {
        debug_assert!(self.types_on_stack(to_pop));
        self.allocs
            .operands
            .truncate(self.allocs.operands.len() - to_pop.len());
    }

    fn push_operands(&mut self, to_push: &[ValType]) {
        self.allocs
            .operands
            .extend(to_push.iter().copied().map(Some));
    }

    fn label_types_on_stack(&self, to_check: &Control) -> bool {
        self.types_on_stack(to_check.label_types())
    }

    fn type_on_stack(&self, ty: ValType) -> bool {
        match self.operands().last() {
            None => false,
            Some(None) => true,
            Some(Some(x)) => *x == ty,
        }
    }

    fn types_on_stack(&self, types: &[ValType]) -> bool {
        self.operands().len() >= types.len()
            && self
                .operands()
                .iter()
                .rev()
                .zip(types.iter().rev())
                .all(|(a, b)| match (a, b) {
                    (None, _) => true,
                    (Some(x), y) => x == y,
                })
    }

    #[inline(never)]
    fn arbitrary_block_type(
        &self,
        u: &mut Unstructured,
        module: &ConfiguredModule<C>,
    ) -> Result<BlockType> {
        let mut options: Vec<Box<dyn Fn(&mut Unstructured) -> Result<BlockType>>> = vec![
            Box::new(|_| Ok(BlockType::Empty)),
            Box::new(|u| Ok(BlockType::Result(module.arbitrary_valtype(u)?))),
        ];

        for (i, ty) in module.func_types() {
            if self.types_on_stack(&ty.params) {
                options.push(Box::new(move |_| Ok(BlockType::FuncType(i as u32))));
            }
        }

        let f = u.choose(&options)?;
        f(u)
    }

    pub(crate) fn arbitrary(
        mut self,
        u: &mut Unstructured,
        module: &ConfiguredModule<C>,
    ) -> Result<Vec<Instruction>> {
        let max_instructions = module.config.max_instructions();
        let mut instructions = vec![];

        while !self.allocs.controls.is_empty() {
            let keep_going = instructions.len() < max_instructions
                && u.arbitrary().map_or(false, |b: u8| b != 0);
            if !keep_going {
                self.end_active_control_frames(u, &mut instructions);
                break;
            }

            match choose_instruction(u, module, &mut self) {
                Some(f) => {
                    let inst = f(u, module, &mut self)?;
                    instructions.push(inst);
                }
                // Choosing an instruction can fail because there is not enough
                // underlying data, so we really cannot generate any more
                // instructions. In this case we swallow that error and instead
                // just terminate our wasm function's frames.
                None => {
                    self.end_active_control_frames(u, &mut instructions);
                    break;
                }
            }
        }

        Ok(instructions)
    }

    fn end_active_control_frames(
        &mut self,
        u: &mut Unstructured<'_>,
        instructions: &mut Vec<Instruction>,
    ) {
        while !self.allocs.controls.is_empty() {
            // Ensure that this label is valid by placing the right types onto
            // the operand stack for the end of the label.
            self.guarantee_label_results(u, instructions);

            // Remove the label and clear the operand stack since the label has
            // been removed.
            let label = self.allocs.controls.pop().unwrap();
            self.allocs.operands.truncate(label.height);

            // If this is an `if` that is not stack neutral, then it
            // must have an `else`. Generate synthetic results here in the same
            // manner we did above.
            if label.kind == ControlKind::If && label.params != label.results {
                instructions.push(Instruction::Else);
                self.allocs.controls.push(label.clone());
                self.allocs
                    .operands
                    .extend(label.params.into_iter().map(Some));
                self.guarantee_label_results(u, instructions);
                self.allocs.controls.pop();
                self.allocs.operands.truncate(label.height);
            }

            // The last control frame for the function return does not
            // need an `end` instruction.
            if !self.allocs.controls.is_empty() {
                instructions.push(Instruction::End);
            }

            // Place the results of the label onto the operand stack for use
            // after the label.
            self.allocs
                .operands
                .extend(label.results.into_iter().map(Some));
        }
    }

    /// Modifies the instruction stream to guarantee that the current control
    /// label's results are on the stack and ready for the control label to return.
    fn guarantee_label_results(
        &mut self,
        u: &mut Unstructured<'_>,
        instructions: &mut Vec<Instruction>,
    ) {
        let mut operands = self.operands();
        let label = self.allocs.controls.last().unwrap();

        // Already done, yay!
        if label.results.len() == operands.len() && self.types_on_stack(&label.results) {
            return;
        }

        // Generating an unreachable instruction is always a valid way to
        // generate any types for a label, but it's not too interesting, so
        // don't favor it.
        if u.arbitrary::<u16>().unwrap_or(0) == 1 {
            instructions.push(Instruction::Unreachable);
            return;
        }

        // Arbitrarily massage the stack to get the expected results. First we
        // drop all extraneous results to we're only dealing with those we want
        // to deal with. Afterwards we start at the bottom of the stack and move
        // up, figuring out what matches and what doesn't. As soon as something
        // doesn't match we throw out that and everything else remaining,
        // filling in results with dummy values.
        while operands.len() > label.results.len() {
            instructions.push(Instruction::Drop);
            operands = &operands[..operands.len() - 1];
        }
        for (i, expected) in label.results.iter().enumerate() {
            if let Some(actual) = operands.get(i) {
                if Some(*expected) == *actual {
                    continue;
                }
                for _ in operands[i..].iter() {
                    instructions.push(Instruction::Drop);
                }
                operands = &[];
            }
            instructions.push(arbitrary_val(*expected, u));
        }
    }
}

fn arbitrary_val(ty: ValType, u: &mut Unstructured<'_>) -> Instruction {
    match ty {
        ValType::I32 => Instruction::I32Const(u.arbitrary().unwrap_or(0)),
        ValType::I64 => Instruction::I64Const(u.arbitrary().unwrap_or(0)),
        ValType::F32 => Instruction::F32Const(u.arbitrary().unwrap_or(0.0)),
        ValType::F64 => Instruction::F64Const(u.arbitrary().unwrap_or(0.0)),
        ValType::V128 => Instruction::V128Const(u.arbitrary().unwrap_or(0)),
        ValType::ExternRef => Instruction::RefNull(ValType::ExternRef),
        ValType::FuncRef => Instruction::RefNull(ValType::FuncRef),
    }
}

fn unreachable<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    _: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    Ok(Instruction::Unreachable)
}

fn nop<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    _: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    Ok(Instruction::Nop)
}

fn block<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = block_ty.params_results(module);
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::Block,
        params,
        results,
        height,
    });
    Ok(Instruction::Block(block_ty))
}

fn r#loop<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = block_ty.params_results(module);
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::Loop,
        params,
        results,
        height,
    });
    Ok(Instruction::Loop(block_ty))
}

#[inline]
fn if_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.type_on_stack(ValType::I32)
}

fn r#if<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = block_ty.params_results(module);
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::If,
        params,
        results,
        height,
    });
    Ok(Instruction::If(block_ty))
}

#[inline]
fn else_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    let last_control = builder.allocs.controls.last().unwrap();
    last_control.kind == ControlKind::If
        && builder.operands().len() == last_control.results.len()
        && builder.types_on_stack(&last_control.results)
}

fn r#else<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let control = builder.allocs.controls.pop().unwrap();
    builder.pop_operands(&control.results);
    builder.push_operands(&control.params);
    builder.allocs.controls.push(Control {
        kind: ControlKind::Block,
        ..control
    });
    Ok(Instruction::Else)
}

#[inline]
fn end_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    // Note: first control frame is the function return's control frame, which
    // does not have an associated `end`.
    if builder.allocs.controls.len() <= 1 {
        return false;
    }
    let control = builder.allocs.controls.last().unwrap();
    builder.operands().len() == control.results.len()
        && builder.types_on_stack(&control.results)
        // `if`s that don't leave the stack as they found it must have an
        // `else`.
        && !(control.kind == ControlKind::If && control.params != control.results)
}

fn end<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.allocs.controls.pop();
    Ok(Instruction::End)
}

#[inline]
fn br_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder
        .allocs
        .controls
        .iter()
        .any(|l| builder.label_types_on_stack(l))
}

fn br<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let n = builder
        .allocs
        .controls
        .iter()
        .filter(|l| builder.label_types_on_stack(l))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (target, _) = builder
        .allocs
        .controls
        .iter()
        .rev()
        .enumerate()
        .filter(|(_, l)| builder.label_types_on_stack(l))
        .nth(i)
        .unwrap();
    let control = &builder.allocs.controls[builder.allocs.controls.len() - 1 - target];
    let tys = control.label_types().to_vec();
    builder.pop_operands(&tys);
    Ok(Instruction::Br(target as u32))
}

#[inline]
fn br_if_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    if !builder.type_on_stack(ValType::I32) {
        return false;
    }
    let ty = builder.allocs.operands.pop().unwrap();
    let is_valid = builder
        .allocs
        .controls
        .iter()
        .any(|l| builder.label_types_on_stack(l));
    builder.allocs.operands.push(ty);
    is_valid
}

fn br_if<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let n = builder
        .allocs
        .controls
        .iter()
        .filter(|l| builder.label_types_on_stack(l))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (target, _) = builder
        .allocs
        .controls
        .iter()
        .rev()
        .enumerate()
        .filter(|(_, l)| builder.label_types_on_stack(l))
        .nth(i)
        .unwrap();
    Ok(Instruction::BrIf(target as u32))
}

#[inline]
fn br_table_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    if !builder.type_on_stack(ValType::I32) {
        return false;
    }
    let ty = builder.allocs.operands.pop().unwrap();
    let is_valid = br_valid(module, builder);
    builder.allocs.operands.push(ty);
    is_valid
}

fn br_table<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let n = builder
        .allocs
        .controls
        .iter()
        .filter(|l| builder.label_types_on_stack(l))
        .count();
    debug_assert!(n > 0);

    let i = u.int_in_range(0..=n - 1)?;
    let (default_target, _) = builder
        .allocs
        .controls
        .iter()
        .rev()
        .enumerate()
        .filter(|(_, l)| builder.label_types_on_stack(l))
        .nth(i)
        .unwrap();
    let control = &builder.allocs.controls[builder.allocs.controls.len() - 1 - default_target];

    let targets = builder
        .allocs
        .controls
        .iter()
        .rev()
        .enumerate()
        .filter(|(_, l)| l.label_types() == control.label_types())
        .map(|(t, _)| t as u32)
        .collect();

    let tys = control.label_types().to_vec();
    builder.pop_operands(&tys);

    Ok(Instruction::BrTable(targets, default_target as u32))
}

#[inline]
fn return_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.label_types_on_stack(&builder.allocs.controls[0])
}

fn r#return<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let results = builder.allocs.controls[0].results.clone();
    builder.pop_operands(&results);
    Ok(Instruction::Return)
}

#[inline]
fn call_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder
        .allocs
        .functions
        .keys()
        .any(|k| builder.types_on_stack(k))
}

fn call<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let candidates = builder
        .allocs
        .functions
        .iter()
        .filter(|(k, _)| builder.types_on_stack(k))
        .flat_map(|(_, v)| v.iter().copied())
        .collect::<Vec<_>>();
    assert!(candidates.len() > 0);
    let i = u.int_in_range(0..=candidates.len() - 1)?;
    let (func_idx, ty) = module.funcs().nth(candidates[i] as usize).unwrap();
    builder.pop_operands(&ty.params);
    builder.push_operands(&ty.results);
    Ok(Instruction::Call(func_idx as u32))
}

#[inline]
fn call_indirect_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    if builder.allocs.funcref_tables.is_empty() || !builder.type_on_stack(ValType::I32) {
        return false;
    }
    let ty = builder.allocs.operands.pop().unwrap();
    let is_valid = module
        .func_types()
        .any(|(_, ty)| builder.types_on_stack(&ty.params));
    builder.allocs.operands.push(ty);
    is_valid
}

fn call_indirect<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let choices = module
        .func_types()
        .filter(|(_, ty)| builder.types_on_stack(&ty.params))
        .collect::<Vec<_>>();
    let (type_idx, ty) = u.choose(&choices)?;
    builder.pop_operands(&ty.params);
    builder.push_operands(&ty.results);
    let table = *u.choose(&builder.allocs.funcref_tables)?;
    Ok(Instruction::CallIndirect {
        ty: *type_idx as u32,
        table,
    })
}

#[inline]
fn drop_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    !builder.operands().is_empty()
}

fn drop<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.allocs.operands.pop();
    Ok(Instruction::Drop)
}

#[inline]
fn select_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    if !(builder.operands().len() >= 3 && builder.type_on_stack(ValType::I32)) {
        return false;
    }
    let t = builder.operands()[builder.operands().len() - 2];
    let u = builder.operands()[builder.operands().len() - 3];
    t.is_none() || u.is_none() || t == u
}

fn select<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.allocs.operands.pop();
    let t = builder.allocs.operands.pop().unwrap();
    let u = builder.allocs.operands.pop().unwrap();
    let ty = t.or(u);
    builder.allocs.operands.push(ty);
    match ty {
        Some(ty @ ValType::ExternRef) | Some(ty @ ValType::FuncRef) => {
            Ok(Instruction::TypedSelect(ty))
        }
        Some(ValType::I32) | Some(ValType::I64) | Some(ValType::F32) | Some(ValType::F64)
        | Some(ValType::V128) | None => Ok(Instruction::Select),
    }
}

#[inline]
fn local_get_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    !builder.func_ty.params.is_empty() || !builder.locals.is_empty()
}

fn local_get<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let num_params = builder.func_ty.params.len();
    let n = num_params + builder.locals.len();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    builder.allocs.operands.push(Some(if i < num_params {
        builder.func_ty.params[i]
    } else {
        builder.locals[i - num_params]
    }));
    Ok(Instruction::LocalGet(i as u32))
}

#[inline]
fn local_set_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .any(|ty| builder.type_on_stack(*ty))
}

fn local_set<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let n = builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .filter(|ty| builder.type_on_stack(**ty))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (j, _) = builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .enumerate()
        .filter(|(_, ty)| builder.type_on_stack(**ty))
        .nth(i)
        .unwrap();
    builder.allocs.operands.pop();
    Ok(Instruction::LocalSet(j as u32))
}

fn local_tee<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let n = builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .filter(|ty| builder.type_on_stack(**ty))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (j, _) = builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .enumerate()
        .filter(|(_, ty)| builder.type_on_stack(**ty))
        .nth(i)
        .unwrap();
    Ok(Instruction::LocalTee(j as u32))
}

#[inline]
fn global_get_valid<C: Config>(module: &ConfiguredModule<C>, _: &mut CodeBuilder<C>) -> bool {
    module.globals.len() > 0
}

fn global_get<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    debug_assert!(module.globals.len() > 0);
    let global_idx = u.int_in_range(0..=module.globals.len() - 1)?;
    builder
        .allocs
        .operands
        .push(Some(module.globals[global_idx].val_type));
    Ok(Instruction::GlobalGet(global_idx as u32))
}

#[inline]
fn global_set_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder
        .allocs
        .mutable_globals
        .iter()
        .any(|(ty, _)| builder.type_on_stack(*ty))
}

fn global_set<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let candidates = builder
        .allocs
        .mutable_globals
        .iter()
        .find(|(ty, _)| builder.type_on_stack(**ty))
        .unwrap()
        .1;
    let i = u.int_in_range(0..=candidates.len() - 1)?;
    builder.allocs.operands.pop();
    Ok(Instruction::GlobalSet(candidates[i]))
}

#[inline]
fn have_memory<C: Config>(module: &ConfiguredModule<C>, _: &mut CodeBuilder<C>) -> bool {
    module.memories.len() > 0
}

#[inline]
fn have_memory_and_offset<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    have_memory(module, builder) && builder.type_on_stack(ValType::I32)
}

#[inline]
fn have_data<C: Config>(module: &ConfiguredModule<C>, _: &mut CodeBuilder<C>) -> bool {
    module.data.len() > 0
}

fn i32_load<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load(mem_arg(u, module, &[0, 1])?))
}

fn i64_load<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load(mem_arg(u, module, &[0, 1, 2])?))
}

fn f32_load<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::F32));
    Ok(Instruction::F32Load(mem_arg(u, module, &[0, 1])?))
}

fn f64_load<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::F64));
    Ok(Instruction::F64Load(mem_arg(u, module, &[0, 1, 2])?))
}

fn i32_load_8_s<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load8_S(mem_arg(u, module, &[0])?))
}

fn i32_load_8_u<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load8_U(mem_arg(u, module, &[0])?))
}

fn i32_load_16_s<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load16_S(mem_arg(u, module, &[0, 1])?))
}

fn i32_load_16_u<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load16_U(mem_arg(u, module, &[0, 1])?))
}

fn i64_load_8_s<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load8_S(mem_arg(u, module, &[0])?))
}

fn i64_load_16_s<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load16_S(mem_arg(u, module, &[0, 1])?))
}

fn i64_load_32_s<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load32_S(mem_arg(u, module, &[0, 1, 2])?))
}

fn i64_load_8_u<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load8_U(mem_arg(u, module, &[0])?))
}

fn i64_load_16_u<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load16_U(mem_arg(u, module, &[0, 1])?))
}

fn i64_load_32_u<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load32_U(mem_arg(u, module, &[0, 1, 2])?))
}

#[inline]
fn store_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
    f: impl FnOnce() -> ValType,
) -> bool {
    have_memory(module, builder) && builder.types_on_stack(&[ValType::I32, f()])
}

#[inline]
fn i32_store_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    store_valid(module, builder, || ValType::I32)
}

fn i32_store<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store(mem_arg(u, module, &[0, 1])?))
}

#[inline]
fn i64_store_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    store_valid(module, builder, || ValType::I64)
}

fn i64_store<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store(mem_arg(u, module, &[0, 1, 2])?))
}

#[inline]
fn f32_store_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    store_valid(module, builder, || ValType::F32)
}

fn f32_store<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::F32]);
    Ok(Instruction::F32Store(mem_arg(u, module, &[0, 1])?))
}

#[inline]
fn f64_store_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    store_valid(module, builder, || ValType::F64)
}

fn f64_store<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::F64]);
    Ok(Instruction::F64Store(mem_arg(u, module, &[0, 1, 2])?))
}

fn i32_store_8<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store8(mem_arg(u, module, &[0])?))
}

fn i32_store_16<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store16(mem_arg(u, module, &[0, 1])?))
}

fn i64_store_8<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store8(mem_arg(u, module, &[0])?))
}

fn i64_store_16<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store16(mem_arg(u, module, &[0, 1])?))
}

fn i64_store_32<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store32(mem_arg(u, module, &[0, 1, 2])?))
}

fn memory_size<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::MemorySize(memory_index(u, module)?))
}

#[inline]
fn memory_grow_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    have_memory(module, builder) && builder.type_on_stack(ValType::I32)
}

fn memory_grow<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::MemoryGrow(memory_index(u, module)?))
}

#[inline]
fn memory_init_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    have_memory(module, builder)
        && have_data(module, builder)
        && module.config.bulk_memory_enabled()
        && builder.types_on_stack(&[ValType::I32, ValType::I32, ValType::I32])
}

fn memory_init<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let mem = memory_index(u, module)?;
    let data = data_index(u, module)?;
    builder.pop_operands(&[ValType::I32, ValType::I32, ValType::I32]);
    Ok(Instruction::MemoryInit { mem, data })
}

#[inline]
fn memory_fill_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    have_memory(module, builder)
        && module.config.bulk_memory_enabled()
        && builder.types_on_stack(&[ValType::I32, ValType::I32, ValType::I32])
}

fn memory_fill<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let mem = memory_index(u, module)?;
    builder.pop_operands(&[ValType::I32, ValType::I32, ValType::I32]);
    Ok(Instruction::MemoryFill(mem))
}

#[inline]
fn memory_copy_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    memory_fill_valid(module, builder)
}

fn memory_copy<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let src = memory_index(u, module)?;
    let dst = memory_index(u, module)?;
    builder.pop_operands(&[ValType::I32, ValType::I32, ValType::I32]);
    Ok(Instruction::MemoryCopy { dst, src })
}

#[inline]
fn data_drop_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    have_data(module, builder) && module.config.bulk_memory_enabled()
}

fn data_drop<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    _builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    Ok(Instruction::DataDrop(data_index(u, module)?))
}

fn i32_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Const(x))
}

fn i64_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Const(x))
}

fn f32_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Const(x))
}

fn f64_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Const(x))
}

#[inline]
fn i32_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.type_on_stack(ValType::I32)
}

fn i32_eqz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Eqz)
}

#[inline]
fn i32_i32_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::I32, ValType::I32])
}

fn i32_eq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Eq)
}

fn i32_neq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Neq)
}

fn i32_lt_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LtS)
}

fn i32_lt_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LtU)
}

fn i32_gt_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GtS)
}

fn i32_gt_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GtU)
}

fn i32_le_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LeS)
}

fn i32_le_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LeU)
}

fn i32_ge_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GeS)
}

fn i32_ge_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GeU)
}

#[inline]
fn i64_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::I64])
}

fn i64_eqz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Eqz)
}

#[inline]
fn i64_i64_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::I64, ValType::I64])
}

fn i64_eq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Eq)
}

fn i64_neq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Neq)
}

fn i64_lt_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LtS)
}

fn i64_lt_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LtU)
}

fn i64_gt_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GtS)
}

fn i64_gt_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GtU)
}

fn i64_le_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LeS)
}

fn i64_le_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LeU)
}

fn i64_ge_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GeS)
}

fn i64_ge_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GeU)
}

fn f32_f32_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::F32, ValType::F32])
}

fn f32_eq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Eq)
}

fn f32_neq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Neq)
}

fn f32_lt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Lt)
}

fn f32_gt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Gt)
}

fn f32_le<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Le)
}

fn f32_ge<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Ge)
}

fn f64_f64_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::F64, ValType::F64])
}

fn f64_eq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Eq)
}

fn f64_neq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Neq)
}

fn f64_lt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Lt)
}

fn f64_gt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Gt)
}

fn f64_le<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Le)
}

fn f64_ge<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Ge)
}

fn i32_clz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Clz)
}

fn i32_ctz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Ctz)
}

fn i32_popcnt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Popcnt)
}

fn i32_add<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Add)
}

fn i32_sub<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Sub)
}

fn i32_mul<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Mul)
}

fn i32_div_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32DivS)
}

fn i32_div_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32DivU)
}

fn i32_rem_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32RemS)
}

fn i32_rem_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32RemU)
}

fn i32_and<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32And)
}

fn i32_or<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Or)
}

fn i32_xor<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Xor)
}

fn i32_shl<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Shl)
}

fn i32_shr_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ShrS)
}

fn i32_shr_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ShrU)
}

fn i32_rotl<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Rotl)
}

fn i32_rotr<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Rotr)
}

fn i64_clz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Clz)
}

fn i64_ctz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Ctz)
}

fn i64_popcnt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Popcnt)
}

fn i64_add<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Add)
}

fn i64_sub<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Sub)
}

fn i64_mul<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Mul)
}

fn i64_div_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64DivS)
}

fn i64_div_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64DivU)
}

fn i64_rem_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64RemS)
}

fn i64_rem_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64RemU)
}

fn i64_and<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64And)
}

fn i64_or<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Or)
}

fn i64_xor<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Xor)
}

fn i64_shl<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Shl)
}

fn i64_shr_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ShrS)
}

fn i64_shr_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ShrU)
}

fn i64_rotl<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Rotl)
}

fn i64_rotr<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Rotr)
}

#[inline]
fn f32_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::F32])
}

fn f32_abs<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Abs)
}

fn f32_neg<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Neg)
}

fn f32_ceil<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Ceil)
}

fn f32_floor<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Floor)
}

fn f32_trunc<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Trunc)
}

fn f32_nearest<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Nearest)
}

fn f32_sqrt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Sqrt)
}

fn f32_add<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Add)
}

fn f32_sub<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Sub)
}

fn f32_mul<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Mul)
}

fn f32_div<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Div)
}

fn f32_min<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Min)
}

fn f32_max<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Max)
}

fn f32_copysign<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Copysign)
}

#[inline]
fn f64_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::F64])
}

fn f64_abs<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Abs)
}

fn f64_neg<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Neg)
}

fn f64_ceil<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Ceil)
}

fn f64_floor<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Floor)
}

fn f64_trunc<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Trunc)
}

fn f64_nearest<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Nearest)
}

fn f64_sqrt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Sqrt)
}

fn f64_add<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Add)
}

fn f64_sub<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Sub)
}

fn f64_mul<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Mul)
}

fn f64_div<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Div)
}

fn f64_min<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Min)
}

fn f64_max<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Max)
}

fn f64_copysign<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Copysign)
}

fn i32_wrap_i64<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32WrapI64)
}

fn i32_trunc_f32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF32S)
}

fn i32_trunc_f32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF32U)
}

fn i32_trunc_f64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF64S)
}

fn i32_trunc_f64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF64U)
}

fn i64_extend_i32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ExtendI32S)
}

fn i64_extend_i32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ExtendI32U)
}

fn i64_trunc_f32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF32S)
}

fn i64_trunc_f32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF32U)
}

fn i64_trunc_f64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF64S)
}

fn i64_trunc_f64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF64U)
}

fn f32_convert_i32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI32S)
}

fn f32_convert_i32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI32U)
}

fn f32_convert_i64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI64S)
}

fn f32_convert_i64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI64U)
}

fn f32_demote_f64<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32DemoteF64)
}

fn f64_convert_i32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI32S)
}

fn f64_convert_i32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI32U)
}

fn f64_convert_i64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI64S)
}

fn f64_convert_i64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI64U)
}

fn f64_promote_f32<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64PromoteF32)
}

fn i32_reinterpret_f32<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ReinterpretF32)
}

fn i64_reinterpret_f64<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ReinterpretF64)
}

fn f32_reinterpret_i32<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ReinterpretI32)
}

fn f64_reinterpret_i64<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ReinterpretI64)
}

fn i32_extend_8_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Extend8S)
}

fn i32_extend_16_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Extend16S)
}

fn i64_extend_8_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend8S)
}

fn i64_extend_16_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend16S)
}

fn i64_extend_32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend32S)
}

fn i32_trunc_sat_f32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF32S)
}

fn i32_trunc_sat_f32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF32U)
}

fn i32_trunc_sat_f64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF64S)
}

fn i32_trunc_sat_f64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF64U)
}

fn i64_trunc_sat_f32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF32S)
}

fn i64_trunc_sat_f32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF32U)
}

fn i64_trunc_sat_f64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF64S)
}

fn i64_trunc_sat_f64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF64U)
}

fn memory_offset<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    memory_index: u32,
) -> Result<u32> {
    let (a, b, c) = module.config.memory_offset_choices();
    assert!(a + b + c != 0);

    let memory_type = &module.memories[memory_index as usize];
    let min = memory_type.limits.min.saturating_mul(65536);
    let max = memory_type
        .limits
        .max
        .map_or(u32::MAX, |max| max.saturating_mul(65536));

    let choice = u.int_in_range(0..=a + b + c - 1)?;
    if choice < a {
        u.int_in_range(0..=min)
    } else if choice < a + b {
        u.int_in_range(min..=max)
    } else {
        u.int_in_range(max..=u32::MAX)
    }
}

fn mem_arg<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    alignments: &[u32],
) -> Result<MemArg> {
    let memory_index = memory_index(u, module)?;
    let offset = memory_offset(u, module, memory_index)?;
    let align = *u.choose(alignments)?;
    Ok(MemArg {
        memory_index,
        offset,
        align,
    })
}

fn memory_index<C: Config>(u: &mut Unstructured, module: &ConfiguredModule<C>) -> Result<u32> {
    u.int_in_range(0..=module.memories.len() as u32 - 1)
}

fn data_index<C: Config>(u: &mut Unstructured, module: &ConfiguredModule<C>) -> Result<u32> {
    let data = module.data.len() as u32;
    assert!(data > 0);
    if data == 1 {
        Ok(0)
    } else {
        u.int_in_range(0..=data - 1)
    }
}

#[inline]
fn ref_null_valid<C: Config>(module: &ConfiguredModule<C>, _: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled()
}

fn ref_null<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let ty = *u.choose(&[ValType::ExternRef, ValType::FuncRef])?;
    builder.push_operands(&[ty]);
    Ok(Instruction::RefNull(ty))
}

#[inline]
fn ref_func_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled() && builder.allocs.referenced_functions.len() > 0
}

fn ref_func<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let i = *u.choose(&builder.allocs.referenced_functions)?;
    builder.push_operands(&[ValType::FuncRef]);
    Ok(Instruction::RefFunc(i))
}

#[inline]
fn ref_is_null_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.reference_types_enabled()
        && (builder.type_on_stack(ValType::ExternRef) || builder.type_on_stack(ValType::FuncRef))
}

fn ref_is_null<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    pop_reference_type(builder);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::RefIsNull)
}

#[inline]
fn table_fill_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled()
        && module.config.bulk_memory_enabled()
        && [ValType::ExternRef, ValType::FuncRef].iter().any(|ty| {
            builder.types_on_stack(&[ValType::I32, *ty, ValType::I32])
                && module.tables.iter().any(|t| t.elem_ty == *ty)
        })
}

fn table_fill<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    let ty = pop_reference_type(builder);
    builder.pop_operands(&[ValType::I32]);
    let table = table_index(ty, u, module)?;
    Ok(Instruction::TableFill { table })
}

#[inline]
fn table_set_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled()
        && [ValType::ExternRef, ValType::FuncRef].iter().any(|ty| {
            builder.types_on_stack(&[ValType::I32, *ty])
                && module.tables.iter().any(|t| t.elem_ty == *ty)
        })
}

fn table_set<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let ty = pop_reference_type(builder);
    builder.pop_operands(&[ValType::I32]);
    let table = table_index(ty, u, module)?;
    Ok(Instruction::TableSet { table })
}

#[inline]
fn table_get_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled()
        && builder.type_on_stack(ValType::I32)
        && module.tables.len() > 0
}

fn table_get<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    let idx = u.int_in_range(0..=module.tables.len() - 1)?;
    let ty = module.tables[idx].elem_ty;
    builder.push_operands(&[ty]);
    Ok(Instruction::TableGet { table: idx as u32 })
}

#[inline]
fn table_size_valid<C: Config>(module: &ConfiguredModule<C>, _: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled() && module.tables.len() > 0
}

fn table_size<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let table = u.int_in_range(0..=module.tables.len() - 1)? as u32;
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::TableSize { table })
}

#[inline]
fn table_grow_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled()
        && [ValType::ExternRef, ValType::FuncRef].iter().any(|ty| {
            builder.types_on_stack(&[*ty, ValType::I32])
                && module.tables.iter().any(|t| t.elem_ty == *ty)
        })
}

fn table_grow<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    let ty = pop_reference_type(builder);
    let table = table_index(ty, u, module)?;
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::TableGrow { table })
}

#[inline]
fn table_copy_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled()
        && module.tables.len() > 0
        && builder.types_on_stack(&[ValType::I32, ValType::I32, ValType::I32])
}

fn table_copy<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32, ValType::I32]);
    let src = u.int_in_range(0..=module.tables.len() - 1)? as u32;
    let dst = table_index(module.tables[src as usize].elem_ty, u, module)?;
    Ok(Instruction::TableCopy { src, dst })
}

#[inline]
fn table_init_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled()
        && builder.allocs.table_init_possible
        && builder.types_on_stack(&[ValType::I32, ValType::I32, ValType::I32])
}

fn table_init<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32, ValType::I32]);
    let segments = module
        .elems
        .iter()
        .enumerate()
        .filter(|(_, e)| module.tables.iter().any(|t| t.elem_ty == e.ty))
        .map(|(i, _)| i)
        .collect::<Vec<_>>();
    let segment = *u.choose(&segments)?;
    let table = table_index(module.elems[segment].ty, u, module)?;
    Ok(Instruction::TableInit {
        segment: segment as u32,
        table,
    })
}

#[inline]
fn elem_drop_valid<C: Config>(module: &ConfiguredModule<C>, _builder: &mut CodeBuilder<C>) -> bool {
    module.config.reference_types_enabled() && module.elems.len() > 0
}

fn elem_drop<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    _builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let segment = u.int_in_range(0..=module.elems.len() - 1)? as u32;
    Ok(Instruction::ElemDrop { segment })
}

fn pop_reference_type<C: Config>(builder: &mut CodeBuilder<C>) -> ValType {
    if builder.type_on_stack(ValType::ExternRef) {
        builder.pop_operands(&[ValType::ExternRef]);
        ValType::ExternRef
    } else {
        builder.pop_operands(&[ValType::FuncRef]);
        ValType::FuncRef
    }
}

fn table_index<C: Config>(
    ty: ValType,
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
) -> Result<u32> {
    let tables = module
        .tables
        .iter()
        .enumerate()
        .filter(|(_, t)| t.elem_ty == ty)
        .map(|t| t.0 as u32)
        .collect::<Vec<_>>();
    Ok(*u.choose(&tables)?)
}

fn lane_index(u: &mut Unstructured, number_of_lanes: u8) -> Result<u8> {
    u.int_in_range(0..=(number_of_lanes - 1))
}

#[inline]
fn simd_v128_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.types_on_stack(&[ValType::V128])
}

#[inline]
fn simd_v128_v128_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.types_on_stack(&[ValType::V128, ValType::V128])
}

#[inline]
fn simd_v128_v128_v128_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled()
        && builder.types_on_stack(&[ValType::V128, ValType::V128, ValType::V128])
}

#[inline]
fn simd_v128_i32_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.types_on_stack(&[ValType::V128, ValType::I32])
}

#[inline]
fn simd_v128_i64_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.types_on_stack(&[ValType::V128, ValType::I64])
}

#[inline]
fn simd_v128_f32_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.types_on_stack(&[ValType::V128, ValType::F32])
}

#[inline]
fn simd_v128_f64_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.types_on_stack(&[ValType::V128, ValType::F64])
}

#[inline]
fn simd_i32_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.type_on_stack(ValType::I32)
}

#[inline]
fn simd_i64_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.type_on_stack(ValType::I64)
}

#[inline]
fn simd_f32_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.type_on_stack(ValType::F32)
}

#[inline]
fn simd_f64_on_stack<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && builder.type_on_stack(ValType::F64)
}

#[inline]
fn simd_have_memory_and_offset<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled()
        && have_memory(module, builder)
        && builder.type_on_stack(ValType::I32)
}

#[inline]
fn simd_have_memory_and_offset_and_v128<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled()
        && have_memory(module, builder)
        && builder.types_on_stack(&[ValType::I32, ValType::V128])
}

#[inline]
fn simd_v128_store_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    module.config.simd_enabled() && store_valid(module, builder, || ValType::V128)
}

#[inline]
fn simd_enabled<C: Config>(module: &ConfiguredModule<C>, _: &mut CodeBuilder<C>) -> bool {
    module.config.simd_enabled()
}

macro_rules! simd_load {
    ($instruction:ident, $generator_fn_name:ident, $alignments:expr) => {
        fn $generator_fn_name<C: Config>(
            u: &mut Unstructured,
            module: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        ) -> Result<Instruction> {
            builder.pop_operands(&[ValType::I32]);
            builder.push_operands(&[ValType::V128]);
            Ok(Instruction::$instruction {
                memarg: mem_arg(u, module, $alignments)?,
            })
        }
    };
}

simd_load!(V128Load, v128_load, &[0, 1, 2, 3, 4]);
simd_load!(V128Load8x8S, v128_load8x8s, &[0, 1, 2, 3]);
simd_load!(V128Load8x8U, v128_load8x8u, &[0, 1, 2, 3]);
simd_load!(V128Load16x4S, v128_load16x4s, &[0, 1, 2, 3]);
simd_load!(V128Load16x4U, v128_load16x4u, &[0, 1, 2, 3]);
simd_load!(V128Load32x2S, v128_load32x2s, &[0, 1, 2, 3]);
simd_load!(V128Load32x2U, v128_load32x2u, &[0, 1, 2, 3]);
simd_load!(V128Load8Splat, v128_load8_splat, &[0]);
simd_load!(V128Load16Splat, v128_load16_splat, &[0, 1]);
simd_load!(V128Load32Splat, v128_load32_splat, &[0, 1, 2]);
simd_load!(V128Load64Splat, v128_load64_splat, &[0, 1, 2, 3]);
simd_load!(V128Load32Zero, v128_load32_zero, &[0, 1, 2]);
simd_load!(V128Load64Zero, v128_load64_zero, &[0, 1, 2, 3]);

fn v128_store<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::V128]);
    Ok(Instruction::V128Store {
        memarg: mem_arg(u, module, &[0, 1, 2, 3, 4])?,
    })
}

macro_rules! simd_load_lane {
    ($instruction:ident, $generator_fn_name:ident, $alignments:expr, $number_of_lanes:expr) => {
        fn $generator_fn_name<C: Config>(
            u: &mut Unstructured,
            module: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        ) -> Result<Instruction> {
            builder.pop_operands(&[ValType::I32, ValType::V128]);
            builder.push_operands(&[ValType::V128]);
            Ok(Instruction::$instruction {
                memarg: mem_arg(u, module, $alignments)?,
                lane: lane_index(u, $number_of_lanes)?,
            })
        }
    };
}

simd_load_lane!(V128Load8Lane, v128_load8_lane, &[0], 16);
simd_load_lane!(V128Load16Lane, v128_load16_lane, &[0, 1], 8);
simd_load_lane!(V128Load32Lane, v128_load32_lane, &[0, 1, 2], 4);
simd_load_lane!(V128Load64Lane, v128_load64_lane, &[0, 1, 2, 3], 2);

macro_rules! simd_store_lane {
    ($instruction:ident, $generator_fn_name:ident, $alignments:expr, $number_of_lanes:expr) => {
        fn $generator_fn_name<C: Config>(
            u: &mut Unstructured,
            module: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        ) -> Result<Instruction> {
            builder.pop_operands(&[ValType::I32, ValType::V128]);
            Ok(Instruction::$instruction {
                memarg: mem_arg(u, module, $alignments)?,
                lane: lane_index(u, $number_of_lanes)?,
            })
        }
    };
}

simd_store_lane!(V128Store8Lane, v128_store8_lane, &[0], 16);
simd_store_lane!(V128Store16Lane, v128_store16_lane, &[0, 1], 8);
simd_store_lane!(V128Store32Lane, v128_store32_lane, &[0, 1, 2], 4);
simd_store_lane!(V128Store64Lane, v128_store64_lane, &[0, 1, 2, 3], 2);

fn v128_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.push_operands(&[ValType::V128]);
    let c = i128::from_le_bytes(u.arbitrary()?);
    Ok(Instruction::V128Const(c))
}

fn i8x16_shuffle<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::V128, ValType::V128]);
    builder.push_operands(&[ValType::V128]);
    let mut lanes = [0; 16];
    for i in 0..16 {
        lanes[i] = u.int_in_range(0..=31)?;
    }
    Ok(Instruction::I8x16Shuffle { lanes: lanes })
}

macro_rules! simd_lane_access {
    ($instruction:ident, $generator_fn_name:ident, $in_types:expr => $out_types:expr, $number_of_lanes:expr) => {
        fn $generator_fn_name<C: Config>(
            u: &mut Unstructured,
            _: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        ) -> Result<Instruction> {
            builder.pop_operands($in_types);
            builder.push_operands($out_types);
            Ok(Instruction::$instruction {
                lane: lane_index(u, $number_of_lanes)?,
            })
        }
    };
}

simd_lane_access!(I8x16ExtractLaneS, i8x16_extract_lane_s, &[ValType::V128] => &[ValType::I32], 16);
simd_lane_access!(I8x16ExtractLaneU, i8x16_extract_lane_u, &[ValType::V128] => &[ValType::I32], 16);
simd_lane_access!(I8x16ReplaceLane, i8x16_replace_lane, &[ValType::V128, ValType::I32] => &[ValType::V128], 16);
simd_lane_access!(I16x8ExtractLaneS, i16x8_extract_lane_s, &[ValType::V128] => &[ValType::I32], 8);
simd_lane_access!(I16x8ExtractLaneU, i16x8_extract_lane_u, &[ValType::V128] => &[ValType::I32], 8);
simd_lane_access!(I16x8ReplaceLane, i16x8_replace_lane, &[ValType::V128, ValType::I32] => &[ValType::V128], 8);
simd_lane_access!(I32x4ExtractLane, i32x4_extract_lane, &[ValType::V128] => &[ValType::I32], 4);
simd_lane_access!(I32x4ReplaceLane, i32x4_replace_lane, &[ValType::V128, ValType::I32] => &[ValType::V128], 4);
simd_lane_access!(I64x2ExtractLane, i64x2_extract_lane, &[ValType::V128] => &[ValType::I64], 2);
simd_lane_access!(I64x2ReplaceLane, i64x2_replace_lane, &[ValType::V128, ValType::I64] => &[ValType::V128], 2);
simd_lane_access!(F32x4ExtractLane, f32x4_extract_lane, &[ValType::V128] => &[ValType::F32], 4);
simd_lane_access!(F32x4ReplaceLane, f32x4_replace_lane, &[ValType::V128, ValType::F32] => &[ValType::V128], 4);
simd_lane_access!(F64x2ExtractLane, f64x2_extract_lane, &[ValType::V128] => &[ValType::F64], 2);
simd_lane_access!(F64x2ReplaceLane, f64x2_replace_lane, &[ValType::V128, ValType::F64] => &[ValType::V128], 2);

macro_rules! simd_binop {
    ($instruction:ident, $generator_fn_name:ident) => {
        fn $generator_fn_name<C: Config>(
            _: &mut Unstructured,
            _: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        ) -> Result<Instruction> {
            builder.pop_operands(&[ValType::V128, ValType::V128]);
            builder.push_operands(&[ValType::V128]);
            Ok(Instruction::$instruction)
        }
    };
}

macro_rules! simd_unop {
    ($instruction:ident, $generator_fn_name:ident) => {
        simd_unop!($instruction, $generator_fn_name, V128 -> V128);
    };

    ($instruction:ident, $generator_fn_name:ident, $in_type:ident -> $out_type:ident) => {
        fn $generator_fn_name<C: Config>(
            _: &mut Unstructured,
            _: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        ) -> Result<Instruction> {
            builder.pop_operands(&[ValType::$in_type]);
            builder.push_operands(&[ValType::$out_type]);
            Ok(Instruction::$instruction)
        }
    };
}

macro_rules! simd_shift {
    ($instruction:ident, $generator_fn_name:ident) => {
        fn $generator_fn_name<C: Config>(
            _: &mut Unstructured,
            _: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        ) -> Result<Instruction> {
            builder.pop_operands(&[ValType::V128, ValType::I32]);
            builder.push_operands(&[ValType::V128]);
            Ok(Instruction::$instruction)
        }
    };
}

simd_unop!(I8x16Splat, i8x16_splat, I32 -> V128);
simd_unop!(I16x8Splat, i16x8_splat, I32 -> V128);
simd_unop!(I32x4Splat, i32x4_splat, I32 -> V128);
simd_unop!(I64x2Splat, i64x2_splat, I64 -> V128);
simd_unop!(F32x4Splat, f32x4_splat, F32 -> V128);
simd_unop!(F64x2Splat, f64x2_splat, F64 -> V128);
simd_binop!(I8x16Swizzle, i8x16_swizzle);
simd_binop!(I8x16Eq, i8x16_eq);
simd_binop!(I8x16Ne, i8x16_ne);
simd_binop!(I8x16LtS, i8x16_lt_s);
simd_binop!(I8x16LtU, i8x16_lt_u);
simd_binop!(I8x16GtS, i8x16_gt_s);
simd_binop!(I8x16GtU, i8x16_gt_u);
simd_binop!(I8x16LeS, i8x16_le_s);
simd_binop!(I8x16LeU, i8x16_le_u);
simd_binop!(I8x16GeS, i8x16_ge_s);
simd_binop!(I8x16GeU, i8x16_ge_u);
simd_binop!(I16x8Eq, i16x8_eq);
simd_binop!(I16x8Ne, i16x8_ne);
simd_binop!(I16x8LtS, i16x8_lt_s);
simd_binop!(I16x8LtU, i16x8_lt_u);
simd_binop!(I16x8GtS, i16x8_gt_s);
simd_binop!(I16x8GtU, i16x8_gt_u);
simd_binop!(I16x8LeS, i16x8_le_s);
simd_binop!(I16x8LeU, i16x8_le_u);
simd_binop!(I16x8GeS, i16x8_ge_s);
simd_binop!(I16x8GeU, i16x8_ge_u);
simd_binop!(I32x4Eq, i32x4_eq);
simd_binop!(I32x4Ne, i32x4_ne);
simd_binop!(I32x4LtS, i32x4_lt_s);
simd_binop!(I32x4LtU, i32x4_lt_u);
simd_binop!(I32x4GtS, i32x4_gt_s);
simd_binop!(I32x4GtU, i32x4_gt_u);
simd_binop!(I32x4LeS, i32x4_le_s);
simd_binop!(I32x4LeU, i32x4_le_u);
simd_binop!(I32x4GeS, i32x4_ge_s);
simd_binop!(I32x4GeU, i32x4_ge_u);
simd_binop!(I64x2Eq, i64x2_eq);
simd_binop!(I64x2Ne, i64x2_ne);
simd_binop!(I64x2LtS, i64x2_lt_s);
simd_binop!(I64x2GtS, i64x2_gt_s);
simd_binop!(I64x2LeS, i64x2_le_s);
simd_binop!(I64x2GeS, i64x2_ge_s);
simd_binop!(F32x4Eq, f32x4_eq);
simd_binop!(F32x4Ne, f32x4_ne);
simd_binop!(F32x4Lt, f32x4_lt);
simd_binop!(F32x4Gt, f32x4_gt);
simd_binop!(F32x4Le, f32x4_le);
simd_binop!(F32x4Ge, f32x4_ge);
simd_binop!(F64x2Eq, f64x2_eq);
simd_binop!(F64x2Ne, f64x2_ne);
simd_binop!(F64x2Lt, f64x2_lt);
simd_binop!(F64x2Gt, f64x2_gt);
simd_binop!(F64x2Le, f64x2_le);
simd_binop!(F64x2Ge, f64x2_ge);
simd_unop!(V128Not, v128_not);
simd_binop!(V128And, v128_and);
simd_binop!(V128AndNot, v128_and_not);
simd_binop!(V128Or, v128_or);
simd_binop!(V128Xor, v128_xor);
simd_unop!(V128AnyTrue, v128_any_true, V128 -> I32);
simd_unop!(I8x16Abs, i8x16_abs);
simd_unop!(I8x16Neg, i8x16_neg);
simd_unop!(I8x16Popcnt, i8x16_popcnt);
simd_unop!(I8x16AllTrue, i8x16_all_true, V128 -> I32);
simd_unop!(I8x16Bitmask, i8x16_bitmask, V128 -> I32);
simd_binop!(I8x16NarrowI16x8S, i8x16_narrow_i16x8s);
simd_binop!(I8x16NarrowI16x8U, i8x16_narrow_i16x8u);
simd_shift!(I8x16Shl, i8x16_shl);
simd_shift!(I8x16ShrS, i8x16_shr_s);
simd_shift!(I8x16ShrU, i8x16_shr_u);
simd_binop!(I8x16Add, i8x16_add);
simd_binop!(I8x16AddSatS, i8x16_add_sat_s);
simd_binop!(I8x16AddSatU, i8x16_add_sat_u);
simd_binop!(I8x16Sub, i8x16_sub);
simd_binop!(I8x16SubSatS, i8x16_sub_sat_s);
simd_binop!(I8x16SubSatU, i8x16_sub_sat_u);
simd_binop!(I8x16MinS, i8x16_min_s);
simd_binop!(I8x16MinU, i8x16_min_u);
simd_binop!(I8x16MaxS, i8x16_max_s);
simd_binop!(I8x16MaxU, i8x16_max_u);
simd_binop!(I8x16RoundingAverageU, i8x16_rounding_average_u);
simd_unop!(I16x8ExtAddPairwiseI8x16S, i16x8_ext_add_pairwise_i8x16s);
simd_unop!(I16x8ExtAddPairwiseI8x16U, i16x8_ext_add_pairwise_i8x16u);
simd_unop!(I16x8Abs, i16x8_abs);
simd_unop!(I16x8Neg, i16x8_neg);
simd_binop!(I16x8Q15MulrSatS, i16x8q15_mulr_sat_s);
simd_unop!(I16x8AllTrue, i16x8_all_true, V128 -> I32);
simd_unop!(I16x8Bitmask, i16x8_bitmask, V128 -> I32);
simd_binop!(I16x8NarrowI32x4S, i16x8_narrow_i32x4s);
simd_binop!(I16x8NarrowI32x4U, i16x8_narrow_i32x4u);
simd_unop!(I16x8ExtendLowI8x16S, i16x8_extend_low_i8x16s);
simd_unop!(I16x8ExtendHighI8x16S, i16x8_extend_high_i8x16s);
simd_unop!(I16x8ExtendLowI8x16U, i16x8_extend_low_i8x16u);
simd_unop!(I16x8ExtendHighI8x16U, i16x8_extend_high_i8x16u);
simd_shift!(I16x8Shl, i16x8_shl);
simd_shift!(I16x8ShrS, i16x8_shr_s);
simd_shift!(I16x8ShrU, i16x8_shr_u);
simd_binop!(I16x8Add, i16x8_add);
simd_binop!(I16x8AddSatS, i16x8_add_sat_s);
simd_binop!(I16x8AddSatU, i16x8_add_sat_u);
simd_binop!(I16x8Sub, i16x8_sub);
simd_binop!(I16x8SubSatS, i16x8_sub_sat_s);
simd_binop!(I16x8SubSatU, i16x8_sub_sat_u);
simd_binop!(I16x8Mul, i16x8_mul);
simd_binop!(I16x8MinS, i16x8_min_s);
simd_binop!(I16x8MinU, i16x8_min_u);
simd_binop!(I16x8MaxS, i16x8_max_s);
simd_binop!(I16x8MaxU, i16x8_max_u);
simd_binop!(I16x8RoundingAverageU, i16x8_rounding_average_u);
simd_binop!(I16x8ExtMulLowI8x16S, i16x8_ext_mul_low_i8x16s);
simd_binop!(I16x8ExtMulHighI8x16S, i16x8_ext_mul_high_i8x16s);
simd_binop!(I16x8ExtMulLowI8x16U, i16x8_ext_mul_low_i8x16u);
simd_binop!(I16x8ExtMulHighI8x16U, i16x8_ext_mul_high_i8x16u);
simd_unop!(I32x4ExtAddPairwiseI16x8S, i32x4_ext_add_pairwise_i16x8s);
simd_unop!(I32x4ExtAddPairwiseI16x8U, i32x4_ext_add_pairwise_i16x8u);
simd_unop!(I32x4Abs, i32x4_abs);
simd_unop!(I32x4Neg, i32x4_neg);
simd_unop!(I32x4AllTrue, i32x4_all_true, V128 -> I32);
simd_unop!(I32x4Bitmask, i32x4_bitmask, V128 -> I32);
simd_unop!(I32x4ExtendLowI16x8S, i32x4_extend_low_i16x8s);
simd_unop!(I32x4ExtendHighI16x8S, i32x4_extend_high_i16x8s);
simd_unop!(I32x4ExtendLowI16x8U, i32x4_extend_low_i16x8u);
simd_unop!(I32x4ExtendHighI16x8U, i32x4_extend_high_i16x8u);
simd_shift!(I32x4Shl, i32x4_shl);
simd_shift!(I32x4ShrS, i32x4_shr_s);
simd_shift!(I32x4ShrU, i32x4_shr_u);
simd_binop!(I32x4Add, i32x4_add);
simd_binop!(I32x4Sub, i32x4_sub);
simd_binop!(I32x4Mul, i32x4_mul);
simd_binop!(I32x4MinS, i32x4_min_s);
simd_binop!(I32x4MinU, i32x4_min_u);
simd_binop!(I32x4MaxS, i32x4_max_s);
simd_binop!(I32x4MaxU, i32x4_max_u);
simd_binop!(I32x4DotI16x8S, i32x4_dot_i16x8s);
simd_binop!(I32x4ExtMulLowI16x8S, i32x4_ext_mul_low_i16x8s);
simd_binop!(I32x4ExtMulHighI16x8S, i32x4_ext_mul_high_i16x8s);
simd_binop!(I32x4ExtMulLowI16x8U, i32x4_ext_mul_low_i16x8u);
simd_binop!(I32x4ExtMulHighI16x8U, i32x4_ext_mul_high_i16x8u);
simd_unop!(I64x2Abs, i64x2_abs);
simd_unop!(I64x2Neg, i64x2_neg);
simd_unop!(I64x2AllTrue, i64x2_all_true, V128 -> I32);
simd_unop!(I64x2Bitmask, i64x2_bitmask, V128 -> I32);
simd_unop!(I64x2ExtendLowI32x4S, i64x2_extend_low_i32x4s);
simd_unop!(I64x2ExtendHighI32x4S, i64x2_extend_high_i32x4s);
simd_unop!(I64x2ExtendLowI32x4U, i64x2_extend_low_i32x4u);
simd_unop!(I64x2ExtendHighI32x4U, i64x2_extend_high_i32x4u);
simd_shift!(I64x2Shl, i64x2_shl);
simd_shift!(I64x2ShrS, i64x2_shr_s);
simd_shift!(I64x2ShrU, i64x2_shr_u);
simd_binop!(I64x2Add, i64x2_add);
simd_binop!(I64x2Sub, i64x2_sub);
simd_binop!(I64x2Mul, i64x2_mul);
simd_binop!(I64x2ExtMulLowI32x4S, i64x2_ext_mul_low_i32x4s);
simd_binop!(I64x2ExtMulHighI32x4S, i64x2_ext_mul_high_i32x4s);
simd_binop!(I64x2ExtMulLowI32x4U, i64x2_ext_mul_low_i32x4u);
simd_binop!(I64x2ExtMulHighI32x4U, i64x2_ext_mul_high_i32x4u);
simd_unop!(F32x4Ceil, f32x4_ceil);
simd_unop!(F32x4Floor, f32x4_floor);
simd_unop!(F32x4Trunc, f32x4_trunc);
simd_unop!(F32x4Nearest, f32x4_nearest);
simd_unop!(F32x4Abs, f32x4_abs);
simd_unop!(F32x4Neg, f32x4_neg);
simd_unop!(F32x4Sqrt, f32x4_sqrt);
simd_binop!(F32x4Add, f32x4_add);
simd_binop!(F32x4Sub, f32x4_sub);
simd_binop!(F32x4Mul, f32x4_mul);
simd_binop!(F32x4Div, f32x4_div);
simd_binop!(F32x4Min, f32x4_min);
simd_binop!(F32x4Max, f32x4_max);
simd_binop!(F32x4PMin, f32x4p_min);
simd_binop!(F32x4PMax, f32x4p_max);
simd_unop!(F64x2Ceil, f64x2_ceil);
simd_unop!(F64x2Floor, f64x2_floor);
simd_unop!(F64x2Trunc, f64x2_trunc);
simd_unop!(F64x2Nearest, f64x2_nearest);
simd_unop!(F64x2Abs, f64x2_abs);
simd_unop!(F64x2Neg, f64x2_neg);
simd_unop!(F64x2Sqrt, f64x2_sqrt);
simd_binop!(F64x2Add, f64x2_add);
simd_binop!(F64x2Sub, f64x2_sub);
simd_binop!(F64x2Mul, f64x2_mul);
simd_binop!(F64x2Div, f64x2_div);
simd_binop!(F64x2Min, f64x2_min);
simd_binop!(F64x2Max, f64x2_max);
simd_binop!(F64x2PMin, f64x2p_min);
simd_binop!(F64x2PMax, f64x2p_max);
simd_unop!(I32x4TruncSatF32x4S, i32x4_trunc_sat_f32x4s);
simd_unop!(I32x4TruncSatF32x4U, i32x4_trunc_sat_f32x4u);
simd_unop!(F32x4ConvertI32x4S, f32x4_convert_i32x4s);
simd_unop!(F32x4ConvertI32x4U, f32x4_convert_i32x4u);
simd_unop!(I32x4TruncSatF64x2SZero, i32x4_trunc_sat_f64x2s_zero);
simd_unop!(I32x4TruncSatF64x2UZero, i32x4_trunc_sat_f64x2u_zero);
simd_unop!(F64x2ConvertLowI32x4S, f64x2_convert_low_i32x4s);
simd_unop!(F64x2ConvertLowI32x4U, f64x2_convert_low_i32x4u);
simd_unop!(F32x4DemoteF64x2Zero, f32x4_demote_f64x2_zero);
simd_unop!(F64x2PromoteLowF32x4, f64x2_promote_low_f32x4);

fn v128_bitselect<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::V128, ValType::V128, ValType::V128]);
    builder.push_operands(&[ValType::V128]);
    Ok(Instruction::V128Bitselect)
}
