//! Helper to encode [Lang] expressions to Wasm.

use crate::{
    Error, Result, WasmMutate,
    module::PrimitiveTypeInfo,
    mutators::peephole::{EG, Lang, MemArg, eggsy::encoder::TraversalEvent},
};
use egg::{Id, Language, RecExpr};
use rand::Rng;
use std::num::Wrapping;
use wasm_encoder::Function;

/// Some custom nodes might need special resource allocation outside the
/// function. Fore xample, if a new global is needed is should be added outside
/// the construction of the function in the `expr2wasm` method.
#[derive(Clone, Debug)]
pub enum ResourceRequest {
    /// Global resource request
    Global {
        /// Global type
        tpe: PrimitiveTypeInfo,
        /// If its mutable
        mutable: bool,
        /// If its shared
        shared: bool,
    },
    // TODO add other needed resources here, for example, needed locals, needed
    // memory etc. Notice that how this resources are translated to Wasm code,
    // you need to change the code inside the `mutate_with_rules` of the
    // PeepholeMutator trait
}

/// Encodes an expression in the [Lang][Lang] intermediate language to Wasm
/// * `rnd`  Random generator
/// * `expr` [Lang] expression to encode
/// * `newfunc` Function stream in which the expression will be encoded
pub fn expr2wasm(
    config: &mut WasmMutate,
    expr: &RecExpr<Lang>,
    newfunc: &mut Function,
    _egraph: &EG,
) -> Result<Vec<ResourceRequest>> {
    let nodes = expr.as_ref();
    // The last node is the root.
    let root = Id::from(nodes.len() - 1);

    struct Context {
        // Current visited node index in the tree traversing
        current_node: Id,
        // In which state of the traversing of the current node the process is
        traversal_event: TraversalEvent,
    }

    impl Context {
        fn new(current_node: Id, traversal_event: TraversalEvent) -> Self {
            Context {
                current_node,
                traversal_event,
            }
        }
    }

    let mut worklist = vec![
        Context::new(root, TraversalEvent::Exit),
        Context::new(root, TraversalEvent::Enter),
    ];

    let enqueue = |worklist: &mut Vec<_>, child| {
        worklist.push(Context::new(child, TraversalEvent::Exit));
        worklist.push(Context::new(child, TraversalEvent::Enter));
    };

    // Resources out of the function body that the expr translation needs, for
    // example, the creation of new globals
    let mut resources = vec![];
    // Next global idx
    let mut global_idx = config.info().get_global_count() as u32;

    // Enqueue the coding back nodes and infer types
    while let Some(context) = worklist.pop() {
        let rootlang = &nodes[usize::from(context.current_node)];

        match context.traversal_event {
            TraversalEvent::Enter => match rootlang {
                Lang::UnfoldI32(_) | Lang::UnfoldI64(_) => {}
                _ => {
                    for operand in rootlang.children().iter().rev() {
                        enqueue(&mut worklist, *operand);
                    }
                }
            },
            TraversalEvent::Exit => {
                let insn = &mut newfunc.instructions();
                match rootlang {
                    Lang::LocalGet(idx) => insn.local_get(*idx),
                    Lang::GlobalGet(idx) => insn.global_get(*idx),
                    Lang::LocalSet(idx, _val) => insn.local_set(*idx),
                    Lang::GlobalSet(idx, _val) => insn.global_set(*idx),
                    Lang::LocalTee(idx, _) => insn.local_tee(*idx),
                    Lang::Wrap(_) => insn.i32_wrap_i64(),
                    Lang::Call(idx, _) => insn.call(*idx),
                    Lang::Drop(_) => insn.drop(),
                    Lang::I32Load(memarg, _) => insn.i32_load(memarg.into()),
                    Lang::I64Load(memarg, _) => insn.i64_load(memarg.into()),
                    Lang::F32Load(memarg, _) => insn.f32_load(memarg.into()),
                    Lang::F64Load(memarg, _) => insn.f64_load(memarg.into()),
                    Lang::I32Load8U(memarg, _) => insn.i32_load8_u(memarg.into()),
                    Lang::I32Load8S(memarg, _) => insn.i32_load8_s(memarg.into()),
                    Lang::I32Load16U(memarg, _) => insn.i32_load16_u(memarg.into()),
                    Lang::I32Load16S(memarg, _) => insn.i32_load16_s(memarg.into()),
                    Lang::I64Load8U(memarg, _) => insn.i64_load8_u(memarg.into()),
                    Lang::I64Load8S(memarg, _) => insn.i64_load8_s(memarg.into()),
                    Lang::I64Load16U(memarg, _) => insn.i64_load16_u(memarg.into()),
                    Lang::I64Load16S(memarg, _) => insn.i64_load16_s(memarg.into()),
                    Lang::I64Load32U(memarg, _) => insn.i64_load32_u(memarg.into()),
                    Lang::I64Load32S(memarg, _) => insn.i64_load32_s(memarg.into()),
                    Lang::RandI32 => insn.i32_const(config.rng().random()),
                    Lang::RandI64 => insn.i64_const(config.rng().random()),
                    Lang::RandF32 => insn.f32_const(f32::from_bits(config.rng().random()).into()),
                    Lang::RandF64 => insn.f64_const(f64::from_bits(config.rng().random()).into()),
                    Lang::Undef => {
                        // Do nothing
                        insn
                    }
                    Lang::UnfoldI32(value) => {
                        let child = &nodes[usize::from(*value)];
                        match child {
                            Lang::I32(value) => {
                                // Getting type from eclass.

                                let r: i32 = config.rng().random();
                                insn.i32_const(r);
                                insn.i32_const((Wrapping(*value) - Wrapping(r)).0);
                                insn.i32_add();
                            }
                            _ => {
                                return Err(Error::other(format!(
                                    "The current eterm cannot be unfolded {child:?}.\n expr {expr}"
                                )));
                            }
                        }
                        insn
                    }
                    Lang::UnfoldI64(value) => {
                        let child = &nodes[usize::from(*value)];
                        match child {
                            Lang::I64(value) => {
                                // Getting type from eclass.

                                let r: i64 = config.rng().random();
                                insn.i64_const(r);
                                insn.i64_const((Wrapping(*value) - Wrapping(r)).0);
                                insn.i64_add();
                            }
                            _ => {
                                return Err(Error::other(format!(
                                    "The current eterm cannot be unfolded {child:?}.\n expr {expr}"
                                )));
                            }
                        }
                        insn
                    }
                    Lang::I32(v) => insn.i32_const(*v),
                    Lang::I64(v) => insn.i64_const(*v),
                    Lang::F32(v) => insn.f32_const(v.to_f32().into()),
                    Lang::F64(v) => insn.f64_const(v.to_f64().into()),
                    Lang::V128(v) => insn.v128_const(*v),
                    Lang::I32Add(_) => insn.i32_add(),
                    Lang::I64Add(_) => insn.i64_add(),
                    Lang::I32Sub(_) => insn.i32_sub(),
                    Lang::I64Sub(_) => insn.i64_sub(),
                    Lang::I32Mul(_) => insn.i32_mul(),
                    Lang::I64Mul(_) => insn.i64_mul(),
                    Lang::I32And(_) => insn.i32_and(),
                    Lang::I64And(_) => insn.i64_and(),
                    Lang::I32Or(_) => insn.i32_or(),
                    Lang::I64Or(_) => insn.i64_or(),
                    Lang::I32Xor(_) => insn.i32_xor(),
                    Lang::I64Xor(_) => insn.i64_xor(),
                    Lang::I32Shl(_) => insn.i32_shl(),
                    Lang::I64Shl(_) => insn.i64_shl(),
                    Lang::I32ShrU(_) => insn.i32_shr_u(),
                    Lang::I64ShrU(_) => insn.i64_shr_u(),
                    Lang::I32DivU(_) => insn.i32_div_u(),
                    Lang::I64DivU(_) => insn.i64_div_u(),
                    Lang::I32DivS(_) => insn.i32_div_s(),
                    Lang::I64DivS(_) => insn.i64_div_s(),
                    Lang::I32ShrS(_) => insn.i32_shr_s(),
                    Lang::I64ShrS(_) => insn.i64_shr_s(),
                    Lang::I32RotR(_) => insn.i32_rotr(),
                    Lang::I64RotR(_) => insn.i64_rotr(),
                    Lang::I32RotL(_) => insn.i32_rotl(),
                    Lang::I64RotL(_) => insn.i64_rotl(),
                    Lang::I32RemS(_) => insn.i32_rem_s(),
                    Lang::I64RemS(_) => insn.i64_rem_s(),
                    Lang::I32RemU(_) => insn.i32_rem_u(),
                    Lang::I64RemU(_) => insn.i64_rem_u(),
                    Lang::I32Eqz(_) => insn.i32_eqz(),
                    Lang::I64Eqz(_) => insn.i64_eqz(),
                    Lang::I32Eq(_) => insn.i32_eq(),
                    Lang::I64Eq(_) => insn.i64_eq(),
                    Lang::I32Ne(_) => insn.i32_ne(),
                    Lang::I64Ne(_) => insn.i64_ne(),
                    Lang::I32LtS(_) => insn.i32_lt_s(),
                    Lang::I64LtS(_) => insn.i64_lt_s(),
                    Lang::I32LtU(_) => insn.i32_lt_u(),
                    Lang::I64LtU(_) => insn.i64_lt_u(),
                    Lang::I32GtS(_) => insn.i32_gt_s(),
                    Lang::I64GtS(_) => insn.i64_gt_s(),
                    Lang::I32GtU(_) => insn.i32_gt_u(),
                    Lang::I64GtU(_) => insn.i64_gt_u(),
                    Lang::I32LeS(_) => insn.i32_le_s(),
                    Lang::I64LeS(_) => insn.i64_le_s(),
                    Lang::I32LeU(_) => insn.i32_le_u(),
                    Lang::I64LeU(_) => insn.i64_le_u(),
                    Lang::I32GeS(_) => insn.i32_ge_s(),
                    Lang::I64GeS(_) => insn.i64_ge_s(),
                    Lang::I32GeU(_) => insn.i32_ge_u(),
                    Lang::I64GeU(_) => insn.i64_ge_u(),
                    Lang::I32Clz(_) => insn.i32_clz(),
                    Lang::I32Ctz(_) => insn.i32_ctz(),
                    Lang::I64Ctz(_) => insn.i64_ctz(),
                    Lang::I64Clz(_) => insn.i64_clz(),
                    Lang::F32Abs(_) => insn.f32_abs(),
                    Lang::F64Abs(_) => insn.f64_abs(),
                    Lang::F32Neg(_) => insn.f32_neg(),
                    Lang::F64Neg(_) => insn.f64_neg(),
                    Lang::F32Sqrt(_) => insn.f32_sqrt(),
                    Lang::F64Sqrt(_) => insn.f64_sqrt(),
                    Lang::F32Ceil(_) => insn.f32_ceil(),
                    Lang::F64Ceil(_) => insn.f64_ceil(),
                    Lang::F32Floor(_) => insn.f32_floor(),
                    Lang::F64Floor(_) => insn.f64_floor(),
                    Lang::F32Trunc(_) => insn.f32_trunc(),
                    Lang::F64Trunc(_) => insn.f64_trunc(),
                    Lang::F32Nearest(_) => insn.f32_nearest(),
                    Lang::F64Nearest(_) => insn.f64_nearest(),
                    Lang::I32TruncF32S(_) => insn.i32_trunc_f32_s(),
                    Lang::I32TruncF32U(_) => insn.i32_trunc_f32_u(),
                    Lang::I32TruncF64S(_) => insn.i32_trunc_f64_s(),
                    Lang::I32TruncF64U(_) => insn.i32_trunc_f64_u(),
                    Lang::I64TruncF32S(_) => insn.i64_trunc_f32_s(),
                    Lang::I64TruncF32U(_) => insn.i64_trunc_f32_u(),
                    Lang::I64TruncF64S(_) => insn.i64_trunc_f64_s(),
                    Lang::I64TruncF64U(_) => insn.i64_trunc_f64_u(),
                    Lang::F32ConvertI32S(_) => insn.f32_convert_i32_s(),
                    Lang::F32ConvertI32U(_) => insn.f32_convert_i32_u(),
                    Lang::F32ConvertI64S(_) => insn.f32_convert_i64_s(),
                    Lang::F32ConvertI64U(_) => insn.f32_convert_i64_u(),
                    Lang::F32DemoteF64(_) => insn.f32_demote_f64(),
                    Lang::F64ConvertI32S(_) => insn.f64_convert_i32_s(),
                    Lang::F64ConvertI32U(_) => insn.f64_convert_i32_u(),
                    Lang::F64ConvertI64S(_) => insn.f64_convert_i64_s(),
                    Lang::F64ConvertI64U(_) => insn.f64_convert_i64_u(),
                    Lang::F64PromoteF32(_) => insn.f64_promote_f32(),
                    Lang::I32ReinterpretF32(_) => insn.i32_reinterpret_f32(),
                    Lang::I64ReinterpretF64(_) => insn.i64_reinterpret_f64(),
                    Lang::F32ReinterpretI32(_) => insn.f32_reinterpret_i32(),
                    Lang::F64ReinterpretI64(_) => insn.f64_reinterpret_i64(),
                    Lang::I32TruncSatF32S(_) => insn.i32_trunc_sat_f32_s(),
                    Lang::I32TruncSatF32U(_) => insn.i32_trunc_sat_f32_u(),
                    Lang::I32TruncSatF64S(_) => insn.i32_trunc_sat_f64_s(),
                    Lang::I32TruncSatF64U(_) => insn.i32_trunc_sat_f64_u(),
                    Lang::I64TruncSatF32S(_) => insn.i64_trunc_sat_f32_s(),
                    Lang::I64TruncSatF32U(_) => insn.i64_trunc_sat_f32_u(),
                    Lang::I64TruncSatF64S(_) => insn.i64_trunc_sat_f64_s(),
                    Lang::I64TruncSatF64U(_) => insn.i64_trunc_sat_f64_u(),
                    Lang::I32Popcnt(_) => insn.i32_popcnt(),
                    Lang::I64Popcnt(_) => insn.i64_popcnt(),
                    Lang::I32Extend8S(_) => insn.i32_extend8_s(),
                    Lang::I64Extend8S(_) => insn.i64_extend8_s(),
                    Lang::I32Extend16S(_) => insn.i32_extend16_s(),
                    Lang::I64Extend16S(_) => insn.i64_extend16_s(),
                    Lang::I64Extend32S(_) => insn.i64_extend32_s(),
                    Lang::I64ExtendI32S(_) => insn.i64_extend_i32_s(),
                    Lang::I64ExtendI32U(_) => insn.i64_extend_i32_u(),
                    Lang::F32Add(_) => insn.f32_add(),
                    Lang::F64Add(_) => insn.f64_add(),
                    Lang::F32Sub(_) => insn.f32_sub(),
                    Lang::F64Sub(_) => insn.f64_sub(),
                    Lang::F32Mul(_) => insn.f32_mul(),
                    Lang::F64Mul(_) => insn.f64_mul(),
                    Lang::F32Div(_) => insn.f32_div(),
                    Lang::F64Div(_) => insn.f64_div(),
                    Lang::F32Min(_) => insn.f32_min(),
                    Lang::F64Min(_) => insn.f64_min(),
                    Lang::F32Max(_) => insn.f32_max(),
                    Lang::F64Max(_) => insn.f64_max(),
                    Lang::F32Copysign(_) => insn.f32_copysign(),
                    Lang::F64Copysign(_) => insn.f64_copysign(),
                    Lang::F32Eq(_) => insn.f32_eq(),
                    Lang::F64Eq(_) => insn.f64_eq(),
                    Lang::F32Ne(_) => insn.f32_ne(),
                    Lang::F64Ne(_) => insn.f64_ne(),
                    Lang::F32Lt(_) => insn.f32_lt(),
                    Lang::F64Lt(_) => insn.f64_lt(),
                    Lang::F32Gt(_) => insn.f32_gt(),
                    Lang::F64Gt(_) => insn.f64_gt(),
                    Lang::F32Le(_) => insn.f32_le(),
                    Lang::F64Le(_) => insn.f64_le(),
                    Lang::F32Ge(_) => insn.f32_ge(),
                    Lang::F64Ge(_) => insn.f64_ge(),
                    Lang::I32Store(memarg, _) => insn.i32_store(memarg.into()),
                    Lang::I64Store(memarg, _) => insn.i64_store(memarg.into()),
                    Lang::F32Store(memarg, _) => insn.f32_store(memarg.into()),
                    Lang::F64Store(memarg, _) => insn.f64_store(memarg.into()),
                    Lang::I32Store8(memarg, _) => insn.i32_store8(memarg.into()),
                    Lang::I32Store16(memarg, _) => insn.i32_store16(memarg.into()),
                    Lang::I64Store8(memarg, _) => insn.i64_store8(memarg.into()),
                    Lang::I64Store16(memarg, _) => insn.i64_store16(memarg.into()),
                    Lang::I64Store32(memarg, _) => insn.i64_store32(memarg.into()),
                    Lang::Nop => insn.nop(),
                    Lang::Container(_) => {
                        // Do nothing
                        insn
                    }
                    Lang::Select(_) => insn.select(),
                    Lang::MemoryGrow(mem, _) => insn.memory_grow(*mem),
                    Lang::MemorySize(mem) => insn.memory_size(*mem),
                    Lang::MemoryInit(init, _) => insn.memory_init(init.memory, init.segment),
                    Lang::MemoryCopy(cp, _) => insn.memory_copy(cp.dst, cp.src),
                    Lang::MemoryFill(mem, _) => insn.memory_fill(*mem),
                    Lang::DataDrop(idx) => insn.data_drop(*idx),
                    Lang::TableInit(init, _) => insn.table_init(init.table, init.segment),
                    Lang::TableCopy(cp, _) => insn.table_copy(cp.dst, cp.src),
                    Lang::TableFill(table, _) => insn.table_fill(*table),
                    Lang::ElemDrop(idx) => insn.elem_drop(*idx),
                    Lang::TableGrow(table, _) => insn.table_grow(*table),
                    Lang::TableSize(table) => insn.table_size(*table),
                    Lang::TableGet(table, _) => insn.table_get(*table),
                    Lang::TableSet(table, _) => insn.table_set(*table),
                    Lang::I32UseGlobal(_) => {
                        // Request a new global
                        let request = ResourceRequest::Global {
                            tpe: PrimitiveTypeInfo::I32,
                            mutable: true,
                            shared: false,
                        };
                        resources.push(request);

                        insn.global_set(global_idx);
                        insn.global_get(global_idx);
                        global_idx += 1;
                        insn
                    }
                    Lang::I64UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            tpe: PrimitiveTypeInfo::I64,
                            mutable: true,
                            shared: false,
                        };
                        resources.push(request);

                        insn.global_set(global_idx);
                        insn.global_get(global_idx);
                        global_idx += 1;
                        insn
                    }
                    Lang::F32UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            tpe: PrimitiveTypeInfo::F32,
                            mutable: true,
                            shared: false,
                        };
                        resources.push(request);

                        insn.global_set(global_idx);
                        insn.global_get(global_idx);
                        global_idx += 1;
                        insn
                    }
                    Lang::F64UseGlobal(_) => {
                        let request = ResourceRequest::Global {
                            tpe: PrimitiveTypeInfo::F64,
                            mutable: true,
                            shared: false,
                        };
                        resources.push(request);

                        insn.global_set(global_idx);
                        insn.global_get(global_idx);
                        global_idx += 1;
                        insn
                    }
                    Lang::RefNull(valtype) => insn.ref_null((*valtype).into()),
                    Lang::RefFunc(idx) => insn.ref_func(*idx),
                    Lang::RefIsNull(_) => insn.ref_is_null(),

                    Lang::V128Not(_) => insn.v128_not(),
                    Lang::V128And(_) => insn.v128_and(),
                    Lang::V128AndNot(_) => insn.v128_andnot(),
                    Lang::V128Or(_) => insn.v128_or(),
                    Lang::V128Xor(_) => insn.v128_xor(),
                    Lang::V128AnyTrue(_) => insn.v128_any_true(),
                    Lang::V128Bitselect(_) => insn.v128_bitselect(),

                    Lang::V128Load(memarg, _) => insn.v128_load(memarg.into()),
                    Lang::V128Load8x8S(memarg, _) => insn.v128_load8x8_s(memarg.into()),
                    Lang::V128Load8x8U(memarg, _) => insn.v128_load8x8_u(memarg.into()),
                    Lang::V128Load16x4S(memarg, _) => insn.v128_load16x4_s(memarg.into()),
                    Lang::V128Load16x4U(memarg, _) => insn.v128_load16x4_u(memarg.into()),
                    Lang::V128Load32x2S(memarg, _) => insn.v128_load32x2_s(memarg.into()),
                    Lang::V128Load32x2U(memarg, _) => insn.v128_load32x2_u(memarg.into()),
                    Lang::V128Load8Splat(memarg, _) => insn.v128_load8_splat(memarg.into()),
                    Lang::V128Load16Splat(memarg, _) => insn.v128_load16_splat(memarg.into()),
                    Lang::V128Load32Splat(memarg, _) => insn.v128_load32_splat(memarg.into()),
                    Lang::V128Load64Splat(memarg, _) => insn.v128_load64_splat(memarg.into()),
                    Lang::V128Load32Zero(memarg, _) => insn.v128_load32_zero(memarg.into()),
                    Lang::V128Load64Zero(memarg, _) => insn.v128_load64_zero(memarg.into()),
                    Lang::V128Store(memarg, _) => insn.v128_store(memarg.into()),
                    Lang::V128Load8Lane(memarg, _) => {
                        insn.v128_load8_lane((&memarg.memarg).into(), memarg.lane)
                    }
                    Lang::V128Load16Lane(memarg, _) => {
                        insn.v128_load16_lane((&memarg.memarg).into(), memarg.lane)
                    }
                    Lang::V128Load32Lane(memarg, _) => {
                        insn.v128_load32_lane((&memarg.memarg).into(), memarg.lane)
                    }
                    Lang::V128Load64Lane(memarg, _) => {
                        insn.v128_load64_lane((&memarg.memarg).into(), memarg.lane)
                    }
                    Lang::V128Store8Lane(memarg, _) => {
                        insn.v128_store8_lane((&memarg.memarg).into(), memarg.lane)
                    }
                    Lang::V128Store16Lane(memarg, _) => {
                        insn.v128_store16_lane((&memarg.memarg).into(), memarg.lane)
                    }
                    Lang::V128Store32Lane(memarg, _) => {
                        insn.v128_store32_lane((&memarg.memarg).into(), memarg.lane)
                    }
                    Lang::V128Store64Lane(memarg, _) => {
                        insn.v128_store64_lane((&memarg.memarg).into(), memarg.lane)
                    }

                    Lang::I8x16ExtractLaneS(lane, _) => insn.i8x16_extract_lane_s(*lane),
                    Lang::I8x16ExtractLaneU(lane, _) => insn.i8x16_extract_lane_u(*lane),
                    Lang::I8x16ReplaceLane(lane, _) => insn.i8x16_replace_lane(*lane),
                    Lang::I16x8ExtractLaneS(lane, _) => insn.i16x8_extract_lane_s(*lane),
                    Lang::I16x8ExtractLaneU(lane, _) => insn.i16x8_extract_lane_u(*lane),
                    Lang::I16x8ReplaceLane(lane, _) => insn.i16x8_replace_lane(*lane),
                    Lang::I32x4ExtractLane(lane, _) => insn.i32x4_extract_lane(*lane),
                    Lang::I32x4ReplaceLane(lane, _) => insn.i32x4_replace_lane(*lane),
                    Lang::I64x2ExtractLane(lane, _) => insn.i64x2_extract_lane(*lane),
                    Lang::I64x2ReplaceLane(lane, _) => insn.i64x2_replace_lane(*lane),
                    Lang::F32x4ExtractLane(lane, _) => insn.f32x4_extract_lane(*lane),
                    Lang::F32x4ReplaceLane(lane, _) => insn.f32x4_replace_lane(*lane),
                    Lang::F64x2ExtractLane(lane, _) => insn.f64x2_extract_lane(*lane),
                    Lang::F64x2ReplaceLane(lane, _) => insn.f64x2_replace_lane(*lane),

                    Lang::I8x16Swizzle(_) => insn.i8x16_swizzle(),
                    Lang::I8x16Shuffle(indices, _) => insn.i8x16_shuffle(indices.indices),
                    Lang::I8x16Splat(_) => insn.i8x16_splat(),
                    Lang::I16x8Splat(_) => insn.i16x8_splat(),
                    Lang::I32x4Splat(_) => insn.i32x4_splat(),
                    Lang::I64x2Splat(_) => insn.i64x2_splat(),
                    Lang::F32x4Splat(_) => insn.f32x4_splat(),
                    Lang::F64x2Splat(_) => insn.f64x2_splat(),

                    Lang::I8x16Eq(_) => insn.i8x16_eq(),
                    Lang::I8x16Ne(_) => insn.i8x16_ne(),
                    Lang::I8x16LtS(_) => insn.i8x16_lt_s(),
                    Lang::I8x16LtU(_) => insn.i8x16_lt_u(),
                    Lang::I8x16GtS(_) => insn.i8x16_gt_s(),
                    Lang::I8x16GtU(_) => insn.i8x16_gt_u(),
                    Lang::I8x16LeS(_) => insn.i8x16_le_s(),
                    Lang::I8x16LeU(_) => insn.i8x16_le_u(),
                    Lang::I8x16GeS(_) => insn.i8x16_ge_s(),
                    Lang::I8x16GeU(_) => insn.i8x16_ge_u(),
                    Lang::I16x8Eq(_) => insn.i16x8_eq(),
                    Lang::I16x8Ne(_) => insn.i16x8_ne(),
                    Lang::I16x8LtS(_) => insn.i16x8_lt_s(),
                    Lang::I16x8LtU(_) => insn.i16x8_lt_u(),
                    Lang::I16x8GtS(_) => insn.i16x8_gt_s(),
                    Lang::I16x8GtU(_) => insn.i16x8_gt_u(),
                    Lang::I16x8LeS(_) => insn.i16x8_le_s(),
                    Lang::I16x8LeU(_) => insn.i16x8_le_u(),
                    Lang::I16x8GeS(_) => insn.i16x8_ge_s(),
                    Lang::I16x8GeU(_) => insn.i16x8_ge_u(),
                    Lang::I32x4Eq(_) => insn.i32x4_eq(),
                    Lang::I32x4Ne(_) => insn.i32x4_ne(),
                    Lang::I32x4LtS(_) => insn.i32x4_lt_s(),
                    Lang::I32x4LtU(_) => insn.i32x4_lt_u(),
                    Lang::I32x4GtS(_) => insn.i32x4_gt_s(),
                    Lang::I32x4GtU(_) => insn.i32x4_gt_u(),
                    Lang::I32x4LeS(_) => insn.i32x4_le_s(),
                    Lang::I32x4LeU(_) => insn.i32x4_le_u(),
                    Lang::I32x4GeS(_) => insn.i32x4_ge_s(),
                    Lang::I32x4GeU(_) => insn.i32x4_ge_u(),
                    Lang::I64x2Eq(_) => insn.i64x2_eq(),
                    Lang::I64x2Ne(_) => insn.i64x2_ne(),
                    Lang::I64x2LtS(_) => insn.i64x2_lt_s(),
                    Lang::I64x2GtS(_) => insn.i64x2_gt_s(),
                    Lang::I64x2LeS(_) => insn.i64x2_le_s(),
                    Lang::I64x2GeS(_) => insn.i64x2_ge_s(),
                    Lang::F32x4Eq(_) => insn.f32x4_eq(),
                    Lang::F32x4Ne(_) => insn.f32x4_ne(),
                    Lang::F32x4Lt(_) => insn.f32x4_lt(),
                    Lang::F32x4Gt(_) => insn.f32x4_gt(),
                    Lang::F32x4Le(_) => insn.f32x4_le(),
                    Lang::F32x4Ge(_) => insn.f32x4_ge(),
                    Lang::F64x2Eq(_) => insn.f64x2_eq(),
                    Lang::F64x2Ne(_) => insn.f64x2_ne(),
                    Lang::F64x2Lt(_) => insn.f64x2_lt(),
                    Lang::F64x2Gt(_) => insn.f64x2_gt(),
                    Lang::F64x2Le(_) => insn.f64x2_le(),
                    Lang::F64x2Ge(_) => insn.f64x2_ge(),

                    Lang::I8x16Abs(_) => insn.i8x16_abs(),
                    Lang::I8x16Neg(_) => insn.i8x16_neg(),
                    Lang::I8x16Popcnt(_) => insn.i8x16_popcnt(),
                    Lang::I8x16AllTrue(_) => insn.i8x16_all_true(),
                    Lang::I8x16Bitmask(_) => insn.i8x16_bitmask(),
                    Lang::I8x16NarrowI16x8S(_) => insn.i8x16_narrow_i16x8_s(),
                    Lang::I8x16NarrowI16x8U(_) => insn.i8x16_narrow_i16x8_u(),
                    Lang::I8x16Shl(_) => insn.i8x16_shl(),
                    Lang::I8x16ShrS(_) => insn.i8x16_shr_s(),
                    Lang::I8x16ShrU(_) => insn.i8x16_shr_u(),
                    Lang::I8x16Add(_) => insn.i8x16_add(),
                    Lang::I8x16AddSatS(_) => insn.i8x16_add_sat_s(),
                    Lang::I8x16AddSatU(_) => insn.i8x16_add_sat_u(),
                    Lang::I8x16Sub(_) => insn.i8x16_sub(),
                    Lang::I8x16SubSatS(_) => insn.i8x16_sub_sat_s(),
                    Lang::I8x16SubSatU(_) => insn.i8x16_sub_sat_u(),
                    Lang::I8x16MinS(_) => insn.i8x16_min_s(),
                    Lang::I8x16MinU(_) => insn.i8x16_min_u(),
                    Lang::I8x16MaxS(_) => insn.i8x16_max_s(),
                    Lang::I8x16MaxU(_) => insn.i8x16_max_u(),
                    Lang::I8x16AvgrU(_) => insn.i8x16_avgr_u(),

                    Lang::I16x8ExtAddPairwiseI8x16S(_) => insn.i16x8_extadd_pairwise_i8x16_s(),
                    Lang::I16x8ExtAddPairwiseI8x16U(_) => insn.i16x8_extadd_pairwise_i8x16_u(),
                    Lang::I16x8Abs(_) => insn.i16x8_abs(),
                    Lang::I16x8Neg(_) => insn.i16x8_neg(),
                    Lang::I16x8Q15MulrSatS(_) => insn.i16x8_q15mulr_sat_s(),
                    Lang::I16x8AllTrue(_) => insn.i16x8_all_true(),
                    Lang::I16x8Bitmask(_) => insn.i16x8_bitmask(),
                    Lang::I16x8NarrowI32x4S(_) => insn.i16x8_narrow_i32x4_s(),
                    Lang::I16x8NarrowI32x4U(_) => insn.i16x8_narrow_i32x4_u(),
                    Lang::I16x8ExtendLowI8x16S(_) => insn.i16x8_extend_low_i8x16_s(),
                    Lang::I16x8ExtendHighI8x16S(_) => insn.i16x8_extend_high_i8x16_s(),
                    Lang::I16x8ExtendLowI8x16U(_) => insn.i16x8_extend_low_i8x16_u(),
                    Lang::I16x8ExtendHighI8x16U(_) => insn.i16x8_extend_high_i8x16_u(),
                    Lang::I16x8Shl(_) => insn.i16x8_shl(),
                    Lang::I16x8ShrS(_) => insn.i16x8_shr_s(),
                    Lang::I16x8ShrU(_) => insn.i16x8_shr_u(),
                    Lang::I16x8Add(_) => insn.i16x8_add(),
                    Lang::I16x8AddSatS(_) => insn.i16x8_add_sat_s(),
                    Lang::I16x8AddSatU(_) => insn.i16x8_add_sat_u(),
                    Lang::I16x8Sub(_) => insn.i16x8_sub(),
                    Lang::I16x8SubSatS(_) => insn.i16x8_sub_sat_s(),
                    Lang::I16x8SubSatU(_) => insn.i16x8_sub_sat_u(),
                    Lang::I16x8Mul(_) => insn.i16x8_mul(),
                    Lang::I16x8MinS(_) => insn.i16x8_min_s(),
                    Lang::I16x8MinU(_) => insn.i16x8_min_u(),
                    Lang::I16x8MaxS(_) => insn.i16x8_max_s(),
                    Lang::I16x8MaxU(_) => insn.i16x8_max_u(),
                    Lang::I16x8AvgrU(_) => insn.i16x8_avgr_u(),
                    Lang::I16x8ExtMulLowI8x16S(_) => insn.i16x8_extmul_low_i8x16_s(),
                    Lang::I16x8ExtMulHighI8x16S(_) => insn.i16x8_extmul_high_i8x16_s(),
                    Lang::I16x8ExtMulLowI8x16U(_) => insn.i16x8_extmul_low_i8x16_u(),
                    Lang::I16x8ExtMulHighI8x16U(_) => insn.i16x8_extmul_high_i8x16_u(),

                    Lang::I32x4ExtAddPairwiseI16x8S(_) => insn.i32x4_extadd_pairwise_i16x8_s(),
                    Lang::I32x4ExtAddPairwiseI16x8U(_) => insn.i32x4_extadd_pairwise_i16x8_u(),
                    Lang::I32x4Abs(_) => insn.i32x4_abs(),
                    Lang::I32x4Neg(_) => insn.i32x4_neg(),
                    Lang::I32x4AllTrue(_) => insn.i32x4_all_true(),
                    Lang::I32x4Bitmask(_) => insn.i32x4_bitmask(),
                    Lang::I32x4ExtendLowI16x8S(_) => insn.i32x4_extend_low_i16x8_s(),
                    Lang::I32x4ExtendHighI16x8S(_) => insn.i32x4_extend_high_i16x8_s(),
                    Lang::I32x4ExtendLowI16x8U(_) => insn.i32x4_extend_low_i16x8_u(),
                    Lang::I32x4ExtendHighI16x8U(_) => insn.i32x4_extend_high_i16x8_u(),
                    Lang::I32x4Shl(_) => insn.i32x4_shl(),
                    Lang::I32x4ShrS(_) => insn.i32x4_shr_s(),
                    Lang::I32x4ShrU(_) => insn.i32x4_shr_u(),
                    Lang::I32x4Add(_) => insn.i32x4_add(),
                    Lang::I32x4Sub(_) => insn.i32x4_sub(),
                    Lang::I32x4Mul(_) => insn.i32x4_mul(),
                    Lang::I32x4MinS(_) => insn.i32x4_min_s(),
                    Lang::I32x4MinU(_) => insn.i32x4_min_u(),
                    Lang::I32x4MaxS(_) => insn.i32x4_max_s(),
                    Lang::I32x4MaxU(_) => insn.i32x4_max_u(),
                    Lang::I32x4DotI16x8S(_) => insn.i32x4_dot_i16x8_s(),
                    Lang::I32x4ExtMulLowI16x8S(_) => insn.i32x4_extmul_low_i16x8_s(),
                    Lang::I32x4ExtMulHighI16x8S(_) => insn.i32x4_extmul_high_i16x8_s(),
                    Lang::I32x4ExtMulLowI16x8U(_) => insn.i32x4_extmul_low_i16x8_u(),
                    Lang::I32x4ExtMulHighI16x8U(_) => insn.i32x4_extmul_high_i16x8_u(),

                    Lang::I64x2Abs(_) => insn.i64x2_abs(),
                    Lang::I64x2Neg(_) => insn.i64x2_neg(),
                    Lang::I64x2AllTrue(_) => insn.i64x2_all_true(),
                    Lang::I64x2Bitmask(_) => insn.i64x2_bitmask(),
                    Lang::I64x2ExtendLowI32x4S(_) => insn.i64x2_extend_low_i32x4_s(),
                    Lang::I64x2ExtendHighI32x4S(_) => insn.i64x2_extend_high_i32x4_s(),
                    Lang::I64x2ExtendLowI32x4U(_) => insn.i64x2_extend_low_i32x4_u(),
                    Lang::I64x2ExtendHighI32x4U(_) => insn.i64x2_extend_high_i32x4_u(),
                    Lang::I64x2Shl(_) => insn.i64x2_shl(),
                    Lang::I64x2ShrS(_) => insn.i64x2_shr_s(),
                    Lang::I64x2ShrU(_) => insn.i64x2_shr_u(),
                    Lang::I64x2Add(_) => insn.i64x2_add(),
                    Lang::I64x2Sub(_) => insn.i64x2_sub(),
                    Lang::I64x2Mul(_) => insn.i64x2_mul(),
                    Lang::I64x2ExtMulLowI32x4S(_) => insn.i64x2_extmul_low_i32x4_s(),
                    Lang::I64x2ExtMulHighI32x4S(_) => insn.i64x2_extmul_high_i32x4_s(),
                    Lang::I64x2ExtMulLowI32x4U(_) => insn.i64x2_extmul_low_i32x4_u(),
                    Lang::I64x2ExtMulHighI32x4U(_) => insn.i64x2_extmul_high_i32x4_u(),

                    Lang::F32x4Ceil(_) => insn.f32x4_ceil(),
                    Lang::F32x4Floor(_) => insn.f32x4_floor(),
                    Lang::F32x4Trunc(_) => insn.f32x4_trunc(),
                    Lang::F32x4Nearest(_) => insn.f32x4_nearest(),
                    Lang::F32x4Abs(_) => insn.f32x4_abs(),
                    Lang::F32x4Neg(_) => insn.f32x4_neg(),
                    Lang::F32x4Sqrt(_) => insn.f32x4_sqrt(),
                    Lang::F32x4Add(_) => insn.f32x4_add(),
                    Lang::F32x4Sub(_) => insn.f32x4_sub(),
                    Lang::F32x4Mul(_) => insn.f32x4_mul(),
                    Lang::F32x4Div(_) => insn.f32x4_div(),
                    Lang::F32x4Min(_) => insn.f32x4_min(),
                    Lang::F32x4Max(_) => insn.f32x4_max(),
                    Lang::F32x4PMin(_) => insn.f32x4_pmin(),
                    Lang::F32x4PMax(_) => insn.f32x4_pmax(),
                    Lang::F64x2Ceil(_) => insn.f64x2_ceil(),
                    Lang::F64x2Floor(_) => insn.f64x2_floor(),
                    Lang::F64x2Trunc(_) => insn.f64x2_trunc(),
                    Lang::F64x2Nearest(_) => insn.f64x2_nearest(),
                    Lang::F64x2Abs(_) => insn.f64x2_abs(),
                    Lang::F64x2Neg(_) => insn.f64x2_neg(),
                    Lang::F64x2Sqrt(_) => insn.f64x2_sqrt(),
                    Lang::F64x2Add(_) => insn.f64x2_add(),
                    Lang::F64x2Sub(_) => insn.f64x2_sub(),
                    Lang::F64x2Mul(_) => insn.f64x2_mul(),
                    Lang::F64x2Div(_) => insn.f64x2_div(),
                    Lang::F64x2Min(_) => insn.f64x2_min(),
                    Lang::F64x2Max(_) => insn.f64x2_max(),
                    Lang::F64x2PMin(_) => insn.f64x2_pmin(),
                    Lang::F64x2PMax(_) => insn.f64x2_pmax(),

                    Lang::I32x4TruncSatF32x4S(_) => insn.i32x4_trunc_sat_f32x4_s(),
                    Lang::I32x4TruncSatF32x4U(_) => insn.i32x4_trunc_sat_f32x4_u(),
                    Lang::F32x4ConvertI32x4S(_) => insn.f32x4_convert_i32x4_s(),
                    Lang::F32x4ConvertI32x4U(_) => insn.f32x4_convert_i32x4_u(),
                    Lang::I32x4TruncSatF64x2SZero(_) => insn.i32x4_trunc_sat_f64x2_s_zero(),
                    Lang::I32x4TruncSatF64x2UZero(_) => insn.i32x4_trunc_sat_f64x2_u_zero(),
                    Lang::F64x2ConvertLowI32x4S(_) => insn.f64x2_convert_low_i32x4_s(),
                    Lang::F64x2ConvertLowI32x4U(_) => insn.f64x2_convert_low_i32x4_u(),
                    Lang::F32x4DemoteF64x2Zero(_) => insn.f32x4_demote_f64x2_zero(),
                    Lang::F64x2PromoteLowF32x4(_) => insn.f64x2_promote_low_f32x4(),

                    Lang::I8x16RelaxedSwizzle(_) => insn.i8x16_relaxed_swizzle(),
                    Lang::I32x4RelaxedTruncF32x4S(_) => insn.i32x4_relaxed_trunc_f32x4_s(),
                    Lang::I32x4RelaxedTruncF32x4U(_) => insn.i32x4_relaxed_trunc_f32x4_u(),
                    Lang::I32x4RelaxedTruncF64x2SZero(_) => insn.i32x4_relaxed_trunc_f64x2_s_zero(),
                    Lang::I32x4RelaxedTruncF64x2UZero(_) => insn.i32x4_relaxed_trunc_f64x2_u_zero(),
                    Lang::F32x4RelaxedMadd(_) => insn.f32x4_relaxed_madd(),
                    Lang::F32x4RelaxedNmadd(_) => insn.f32x4_relaxed_nmadd(),
                    Lang::F64x2RelaxedMadd(_) => insn.f64x2_relaxed_madd(),
                    Lang::F64x2RelaxedNmadd(_) => insn.f64x2_relaxed_nmadd(),
                    Lang::I8x16RelaxedLaneselect(_) => insn.i8x16_relaxed_laneselect(),
                    Lang::I16x8RelaxedLaneselect(_) => insn.i16x8_relaxed_laneselect(),
                    Lang::I32x4RelaxedLaneselect(_) => insn.i32x4_relaxed_laneselect(),
                    Lang::I64x2RelaxedLaneselect(_) => insn.i64x2_relaxed_laneselect(),
                    Lang::F32x4RelaxedMin(_) => insn.f32x4_relaxed_min(),
                    Lang::F32x4RelaxedMax(_) => insn.f32x4_relaxed_max(),
                    Lang::F64x2RelaxedMin(_) => insn.f64x2_relaxed_min(),
                    Lang::F64x2RelaxedMax(_) => insn.f64x2_relaxed_max(),
                    Lang::I16x8RelaxedQ15mulrS(_) => insn.i16x8_relaxed_q15mulr_s(),
                    Lang::I16x8RelaxedDotI8x16I7x16S(_) => insn.i16x8_relaxed_dot_i8x16_i7x16_s(),
                    Lang::I32x4RelaxedDotI8x16I7x16AddS(_) => {
                        insn.i32x4_relaxed_dot_i8x16_i7x16_add_s()
                    }
                };
            }
        }
    }
    Ok(resources)
}

impl From<&MemArg> for wasm_encoder::MemArg {
    fn from(arg: &MemArg) -> wasm_encoder::MemArg {
        wasm_encoder::MemArg {
            offset: arg.static_offset,
            align: arg.align.into(),
            memory_index: arg.mem,
        }
    }
}
