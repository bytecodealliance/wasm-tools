#[macro_use]
extern crate criterion;

use anyhow::Result;
use criterion::Criterion;
use once_cell::unsync::Lazy;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use wasmparser::{
    BlockType, BrTable, DataKind, ElementKind, Ieee32, Ieee64, MemoryImmediate, Parser, Payload,
    SIMDLaneIndex, ValType, Validator, VisitOperator, WasmFeatures, V128,
};

/// A benchmark input.
pub struct BenchmarkInput {
    /// The path to the benchmark file important for handling errors.
    pub path: PathBuf,
    /// The encoded Wasm module that is run by the benchmark.
    pub wasm: Vec<u8>,
}

impl BenchmarkInput {
    /// Creates a new benchmark input.
    pub fn new(test_path: PathBuf, encoded_wasm: Vec<u8>) -> Self {
        Self {
            path: test_path,
            wasm: encoded_wasm,
        }
    }
}

/// Returns a vector of all found benchmark input files under the given directory.
///
/// Benchmark input files can be `.wat` or `.wast` formatted files.
/// For `.wast` files we pull out all the module directives and run them in the benchmarks.
fn collect_test_files(path: &Path, list: &mut Vec<BenchmarkInput>) -> Result<()> {
    for entry in path.read_dir()? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_test_files(&path, list)?;
            continue;
        }
        match path.extension().and_then(|ext| ext.to_str()) {
            Some("wasm") => {
                let wasm = fs::read(&path)?;
                list.push(BenchmarkInput::new(path, wasm));
            }
            Some("wat") | Some("txt") => {
                if let Ok(wasm) = wat::parse_file(&path) {
                    list.push(BenchmarkInput::new(path, wasm));
                }
            }
            Some("wast") => {
                let contents = fs::read_to_string(&path)?;
                let buf = match wast::parser::ParseBuffer::new(&contents) {
                    Ok(buf) => buf,
                    Err(_) => continue,
                };
                let wast: wast::Wast<'_> = match wast::parser::parse(&buf) {
                    Ok(wast) => wast,
                    Err(_) => continue,
                };
                for directive in wast.directives {
                    match directive {
                        wast::WastDirective::Wat(mut module) => {
                            let wasm = module.encode()?;
                            list.push(BenchmarkInput::new(path.clone(), wasm));
                        }
                        _ => continue,
                    }
                }
            }
            _ => (),
        }
    }
    Ok(())
}

/// Reads the input given the Wasm parser or validator.
///
/// The `path` specifies which benchmark input file we are currently operating on
/// so that we can report better errors in case of failures.
fn read_all_wasm(wasm: &[u8]) -> Result<()> {
    use Payload::*;
    for item in Parser::new(0).parse_all(wasm) {
        match item? {
            TypeSection(s) => {
                for item in s {
                    item?;
                }
            }
            ImportSection(s) => {
                for item in s {
                    item?;
                }
            }
            FunctionSection(s) => {
                for item in s {
                    item?;
                }
            }
            TableSection(s) => {
                for item in s {
                    item?;
                }
            }
            MemorySection(s) => {
                for item in s {
                    item?;
                }
            }
            TagSection(s) => {
                for item in s {
                    item?;
                }
            }
            GlobalSection(s) => {
                for item in s {
                    for op in item?.init_expr.get_operators_reader() {
                        op?;
                    }
                }
            }
            ExportSection(s) => {
                for item in s {
                    item?;
                }
            }
            ElementSection(s) => {
                for item in s {
                    let item = item?;
                    if let ElementKind::Active { offset_expr, .. } = item.kind {
                        for op in offset_expr.get_operators_reader() {
                            op?;
                        }
                    }
                    for op in item.items.get_items_reader()? {
                        op?;
                    }
                }
            }
            DataSection(s) => {
                for item in s {
                    let item = item?;
                    if let DataKind::Active { offset_expr, .. } = item.kind {
                        for op in offset_expr.get_operators_reader() {
                            op?;
                        }
                    }
                }
            }
            CodeSectionEntry(body) => {
                let mut reader = body.get_binary_reader();
                for _ in 0..reader.read_var_u32()? {
                    reader.read_var_u32()?;
                    reader.read_val_type()?;
                }
                while !reader.eof() {
                    reader.visit_operator(&mut NopVisit)?;
                }
            }

            // Component sections
            ModuleSection { .. } => {}
            InstanceSection(s) => {
                for item in s {
                    item?;
                }
            }
            AliasSection(s) => {
                for item in s {
                    item?;
                }
            }
            CoreTypeSection(s) => {
                for item in s {
                    item?;
                }
            }
            ComponentSection { .. } => {}
            ComponentInstanceSection(s) => {
                for item in s {
                    item?;
                }
            }
            ComponentAliasSection(s) => {
                for item in s {
                    item?;
                }
            }
            ComponentTypeSection(s) => {
                for item in s {
                    item?;
                }
            }
            ComponentCanonicalSection(s) => {
                for item in s {
                    item?;
                }
            }
            ComponentStartSection { .. } => {}
            ComponentImportSection(s) => {
                for item in s {
                    item?;
                }
            }
            ComponentExportSection(s) => {
                for item in s {
                    item?;
                }
            }

            Version { .. }
            | StartSection { .. }
            | DataCountSection { .. }
            | UnknownSection { .. }
            | CustomSection { .. }
            | CodeSectionStart { .. }
            | End(_) => {}
        }
    }
    Ok(())
}

/// Returns the default benchmark inputs that are proper `wasmparser` benchmark
/// test inputs.
fn collect_benchmark_inputs() -> Vec<BenchmarkInput> {
    let mut ret = Vec::new();
    collect_test_files("../../tests".as_ref(), &mut ret).unwrap();
    // Sort to ideally get more deterministic perf that ignores filesystems
    ret.sort_by_key(|p| p.path.clone());
    ret
}

fn define_benchmarks(c: &mut Criterion) {
    fn validator() -> Validator {
        Validator::new_with_features(WasmFeatures {
            reference_types: true,
            multi_value: true,
            simd: true,
            relaxed_simd: true,
            exceptions: true,
            component_model: true,
            bulk_memory: true,
            threads: true,
            tail_call: true,
            multi_memory: true,
            memory64: true,
            extended_const: true,
            deterministic_only: false,
            mutable_global: true,
            saturating_float_to_int: true,
            sign_extension: true,
        })
    }

    let test_inputs = once_cell::unsync::Lazy::new(collect_benchmark_inputs);

    let parse_inputs = once_cell::unsync::Lazy::new(|| {
        let mut list = Vec::new();
        for input in test_inputs.iter() {
            if read_all_wasm(&input.wasm).is_ok() {
                list.push(&input.wasm);
            }
        }
        list
    });
    c.bench_function("parse/tests", |b| {
        Lazy::force(&parse_inputs);
        b.iter(|| {
            for wasm in parse_inputs.iter() {
                read_all_wasm(wasm).unwrap();
            }
        })
    });

    let validate_inputs = once_cell::unsync::Lazy::new(|| {
        let mut list = Vec::new();
        for input in test_inputs.iter() {
            if validator().validate_all(&input.wasm).is_ok() {
                list.push(&input.wasm);
            }
        }
        list
    });
    c.bench_function("validate/tests", |b| {
        Lazy::force(&validate_inputs);
        b.iter(|| {
            for wasm in validate_inputs.iter() {
                validator().validate_all(wasm).unwrap();
            }
        })
    });

    for file in std::fs::read_dir("benches").unwrap() {
        let file = file.unwrap();
        let path = file.path();
        if path.extension().and_then(|s| s.to_str()) != Some("wasm") {
            continue;
        }
        let name = path.file_stem().unwrap().to_str().unwrap();
        let wasm = Lazy::new(|| std::fs::read(&path).unwrap());
        c.bench_function(&format!("validate/{name}"), |b| {
            Lazy::force(&wasm);
            b.iter(|| {
                validator().validate_all(&wasm).unwrap();
            })
        });
        c.bench_function(&format!("parse/{name}"), |b| {
            Lazy::force(&wasm);
            b.iter(|| {
                read_all_wasm(&wasm).unwrap();
            })
        });
    }
}

criterion_group!(benchmark, define_benchmarks);
criterion_main!(benchmark);

struct NopVisit;

#[allow(unused_variables)]
impl<'a> VisitOperator<'a> for NopVisit {
    type Output = ();

    fn visit_nop(&mut self, offset: usize) {}
    fn visit_unreachable(&mut self, offset: usize) {}
    fn visit_block(&mut self, offset: usize, ty: BlockType) {}
    fn visit_loop(&mut self, offset: usize, ty: BlockType) {}
    fn visit_if(&mut self, offset: usize, ty: BlockType) {}
    fn visit_else(&mut self, offset: usize) {}
    fn visit_try(&mut self, offset: usize, ty: BlockType) {}
    fn visit_catch(&mut self, offset: usize, index: u32) {}
    fn visit_throw(&mut self, offset: usize, index: u32) {}
    fn visit_rethrow(&mut self, offset: usize, relative_depth: u32) {}
    fn visit_delegate(&mut self, offset: usize, relative_depth: u32) {}
    fn visit_catch_all(&mut self, offset: usize) {}
    fn visit_end(&mut self, offset: usize) {}
    fn visit_br(&mut self, offset: usize, relative_depth: u32) {}
    fn visit_br_if(&mut self, offset: usize, relative_depth: u32) {}
    fn visit_br_table(&mut self, offset: usize, table: &BrTable<'a>) {
        for target in table.targets() {
            target.unwrap();
        }
    }
    fn visit_return(&mut self, offset: usize) {}
    fn visit_call(&mut self, offset: usize, function_index: u32) {}
    fn visit_return_call(&mut self, offset: usize, function_index: u32) {}
    fn visit_call_indirect(&mut self, offset: usize, index: u32, table_index: u32, table_byte: u8) {
    }
    fn visit_return_call_indirect(&mut self, offset: usize, index: u32, table_index: u32) {}
    fn visit_drop(&mut self, offset: usize) {}
    fn visit_select(&mut self, offset: usize) {}
    fn visit_typed_select(&mut self, offset: usize, ty: ValType) {}
    fn visit_local_get(&mut self, offset: usize, local_index: u32) {}
    fn visit_local_set(&mut self, offset: usize, local_index: u32) {}
    fn visit_local_tee(&mut self, offset: usize, local_index: u32) {}
    fn visit_global_get(&mut self, offset: usize, global_index: u32) {}
    fn visit_global_set(&mut self, offset: usize, global_index: u32) {}
    fn visit_i32_load(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_load(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_f32_load(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_f64_load(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_load8_s(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_load16_s(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_load32_s(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_load32_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_store(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_store(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_f32_store(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_f64_store(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_store8(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_store16(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_store8(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_store16(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_store32(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_memory_size(&mut self, offset: usize, mem: u32, mem_byte: u8) {}
    fn visit_memory_grow(&mut self, offset: usize, mem: u32, mem_byte: u8) {}
    fn visit_i32_const(&mut self, offset: usize, value: i32) {}
    fn visit_i64_const(&mut self, offset: usize, value: i64) {}
    fn visit_f32_const(&mut self, offset: usize, value: Ieee32) {}
    fn visit_f64_const(&mut self, offset: usize, value: Ieee64) {}
    fn visit_i32_eqz(&mut self, offset: usize) {}
    fn visit_i32_eq(&mut self, offset: usize) {}
    fn visit_i32_ne(&mut self, offset: usize) {}
    fn visit_i32_lt_s(&mut self, offset: usize) {}
    fn visit_i32_lt_u(&mut self, offset: usize) {}
    fn visit_i32_gt_s(&mut self, offset: usize) {}
    fn visit_i32_gt_u(&mut self, offset: usize) {}
    fn visit_i32_le_s(&mut self, offset: usize) {}
    fn visit_i32_le_u(&mut self, offset: usize) {}
    fn visit_i32_ge_s(&mut self, offset: usize) {}
    fn visit_i32_ge_u(&mut self, offset: usize) {}
    fn visit_i64_eqz(&mut self, offset: usize) {}
    fn visit_i64_eq(&mut self, offset: usize) {}
    fn visit_i64_ne(&mut self, offset: usize) {}
    fn visit_i64_lt_s(&mut self, offset: usize) {}
    fn visit_i64_lt_u(&mut self, offset: usize) {}
    fn visit_i64_gt_s(&mut self, offset: usize) {}
    fn visit_i64_gt_u(&mut self, offset: usize) {}
    fn visit_i64_le_s(&mut self, offset: usize) {}
    fn visit_i64_le_u(&mut self, offset: usize) {}
    fn visit_i64_ge_s(&mut self, offset: usize) {}
    fn visit_i64_ge_u(&mut self, offset: usize) {}
    fn visit_f32_eq(&mut self, offset: usize) {}
    fn visit_f32_ne(&mut self, offset: usize) {}
    fn visit_f32_lt(&mut self, offset: usize) {}
    fn visit_f32_gt(&mut self, offset: usize) {}
    fn visit_f32_le(&mut self, offset: usize) {}
    fn visit_f32_ge(&mut self, offset: usize) {}
    fn visit_f64_eq(&mut self, offset: usize) {}
    fn visit_f64_ne(&mut self, offset: usize) {}
    fn visit_f64_lt(&mut self, offset: usize) {}
    fn visit_f64_gt(&mut self, offset: usize) {}
    fn visit_f64_le(&mut self, offset: usize) {}
    fn visit_f64_ge(&mut self, offset: usize) {}
    fn visit_i32_clz(&mut self, offset: usize) {}
    fn visit_i32_ctz(&mut self, offset: usize) {}
    fn visit_i32_popcnt(&mut self, offset: usize) {}
    fn visit_i32_add(&mut self, offset: usize) {}
    fn visit_i32_sub(&mut self, offset: usize) {}
    fn visit_i32_mul(&mut self, offset: usize) {}
    fn visit_i32_div_s(&mut self, offset: usize) {}
    fn visit_i32_div_u(&mut self, offset: usize) {}
    fn visit_i32_rem_s(&mut self, offset: usize) {}
    fn visit_i32_rem_u(&mut self, offset: usize) {}
    fn visit_i32_and(&mut self, offset: usize) {}
    fn visit_i32_or(&mut self, offset: usize) {}
    fn visit_i32_xor(&mut self, offset: usize) {}
    fn visit_i32_shl(&mut self, offset: usize) {}
    fn visit_i32_shr_s(&mut self, offset: usize) {}
    fn visit_i32_shr_u(&mut self, offset: usize) {}
    fn visit_i32_rotl(&mut self, offset: usize) {}
    fn visit_i32_rotr(&mut self, offset: usize) {}
    fn visit_i64_clz(&mut self, offset: usize) {}
    fn visit_i64_ctz(&mut self, offset: usize) {}
    fn visit_i64_popcnt(&mut self, offset: usize) {}
    fn visit_i64_add(&mut self, offset: usize) {}
    fn visit_i64_sub(&mut self, offset: usize) {}
    fn visit_i64_mul(&mut self, offset: usize) {}
    fn visit_i64_div_s(&mut self, offset: usize) {}
    fn visit_i64_div_u(&mut self, offset: usize) {}
    fn visit_i64_rem_s(&mut self, offset: usize) {}
    fn visit_i64_rem_u(&mut self, offset: usize) {}
    fn visit_i64_and(&mut self, offset: usize) {}
    fn visit_i64_or(&mut self, offset: usize) {}
    fn visit_i64_xor(&mut self, offset: usize) {}
    fn visit_i64_shl(&mut self, offset: usize) {}
    fn visit_i64_shr_s(&mut self, offset: usize) {}
    fn visit_i64_shr_u(&mut self, offset: usize) {}
    fn visit_i64_rotl(&mut self, offset: usize) {}
    fn visit_i64_rotr(&mut self, offset: usize) {}
    fn visit_f32_abs(&mut self, offset: usize) {}
    fn visit_f32_neg(&mut self, offset: usize) {}
    fn visit_f32_ceil(&mut self, offset: usize) {}
    fn visit_f32_floor(&mut self, offset: usize) {}
    fn visit_f32_trunc(&mut self, offset: usize) {}
    fn visit_f32_nearest(&mut self, offset: usize) {}
    fn visit_f32_sqrt(&mut self, offset: usize) {}
    fn visit_f32_add(&mut self, offset: usize) {}
    fn visit_f32_sub(&mut self, offset: usize) {}
    fn visit_f32_mul(&mut self, offset: usize) {}
    fn visit_f32_div(&mut self, offset: usize) {}
    fn visit_f32_min(&mut self, offset: usize) {}
    fn visit_f32_max(&mut self, offset: usize) {}
    fn visit_f32_copysign(&mut self, offset: usize) {}
    fn visit_f64_abs(&mut self, offset: usize) {}
    fn visit_f64_neg(&mut self, offset: usize) {}
    fn visit_f64_ceil(&mut self, offset: usize) {}
    fn visit_f64_floor(&mut self, offset: usize) {}
    fn visit_f64_trunc(&mut self, offset: usize) {}
    fn visit_f64_nearest(&mut self, offset: usize) {}
    fn visit_f64_sqrt(&mut self, offset: usize) {}
    fn visit_f64_add(&mut self, offset: usize) {}
    fn visit_f64_sub(&mut self, offset: usize) {}
    fn visit_f64_mul(&mut self, offset: usize) {}
    fn visit_f64_div(&mut self, offset: usize) {}
    fn visit_f64_min(&mut self, offset: usize) {}
    fn visit_f64_max(&mut self, offset: usize) {}
    fn visit_f64_copysign(&mut self, offset: usize) {}
    fn visit_i32_wrap_i64(&mut self, offset: usize) {}
    fn visit_i32_trunc_f32s(&mut self, offset: usize) {}
    fn visit_i32_trunc_f32u(&mut self, offset: usize) {}
    fn visit_i32_trunc_f64s(&mut self, offset: usize) {}
    fn visit_i32_trunc_f64u(&mut self, offset: usize) {}
    fn visit_i64_extend_i32s(&mut self, offset: usize) {}
    fn visit_i64_extend_i32u(&mut self, offset: usize) {}
    fn visit_i64_trunc_f32s(&mut self, offset: usize) {}
    fn visit_i64_trunc_f32u(&mut self, offset: usize) {}
    fn visit_i64_trunc_f64s(&mut self, offset: usize) {}
    fn visit_i64_trunc_f64u(&mut self, offset: usize) {}
    fn visit_f32_convert_i32s(&mut self, offset: usize) {}
    fn visit_f32_convert_i32u(&mut self, offset: usize) {}
    fn visit_f32_convert_i64s(&mut self, offset: usize) {}
    fn visit_f32_convert_i64u(&mut self, offset: usize) {}
    fn visit_f32_demote_f64(&mut self, offset: usize) {}
    fn visit_f64_convert_i32s(&mut self, offset: usize) {}
    fn visit_f64_convert_i32u(&mut self, offset: usize) {}
    fn visit_f64_convert_i64s(&mut self, offset: usize) {}
    fn visit_f64_convert_i64u(&mut self, offset: usize) {}
    fn visit_f64_promote_f32(&mut self, offset: usize) {}
    fn visit_i32_reinterpret_f32(&mut self, offset: usize) {}
    fn visit_i64_reinterpret_f64(&mut self, offset: usize) {}
    fn visit_f32_reinterpret_i32(&mut self, offset: usize) {}
    fn visit_f64_reinterpret_i64(&mut self, offset: usize) {}
    fn visit_i32_trunc_sat_f32s(&mut self, offset: usize) {}
    fn visit_i32_trunc_sat_f32u(&mut self, offset: usize) {}
    fn visit_i32_trunc_sat_f64s(&mut self, offset: usize) {}
    fn visit_i32_trunc_sat_f64u(&mut self, offset: usize) {}
    fn visit_i64_trunc_sat_f32s(&mut self, offset: usize) {}
    fn visit_i64_trunc_sat_f32u(&mut self, offset: usize) {}
    fn visit_i64_trunc_sat_f64s(&mut self, offset: usize) {}
    fn visit_i64_trunc_sat_f64u(&mut self, offset: usize) {}
    fn visit_i32_extend8_s(&mut self, offset: usize) {}
    fn visit_i32_extend16_s(&mut self, offset: usize) {}
    fn visit_i64_extend8_s(&mut self, offset: usize) {}
    fn visit_i64_extend16_s(&mut self, offset: usize) {}
    fn visit_i64_extend32_s(&mut self, offset: usize) {}
    fn visit_i32_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_load32_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_load16_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_store32(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw16_add_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw16_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw16_and_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw16_or_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw16_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw8_add_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw8_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw8_and_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw8_or_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw8_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw32_add_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw32_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw32_and_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw32_or_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw32_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw16_add_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw16_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw16_and_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw16_or_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw16_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw8_add_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw8_sub_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw8_and_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw8_or_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw8_xor_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw_xchg(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw16_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw8_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw_cmpxchg(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw16_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i32_atomic_rmw8_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw_xchg(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw32_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw16_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw8_xchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw_cmpxchg(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw32_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw16_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_i64_atomic_rmw8_cmpxchg_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_memory_atomic_notify(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_memory_atomic_wait32(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_memory_atomic_wait64(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_atomic_fence(&mut self, offset: usize, flags: u8) {}
    fn visit_ref_null(&mut self, offset: usize, ty: ValType) {}
    fn visit_ref_is_null(&mut self, offset: usize) {}
    fn visit_ref_func(&mut self, offset: usize, function_index: u32) {}
    fn visit_v128_load(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_store(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_const(&mut self, offset: usize, value: V128) {}
    fn visit_i8x16_splat(&mut self, offset: usize) {}
    fn visit_i16x8_splat(&mut self, offset: usize) {}
    fn visit_i32x4_splat(&mut self, offset: usize) {}
    fn visit_i64x2_splat(&mut self, offset: usize) {}
    fn visit_f32x4_splat(&mut self, offset: usize) {}
    fn visit_f64x2_splat(&mut self, offset: usize) {}
    fn visit_i8x16_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i8x16_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i16x8_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i16x8_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i8x16_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i16x8_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_i64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_f32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_f32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_f64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_f64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex) {}
    fn visit_f32x4_eq(&mut self, offset: usize) {}
    fn visit_f32x4_ne(&mut self, offset: usize) {}
    fn visit_f32x4_lt(&mut self, offset: usize) {}
    fn visit_f32x4_gt(&mut self, offset: usize) {}
    fn visit_f32x4_le(&mut self, offset: usize) {}
    fn visit_f32x4_ge(&mut self, offset: usize) {}
    fn visit_f64x2_eq(&mut self, offset: usize) {}
    fn visit_f64x2_ne(&mut self, offset: usize) {}
    fn visit_f64x2_lt(&mut self, offset: usize) {}
    fn visit_f64x2_gt(&mut self, offset: usize) {}
    fn visit_f64x2_le(&mut self, offset: usize) {}
    fn visit_f64x2_ge(&mut self, offset: usize) {}
    fn visit_f32x4_add(&mut self, offset: usize) {}
    fn visit_f32x4_sub(&mut self, offset: usize) {}
    fn visit_f32x4_mul(&mut self, offset: usize) {}
    fn visit_f32x4_div(&mut self, offset: usize) {}
    fn visit_f32x4_min(&mut self, offset: usize) {}
    fn visit_f32x4_max(&mut self, offset: usize) {}
    fn visit_f32x4_p_min(&mut self, offset: usize) {}
    fn visit_f32x4_p_max(&mut self, offset: usize) {}
    fn visit_f64x2_add(&mut self, offset: usize) {}
    fn visit_f64x2_sub(&mut self, offset: usize) {}
    fn visit_f64x2_mul(&mut self, offset: usize) {}
    fn visit_f64x2_div(&mut self, offset: usize) {}
    fn visit_f64x2_min(&mut self, offset: usize) {}
    fn visit_f64x2_max(&mut self, offset: usize) {}
    fn visit_f64x2_p_min(&mut self, offset: usize) {}
    fn visit_f64x2_p_max(&mut self, offset: usize) {}
    fn visit_f32x4_relaxed_min(&mut self, offset: usize) {}
    fn visit_f32x4_relaxed_max(&mut self, offset: usize) {}
    fn visit_f64x2_relaxed_min(&mut self, offset: usize) {}
    fn visit_f64x2_relaxed_max(&mut self, offset: usize) {}
    fn visit_i8x16_eq(&mut self, offset: usize) {}
    fn visit_i8x16_ne(&mut self, offset: usize) {}
    fn visit_i8x16_lt_s(&mut self, offset: usize) {}
    fn visit_i8x16_lt_u(&mut self, offset: usize) {}
    fn visit_i8x16_gt_s(&mut self, offset: usize) {}
    fn visit_i8x16_gt_u(&mut self, offset: usize) {}
    fn visit_i8x16_le_s(&mut self, offset: usize) {}
    fn visit_i8x16_le_u(&mut self, offset: usize) {}
    fn visit_i8x16_ge_s(&mut self, offset: usize) {}
    fn visit_i8x16_ge_u(&mut self, offset: usize) {}
    fn visit_i16x8_eq(&mut self, offset: usize) {}
    fn visit_i16x8_ne(&mut self, offset: usize) {}
    fn visit_i16x8_lt_s(&mut self, offset: usize) {}
    fn visit_i16x8_lt_u(&mut self, offset: usize) {}
    fn visit_i16x8_gt_s(&mut self, offset: usize) {}
    fn visit_i16x8_gt_u(&mut self, offset: usize) {}
    fn visit_i16x8_le_s(&mut self, offset: usize) {}
    fn visit_i16x8_le_u(&mut self, offset: usize) {}
    fn visit_i16x8_ge_s(&mut self, offset: usize) {}
    fn visit_i16x8_ge_u(&mut self, offset: usize) {}
    fn visit_i32x4_eq(&mut self, offset: usize) {}
    fn visit_i32x4_ne(&mut self, offset: usize) {}
    fn visit_i32x4_lt_s(&mut self, offset: usize) {}
    fn visit_i32x4_lt_u(&mut self, offset: usize) {}
    fn visit_i32x4_gt_s(&mut self, offset: usize) {}
    fn visit_i32x4_gt_u(&mut self, offset: usize) {}
    fn visit_i32x4_le_s(&mut self, offset: usize) {}
    fn visit_i32x4_le_u(&mut self, offset: usize) {}
    fn visit_i32x4_ge_s(&mut self, offset: usize) {}
    fn visit_i32x4_ge_u(&mut self, offset: usize) {}
    fn visit_i64x2_eq(&mut self, offset: usize) {}
    fn visit_i64x2_ne(&mut self, offset: usize) {}
    fn visit_i64x2_lt_s(&mut self, offset: usize) {}
    fn visit_i64x2_gt_s(&mut self, offset: usize) {}
    fn visit_i64x2_le_s(&mut self, offset: usize) {}
    fn visit_i64x2_ge_s(&mut self, offset: usize) {}
    fn visit_v128_and(&mut self, offset: usize) {}
    fn visit_v128_and_not(&mut self, offset: usize) {}
    fn visit_v128_or(&mut self, offset: usize) {}
    fn visit_v128_xor(&mut self, offset: usize) {}
    fn visit_i8x16_add(&mut self, offset: usize) {}
    fn visit_i8x16_add_sat_s(&mut self, offset: usize) {}
    fn visit_i8x16_add_sat_u(&mut self, offset: usize) {}
    fn visit_i8x16_sub(&mut self, offset: usize) {}
    fn visit_i8x16_sub_sat_s(&mut self, offset: usize) {}
    fn visit_i8x16_sub_sat_u(&mut self, offset: usize) {}
    fn visit_i8x16_min_s(&mut self, offset: usize) {}
    fn visit_i8x16_min_u(&mut self, offset: usize) {}
    fn visit_i8x16_max_s(&mut self, offset: usize) {}
    fn visit_i8x16_max_u(&mut self, offset: usize) {}
    fn visit_i16x8_add(&mut self, offset: usize) {}
    fn visit_i16x8_add_sat_s(&mut self, offset: usize) {}
    fn visit_i16x8_add_sat_u(&mut self, offset: usize) {}
    fn visit_i16x8_sub(&mut self, offset: usize) {}
    fn visit_i16x8_sub_sat_s(&mut self, offset: usize) {}
    fn visit_i16x8_sub_sat_u(&mut self, offset: usize) {}
    fn visit_i16x8_mul(&mut self, offset: usize) {}
    fn visit_i16x8_min_s(&mut self, offset: usize) {}
    fn visit_i16x8_min_u(&mut self, offset: usize) {}
    fn visit_i16x8_max_s(&mut self, offset: usize) {}
    fn visit_i16x8_max_u(&mut self, offset: usize) {}
    fn visit_i32x4_add(&mut self, offset: usize) {}
    fn visit_i32x4_sub(&mut self, offset: usize) {}
    fn visit_i32x4_mul(&mut self, offset: usize) {}
    fn visit_i32x4_min_s(&mut self, offset: usize) {}
    fn visit_i32x4_min_u(&mut self, offset: usize) {}
    fn visit_i32x4_max_s(&mut self, offset: usize) {}
    fn visit_i32x4_max_u(&mut self, offset: usize) {}
    fn visit_i32x4_dot_i16x8_s(&mut self, offset: usize) {}
    fn visit_i64x2_add(&mut self, offset: usize) {}
    fn visit_i64x2_sub(&mut self, offset: usize) {}
    fn visit_i64x2_mul(&mut self, offset: usize) {}
    fn visit_i8x16_rounding_average_u(&mut self, offset: usize) {}
    fn visit_i16x8_rounding_average_u(&mut self, offset: usize) {}
    fn visit_i8x16_narrow_i16x8_s(&mut self, offset: usize) {}
    fn visit_i8x16_narrow_i16x8_u(&mut self, offset: usize) {}
    fn visit_i16x8_narrow_i32x4_s(&mut self, offset: usize) {}
    fn visit_i16x8_narrow_i32x4_u(&mut self, offset: usize) {}
    fn visit_i16x8_ext_mul_low_i8x16_s(&mut self, offset: usize) {}
    fn visit_i16x8_ext_mul_high_i8x16_s(&mut self, offset: usize) {}
    fn visit_i16x8_ext_mul_low_i8x16_u(&mut self, offset: usize) {}
    fn visit_i16x8_ext_mul_high_i8x16_u(&mut self, offset: usize) {}
    fn visit_i32x4_ext_mul_low_i16x8_s(&mut self, offset: usize) {}
    fn visit_i32x4_ext_mul_high_i16x8_s(&mut self, offset: usize) {}
    fn visit_i32x4_ext_mul_low_i16x8_u(&mut self, offset: usize) {}
    fn visit_i32x4_ext_mul_high_i16x8_u(&mut self, offset: usize) {}
    fn visit_i64x2_ext_mul_low_i32x4_s(&mut self, offset: usize) {}
    fn visit_i64x2_ext_mul_high_i32x4_s(&mut self, offset: usize) {}
    fn visit_i64x2_ext_mul_low_i32x4_u(&mut self, offset: usize) {}
    fn visit_i64x2_ext_mul_high_i32x4_u(&mut self, offset: usize) {}
    fn visit_i16x8_q15_mulr_sat_s(&mut self, offset: usize) {}
    fn visit_f32x4_ceil(&mut self, offset: usize) {}
    fn visit_f32x4_floor(&mut self, offset: usize) {}
    fn visit_f32x4_trunc(&mut self, offset: usize) {}
    fn visit_f32x4_nearest(&mut self, offset: usize) {}
    fn visit_f64x2_ceil(&mut self, offset: usize) {}
    fn visit_f64x2_floor(&mut self, offset: usize) {}
    fn visit_f64x2_trunc(&mut self, offset: usize) {}
    fn visit_f64x2_nearest(&mut self, offset: usize) {}
    fn visit_f32x4_abs(&mut self, offset: usize) {}
    fn visit_f32x4_neg(&mut self, offset: usize) {}
    fn visit_f32x4_sqrt(&mut self, offset: usize) {}
    fn visit_f64x2_abs(&mut self, offset: usize) {}
    fn visit_f64x2_neg(&mut self, offset: usize) {}
    fn visit_f64x2_sqrt(&mut self, offset: usize) {}
    fn visit_f32x4_demote_f64x2_zero(&mut self, offset: usize) {}
    fn visit_f64x2_promote_low_f32x4(&mut self, offset: usize) {}
    fn visit_f64x2_convert_low_i32x4_s(&mut self, offset: usize) {}
    fn visit_f64x2_convert_low_i32x4_u(&mut self, offset: usize) {}
    fn visit_i32x4_trunc_sat_f32x4_s(&mut self, offset: usize) {}
    fn visit_i32x4_trunc_sat_f32x4_u(&mut self, offset: usize) {}
    fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, offset: usize) {}
    fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, offset: usize) {}
    fn visit_f32x4_convert_i32x4_s(&mut self, offset: usize) {}
    fn visit_f32x4_convert_i32x4_u(&mut self, offset: usize) {}
    fn visit_v128_not(&mut self, offset: usize) {}
    fn visit_i8x16_abs(&mut self, offset: usize) {}
    fn visit_i8x16_neg(&mut self, offset: usize) {}
    fn visit_i8x16_popcnt(&mut self, offset: usize) {}
    fn visit_i16x8_abs(&mut self, offset: usize) {}
    fn visit_i16x8_neg(&mut self, offset: usize) {}
    fn visit_i32x4_abs(&mut self, offset: usize) {}
    fn visit_i32x4_neg(&mut self, offset: usize) {}
    fn visit_i64x2_abs(&mut self, offset: usize) {}
    fn visit_i64x2_neg(&mut self, offset: usize) {}
    fn visit_i16x8_extend_low_i8x16_s(&mut self, offset: usize) {}
    fn visit_i16x8_extend_high_i8x16_s(&mut self, offset: usize) {}
    fn visit_i16x8_extend_low_i8x16_u(&mut self, offset: usize) {}
    fn visit_i16x8_extend_high_i8x16_u(&mut self, offset: usize) {}
    fn visit_i32x4_extend_low_i16x8_s(&mut self, offset: usize) {}
    fn visit_i32x4_extend_high_i16x8_s(&mut self, offset: usize) {}
    fn visit_i32x4_extend_low_i16x8_u(&mut self, offset: usize) {}
    fn visit_i32x4_extend_high_i16x8_u(&mut self, offset: usize) {}
    fn visit_i64x2_extend_low_i32x4_s(&mut self, offset: usize) {}
    fn visit_i64x2_extend_high_i32x4_s(&mut self, offset: usize) {}
    fn visit_i64x2_extend_low_i32x4_u(&mut self, offset: usize) {}
    fn visit_i64x2_extend_high_i32x4_u(&mut self, offset: usize) {}
    fn visit_i16x8_ext_add_pairwise_i8x16_s(&mut self, offset: usize) {}
    fn visit_i16x8_ext_add_pairwise_i8x16_u(&mut self, offset: usize) {}
    fn visit_i32x4_ext_add_pairwise_i16x8_s(&mut self, offset: usize) {}
    fn visit_i32x4_ext_add_pairwise_i16x8_u(&mut self, offset: usize) {}
    fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, offset: usize) {}
    fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, offset: usize) {}
    fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, offset: usize) {}
    fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, offset: usize) {}
    fn visit_v128_bitselect(&mut self, offset: usize) {}
    fn visit_f32x4_fma(&mut self, offset: usize) {}
    fn visit_f32x4_fms(&mut self, offset: usize) {}
    fn visit_f64x2_fma(&mut self, offset: usize) {}
    fn visit_f64x2_fms(&mut self, offset: usize) {}
    fn visit_i8x16_lane_select(&mut self, offset: usize) {}
    fn visit_i16x8_lane_select(&mut self, offset: usize) {}
    fn visit_i32x4_lane_select(&mut self, offset: usize) {}
    fn visit_i64x2_lane_select(&mut self, offset: usize) {}
    fn visit_v128_any_true(&mut self, offset: usize) {}
    fn visit_i8x16_all_true(&mut self, offset: usize) {}
    fn visit_i8x16_bitmask(&mut self, offset: usize) {}
    fn visit_i16x8_all_true(&mut self, offset: usize) {}
    fn visit_i16x8_bitmask(&mut self, offset: usize) {}
    fn visit_i32x4_all_true(&mut self, offset: usize) {}
    fn visit_i32x4_bitmask(&mut self, offset: usize) {}
    fn visit_i64x2_all_true(&mut self, offset: usize) {}
    fn visit_i64x2_bitmask(&mut self, offset: usize) {}
    fn visit_i8x16_shl(&mut self, offset: usize) {}
    fn visit_i8x16_shr_s(&mut self, offset: usize) {}
    fn visit_i8x16_shr_u(&mut self, offset: usize) {}
    fn visit_i16x8_shl(&mut self, offset: usize) {}
    fn visit_i16x8_shr_s(&mut self, offset: usize) {}
    fn visit_i16x8_shr_u(&mut self, offset: usize) {}
    fn visit_i32x4_shl(&mut self, offset: usize) {}
    fn visit_i32x4_shr_s(&mut self, offset: usize) {}
    fn visit_i32x4_shr_u(&mut self, offset: usize) {}
    fn visit_i64x2_shl(&mut self, offset: usize) {}
    fn visit_i64x2_shr_s(&mut self, offset: usize) {}
    fn visit_i64x2_shr_u(&mut self, offset: usize) {}
    fn visit_i8x16_swizzle(&mut self, offset: usize) {}
    fn visit_i8x16_relaxed_swizzle(&mut self, offset: usize) {}
    fn visit_i8x16_shuffle(&mut self, offset: usize, lanes: [SIMDLaneIndex; 16]) {}
    fn visit_v128_load8_splat(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load16_splat(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load32_splat(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load32_zero(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load64_splat(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load64_zero(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load8x8_s(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load8x8_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load16x4_s(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load16x4_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load32x2_s(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load32x2_u(&mut self, offset: usize, memarg: MemoryImmediate) {}
    fn visit_v128_load8_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) {
    }
    fn visit_v128_load16_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) {
    }
    fn visit_v128_load32_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) {
    }
    fn visit_v128_load64_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) {
    }
    fn visit_v128_store8_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) {
    }
    fn visit_v128_store16_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) {
    }
    fn visit_v128_store32_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) {
    }
    fn visit_v128_store64_lane(
        &mut self,
        offset: usize,
        memarg: MemoryImmediate,
        lane: SIMDLaneIndex,
    ) {
    }
    fn visit_memory_init(&mut self, offset: usize, mem: u32, segment: u32) {}
    fn visit_data_drop(&mut self, offset: usize, segment: u32) {}
    fn visit_memory_copy(&mut self, offset: usize, src: u32, dst: u32) {}
    fn visit_memory_fill(&mut self, offset: usize, mem: u32) {}
    fn visit_table_init(&mut self, offset: usize, segment: u32, table: u32) {}
    fn visit_elem_drop(&mut self, offset: usize, segment: u32) {}
    fn visit_table_copy(&mut self, offset: usize, dst_table: u32, src_table: u32) {}
    fn visit_table_get(&mut self, offset: usize, table: u32) {}
    fn visit_table_set(&mut self, offset: usize, table: u32) {}
    fn visit_table_grow(&mut self, offset: usize, table: u32) {}
    fn visit_table_size(&mut self, offset: usize, table: u32) {}
    fn visit_table_fill(&mut self, offset: usize, table: u32) {}
}
