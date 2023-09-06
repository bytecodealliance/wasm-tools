use arbitrary::{Result, Unstructured};
use std::sync::atomic::{AtomicU64, Ordering};
use wasmparser::WasmFeatures;

static NUM_RUNS: AtomicU64 = AtomicU64::new(0);
static NUM_SUCCESSFUL_MUTATIONS: AtomicU64 = AtomicU64::new(0);

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    // Generate a random Wasm module with `wasm-smith` as well as a RNG seed for
    // use with `wasm-mutate`.

    let mut seed = 0;
    let mut preserve_semantics = false;
    let (wasm, config) = crate::generate_valid_module(u, |config, u| {
        config.exceptions_enabled = false;
        seed = u.arbitrary()?;
        preserve_semantics = u.arbitrary()?;
        Ok(())
    })?;
    log::debug!("seed = {}", seed);

    // Keep track of how many runs we've done thus far and how many of those
    // runs had successful mutations.

    let old_num_runs = NUM_RUNS.fetch_add(1, Ordering::Relaxed);
    if old_num_runs % 4096 == 4095 && log::log_enabled!(log::Level::Info) {
        let successful = NUM_SUCCESSFUL_MUTATIONS.load(Ordering::Relaxed);
        let percent = successful as f64 / old_num_runs as f64 * 100.0;
        log::info!(
            "{} / {} ({:.2}%) successful mutations.",
            successful,
            old_num_runs,
            percent
        );
    }

    // Mutate the Wasm with `wasm-mutate`. Assert that each mutation is still
    // valid Wasm.

    let mut wasm_mutate = wasm_mutate::WasmMutate::default();
    wasm_mutate.seed(seed);
    wasm_mutate.fuel(300);
    wasm_mutate.preserve_semantics(
        // If we are going to check that we get the same evaluated results
        // before and after mutation, then we need to preserve semantics.
        cfg!(feature = "wasmtime") && preserve_semantics,
    );

    let iterator = match wasm_mutate.run(&wasm) {
        Ok(iterator) => iterator,
        Err(e) => {
            log::warn!("Failed to mutate the Wasm: {}", e);
            return Ok(());
        }
    };

    // Note that on-by-default features in wasmparser are not disabled here if
    // the feature was disabled in `config` when the module was generated. For
    // example if the input module doesn't have simd then wasm-mutate may
    // produce a module that uses simd, which is ok and expected.
    //
    // Otherwise only forward some off-by-default features which are affected by
    // wasm-smith's generation of modules and wasm-mutate otherwise won't add
    // itself if it doesn't already exist.
    let mut features = WasmFeatures::default();
    features.relaxed_simd = config.relaxed_simd_enabled;
    features.multi_memory = config.max_memories > 1;
    features.memory64 = config.memory64_enabled;
    features.threads = config.threads_enabled;

    for (i, mutated_wasm) in iterator.take(10).enumerate() {
        let mutated_wasm = match mutated_wasm {
            Ok(w) => w,
            Err(e) => match e.kind() {
                wasm_mutate::ErrorKind::NoMutationsApplicable => continue,
                _ => panic!("Unexpected mutation failure: {}", e),
            },
        };

        // Increase ony once for the same input Wasm.
        if i == 0 {
            NUM_SUCCESSFUL_MUTATIONS.fetch_add(1, Ordering::Relaxed);
        }

        let validation_result =
            wasmparser::Validator::new_with_features(features).validate_all(&mutated_wasm);

        if log::log_enabled!(log::Level::Debug) {
            log::debug!("writing mutated Wasm to `mutated.wasm`");
            std::fs::write("mutated.wasm", &mutated_wasm)
                .expect("should write `mutated.wasm` okay");
            if let Ok(mutated_wat) = wasmprinter::print_bytes(&mutated_wasm) {
                log::debug!("writing mutated WAT to `mutated.wat`");
                std::fs::write("mutated.wat", &mutated_wat)
                    .expect("should write `mutated.wat` okay");
            }
        }

        validation_result.expect("`wasm-mutate` should always produce a valid Wasm file");

        #[cfg(feature = "wasmtime")]
        if preserve_semantics {
            eval::assert_same_evaluation(&wasm, &mutated_wasm);
        }
    }

    Ok(())
}

#[cfg(feature = "wasmtime")]
#[path = "../../crates/fuzz-stats/src/lib.rs"]
pub mod fuzz_stats;

#[cfg(feature = "wasmtime")]
mod eval {
    use super::fuzz_stats::{dummy, limits::StoreLimits};
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    use wasmtime::{ResourceLimiter, Val};

    /// Compile, instantiate, and evaluate both the original and mutated Wasm.
    ///
    /// We should get identical results because we told `wasm-mutate` to preserve
    /// semantics.
    pub fn assert_same_evaluation(wasm: &[u8], mutated_wasm: &[u8]) {
        // FIXME: should re-enable this when the fuzzer works again, needs
        // someone to invest energy into running this locally for a long time
        // and then be on the hook for incoming oss-fuzz bugs.
        if true {
            return;
        }

        let mut config = wasmtime::Config::default();
        config.cranelift_nan_canonicalization(true);
        config.consume_fuel(true);

        let engine = wasmtime::Engine::new(&config).unwrap();

        let (orig_module, mutated_module) = match (
            wasmtime::Module::new(&engine, &wasm),
            wasmtime::Module::new(&engine, &mutated_wasm),
        ) {
            (Ok(o), Ok(m)) => (o, m),
            // Ideally we would assert that they both errored if either one did, but
            // it is possible that a mutation bumped some count above/below an
            // implementation limit.
            (_, _) => return,
        };

        let limits = StoreLimits {
            remaining_memory: 1 << 30,
            oom: false,
        };
        let mut orig_store = wasmtime::Store::new(&engine, limits.clone());
        let mut mutated_store = wasmtime::Store::new(&engine, limits);
        orig_store.limiter(|s| s as &mut dyn ResourceLimiter);
        mutated_store.limiter(|s| s as &mut dyn ResourceLimiter);
        let orig_imports = match dummy::dummy_imports(&mut orig_store, &orig_module) {
            Ok(imps) => imps,
            Err(_) => return,
        };
        let mutated_imports = match dummy::dummy_imports(&mut mutated_store, &mutated_module) {
            Ok(imps) => imps,
            Err(_) => return,
        };

        let (orig_instance, mutated_instance) = match (
            wasmtime::Instance::new(&mut orig_store, &orig_module, &orig_imports),
            wasmtime::Instance::new(&mut mutated_store, &mutated_module, &mutated_imports),
        ) {
            (Ok(x), Ok(y)) => (x, y),
            (_, _) => return,
        };

        assert_same_state(
            &orig_module,
            &mut orig_store,
            orig_instance,
            &mut mutated_store,
            mutated_instance,
        );
        let should_have_same_state = assert_same_calls(
            &orig_module,
            &mut orig_store,
            orig_instance,
            &mut mutated_store,
            mutated_instance,
        );

        if should_have_same_state {
            assert_same_state(
                &orig_module,
                &mut orig_store,
                orig_instance,
                &mut mutated_store,
                mutated_instance,
            );
        }
    }

    fn assert_same_state(
        orig_module: &wasmtime::Module,
        orig_store: &mut wasmtime::Store<StoreLimits>,
        orig_instance: wasmtime::Instance,
        mutated_store: &mut wasmtime::Store<StoreLimits>,
        mutated_instance: wasmtime::Instance,
    ) {
        for export in orig_module.exports() {
            match export.ty() {
                wasmtime::ExternType::Global(_) => {
                    let orig = orig_instance
                        .get_export(&mut *orig_store, export.name())
                        .unwrap()
                        .into_global()
                        .unwrap()
                        .get(&mut *orig_store);
                    let mutated = mutated_instance
                        .get_export(&mut *mutated_store, export.name())
                        .unwrap()
                        .into_global()
                        .unwrap()
                        .get(&mut *mutated_store);
                    assert_val_eq(&orig, &mutated);
                }
                wasmtime::ExternType::Memory(_) => {
                    let orig = orig_instance
                        .get_export(&mut *orig_store, export.name())
                        .unwrap()
                        .into_memory()
                        .unwrap();
                    let mut h = DefaultHasher::default();
                    orig.data(&orig_store).hash(&mut h);
                    let orig = h.finish();
                    let mutated = mutated_instance
                        .get_export(&mut *mutated_store, export.name())
                        .unwrap()
                        .into_memory()
                        .unwrap();
                    let mut h = DefaultHasher::default();
                    mutated.data(&mutated_store).hash(&mut h);
                    let mutated = h.finish();
                    assert_eq!(orig, mutated, "original and mutated Wasm memories diverged");
                }
                _ => continue,
            }
        }
    }

    fn assert_same_calls(
        orig_module: &wasmtime::Module,
        orig_store: &mut wasmtime::Store<StoreLimits>,
        orig_instance: wasmtime::Instance,
        mutated_store: &mut wasmtime::Store<StoreLimits>,
        mutated_instance: wasmtime::Instance,
    ) -> bool {
        for export in orig_module.exports() {
            let func_ty = match export.ty() {
                wasmtime::ExternType::Func(func_ty) => func_ty,
                _ => continue,
            };
            let orig_func = orig_instance
                .get_func(&mut *orig_store, export.name())
                .unwrap();
            let mutated_func = mutated_instance
                .get_func(&mut *mutated_store, export.name())
                .unwrap();
            let args = dummy::dummy_values(func_ty.params());
            let mut orig_results = vec![Val::I32(0); func_ty.results().len()];
            let mut mutated_results = orig_results.clone();
            log::debug!("invoking `{}`", export.name());
            match (
                {
                    orig_store.add_fuel(1_000).unwrap();
                    orig_func.call(&mut *orig_store, &args, &mut orig_results)
                },
                {
                    mutated_store.add_fuel(1000).unwrap();
                    mutated_func.call(&mut *mutated_store, &args, &mut mutated_results)
                },
            ) {
                (Ok(()), Ok(())) => {
                    for (orig_val, mutated_val) in orig_results.iter().zip(mutated_results.iter()) {
                        assert_val_eq(orig_val, mutated_val);
                    }
                }
                // If either test case ran out of fuel then that's ok since
                // mutation may add code or delete code which causes one side to
                // take more or less fuel than the other. In this situation,
                // however, execution has diverged so throw out the test case.
                (Err(e), _) | (_, Err(e))
                    if e.downcast_ref() == Some(&wasmtime::Trap::OutOfFuel) =>
                {
                    return false
                }
                (Err(orig), Err(mutated)) => {
                    log::debug!("original error {orig:?}");
                    log::debug!("mutated error {mutated:?}");
                    continue;
                }
                (orig, mutated) => panic!(
                    "mutated and original Wasm diverged: orig = {:?}; mutated = {:?}",
                    orig, mutated,
                ),
            }
        }

        true
    }

    fn assert_val_eq(orig_val: &wasmtime::Val, mutated_val: &wasmtime::Val) {
        match (orig_val, mutated_val) {
            (wasmtime::Val::I32(o), wasmtime::Val::I32(m)) => assert_eq!(o, m),
            (wasmtime::Val::I64(o), wasmtime::Val::I64(m)) => assert_eq!(o, m),
            (wasmtime::Val::F32(o), wasmtime::Val::F32(m)) => {
                let o = f32::from_bits(*o);
                let m = f32::from_bits(*m);
                assert!(o == m || (o.is_nan() && m.is_nan()));
            }
            (wasmtime::Val::F64(o), wasmtime::Val::F64(m)) => {
                let o = f64::from_bits(*o);
                let m = f64::from_bits(*m);
                assert!(o == m || (o.is_nan() && m.is_nan()));
            }
            (wasmtime::Val::V128(o), wasmtime::Val::V128(m)) => {
                assert_eq!(o, m)
            }
            (wasmtime::Val::ExternRef(o), wasmtime::Val::ExternRef(m)) => {
                assert_eq!(o.is_none(), m.is_none())
            }
            (wasmtime::Val::FuncRef(o), wasmtime::Val::FuncRef(m)) => {
                assert_eq!(o.is_none(), m.is_none())
            }
            (o, m) => panic!(
                "mutated and original Wasm diverged: orig = {:?}; mutated = {:?}",
                o, m,
            ),
        }
    }
}
