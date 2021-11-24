#![no_main]

use libfuzzer_sys::fuzz_target;
use std::sync::atomic::{AtomicU64, Ordering};
use wasmparser::WasmFeatures;

static NUM_RUNS: AtomicU64 = AtomicU64::new(0);
static NUM_SUCCESSFUL_MUTATIONS: AtomicU64 = AtomicU64::new(0);

fuzz_target!(|bytes: &[u8]| {
    let _ = env_logger::try_init();

    // Generate a random Wasm module with `wasm-smith` as well as a RNG seed for
    // use with `wasm-mutate`.

    let mut seed = 0;
    let (wasm, _config) = match wasm_tools_fuzz::generate_valid_module(bytes, |config, u| {
        config.module_linking_enabled = false;
        config.exceptions_enabled = false;
        config.simd_enabled = false;
        config.reference_types_enabled = false;
        config.memory64_enabled = false;
        config.max_memories = 1;
        seed = u.arbitrary()?;
        Ok(())
    }) {
        Ok(m) => m,
        Err(_) => return,
    };
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

    // Mutate the Wasm with `wasm-mutate`. We always preserve semantics.
    let mut wasm_mutate = wasm_mutate::WasmMutate::default();
    wasm_mutate.seed(seed);
    wasm_mutate.fuel(1000);
    wasm_mutate.preserve_semantics(true);

    let mutated_wasm_iterator = wasm_mutate.run(&wasm);
    let mut found = false;

    match mutated_wasm_iterator {
        Ok(mut iterator) => {
            while let Some(mutated_wasm) = iterator.next() {
                let features = WasmFeatures::default();
                let mut validator = wasmparser::Validator::new();
                validator.wasm_features(features);

                match mutated_wasm {
                    Ok(mutated_wasm) => {
                        // Increase ony once for the same input Wasm
                        if !found {
                            NUM_SUCCESSFUL_MUTATIONS.fetch_add(1, Ordering::Relaxed);
                            found = true;
                        } else {
                            NUM_SUCCESSFUL_MUTATIONS.fetch_add(1, Ordering::Relaxed);
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
                        }
                        let validation_result = validator.validate_all(&mutated_wasm);
                        log::debug!("validation result = {:?}", validation_result);
                        assert!(
                            validation_result.is_ok(),
                            "`wasm-mutate` should always produce a valid Wasm file"
                        );

                        #[cfg(feature = "wasmtime")]
                        eval::assert_same_evaluation(&wasm, &mutated_wasm);
                    }
                    Err(e) => match e {
                        wasm_mutate::Error::NoMutationsApplicable => continue,
                        e => panic!("Unexpected mutation failure: {}", e),
                    },
                }
            }
        }
        Err(e) => match e {
            wasm_mutate::Error::NoMutationsApplicable => return,
            e => panic!("Unexpected mutation failure: {}", e),
        },
    }
});

#[cfg(feature = "wasmtime")]
#[path = "../../crates/fuzz-stats/src/dummy.rs"]
pub mod dummy;

#[cfg(feature = "wasmtime")]
mod eval {
    use super::dummy;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    /// Compile, instantiate, and evaluate both the original and mutated Wasm.
    ///
    /// We should get identical results because we told `wasm-mutate` to preserve
    /// semantics.
    pub fn assert_same_evaluation(wasm: &[u8], mutated_wasm: &[u8]) {
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

        let mut store = wasmtime::Store::new(&engine, ());
        let (orig_imports, mutated_imports) = match dummy::dummy_imports(&mut store, &orig_module) {
            Ok(imps) => (imps.clone(), imps),
            Err(_) => return,
        };

        let (orig_instance, mutated_instance) = match (
            wasmtime::Instance::new(&mut store, &orig_module, &orig_imports),
            wasmtime::Instance::new(&mut store, &mutated_module, &mutated_imports),
        ) {
            (Ok(x), Ok(y)) => (x, y),
            (_, _) => return,
        };

        assert_same_state(&mut store, &orig_module, orig_instance, mutated_instance);
        assert_same_calls(&mut store, &orig_module, orig_instance, mutated_instance);
        assert_same_state(&mut store, &orig_module, orig_instance, mutated_instance);
    }

    fn assert_same_state(
        store: &mut wasmtime::Store<()>,
        orig_module: &wasmtime::Module,
        orig_instance: wasmtime::Instance,
        mutated_instance: wasmtime::Instance,
    ) {
        for export in orig_module.exports() {
            match export.ty() {
                wasmtime::ExternType::Global(_) => {
                    let orig = orig_instance
                        .get_export(&mut *store, export.name())
                        .unwrap()
                        .into_global()
                        .unwrap()
                        .get(&mut *store);
                    let mutated = mutated_instance
                        .get_export(&mut *store, export.name())
                        .unwrap()
                        .into_global()
                        .unwrap()
                        .get(&mut *store);
                    assert_val_eq(&orig, &mutated);
                }
                wasmtime::ExternType::Memory(_) => {
                    let orig = orig_instance
                        .get_export(&mut *store, export.name())
                        .unwrap()
                        .into_memory()
                        .unwrap();
                    let mut h = DefaultHasher::default();
                    orig.data(&store).hash(&mut h);
                    let orig = h.finish();
                    let mutated = mutated_instance
                        .get_export(&mut *store, export.name())
                        .unwrap()
                        .into_memory()
                        .unwrap();
                    let mut h = DefaultHasher::default();
                    mutated.data(&store).hash(&mut h);
                    let mutated = h.finish();
                    assert_eq!(orig, mutated, "original and mutated Wasm memories diverged");
                }
                _ => continue,
            }
        }
    }

    fn assert_same_calls(
        store: &mut wasmtime::Store<()>,
        orig_module: &wasmtime::Module,
        orig_instance: wasmtime::Instance,
        mutated_instance: wasmtime::Instance,
    ) {
        for export in orig_module.exports() {
            match export.ty() {
                wasmtime::ExternType::Func(func_ty) => {
                    let orig_func = orig_instance.get_func(&mut *store, export.name()).unwrap();
                    let mutated_func = mutated_instance
                        .get_func(&mut *store, export.name())
                        .unwrap();
                    let args = dummy::dummy_values(func_ty.params());
                    match (
                        {
                            store.add_fuel(1_000).unwrap();
                            orig_func.call(&mut *store, &args)
                        },
                        {
                            let consumed = store.fuel_consumed().unwrap();
                            store.add_fuel(consumed).unwrap();
                            mutated_func.call(&mut *store, &args)
                        },
                    ) {
                        (Ok(orig_vals), Ok(mutated_vals)) => {
                            assert_eq!(orig_vals.len(), mutated_vals.len());
                            for (orig_val, mutated_val) in orig_vals.iter().zip(mutated_vals.iter())
                            {
                                assert_val_eq(orig_val, mutated_val);
                            }
                        }
                        (Err(_), Err(_)) => continue,
                        (orig, mutated) => panic!(
                            "mutated and original Wasm diverged: orig = {:?}; mutated = {:?}",
                            orig, mutated,
                        ),
                    }
                }
                _ => continue,
            }
        }
    }

    fn assert_val_eq(orig_val: &wasmtime::Val, mutated_val: &wasmtime::Val) {
        match (orig_val, mutated_val) {
            (wasmtime::Val::I32(o), wasmtime::Val::I32(m)) => assert_eq!(o, m),
            (wasmtime::Val::I64(o), wasmtime::Val::I64(m)) => assert_eq!(o, m),
            (wasmtime::Val::F32(o), wasmtime::Val::F32(m)) => assert_eq!(o, m),
            (wasmtime::Val::F64(o), wasmtime::Val::F64(m)) => assert_eq!(o, m),
            (o, m) => panic!(
                "mutated and original Wasm diverged: orig = {:?}; mutated = {:?}",
                o, m,
            ),
        }
    }
}
