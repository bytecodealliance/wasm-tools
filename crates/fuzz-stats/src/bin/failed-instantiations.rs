use arbitrary::{Arbitrary, Error, Unstructured};
use rand::RngCore;
use std::sync::atomic::{AtomicIsize, AtomicUsize, Ordering::SeqCst};
use std::sync::Arc;
use wasm_smith::SwarmConfig;
use wasmtime::*;

struct State {
    engine: Engine,
    print: bool,
    remaining: AtomicIsize,
    total: AtomicUsize,
    instantiate_trap: AtomicUsize,
    instantiate_oom: AtomicUsize,
}

fn main() {
    Arc::new(State::new()).run();
}

// Theoretically this test can fail because it's based on random data. In
// practice it's expected that the "fails to instantiate" rate is <2%, so if we
// cross the 10% threshold that's quite bad.
#[test]
fn under_10_percent() {
    let mut state = State::new();
    state.print = false;
    state.remaining.store(1000, SeqCst);
    let state = Arc::new(state);
    state.run();

    let total = state.total.load(SeqCst);
    let bad = state.instantiate_trap.load(SeqCst) + state.instantiate_oom.load(SeqCst);
    assert!(
        bad < total / 10,
        "{} modules failed to instantiate out of {}, this failure rate is too high",
        bad,
        total
    );
}

impl State {
    fn new() -> State {
        let mut config = Config::new();
        config.wasm_multi_memory(true);
        config.wasm_simd(true);
        State {
            engine: Engine::new(&config).unwrap(),
            print: true,
            total: AtomicUsize::new(0),
            remaining: AtomicIsize::new(isize::max_value()),
            instantiate_trap: AtomicUsize::new(0),
            instantiate_oom: AtomicUsize::new(0),
        }
    }

    fn run(self: &Arc<Self>) {
        let threads = (0..num_cpus::get())
            .map(|_| {
                let state = self.clone();
                std::thread::spawn(move || state.run_worker())
            })
            .collect::<Vec<_>>();
        for thread in threads {
            thread.join().unwrap();
        }
    }

    fn run_worker(&self) {
        let mut rng = rand::thread_rng();
        let mut data = Vec::new();

        while self.remaining.fetch_sub(1, SeqCst) >= 0 {
            data.truncate(0);
            data.resize(1024, 0);
            rng.fill_bytes(&mut data);
            loop {
                match self.run_once(&mut data) {
                    Ok(()) => break,
                    Err(Error::NotEnoughData) => {
                        let cur = data.len();
                        let extra = 1024;
                        data.resize(cur + extra, 0);
                        rng.fill_bytes(&mut data[cur..]);
                    }
                    Err(e) => panic!("failed to generated module: {}", e),
                }
            }
        }
    }

    /// Generates a random modules using `data`, and then attempts to
    /// instantiate it.
    ///
    /// Records when instantiation fails and why it fails.
    fn run_once(&self, data: &[u8]) -> Result<(), Error> {
        let mut u = Unstructured::new(data);
        // Here `SwarmConfig` is used to get hopefully a bit more coverage of
        // interesting states, and we also forcibly disable all `start`
        // functions for now. Not much work has gone into minimizing the traps
        // generated from wasm functions themselves, and this shouldn't be
        // enabled until that's been worked on.
        let mut config = SwarmConfig::arbitrary(&mut u)?;
        config.allow_start_export = false;
        let mut wasm = wasm_smith::Module::new(config, &mut u)?;
        wasm.ensure_termination(10_000);
        let wasm = wasm.to_bytes();

        // We install a resource limiter in the store which limits the store to
        // 1gb of memory. That's half the default allocation of memory for
        // libfuzzer-based fuzzers by default, and ideally we're not in a
        // situation where most of the modules are above this threshold.
        let module = Module::new(&self.engine, &wasm).expect("failed to compile module");
        let mut store = Store::new(
            &self.engine,
            StoreLimits {
                remaining_memory: 1 << 30,
                oom: false,
            },
        );
        store.limiter(|s| s as &mut dyn ResourceLimiter);

        // Synthesize dummy imports based on what the module asked for, and then
        // instantiate!
        let instance = fuzz_stats::dummy::dummy_imports(&mut store, &module)
            .and_then(|imports| Instance::new(&mut store, &module, &imports));

        match instance {
            // If instantiation succeeded, we're not too interested in anything
            // else right now. In the future we should probably run exported
            // functions and record whether a trap happened or not.
            Ok(_i) => {}

            Err(e) => {
                // Traps are ok if they happen during instantiation. This is an
                // expected occurrence we want to account for.
                if e.downcast_ref::<Trap>().is_some() {
                    std::fs::write("trap.wasm", &wasm).unwrap();
                    self.instantiate_trap.fetch_add(1, SeqCst);

                // Ooms, like traps, are normal during instantiations. This
                // can happen, for example, if a defined memory is very large.
                } else if store.data().oom {
                    std::fs::write("oom.wasm", &wasm).unwrap();
                    self.instantiate_oom.fetch_add(1, SeqCst);

                // In theory nothing else fails to instantiate. If it does, then
                // panic.
                } else {
                    std::fs::write("panic.wasm", &wasm).unwrap();
                    panic!("unknown: {}", e);
                }
            }
        }

        let prev_total = self.total.fetch_add(1, SeqCst);
        if prev_total % 10_000 == 0 && self.print {
            self.print(prev_total + 1);
        }

        Ok(())
    }

    /// Prints summary statistics of how many modules have been instantiated so
    /// far and how many of them have oom'd or trap'd.
    fn print(&self, total: usize) {
        print!("total: {:8}", total);
        let stat = |name: &str, stat: &AtomicUsize| {
            let stat = stat.load(SeqCst);
            if stat > 0 {
                print!(" {} {:5.02}% ", name, (stat as f64) / (total as f64) * 100.);
            }
        };
        stat("i-oom", &self.instantiate_oom);
        stat("i-trap", &self.instantiate_trap);
        println!();
    }
}

struct StoreLimits {
    remaining_memory: usize,
    oom: bool,
}

impl StoreLimits {
    fn alloc(&mut self, amt: usize) -> bool {
        match self.remaining_memory.checked_sub(amt) {
            Some(mem) => {
                self.remaining_memory = mem;
                true
            }
            None => {
                self.oom = true;
                false
            }
        }
    }
}

impl ResourceLimiter for StoreLimits {
    fn memory_growing(&mut self, current: usize, desired: usize, _maximum: Option<usize>) -> bool {
        self.alloc(desired - current)
    }

    fn table_growing(&mut self, current: u32, desired: u32, _maximum: Option<u32>) -> bool {
        let delta = (desired - current) as usize * std::mem::size_of::<usize>();
        self.alloc(delta)
    }
}
