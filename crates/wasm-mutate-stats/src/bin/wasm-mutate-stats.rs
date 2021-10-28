use rand::Rng;
use rand::{rngs::SmallRng, SeedableRng};
use wasmtime::Engine;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::{PathBuf};
use std::sync::atomic::{AtomicBool};
use std::{collections::HashMap, sync::Arc};
use wasm_mutate::WasmMutate;
use structopt::StructOpt;
use std::sync::{Mutex};
use core::sync::atomic::Ordering::{SeqCst, Relaxed};

/// Stats for wasm-mutate.
///
/// TODO
///
#[derive(StructOpt)]
struct Options {
    /// The input folder that contains the Wasm binaries.
    ///
    input: PathBuf,
    /// The timeout, 0 to wait for keyboard interrupt
    /// 
    #[structopt(short = "t", long = "timeout")]
    timeout: Option<u64>,
    /// The seed of the random mutation, 0 by default
    /// 
    #[structopt(short = "s", long = "seed")]
    seed: Option<u64>
}

// Used to stop threads
pub enum Signals {
    TimeOut
}

struct State {
    // To print the report
    print: bool,
    // To save temporary files like object generation from cranelift
    save_files: bool,
    // Tuples of filenames and the corresponding byte stream
    corpus: Vec<(PathBuf, Vec<u8>)>,
    // Engine used to compile the mutated Wasm
    engine : wasmtime::Engine,
    // timeout_reached state
    timeout_reached: AtomicBool,
}


fn main() {
    // Init logs
    let _ = env_logger::init();
    
    let opts = Options::from_args();
    let timeout = match opts.timeout {
        Some(u) => u,
        None => 0
    };
    let seed = match opts.seed {
        Some(u) => u,
        None => 0
    };

    // Start benchmarking
    Arc::new(State::new(opts.input)).run(timeout, seed);
}

impl State {
    pub fn new(input_folder: PathBuf) -> Self {
        // Read corpus folder
        let mut corpus = Vec::with_capacity(2000);
        let entries =
            std::fs::read_dir(input_folder).expect("failed to read dir");
        for e in entries {
            let e = e.expect("failed to read dir entry");
            if e.file_type().unwrap().is_file() {
                let seed = std::fs::read(e.path()).expect("failed to read seed file");
                if  e.path().extension().unwrap() == "wasm" {
                    corpus.push((e.path(), seed));
                }
            }
        }
        State {
            print: true,
            save_files: true,
            corpus,
            timeout_reached: AtomicBool::new(false),
            engine: Engine::default()
        }
    }

    fn run(self: &Arc<Self>, timeout: u64, seed: u64) {

        if self.corpus.len() == 0 {
            log::error!("No Wasm files in the folder");
            return;
        }
        // create a folder to save the mutated files
        let artifact_folders = self.corpus.iter().map(|(name, _)|{
            let newfolder = format!("{}/artifacts/{}", name.parent()
                .expect("Invalid parent")
                .parent()
                .expect("Invalid parent")
                .display(), 
                name.file_name().expect("File name could not be retrieved")
                .to_str().unwrap()
            );
            log::debug!("Artifacts saved at {:?}", newfolder);
            std::fs::create_dir_all(&newfolder).expect("Artifacts folder could not be created");
            newfolder
        }).collect::<Vec<_>>();
        let threads = (0..self.corpus.len())
            .into_iter()
            .map(|usize| {
                let state = self.clone();
                let artifact_folder = artifact_folders[usize].clone();
                std::thread::spawn(move || {
                    //sema1.lock();
                    state.generate(usize, seed, artifact_folder)
                })
            })
            .collect::<Vec<_>>();

        // Sleep the main thread as many seconds are defined in the timeout
        if timeout > 0 {
            std::thread::sleep(std::time::Duration::new(timeout, 0));
        } else {
            todo!();
            // Wait for keyboard interrupt
        }
        // Send termination signal, false || true = true
        self.timeout_reached.fetch_or(true, SeqCst);
        // Collect the results in the receiver
        for thread in threads {
            // Expect all module here
            thread.join().unwrap();
        }

        // Second stage, reports
        for (wasmidx, (name, _)) in self.corpus.iter().enumerate() {
            log::debug!("Input wasm \"{}\"", name.file_name()
                .expect("Missing file name")
                .to_str().unwrap());
            let artifacts_folder = format!("{}/artifacts/{}", name.parent()
                .expect("Invalid parent")
                .parent()
                .expect("Invalid parent")
                .display(), 
                name.file_name().expect("File name could not be retrieved")
                .to_str().unwrap()
            );

            // Read dir looking for Wasm
            let entries =
            std::fs::read_dir(artifacts_folder).expect("failed to read dir");
            let mut files_count = 0;
            
            let mut worklist = Vec::new();
            for e in entries {
                let e = e.expect("failed to read dir entry");
                if e.file_type().unwrap().is_file() {
                    if  e.path().extension().unwrap() == "wasm" {
                        worklist.push(e.path());
                        files_count += 1;
                    }
                }

            }
            let wasm_hashes = HashMap::new();
            let low_hashes = HashMap::new();
            log::debug!("{} files generated (+ original)", files_count);
            // Create shared worklist
            let wasm_hash_wrapper = Arc::new(Mutex::new(wasm_hashes));
            let low_hash_wrapper = Arc::new(Mutex::new(low_hashes));
            // Start compilation workers
            let num_workers = num_cpus::get();
            let chunksize = worklist.len()/num_workers + 1;
            let workers = (0..num_workers).map(|i|{
                    let wasm_hash_copy = wasm_hash_wrapper.clone();
                    let low_hash_copy = low_hash_wrapper.clone();
                    let state = self.clone();
                    let range = (i*chunksize,(i + 1)*chunksize);
                    let worklist_copy  = Arc::new(worklist.clone());
                    let object_folder = format!("{}", artifact_folders[wasmidx]);
                    std::thread::spawn(move || state.compile_and_save(range, worklist_copy,wasm_hash_copy,low_hash_copy, object_folder
                    ) )
            }).collect::<Vec<_>>();

            log::debug!("Waiting for {} compiler workers", workers.len());
            for worker in workers {
                worker.join().expect("Thread panicked !");
            }
            let whashes_len = wasm_hash_wrapper.lock().unwrap().len();
            let lhashes_len = low_hash_wrapper.lock().unwrap().len();
            log::debug!("{} unique Wasm ({:.2}%)",whashes_len , 100.0 * whashes_len as f64 / files_count as f64);
            log::debug!("{} unique Low-Level ({:.2}%)", lhashes_len, 100.0 * lhashes_len as f64 / whashes_len as f64);

            // Check low-level prevalence
            // create the engine outside this function and pass it in as an argument, so that
            // it is shared across all modules.
        }
    }

    fn compile_and_save(&self, range: (usize, usize), worklist: Arc<Vec<PathBuf>>, wasm_hashes: Arc<Mutex<HashMap<u64, Vec<String>>>>, low_hashes: Arc<Mutex<HashMap<u64, Vec<String>>>>, artifact_folder: String) {

        let newfolder = format!("{}/aot", artifact_folder);
        std::fs::create_dir_all(&newfolder).expect("Artifacts folder could not be created");

        for entryindex in range.0..range.1 {
            if let Some(entry) = worklist.get(entryindex) {
                let data = std::fs::read(entry).expect("failed to read seed file");
                // Hash and report
                let h = self.hash(data.clone());
                let fname = entry.display().to_string();

                // Saving the hash for later reporting
                wasm_hashes.lock().unwrap().entry(h).and_modify(|v: &mut Vec<_>| v.push(fname.clone()))
                .or_insert(vec![fname.clone()]);

                let module = wasmtime::Module::new(&self.engine, &data)
                        .expect("the mutated Wasm should be valid");
                        
                let obj = module.serialize().expect("Wasm module could not be serialized");

                // Save the obj to the file system as well
                if self.save_files {
                    let fname = format!("{}/{}.aot", &newfolder, entryindex);
                    std::fs::write(&fname, &obj).expect("Aot file could be written to filesystem");
                }
                
                let low_hash = self.hash(obj.clone());

                // Saving the hash for later reporting
                low_hashes.lock().unwrap().entry(low_hash).and_modify(|v: &mut Vec<_>| v.push(fname.clone()))
                .or_insert(vec![fname]);
            }
        }
    }

    fn generate(&self, wasm_idx: usize, seed: u64, artifact_folder: String) {

        let mut wasmmutate = WasmMutate::default();
        let (name, data) = &self.corpus[wasm_idx];

        log::debug!("Wasm input {:?}", name);
        let mut wasm = data.clone();
        // Generate until thread is interrupted
        let mut rotations = 0;
        
        let to_write = Arc::new(Mutex::new(Vec::new()));
        let to_write_clone = to_write.clone();
        let finish_writing = AtomicBool::new(false);
        let finish_writing_wrap = Arc::new(finish_writing);
        let finish_writing_wrap_clone = finish_writing_wrap.clone();
        let name_clone = name.clone();
        // Spawn Wasm to file writer
        let encoder = std::thread::spawn(move ||{
            let mut counter = 0;
            

            while !finish_writing_wrap_clone.load(Relaxed) {
                // pop from worklist
                match  to_write_clone
                    .lock()
                    .unwrap()
                    .pop() {
                        Some(wasm) => {
                            //Write down
                            std::fs::write(format!("{}/mutated.{}.wasm", &artifact_folder, counter), &wasm).expect("Something went wrong");
                            counter += 1;
                        },
                        None => {

                        },
                }
            }
            log::debug!("Writing down pending mutated binaries!");
            // Then write pending wasms
            while let Some(wasm) = to_write_clone.lock().unwrap().pop(){
                std::fs::write(format!("cor/mutated.{}.wasm", counter), &wasm).expect("Something went wrong");
                counter += 1;
            }
        });

        
        // Save the original as well
        to_write
        .lock()
        .unwrap()
        .push(wasm.clone());

        let mut rng = SmallRng::seed_from_u64(seed);
        while !self.timeout_reached.load(Relaxed) {
            wasmmutate.seed(rng.gen());
            wasmmutate.preserve_semantics(true);

            // First stage, generate and return the mutated
            let mutated = match wasmmutate.run(&wasm) {
                Ok(mutated) => mutated,
                Err(e) => match e {
                    wasm_mutate::Error::NoMutationsApplicable => wasm,
                    _ => panic!("Invalid mutation process"),
                },
            };

            let mut validator = wasmparser::Validator::new();
            match validator.validate_all(&mutated.clone()) {
                Ok(_) => {
                    // send the bytes for storage and compilation to another worker
                    to_write
                        .lock()
                        .unwrap()
                        .push(mutated.clone());
                    wasm = mutated;
                },
                Err(_) => panic!(""),
            }

            rotations += 1;
        }
        
        // Send signal to encoder to break infinite consumer loop
        finish_writing_wrap.store(true, SeqCst);
        encoder.join().expect("Process exited or is not available");
    }

    fn hash(&self, data: Vec<u8>) -> u64 {
        // Default hasher
        let mut h = DefaultHasher::default();
        data.hash(&mut h);
        h.finish()
    }
}
