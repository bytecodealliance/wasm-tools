use anyhow::Context;
use clap::Parser;
use core::sync::atomic::Ordering::{Relaxed, SeqCst};
use rand::Rng;
use rand::{rngs::SmallRng, SeedableRng};
use std::collections::hash_map::DefaultHasher;
use std::ffi::OsStr;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicBool;
use std::sync::Mutex;
use std::time::Duration;
use std::{collections::HashMap, sync::Arc};
use std::{panic, process};
use wasm_mutate::WasmMutate;
use wasmtime::{Config, Engine, OptLevel};

#[derive(Debug)]
enum ParsingOptLevelError {
    Parsing(String),
}

impl Display for ParsingOptLevelError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingOptLevelError::Parsing(msg) => f.write_str(msg),
        }
    }
}

impl std::error::Error for ParsingOptLevelError {}

/// Parses the list of optimizations to set in wasmtime
fn parse_optimization_types(s: &str) -> Result<OptLevel, ParsingOptLevelError> {
    match s {
        "O0" => Ok(OptLevel::None),
        "O2" => Ok(OptLevel::Speed),
        "Os" => Ok(OptLevel::SpeedAndSize),
        _ => Err(ParsingOptLevelError::Parsing(format!(
            "Invalid optimization type {}",
            s
        ))),
    }
}

/// # Stats for wasm-mutate.
///
/// `wasm-mutate-stats` is a benchmark to illustrate how many mutation of the same input Wasm can be generated
/// per time. This tool reads all Wasm in a folder (only in the root level) and for each Wasm file, it will iteratively
/// run the wasm-mutate tool until a timeout is reached. Every iteration mutates the previous mutated Wasm.
///
/// The tool works in three main stages:
/// - Generate the mutations until timeout is reached.
/// - Compile the generated Wasm with the wasmtime engine to check for the preservation of the mutations
///   at low level.
/// - Generate a report.
///
/// The final report illustrates, for each Wasm file in the input folder: how many unique mutations were generated,
/// and for each wasmtime configuration, how many of them still preserved.
///
/// All the generated Wasm files are saved into al folder named **artifacts** in the same level of the input
/// folder.
///
/// ## Example
/// `wasm-mutate-stats corpus 300 -c O0 -c O2 --triple x86_64-apple-darwin`
///
/// The previous example will run wasm-mutate-stats over the folder corpus, with a timeout of 300 seconds,
/// checking preservation for O0(No optimizations) and O2(size and speed optimization), using architecture
/// triple `x86_64-apple-darwin`.
///
#[derive(Parser)]
struct Options {
    /// The input folder that contains the Wasm binaries.
    input: PathBuf,
    /// The timeout, 0 to wait for keyboard interrupt
    timeout: u64,
    /// The seed of the random mutation, 0 by default
    #[clap(short = 's', long = "seed")]
    seed: u64,
    /// List of engine configurations.
    /// Allowed values: [O0, O2, Os]
    /// If it is not set, the default configuration of wasmtime will be used
    #[clap(short = 'c', long = "compilation-configs", parse(try_from_str=parse_optimization_types) )]
    configs: Option<Vec<OptLevel>>,
    /// Target triple during compilation, e.g. "x86_64-apple-darwin"
    #[clap(short = 'a', long = "triple")]
    triple: Option<String>,
    /// Only generate report, if this option is set, it will skip the generation
    #[clap(short = 'k', long = "skip")]
    skip_generation: bool,
}

struct State {
    // Tuples of filenames and the corresponding byte stream
    corpus: Vec<(PathBuf, Vec<u8>)>,
    // Engine used to compile the mutated Wasm
    engines: Vec<(wasmtime::Engine, OptLevel)>,
    // timeout_reached state
    timeout_reached: AtomicBool,
    // To avoid generation of mutations, just print report over prexisting folders
    do_not_generate: bool,
    // seed
    seed: u64,
    // timeout to stop generation
    timeout: u64,
}

fn main() -> anyhow::Result<()> {
    // Init logs
    env_logger::init();

    let opts = Options::parse();
    let timeout = opts.timeout;
    let seed = opts.seed;

    let triple = &opts.triple;
    let engines = opts
        .configs
        .map(|configs| {
            configs
                .iter()
                .map(|optlevel| {
                    let mut config = Config::default();
                    config.cranelift_opt_level(optlevel.clone());
                    if let Some(triple) = triple {
                        config.target(triple).context("Invalid target")?;
                    }
                    Ok((
                        Engine::new(&config).context("Engine could not be initialized")?,
                        optlevel.clone(),
                    ))
                })
                .collect::<anyhow::Result<Vec<(Engine, OptLevel)>>>()
        })
        .or_else(|| Some(Ok(vec![]))) // To not compile the mutated Wasm is the default option, if no configs are passed
        .context("Failed to created configurations")?;
    // Start benchmarking
    Arc::new(State::new(
        opts.input,
        engines?,
        opts.skip_generation,
        seed,
        timeout,
    )?)
    .run()?;
    Ok(())
}

impl State {
    pub fn new(
        input_folder: PathBuf,
        engines: Vec<(Engine, OptLevel)>,
        skip_generation: bool,
        seed: u64,
        timeout: u64,
    ) -> anyhow::Result<Self> {
        // Read corpus folder
        let mut corpus = Vec::with_capacity(2000);
        let entries = std::fs::read_dir(input_folder.clone())
            .with_context(|| format!("failed to read directory {}", input_folder.display()))?;
        for e in entries {
            let e = e.context("failed to read dir entry")?;
            if e.file_type()
                .context("File type could not be retrieved")?
                .is_file()
            {
                let seed = std::fs::read(e.path()).context("failed to read file content")?;
                if e.path().extension() == Some(OsStr::new("wasm")) {
                    corpus.push((e.path(), seed));
                }
            }
        }

        Ok(State {
            corpus,
            timeout_reached: AtomicBool::new(false),
            // Add many engines as configurations are passed
            engines,
            do_not_generate: skip_generation,
            seed,
            timeout,
        })
    }

    fn run(self: &Arc<Self>) -> anyhow::Result<()> {
        if self.corpus.is_empty() {
            anyhow::bail!("No Wasm files into the directory");
        }
        // create a folder to save the mutated files
        let artifact_folders = self
            .corpus
            .iter()
            .map(|(name, _)| {
                let filename = name.file_name().with_context(|| {
                    format!("File name could not be retrieved for {}", name.display())
                })?;
                let newfolder = self
                    .get_parent_folders(name.clone())?
                    .join(PathBuf::from("artifacts"))
                    .join(filename);

                std::fs::create_dir_all(&newfolder).with_context(|| {
                    format!(
                        "Artifacts folder could not be created {}",
                        newfolder.display()
                    )
                })?;
                println!("Artifacts saved at {:?}", newfolder);
                Ok(newfolder)
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        if !self.do_not_generate {
            let elapsed = self.generate_wasm_files(&artifact_folders)?;
            // Second stage, compile
            for (wasmidx, (name, _)) in self.corpus.iter().enumerate() {
                let filename = name
                    .file_name()
                    .with_context(|| format!("Unable to get file name for {}", name.display()))?;
                let artifacts_folder = self
                    .get_parent_folders(name.clone())?
                    .join("artifacts")
                    .join(filename);

                // Read dir looking for Wasm
                let entries = std::fs::read_dir(&artifacts_folder).with_context(|| {
                    format!("Failed to read dir {}", artifacts_folder.display())
                })?;

                let mut worklist = Vec::new();
                for e in entries {
                    let e = e.context("Failed to read dir entry")?;
                    if e.file_type()
                        .context("File type could not be retrieved")?
                        .is_file()
                        && e.path().extension() == Some(OsStr::new("wasm"))
                    {
                        worklist.push(e.path());
                    }
                }
                let files_count = worklist.clone().len();
                println!(
                    "{} files generated (+ original) in {}.{:03} seconds for {}",
                    files_count,
                    elapsed.as_secs(),
                    elapsed.subsec_millis(),
                    name.display()
                );
                // Create shared worklist
                // This is the second stage, to compile all saved Wasm
                if !self.engines.is_empty() {
                    self.compile_and_save(&worklist, &artifact_folders[wasmidx])?;
                }
            }
        }
        for (wasmidx, (name, _)) in self.corpus.iter().enumerate() {
            let filename = name.file_name().with_context(|| {
                format!("File name could not be retrieved for {}", name.display())
            })?;

            println!("Input wasm \"{:?}\"", filename);
            let artifacts_folder = self
                .get_parent_folders(name.clone())?
                .join("artifacts")
                .join(filename);

            // Read dir looking for Wasm
            let entries = std::fs::read_dir(&artifacts_folder)
                .with_context(|| format!("Failed to read dir {}", artifacts_folder.display()))?;
            let mut worklist = Vec::new();
            for e in entries {
                let e = e.context("failed to read dir entry")?;
                if e.file_type()
                    .context("File type could not be retrieved")?
                    .is_file()
                    && e.path().extension() == Some(OsStr::new("wasm"))
                {
                    worklist.push(e.path());
                }
            }
            self.print_report(&artifact_folders[wasmidx], &worklist)?;
        }
        Ok(())
    }

    fn get_parent_folders(&self, path: PathBuf) -> anyhow::Result<PathBuf> {
        Ok(path
            .parent()
            .context("corpus files always have a parent")?
            .parent()
            .context("corpus files always have a grandparent")?
            .to_path_buf())
    }

    fn generate_wasm_files(
        self: &Arc<Self>,
        artifact_folders: &[PathBuf],
    ) -> anyhow::Result<Duration> {
        let generation_start = std::time::Instant::now();

        let threads = (0..self.corpus.len())
            .into_iter()
            .map(|usize| {
                let state = self.clone();
                let artifact_folder = artifact_folders[usize].clone();
                std::thread::spawn(move || state.generate(usize, state.seed, &artifact_folder))
            })
            .collect::<Vec<_>>();

        // Sleep the main thread as many seconds are defined in the timeout
        std::thread::sleep(std::time::Duration::new(self.timeout, 0));
        // Send termination signal, false || true = true
        self.timeout_reached.fetch_or(true, SeqCst);

        for thread in threads {
            // Expect all threads here
            thread.join().expect("Thread panicked!")?;
        }
        let elapsed = generation_start.elapsed();

        Ok(elapsed)
    }

    fn print_report(&self, artifact_folder: &Path, worklist: &[PathBuf]) -> anyhow::Result<()> {
        let mut wasm_hashes = HashMap::new();

        for w in worklist {
            let data = std::fs::read(w).expect("Wasm file could not be read");
            let h = self.hash(&data);

            wasm_hashes
                .entry(h)
                .and_modify(|f: &mut Vec<_>| f.push(w.clone()))
                .or_insert_with(|| vec![w.clone()]);
        }
        let whashes_len = wasm_hashes.len();
        println!(
            "\t{}/{} unique Wasm ({:.2}%)",
            whashes_len,
            worklist.len(),
            100.0 * whashes_len as f64 / worklist.len() as f64
        );

        for (_, optlevel) in &self.engines {
            self.print_report_compilation(artifact_folder, optlevel, whashes_len)?
        }

        Ok(())
    }

    fn print_report_compilation(
        &self,
        artifact_folder: &Path,
        optlevel: &OptLevel,
        number_of_wasm: usize,
    ) -> anyhow::Result<()> {
        let mut hashes = HashMap::new();
        //let low_hashes = HashMap::new();
        let objfolder = artifact_folder.join("obj").join(format!("{:?}", optlevel));
        let entries = std::fs::read_dir(&objfolder)
            .with_context(|| format!("failed to read dir {}", objfolder.display()))?;

        for e in entries {
            let e = e.context("failed to read dir entry")?;
            if e.file_type()
                .context("File type could not be retrieved")?
                .is_file()
                && e.path().extension() == Some(OsStr::new("obj"))
            {
                let data = std::fs::read(e.path()).with_context(|| {
                    format!("Object file could not be read {}", e.path().display())
                })?;
                let h = self.hash(&data);
                hashes
                    .entry(h)
                    .and_modify(|f: &mut Vec<_>| f.push(e.path()))
                    .or_insert_with(|| vec![e.path()]);
            }
        }
        let hashes_len = hashes.len();
        println!(
            "\t\t{}/{} unique objects for opt config ({:?}) ({:.2}%)",
            hashes_len,
            number_of_wasm,
            optlevel,
            100.0 * hashes_len as f64 / number_of_wasm as f64
        );

        Ok(())
    }

    fn compile_and_save(&self, worklist: &[PathBuf], artifact_folder: &Path) -> anyhow::Result<()> {
        for (_, config) in &self.engines {
            let newfolder = artifact_folder.join("obj").join(format!("{:?}", config));
            std::fs::create_dir_all(&newfolder).with_context(|| {
                format!(
                    "Artifacts folder {} could not be created",
                    newfolder.display()
                )
            })?;
        }

        for (entryidx, entry) in worklist.iter().enumerate() {
            let data = std::fs::read(entry).context("failed to read seed file")?;
            // Compile each configuration
            // Spwan compilation
            for (engine, optlevel) in &self.engines {
                let module = wasmtime::Module::new(engine, &data).with_context(|| {
                    format!(
                        "The mutated Wasm should be valid, wasm file {:?}, config {:?}",
                        entry.display(),
                        optlevel,
                    )
                })?;

                let obj = module
                    .serialize()
                    .context("Wasm module could not be serialized")?;

                // Save the obj to the file system as well

                let filename = artifact_folder
                    .join("obj")
                    .join(format!("{:?}", optlevel))
                    .join(format!("{}.obj", entryidx));
                std::fs::write(&filename, &obj)
                    .context("Aot file could be written to filesystem")?;
            }
        }
        Ok(())
    }

    fn generate(
        self: &Arc<Self>,
        wasm_idx: usize,
        seed: u64,
        artifact_folder: &Path,
    ) -> anyhow::Result<()> {
        let mut wasmmutate = WasmMutate::default();
        let (name, data) = &self.corpus[wasm_idx];
        println!("Wasm input {}", name.display());
        let mut wasm = data.clone();
        let artifact_folder_cp = artifact_folder.to_path_buf();
        // Generate until thread is interrupted
        let to_write = Arc::new(Mutex::new(Vec::new()));
        let to_write_clone = to_write.clone();
        let finish_writing = AtomicBool::new(false);
        let finish_writing_wrap = Arc::new(finish_writing);
        let finish_writing_wrap_clone = finish_writing_wrap.clone();
        // Spawn Wasm to file writer
        let encoder = std::thread::spawn(move || -> anyhow::Result<()> {
            let mut counter = 0;

            while !finish_writing_wrap_clone.load(Relaxed) {
                // pop from worklist
                if let Some(wasm) = to_write_clone.lock().unwrap().pop() {
                    let filename = artifact_folder_cp.join(format!("mutated.{}.wasm", counter));
                    std::fs::write(filename, &wasm).context("Failed to write mutated wasm")?;
                    counter += 1;
                }
            }
            eprintln!("Writing down pending mutated binaries!");
            // Then write pending wasms
            while let Some(wasm) = to_write_clone.lock().unwrap().pop() {
                let filename = artifact_folder_cp.join(format!("mutated.{}.wasm", counter));

                std::fs::write(filename, &wasm).context("Failed to write mutated wasm")?;
                counter += 1;
            }

            Ok(())
        });

        // Save the original as well
        to_write.lock().unwrap().push(wasm.clone());

        let mut rng = SmallRng::seed_from_u64(seed);
        let wasmcp = wasm.clone();

        while !self.timeout_reached.load(Relaxed) {
            let seed = rng.gen();
            wasmmutate.seed(seed);
            wasmmutate.fuel(1000);
            wasmmutate.preserve_semantics(true);

            // Set a panic hook since some errors are not carried out, this looks more like a patch
            let self_clone = self.clone();
            let artifact_clone = artifact_folder.to_path_buf();
            let finish_writing_wrap_clone2 = finish_writing_wrap.clone();

            let data_clone = wasm.clone();
            panic::set_hook(Box::new(move |panic_info| {
                // invoke the default handler and exit the process
                println!("Internal undhandled panicking \n{:?}!", panic_info);
                // stop generator
                finish_writing_wrap_clone2.store(true, SeqCst);
                // report current crash
                self_clone.save_crash(&data_clone, None, seed, &artifact_clone);
                process::exit(1);
            }));

            // First stage, generate and return the mutated
            let it = match wasmmutate.run(&wasmcp) {
                Ok(it) => it,
                Err(e) => match e.kind() {
                    wasm_mutate::ErrorKind::NoMutationsApplicable => {
                        Box::new(std::iter::once(Ok(wasm.clone())))
                    }
                    _ => {
                        // Stop writing worker
                        finish_writing_wrap.store(true, SeqCst);
                        // Saving report
                        let h1 = self.hash(&wasm);
                        self.save_crash(&wasm, None, seed, artifact_folder)?;
                        anyhow::bail!(format!("Mutation invalid for entry {} seed {}.\n Crashing wasm is saved at crashes folder with name '<seed>.original.wasm'", h1, seed))
                    }
                },
            };

            for mutated in it {
                let mut validator = wasmparser::Validator::new();
                match mutated {
                    Ok(mutated) => {
                        match validator.validate_all(&mutated.clone()) {
                            Ok(_) => {
                                // send the bytes for storage and compilation to another worker
                                to_write.lock().unwrap().push(mutated.clone());
                                // FIXME, this will always set wasm to the result of the
                                // last mutation
                                wasm = mutated;
                            }
                            Err(_) => {
                                // Stop writing worker
                                finish_writing_wrap.store(true, SeqCst);
                                let h1 = self.hash(&wasm);
                                let h2 = self.hash(&mutated);
                                self.save_crash(&wasm, Some(&mutated), seed, artifact_folder)?;
                                anyhow::bail!(format!(
                                    "All generated Wasm should be valid {} -> {}, seed {}",
                                    h1, h2, seed
                                ));
                            }
                        }
                    }
                    Err(e) => {
                        // Stop writing worker
                        finish_writing_wrap.store(true, SeqCst);
                        self.save_crash(&wasm, None, seed, artifact_folder)?;
                        anyhow::bail!(format!("Error during module writing {:?}", e));
                    }
                }
            }
        }

        // Send signal to encoder to break infinite consumer loop
        finish_writing_wrap.store(true, SeqCst);
        encoder
            .join()
            .expect("Process exited or is not available")?;

        Ok(())
    }

    fn save_crash(
        &self,
        wasm: &[u8],
        mutated: Option<&[u8]>,
        seed: u64,
        artifacts_folder: &Path,
    ) -> anyhow::Result<()> {
        println!("Saving crash");
        let newfolder = artifacts_folder.join("crashes");
        std::fs::create_dir_all(&newfolder).with_context(|| {
            format!("Crash folder could not be created {}", newfolder.display())
        })?;

        let newfile = newfolder.join(format!("{}.original.wasm", seed));

        std::fs::write(&newfile, wasm)?;

        // Saving the mutation if one

        if let Some(mutated) = mutated {
            let newfile = newfolder.join(format!("{}.mutated.wasm", seed));
            std::fs::write(newfile, mutated)?;
        }

        println!("Crash saved with name {}", newfile.display());
        Ok(())
    }

    fn hash(&self, data: &[u8]) -> u64 {
        // Default hasher
        let mut h = DefaultHasher::default();
        data.hash(&mut h);
        h.finish()
    }
}
