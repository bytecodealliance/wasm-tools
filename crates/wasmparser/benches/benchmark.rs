#[macro_use]
extern crate criterion;

use criterion::Criterion;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use wasmparser::{
    validate, OperatorValidatorConfig, Parser, ParserState, ValidatingParser,
    ValidatingParserConfig, WasmDecoder,
};

const VALIDATOR_CONFIG: Option<ValidatingParserConfig> = Some(ValidatingParserConfig {
    operator_config: OperatorValidatorConfig {
        enable_threads: true,
        enable_reference_types: true,
        enable_simd: true,
        enable_bulk_memory: true,
        enable_multi_value: true,
        enable_tail_call: true,
    },
});

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

/// Read a `.wat` formatted benchmark test file as benchmark input.
fn read_wat_module(path: &PathBuf) -> BenchmarkInput {
    let encoded_wasm =
        wat::parse_file(path).expect("encountered error while parsing `.wat` file into `.wasm`");
    BenchmarkInput::new(path.clone(), encoded_wasm)
}

/// Read a `.wast` formatted benchmark test file as benchmark input.
///
/// We simply pull out all the module directives of the `.wast` file and return them.
fn read_wast_module(path: &PathBuf) -> Vec<BenchmarkInput> {
    let mut inputs = Vec::new();
    let mut wast_file = fs::File::open(path).ok().unwrap();
    let mut wast_file_contents = String::new();
    use io::Read as _;
    wast_file.read_to_string(&mut wast_file_contents).unwrap();
    let parse_buffer = wast::parser::ParseBuffer::new(&wast_file_contents).unwrap();
    'outer: while let Ok(directive) = wast::parser::parse::<wast::WastDirective>(&parse_buffer) {
        match directive {
            wast::WastDirective::Module(mut module) => {
                let encoded_wasm = module
                    .encode()
                    .expect("encountered error while encoding the Wast module into Wasm");
                inputs.push(BenchmarkInput::new(path.clone(), encoded_wasm));
            }
            _ => continue 'outer,
        }
    }
    inputs
}

/// Visits all directory entries within the given directory path.
///
/// - `pred` can be used to filter some directories, e.g. all directories named
///   `"proposals"`.
/// - `cb` is the callback that is being called for every file within the non
///   filtered and visited directories.
fn visit_dirs<P, F>(dir: &Path, pred: &P, cb: &mut F) -> io::Result<()>
where
    P: Fn(&fs::DirEntry) -> bool,
    F: FnMut(&fs::DirEntry),
{
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() && pred(&entry) {
                visit_dirs(&path, pred, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}

/// Returns a vector of all found benchmark input files under the given directory.
///
/// Benchmark input files can be `.wat` or `.wast` formatted files.
/// For `.wast` files we pull out all the module directives and run them in the benchmarks.
fn collect_test_files<P>(path: P) -> Vec<BenchmarkInput>
where
    P: AsRef<Path>,
{
    let mut file_contents: Vec<BenchmarkInput> = vec![];
    visit_dirs(
        path.as_ref(),
        &|_| true, // accept all benchmarks
        &mut |dir_entry| {
            let ext: Option<String> = dir_entry
                .path()
                .extension()
                .and_then(|ext| ext.to_str().map(|str| str.to_string()));
            match ext.as_ref().map(|string| string.as_str()) {
                Some("wat") => file_contents.push(read_wat_module(&dir_entry.path())),
                Some("wast") => {
                    for wasm_module in read_wast_module(&dir_entry.path()) {
                        file_contents.push(wasm_module)
                    }
                }
                _ => (),
            }
        },
    )
    .expect("encountered error while reading test directory");
    file_contents
}

/// Reads the input given the Wasm parser or validator.
///
/// The `path` specifies which benchmark input file we are currently operating on
/// so that we can report better errors in case of failures.
fn read_all_wasm<'a, T>(path: &PathBuf, mut d: T)
where
    T: WasmDecoder<'a>,
{
    loop {
        match *d.read() {
            ParserState::Error(ref e) => {
                panic!("unexpected error while reading Wasm at {:?}: {}", path, e)
            }
            ParserState::EndWasm => return,
            _ => (),
        }
    }
}

fn it_works_benchmark(c: &mut Criterion) {
    let mut inputs = collect_test_files("../../testsuite");
    c.bench_function("it works benchmark", move |b| {
        for input in &mut inputs {
            b.iter(|| read_all_wasm(&input.path, Parser::new(input.wasm.as_slice())));
        }
    });
}

fn validator_not_fails_benchmark(c: &mut Criterion) {
    let mut inputs = collect_test_files("../../testsuite");
    c.bench_function("validator no fails benchmark", move |b| {
        for input in &mut inputs {
            b.iter(|| {
                read_all_wasm(
                    &input.path,
                    ValidatingParser::new(input.wasm.as_slice(), VALIDATOR_CONFIG),
                )
            });
        }
    });
}

fn validate_benchmark(c: &mut Criterion) {
    let mut inputs = collect_test_files("../../testsuite");
    c.bench_function("validate benchmark", move |b| {
        for input in &mut inputs {
            b.iter(|| validate(input.wasm.as_slice(), VALIDATOR_CONFIG));
        }
    });
}

criterion_group!(
    benchmark,
    it_works_benchmark,
    validator_not_fails_benchmark,
    validate_benchmark
);
criterion_main!(benchmark);
