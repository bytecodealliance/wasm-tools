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

fn read_file_data(path: &PathBuf) -> Vec<u8> {
    wat::parse_file(path).expect("encountered error while parsing Wasm text format file")
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

fn collect_test_files<P>(path: P) -> Vec<Vec<u8>>
where
    P: AsRef<Path>,
{
    let mut file_contents: Vec<Vec<u8>> = vec![];
    visit_dirs(
        path.as_ref(),
        &|dir_entry| dir_entry.file_name().to_str() != Some("proposals"),
        &mut |dir_entry| {
            if dir_entry.path().extension().and_then(|ext| ext.to_str()) == Some("wast") {
                file_contents.push(read_file_data(&dir_entry.path()))
            }
        },
    )
    .expect("encountered error while reading test directory");
    file_contents
}

fn read_all_wasm<'a, T>(mut d: T)
where
    T: WasmDecoder<'a>,
{
    loop {
        match *d.read() {
            ParserState::Error(ref e) => panic!("unexpected error while reading Wasm: {}", e),
            ParserState::EndWasm => return,
            _ => (),
        }
    }
}

fn it_works_benchmark(c: &mut Criterion) {
    let mut data = collect_test_files("../../tests");
    c.bench_function("it works benchmark", move |b| {
        for d in &mut data {
            b.iter(|| read_all_wasm(Parser::new(d.as_slice())));
        }
    });
}

fn validator_not_fails_benchmark(c: &mut Criterion) {
    let mut data = collect_test_files("../../tests");
    c.bench_function("validator no fails benchmark", move |b| {
        for d in &mut data {
            b.iter(|| read_all_wasm(ValidatingParser::new(d.as_slice(), VALIDATOR_CONFIG)));
        }
    });
}

fn validate_benchmark(c: &mut Criterion) {
    let mut data = collect_test_files("../../tests");
    c.bench_function("validate benchmark", move |b| {
        for d in &mut data {
            b.iter(|| validate(&d, VALIDATOR_CONFIG));
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
