#[macro_use]
extern crate criterion;

use criterion::Criterion;
use std::fs::{read_dir, File};
use std::io::Read;
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

fn read_file_data(path: &PathBuf) -> Vec<u8> {
    let mut data = Vec::new();
    let mut f = File::open(path).ok().unwrap();
    f.read_to_end(&mut data).unwrap();
    data
}

fn collect_test_files<P>(path: P) -> Vec<Vec<u8>>
where
    P: AsRef<Path>,
{
    let mut file_contents: Vec<Vec<u8>> = vec![];
    for entry in read_dir(path).expect("cannot find the benchmark test files") {
        let dir = entry.unwrap();
        if !dir.file_type().unwrap().is_file() {
            continue;
        }
        file_contents.push(read_file_data(&dir.path()));
    }
    file_contents
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
