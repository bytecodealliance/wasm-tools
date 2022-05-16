set -ex

cargo test --test roundtrip

rm -rf cov
mkdir cov

export LLVM_PROFILE_FILE="`pwd`/cov/coverage-%m.profraw"
export RUSTFLAGS="-C instrument-coverage"
export CARGO_TARGET_DIR=`pwd`/target/coverage

cargo test --test roundtrip

llvm-profdata merge -sparse cov/coverage-*.profraw -o cov/coverage.profdata
llvm-cov show -Xdemangler=rustfilt \
  ./target/coverage/debug/deps/roundtrip-92d636af98ebdc72 \
  --format=html \
  --ignore-filename-regex='/.cargo/registry' \
  --ignore-filename-regex='/rustc/' \
  --instr-profile=cov/coverage.profdata \
  --show-line-counts-or-regions \
  --show-instantiations \
  --show-regions \
  --output-dir=cov

