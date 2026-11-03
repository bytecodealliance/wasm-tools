set -ex

tmp=../../../target/dl/check
mkdir -p $tmp
bash ./build.sh $tmp
for lib in libdl32.so libdl64.so; do
  if ! diff $tmp/$lib ../$lib; then
    echo "$lib is out-of-date; please run crates/wit-component/dl/build.sh to update it">&2
    exit 1
  fi
done
