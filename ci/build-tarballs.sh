#!/bin/bash

set -ex

platform=$1
target=$CARGO_BUILD_TARGET

rm -rf tmp
mkdir tmp
mkdir -p dist

tag=$(./ci/print-current-version.sh)
bin_pkgname=wasm-tools-$tag-$platform

mkdir tmp/$bin_pkgname
cp LICENSE-* README.md tmp/$bin_pkgname

case $platform in
  *-windows)
    fmt=zip
    cp target/$target/release/wasm-tools.exe tmp/$bin_pkgname
    ;;
  wasm*)
    fmt=tar
    cp target/$target/release/wasm-tools.wasm tmp/$bin_pkgname
    ;;
  *)
    fmt=tar
    cp target/$target/release/wasm-tools tmp/$bin_pkgname
    ;;
esac


mktarball() {
  dir=$1
  if [ "$fmt" = "tar" ]; then
    tar czvf dist/$dir.tar.gz -C tmp $dir
  else
    # Note that this runs on Windows, and it looks like GitHub Actions doesn't
    # have a `zip` tool there, so we use something else
    (cd tmp && 7z a ../dist/$dir.zip $dir/)
  fi
}

mktarball $bin_pkgname
