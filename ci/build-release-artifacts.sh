#!/bin/bash

# A script to build the release artifacts of this repository into the `target`
# directory. Note that this script only produces the artifacts through Cargo and
# doesn't package things up. That's intended for the `build-tarballs.sh` script.

set -ex

# If `$DOCKER_IMAGE` is set then run the build inside of that docker container
# instead of on the host machine. In CI this uses `./ci/docker/*/Dockerfile` to
# have precise glibc requirements for Linux platforms for example.
if [ "$DOCKER_IMAGE" != "" ]; then
  if [ -f "$DOCKER_IMAGE" ]; then
    docker build --tag build-image --file $DOCKER_IMAGE ci/docker
    DOCKER_IMAGE=build-image
  fi

  # Inherit the environment's rustc and env vars related to cargo/rust, and then
  # otherwise re-execute ourselves and we'll be missing `$DOCKER_IMAGE` in the
  # container so we'll continue below.
  exec docker run --interactive \
    --volume `pwd`:`pwd` \
    --volume `rustc --print sysroot`:/rust:ro \
    --workdir `pwd` \
    --interactive \
    --env-file <(env | grep 'CARGO\|RUST') \
    $DOCKER_IMAGE \
    bash -c "PATH=\$PATH:/rust/bin RUSTFLAGS=\"\$RUSTFLAGS \$EXTRA_RUSTFLAGS\" `pwd`/$0 $*"
fi

# Default build flags for release artifacts. Leave debugging for
# builds-from-source which have richer information anyway, and additionally the
# CLI won't benefit from catching unwinds.
export CARGO_PROFILE_RELEASE_STRIP=debuginfo
export CARGO_PROFILE_RELEASE_PANIC=abort

exec cargo build --release
