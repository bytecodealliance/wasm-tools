#!/usr/bin/env bash
set -e

# record current bench results
cargo bench --bench benchmark -- --noplot --save-baseline after

# switch to main branch and record its bench results
git checkout main && \
cargo bench --bench benchmark -- --noplot --save-baseline before

# compare
cargo install critcmp --force && \
critcmp before after
