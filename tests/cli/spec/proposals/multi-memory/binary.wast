;; RUN: wast \
;;      --assert default \
;;      --assert permissive \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm2,multi-memory \
;;      tests/testsuite/proposals/multi-memory/binary.wast
