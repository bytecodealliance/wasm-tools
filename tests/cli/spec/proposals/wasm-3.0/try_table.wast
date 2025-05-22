;; RUN: wast \
;;      --assert default \
;;      --assert permissive \
;;      --assert no-test-folded \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm3 \
;;      tests/testsuite/proposals/wasm-3.0/try_table.wast
