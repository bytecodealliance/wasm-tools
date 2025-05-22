;; RUN: wast \
;;      --assert default \
;;      --assert permissive \
;;      --assert no-test-folded \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm2,exceptions,tail-call \
;;      tests/testsuite/proposals/exception-handling/try_table.wast
