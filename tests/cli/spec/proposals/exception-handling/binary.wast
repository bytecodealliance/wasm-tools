;; RUN: wast \
;;      --assert default \
;;      --assert permissive \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm2,exceptions,tail-call \
;;      tests/testsuite/proposals/exception-handling/binary.wast
