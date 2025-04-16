;; RUN: wast \
;;      --assert default \
;;      --assert permissive \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm2,function-references,tail-call \
;;      tests/testsuite/proposals/function-references/select.wast
