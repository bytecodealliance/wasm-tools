;; RUN: wast \
;;      --assert default \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm2,function-references,tail-call \
;;      tests/testsuite/proposals/function-references/br_on_non_null.wast
