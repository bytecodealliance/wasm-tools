;; RUN: wast \
;;      --assert default \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm2,function-references,gc,tail-call \
;;      tests/testsuite/proposals/gc/ref.wast
