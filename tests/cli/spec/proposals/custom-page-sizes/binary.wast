;; RUN: wast \
;;      --assert default \
;;      --assert permissive \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm3,custom-page-sizes \
;;      tests/testsuite/proposals/custom-page-sizes/binary.wast
