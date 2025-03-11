;; RUN: wast \
;;      --assert default \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm2,relaxed-simd \
;;      tests/testsuite/proposals/relaxed-simd/i16x8_relaxed_q15mulr_s.wast
