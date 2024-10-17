;; RUN: validate % -f wasm1 -f simd

(module
  (import "" "" (global v128))
)
