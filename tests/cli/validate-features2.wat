;; RUN: validate --features=-all,simd %

(module
  (import "x" "y" (global v128))
)

