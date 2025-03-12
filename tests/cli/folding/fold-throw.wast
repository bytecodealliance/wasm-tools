;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-throw.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (tag $e0 (param i32))
  (tag $e1 (param f32 i32))

  (func i32.const 1 throw $e0)
  (func f32.const 2 i32.const 1 throw $e1))
