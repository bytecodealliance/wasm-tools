;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-basic.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (func $fold-binary (result i32)
    i32.const 1
    i32.const 2
    i32.add)

  (func $fold-binary-chain (result i32)
    i32.const 1
    i32.const 1
    i32.const 1
    i32.add
    i32.sub)

  (func $fold-compare (result i32)
    f32.const 1
    f32.const 2
    f32.le)

  (func $fold-unary (result f32)
    f32.const 1
    f32.neg
    f32.neg)

  (func $fold-convert (result i64)
    f64.const 0
    f32.demote_f64
    i32.trunc_f32_s
    i64.extend_i32_s)

  (func $fold-select (result f32)
    f32.const 1
    f32.const 2
    i32.const 3
    select))
