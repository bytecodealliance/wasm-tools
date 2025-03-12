;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-local-getset.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (func $fold-local-get (result f32)
    (local f32 f32 f64 f64)
    local.get 0
    local.get 1
    f32.add
    local.get 2
    local.get 3
    f64.add
    f32.demote_f64
    f32.add)

  (func $fold-local-set
    (local i64 i32)
    i64.const 1
    i64.const 2
    i64.const 3
    i64.xor
    i64.xor
    local.set 0
    i32.const 4
    local.set 1)

  (func $fold-local-tee (result i32)
    (local i32 i32)
    i32.const 1
    local.tee 0
    i32.const 2
    local.tee 1
    i32.add)
  )
