;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-load-store64.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (memory i64 1)
  (func $fold-load
    i64.const 1
    i32.load
    drop
    i64.const 2
    i32.load
    drop)

  (func $fold-store
    i64.const 1
    i64.load
    f32.const 2
    f32.store)

  (func $fold-memory-size (result i32)
    memory.size
    i64.const 1
    i64.add
    drop
    i32.const 2)

  (func $fold-memory-grow (result i32)
    i64.const 1
    i64.const 2
    memory.grow
    i64.lt_s))
