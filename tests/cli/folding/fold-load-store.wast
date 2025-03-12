;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-load.store.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (memory 1)
  (func $fold-load
    i32.const 1
    i32.load
    drop
    i32.const 2
    i32.load
    drop)

  (func $fold-store
    i32.const 1
    i32.load
    f32.const 2
    f32.store)

  (func $fold-memory-size (result i32)
    memory.size
    i32.const 1
    i32.add
    drop
    i32.const 2)

  (func $fold-memory-grow (result i32)
    i32.const 1
    i32.const 2
    memory.grow
    i32.lt_s))
