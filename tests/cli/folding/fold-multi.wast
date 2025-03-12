;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-multi.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (func $dup (result i32 i32)
    i32.const 0
    i32.const 1
  )

  (func $fold-two (result i32)
    call $dup
    i32.add
  )

  (func $cant-fold (result i32)
    call $dup
    i32.const 1
    i32.add
    drop
  )

  (func $partial-fold (result i32)
    call $dup
    call $dup
    i32.add
    i32.sub
    drop
  )
)
