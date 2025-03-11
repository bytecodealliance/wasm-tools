;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-bulk-memory.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (memory 1)
  (data "a")
  (func
    i32.const 0 i32.const 0 i32.const 0 memory.init 0
    data.drop 0
    i32.const 0 i32.const 0 i32.const 0 memory.copy
    i32.const 0 i32.const 0 i32.const 0 memory.fill
  )

  (table 1 funcref)
  (elem func 0)
  (func
    i32.const 0 i32.const 0 i32.const 0 table.init 0
    elem.drop 0
    i32.const 0 i32.const 0 i32.const 0 table.copy
  )
)
