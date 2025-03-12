;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-nop.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (func (result i32)
    i32.const 1
    i32.const 2
    nop
    i32.add))
