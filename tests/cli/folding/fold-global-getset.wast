;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-global-getset.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (global (mut i32) (i32.const 1))
  (global (mut f32) (f32.const 1.5))

  (func $fold-global-get (result i32)
    global.get 1
    drop
    global.get 0
    global.get 0
    i32.mul)

  (func $fold-global-set
    global.get 0
    i32.const 1
    i32.add
    global.set 0
    f32.const 2
    global.set 1)
)
