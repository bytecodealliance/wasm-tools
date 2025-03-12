;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-tail-call.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (type $t (func))
  (type $t2 (func (param f32 f32) (result f32)))
  (table 1 funcref)

  (func $f
    return_call $f)

  (func $f2 (param f32 f32) (result f32)
    f32.const 0
    f32.const 0
    return_call $f2)

  (func
    i32.const 0
    return_call_indirect (type $t))

  (func (result f32)
    f32.const 0
    f32.const 0
    i32.const 0
    return_call_indirect (type $t2))
)
