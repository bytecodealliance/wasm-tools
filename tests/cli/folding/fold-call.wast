;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-call.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (type $i_i (func (param i32) (result i32)))

  (func $i_i (type $i_i)
    i32.const 1)

  (func $if_f (param i32 f32) (result f32)
    f32.const 0)

  (func $ffff_v (param f32 f32 f32 f32))

  (func $fold-call (result i32)
    i32.const 1
    call $i_i
    drop
    i32.const 2
    call $i_i)

  (func $fold-call-2
    f32.const 0
    f32.const 1
    f32.const 2
    i32.const 3
    f32.const 4
    call $if_f
    call $ffff_v)

  (table funcref (elem $i_i $if_f))
  (func $fold-call-indirect (result i32)
    i32.const 1
    i32.const 2
    call_indirect (type $i_i))
)
