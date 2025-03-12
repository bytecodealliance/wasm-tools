;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-block.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (func $fold-block (result f32)
    f32.const 2
    block
      f32.const 3
      br 0
    end
    f32.const 1
    f32.add)

  (func $fold-block-br-value (result i32)
    block (result i32)
      i32.const 1
      br 0
    end)

  (func $fold-loop
    loop (result i64)
      br 0
      i64.const 1
    end
    drop)

  (func $fold-loop-br (result i32)
    loop (result i32)
      i32.const 1
      br 0
    end)

  (func $fold-if
    i32.const 1
    if
      nop
      nop
    end)

  (func $fold-if-else
    i32.const 1
    if (result i32)
      i32.const 2
    else
      i32.const 3
    end
    drop)

  (func $fold-if-else-br
    i32.const 1
    if (result i32)
      i32.const 2
      br 0
    else
      i32.const 3
    end
    drop))
