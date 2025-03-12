;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-reference-types.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (table $t 1 externref)
  (elem declare func 0)
  (func
    (local externref)

    i32.const 0 table.get $t drop
    i32.const 0 local.get 0 table.set $t
    local.get 0 i32.const 0 table.grow $t drop
    table.size $t drop
    i32.const 0 local.get 0 i32.const 0 table.fill $t
    ref.null extern drop
    local.get 0 ref.is_null drop
    ref.func 0 drop
  )
)
