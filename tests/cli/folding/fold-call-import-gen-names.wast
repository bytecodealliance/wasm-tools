;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-call-import-gen-names.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (import "a" "b" (func (param i32) (result i32)))

  (func (export "c") (param i64) (result i32)
    local.get 0
    i32.wrap_i64)

  ;; Make sure that calls are folded properly when referencing generated names
  ;; (there was a bug here).

  (func (result i32)
    i32.const 1
    call 0
    i32.const 2
    call 0
    i32.add

    i64.const 3
    call 1
    drop))
