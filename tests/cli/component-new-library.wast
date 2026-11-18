;; Test --library flag: basic default env→Main mapping
;;
;; The library module imports a function from the "env" instance,
;; satisfied by the main module's exports. The main module exports `add`
;; which the library imports as `env.add`.
;;
;; RUN[extract]: json-from-wast % -o %tmpdir/out.json --wasm-dir %tmpdir
;; RUN: component new %tmpdir/out.0.wasm \
;;     --library lib=%tmpdir/out.1.wasm -t

(module
  (func (export "add") (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add
  )
)

(module
  (import "env" "add" (func $add (param i32 i32) (result i32)))
  (func (export "sum") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $add
  )
)
