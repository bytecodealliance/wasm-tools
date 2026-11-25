;; Test --library flag: multiple libraries
;;
;; Link multiple modules with separate --library flags.
;;
;; RUN[extract]: json-from-wast % -o %tmpdir/out.json --wasm-dir %tmpdir
;; RUN: component new %tmpdir/out.0.wasm \
;;     --library foo=%tmpdir/out.1.wasm \
;;     --library bar=%tmpdir/out.2.wasm -t

(module
  (func (export "add") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
  (func (export "mul") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.mul
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

(module
  (import "env" "mul" (func $mul (param i32 i32) (result i32)))
  (func (export "product") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $mul
  )
)
