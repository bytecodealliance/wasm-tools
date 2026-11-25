;; Test --library flag: error when library imports from non-env instance
;;
;; The library imports from an instance named `custom` which is not
;; provided by the default `env` mapping, so encoding should fail.
;;
;; RUN[extract]: json-from-wast % -o %tmpdir/out.json --wasm-dir %tmpdir
;; FAIL: component new %tmpdir/out.0.wasm \
;;     --library bar=%tmpdir/out.1.wasm

(module
  (func (export "add") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
)

(module
  (import "custom" "add" (func $add (param i32 i32) (result i32)))
  (func (export "sum") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $add
  )
)

