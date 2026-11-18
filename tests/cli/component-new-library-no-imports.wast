;; Test --library flag: library with no imports from env
;;
;; A library that doesn't import from `env` is still linked into the
;; component and its exports are available to the main module.
;;
;; RUN[extract]: json-from-wast % -o %tmpdir/out.json --wasm-dir %tmpdir
;; RUN: component new %tmpdir/out.0.wasm \
;;     --library bar=%tmpdir/out.1.wasm -t

(module
  (import "bar" "get" (func $get (result i32)))
  (func (export "run") (result i32)
    call $get
  )
)

(module
  (func (export "get") (result i32)
    i32.const 42
  )
)

