;; Test --library flag: name inference from filename
;;
;; When the NAME= prefix is omitted, the name is inferred from the filename.
;; The extracted file is named `out.1.wasm`, so the inferred library name will
;; be `out.1`.
;;
;; RUN[extract]: json-from-wast % -o %tmpdir/out.json --wasm-dir %tmpdir
;; RUN: component new %tmpdir/out.0.wasm \
;;     --library %tmpdir/out.1.wasm -t

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
