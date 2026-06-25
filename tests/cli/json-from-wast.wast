;; RUN: json-from-wast % --wasm-dir %tmpdir
;; RUN[output-stem]: json-from-wast % -o %tmpdir/out.json --wasm-dir %tmpdir | validate %tmpdir/out.0.wasm

(module $m
  (func (export "f") (result i32) i32.const -1)
  (func (export "g") (result i64) i64.const -2)
  (func (export "id") (param i32) (result i32) local.get 0)
)
(assert_return (invoke "f") (i32.const -1))
(assert_return (invoke "g") (i64.const -2))
(assert_return (invoke "id" (i32.const -1)) (i32.const -1))
