;; FAIL: parse % -o %tmpdir/strict.wasm
;; RUN[lenient]: WAST_STRICT_COMPONENT_INDICES=0 parse % -o %tmpdir/lenient.wasm

;; The legacy `(callback (func ...))` syntax is rejected in strict mode; the
;; new spec requires `(callback (core func ...))`.
(component
  (core module $m
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "f") (result i32) unreachable))
  (core instance $m (instantiate $m))
  (func async
    (canon lift (core func $m "f") async (callback (func $m "cb"))))
)
