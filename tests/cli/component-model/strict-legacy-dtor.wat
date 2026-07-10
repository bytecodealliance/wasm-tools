;; FAIL: parse % -o %tmpdir/strict.wasm
;; RUN[lenient]: WAST_STRICT_COMPONENT_INDICES=0 parse % -o %tmpdir/lenient.wasm

;; The legacy `(dtor (func ...))` syntax is rejected in strict mode; the new
;; spec requires `(dtor <idx>)` or `(dtor (core func ...))`.
(component
  (core module $m (func (export "dtor") (param i32)))
  (core instance $m (instantiate $m))
  (alias core export $m "dtor" (core func $dtor))
  (type $R (resource (rep i32) (dtor (func $dtor))))
)
