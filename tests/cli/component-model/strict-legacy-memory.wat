;; FAIL: parse % -o %tmpdir/strict.wasm
;; RUN[lenient]: WAST_STRICT_COMPONENT_INDICES=0 parse % -o %tmpdir/lenient.wasm

;; The legacy `(memory $i "name")` syntax is rejected in strict mode; the new
;; spec requires `(memory (core memory $i "name"))`.
(component
  (import "f" (func $f (param "x" string)))
  (core module $libc
    (memory (export "mem") 1)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable))
  (core instance $libc (instantiate $libc))
  (core func (canon lower (func $f)
    (memory $libc "mem")
    (realloc (core func $libc "realloc"))))
)
