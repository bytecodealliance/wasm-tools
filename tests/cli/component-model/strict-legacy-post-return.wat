;; FAIL: parse % -o %tmpdir/strict.wasm
;; RUN[lenient]: WAST_STRICT_COMPONENT_INDICES=0 parse % -o %tmpdir/lenient.wasm

;; The legacy `(post-return (func ...))` syntax is rejected in strict mode;
;; the new spec requires `(post-return (core func ...))`.
(component
  (core module $m
    (memory (export "mem") 1)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "run") (param i32 i32) (result i32) unreachable)
    (func (export "post") (param i32)))
  (core instance $m (instantiate $m))
  (func (export "run") (param "s" string) (result string)
    (canon lift (core func $m "run")
      (memory 0)
      (realloc (core func $m "realloc"))
      (post-return (func $m "post"))))
)
