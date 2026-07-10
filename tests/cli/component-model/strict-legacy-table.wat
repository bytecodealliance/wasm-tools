;; FAIL: parse % -o %tmpdir/strict.wasm
;; RUN[lenient]: WAST_STRICT_COMPONENT_INDICES=0 parse % -o %tmpdir/lenient.wasm

;; The legacy `(table ...)` syntax in `thread.new-indirect` and
;; `thread.spawn-indirect` is rejected in strict mode; the new spec requires
;; a bare index or `(core table ...)`.
(component
  (core type $start (func (param $context i32)))
  (core module $libc (table (export "start-table") 1 (ref null func)))
  (core instance $libc (instantiate $libc))
  (core func (canon thread.new-indirect $start (table $libc "start-table")))
)
