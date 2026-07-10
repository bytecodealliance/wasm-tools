;; FAIL: parse % -o %tmpdir/strict.wasm
;; RUN[lenient]: WAST_STRICT_COMPONENT_INDICES=0 parse % -o %tmpdir/lenient.wasm

;; The legacy `(core-type (type ...))` syntax is rejected in strict mode; the
;; new spec requires a bare index or `(core-type (core type ...))`.
(component
  (core type $ty (func (param externref)))
  (import "i" (instance $i
                        (export "r" (type $resource (sub resource)))
                        (export "f" (func (param "x" (own $resource))))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type (type $ty))))
)
