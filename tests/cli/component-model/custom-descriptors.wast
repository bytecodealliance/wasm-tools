;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-descriptors

;; An exact function export of a core instance can be aliased as a core func.
(component
  (core type $mt (module
    (type $f (func))
    (export "f" (func (exact (type $f))))
  ))
  (import "m" (core module $m (type $mt)))
  (core instance $i (instantiate $m))
  (alias core export $i "f" (core func $f))
  (core module $n (import "" "f" (func)))
  (core instance (instantiate $n (with "" (instance (export "f" (func $f))))))
)
