;; RUN: dump %

(component
  (core module $m
    (func (export ""))
  )
  (core module $m2 (import "" "a" (func)))
  (core instance $M (instantiate $m))
  (alias core export $M "" (core func $f))
  (core instance (instantiate $m2 (with "" (instance
    (export "a" (func $f))
  ))))
)
