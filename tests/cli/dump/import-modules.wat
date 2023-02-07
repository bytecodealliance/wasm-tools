;; RUN: dump %

(component
  (import "a" (core module $m1
    (import "" "f" (func))
  ))
  (core module $m2
    (func (export "f"))
  )
  (core instance $i1 (instantiate $m2))
  (core instance $i2 (instantiate $m1 (with "a" (instance $i1))))
)
