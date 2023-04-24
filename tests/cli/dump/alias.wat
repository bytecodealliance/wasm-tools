;; RUN: dump %

(component
  (import "i" (instance $i
    (export "f1" (func))
    (export "f2" (func (param "p1" string)))
  ))

  (func (alias export $i "f1"))
  (alias export $i "f2" (func))

  (core func (canon lower (func $i "f1")))

  (core module $m
    (func (export "f3"))
  )

  (core instance $m (instantiate $m))
  (core func (alias core export $m "f3"))
  (alias core export $m "f3" (core func))
)
