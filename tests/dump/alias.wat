(component
  (import "i" (instance $i
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param string)))
  ))

  (func (alias export $i "f1"))
  (alias export $i "f2" (func))

  (core func (canon lower (func $i "f1")))

  (core module $m
    (func (export "f3"))
  )

  (core instance $m (instantiate $m))
  (core func (alias export $m "f3"))
  (core alias export $m "f3" (func))
)
