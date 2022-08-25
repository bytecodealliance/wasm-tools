(component
  (type (instance (export "m" (func (param "x" string) (result string)))))
  (import "b1" (instance (type 0)))
  (import "b2" (instance (type 0)))
  (alias export 0 "m" (func))
  (alias export 1 "m" (func))
  (export "m1" (func 0))
  (export "m2" (func 1))
)
