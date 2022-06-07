(component
  (core module $m
    (func (export ""))
  )
  (core module $m2 (import "" "a" (func)))
  (core instance $M (instantiate $m))
  (core alias export $M "" (func $f))
  (core instance (instantiate $m2 (with "" (instance
    (export "a" (func $f))
  ))))
)
