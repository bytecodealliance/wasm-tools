(component
  (module $m
    (func (export ""))
  )
  (module $m2 (import "" "a" (func)))
  (instance $M (instantiate (module $m)))
  (alias export $M "" (func $f))
  (instance (instantiate (module $m2) (with "" (instance
    (export "a" (func $f))
  ))))
)
