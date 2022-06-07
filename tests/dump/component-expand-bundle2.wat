(component
  (component $c
    (core module (export ""))
  )
  (component $c2 (import "" (component (import "" (core module)))))
  (instance $C (instantiate $c))
  (alias export $C "" (core module $m))
  (instance (instantiate $c2 (with "" (instance
    (export "" (core module $m))
  ))))
)
