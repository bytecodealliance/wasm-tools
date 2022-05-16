(component
  (component $c
    (module (export ""))
  )
  (component $c2 (import "" (component (import "" (module)))))
  (instance $C (instantiate (component $c)))
  (alias export $C "" (module $m))
  (instance (instantiate (component $c2) (with "" (instance
    (export "" (module $m))
  ))))
)
