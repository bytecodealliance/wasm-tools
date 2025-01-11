(module
  (import "" (module $m
    (import "" (module))
  ))
  (import "" (module $i
    (import "" (func))
  ))
  (instance $i (instantiate $m (import "" (module $i))))
)
