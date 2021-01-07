(module
  (import "" (module $m
    (import "" (module))
  ))
  (import "" (module $i
    (import "" (func))
  ))
  (instance $i (instantiate $m (arg "" (module $i))))
)
