(module
  (import "" (module $m
    (import "" (module))
    (import "" (func))
    (import "" (global i32))
    (import "" (table 1 funcref))
    (import "" (instance))
    (import "" (memory 1))
  ))
  (type $empty (instance))
)
