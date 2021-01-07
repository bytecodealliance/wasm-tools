(module
  (import "f" (func))
  (import "m" (module
    (import "" (func))
  ))
  (func (import "f2"))
  (global (import "g") i32)
  (table (import "t") 1 funcref)
  (memory (import "mem") 1)
)
