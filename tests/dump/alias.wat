(module
  (import "i" (instance $i
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param i32)))
  ))

  (func (export "run")
    call (func $i "f1")
  )
)
