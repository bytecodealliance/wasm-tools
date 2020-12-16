(module
  (import "i" (instance $i
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param i32)))
  ))

  ;; TODO figure out syntactic sugar here
  (alias $i "f1" (func $i.$f1))

  (func (export "run")
    call $i.$f1
  )
)
