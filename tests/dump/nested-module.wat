
(module
  (module (import ""))

  (module)
  (module)

  (type $f (module))
  (module (type $f))

  (module (export "x"))

  (module
    (module)
  )

  (module
    (module $m)
    (import "" (func (param i32)))
    (export "a" (module $m))

    (instance (export "b") (import "")
      (export "b" (func))
    )
  )
)
