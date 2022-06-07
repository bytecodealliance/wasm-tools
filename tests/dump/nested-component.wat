
(component
  (component (import ""))

  (component)
  (component)

  (component (export "x"))

  (component
    (component)
  )

  (component
    (core module $m)
    (import "" (func (param string)))
    (export "a" (core module $m))

    (instance (export "b") (import "")
      (export "b" (func))
    )
  )
)
