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

;; does the `import` use the type annotation specified later?
(module
  (import "" (module))
  (type (module))
)
