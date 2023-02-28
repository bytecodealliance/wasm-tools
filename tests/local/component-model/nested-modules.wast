(component
  (import "i1" (core module))

  (core module)
  (core module)

  (core module (export "x"))

  (component
    (core module)
  )

  (component
    (core module $m)
    (import "a" (func (param "p" string)))
    (export "b" (core module $m))
  )
)

;; does the `import` use the type annotation specified later?
(component
  (import "a" (core module))
  (core type (module))
)

;; be sure to typecheck nested modules
(assert_invalid
  (component
    (core module
      (func
        i32.add)
    )
  )
  "type mismatch")

;; interleave module definitions with imports/aliases and ensure that we
;; typecheck the module code section correctly
(component
  (core module
    (func (export ""))
  )
  (import "a" (core module))
  (core module
    (func (export "") (result i32) i32.const 5)
  )
  (import "b" (instance (export "a" (core module))))
  (alias export 0 "a" (core module))
)
