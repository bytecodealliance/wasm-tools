(component
  (import "" (module))

  (module)
  (module)

  (module (export "x"))

  (component
    (module)
  )

  (component
    (module $m)
    (import "" (func (param string)))
    (export "a" (module $m))
  )
)

;; does the `import` use the type annotation specified later?
(component
  (import "" (module))
  (type (module))
)

;; be sure to typecheck nested modules
(assert_invalid
  (component
    (module
      (func
        i32.add)
    )
  )
  "type mismatch")

;; interleave module definitions with imports/aliases and ensure that we
;; typecheck the module code section correctly
(component
  (module
    (func (export "")))
  (import "" (module))
  (module
    (func (export "") (result i32)
      i32.const 5))
  (import "b" (instance (export "" (module))))
  (alias export 0 "" (module))
)
