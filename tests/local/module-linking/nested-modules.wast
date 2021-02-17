(module
  (module (import ""))

  (module)
  (module)

  (module (export "x"))

  (module
    (module)
  )

  (module
    (module $m)
    (import "" (func (param i32)))
    (export "a" (module $m))

    (instance (export "b") (import "b")
      (export "b" (func))
    )
  )
)

;; does the `import` use the type annotation specified later?
(module
  (import "" (module))
  (type (module))
)

;; be sure to typecheck nested modules
(assert_invalid
  (module
    (module
      (func
        i32.add)
    )
  )
  "type mismatch")

;; interleave module definitions with imports/aliases and ensure that we
;; typecheck the module code section correctly
(module
  (module
    (func (export "")))
  (import "" (module))
  (module
    (func (export "") (result i32)
      i32.const 5))
  (import "b" (instance (export "" (module))))
  (alias 0 "" (module))
)
