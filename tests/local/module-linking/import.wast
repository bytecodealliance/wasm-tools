(module
  (import "" (func))
  (import "" (instance))

  (import "" (instance
    (export "" (func))
  ))
  (import "" (module
    (import "" (module))
    (export "" (func))
  ))
)

(assert_invalid
  (module
    (type (func))
    (import "" (instance (type 0)))
  )
  "type index is not an instance")
(assert_invalid
  (module
    (type (func))
    (import "" (module (type 0)))
  )
  "type index is not a module")
(assert_invalid
  (module
    (type (module))
    (import "" (func (type 0)))
  )
  "type index is not a function")
