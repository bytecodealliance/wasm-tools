(assert_invalid
  (module
    (type (instance))
    (func (type 0))
  )
  "type index is not a function")

(assert_invalid
  (module
    (type (instance))
    (import "" (func (type 0)))
  )
  "type index is not a function")

(assert_invalid
  (module
    (type (instance))
    (module (type 0))
  )
  "type index is not a module")

(assert_invalid
  (module
    (type (instance))
    (import "" (module (type 0)))
  )
  "type index is not a module")

(assert_invalid
  (module
    (type (func))
    (import "" (instance (type 0)))
  )
  "type index is not an instance")

(assert_invalid
  (module
    (type (func))
    (type (module
      (import "" (instance (type 0)))
    ))
  )
  "type index is not an instance")
(assert_invalid
  (module
    (type (func))
    (type (module
      (import "" (module (type 0)))
    ))
  )
  "type index is not a module")
(assert_invalid
  (module
    (type (instance))
    (type (module
      (import "" (func (type 0)))
    ))
  )
  "type index is not a func")
