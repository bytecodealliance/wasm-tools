(assert_invalid
  (component
    (import "f" (func $f))
    (start $f)
  )
  "support for component model `value`s is not enabled")

(assert_invalid
  (component
    (import "f" (value string))
  )
  "support for component model `value`s is not enabled")

(assert_invalid
  (component
    (export "f" (value 0))
  )
  "support for component model `value`s is not enabled")

(assert_invalid
  (component
    (alias export 0 "f" (value))
  )
  "support for component model `value`s is not enabled")
(assert_invalid
  (component
    (import "f1" (func))
    (export "f" (func 0) (value string))
  )
  "support for component model `value`s is not enabled")
(assert_invalid
  (component
    (component)
    (instance (instantiate 0 (with "" (value 0))))
  )
  "support for component model `value`s is not enabled")
(assert_invalid
  (component
    (instance (export "i" (value 0)))
  )
  "support for component model `value`s is not enabled")
