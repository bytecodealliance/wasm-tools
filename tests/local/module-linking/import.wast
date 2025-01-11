(module
  (import "a" (func))
  (import "b" (instance))

  (import "c" (instance
    (export "" (func))
  ))
  (import "d" (module
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
(assert_invalid
  (module
    (import "" (func))
    (import "" (func))
  )
  "duplicate import name ``")
(assert_invalid
  (module
    (import "" (func))
    (import "" "" (func))
  )
  "cannot define the import `` twice")
(assert_invalid
  (module
    (import "" "a" (func))
    (import "" "a" (func))
  )
  "duplicate import name `::a`")
(assert_invalid
  (module
    (import "" "" (func))
    (module (func))
    (import "" "a" (func))
  )
  "cannot define the import `` twice")
