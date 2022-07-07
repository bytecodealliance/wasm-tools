(component
  (import "a" (func))
  (import "b" (instance))
  (import "c" (instance
    (export "" (func))
  ))
  (import "d" (component
    (import "" (core module))
    (export "" (func))
  ))
  (type $t (func))
  (import "e" (type (eq $t)))
)

(assert_invalid
  (component
    (type $f (func))
    (import "" (instance (type $f)))
  )
  "type index 0 is not an instance type")

(assert_invalid
  (component
    (core type $f (func))
    (import "" (core module (type $f)))
  )
  "core type index 0 is not a module type")

(assert_invalid
  (component
    (type $f string)
    (import "" (func (type $f)))
  )
  "type index 0 is not a function type")

;; Disallow duplicate imports for core wasm modules
(assert_invalid
  (component
    (core type (module
      (import "" "" (func))
      (import "" "" (func))
    ))
  )
  "duplicate import name `:`")
(assert_invalid
  (component
    (core module
      (import "" "" (func))
      (import "" "" (func))
    )
  )
  "duplicate import name `:`")
(assert_invalid
  (component
    (core type (module
      (import "" "a" (func))
      (import "" "a" (func))
    ))
  )
  "duplicate import name `:a`")
(assert_invalid
  (component
    (core module
      (import "" "a" (func))
      (import "" "a" (func))
    )
  )
  "duplicate import name `:a`")

(assert_malformed
  (component quote
    "(import \"\" (func))"
    "(import \"\" (func))"
  )
  "duplicate import name `` already defined")

(assert_malformed
  (component quote
    "(type (component"
      "(import \"\" (func))"
      "(import \"\" (func))"
    "))"
  )
  "duplicate import name `` already defined")

(assert_invalid
  (component
    (import "" (func (type 100)))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (func (type 100) (canon lift (core func $i "")))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (import "" (value string))
  )
  "value index 0 was not used as part of an instantiation, start function, or export")

(component
  (import "" (value string))
  (export "" (value 0))
)
