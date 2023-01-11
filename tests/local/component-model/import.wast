(component
  (import "a" (func))
  (import "b" (instance))
  (import "c" (instance
    (export "a" (func))
  ))
  (import "d" (component
    (import "a" (core module))
    (export "a" (func))
  ))
  (type $t (func))
  (import "e" (type (eq $t)))
)

(assert_invalid
  (component
    (type $f (func))
    (import "a" (instance (type $f)))
  )
  "type index 0 is not an instance type")

(assert_invalid
  (component
    (core type $f (func))
    (import "a" (core module (type $f)))
  )
  "core type index 0 is not a module type")

(assert_invalid
  (component
    (type $f string)
    (import "a" (func (type $f)))
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
    "(import \"a\" (func))"
    "(import \"a\" (func))"
  )
  "import name `a` conflicts with previous import name `a`")

(assert_malformed
  (component quote
    "(type (component"
      "(import \"a\" (func))"
      "(import \"a\" (func))"
    "))"
  )
  "import name `a` conflicts with previous import name `a`")

(assert_invalid
  (component
    (import "a" (func (type 100)))
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
    (import "a" (value string))
  )
  "value index 0 was not used as part of an instantiation, start function, or export")

(component
  (import "a" (value string))
  (export "a" (value 0))
)

(component
  (import "a" "https://example.com" (func))
)

;; Empty URLs are treated as no URL
(component
  (import "a" "" (func))
)

(assert_invalid
  (component
    (import "a" "foo" (func))
  )
  "relative URL without a base")

(assert_invalid
  (component
    (import "a" "https://example.com" (func))
    (import "b" "https://example.com" (func))
  )
  "duplicate import URL `https://example.com/`")
