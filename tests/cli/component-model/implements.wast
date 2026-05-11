;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-implements

(component
  (component
    (import "a" (implements "a:b/c") (instance))
    (import "b" (implements "a:b/c") (instance))
    (import "c" (implements "a:b/c@1.0.0") (instance))
    (import "my-label" (implements "ns:pkg/iface") (instance))
    (import "a:b/c" (instance))
    (import "a:b/c@1.0.0" (instance))

    (instance $a)

    (export "a" (implements "a:b/c") (instance $a))
    (export "b" (implements "a:b/c") (instance $a))
    (export "c" (implements "a:b/c@1.0.0") (instance $a))
    (export "my-label" (implements "ns:pkg/iface") (instance $a))
    (export "a:b/c" (instance $a))
    (export "a:b/c@1.0.0" (instance $a))
  )

  (type (instance
    (export "a" (implements "a:b/c") (instance))
  ))
  (type (component
    (import "a" (implements "a:b/c") (instance))
    (export "a" (implements "a:b/c") (instance))
  ))

  (instance $a)
  (instance
    (export "a" (implements "a:b/c") (instance $a))
  )
)

(assert_invalid
  (component (import "a" (implements "not-valid") (instance)))
  "must be an interface")
(assert_invalid
  (component (import "a" (implements "") (instance)))
  "not a valid name")

(assert_invalid
  (component
    (import "a" (implements "a:b/c") (instance))
    (import "a" (implements "a:b/c") (instance))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "a" (implements "a1:b/c") (instance))
    (import "a" (implements "a2:b/c") (instance))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "a" (implements "a:b/c") (instance))
    (import "a" (implements "a:b/c@1.0.0") (instance))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "a" (instance))
    (import "a" (implements "a:b/c") (instance))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "a" (implements "a:b/c") (func))
  )
  "only instance names can have an `implements`")

(assert_invalid
  (component
    (import "a1:b/c" (implements "a2:b/c") (instance))
  )
  "name `a1:b/c` is not valid with `implements`")

;; validity checks apply to other locations of `implements`, such as
;; component/instance types and bag-of-exports.
(assert_invalid
  (component (type (component (import "a" (implements "not-valid") (instance)))))
  "must be an interface")
(assert_invalid
  (component (type (component (export "a" (implements "") (instance)))))
  "not a valid name")
(assert_invalid
  (component (type (instance (export "a" (implements "a:b/c") (func)))))
  "only instance names")
(assert_invalid
  (component
    (instance)
    (instance (export "x" (implements "a") (instance 0)))
  )
  "must be an interface")
