;; test that "bundle of exports" works in `export` statements
;;
;; TODO: may want to confirm with the spec whether or not this is intended.
(component
  (export "" (instance))
)

(assert_invalid
  (component (export "" (instance 0)))
  "index out of bounds")

(assert_invalid
  (component (export "" (component 0)))
  "index out of bounds")

(assert_invalid
  (component (export "" (module 0)))
  "index out of bounds")

(assert_invalid
  (component (export "" (func 0)))
  "index out of bounds")

(assert_invalid
  (component (export "" (value 0)))
  "index out of bounds")

(component
  (import "1" (instance $i))
  (import "2" (module $m))
  (import "3" (component $c))
  (import "4" (value $v string))
  (import "5" (func $f))

  (export "1" (instance $i))
  (export "2" (module $m))
  (export "3" (component $c))
  (export "4" (value $v))
  (export "5" (func $f))
)

(assert_invalid
  (component
    (import "" (value $v string))
    (export "1" (value $v))
    (export "2" (value $v))
  )
  "cannot be used more than once")
