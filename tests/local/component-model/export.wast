(assert_invalid
  (component (export "" (instance 0)))
  "index out of bounds")

(assert_invalid
  (component (export "" (component 0)))
  "index out of bounds")

(assert_invalid
  (component (export "" (core module 0)))
  "index out of bounds")

(assert_invalid
  (component (export "" (func 0)))
  "index out of bounds")

(assert_invalid
  (component (export "" (value 0)))
  "index out of bounds")

(component
  (import "a" (instance $i))
  (import "b" (core module $m))
  (import "c" (component $c))
  (import "d" (value $v string))
  (import "e" (func $f))

  (export "f" (instance $i))
  (export "g" (core module $m))
  (export "h" (component $c))
  (export "i" (value $v))
  (export "j" (func $f))
)

(assert_invalid
  (component
    (import "a" (value $v string))
    (export "b" (value $v))
    (export "c" (value $v))
  )
  "cannot be used more than once")


(component
  (import "a" (func))
  (export "b" "https://example.com" (func 0))
)

;; Empty URLs are treated as no URL
(component
  (import "a" (func))
  (export "b" "" (func 0))
)

(assert_invalid
  (component
    (import "a" (func))
    (export "b" "foo" (func 0))
  )
  "relative URL without a base")

(assert_invalid
  (component
    (import "a" "https://example.com" (func))
    (import "b" "https://example.com" (func))
  )
  "duplicate import URL `https://example.com/`")
