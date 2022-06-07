
(assert_invalid
  (component
    (import "" (func $f (param string)))
    (start $f (result))
  )
  "start function requires 1 arguments")

(assert_invalid
  (component
    (import "" (func $f (param string)))
    (import "v" (value $v string))
    (start $f (value $v) (value $v) (result))
  )
  "start function requires 1 arguments")

(assert_invalid
  (component
    (import "" (func $f (param string) (param string)))
    (import "v" (value $v string))
    (start $f (value $v) (value $v) (result))
  )
  "cannot be used more than once")

(assert_invalid
  (component
    (import "" (func $f (param string) (param string)))
    (import "v" (value $v string))
    (import "v2" (value $v2 u32))
    (start $f (value $v) (value $v2) (result))
  )
  "type mismatch for component start function argument 1")

(component
  (import "" (func $f (param string) (param string)))
  (import "v" (value $v string))
  (import "v2" (value $v2 string))
  (start $f (value $v) (value $v2) (result))
)

(assert_invalid
  (component
    (import "" (func $f))
    (start $f (result))
    (start $f (result))
  )
  "cannot have more than one start")
