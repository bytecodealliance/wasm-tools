
(assert_invalid
  (component
    (import "" (func $f (param string)))
    (start $f)
  )
  "start function requires 1 arguments")

(assert_invalid
  (component
    (import "" (func $f (param string)))
    (import "v" (value $v string))
    (start $f (value $v) (value $v))
  )
  "start function requires 1 arguments")

(assert_invalid
  (component
    (import "" (func $f (param "1" string) (param "2" string)))
    (import "v" (value $v string))
    (start $f (value $v) (value $v))
  )
  "cannot be used more than once")

(assert_invalid
  (component
    (import "" (func $f (param "x" string) (param "y" string)))
    (import "v" (value $v string))
    (import "v2" (value $v2 u32))
    (start $f (value $v) (value $v2))
  )
  "type mismatch for component start function argument 1")

(component
  (import "" (func $f (param "z" string) (param "a" string)))
  (import "v" (value $v string))
  (import "v2" (value $v2 string))
  (start $f (value $v) (value $v2))
)

(component
  (import "" (func $f (result string)))
  (start $f (result (value $a)))
  (export "a" (value $a))
)

(component
  (import "" (func $f (param "a" string) (param "b" string) (result "c" s32) (result "d" s32)))
  (import "v" (value $v string))
  (import "v2" (value $v2 string))
  (start $f (value $v) (value $v2) (result (value $c)) (result (value $d)))
  (export "c" (value $c))
  (export "d" (value $d))
)

(assert_invalid
  (component
    (import "" (func $f (param "a" string) (param "b" string) (result "c" s32) (result "d" s32)))
    (import "v" (value $v string))
    (import "v2" (value $v2 string))
    (start $f (value $v) (value $v2) (result (value $c)))
    (export "c" (value $c))
  )
  "component start function has a result count of 1 but the function type has a result count of 2"
)

(assert_invalid
  (component
    (import "" (func $f))
    (start $f)
    (start $f)
  )
  "cannot have more than one start")
