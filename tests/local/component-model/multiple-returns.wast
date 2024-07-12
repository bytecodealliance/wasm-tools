(component
  (import "a" (func (result "foo" string) (result "bar" s32) (result "baz" u32)))
)

(component
  (import "a" (func $f (param "a" string) (param "b" string) (result "c" s32) (result "d" s32)))
  (import "b" (value $v string))
  (import "c" (value $v2 string))
  (start $f (value $v) (value $v2) (result (value $c)) (result (value $d)))
  (export "d" (value $c))
  (export "e" (value $d))
)

(assert_invalid
  (component
    (import "a" (func $f (param "a" string) (param "b" string) (result "c" s32) (result "d" s32)))
    (import "b" (value $v string))
    (import "c" (value $v2 string))
    (start $f (value $v) (value $v2) (result (value $c)))
    (export "a" (value $c))
  )
  "component start function has a result count of 1 but the function type has a result count of 2"
)

(assert_invalid
  (component
    (import "a" (func (result "foo" string) (result s32) (result "bar" u32)))
  )
  "function result name cannot be empty"
)

(assert_invalid
  (component
    (type (func (result "FOO" string) (result "foo" u32)))
  )
  "function result name `foo` conflicts with previous result name `FOO`"
)
