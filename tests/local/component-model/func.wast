(component
  (import "" (func (param "foo" string)))
  (import "a" (func (param "foo" string) (param "bar" s32) (param "baz" u32)))
  (import "b" (func (result "foo" (tuple))))
  (import "c" (func (result "foo" string) (result "bar" s32) (result "baz" u32)))
)

(component
  (import "" (func))
  (import "a" (func (param "p1" string)))
  (import "b" (func (result u32)))
  (import "c" (func (param "p1" bool) (result string)))
)

(assert_invalid
  (component
    (import "a" (func (result "foo" string) (result s32) (result "bar" u32)))
  )
  "function result name cannot be empty"
)

(assert_invalid
  (component
    (type (func (param "foo" string) (param "foo" u32)))
  )
  "duplicate parameter name"
)

(assert_invalid
  (component
    (type (func (result "foo" string) (result "foo" u32)))
  )
  "duplicate result name"
)

(assert_invalid
  (component
      (core module $m
          (memory (export "memory") 1)
          (func (export "foo") (result i32) unreachable)
      )
      (core instance $i (instantiate $m))

      (func (export "tuple") (result (tuple s8 u8))
          (canon lift (core func $i "foo"))
      )
  )
  "canonical option `memory` is required"
)

(component
  (import "" (func $log (param "msg" string)))
  (core module $libc
    (memory (export "memory") 1)
  )
  (core instance $libc (instantiate $libc))
  (core func (canon lower (func $log) (memory $libc "memory")))
)

(component
  (core module $m
    (memory (export "memory") 1)
    (func (export "ret-list") (result i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "ret-list") (result (list u8))
    (canon lift (core func $i "ret-list") (memory $i "memory"))
  )
)

(component
  (type $big (func
    (param "1" u32) (param "2" u32) (param "3" u32) (param "4" u32) (param "5" u32)
    (param "6" u32) (param "7" u32) (param "8" u32) (param "9" u32) (param "10" u32)
    (param "11" u32) (param "12" u32) (param "13" u32) (param "14" u32) (param "15" u32)
    (param "16" u32) (param "17" u32) (param "18" u32) (param "19" u32) (param "20" u32)
  ))

  (component $c
    (import "big" (func $big (type $big)))
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func $big (canon lower (func $big) (memory $libc "memory")))
  )
)

(assert_invalid
  (component
    (core module $m
      (memory (export "memory") 1)
      (func (export "roundtrip") (param i32))
    )
    (core instance $m (instantiate $m))

    (type $roundtrip (func
      (param "1" u32) (param "2" u32) (param "3" u32) (param "4" u32) (param "5" u32)
      (param "6" u32) (param "7" u32) (param "8" u32) (param "9" u32) (param "10" u32)
      (param "11" u32) (param "12" u32) (param "13" u32) (param "14" u32) (param "15" u32)
      (param "16" u32) (param "17" u32) (param "18" u32) (param "19" u32) (param "20" u32)
    ))

    (func $roundtrip (type $roundtrip)
      (canon lift (core func $m "roundtrip") (memory $m "memory"))
    )
    (export "roundtrip" (func $roundtrip))
  )
  "canonical option `realloc` is required"
)

(assert_invalid
  (component
    (import "" (func $log (result string)))
    (core module $libc
      (memory (export "memory") 1)
    )
    (core instance $libc (instantiate $libc))
    (core func (canon lower (func $log) (memory $libc "memory")))
  )
  "canonical option `realloc` is required"
)

(assert_invalid
  (component
    (core module $m
      (memory (export "memory") 1)
      (func (export "param-list") (param i32 i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "param-list") (param "bytes" (list u8))
      (canon lift (core func $i "param-list") (memory $i "memory"))
    )
  )
  "canonical option `realloc` is required"
)
