(component
  (import "a" (func (param "foo" string)))
  (import "b" (func (param "foo" string) (param "bar" s32) (param "baz" u32)))
  (import "c" (func (result "foo" (tuple u8))))
  (import "d" (func (result "foo" string) (result "bar" s32) (result "baz" u32)))
)

(component
  (import "a" (func))
  (import "b" (func (param "p1" string)))
  (import "c" (func (result u32)))
  (import "d" (func (param "p1" bool) (result string)))
)

(assert_invalid
  (component
    (import "a" (func (result "foo" string) (result s32) (result "bar" u32)))
  )
  "function result name cannot be empty"
)

(assert_invalid
  (component
    (type (func (param "foo" string) (param "FOO" u32)))
  )
  "function parameter name `FOO` conflicts with previous parameter name `foo`"
)

(assert_invalid
  (component
    (type (func (result "FOO" string) (result "foo" u32)))
  )
  "function result name `foo` conflicts with previous result name `FOO`"
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
  (import "a" (func $log (param "msg" string)))
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
    (param "p1" u32) (param "p2" u32) (param "p3" u32) (param "p4" u32) (param "p5" u32)
    (param "p6" u32) (param "p7" u32) (param "p8" u32) (param "p9" u32) (param "p10" u32)
    (param "p11" u32) (param "p12" u32) (param "p13" u32) (param "p14" u32) (param "p15" u32)
    (param "p16" u32) (param "p17" u32) (param "p18" u32) (param "p19" u32) (param "p20" u32)
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
      (param "p1" u32) (param "p2" u32) (param "p3" u32) (param "p4" u32) (param "p5" u32)
      (param "p6" u32) (param "p7" u32) (param "p8" u32) (param "p9" u32) (param "p10" u32)
      (param "p11" u32) (param "p12" u32) (param "p13" u32) (param "p14" u32) (param "p15" u32)
      (param "p16" u32) (param "p17" u32) (param "p18" u32) (param "p19" u32) (param "p20" u32)
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
    (import "a" (func $log (result string)))
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
