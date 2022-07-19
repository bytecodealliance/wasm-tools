(component
  (import "" (func (param "foo" string)))
  (import "a" (func (param "foo" string) (param s32) (param "bar" u32)))
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
  (import "" (func $log (param string)))
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

(assert_invalid
  (component
    (core module $m
      (memory (export "memory") 1)
      (func (export "roundtrip") (param i32))
    )
    (core instance $m (instantiate $m))

    (type $roundtrip (func
      (param u32) (param u32) (param u32) (param u32) (param u32)
      (param u32) (param u32) (param u32) (param u32) (param u32)
      (param u32) (param u32) (param u32) (param u32) (param u32)
      (param u32) (param u32) (param u32) (param u32) (param u32)
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

    (func (export "param-list") (param (list u8))
      (canon lift (core func $i "param-list") (memory $i "memory"))
    )
  )
  "canonical option `realloc` is required"
)
