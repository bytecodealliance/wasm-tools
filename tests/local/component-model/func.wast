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
