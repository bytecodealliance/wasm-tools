;; RUN: wast % --assert default --snapshot tests/snapshots -f cm-fixed-length-lists

(component
  (core module $m
    (memory (export "memory") 1)
    (func (export "ret-list") (result i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "ret-list") (result (list u32 4))
    (canon lift (core func $i "ret-list") (memory $i "memory"))
  )
)

(component
  (core module $m
    (func (export "param-list") (param i32 i32 i32 i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "param-list") (param "l" (list u32 4))
    (canon lift (core func $i "param-list"))
  )
)

(assert_invalid
  (component
    (core module $m
      (memory (export "memory") 1)
      (func (export "ret-list") (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "ret-list") (result (list u32 0))
      (canon lift (core func $i "ret-list") (memory $i "memory"))
    )
  )
  "Fixed size lists must have more than zero elements (at offset 0x54)"
)

(assert_malformed
  (component quote
    "(core module $m"
      "(memory (export \"memory\") 1)"
      "(func (export \"ret-list\") (result i32) unreachable)"
    ")"
    "(core instance $i (instantiate $m))"

    "(func (export \"ret-list\") (result (list u32 10000000000))"
      "(canon lift (core func $i \"ret-list\") (memory $i \"memory\"))"
    ")"
  )
  "invalid u32 number: constant out of range"
)

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (list s32 10))
      (import "x" (type (eq $t)))
    ))

    (type $x (list u32 9))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch for import `x`")


(assert_invalid
  (component
    (import "y" (component $c
      (type $t (list s32 10))
      (import "x" (type (eq $t)))
    ))

    (type $x (list u32))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch for import `x`")
