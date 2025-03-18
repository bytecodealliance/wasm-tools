;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_invalid
  (component
    (core module $A
      (import "" "" (memory 1)))
    (core module $B
      (memory (export "") i64 1))
    (core instance $b (instantiate $B))
    (core instance $a (instantiate $A (with "" (instance $b))))
  )
  "mismatch in index type used for memories")

(assert_invalid
  (component
    (core module $A
      (import "" "" (memory i64 1)))
    (core module $B
      (memory (export "") 1))
    (core instance $b (instantiate $B))
    (core instance $a (instantiate $A (with "" (instance $b))))
  )
  "mismatch in index type used for memories")

(component
  (core module $A
    (memory (export "m") i64 1))
  (core instance $A (instantiate $A))
  (alias core export $A "m" (core memory $m))

  (core module $B (import "" "" (memory i64 1)))
  (core instance (instantiate $B (with "" (instance (export "" (memory $m))))))
)

(component
  (core module $A
    (table (export "m") i64 1 funcref))
  (core instance $A (instantiate $A))
  (alias core export $A "m" (core table $m))

  (core module $B (import "" "" (table i64 1 funcref)))
  (core instance (instantiate $B (with "" (instance (export "" (table $m))))))
)

(assert_invalid
  (component
    (import "x" (func $x (param "x" string)))
    (core module $A
      (memory (export "m") i64 1))
    (core instance $A (instantiate $A))
    (alias core export $A "m" (core memory $m))
    (core func (canon lower (func $x) (memory $m)))
  )
  "canonical ABI memory is not a 32-bit linear memory")
