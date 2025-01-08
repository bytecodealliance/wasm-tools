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

(assert_invalid
  (component
    (core module $A
      (memory (export "m") i64 1))
    (core instance $A (instantiate $A))
    (alias core export $A "m" (core memory $m))
  )
  "64-bit linear memories are not compatible with components yet")

(assert_invalid
  (component
    (core module $A
      (table (export "m") i64 1 funcref))
    (core instance $A (instantiate $A))
    (alias core export $A "m" (core table $m))
  )
  "64-bit tables are not compatible with components yet")
