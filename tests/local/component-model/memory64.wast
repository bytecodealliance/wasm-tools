(assert_invalid
  (component
    (core module $A
      (import "" "" (memory 1)))
    (core module $B
      (memory (export "") i64 1))
    (core instance $b (instantiate $B))
    (core instance $a (instantiate $A (with "" (instance $b))))
  )
  "memory type mismatch")

(assert_invalid
  (component
    (core module $A
      (import "" "" (memory i64 1)))
    (core module $B
      (memory (export "") 1))
    (core instance $b (instantiate $B))
    (core instance $a (instantiate $A (with "" (instance $b))))
  )
  "memory type mismatch")
