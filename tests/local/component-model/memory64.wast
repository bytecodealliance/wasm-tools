(assert_invalid
  (module
    (module $A
      (import "" (memory 1)))
    (module $B
      (memory (export "") i64 1))
    (instance $b (instantiate $B))
    (instance $a (instantiate $A (import "" (memory $b ""))))
  )
  "memory type mismatch")

(assert_invalid
  (module
    (module $A
      (import "" (memory i64 1)))
    (module $B
      (memory (export "") 1))
    (instance $b (instantiate $B))
    (instance $a (instantiate $A (import "" (memory $b ""))))
  )
  "memory type mismatch")
