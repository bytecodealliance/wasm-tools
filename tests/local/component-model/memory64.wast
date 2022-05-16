(assert_invalid
  (component
    (module $A
      (import "" "" (memory 1)))
    (module $B
      (memory (export "") i64 1))
    (instance $b (instantiate (module $B)))
    (instance $a (instantiate (module $A) (with "" (instance $b))))
  )
  "memory type mismatch")

(assert_invalid
  (component
    (module $A
      (import "" "" (memory i64 1)))
    (module $B
      (memory (export "") 1))
    (instance $b (instantiate (module $B)))
    (instance $a (instantiate (module $A) (with "" (instance $b))))
  )
  "memory type mismatch")
