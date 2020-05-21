
(module
  (import "" (module $m))
  (import "" (module $m2))
  (import "" (instance $b))
  (func $f)
  (global $g i32 (i32.const 0))
  (memory $mem 1)
  (table $table 1 funcref)

  (instance $a
    (instantiate $m
      (module $m2)
      (func $f)
      (global $g)
      (instance $b)
      (memory $mem)
      (table $table)
    )
  )
)
