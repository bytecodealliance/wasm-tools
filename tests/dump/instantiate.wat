
(module
  (import "a" (module $m))
  (import "b" (module $m2))
  (import "c" (instance $b))
  (func $f)
  (global $g i32 (i32.const 0))
  (memory $mem 1)
  (table $table 1 funcref)

  (instance $a
    (instantiate $m
      "a" (module $m2)
      "b" (func $f)
      "c" (global $g)
      "d" (instance $b)
      "e" (memory $mem)
      "f" (table $table)
    )
  )
)
