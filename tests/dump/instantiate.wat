
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
      (import "a" (module $m2))
      (import "b" (func $f))
      (import "c" (global $g))
      (import "d" (instance $b))
      (import "e" (memory $mem))
      (import "f" (table $table))
    )
  )
)
