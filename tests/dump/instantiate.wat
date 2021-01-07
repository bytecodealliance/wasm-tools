
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
      (arg "a" (module $m2))
      (arg "b" (func $f))
      (arg "c" (global $g))
      (arg "d" (instance $b))
      (arg "e" (memory $mem))
      (arg "f" (table $table))
    )
  )
)
