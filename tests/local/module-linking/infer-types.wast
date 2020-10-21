(module
  (module
    (import "" (func $f))
    (export "1" (func $f))

    (import "" (global $g i32))
    (export "2" (global $g))

    (import "" (table $t 1 funcref))
    (export "3" (table $t))

    (import "" (memory $m 1))
    (export "4" (memory $m))

    (import "" (instance $i))
    (export "5" (instance $i))

    (import "" (module $m))
    (export "6" (module $m))
  )
)

(module
  (module $m
    (import "" (module $m2))

    (func $f)
    (export "1" (func $f))

    (global $g i32 (i32.const 0))
    (export "2" (global $g))

    (table $t 1 funcref)
    (export "3" (table $t))

    (memory $m 1)
    (export "4" (memory $m))

    (module $m)
    (export "5" (module $m))

    (instance $i (instantiate $m2))
    (export "6" (instance $i))

    (instance $i2 (instantiate $m))
    (export "7" (instance $i2))
  )

  (module
    (module $empty)
    (instance $i (instantiate $m (module $empty)))
    (export "1" (func $i.$f))
    (export "2" (global $i.$g))
    (export "3" (table $i.$t))
    (export "4" (memory $i.$m))
    (export "5" (module $i.$m))
    (export "6" (instance $i.$i))
    (export "7" (instance $i.$i2))
  )
)
