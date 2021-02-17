(module
  (import "" (instance $i
    (export "1" (func $func))
    (export "2" (memory $memory 1))
    (export "3" (table $table 1 funcref))
    (export "4" (global $global i32))
    (export "5" (module $module))
    (export "6" (instance $instance))
  ))

  (module $m
    (import "1" (func))
    (import "2" (memory 1))
    (import "3" (global i32))
    (import "4" (table 1 funcref))
    (import "5" (module))
    (import "6" (instance))
  )

  (instance (instantiate $m
    (import "1" (func $i "1"))
    (import "2" (memory $i "2"))
    (import "3" (global $i "4"))
    (import "4" (table $i "3"))
    (import "5" (module $i "5"))
    (import "6" (instance $i "6"))
  ))
)
