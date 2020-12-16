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

  ;; TODO figure out syntactic sugar here
  (alias $i "1" (func $i.$func))
  (alias $i "2" (memory $i.$memory))
  (alias $i "3" (table $i.$table))
  (alias $i "4" (global $i.$global))
  (alias $i "5" (module $i.$module))
  (alias $i "6" (instance $i.$instance))

  (instance (instantiate $m
    (arg "1" (func $i.$func))
    (arg "2" (memory $i.$memory))
    (arg "3" (global $i.$global))
    (arg "4" (table $i.$table))
    (arg "5" (module $i.$module))
    (arg "6" (instance $i.$instance))
  ))
)
