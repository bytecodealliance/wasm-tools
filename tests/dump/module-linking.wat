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
    (import "" (func))
    (import "" (memory 1))
    (import "" (global i32))
    (import "" (table 1 funcref))
    (import "" (module))
    (import "" (instance))
  )

  (instance (instantiate $m
    (func $i.$func)
    (memory $i.$memory)
    (global $i.$global)
    (table $i.$table)
    (module $i.$module)
    (instance $i.$instance)
  ))
)
