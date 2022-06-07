(component
  (type $t (instance
    (export "1" (core module))
    (export "2" (func))
    (export "3" (value string))
    (export "4" (instance))
    (export "5" (component))
  ))

  (import "" (instance $i (type $t)))

  (component $c
    (import "1" (core module))
    (import "2" (func))
    (import "3" (value string))
    (import "4" (instance))
    (import "5" (component))
  )

  (instance (instantiate $c
    (with "1" (core module $i "1"))
    (with "2" (func $i "2"))
    (with "3" (value $i "4"))
    (with "4" (instance $i "3"))
    (with "5" (component $i "5"))
  ))

  (component $c2
    (import "" (instance (type $t)))
  )

  (alias export $i "1" (core module $m))
  (alias export $i "2" (func $f))
  (alias export $i "3" (value $v))
  (alias export $i "4" (instance $i2))
  (alias export $i "5" (component $c3))

  (instance
    (instantiate $c2
      (with "" (instance
        (export "1" (core module $m))
        (export "2" (func $f))
        (export "3" (value $v))
        (export "4" (instance $i2))
        (export "5" (component $c3))
      ))
    )
  )

  (core module $m1
    (func (export "1"))
    (memory (export "2") 1)
    (global (export "3") i32)
    (table (export "4") 1 funcref)
  )

  (core module $m2
    (import "" "1" (func))
    (import "" "2" (memory 1))
    (import "" "3" (global i32))
    (import "" "4" (table 1 funcref))
  )

  (core instance $i (instantiate $m1))
  (core instance (instantiate $m2 (with "" (instance $i))))

  (core alias export $i "1" (func $f))
  (core alias export $i "2" (memory $m))
  (core alias export $i "3" (global $g))
  (core alias export $i "4" (table $t))

  (core instance
    (instantiate $m2
      (with "" (instance
        (export "1" (func $f))
        (export "2" (memory $m))
        (export "3" (global $g))
        (export "4" (table $t))
      ))
    )
  )
)
