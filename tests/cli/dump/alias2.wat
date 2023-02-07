;; RUN: dump %

(component
  (type $t (instance
    (export "a" (core module))
    (export "b" (func))
    (export "c" (value string))
    (export "d" (instance))
    (export "e" (component))
  ))

  (import "a" (instance $i (type $t)))

  (component $c
    (import "a" (core module))
    (import "b" (func))
    (import "c" (value string))
    (import "d" (instance))
    (import "e" (component))
  )

  (instance (instantiate $c
    (with "a" (core module $i "a"))
    (with "b" (func $i "b"))
    (with "c" (value $i "c"))
    (with "d" (instance $i "d"))
    (with "e" (component $i "e"))
  ))

  (component $c2
    (import "a" (instance (type $t)))
  )

  (alias export $i "a" (core module $m))
  (alias export $i "b" (func $f))
  (alias export $i "c" (value $v))
  (alias export $i "d" (instance $i2))
  (alias export $i "e" (component $c3))

  (instance
    (instantiate $c2
      (with "a" (instance
        (export "a" (core module $m))
        (export "b" (func $f))
        (export "c" (value $v))
        (export "d" (instance $i2))
        (export "e" (component $c3))
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

  (alias core export $i "1" (core func $f))
  (alias core export $i "2" (core memory $m))
  (alias core export $i "3" (core global $g))
  (alias core export $i "4" (core table $t))

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
