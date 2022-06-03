(component
  (import "" (instance $i
    (export "1" (core module))
    (export "2" (func))
    (export "3" (value string))
    (export "4" (instance))
    (export "5" (component))
  ))

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
)
