;; RUN: dump %

(component
  (import "a" (instance $i
    (export "a" (core module))
    (export "b" (func))
    (export "c" (value string))
    (export "d" (instance))
    (export "e" (component))
  ))

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
)
