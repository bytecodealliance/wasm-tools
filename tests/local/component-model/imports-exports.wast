;; With what's defined so far, we can define a component that imports, links and exports other components:

;; FIXME(#588) this should be valid
(assert_invalid
(component
  (import "c" (instance $c
    (export "f" (func (result string)))
  ))
  (import "d" (component $D
    (import "c" (instance $c
      (export "f" (func (result string)))
    ))
    (export "g" (func (result string)))
  ))
  (instance $d1 (instantiate (component $D)
    (with "c" (instance $c))
  ))
  (instance $d2 (instantiate (component $D)
    (with "c" (instance
      (export "f" (func $d1 "g"))
    ))
  ))
  (export "d2" (instance $d2))
)
"instance 1 is not a module instance")
