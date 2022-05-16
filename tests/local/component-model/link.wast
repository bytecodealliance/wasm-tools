;; Based on this, we can link two modules $A and $B together with the following component:

(component
  (module $A
    (func (export "one") (result i32) (i32.const 1))
  )
  (module $B
    (func (import "a" "one") (result i32))
  )
  (instance $a (instantiate (module $A)))
  (instance $b (instantiate (module $B) (with "a" (instance $a))))
)
