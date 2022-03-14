;; With what's defined so far, we're able to link modules with arbitrary renamings:

(component
  (module $A
    (func (export "one") (result i32) (i32.const 1))
    (func (export "two") (result i32) (i32.const 2))
    (func (export "three") (result i32) (i32.const 3))
  )
  (module $B
    (func (import "a" "one") (result i32))
  )
  (instance $a (instantiate (module $A)))
  (instance $b1 (instantiate (module $B)
    (with "a" (instance $a))            ;; no renaming
  ))
  (alias export $a "two" (func $a_two))
  (instance $b2 (instantiate (module $B)
    (with "a" (instance
      (export "one" (func $a_two))      ;; renaming, using explicit alias
    ))
  ))
  (instance $b3 (instantiate (module $B)
    (with "a" (instance
      (export "one" (func $a "three"))  ;; renaming, using inline alias sugar
    ))
  ))
)
