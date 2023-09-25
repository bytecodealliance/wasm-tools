(module
  (func $a1
    i32.const 0
    (if (then) (else))
  )
  (func $a2
    (if (i32.const 1) (i32.eqz) (then) (else))
  )
  (func $a3
    (if (i32.const 1) (i32.eqz) (then))
  )
  (func $a4
    (if (i32.const 1) (i32.eqz) (then nop))
  )
)
(assert_invalid
  (module quote
    "(func (if))"
  )
  "no `then`")

(assert_invalid
  (module quote
    "(func (if (else)))"
  )
  "no `then`")

(assert_invalid
  (module quote
    "(func (if nop (else)))"
  )
  "expected `(`")
(assert_invalid
  (module quote
    "(func (if (nop) (else)))"
  )
  "no `then`")
(assert_invalid
  (module quote
    "(func (if (nop) nop (then)))"
  )
  "expected `(`")
