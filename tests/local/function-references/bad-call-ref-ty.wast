(assert_invalid
  (module
    (type $t (func (param i32)))
    (func (param $f funcref)
      (call_ref $t (local.get $f))
    )
  )
  "expected (ref null $type), found funcref")

(assert_invalid
  (module
    (type $t (func (param i32)))
    (func (param $f (ref $t))
      (call_ref $t (local.get $f))
    )
  )
  "expected i32 but nothing on stack")

(assert_invalid
  (module
    (type $t (func ))
    (func (param $f (ref $t))
      (call_ref 5 (local.get $f))
    )
  )
  "type index out of bounds")

(assert_invalid
  (module
    (type $t (func ))
    (func (param $f (ref $t))
      (call_ref 100 (local.get $f))
    )
  )
  "type index out of bounds")
