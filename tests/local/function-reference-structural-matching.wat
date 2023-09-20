(module
  (type $t1 (func))

  (type $t2_a (func (param (ref $t1) (ref $t1))))

  (type $t2_b (func (param (ref $t1) (ref $t1))))

  (func $f (param (ref $t2_a))
    nop
  )

  (func $g (param (ref $t2_b))
    local.get 0
    call $f
  )
)
