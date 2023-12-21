;; --enable-gc

(module
  (type (struct))
  (type (struct (field i32)))
  (type (struct (field (mut i32))))
  (type (struct (field i32) (field i32)))
  (type (struct (field i32) (field (mut i32))))
  (type (struct (field (mut i32)) (field (mut i32))))

  (type $a (struct (field $field_a f32)))
  (type $b (struct (field $field_b (mut f32))))

  (type (struct (field $field_a externref)))
  (type (struct (field $field_b externref) (field $field_c funcref)))

  (func (param (ref $a) (ref $b))
    f32.const 1.0
    struct.new $a
    drop

    struct.new_default $a
    drop

    local.get 0
    struct.get $a $field_a
    drop

    local.get 1
    f32.const 1.0
    struct.set $b $field_b
  )
)
