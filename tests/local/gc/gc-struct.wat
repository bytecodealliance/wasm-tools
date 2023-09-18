;; --enable-gc

(module
  (type (struct))
  (type (struct (field i32)))
  (type (struct (field (mut i32))))
  (type (struct (field i32) (field i32)))
  (type (struct (field i32) (field (mut i32))))
  (type (struct (field (mut i32)) (field (mut i32))))

  (type $a (struct (field $field_a f32)))
  (type $b (struct (field $field_b f32)))

  (type (struct (field $field_a externref)))
  (type (struct (field $field_b externref) (field $field_c funcref)))

  (func
    struct.new $a
    struct.new_default $a
    struct.get $a $field_a
    struct.set $b $field_b
  )
)
