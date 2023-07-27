;; --enable-gc

(module
  (type (struct))
  (type (struct (field i32)))
  (type (struct (field (mut i32))))
  (type (struct (field i32) (field i32)))
  (type (struct (field i32) (field (mut i32))))
  (type (struct (field (mut i32)) (field (mut i32))))

  (type $a (struct (field f32)))
  (type $b (struct (field f32)))

  (type (struct (field $field_a externref)))
  (type (struct (field $field_b externref) (field $field_c funcref)))

  (type (struct (field)))
  (type (struct (field i32 i32)))
  (type (struct (field i32 (mut i32))))
  (type (struct (field (mut i32) i32)))
  (type (struct (field (mut i32) (mut i32))))
  (type (struct (field $a i32) (field f32 f32 f32)))

  (type (struct (field i32) (field (ref null $a)) (field (mut (ref null $b)))))
  (type (struct (field i32 (ref null $a) (mut (ref null $b)))))
  (type (struct (field i32 i64 i8) (field) (field) (field (ref null i31) anyref)))
)
