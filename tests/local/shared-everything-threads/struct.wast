;; Shared struct declaration syntax.
(module
  (type (shared (struct)))
  (type (sub final (shared (struct))))
  (rec
    (type (sub final (shared (struct))))
  )

  (global (ref 0) (struct.new 1))
  (global (ref 1) (struct.new 2))
  (global (ref 2) (struct.new 0))
)

;; Shared structs are distinct from non-shared structs.
(assert_invalid
  (module
    (type (shared (struct)))
    (type (struct))

    (global (ref 0) (struct.new 1))
  )
  "type mismatch"
)

(assert_invalid
  (module
    (type (shared (struct)))
    (type (struct))

    (global (ref 1) (struct.new 0))
  )
  "type mismatch"
)

;; Shared structs may not be subtypes of non-shared structs.
(assert_invalid
  (module
    (type (sub (struct)))
    (type (sub 0 (shared (struct))))
  )
  "must match super type"
)

;; Non-shared structs may not be subtypes of shared structs.
(assert_invalid
  (module
    (type (sub (shared (struct))))
    (type (sub 0 (struct)))
  )
  "must match super type"
)

;; Shared structs may not contain non-shared references.
(assert_invalid
  (module
    (type (shared (struct (field anyref))))
  )
  "must contain shared type"
)

;; But they may contain shared references.
(module
  (type (shared (struct (field (ref null (shared any))))))
)

;; Non-shared structs may contain shared references.
(module
  (type (struct (field (ref null (shared any)))))
)

;; Struct instructions work on shared structs.
(module
  (type $i8 (shared (struct (field (mut i8)))))
  (type $i32 (shared (struct (field (mut i32)))))
  (type $unshared (struct (field (mut i8))))

  (func (struct.new $i8 (i32.const 0)) (drop))

  (func (struct.new_default $i8) (drop))

  (func (param (ref null $i8))
    (struct.get_s $i8 0 (local.get 0)) (drop))

  (func (param (ref null $i8))
    (struct.get_u $i8 0 (local.get 0)) (drop))

  (func (param (ref null $i32))
    (struct.get $i32 0 (local.get 0)) (drop))

  (func (param (ref null $i8))
    (struct.set $i8 0 (local.get 0) (i32.const 0)))
)

;; Check struct.atomic.rmw.* instructions
(module
  (type $s (shared (struct
    (field $i8 (mut i8))
    (field $i16 (mut i16))
    (field $i32 (mut i32))
    (field $i64 (mut i64))
    (field $anyref (mut (ref null (shared any))))
    (field $eqref (mut (ref null (shared eq)))))))
  (func (export "struct-atomic-get-i32-seq_cst") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get seq_cst $s $i32)
  (func (export "struct-atomic-get-i64-seq_cst") (param $x (ref null $s)) (result i64)
    local.get $x
    struct.atomic.get seq_cst $s $i64)
  (func (export "struct-atomic-get-anyref-seq_cst") (param $x (ref null $s)) (result (ref null (shared any)))
    local.get $x
    struct.atomic.get seq_cst $s $anyref)
  (func (export "struct-atomic-get-i32-acq_rel") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get acq_rel $s $i32)
  (func (export "struct-atomic-get-i64-acq_rel") (param $x (ref null $s)) (result i64)
    local.get $x
    struct.atomic.get acq_rel $s $i64)
  (func (export "struct-atomic-get-anyref-acq_rel") (param $x (ref null $s)) (result (ref null (shared any)))
    local.get $x
    struct.atomic.get acq_rel $s $anyref)
  (func (export "struct-atomic-get_s-i8-seq_cst") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_s seq_cst $s $i8)
  (func (export "struct-atomic-get_s-i16-seq_cst") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_s seq_cst $s $i16)
  (func (export "struct-atomic-get_s-i8-acq_rel") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_s acq_rel $s $i8)
  (func (export "struct-atomic-get_s-i16-acq_rel") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_s acq_rel $s $i16)
  (func (export "struct-atomic-get_u-i8-seq_cst") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_u seq_cst $s $i8)
  (func (export "struct-atomic-get_u-i16-seq_cst") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_u seq_cst $s $i16)
  (func (export "struct-atomic-get_u-i8-acq_rel") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_u acq_rel $s $i8)
  (func (export "struct-atomic-get_u-i16-acq_rel") (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_u acq_rel $s $i16)
  (func (export "struct-atomic-set-i8-seq_cst") (param $x (ref null $s)) (param $y i32)
    local.get $x
    local.get $y
    struct.atomic.set seq_cst $s $i8)
  (func (export "struct-atomic-set-i16-seq_cst") (param $x (ref null $s)) (param $y i32)
    local.get $x
    local.get $y
    struct.atomic.set seq_cst $s $i16)
  (func (export "struct-atomic-set-i32-seq_cst") (param $x (ref null $s)) (param $y i32)
    local.get $x
    local.get $y
    struct.atomic.set seq_cst $s $i32)
  (func (export "struct-atomic-set-i64-seq_cst") (param $x (ref null $s)) (param $y i64)
    local.get $x
    local.get $y
    struct.atomic.set seq_cst $s $i64)
  (func (export "struct-atomic-set-anyref-seq_cst") (param $x (ref null $s)) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.set seq_cst $s $anyref)
  (func (export "struct-atomic-set-i8-acq_rel") (param $x (ref null $s)) (param $y i32)
    local.get $x
    local.get $y
    struct.atomic.set acq_rel $s $i8)
  (func (export "struct-atomic-set-i16-acq_rel") (param $x (ref null $s)) (param $y i32)
    local.get $x
    local.get $y
    struct.atomic.set acq_rel $s $i16)
  (func (export "struct-atomic-set-i32-acq_rel") (param $x (ref null $s)) (param $y i32)
    local.get $x
    local.get $y
    struct.atomic.set acq_rel $s $i32)
  (func (export "struct-atomic-set-i64-acq_rel") (param $x (ref null $s)) (param $y i64)
    local.get $x
    local.get $y
    struct.atomic.set acq_rel $s $i64)
  (func (export "struct-atomic-set-anyref-acq_rel") (param $x (ref null $s)) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.set acq_rel $s $anyref)
  (func (export "struct-atomic-rmw.add-i32-seq_cst") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.add seq_cst $s $i32)
  (func (export "struct-atomic-rmw.add-i64-seq_cst") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.add seq_cst $s $i64)
  (func (export "struct-atomic-rmw.add-i32-acq_rel") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.add acq_rel $s $i32)
  (func (export "struct-atomic-rmw.add-i64-acq_rel") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.add acq_rel $s $i64)
  (func (export "struct-atomic-rmw.sub-i32-seq_cst") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.sub seq_cst $s $i32)
  (func (export "struct-atomic-rmw.sub-i64-seq_cst") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.sub seq_cst $s $i64)
  (func (export "struct-atomic-rmw.sub-i32-acq_rel") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.sub acq_rel $s $i32)
  (func (export "struct-atomic-rmw.sub-i64-acq_rel") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.sub acq_rel $s $i64)
  (func (export "struct-atomic-rmw.and-i32-seq_cst") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.and seq_cst $s $i32)
  (func (export "struct-atomic-rmw.and-i64-seq_cst") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.and seq_cst $s $i64)
  (func (export "struct-atomic-rmw.and-i32-acq_rel") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.and acq_rel $s $i32)
  (func (export "struct-atomic-rmw.and-i64-acq_rel") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.and acq_rel $s $i64)
  (func (export "struct-atomic-rmw.or-i32-seq_cst") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.or seq_cst $s $i32)
  (func (export "struct-atomic-rmw.or-i64-seq_cst") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.or seq_cst $s $i64)
  (func (export "struct-atomic-rmw.or-i32-acq_rel") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.or acq_rel $s $i32)
  (func (export "struct-atomic-rmw.or-i64-acq_rel") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.or acq_rel $s $i64)
  (func (export "struct-atomic-rmw.xor-i32-seq_cst") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.xor seq_cst $s $i32)
  (func (export "struct-atomic-rmw.xor-i64-seq_cst") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.xor seq_cst $s $i64)
  (func (export "struct-atomic-rmw.xor-i32-acq_rel") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.xor acq_rel $s $i32)
  (func (export "struct-atomic-rmw.xor-i64-acq_rel") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.xor acq_rel $s $i64)
  (func (export "struct-atomic-rmw.xchg-i32-seq_cst") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.xchg seq_cst $s $i32)
  (func (export "struct-atomic-rmw.xchg-i64-seq_cst") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.xchg seq_cst $s $i64)
  (func (export "struct-atomic-rmw.xchg-anyref-seq_cst") (param $x (ref null $s)) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.rmw.xchg seq_cst $s $anyref)
  (func (export "struct-atomic-rmw.xchg-i32-acq_rel") (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.xchg acq_rel $s $i32)
  (func (export "struct-atomic-rmw.xchg-i64-acq_rel") (param $x (ref null $s)) (param $y i64) (result i64)
    local.get $x
    local.get $y
    struct.atomic.rmw.xchg acq_rel $s $i64)
  (func (export "struct-atomic-rmw.xchg-anyref-acq_rel") (param $x (ref null $s)) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.rmw.xchg acq_rel $s $anyref)
  (func (export "struct-atomic-rmw.cmpxchg-i32-seq_cst") (param $x (ref null $s)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    struct.atomic.rmw.cmpxchg seq_cst $s $i32)
  (func (export "struct-atomic-rmw.cmpxchg-i64-seq_cst") (param $x (ref null $s)) (param $y i64) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    struct.atomic.rmw.cmpxchg seq_cst $s $i64)
  (func (export "struct-atomic-rmw.cmpxchg-eqref-seq_cst") (param $x (ref null $s)) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    struct.atomic.rmw.cmpxchg seq_cst $s $eqref)
  (func (export "struct-atomic-rmw.cmpxchg-i32-acq_rel") (param $x (ref null $s)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    struct.atomic.rmw.cmpxchg acq_rel $s $i32)
  (func (export "struct-atomic-rmw.cmpxchg-i64-acq_rel") (param $x (ref null $s)) (param $y i64) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    struct.atomic.rmw.cmpxchg acq_rel $s $i64)
  (func (export "struct-atomic-rmw.cmpxchg-eqref-acq_rel") (param $x (ref null $s)) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    struct.atomic.rmw.cmpxchg acq_rel $s $eqref)
)

(assert_invalid (; get, i8 ;)
  (module
    (type $s (shared (struct (field $i8 (mut i8)))))
  (func (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get seq_cst $s $i8)
  )
  "non-packed storage type"
)
(assert_invalid (; get_s, i32 ;)
  (module
    (type $s (shared (struct (field $i32 (mut i32)))))
  (func (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_s seq_cst $s $i32)
  )
  "non-packed storage types"
)
(assert_invalid (; get_s, anyref ;)
  (module
    (type $s (shared (struct (field $anyref (mut (ref null (shared any)))))))
  (func (param $x (ref null $s)) (result (ref null (shared any)))
    local.get $x
    struct.atomic.get_s seq_cst $s $anyref)
  )
  "non-packed storage types"
)
(assert_invalid (; get_u, i32 ;)
  (module
    (type $s (shared (struct (field $i32 (mut i32)))))
  (func (param $x (ref null $s)) (result i32)
    local.get $x
    struct.atomic.get_u seq_cst $s $i32)
  )
  "non-packed storage types"
)
(assert_invalid (; get_u, anyref ;)
  (module
    (type $s (shared (struct (field $anyref (mut (ref null (shared any)))))))
  (func (param $x (ref null $s)) (result (ref null (shared any)))
    local.get $x
    struct.atomic.get_u seq_cst $s $anyref)
  )
  "non-packed storage types"
)
(assert_invalid (; rmw.add, anyref ;)
  (module
    (type $s (shared (struct (field $anyref (mut (ref null (shared any)))))))
  (func (param $x (ref null $s)) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.rmw.add seq_cst $s $anyref)
  )
  "invalid type"
)
(assert_invalid (; rmw.sub, anyref ;)
  (module
    (type $s (shared (struct (field $anyref (mut (ref null (shared any)))))))
  (func (param $x (ref null $s)) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.rmw.sub seq_cst $s $anyref)
  )
  "invalid type"
)
(assert_invalid (; rmw.and, anyref ;)
  (module
    (type $s (shared (struct (field $anyref (mut (ref null (shared any)))))))
  (func (param $x (ref null $s)) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.rmw.and seq_cst $s $anyref)
  )
  "invalid type"
)
(assert_invalid (; rmw.or, anyref ;)
  (module
    (type $s (shared (struct (field $anyref (mut (ref null (shared any)))))))
  (func (param $x (ref null $s)) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.rmw.or seq_cst $s $anyref)
  )
  "invalid type"
)
(assert_invalid (; rmw.xor, anyref ;)
  (module
    (type $s (shared (struct (field $anyref (mut (ref null (shared any)))))))
  (func (param $x (ref null $s)) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    struct.atomic.rmw.xor seq_cst $s $anyref)
  )
  "invalid type"
)
(assert_invalid (; rmw.xchg, i8 ;)
  (module
    (type $s (shared (struct (field $i8 (mut i8)))))
  (func (param $x (ref null $s)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    struct.atomic.rmw.xchg seq_cst $s $i8)
  )
  "invalid type"
)
(assert_invalid (; rmw.cmpxchg, i8 ;)
  (module
    (type $s (shared (struct (field $i8 (mut i8)))))
  (func (param $x (ref null $s)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    struct.atomic.rmw.cmpxchg seq_cst $s $i8)
  )
  "invalid type"
)
(assert_invalid (; rmw.cmpxchg, anyref ;)
  (module
    (type $s (shared (struct (field $anyref (mut (ref null (shared any)))))))
  (func (param $x (ref null $s)) (param $y (ref null (shared any))) (param $z (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    struct.atomic.rmw.cmpxchg seq_cst $s $anyref)
  )
  "invalid type"
)
