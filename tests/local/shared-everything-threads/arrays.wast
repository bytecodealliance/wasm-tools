;; Shared array declaration syntax.
(module
  (type (shared (array i8)))
  (type (sub final (shared (array i8))))
  (rec
    (type (sub final (shared (array i8))))
  )

  (global (ref 0) (array.new_default 1 (i32.const 1)))
  (global (ref 1) (array.new_default 2 (i32.const 1)))
  (global (ref 2) (array.new_default 0 (i32.const 1)))
)

;; Shared arrays are distinct from non-shared arrays.
(assert_invalid
  (module
    (type (shared (array i8)))
    (type (array i8))

    (global (ref 0) (array.new_default 1 (i32.const 1)))
  )
  "type mismatch"
)

(assert_invalid
  (module
    (type (shared (array i8)))
    (type (array i8))

    (global (ref 1) (array.new 0))
  )
  "type mismatch"
)

;; Shared arrays may not be subtypes of non-shared arrays.
(assert_invalid
  (module
    (type (sub (array i8)))
    (type (sub 0 (shared (array i8))))
  )
  "sub type must match super type"
)

;; Non-shared arrays may not be subtypes of shared arrays.
(assert_invalid
  (module
    (type (sub (shared (array i8))))
    (type (sub 0 (array i8)))
  )
  "sub type must match super type"
)

;; Shared arrays may not contain non-shared references.
(assert_invalid
  (module
    (type (shared (array anyref)))
  )
  "must contain shared type"
)

;; But they may contain shared references.
(module
  (type (shared (array (ref null (shared any)))))
)

;; Non-shared arrays may contain shared references.
(module
  (type (array (ref null (shared any))))
)

;; Array instructions work on shared arrays.
(module
  (type $i8 (shared (array (mut i8))))
  (type $i32 (shared (array (mut i32))))
  (type $unshared (array (mut i8)))

  (data)
  (elem arrayref)

  (func (array.new $i8 (i32.const 0) (i32.const 0)) (drop))

  (func (array.new_default $i8 (i32.const 0)) (drop))

  (func (array.new_fixed $i8 0) (drop))

  (func (param (ref null $i8))
    (array.get_s $i8 (local.get 0) (i32.const 0)) (drop))

  (func (param (ref null $i8))
    (array.get_u $i8 (local.get 0) (i32.const 0)) (drop))

  (func (param (ref null $i32))
    (array.get $i32 (local.get 0) (i32.const 0)) (drop))

  (func (param (ref null $i8))
    (array.set $i8 (local.get 0) (i32.const 0) (i32.const 0)))

  (func (param (ref null $i8) (ref null $i8))
    (array.copy $i8 $i8 (local.get 0) (i32.const 0) (local.get 1) (i32.const 0) (i32.const 0)))

  (func (param (ref null $i8) (ref null $unshared))
    (array.copy $i8 $unshared (local.get 0) (i32.const 0) (local.get 1) (i32.const 0) (i32.const 0)))

  (func (param (ref null $unshared) (ref null $i8))
    (array.copy $unshared $i8 (local.get 0) (i32.const 0) (local.get 1) (i32.const 0) (i32.const 0)))

  (func (param (ref null $i8))
    (array.fill $i8 (local.get 0) (i32.const 0) (i32.const 0) (i32.const 0)))

  (func (param (ref null $i8))
    (array.init_data $i8 0 (local.get 0) (i32.const 0) (i32.const 0) (i32.const 0)))

  (func (param (ref null $i8))
    (array.init_data $i8 0 (local.get 0) (i32.const 0) (i32.const 0) (i32.const 0)))
)

;; Bottom types can be used as shared arrays.
(module
  (type $i8 (shared (array (mut i8))))
  (type $i32 (shared (array (mut i32))))
  (type $funcs (shared (array (mut (ref null (shared func))))))

  (data)
  ;; See https://github.com/bytecodealliance/wasm-tools/issues/1717.
  (elem (ref null (shared func)))

  (func (array.get_s $i8 (ref.null (shared none)) (i32.const 0)) (drop))
  (func (array.get_u $i8 (ref.null (shared none)) (i32.const 0)) (drop))
  (func (array.get $i32 (ref.null (shared none)) (i32.const 0)) (drop))
  (func (array.set $i8 (ref.null (shared none)) (i32.const 0) (i32.const 0)))
  (func (param (ref null $i8))
    (array.copy $i8 $i8 (ref.null (shared none)) (i32.const 0) (local.get 0) (i32.const 0) (i32.const 0)))
  (func (param (ref null $i8))
    (array.copy $i8 $i8 (local.get 0) (i32.const 0) (ref.null (shared none)) (i32.const 0) (i32.const 0)))
  (func (array.copy $i8 $i8 (ref.null (shared none)) (i32.const 0) (ref.null (shared none)) (i32.const 0) (i32.const 0)))
  (func (array.fill $i8 (ref.null (shared none)) (i32.const 0) (i32.const 0) (i32.const 0)))
  (func (array.init_data $i8 0 (ref.null (shared none)) (i32.const 0) (i32.const 0) (i32.const 0)))
  (func (array.init_elem $funcs 0 (ref.null (shared none)) (i32.const 0) (i32.const 0) (i32.const 0)))
)

;; Check that field-modifying instructions only work on mutable fields.
(assert_invalid
  (module
    (type $a (shared (array (ref (shared any)))))
    (func (param $a (ref $a)) (param $ar (ref (shared any))) (result (ref (shared any)))
      (array.atomic.set seq_cst $a (local.get $a) (i32.const 0) (local.get $ar))
    )
  )
  "array is immutable"
)
(assert_invalid
  (module
    (type $a (shared (array i32)))
    (func (param $a (ref $a)) (result i32)
      (array.atomic.rmw.add seq_cst $a (local.get $a) (i32.const 0) (i32.const 1))
    )
  )
  "array is immutable"
)
(assert_invalid
  (module
    (type $a (shared (array i64)))
    (func (param $a (ref $a)) (result i64)
      (array.atomic.rmw.sub seq_cst $a (local.get $a) (i32.const 0) (i64.const 1))
    )
  )
  "array is immutable"
)
(assert_invalid
  (module
    (type $a (shared (array i32)))
    (func (param $a (ref $a)) (result i32)
      (array.atomic.rmw.and acq_rel $a (local.get $a) (i32.const 0) (i32.const 1))
    )
  )
  "array is immutable"
)
(assert_invalid
  (module
    (type $a (shared (array i64)))
    (func (param $a (ref $a)) (result i64)
      (array.atomic.rmw.or acq_rel $a (local.get $a) (i32.const 0) (i64.const 1))
    )
  )
  "array is immutable"
)
(assert_invalid
  (module
    (type $a (shared (array i32)))
    (func (param $a (ref $a)) (result i32)
      (array.atomic.rmw.xor seq_cst $a (local.get $a) (i32.const 0) (i32.const 1))
    )
  )
  "array is immutable"
)
(assert_invalid
  (module
    (type $a (shared (array (ref (shared any)))))
    (func (param $a (ref $a)) (param $ar (ref (shared any))) (result (ref (shared any)))
      (array.atomic.rmw.xchg seq_cst $a (local.get $a) (i32.const 0) (local.get $ar))
    )
  )
  "array is immutable"
)
(assert_invalid
  (module
    (type $a (shared (array (ref (shared eq)))))
    (func (param $a (ref $a)) (param $e1 (ref (shared eq))) (param $e2 (ref (shared eq))) (result)
      (array.atomic.rmw.cmpxchg acq_rel $a (local.get $a) (i32.const 0) (local.get $e1) (local.get $e2))
    )
  )
  "array is immutable"
)

;; Exhaustively check `array.atomic.rmw.*` instructions.
(module (; get, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-get-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get seq_cst $a)
)

(module (; get, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-get-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (result i64)
    local.get $x
    local.get $y
    array.atomic.get seq_cst $a)
)

(module (; get, anyref, seq_cst ;)
  (type $a (shared (array (mut (ref null (shared any))))))
  (func (export "array-atomic-get-anyref-seq_cst") (param $x (ref null $a)) (param $y i32) (result (ref null (shared any)))
    local.get $x
    local.get $y
    array.atomic.get seq_cst $a)
)

(module (; get, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-get-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get acq_rel $a)
)

(module (; get, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-get-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (result i64)
    local.get $x
    local.get $y
    array.atomic.get acq_rel $a)
)

(module (; get, anyref, acq_rel ;)
  (type $a (shared (array (mut (ref null (shared any))))))
  (func (export "array-atomic-get-anyref-acq_rel") (param $x (ref null $a)) (param $y i32) (result (ref null (shared any)))
    local.get $x
    local.get $y
    array.atomic.get acq_rel $a)
)

(module (; get_s, i8, seq_cst ;)
  (type $a (shared (array (mut i8))))
  (func (export "array-atomic-get_s-i8-seq_cst") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_s seq_cst $a)
)

(module (; get_s, i16, seq_cst ;)
  (type $a (shared (array (mut i16))))
  (func (export "array-atomic-get_s-i16-seq_cst") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_s seq_cst $a)
)

(module (; get_s, i8, acq_rel ;)
  (type $a (shared (array (mut i8))))
  (func (export "array-atomic-get_s-i8-acq_rel") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_s acq_rel $a)
)

(module (; get_s, i16, acq_rel ;)
  (type $a (shared (array (mut i16))))
  (func (export "array-atomic-get_s-i16-acq_rel") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_s acq_rel $a)
)

(module (; get_u, i8, seq_cst ;)
  (type $a (shared (array (mut i8))))
  (func (export "array-atomic-get_u-i8-seq_cst") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_u seq_cst $a)
)

(module (; get_u, i16, seq_cst ;)
  (type $a (shared (array (mut i16))))
  (func (export "array-atomic-get_u-i16-seq_cst") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_u seq_cst $a)
)

(module (; get_u, i8, acq_rel ;)
  (type $a (shared (array (mut i8))))
  (func (export "array-atomic-get_u-i8-acq_rel") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_u acq_rel $a)
)

(module (; get_u, i16, acq_rel ;)
  (type $a (shared (array (mut i16))))
  (func (export "array-atomic-get_u-i16-acq_rel") (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_u acq_rel $a)
)

(module (; set, i8, seq_cst ;)
  (type $a (shared (array (mut i8))))
  (func (export "array-atomic-set-i8-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set seq_cst $a)
)

(module (; set, i16, seq_cst ;)
  (type $a (shared (array (mut i16))))
  (func (export "array-atomic-set-i16-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set seq_cst $a)
)

(module (; set, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-set-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set seq_cst $a)
)

(module (; set, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-set-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set seq_cst $a)
)

(module (; set, anyref, seq_cst ;)
  (type $a (shared (array (mut (ref null (shared any))))))
  (func (export "array-atomic-set-anyref-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set seq_cst $a)
)

(module (; set, i8, acq_rel ;)
  (type $a (shared (array (mut i8))))
  (func (export "array-atomic-set-i8-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set acq_rel $a)
)

(module (; set, i16, acq_rel ;)
  (type $a (shared (array (mut i16))))
  (func (export "array-atomic-set-i16-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set acq_rel $a)
)

(module (; set, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-set-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set acq_rel $a)
)

(module (; set, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-set-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set acq_rel $a)
)

(module (; set, anyref, acq_rel ;)
  (type $a (shared (array (mut (ref null (shared any))))))
  (func (export "array-atomic-set-anyref-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.set acq_rel $a)
)

(module (; rmw.add, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.add-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.add seq_cst $a)
)

(module (; rmw.add, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.add-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.add seq_cst $a)
)

(module (; rmw.add, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.add-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.add acq_rel $a)
)

(module (; rmw.add, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.add-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.add acq_rel $a)
)

(module (; rmw.sub, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.sub-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.sub seq_cst $a)
)

(module (; rmw.sub, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.sub-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.sub seq_cst $a)
)

(module (; rmw.sub, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.sub-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.sub acq_rel $a)
)

(module (; rmw.sub, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.sub-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.sub acq_rel $a)
)

(module (; rmw.and, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.and-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.and seq_cst $a)
)

(module (; rmw.and, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.and-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.and seq_cst $a)
)

(module (; rmw.and, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.and-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.and acq_rel $a)
)

(module (; rmw.and, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.and-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.and acq_rel $a)
)

(module (; rmw.or, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.or-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.or seq_cst $a)
)

(module (; rmw.or, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.or-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.or seq_cst $a)
)

(module (; rmw.or, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.or-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.or acq_rel $a)
)

(module (; rmw.or, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.or-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.or acq_rel $a)
)

(module (; rmw.xor, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.xor-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xor seq_cst $a)
)

(module (; rmw.xor, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.xor-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xor seq_cst $a)
)

(module (; rmw.xor, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.xor-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xor acq_rel $a)
)

(module (; rmw.xor, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.xor-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xor acq_rel $a)
)

(module (; rmw.xchg, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.xchg-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xchg seq_cst $a)
)

(module (; rmw.xchg, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.xchg-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xchg seq_cst $a)
)

(module (; rmw.xchg, anyref, seq_cst ;)
  (type $a (shared (array (mut (ref null (shared any))))))
  (func (export "array-atomic-rmw.xchg-anyref-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xchg seq_cst $a)
)

(module (; rmw.xchg, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.xchg-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xchg acq_rel $a)
)

(module (; rmw.xchg, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.xchg-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xchg acq_rel $a)
)

(module (; rmw.xchg, anyref, acq_rel ;)
  (type $a (shared (array (mut (ref null (shared any))))))
  (func (export "array-atomic-rmw.xchg-anyref-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xchg acq_rel $a)
)

(module (; rmw.cmpxchg, i32, seq_cst ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.cmpxchg-i32-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i32) (param $A i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    local.get $A
    array.atomic.rmw.cmpxchg seq_cst $a)
)

(module (; rmw.cmpxchg, i64, seq_cst ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.cmpxchg-i64-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z i64) (param $A i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    local.get $A
    array.atomic.rmw.cmpxchg seq_cst $a)
)

(module (; rmw.cmpxchg, eqref, seq_cst ;)
  (type $a (shared (array (mut (ref null (shared eq))))))
  (func (export "array-atomic-rmw.cmpxchg-eqref-seq_cst") (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared eq))) (param $A (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    local.get $A
    array.atomic.rmw.cmpxchg seq_cst $a)
)

(module (; rmw.cmpxchg, i32, acq_rel ;)
  (type $a (shared (array (mut i32))))
  (func (export "array-atomic-rmw.cmpxchg-i32-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i32) (param $A i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    local.get $A
    array.atomic.rmw.cmpxchg acq_rel $a)
)

(module (; rmw.cmpxchg, i64, acq_rel ;)
  (type $a (shared (array (mut i64))))
  (func (export "array-atomic-rmw.cmpxchg-i64-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z i64) (param $A i64) (result i64)
    local.get $x
    local.get $y
    local.get $z
    local.get $A
    array.atomic.rmw.cmpxchg acq_rel $a)
)

(module (; rmw.cmpxchg, eqref, acq_rel ;)
  (type $a (shared (array (mut (ref null (shared eq))))))
  (func (export "array-atomic-rmw.cmpxchg-eqref-acq_rel") (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared eq))) (param $A (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    local.get $A
    array.atomic.rmw.cmpxchg acq_rel $a)
)

(assert_invalid (; get, i8 ;)
  (module
    (type $a (shared (array (mut i8))))
  (func (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get seq_cst $a)
  )
  "packed storage type"
)
(assert_invalid (; get_s, i32 ;)
  (module
    (type $a (shared (array (mut i32))))
  (func (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_s seq_cst $a)
  )
  "non-packed storage type"
)
(assert_invalid (; get_s, anyref ;)
  (module
    (type $a (shared (array (mut (ref null (shared any))))))
  (func (param $x (ref null $a)) (param $y i32) (result (ref null (shared any)))
    local.get $x
    local.get $y
    array.atomic.get_s seq_cst $a)
  )
  "non-packed storage type"
)
(assert_invalid (; get_u, i32 ;)
  (module
    (type $a (shared (array (mut i32))))
  (func (param $x (ref null $a)) (param $y i32) (result i32)
    local.get $x
    local.get $y
    array.atomic.get_u seq_cst $a)
  )
  "non-packed storage type"
)
(assert_invalid (; get_u, anyref ;)
  (module
    (type $a (shared (array (mut (ref null (shared any))))))
  (func (param $x (ref null $a)) (param $y i32) (result (ref null (shared any)))
    local.get $x
    local.get $y
    array.atomic.get_u seq_cst $a)
  )
  "non-packed storage type"
)
(assert_invalid (; rmw.add, anyref ;)
  (module
    (type $a (shared (array (mut (ref null (shared any))))))
  (func (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.add seq_cst $a)
  )
  "invalid type"
)
(assert_invalid (; rmw.sub, anyref ;)
  (module
    (type $a (shared (array (mut (ref null (shared any))))))
  (func (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.sub seq_cst $a)
  )
  "invalid type"
)
(assert_invalid (; rmw.and, anyref ;)
  (module
    (type $a (shared (array (mut (ref null (shared any))))))
  (func (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.and seq_cst $a)
  )
  "invalid type"
)
(assert_invalid (; rmw.or, anyref ;)
  (module
    (type $a (shared (array (mut (ref null (shared any))))))
  (func (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.or seq_cst $a)
  )
  "invalid type"
)
(assert_invalid (; rmw.xor, anyref ;)
  (module
    (type $a (shared (array (mut (ref null (shared any))))))
  (func (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xor seq_cst $a)
  )
  "invalid type"
)
(assert_invalid (; rmw.xchg, i8 ;)
  (module
    (type $a (shared (array (mut i8))))
  (func (param $x (ref null $a)) (param $y i32) (param $z i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    array.atomic.rmw.xchg seq_cst $a)
  )
  "invalid type"
)
(assert_invalid (; rmw.cmpxchg, i8 ;)
  (module
    (type $a (shared (array (mut i8))))
  (func (param $x (ref null $a)) (param $y i32) (param $z i32) (param $A i32) (result i32)
    local.get $x
    local.get $y
    local.get $z
    local.get $A
    array.atomic.rmw.cmpxchg seq_cst $a)
  )
  "invalid type"
)
(assert_invalid (; rmw.cmpxchg, anyref ;)
  (module
    (type $a (shared (array (mut (ref null (shared any))))))
  (func (param $x (ref null $a)) (param $y i32) (param $z (ref null (shared any))) (param $A (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    local.get $z
    local.get $A
    array.atomic.rmw.cmpxchg seq_cst $a)
  )
  "invalid type"
)

(assert_invalid
  (module
    (type $s (shared (array f32)))
    (func
      unreachable
      array.atomic.get seq_cst $s
      drop
    ))
  "invalid type: `array.atomic.get` only allows `i32`, `i64` and subtypes of `anyref`"
)

(assert_invalid
  (module
    (type $s (shared (array (ref (shared func)))))
    (func
      unreachable
      array.atomic.get seq_cst $s
      drop
    ))
  "invalid type: `array.atomic.get` only allows `i32`, `i64` and subtypes of `anyref`"
)

(assert_invalid
  (module
    (type $s (shared (array i8)))
    (func
      unreachable
      array.atomic.get seq_cst $s
      drop
    ))
  "cannot use array.get with packed storage types"
)

(assert_invalid
  (module
    (type $s (shared (array (mut (ref (shared extern))))))
    (func
      unreachable
      array.atomic.set seq_cst $s
    ))
  "invalid type: `array.atomic.set` only allows `i8`, `i16`, `i32`, `i64` and subtypes of `anyref`"
)
