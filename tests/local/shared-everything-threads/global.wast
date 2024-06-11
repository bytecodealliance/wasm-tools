;; Check shared attribute.

(module
  ;; Imported.
  (global (import "spectest" "global_i32") (shared i32))
  (global (import "spectest" "global_i32_mut") (shared mut i32))
  (global (import "spectest" "global_i64") (shared i64))
  (global (import "spectest" "global_i64_mut") (shared mut i64))
  (global (import "spectest" "global_f32") (shared i32))
  (global (import "spectest" "global_f32_mut") (shared mut f32))
  (global (import "spectest" "global_f64") (shared f64))
  (global (import "spectest" "global_f64_mut") (shared mut f64))
  (global (import "spectest" "global_v128") (shared v128))
  (global (import "spectest" "global_v128_mut") (shared mut v128))

  ;; Initialized.
  (global (shared i32) (i32.const 0))
  (global (shared mut i32) (i32.const 0))
  (global (shared i64) (i64.const 0))
  (global (shared mut i64) (i64.const 0))
  (global (shared f32) (f32.const 0))
  (global (shared mut f32) (f32.const 0))
  (global (shared f64) (f64.const 0))
  (global (shared mut f64) (f64.const 0))
  (global (shared v128) (v128.const i64x2 0 0))
  (global (shared mut v128) (v128.const i64x2 0 0))
)

(assert_malformed
  (module quote "(global (mut shared i64) (i64.const -1))")
  "unexpected token")

(assert_invalid
  (module (global $a (import "spectest" "global_ref") (shared funcref)))
  "shared value type")

;; Check global.atomic.get, global.atomic.set.

(module
  (global $a (import "spectest" "global_i32") (shared i32))
  (global $b (import "spectest" "global_i64") (shared mut i64))
  (global $d (shared i32) (i32.const 0))
  (global $e (shared mut i64) (i64.const 1))

  (func (export "get-a-seqcst") (result i32) (global.atomic.get seq_cst $a))
  (func (export "set-b-seqcst") (global.atomic.set seq_cst $b (i64.const 1)))
  (func (export "get-d-acqrel") (result i32) (global.atomic.get acq_rel $d))
  (func (export "set-e-acqrel") (global.atomic.set acq_rel $e (i64.const 2)))
)

(assert_invalid
  (module
    (global $a (shared i32) (i32.const 0))
    (func (export "set-shared") (global.atomic.set seq_cst $a (i32.const 1)))
  )
  "global is immutable")

(assert_invalid
  (module
    (global $a (shared mut f32) (f32.const 0))
    (func (result f32) (global.atomic.get seq_cst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f32) (f32.const 0))
    (func (global.atomic.set seq_cst $a (f32.const 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f32) (f32.const 0))
    (func (result f32) (global.atomic.get acq_rel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f32) (f32.const 0))
    (func (global.atomic.set acq_rel $a (f32.const 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared f32) (f32.const 0))
    (func (result f32) (global.atomic.get seq_cst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared f32) (f32.const 0))
    (func (result f32) (global.atomic.get acq_rel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (result f64) (global.atomic.get seq_cst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (global.atomic.set seq_cst $a (f64.const 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (result f64) (global.atomic.get acq_rel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (global.atomic.set acq_rel $a (f64.const 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared f64) (f64.const 0))
    (func (result f64) (global.atomic.get seq_cst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared f64) (f64.const 0))
    (func (result f64) (global.atomic.get acq_rel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut v128) (v128.const i64x2 0 0))
    (func (result v128) (global.atomic.get seq_cst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut v128) (v128.const i64x2 0 0))
    (func (global.atomic.set seq_cst $a (v128.const i64x2 0 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut v128) (v128.const i64x2 0 0))
    (func (result v128) (global.atomic.get acq_rel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut v128) (v128.const i64x2 0 0))
    (func (global.atomic.set acq_rel $a (v128.const i64x2 0 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared v128) (v128.const i64x2 0 0))
    (func (result v128) (global.atomic.get seq_cst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared v128) (v128.const i64x2 0 0))
    (func (result v128) (global.atomic.get acq_rel $a))
  )
  "invalid type")

;; Check global.atomic.rmw.*.
(module (;i32;)
  (global $a (import "spectest" "global_i32") (shared mut i32))
  (global $b (shared mut i32) (i32.const 0))
  (func (export "rmw-add-i32-seq_cst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.add seq_cst $a)
  (func (export "rmw-add-i32-seq_cst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.add seq_cst $b)
  (func (export "rmw-add-i32-acq_rel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.add acq_rel $a)
  (func (export "rmw-add-i32-acq_rel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.add acq_rel $b)
  (func (export "rmw-sub-i32-seq_cst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.sub seq_cst $a)
  (func (export "rmw-sub-i32-seq_cst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.sub seq_cst $b)
  (func (export "rmw-sub-i32-acq_rel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.sub acq_rel $a)
  (func (export "rmw-sub-i32-acq_rel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.sub acq_rel $b)
  (func (export "rmw-and-i32-seq_cst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.and seq_cst $a)
  (func (export "rmw-and-i32-seq_cst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.and seq_cst $b)
  (func (export "rmw-and-i32-acq_rel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.and acq_rel $a)
  (func (export "rmw-and-i32-acq_rel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.and acq_rel $b)
  (func (export "rmw-or-i32-seq_cst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.or seq_cst $a)
  (func (export "rmw-or-i32-seq_cst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.or seq_cst $b)
  (func (export "rmw-or-i32-acq_rel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.or acq_rel $a)
  (func (export "rmw-or-i32-acq_rel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.or acq_rel $b)
  (func (export "rmw-xor-i32-seq_cst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xor seq_cst $a)
  (func (export "rmw-xor-i32-seq_cst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xor seq_cst $b)
  (func (export "rmw-xor-i32-acq_rel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xor acq_rel $a)
  (func (export "rmw-xor-i32-acq_rel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xor acq_rel $b)
  (func (export "rmw-xchg-i32-seq_cst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xchg seq_cst $a)
  (func (export "rmw-xchg-i32-seq_cst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xchg seq_cst $b)
  (func (export "rmw-xchg-i32-acq_rel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xchg acq_rel $a)
  (func (export "rmw-xchg-i32-acq_rel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xchg acq_rel $b)
  (func (export "rmw-cmpxchg-i32-seq_cst-$a") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg seq_cst $a)
  (func (export "rmw-cmpxchg-i32-seq_cst-$b") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg seq_cst $b)
  (func (export "rmw-cmpxchg-i32-acq_rel-$a") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg acq_rel $a)
  (func (export "rmw-cmpxchg-i32-acq_rel-$b") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg acq_rel $b)
)

(module (;i64;)
  (global $a (import "spectest" "global_i64") (shared mut i64))
  (global $b (shared mut i64) (i64.const 0))
  (func (export "rmw-add-i64-seq_cst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.add seq_cst $a)
  (func (export "rmw-add-i64-seq_cst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.add seq_cst $b)
  (func (export "rmw-add-i64-acq_rel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.add acq_rel $a)
  (func (export "rmw-add-i64-acq_rel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.add acq_rel $b)
  (func (export "rmw-sub-i64-seq_cst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.sub seq_cst $a)
  (func (export "rmw-sub-i64-seq_cst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.sub seq_cst $b)
  (func (export "rmw-sub-i64-acq_rel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.sub acq_rel $a)
  (func (export "rmw-sub-i64-acq_rel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.sub acq_rel $b)
  (func (export "rmw-and-i64-seq_cst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.and seq_cst $a)
  (func (export "rmw-and-i64-seq_cst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.and seq_cst $b)
  (func (export "rmw-and-i64-acq_rel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.and acq_rel $a)
  (func (export "rmw-and-i64-acq_rel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.and acq_rel $b)
  (func (export "rmw-or-i64-seq_cst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.or seq_cst $a)
  (func (export "rmw-or-i64-seq_cst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.or seq_cst $b)
  (func (export "rmw-or-i64-acq_rel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.or acq_rel $a)
  (func (export "rmw-or-i64-acq_rel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.or acq_rel $b)
  (func (export "rmw-xor-i64-seq_cst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xor seq_cst $a)
  (func (export "rmw-xor-i64-seq_cst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xor seq_cst $b)
  (func (export "rmw-xor-i64-acq_rel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xor acq_rel $a)
  (func (export "rmw-xor-i64-acq_rel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xor acq_rel $b)
  (func (export "rmw-xchg-i64-seq_cst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xchg seq_cst $a)
  (func (export "rmw-xchg-i64-seq_cst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xchg seq_cst $b)
  (func (export "rmw-xchg-i64-acq_rel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xchg acq_rel $a)
  (func (export "rmw-xchg-i64-acq_rel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xchg acq_rel $b)
  (func (export "rmw-cmpxchg-i64-seq_cst-$a") (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg seq_cst $a)
  (func (export "rmw-cmpxchg-i64-seq_cst-$b") (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg seq_cst $b)
  (func (export "rmw-cmpxchg-i64-acq_rel-$a") (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg acq_rel $a)
  (func (export "rmw-cmpxchg-i64-acq_rel-$b") (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg acq_rel $b)
)

