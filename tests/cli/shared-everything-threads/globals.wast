;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads

;; Check the `shared` attribute on globals.

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

  (func (export "get-a-seqcst") (result i32) (global.atomic.get seqcst $a))
  (func (export "set-b-seqcst") (global.atomic.set seqcst $b (i64.const 1)))
  (func (export "get-d-acqrel") (result i32) (global.atomic.get acqrel $d))
  (func (export "set-e-acqrel") (global.atomic.set acqrel $e (i64.const 2)))
)

(assert_invalid
  (module
    (global $a (shared i32) (i32.const 0))
    (func (export "set-shared") (global.atomic.set seqcst $a (i32.const 1)))
  )
  "global is immutable")

(assert_invalid
  (module
    (global $a (shared mut f32) (f32.const 0))
    (func (result f32) (global.atomic.get seqcst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f32) (f32.const 0))
    (func (global.atomic.set seqcst $a (f32.const 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f32) (f32.const 0))
    (func (result f32) (global.atomic.get acqrel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f32) (f32.const 0))
    (func (global.atomic.set acqrel $a (f32.const 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared f32) (f32.const 0))
    (func (result f32) (global.atomic.get seqcst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared f32) (f32.const 0))
    (func (result f32) (global.atomic.get acqrel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (result f64) (global.atomic.get seqcst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (global.atomic.set seqcst $a (f64.const 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (result f64) (global.atomic.get acqrel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (global.atomic.set acqrel $a (f64.const 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared f64) (f64.const 0))
    (func (result f64) (global.atomic.get seqcst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared f64) (f64.const 0))
    (func (result f64) (global.atomic.get acqrel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut v128) (v128.const i64x2 0 0))
    (func (result v128) (global.atomic.get seqcst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut v128) (v128.const i64x2 0 0))
    (func (global.atomic.set seqcst $a (v128.const i64x2 0 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut v128) (v128.const i64x2 0 0))
    (func (result v128) (global.atomic.get acqrel $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared mut v128) (v128.const i64x2 0 0))
    (func (global.atomic.set acqrel $a (v128.const i64x2 0 0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared v128) (v128.const i64x2 0 0))
    (func (result v128) (global.atomic.get seqcst $a))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (shared v128) (v128.const i64x2 0 0))
    (func (result v128) (global.atomic.get acqrel $a))
  )
  "invalid type")

;; Check global.atomic.rmw.*.
(module (;i32;)
  (global $a (import "spectest" "global_i32") (shared mut i32))
  (global $b (shared mut i32) (i32.const 0))
  (func (export "rmw-add-i32-seqcst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.add seqcst $a)
  (func (export "rmw-add-i32-seqcst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.add seqcst $b)
  (func (export "rmw-add-i32-acqrel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.add acqrel $a)
  (func (export "rmw-add-i32-acqrel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.add acqrel $b)
  (func (export "rmw-sub-i32-seqcst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.sub seqcst $a)
  (func (export "rmw-sub-i32-seqcst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.sub seqcst $b)
  (func (export "rmw-sub-i32-acqrel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.sub acqrel $a)
  (func (export "rmw-sub-i32-acqrel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.sub acqrel $b)
  (func (export "rmw-and-i32-seqcst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.and seqcst $a)
  (func (export "rmw-and-i32-seqcst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.and seqcst $b)
  (func (export "rmw-and-i32-acqrel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.and acqrel $a)
  (func (export "rmw-and-i32-acqrel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.and acqrel $b)
  (func (export "rmw-or-i32-seqcst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.or seqcst $a)
  (func (export "rmw-or-i32-seqcst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.or seqcst $b)
  (func (export "rmw-or-i32-acqrel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.or acqrel $a)
  (func (export "rmw-or-i32-acqrel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.or acqrel $b)
  (func (export "rmw-xor-i32-seqcst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xor seqcst $a)
  (func (export "rmw-xor-i32-seqcst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xor seqcst $b)
  (func (export "rmw-xor-i32-acqrel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xor acqrel $a)
  (func (export "rmw-xor-i32-acqrel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xor acqrel $b)
  (func (export "rmw-xchg-i32-seqcst-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xchg seqcst $a)
  (func (export "rmw-xchg-i32-seqcst-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xchg seqcst $b)
  (func (export "rmw-xchg-i32-acqrel-$a") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xchg acqrel $a)
  (func (export "rmw-xchg-i32-acqrel-$b") (param $x i32) (result i32)
    local.get $x
    global.atomic.rmw.xchg acqrel $b)
  (func (export "rmw-cmpxchg-i32-seqcst-$a") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg seqcst $a)
  (func (export "rmw-cmpxchg-i32-seqcst-$b") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg seqcst $b)
  (func (export "rmw-cmpxchg-i32-acqrel-$a") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg acqrel $a)
  (func (export "rmw-cmpxchg-i32-acqrel-$b") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg acqrel $b)
)

(module (;i64;)
  (global $a (import "spectest" "global_i64") (shared mut i64))
  (global $b (shared mut i64) (i64.const 0))
  (func (export "rmw-add-i64-seqcst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.add seqcst $a)
  (func (export "rmw-add-i64-seqcst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.add seqcst $b)
  (func (export "rmw-add-i64-acqrel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.add acqrel $a)
  (func (export "rmw-add-i64-acqrel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.add acqrel $b)
  (func (export "rmw-sub-i64-seqcst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.sub seqcst $a)
  (func (export "rmw-sub-i64-seqcst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.sub seqcst $b)
  (func (export "rmw-sub-i64-acqrel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.sub acqrel $a)
  (func (export "rmw-sub-i64-acqrel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.sub acqrel $b)
  (func (export "rmw-and-i64-seqcst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.and seqcst $a)
  (func (export "rmw-and-i64-seqcst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.and seqcst $b)
  (func (export "rmw-and-i64-acqrel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.and acqrel $a)
  (func (export "rmw-and-i64-acqrel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.and acqrel $b)
  (func (export "rmw-or-i64-seqcst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.or seqcst $a)
  (func (export "rmw-or-i64-seqcst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.or seqcst $b)
  (func (export "rmw-or-i64-acqrel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.or acqrel $a)
  (func (export "rmw-or-i64-acqrel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.or acqrel $b)
  (func (export "rmw-xor-i64-seqcst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xor seqcst $a)
  (func (export "rmw-xor-i64-seqcst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xor seqcst $b)
  (func (export "rmw-xor-i64-acqrel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xor acqrel $a)
  (func (export "rmw-xor-i64-acqrel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xor acqrel $b)
  (func (export "rmw-xchg-i64-seqcst-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xchg seqcst $a)
  (func (export "rmw-xchg-i64-seqcst-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xchg seqcst $b)
  (func (export "rmw-xchg-i64-acqrel-$a") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xchg acqrel $a)
  (func (export "rmw-xchg-i64-acqrel-$b") (param $x i64) (result i64)
    local.get $x
    global.atomic.rmw.xchg acqrel $b)
  (func (export "rmw-cmpxchg-i64-seqcst-$a") (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg seqcst $a)
  (func (export "rmw-cmpxchg-i64-seqcst-$b") (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg seqcst $b)
  (func (export "rmw-cmpxchg-i64-acqrel-$a") (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg acqrel $a)
  (func (export "rmw-cmpxchg-i64-acqrel-$b") (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    global.atomic.rmw.cmpxchg acqrel $b)
)

(assert_invalid
  (module
    (global $g (mut funcref) (ref.null func))
    (func
      ref.null func
      global.atomic.rmw.xor acqrel $g
      drop
    )
  )
  "invalid type: `global.atomic.rmw.*` only allows `i32` and `i64`")

;; Check global.atomic.rmw.* on immutable globals.
(assert_invalid
  (module
    (global $g (shared i32) (i32.const 0))
    (func (result i32)
      i32.const 1
      global.atomic.rmw.add seqcst $g
    )
  )
  "global is immutable")

(assert_invalid
  (module
    (global $g (shared i32) (i32.const 0))
    (func (result i32)
      i32.const 1
      global.atomic.rmw.sub seqcst $g
    )
  )
  "global is immutable")

(assert_invalid
  (module
    (global $g (shared i32) (i32.const 0))
    (func (result i32)
      i32.const 1
      global.atomic.rmw.and seqcst $g
    )
  )
  "global is immutable")

(assert_invalid
  (module
    (global $g (shared i32) (i32.const 0))
    (func (result i32)
      i32.const 1
      global.atomic.rmw.or seqcst $g
    )
  )
  "global is immutable")

(assert_invalid
  (module
    (global $g (shared i32) (i32.const 0))
    (func (result i32)
      i32.const 1
      global.atomic.rmw.xor seqcst $g
    )
  )
  "global is immutable")

(assert_invalid
  (module
    (global $g (shared i32) (i32.const 0))
    (func (result i32)
      i32.const 1
      global.atomic.rmw.xchg seqcst $g
    )
  )
  "global is immutable")

(assert_invalid
  (module
    (global $g (shared i32) (i32.const 0))
    (func (result i32)
      i32.const 1
      i32.const 2
      global.atomic.rmw.cmpxchg seqcst $g
    )
  )
  "global is immutable")

(assert_invalid
  (module
    (func
      global.atomic.rmw.xor acqrel 200
    )
  )
  "global index out of bounds")

(assert_invalid
  (module
    (global $g (mut funcref) (ref.null func))
    (func
      ref.null func
      global.atomic.rmw.xchg acqrel $g
      drop
    )
  )
  "invalid type: `global.atomic.rmw.xchg` only allows `i32`, `i64` and subtypes of `anyref`")

(module
  (global $g (mut eqref) (ref.null eq))
  (func
    ref.null eq
    global.atomic.rmw.xchg acqrel $g
    drop
  )
)

(assert_invalid
  (module
    (global $g (mut anyref) (ref.null any))
    (func
      ref.null any
      ref.null any
      global.atomic.rmw.cmpxchg acqrel $g
      drop
    )
  )
  "invalid type: `global.atomic.rmw.cmpxchg` only allows `i32`, `i64` and subtypes of `eqref`")

(module
  (global $g (mut eqref) (ref.null eq))
  (func
    ref.null eq
    ref.null eq
    global.atomic.rmw.cmpxchg acqrel $g
    drop
  )
)
