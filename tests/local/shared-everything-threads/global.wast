;; Styled after ../../testsuite/global.wast

(module
  (global $a (import "spectest" "global_i32") (shared i32))
  (global $b (import "spectest" "global_i64") (shared mut i64))
  (global $c (shared i32) (i32.const 0))
  (global $d (shared mut i64) (i64.const 1))

  (func (export "get-a-seqcst") (result i32) (global.atomic.get seq_cst $a))
  (func (export "get-c-acqrel") (result i32) (global.atomic.get acq_rel $c))
  (func (export "set-b-seqcst") (global.atomic.set seq_cst $b (i64.const 1)))
  (func (export "set-d-acqrel") (global.atomic.set acq_rel $d (i64.const 2)))
)

(assert_malformed
  (module quote "(global (mut shared i64) (i64.const -1))")
  "unexpected token")

(assert_invalid
  (module
    (global $a (shared f32) (f32.const 0))
    (func (export "set-shared") (global.atomic.set seq_cst $a (f32.const 1.0)))
  )
  "global is immutable")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (export "set-shared") (global.atomic.set seq_cst $a (f64.const 1.0)))
  )
  "invalid type")
