;; Styled after ../../testsuite/global.wast

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

(assert_malformed
  (module quote "(global (mut shared i64) (i64.const -1))")
  "unexpected token")

(assert_invalid
  (module
    (global $a (shared i32) (i32.const 0))
    (func (export "set-shared") (global.atomic.set seq_cst $a (i32.const 1)))
  )
  "global is immutable")

(assert_invalid
  (module
    (global $a (shared mut f64) (f64.const 0))
    (func (export "set-shared") (global.atomic.set acq_rel $a (f64.const 1.0)))
  )
  "invalid type")

(assert_invalid
  (module
    (global $a (import "spectest" "global_ref") (shared funcref))
  )
  "shared value type")
