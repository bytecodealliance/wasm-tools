;; Styled after ../../testsuite/global.wast

(module
  (global (import "spectest" "global_i32") (shared i32))
  (global (import "spectest" "global_f64") (shared mut f64))
  (global $a (shared i64) (i64.const 0))
  (global $b (shared mut i64) (i64.const 1))
)

(assert_malformed
  (module quote "(global (mut shared i64) (i64.const -1))")
  "unexpected token")
