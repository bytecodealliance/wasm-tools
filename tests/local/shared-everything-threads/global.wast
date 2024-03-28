;; Styled after ../../testsuite/global.wast

(module
  (global $a (shared i64) (i64.const 0))
  (global $b (shared mut i64) (i64.const 1))
)

(assert_malformed
  (module quote "(global (mut shared i64) (i64.const -1))")
  "unexpected token")
