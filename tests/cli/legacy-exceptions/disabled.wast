;; RUN: wast %

(assert_invalid
  (module (func try end))
  "legacy exceptions support is not enabled")

(assert_invalid
  (module (func catch 0))
  "legacy exceptions support is not enabled")

(assert_invalid
  (module (func catch_all))
  "legacy exceptions support is not enabled")
