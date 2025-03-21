;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module quote
    "(func end)")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end block)")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end i32.add)")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end unreachable)")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end br 0)")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end return)")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end return_call 0)")
  "operators remaining after end of function")
