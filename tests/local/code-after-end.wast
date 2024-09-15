(assert_invalid
  (module
    (func end))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end block))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end i32.add))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end unreachable))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end br 0))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end return))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end return_call 0))
  "operators remaining after end of function")
