(assert_invalid
  (module
    (memory 0)
    (func $f)
    (data (ref.func $f) ""))
  "type mismatch")
