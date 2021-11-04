(assert_invalid
  (module
    (memory 0)
    (func $f)
    (elem declare $f)
    (data (ref.func $f) ""))
  "type mismatch")

(assert_invalid
  (module
    (memory 0)
    (func $f)
    (data (ref.func $f) ""))
  "undeclared function reference")
