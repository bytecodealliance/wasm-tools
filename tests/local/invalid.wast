(assert_invalid
  (module
    (table 1 funcref)
    (func table.init 0 100))
  "unknown elem segment")

(assert_invalid
  (module
    (func else))
  "else found outside of an `if` block")
