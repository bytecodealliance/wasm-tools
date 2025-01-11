(assert_invalid
  (module
    (table 1_000_000_000 funcref)
  )
  "minimum table size is out of bounds")
