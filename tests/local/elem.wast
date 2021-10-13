(assert_invalid
  (module
    (table 1 funcref)
    (elem (i32.const 0) funcref (ref.null extern))
  )
  "type mismatch"
)
(assert_invalid
  (module
    (table 1 funcref)
    (elem (i32.const 0) funcref (if))
  )
  "constant expression required"
)
