(assert_invalid
  (module
    (table funcref (elem (call_indirect)))
  )
  "non-constant operator")

(assert_invalid
  (module
    (table 1 1 funcref (call_indirect))
  )
  "non-constant operator")
