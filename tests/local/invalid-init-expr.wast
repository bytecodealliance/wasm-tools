(assert_invalid
  (module
    (table funcref (elem (call_indirect)))
  )
  "non-constant operator")

(assert_invalid
  (module
    (table 1 1 funcref (call_indirect))
  )
  ;; note that this error message will change when wasmparser implements the
  ;; above feature
  "invalid value type")
