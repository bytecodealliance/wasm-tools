(assert_invalid
  (module
    (table shared 1 funcref)
  )
  "shared tables require the shared-everything-threads proposal")
