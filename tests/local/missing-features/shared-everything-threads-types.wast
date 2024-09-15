(assert_invalid
  (module
    (type (shared (func)))
  )
  "shared composite types require the shared-everything-threads proposal")
