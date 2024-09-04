(assert_invalid
  (module
    (func
      ref.null (shared any)
      drop
    )
  )
  "shared reference types require the shared-everything-threads proposal")
