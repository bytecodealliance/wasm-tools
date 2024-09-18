(assert_invalid
  (component
    ;; Refer to a non-existent function type (i.e., 0); validation for
    ;; `thread.spawn` happens first.
    (core func $spawn (canon thread.spawn 0))
  )
  "`thread.spawn` requires the shared-everything-threads proposal")

(assert_invalid
  (component
    (core func $concurrency (canon thread.hw_concurrency))
  )
  "`thread.hw_concurrency` requires the shared-everything-threads proposal")
