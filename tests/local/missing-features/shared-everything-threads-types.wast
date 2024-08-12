(assert_invalid
  (module
    (type (shared (func)))
  )
  "shared composite types require the shared-everything-threads proposal")

;; Other uses of `shared` types are first gated behind the reference types
;; feature, which the `missing-features` test cannot selectively enable.
