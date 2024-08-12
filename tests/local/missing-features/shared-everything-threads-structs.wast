(assert_invalid
  (module
    (type (shared (struct)))
  )
  "shared composite types require the shared-everything-threads proposal")

;; Any uses of `struct.atomic.*` are first gated behind the GC feature, which
;; the `missing-features` test cannot selectively enable.
