(assert_invalid
  (module
    (type (shared (array i8)))
  )
  "shared composite types require the shared-everything-threads proposal")

;; Any uses of `array.atomic.*` are first gated behind the GC feature, which the
;; `missing-features` test cannot selectively enable.
