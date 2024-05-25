(assert_malformed
  (module quote
    "(func $\"\\ff\")"
  )
  "malformed UTF-8 encoding of string-based id")
