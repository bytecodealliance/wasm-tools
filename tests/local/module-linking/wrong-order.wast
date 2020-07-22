(assert_invalid
  (module binary
    "\00asm\01\00\00\00"

    "\01\01\00" ;; type section, 1 byte, 0 entries
    "\0b\01\00" ;; data section, 1 byte, 0 entries
    "\01\01\00" ;; type section, 1 byte, 0 entries
  )
  "section out of order")
