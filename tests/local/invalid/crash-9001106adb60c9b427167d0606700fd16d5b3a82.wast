(assert_invalid
  (module binary

    "\00\61\73\6d" ;; header
    "\01\00\00\00" ;; version
    "\01\02"       ;; type section
    "\00"          ;; 0 count

    ;; trailing bytes
    "\00"
  )
  "unexpected data at the end of the section")
