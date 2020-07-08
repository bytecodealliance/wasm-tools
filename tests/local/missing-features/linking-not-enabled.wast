(assert_invalid
  (module binary
    "\00asm"
    "\01\00\00\00"
    "\64\01\00"
    )
  "module linking proposal not enabled")
