(assert_invalid
  (module binary
    "\00asm"
    "\01\00\00\00"
    "\0e\01\00"
    )
  "module linking proposal not enabled")
