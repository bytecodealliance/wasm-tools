(assert_invalid
  (module binary
    "\00asm"
    "\0a\00\01\00"
    )
  "WebAssembly component model feature not enabled")
