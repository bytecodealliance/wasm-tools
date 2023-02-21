(assert_invalid
  (module binary
    "\00asm"
    "\0a\00\01\00"
    )
  "encoded as a component but the WebAssembly component model feature is not enabled")
