(assert_invalid
  (module binary
    "\00asm"
    "\0a\00\01\00"
    )
  "unknown binary version: 0xa, note: the WebAssembly component model feature is not enabled")
