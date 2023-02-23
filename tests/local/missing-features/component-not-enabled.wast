(assert_invalid
  (module binary
    "\00asm"
    "\0a\00\01\00"
    )
  "unknown binary version and encoding combination: 0xa and 0x1, note: encoded as a component but the WebAssembly component model feature is not enabled - enable the feature to allow component validation")
