(assert_invalid
  (module binary
    "\00asm" "\01\00\00\00"
    "\0d\01\00"
    )
  "exceptions proposal not enabled")

(assert_invalid
  (module
    (import "" "" (tag))
    )
  "exceptions proposal not enabled")
