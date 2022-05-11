(assert_invalid
  (module binary
    "\00\61\73\6d" ;; header
    "\01\00\00\00" ;; version
    "\01\04\01\60\00\00" ;; type section, 1 type, (func)
    "\03\02\01\00" ;; func section, 1 function, type 0

    ;; code section, 9 functions
    "\0a\01\09"
  )
  "function and code section have inconsistent lengths")
