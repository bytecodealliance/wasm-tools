(assert_invalid
  (module binary
    "\00\61\73\6d"    ;; header
    "\01\00\00\00"    ;; version
    "\04\05\01"       ;; table section, 1 count,
    "\70\01\02\02"    ;; (table 2 2 funcref)

    "\09\08"          ;; element section
    "\01"             ;; 1 count
    "\00"             ;; no flags
    "\41\00"          ;; (init) i32.const 0
    "\0b"             ;; (init) end
    "\ff\ff\ff"       ;; cut-off-leb of number-of-items
  )
  "unexpected end-of-file")
