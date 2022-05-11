(assert_invalid
  (module binary
    "\00\61\73\6d" ;; header
    "\01\00\00\00" ;; version

    "\01\04\01"       ;; type section, 1 count
    "\60\00\00"       ;; (type (func))

    "\03\02\01\00"    ;; func section, 1 count, type 0

    "\0a\12"          ;; code section
    "\01"             ;; 1 count
    "\10"             ;; function size
    "\ff\ff\ff\ff\28" ;; tons of locals, too big
    ;; not enough data for locals
    "\00\00\00\00\00"
    "\00\00\00\00\00"
    "\00"
  )
  "integer too large")
