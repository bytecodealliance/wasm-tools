(assert_invalid
  (module binary
    "\00\61\73\6d" ;; header
    "\01\00\00\00" ;; version
    "\01\04\01\60\00\00" ;; type section, 1 type, (func)
    "\03\02\01\00" ;; func section, 1 function, type 0

    "\0a\12\01"        ;; code section
    "\10"              ;; function size
    "\00"              ;; no locals
    "\02\40"           ;; block
    "\41\01"           ;; i32.const 1

    "\0e"              ;; br_table
    "\02"              ;; 2 count
    "\00\00"           ;; 2 entries
    "\81\80\80\80\21"  ;; default (overlong integer)

    "\0b"              ;; end
    "\0b"              ;; end
  )
  "integer too large")
