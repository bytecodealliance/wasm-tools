(assert_invalid
  (module binary
    "\00asm" "\01\00\00\00" ;; magic header

    "\01\04"    ;; type section
    "\01"       ;; 1 count
    "\60\00\00" ;; no params or results

    "\03\02"    ;; func section
    "\01"       ;; 1 count
    "\00"       ;; type 0

    "\0a\05"    ;; code section
    "\01"       ;; 1 count
    "\03"       ;; size of function
    "\00"       ;; no locals
    "\0b"       ;; end
    "\01"       ;; nop
  )
  "operators remaining after end of function")

(assert_invalid
  (module binary
    "\00asm" "\01\00\00\00" ;; magic header

    "\01\04"    ;; type section
    "\01"       ;; 1 count
    "\60\00\00" ;; no params or results

    "\03\02"    ;; func section
    "\01"       ;; 1 count
    "\00"       ;; type 0

    "\0a\05"    ;; code section
    "\01"       ;; 1 count
    "\03"       ;; size of function
    "\00"       ;; no locals
    "\0b"       ;; end
    "\9d"       ;; f64.trunc
  )
  "operators remaining after end of function")

(assert_invalid
  (module binary
    "\00asm" "\01\00\00\00" ;; magic header

    "\01\04"    ;; type section
    "\01"       ;; 1 count
    "\60\00\00" ;; no params or results

    "\03\02"    ;; func section
    "\01"       ;; 1 count
    "\00"       ;; type 0

    "\0a\05"    ;; code section
    "\01"       ;; 1 count
    "\03"       ;; size of function
    "\00"       ;; no locals
    "\0b"       ;; end
    "\0b"       ;; end
  )
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end unreachable))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end return))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end br 0))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end br 100000))
  "operators remaining after end of function")

(assert_invalid
  (module
    (func end br_table 0))
  "operators remaining after end of function")
