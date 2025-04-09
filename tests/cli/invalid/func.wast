;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
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

(assert_malformed
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

(assert_malformed
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

(assert_malformed
  (module quote
    "(func end unreachable)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\06\01\04\00\0b\00\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end return)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\06\01\04\00\0b\0f\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end br 0)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\07\01\05\00\0b\0c\00\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end br 100000)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\09\01\07\00\0b\0c\a0\8d\06\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end br_table 0)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\08\01\06\00\0b\0e\00\00\0b")
  "operators remaining after end of function")
