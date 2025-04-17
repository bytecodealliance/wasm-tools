;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module binary
    "\00\61\73\6d" ;; header
    "\01\00\00\00" ;; version
    "\02\06"       ;; memory section
    "\01"          ;; 1 count
    "\00\00\00\00" ;; (import "" "" (func))

    ;; trailing bytes
    "\00"
  )
  "section size mismatch: unexpected data at the end of the section")
