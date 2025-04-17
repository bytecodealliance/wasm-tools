;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module binary
    "\00\61\73\6d" ;; header
    "\01\00\00\00" ;; version
    "\05\05"       ;; memory section
    "\01"          ;; 1 count
    "\00"          ;; no flags
    "\01"          ;; minimum

    ;; trailing bytes
    "\00\00"
  )
  "unexpected data at the end of the section")
