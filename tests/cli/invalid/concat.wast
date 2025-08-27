;; RUN: wast --assert default --snapshot tests/snapshots %

;; corrupt binary: multiple modules concatenated
(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00" ;; magic header
    "\01\04"    ;; type section
    "\01"       ;; 1 count
    "\60\00\00" ;; no params or results
    "\00asm" "\01\00\00\00" ;; magic header
  )
  "expected section, got wasm magic number")
