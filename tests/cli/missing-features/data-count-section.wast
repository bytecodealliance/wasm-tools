;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1

;; The data count section was introduced by the bulk-memory proposal.
(assert_invalid
  (module binary
    "\00asm" "\01\00\00\00"
    "\0c\01\00"                       ;; datacount section: 0
  )
  "data count section requires the bulk-memory proposal")
