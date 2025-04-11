;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module binary
    "\00asm\01\00\00\00"

    "\0c\05\ff\ff\ff\ff\01" ;; data section, 5 bytes, huge count
  )
  "data count is non-zero but data section is absent")

