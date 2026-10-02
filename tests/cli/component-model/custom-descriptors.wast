;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-descriptors

;; A core export alias can't refer to an exact function.
(assert_malformed
  (component binary
    "\00\61\73\6d\0d\00\01\00"                    ;; preamble
    "\01\1f\00\61\73\6d\01\00\00\00"              ;; core module section
    "\01\04\01\60\00\00"                          ;;   type section
    "\03\02\01\00"                                ;;   func section
    "\07\05\01\01\66\00\00"                       ;;   export section
    "\0a\04\01\02\00\0b"                          ;;   code section
    "\02\04\01\00\00\00"                          ;; core instance section: (instantiate 0)
    "\06\07\01\00\20\01\00\01\66"                 ;; alias section: core export alias with sort 0x20
  )
  "exact type is not allowed in core export aliases"
)
