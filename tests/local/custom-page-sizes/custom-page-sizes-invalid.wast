(assert_malformed
  (module quote "(memory 0 (pagesize 3))")
  "invalid custom page size"
)

;; Power of two page size that is larger than 64KiB.
(assert_invalid
  (module (memory 0 (pagesize 0x20000)))
  "invalid custom page size"
)

;; Power of two page size that cannot fit in a u64 to exercise checks against
;; shift overflow.
(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00"
    "\05\04\01"                ;; Memory section

    ;; memory 0
    "\08"                      ;; flags w/ custom page size
    "\00"                      ;; minimum = 0
    "\41"                      ;; pagesize = 2**65
  )
  "invalid custom page size"
)
