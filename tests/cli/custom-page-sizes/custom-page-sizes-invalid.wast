;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-page-sizes

;; Page size that is not a power of two.
(assert_malformed
  (module quote "(memory 0 (pagesize 3))")
  "invalid custom page size"
)

;; Power-of-two page sizes that are not 1 or 64KiB.
(assert_invalid
  (module (memory 0 (pagesize 2)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 4)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 8)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 16)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 32)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 64)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 128)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 256)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 512)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 1024)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 2048)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 4096)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 8192)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 16384)))
  "invalid custom page size"
)
(assert_invalid
  (module (memory 0 (pagesize 32768)))
  "invalid custom page size"
)

;; Power-of-two page size that is larger than 64KiB.
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
