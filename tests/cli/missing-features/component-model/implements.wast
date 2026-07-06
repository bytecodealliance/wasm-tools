;; RUN: wast --assert default --snapshot tests/snapshots % -f=-cm-implements

;; textual usage disallowed
(assert_malformed
  (component
    (import "a" (implements "a:b/x") (instance))
  )
  "the `cm-implements` feature is not active")

(assert_malformed
  (component
    (import "a" (external-id "") (instance))
  )
  "the `cm-implements` feature is not active")

;; binary usage, even if it's not used, is disallowed without the feature
(assert_malformed
  (component binary
    "\00asm"        ;; header
    "\0d\00\01\00"  ;; version
    "\0a\05"        ;; import section, 14 bytes
    "\01"           ;; 1 import
    "\02"           ;; "import with flags"
    "\01a"          ;; name = "a"
    "\00"           ;; 0 options
  )
  "the `cm-implements` feature is not active")
