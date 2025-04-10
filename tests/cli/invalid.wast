;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_invalid
  (module
    (table 1 funcref)
    (func table.init 0 100))
  "unknown elem segment")

(assert_malformed
  (module
    (func else))
  "`else` found outside `If` block")
