;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module quote
    "(func $)")
  "empty identifier")
