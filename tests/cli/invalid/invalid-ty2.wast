;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_invalid
  (module
    (func block (type 2))
  )
  "type index out of bounds")
