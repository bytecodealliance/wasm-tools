;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module quote
    "(func block (type 2))"
  )
  "type index out of bounds")

(assert_invalid
  (module
    (func block (type 2) end)
  )
  "type index out of bounds")
