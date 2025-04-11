;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module quote
    "(func block (type 2))"
  )
  "control frames remain at end of function body or expression")

(assert_invalid
  (module
    (func block (type 2) end)
  )
  "type index out of bounds")
