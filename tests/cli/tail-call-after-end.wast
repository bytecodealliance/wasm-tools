;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_invalid
  (module
    (func
      end
      return_call 0
    )
  )
  "operators remaining after end of function")
