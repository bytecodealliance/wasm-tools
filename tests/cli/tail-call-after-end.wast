;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module
    (func
      end
      return_call 0
    )
  )
  "operators remaining after end of function")
