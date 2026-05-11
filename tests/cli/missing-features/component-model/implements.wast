;; RUN: wast --assert default --snapshot tests/snapshots % -f=-cm-implements

(assert_invalid
  (component
    (import "[implements=<a:b/c>]a" (instance))
  )
  "not a valid extern name")
