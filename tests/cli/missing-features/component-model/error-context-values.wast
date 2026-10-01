;; RUN: wast --assert default --snapshot tests/snapshots % -f=cm-values,-cm-error-context

(assert_invalid
  (component
    (import "v" (value $v error-context))
    (export "v2" (value $v))
  )
  "requires the component model error-context feature"
)
