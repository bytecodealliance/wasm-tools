;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-canon-names,cm-implements

;; versionsuffix combined with implements: the suffix refers to the
;; version in implements, not the main label.
(component
  (component
    (import "my-label" (implements "a:b/c@1") (versionsuffix ".2.3") (instance))
    (import "other" (implements "a:b/c@0.2") (versionsuffix ".3") (instance))
    (instance $a)
    (export "x" (implements "a:b/c@1") (versionsuffix ".2.3") (instance $a))
  )
)

(component (import "my-label" (implements "a:b/c@1") (versionsuffix ".2.3") (instance)))

(assert_invalid
  (component (import "my-label" (implements "a:b/c@1") (versionsuffix "2.3") (instance)))
  "invalid interface version")
