;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-canon-names

(component
  (component
    (import "a:b/c@1" (versionsuffix ".2.3") (instance))
    (import "a:b/c@0.2" (versionsuffix ".3") (instance))
    (import "a:b/c@0.0.3" (instance))
    (import "a:b/c@1.2.3-rc.1" (instance))
    (import "a:b/c@0.2.3-rc.1" (instance))
    (import "a:b/c@0.0.3-rc.1" (instance))
  )
)

(assert_invalid
  (component (import "a:b/c@1" (versionsuffix "2.3") (instance)))
  "invalid interface version")

(assert_invalid
  (component (import "a:b/c@1" (versionsuffix ".2") (instance)))
  "invalid interface version")

(assert_invalid
  (component (import "a:b/c@1" (versionsuffix ".2.3") (func)))
  "only instances can have")
