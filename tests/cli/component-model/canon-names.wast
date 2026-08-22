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

;; Track-prefix mismatch: "1.0" + ".1" = "1.0.1" whose track is "1", not "1.0"
(assert_invalid
  (component (import "a:b/c@1.0" (versionsuffix ".1") (instance)))
  "invalid interface version")

;; Track-prefix mismatch: "0.0" + ".3" = "0.0.3" whose track is "0.0.3", not "0.0"
(assert_invalid
  (component (import "a:b/c@0.0" (versionsuffix ".3") (instance)))
  "invalid interface version")

;; Track-prefix mismatch: "2.0" + ".0" = "2.0.0" whose track is "2", not "2.0"
(assert_invalid
  (component (import "a:b/c@2.0" (versionsuffix ".0") (instance)))
  "invalid interface version")
