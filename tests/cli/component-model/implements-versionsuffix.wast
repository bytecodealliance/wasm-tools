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

(component
  (import "a" (implements "a:b/c@0.0.1") (versionsuffix "+b") (instance))
  (import "b" (implements "a:b/c@1.0.0-rc1") (versionsuffix "+b") (instance))
)

;; The version in `implements` must be the canonical version of the full
;; version, and the version suffix must be the rest of it.
(assert_invalid
  (component (import "my-label" (implements "a:b/c@1.2") (versionsuffix ".3") (instance)))
  "is not the canonical version of `1.2.3`")

(assert_invalid
  (component (import "my-label" (implements "a:b/c@0.2.1") (versionsuffix "+b") (instance)))
  "is not the canonical version of `0.2.1+b`")

(assert_invalid
  (component (import "my-label" (implements "a:b/c") (versionsuffix ".1") (instance)))
  "a version suffix requires the name to have a version")
