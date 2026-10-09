;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-canon-names

(component
  (component
    (import "a:b/c@1" (versionsuffix ".2.3") (instance))
    (import "a:b/c@0.2" (versionsuffix ".3") (instance))
    (import "a:b/c@0.0.3" (instance))
    (import "a:b/c@1.2.3-rc.1" (instance))
    (import "a:b/c@0.2.3-rc.1" (instance))
    (import "a:b/c@0.0.3-rc.1" (instance))
    (import "a:b/c@2" (versionsuffix ".0.0") (instance))
    (import "a:b/c@3" (versionsuffix ".2.3+abc") (instance))
    (import "a:b/c@0.3" (versionsuffix ".1+b.2") (instance))
    (import "a:b/c@0.0.1" (versionsuffix "+b") (instance))
    (import "a:b/c@1.0.0-rc1" (versionsuffix "+b") (instance))
  )
)

(component
  (instance $i)
  (export "a:b/c@1" (versionsuffix ".2.3") (instance $i))
  (export "a:b/c@0.2" (versionsuffix ".1") (instance $i))
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

;; The version in the name must be the canonical version of the full version,
;; and the version suffix must be the rest of it.
(assert_invalid
  (component (import "a:b/c@1.2" (versionsuffix ".3") (instance)))
  "is not the canonical version of `1.2.3`")

(assert_invalid
  (component (import "a:b/c@1." (versionsuffix "2.3") (instance)))
  "is not the canonical version of `1.2.3`")

(assert_invalid
  (component (import "a:b/c@0.2.1" (versionsuffix "+b") (instance)))
  "is not the canonical version of `0.2.1+b`")

(assert_invalid
  (component (import "a:b/c@0.0" (versionsuffix ".1") (instance)))
  "is not the canonical version of `0.0.1`")

(assert_invalid
  (component (import "a:b/c@1.0.0" (versionsuffix "-rc1") (instance)))
  "is not the canonical version of `1.0.0-rc1`")

(assert_invalid
  (component (import "a:b/c@1.0.0-rc1" (versionsuffix ".1") (instance)))
  "invalid interface version")

(assert_invalid
  (component
    (instance $i)
    (export "a:b/c@1.2" (versionsuffix ".3") (instance $i)))
  "is not the canonical version of `1.2.3`")

;; A version suffix requires a version in the name.
(assert_invalid
  (component (import "a:b/c" (versionsuffix ".1") (instance)))
  "a version suffix requires the name to have a version")

;; A version suffix requires an interface name if there's no `implements`.
(assert_invalid
  (component (import "a" (versionsuffix ".1") (instance)))
  "`versionsuffix` requires an interface name or `implements`")

(assert_invalid
  (component
    (import "a:b/c@0.2" (versionsuffix ".0") (instance))
    (import "a:b/c@0.2" (versionsuffix ".1") (instance)))
  "import name `a:b/c@0.2.1` conflicts with previous name `a:b/c@0.2.0`")

(assert_invalid
  (component
    (instance $i)
    (export "a:b/c@0.2" (versionsuffix ".0") (instance $i))
    (export "a:b/c@0.2" (versionsuffix ".1") (instance $i)))
  "export name `a:b/c@0.2.1` conflicts with previous name `a:b/c@0.2.0`")

;; Subtyping and alias errors include version suffixes.
(assert_invalid
  (component
    (component $c (import "a:b/c@0.2" (versionsuffix ".1") (instance)))
    (instance (instantiate $c)))
  "missing import named `a:b/c@0.2.1`")

(assert_invalid
  (component
    (component $c
      (import "a:b/c@0.2" (versionsuffix ".1") (instance (export "f" (func)))))
    (instance $i)
    (instance (instantiate $c (with "a:b/c@0.2" (instance $i)))))
  "type mismatch for import `a:b/c@0.2.1`")

(assert_invalid
  (component
    (component $c
      (import "i" (instance (export "a:b/c@0.2" (versionsuffix ".1") (instance)))))
    (instance $i)
    (instance (instantiate $c (with "i" (instance $i)))))
  "missing expected export `a:b/c@0.2.1`")

(assert_invalid
  (component
    (component $c
      (import "i" (instance
        (export "a:b/c@0.2" (versionsuffix ".1") (instance (export "f" (func)))))))
    (instance $inner)
    (instance $i (export "a:b/c@0.2" (versionsuffix ".0") (instance $inner)))
    (instance (instantiate $c (with "i" (instance $i)))))
  "type mismatch in instance export `a:b/c@0.2.1`")

(assert_invalid
  (component
    (import "i" (instance $i (export "a:b/c@0.2" (versionsuffix ".1") (instance))))
    (alias export $i "a:b/c@0.2" (func)))
  "export `a:b/c@0.2.1` for instance 0 is not a func")
