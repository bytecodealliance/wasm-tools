;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-canon-names,cm-implements

;; The `implements`, `version-suffix`, and `external-id` attributes on
;; import/export names are an unordered list in the text format, so they may
;; be specified in any order. Printing always emits them in a canonical order.

;; imports, all permutations
(component
  (import "a" (implements "a:b/c@1") (version-suffix ".2.3") (external-id "x") (instance))
  (import "b" (implements "a:b/c@1") (external-id "x") (version-suffix ".2.3") (instance))
  (import "c" (version-suffix ".2.3") (implements "a:b/c@1") (external-id "x") (instance))
  (import "d" (version-suffix ".2.3") (external-id "x") (implements "a:b/c@1") (instance))
  (import "e" (external-id "x") (implements "a:b/c@1") (version-suffix ".2.3") (instance))
  (import "f" (external-id "x") (version-suffix ".2.3") (implements "a:b/c@1") (instance))

  ;; pairs
  (import "g" (external-id "x") (implements "a:b/c") (instance))
  (import "h" (version-suffix ".2.3") (implements "a:b/c@1") (instance))
  (import "a:b/c@1" (version-suffix ".2.3") (external-id "x") (instance))
  (import "i" (external-id "x") (func))
)

;; exports, all permutations
(component
  (instance $i)
  (export "a" (implements "a:b/c@1") (version-suffix ".2.3") (external-id "x") (instance $i))
  (export "b" (implements "a:b/c@1") (external-id "x") (version-suffix ".2.3") (instance $i))
  (export "c" (version-suffix ".2.3") (implements "a:b/c@1") (external-id "x") (instance $i))
  (export "d" (version-suffix ".2.3") (external-id "x") (implements "a:b/c@1") (instance $i))
  (export "e" (external-id "x") (implements "a:b/c@1") (version-suffix ".2.3") (instance $i))
  (export "f" (external-id "x") (version-suffix ".2.3") (implements "a:b/c@1") (instance $i))

  ;; with an identifier and a type ascription
  (export $g "g" (external-id "x") (implements "a:b/c") (instance $i) (instance))
)

;; instance and component type declarations
(component
  (type (instance
    (export "a" (external-id "x") (version-suffix ".2.3") (implements "a:b/c@1") (instance))
    (export "b" (external-id "x") (implements "a:b/c") (instance))
  ))
  (type (component
    (import "a" (external-id "x") (version-suffix ".2.3") (implements "a:b/c@1") (instance))
    (import "b" (version-suffix ".2.3") (implements "a:b/c@1") (instance))
    (export "a" (version-suffix ".2.3") (external-id "x") (implements "a:b/c@1") (instance))
    (export "b" (external-id "x") (implements "a:b/c") (instance))
  ))
  (import "c" (instance
    (export "a" (external-id "x") (implements "a:b/c") (instance))
  ))
)

;; instances created from inline exports
(component
  (instance $a)
  (instance
    (export "a" (external-id "x") (version-suffix ".2.3") (implements "a:b/c@1") (instance $a))
    (export "b" (external-id "x") (implements "a:b/c") (instance $a))
  )
)

;; Validation still applies regardless of the order attributes are written in.
(assert_invalid
  (component (import "a" (version-suffix ".1") (implements "a:b/c") (instance)))
  "a version suffix requires the name to have a version")

;; Each attribute may only be specified once.
(assert_malformed
  (component quote
    "(import \"a\" (implements \"a:b/c\") (implements \"a:b/c\") (instance))")
  "duplicate `implements` attribute")
(assert_malformed
  (component quote
    "(import \"a\" (implements \"a:b/c\") (external-id \"x\") (implements \"a:b/c\") (instance))")
  "duplicate `implements` attribute")
(assert_malformed
  (component quote
    "(import \"a\" (version-suffix \".1\") (implements \"a:b/c@1\") (version-suffix \".1\") (instance))")
  "duplicate `version-suffix` attribute")
(assert_malformed
  (component quote
    "(import \"a\" (external-id \"x\") (external-id \"y\") (instance))")
  "duplicate `external-id` attribute")
(assert_malformed
  (component quote
    "(instance $i)"
    "(export \"a\" (external-id \"x\") (implements \"a:b/c\") (external-id \"x\") (instance $i))")
  "duplicate `external-id` attribute")
(assert_malformed
  (component quote
    "(type (instance (export \"a\" (implements \"a:b/c\") (external-id \"x\") (implements \"a:b/c\") (instance))))")
  "duplicate `implements` attribute")
(assert_malformed
  (component quote
    "(type (component (import \"a\" (version-suffix \".1\") (implements \"a:b/c@1\") (version-suffix \".1\") (instance))))")
  "duplicate `version-suffix` attribute")
