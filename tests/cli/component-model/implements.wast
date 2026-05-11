;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-implements

(component
  (component
    (import "[implements=<a:b/c>]a" (instance))
    (import "[implements=<a:b/c>]b" (instance))
    (import "[implements=<a:b/c@1.0.0>]c" (instance))
    (import "[implements=<ns:pkg/iface>]my-label" (instance))
    (import "a:b/c" (instance))
    (import "a:b/c@1.0.0" (instance))
  )
)

(assert_invalid
  (component (import "[implements=<not-valid>]a" (instance)))
  "not a valid extern name")
(assert_invalid
  (component (import "[implements=<>]" (instance)))
  "not a valid extern name")
(assert_invalid
  (component (import "[implements=<a:b/c>]" (instance)))
  "not a valid extern name")
(assert_invalid
  (component (import "[implements=<a:b/c>]NOT_KEBAB" (instance)))
  "not a valid extern name")

(assert_invalid
  (component
    (import "[implements=<a:b/c>]a" (instance))
    (import "[implements=<a:b/c>]a" (instance))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "[implements=<a:b/c>]a" (instance))
    (import "[implements=<b:c/d>]a" (instance))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "[implements=<a:b/c>]a" (instance))
    (import "[implements=<a:b/c@1.0.0>]a" (instance))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "[implements=<a:b/c>]a" (instance))
    (import "a" (instance))
  )
  "conflicts with previous name")
