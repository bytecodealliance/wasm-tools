;; RUN: wast --assert default --snapshot tests/snapshots %

;; immutable globals are covariant in their content type
(component
  (core module $A
    (func $f)
    (elem declare func $f)
    (global (export "g") (ref func) (ref.func $f)))
  (core instance $a (instantiate $A))
  (core module $B (import "A" "g" (global funcref)))
  (core instance (instantiate $B (with "A" (instance $a))))
)

;; ... but not contravariant
(assert_invalid
  (component
    (core module $A
      (global (export "g") funcref (ref.null func)))
    (core instance $a (instantiate $A))
    (core module $B (import "A" "g" (global (ref func))))
    (core instance (instantiate $B (with "A" (instance $a))))
  )
  "expected global type (ref func), found funcref")

;; mutable globals are invariant in their content type
(assert_invalid
  (component
    (core module $A
      (func $f)
      (elem declare func $f)
      (global (export "g") (mut (ref func)) (ref.func $f)))
    (core instance $a (instantiate $A))
    (core module $B (import "A" "g" (global (mut funcref))))
    (core instance (instantiate $B (with "A" (instance $a))))
  )
  "expected global type funcref, found (ref func)")
(assert_invalid
  (component
    (core module $A
      (global (export "g") (mut funcref) (ref.null func)))
    (core instance $a (instantiate $A))
    (core module $B (import "A" "g" (global (mut (ref func)))))
    (core instance (instantiate $B (with "A" (instance $a))))
  )
  "expected global type (ref func), found funcref")
(component
  (core module $A
    (global (export "g") (mut funcref) (ref.null func)))
  (core instance $a (instantiate $A))
  (core module $B (import "A" "g" (global (mut funcref))))
  (core instance (instantiate $B (with "A" (instance $a))))
)

;; mutability must match
(assert_invalid
  (component
    (core module $A
      (global (export "g") (mut i32) (i32.const 0)))
    (core instance $a (instantiate $A))
    (core module $B (import "A" "g" (global i32)))
    (core instance (instantiate $B (with "A" (instance $a))))
  )
  "global types differ in mutability")
