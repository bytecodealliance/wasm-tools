;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads

(component
  (core module $A
    (global (export "g") (shared mut i32) (i32.const 0)))
  (core instance $a (instantiate $A))
  (core module $B
    (import "A" "g" (global (shared mut i32))))
  (core instance (instantiate $B (with "A" (instance $a))))
)

(assert_invalid
  (component
    (core module $A
      (global (export "g") (mut i32) (i32.const 0)))
    (core instance $a (instantiate $A))
    (core module $B
      (import "A" "g" (global (shared mut i32))))
    (core instance (instantiate $B (with "A" (instance $a)))))
  "mismatch in the shared flag for globals")

(assert_invalid
  (component
    (core module $A
      (global (export "g") (shared mut i32) (i32.const 0)))
    (core instance $a (instantiate $A))
    (core module $B
      (import "A" "g" (global (mut i32))))
    (core instance (instantiate $B (with "A" (instance $a)))))
  "mismatch in the shared flag for globals")
