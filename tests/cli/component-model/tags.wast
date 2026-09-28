;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (alias core export $i "" (core tag $t))
  )
  "export `` for core instance 0 is not a tag")

(component
  (core module $m (tag (export "")))
  (core instance $i (instantiate $m))
  (alias core export $i "" (core tag $t))
)

(component
  (core module $m (tag (export "")))
  (core instance $i (instantiate $m))
  (core instance
    (export "" (tag $i ""))))

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (core instance
      (export "" (tag 0)))
  )
  "unknown tag 0")

;; Tag types must match exactly when linking: an identical type is fine ...
(component
  (core module $a
    (type $t (struct))
    (tag (export "e") (param (ref $t))))
  (core module $b
    (type $t (struct))
    (import "a" "e" (tag (param (ref $t)))))
  (core instance $a (instantiate $a))
  (core instance $b (instantiate $b (with "a" (instance $a)))))

;; ... but a tag whose function type is a strict subtype is not
(assert_invalid
  (component
    (core module $a
      (type $t (struct))
      (type $sup (sub (func (param (ref $t)))))
      (type $sub (sub $sup (func (param (ref null $t)))))
      (tag (export "e") (type $sub)))
    (core module $b
      (type $t (struct))
      (type $sup (sub (func (param (ref $t)))))
      (import "a" "e" (tag (type $sup))))
    (core instance $a (instantiate $a))
    (core instance $b (instantiate $b (with "a" (instance $a)))))
  "type mismatch")

;; ... and neither is a strict supertype
(assert_invalid
  (component
    (core module $a
      (type $t (struct))
      (type $sup (sub (func (param (ref $t)))))
      (tag (export "e") (type $sup)))
    (core module $b
      (type $t (struct))
      (type $sup (sub (func (param (ref $t)))))
      (type $sub (sub $sup (func (param (ref null $t)))))
      (import "a" "e" (tag (type $sub))))
    (core instance $a (instantiate $a))
    (core instance $b (instantiate $b (with "a" (instance $a)))))
  "type mismatch")
