;; RUN: wast --assert default --snapshot tests/snapshots %

;; `[get]` and `[set]` names are rejected unless the feature is enabled.

(assert_invalid
  (component (import "[get]a" (func (result u32))))
  "`[get]` and `[set]` names require the component model accessors feature")

(assert_invalid
  (component (import "[set]a" (func (param "v" u32))))
  "`[get]` and `[set]` names require the component model accessors feature")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[method][get]r.p" (func (param "self" (borrow $r)) (result u32))))
  "`[get]` and `[set]` names require the component model accessors feature")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[static][set]r.p" (func (param "v" u32))))
  "`[get]` and `[set]` names require the component model accessors feature")
