;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async

(component
  (import "[async]f" (func))
  (import "r" (type $r (sub resource)))
  (import "[async method]r.f" (func (param "self" (borrow $r))))
  (import "[async static]r.f2" (func))
)

;; name conflicts where the "base kebab name" only differs on abi/etc and these
;; should all conflict with one another.

(assert_invalid
  (component
    (import "[async]f" (func))
    (import "f" (func))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "f" (func))
    (import "[async]f" (func))
  )
  "conflicts with previous name")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[method]r.f" (func (param "self" (borrow $r))))
    (import "[async static]r.f" (func)))
  "conflicts with previous name")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[method]r.f" (func (param "self" (borrow $r))))
    (import "[async method]r.f" (func (param "self" (borrow $r)))))
  "conflicts with previous name")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[method]r.f" (func (param "self" (borrow $r))))
    (import "[async static]r.f" (func)))
  "conflicts with previous name")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[async method]r.f" (func (param "self" (borrow $r))))
    (import "[async method]r.f" (func (param "self" (borrow $r)))))
  "conflicts with previous name")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[async method]r.f" (func (param "self" (borrow $r))))
    (import "[static]r.f" (func)))
  "conflicts with previous name")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[async method]r.f" (func (param "self" (borrow $r))))
    (import "[async static]r.f" (func)))
  "conflicts with previous name")

(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (import "[async static]r.f" (func))
    (import "[static]r.f" (func)))
  "conflicts with previous name")
