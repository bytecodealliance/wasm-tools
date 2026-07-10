;; Core instances cannot export types, so resolving a `(core type $i "x")`
;; reference through a core instance export must be an error, not a panic.
(component
  (core module $m)
  (core instance $i (instantiate $m))
  (import "f" (func $f))
  (core func (canon lower (func $f) (core-type (core type $i "t"))))
)
