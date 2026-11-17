;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads

;; A type use with only inline params and results refers to the first
;; singular, final, function type with the same signature. Non-final, shared,
;; and subtyped function types don't qualify, so a fresh type is created.

(module
  (type $nonfinal (sub (func)))
  (type $shared (shared (func)))
  (type $g (func))
  (func $u)
  (elem declare func $u)
  (func (result (ref $g)) (ref.func $u))
)

(module
  (type $nonfinal (sub (func)))
  (type $shared (shared (func)))
  (type $base (sub (func (param i32))))
  (type $child (sub final $base (func (param i32))))
  (type $final (sub final (func (param i64))))
  (func (export "a"))
  (func (export "b") (param i32))
  (func (export "c") (param i64))
)
