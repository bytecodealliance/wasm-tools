;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-descriptors

;; An exact function export of a core instance can be aliased as a core func.
(component
  (core type $mt (module
    (type $f (func))
    (export "f" (func (exact (type $f))))
  ))
  (import "m" (core module $m (type $mt)))
  (core instance $i (instantiate $m))
  (alias core export $i "f" (core func $f))
  (core module $n (import "" "f" (func)))
  (core instance (instantiate $n (with "" (instance (export "f" (func $f))))))
)

;; Exported functions are exact if they are defined in the module or imported
;; exactly.

;; Defined function exported and used to satisfy an exact import.
(component
  (core module $A
    (type $f (func))
    (func (export "f") (type $f)))
  (core module $B
    (type $f (func))
    (import "a" "f" (func (exact (type $f)))))
  (core instance $a (instantiate $A))
  (core instance $b (instantiate $B (with "a" (instance $a))))
)

;; Exactly-imported function re-exported is also exact.
(component
  (core module $A
    (type $f (func))
    (func (export "f") (type $f)))
  (core module $R
    (type $f (func))
    (import "a" "f" (func (exact (type $f))))
    (export "f" (func 0)))
  (core module $B
    (type $f (func))
    (import "a" "f" (func (exact (type $f)))))
  (core instance $a (instantiate $A))
  (core instance $r (instantiate $R (with "a" (instance $a))))
  (core instance $b (instantiate $B (with "a" (instance $r))))
)

;; A module-typed import declaring an exact export, satisfied by a module
;; that defines and exports that function.
(component
  (core module $A
    (type $f (func))
    (func (export "f") (type $f)))
  (component $C
    (import "m" (core module
      (type $f (func))
      (export "f" (func (exact (type $f)))))))
  (instance (instantiate $C (with "m" (core module $A))))
)

;; An inexactly-imported function re-exported is not exact.
(assert_invalid
  (component
    (core module $A
      (type $f (func))
      (func (export "f") (type $f)))
    (core module $R
      (type $f (func))
      (import "a" "f" (func (type $f)))
      (export "f" (func 0)))
    (core module $B
      (type $f (func))
      (import "a" "f" (func (exact (type $f)))))
    (core instance $a (instantiate $A))
    (core instance $r (instantiate $R (with "a" (instance $a))))
    (core instance $b (instantiate $B (with "a" (instance $r))))
  )
  "expected func_exact, found func")

;; Exact exports must still have matching types.
(assert_invalid
  (component
    (core module $A
      (type $f (func))
      (func (export "f") (type $f)))
    (core module $B
      (type $f (func (param i32)))
      (import "a" "f" (func (exact (type $f)))))
    (core instance $a (instantiate $A))
    (core instance $b (instantiate $B (with "a" (instance $a))))
  )
  "type mismatch")
