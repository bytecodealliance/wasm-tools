;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-descriptors

;; A core export alias can't refer to an exact function.
(assert_malformed
  (component binary
    "\00\61\73\6d\0d\00\01\00"                    ;; preamble
    "\01\1f\00\61\73\6d\01\00\00\00"              ;; core module section
    "\01\04\01\60\00\00"                          ;;   type section
    "\03\02\01\00"                                ;;   func section
    "\07\05\01\01\66\00\00"                       ;;   export section
    "\0a\04\01\02\00\0b"                          ;;   code section
    "\02\04\01\00\00\00"                          ;; core instance section: (instantiate 0)
    "\06\07\01\00\20\01\00\01\66"                 ;; alias section: core export alias with sort 0x20
  )
  "exact type is not allowed in core export aliases"
)

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
