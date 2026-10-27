;; RUN: wast --assert default --snapshot tests/snapshots %

;; Outer aliases are printed with the identifier of the component they refer
;; to, and these all need to print as text that refers to the same component.

;; A name that can't be an identifier is printed as a synthetic identifier
;; with an `@name` annotation, and the alias uses only the identifier.
(component (@name "")
  (type u8)
  (alias outer 0 0 (type))
)

;; The inner `$a` shadows the outer one, so the alias prints its count.
(component $a
  (type $t u8)
  (component $a
    (alias outer 1 $t (type))
  )
)

;; The middle component names itself `n` in its own `component-name` section,
;; but it's printed with the name its parent gives it, which is none.
(component binary
  "\00asm\0d\00\01\00"
  "\04\32"                                ;; component section
    "\00asm\0d\00\01\00"
    "\00\13\0ecomponent-name\00\02\01n"   ;; component name: "n"
    "\07\02\01\7d"                        ;; type section: u8
    "\04\0f"                              ;; component section
      "\00asm\0d\00\01\00"
      "\06\05\01\03\02\01\00"             ;; alias outer 1 0 (type)
)
