(component binary
  "\00asm" "\0d\00\01\00"   ;; component header
  "\03\07"          ;; core type section, 7 bytes large
  "\01"             ;; 1 count
  "\00\50"          ;; sub type
  "\00"             ;; no supertypes
  "\60"             ;; function type
  "\00\00"          ;; no parameters, no results
)

;; text equivalent of above
(component (core type (sub (func))))

(component binary
  "\00asm" "\0d\00\01\00"   ;; component header
  "\03\06"          ;; core type section, 6 bytes large
  "\02"             ;; 2 count
  "\50"             ;; module type
  "\00"             ;; empty
  "\60"             ;; function type
  "\00\00"          ;; no parameters, no results
)

;; text equivalent of above
(component (core type (module)) (core type (func)))

(component binary
  "\00asm" "\0d\00\01\00"   ;; component header
  "\03\09"          ;; core type section, 9 bytes large
  "\01"             ;; 1 count
  "\50"             ;; module type
  "\01"             ;; 1 count
  "\01"             ;; core type in module
  "\50"             ;; sub type
  "\00"             ;; no supertypes
  "\60"             ;; function type
  "\00\00"          ;; no parameters, no results
)

;; text equivalent of above
(component (core type (module (type (sub (func))))))

(assert_malformed
  (component binary
    "\00asm" "\0d\00\01\00"   ;; component header
    "\03\06"          ;; core type section, 6 bytes large
    "\01"             ;; 1 count
    "\50"             ;; attempted sub type, but actually a module type
    "\00"             ;; attempted zero super types, but actually empty
    "\60"             ;; function type
    "\00\00"          ;; no parameters, no results
  )
"unexpected data at the end of the section")

(assert_malformed
  (component binary
    "\00asm" "\0d\00\01\00"   ;; component header
    "\03\05"          ;; core type section, 5 bytes large
    "\01"             ;; 1 count
    "\00\60"          ;; attempted function type with invalid prefix
    "\00\00"          ;; no parameters, no results
  )
"invalid leading byte (0x60) for non-final sub type")


;; test various shapes and properties of GC types within a component
(component $C
  (core type $t1 (struct))
  (core type $t2 (array i8))
  (core type $t3 (func))
  (core type (struct
    (field $a (ref $t1))
    (field $b (ref $t2))
    (field $c (ref $t3))
  ))
  (core type $s1 (sub (struct)))
  (core type $s2 (sub final (struct)))
  (core type $s3 (sub $s1 (struct)))
  (core type $f (func (result (ref $f))))
  (core rec)
  (core rec
    (type $f1 (func (result (ref $f2))))
    (type $f2 (func (result (ref $f1))))
  )

  (core type (module
    (alias outer $C $t1 (type $t1))
    (import "x" "x" (func (result (ref $t1))))

    (type $a1 (struct (field $a (ref $t1))))
    (type $a2 (array (ref $a1)))
    (type $a3 (func (result (ref $a2))))
    (type $a4 (sub (struct)))
    (type $a5 (sub final (struct)))
    (rec)
    (rec
      (type $f1 (func (result (ref $f2))))
      (type $f2 (func (result (ref $f1))))
    )
    (type $f (func (result (ref $f))))
  ))
)

;; aliases don't work within core types
(assert_malformed
  (component quote
    "(core type $t1 (struct))"
    "(core type (module (type (sub $t1 (struct)))))"
  )
  "unknown core type: failed to find name `$t1`")

(assert_invalid
  (component $C
    (core type $t1 (struct))
    (core type (sub $t1 (struct)))
  )
  "sub type cannot have a final super type")

(assert_invalid
  (component $C
    (core type (module
      (type $t1 (struct))
      (type (sub $t1 (struct)))
    ))
  )
  "sub type cannot have a final super type")
