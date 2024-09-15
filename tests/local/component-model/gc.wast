(component binary
  "\00asm" "\0d\00\01\00"   ;; component header
  "\03\07"          ;; core type section, 7 bytes large
  "\01"             ;; 1 count
  "\00\50"          ;; sub type
  "\00"             ;; no supertypes
  "\60"             ;; function type
  "\00\00"          ;; no parameters, no results
)

(component binary
  "\00asm" "\0d\00\01\00"   ;; component header
  "\03\06"          ;; core type section, 6 bytes large
  "\02"             ;; 2 count
  "\50"             ;; module type
  "\00"             ;; empty
  "\60"             ;; function type
  "\00\00"          ;; no parameters, no results
)

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
