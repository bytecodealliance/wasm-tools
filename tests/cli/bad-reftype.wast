;; RUN: wast --assert default --snapshot tests/snapshots %

;; A value type is `numtype | vectype | reftype`, and a reference type is either
;; an abstract heap type shorthand byte or the two-byte `0x64 heaptype` (`ref ht`)
;; / `0x63 heaptype` (`ref null ht`) forms. There is no bare type-index value
;; type, so a leading byte of `0x00` (a non-negative sLEB) is malformed.

;; A bare index used as a value type. In value-type position the error is
;; reclassified as "invalid value type".
(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00"    ;; module header

    "\01\08\02"                ;; type section, 2 types
    "\60\00\00"                ;; type 0: (func)
    "\60\01"                   ;; type 1: (func (param ...)), 1 param
    "\00"                      ;; param 0: bare type index 0x00 (invalid value type)
    "\00"                      ;; 0 results
  )
  "invalid value type")

;; The same bug with an over-long LEB encoding of the bare index (`\ff\00` = 127).
;; The reftype discriminator is a single byte, so this must be rejected at decode
;; time rather than re-interpreted as a type index.
(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00"    ;; module header

    "\01\09\02"                ;; type section, 2 types
    "\60\00\00"                ;; type 0: (func)
    "\60\01"                   ;; type 1: (func (param ...)), 1 param
    "\ff\00"                   ;; param 0: bare index 0x00 with an over-long LEB
    "\00"                      ;; 0 results
  )
  "invalid value type")

;; The same bare index in a reference-type-only position (a table's element type)
;; is reported as a malformed reference type.
(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00"    ;; module header

    "\04\04\01"                ;; table section, 1 table
    "\00"                      ;; element type: bare index 0x00 (malformed reftype)
    "\00\00"                   ;; limits: no max, minimum 0
  )
  "malformed reference type")

;; Boundary: the legitimate concrete reftypes `0x63 <idx>` (`ref null $t`) and
;; `0x64 <idx>` (`ref $t`), where the index is an sLEB after the prefix byte, must
;; still be accepted.
(module binary
  "\00asm" "\01\00\00\00"      ;; module header

  "\01\0b\02"                  ;; type section, 2 types
  "\60\00\00"                  ;; type 0: (func)
  "\60\02"                     ;; type 1: (func (param ...)), 2 params, 0 results
  "\63\00"                     ;; param 0: (ref null 0)
  "\64\00"                     ;; param 1: (ref 0)
  "\00"                        ;; 0 results
)
