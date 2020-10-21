(module
  (elem $a externref)
  (elem $b externref (ref.null extern))
)

(assert_invalid
  (module
    (elem $a externref (ref.func 0))
    (func)
  )
  "type mismatch")

(assert_invalid
  (module binary
    "\00asm\01\00\00\00"

    "\09" ;; element section
    "\07" ;; size of section
    "\01" ;; 1 element

    ;; passive segment, extrenref type, 1 element, (ref.null func)
    "\05\6f\01\d0\70\0b"
  )
  "type mismatch")
