(module binary
  "\00asm\01\00\00\00"    ;; the header

  "\05\06\01"       ;; a memory section with 1 entry
  "\01\70\e0\ab\02" ;; definition of the 0th memory

  "\0b\08\01"       ;; a data section with 1 entry
  "\02\00"          ;; data section entry referencing 0th memory using flag 02 encoding.
  "\41\ca\0f\0b\00" ;; offset and data for the entry
)

(module binary
  "\00asm\01\00\00\00"    ;; the header

  "\05\06\01"       ;; a memory section with 1 entry
  "\01\70\e0\ab\02" ;; a definition of the 0th memory

  "\0b\08\01"       ;; a data section with 1 entry
  "\80\00"          ;; data section entry referencing 0th memory using a WebAssembly 1.0 encoding
                    ;; this encoding is not supported in current WebAssembly draft.
  "\41\ca\0f\0b\00" ;; offset and data for the entry
)

(module binary
  "\00asm\01\00\00\00"    ;; the header

  "\04\04\01"             ;; table section with 1 entry
  "\70\00\00"             ;; no max, minimum 0, funcref

  "\09\08\01"             ;; Element section with 1 entry
  "\02\00"                ;; Table index 0, using flag 02 encoding
  "\41\00\0b\00\00"       ;; offset and no elements
)
