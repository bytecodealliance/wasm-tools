(assert_invalid
  (module binary
    "\00asm\01\00\00\00"

    "\0c\05\ff\ff\ff\ff\01" ;; data section, 5 bytes, huge count
  )
  "data count section specifies too many data segments")

