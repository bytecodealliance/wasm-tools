(assert_malformed
 (module binary
   "\00asm" "\01\00\00\00"
   "\05\04\01"                ;; Memory section

   ;; memory 0
   "\08"                      ;; flags w/ custom page size
   "\00"                      ;; minimum = 0
   "\11"                      ;; pagesize = 2**17
 )
 "invalid custom page size"
)
