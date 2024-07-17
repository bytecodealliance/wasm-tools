;; FAIL: print %

(module binary
    "\00asm" "\01\00\00\00"     ;; module header

    "\01"           ;; type section
    "\04"           ;; size of section
    "\01"           ;; one type
    "\60\00\00"     ;; function, no parameters or results

    "\03"   ;; function section
    "\02"   ;; size of function section
    "\01"   ;; one function
    "\00"   ;; type 0

    "\0a"   ;; code section
    "\09"   ;; size of code section
    "\01"   ;; 1 function
    "\07"   ;; size of function
    "\01"   ;; one local
    "\ff\ff\ff\ff\00"   ;; lots of this type
    "\70"   ;; type
)
