;; FAIL: print %

(module binary
    "\00asm" "\01\00\00\00"     ;; module header

    "\03"               ;; function section
    "\05"               ;; section size
    "\ff\ff\ff\ff\0f"   ;; number of functions (u32::MAX)
)
