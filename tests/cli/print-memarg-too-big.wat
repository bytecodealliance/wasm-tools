;; FAIL: print %

(module binary
    "\00asm" "\01\00\00\00"     ;; module header

    "\0b"           ;; data section
    "\07"           ;; size of section
    "\01"           ;; number of segments
    "\00"           ;; flags=active
    "\2e"           ;; i32.load16_s
    "\3f"           ;; alignment
    "\00"           ;; offset
    "\0b"           ;; end
    "\00"           ;; data size
)
