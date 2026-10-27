;; RUN: print %

(module binary
    "\00asm" "\01\00\00\00"     ;; module header

    "\0b"           ;; data section
    "\07"           ;; size of section
    "\01"           ;; number of segments
    "\00"           ;; flags=active
    "\28"           ;; i32.load
    "\0f"           ;; alignment
    "\00"           ;; offset
    "\0b"           ;; end
    "\00"           ;; data size
)
