;; RUN: print %

(module binary
    "\00asm" "\01\00\00\00"     ;; module header

    "\0b"           ;; data section
    "\09"           ;; size of section
    "\01"           ;; number of segments
    "\00"           ;; flags=active
    "\fe\10"        ;; i32.atomic.load
    "\3f"           ;; alignment
    "\00"           ;; offset
    "\01"           ;; offset
    "\0b"           ;; end
    "\00"           ;; data size
)
