;; RUN: dump %

(module binary
  "\00asm"
  "\01\00\00\00"

  "\00" ;; custom section
  "\10" ;; byte length of entire section
  "\04" ;; byte length of "name"
  "name"
  "\02" ;; local name subsection
  "\07" ;; 7 byte subsection
  "\06" ;; six names
  "\22" ;; names for function 0x22
  "\00" ;; 0 count
  "\00" ;; names for function 0
  "\00" ;; 0 count
  "\00" ;; names for function 0
  "\40" ;; 0x40 count
  "\00" ;; pad
  "\00" ;; pad
)
