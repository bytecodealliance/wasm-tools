;; RUN: dump %

(module binary
  "\00asm"
  "\01\00\00\00"

  "\00" ;; custom section
  "\08" ;; byte length of entire section
  "\04" ;; byte length of "name"
  "name"
  "\02" ;; local name subsection
  "\07" ;; 7 byte subsection
  "\06" ;; six names
)
