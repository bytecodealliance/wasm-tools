(assert_invalid
  (module binary
    "\00\61\73\6d"
    "\01\00\00\00"
    "\01\05"
    "\01"
    "\60\00\01\7f"
    "\03\02"
    "\01"
    "\00"
    "\0a\09"
    "\01"
    "\07"
    "\00"
    "\02\00"
    "\41\00"
    "\0b"
    "\0b"
  )
  "blocks, loops, and ifs may only produce a resulttype when multi-value is not enabled")
