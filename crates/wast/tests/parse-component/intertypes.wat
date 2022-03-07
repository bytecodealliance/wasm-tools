(component $C
  (type $A0 unit)
  (type $A1 bool)
  (type $A2 u8)
  (type $A3 s8)
  (type $A4 u16)
  (type $A5 s16)
  (type $A6 u32)
  (type $A7 s32)
  (type $A8 u64)
  (type $A9 s64)
  (type $A10 float32)
  (type $A11 float64)
  (type $A12 char)
  (type $A13 string)

  (type $A14a (record))
  (type $A14b (record (field $x unit)))
  (type $A14c (record (field $x $T)))

  (type $A15a (variant))
  (type $A15b (variant (case $x unit)))
  (type $A15c (variant (case $x $T)))
  (type $A15d (variant (case $x unit (defaults-to $x))))
  (type $A15e (variant (case $x $T (defaults-to $x))))

  (type $A16a (list unit))
  (type $A16b (list $T))

  (type $A17a (tuple))
  (type $A17b (tuple unit))
  (type $A17c (tuple $T))

  (type $A18a (flags))
  (type $A18b (flags $x))

  (type $A19a (enum))
  (type $A19b (enum $x))

  (type $A20a (union))
  (type $A20b (union unit))
  (type $A20c (union $T))

  (type $A21a (option unit))
  (type $A21b (option $T))

  (type $A22a (expected unit unit))
  (type $A22b (expected $T unit))
  (type $A22c (expected unit $T))
  (type $A22d (expected $T $T))
)
