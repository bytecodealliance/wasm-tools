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
  (type $A14b (record (field "x" unit)))
  (type $A14c (record (field "x" $A0)))

  (type $A15a (variant))
  (type $A15b (variant (case "x" unit)))
  (type $A15c (variant (case "x" $A1)))

  ;; FIXME(#594) should implement this
  (; (type $A15d (variant (case "x" unit (defaults-to "x")))) ;)
  (; (type $A15e (variant (case "x" $A2 (defaults-to "x")))) ;)

  (type $A16a (list unit))
  (type $A16b (list $A3))

  (type $A17a (tuple))
  (type $A17b (tuple unit))
  (type $A17c (tuple $A4))

  (type $A18a (flags))
  (type $A18b (flags "x"))

  (type $A19a (enum))
  (type $A19b (enum "x"))

  (type $A20a (union))
  (type $A20b (union unit))
  (type $A20c (union $A5))

  (type $A21a (option unit))
  (type $A21b (option $A6))

  (type $A22a (expected unit unit))
  (type $A22b (expected $A7 unit))
  (type $A22c (expected unit $A8))
  (type $A22d (expected $A9 $A10))
)

(assert_invalid
  (component
    (type $t (module))
    (type (func (param $t)))
  )
  "not an interface type")

(assert_invalid
  (component
    (type $t (module))
    (type (func (result $t)))
  )
  "not an interface type")

(assert_invalid
  (component
    (type $t (module))
    (type (value $t))
  )
  "not an interface type")

(assert_invalid
  (component
    (type $t (module))
    (type (option $t))
  )
  "not an interface type")

(assert_invalid
  (component (type (option 0)))
  "index out of bounds")
(assert_invalid
  (component (type (list 0)))
  "index out of bounds")
(assert_invalid
  (component (type (record (field "x" 0))))
  "index out of bounds")
(assert_invalid
  (component (type (variant (case "x" 0))))
  "index out of bounds")
(assert_invalid
  (component (type (union 0)))
  "index out of bounds")
(assert_invalid
  (component (type (expected 0 1)))
  "index out of bounds")
(assert_invalid
  (component (type (tuple 0)))
  "index out of bounds")

(assert_invalid
  (component (type (record (field "" s32))))
  "name cannot be empty")

(assert_invalid
  (component (type (variant (case "" s32))))
  "name cannot be empty")

(assert_invalid
  (component (type (flags "")))
  "name cannot be empty")

(assert_invalid
  (component (type (enum "")))
  "name cannot be empty")
