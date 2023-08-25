(component $C
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

  (type $A14b (record (field "x" (tuple char))))
  (type $A14c (record (field "x" $A1)))

  (type $A15a (variant (case "x")))
  (type $A15b (variant (case "x" $A1)))
  (type $A15c (variant (case $x "x") (case $y "y" string (refines $x)) (case "z" string (refines $y))))
  (type $A15d (variant (case "x") (case "y" string (refines 0)) (case "z" string (refines 1))))

  (type $A16a (list (tuple u8)))
  (type $A16b (list $A3))

  (type $A17a (tuple u8))
  (type $A17b (tuple $A4))

  (type $A18b (flags "x"))

  (type $A19b (enum "x"))

  (type $A21a (option (tuple u32)))
  (type $A21b (option $A6))

  (type $A22a (result))
  (type $A22b (result $A7))
  (type $A22c (result (error $A8)))
  (type $A22d (result $A9 (error $A10)))
)

(assert_invalid
  (component
    (type $t (variant (case $x "x" string (refines $x))))
  )
  "variant case cannot refine itself"
)

(assert_invalid
  (component
    (type $t (variant (case "x" (refines $y)) (case $y "y" string)))
  )
  "unknown variant case"
)

(assert_invalid
  (component
    (type $t string)
    (type $v (variant (case "x" $t (refines $z))))
  )
  "unknown variant case"
)


(assert_invalid
  (component
    (type $t string)
    (type $v (variant (case "x" $t (refines 1))))
  )
  "variant case can only refine a previously defined case"
)

(assert_invalid
  (component
    (type $t string)
    (type $v (variant (case "x" $t) (case "y" u64 (refines 2)) (case "z" string)))
  )
  "variant case can only refine a previously defined case"
)

(assert_invalid
  (component
    (type $t string)
    (type $v (variant (case $x "x" $t) (case $x "y" $t)))
  )
  "duplicate variant case identifier"
)

(assert_invalid
  (component
    (type $t (func))
    (type (func (param "t" $t)))
  )
  "type index 0 is not a defined type")

(assert_invalid
  (component
    (type $t (instance))
    (type (func (result $t)))
  )
  "type index 0 is not a defined type")

(assert_invalid
  (component
    (type $t (component))
    (type (option $t))
  )
  "type index 0 is not a defined type")

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
  (component (type (result 0 (error 1))))
  "index out of bounds")
(assert_invalid
  (component (type (tuple 0)))
  "index out of bounds")

(assert_invalid
  (component (type (record (field "a-B-c-D" string) (field "A-b-C-d" u8))))
  "record field name `A-b-C-d` conflicts with previous field name `a-B-c-D`")
(assert_invalid
  (component (type (variant (case "x" s64) (case "x" s64))))
  "variant case name `x` conflicts with previous case name `x`")
(assert_invalid
  (component (type (flags "x" "y" "X")))
  "flag name `X` conflicts with previous flag name `x`")
(assert_invalid
  (component (type (enum "x" "y" "X")))
  "enum tag name `X` conflicts with previous tag name `x`")

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

(assert_invalid
  (component (type (variant)))
  "variant type must have at least one case")
