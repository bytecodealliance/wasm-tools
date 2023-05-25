(component
  (func (import "a"))
  (component)
  (instance (instantiate 0 (with "NotKebab-Case" (func 0))))
)

(assert_invalid
  (component
    (import "f" (func))
    (instance (export "1" (func 0)))
  )
  "`1` is not in kebab case"
)

(assert_invalid
  (component
    (instance)
    (alias export 0 "Xml" (func))
  )
  "instance 0 has no export named `Xml`"
)

(assert_invalid
  (component
    (type (flags "a-1-c"))
  )
  "flag name `a-1-c` is not in kebab case"
)

(assert_invalid
  (component
    (type (enum "NevEr"))
  )
  "enum tag name `NevEr` is not in kebab case"
)

(assert_invalid
  (component
    (type (record (field "GoNnA" string)))
  )
  "record field name `GoNnA` is not in kebab case"
)

(assert_invalid
  (component
    (type (variant (case "GIVe" string)))
  )
  "variant case name `GIVe` is not in kebab case"
)


(assert_invalid
  (component
    (type (func (param "yOu" string)))
  )
  "function parameter name `yOu` is not in kebab case"
)

(assert_invalid
  (component
    (type (func (result "uP" string)))
  )
  "name `uP` is not in kebab case"
)

(assert_invalid
  (component
    (type (component (export "NevEr" (func))))
  )
  "`NevEr` is not in kebab case"
)

(assert_invalid
  (component
    (type (component (import "GonnA" (func))))
  )
  "`GonnA` is not in kebab case"
)

(assert_invalid
  (component
    (type (instance (export "lET" (func))))
  )
  "`lET` is not in kebab case"
)

(assert_invalid
  (component
    (instance (export "YoU"))
  )
  "`YoU` is not in kebab case"
)

(assert_invalid
  (component
    (instance (import "DOWn"))
  )
  "`DOWn` is not in kebab case"
)
