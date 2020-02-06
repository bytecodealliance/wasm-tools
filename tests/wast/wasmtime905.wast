(assert_invalid
    (module (func (local anyref)))
    "reference types support is not enabled"
)
