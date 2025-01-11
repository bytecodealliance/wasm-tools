(assert_invalid
    (module (func (local externref)))
    "reference types support is not enabled"
)
