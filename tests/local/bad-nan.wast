(assert_malformed
  (module quote "(func (result f64) (f64.const nan:0x11111111800800080))")
  "constant out of range")

(assert_malformed
  (module quote "(func (result f64) (f64.const nan:0xffffffffffffffff0))")
  "constant out of range")

(assert_malformed
  (module quote "(func (result f32) (f32.const nan:0xffffffff0))")
  "constant out of range")
