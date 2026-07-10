;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module quote "(func (result f64) (f64.const nan:0x11111111800800080))")
  "constant out of range")

(assert_malformed
  (module quote "(func (result f64) (f64.const nan:0xffffffffffffffff0))")
  "constant out of range")

(assert_malformed
  (module quote "(func (result f32) (f32.const nan:0xffffffff0))")
  "constant out of range")

;; The payload `n` of a NaN literal must satisfy `1 <= n < 2^significand`, where
;; the significand is 52 bits for f64 and 23 bits for f32. Payloads that overflow
;; the significand must be rejected rather than silently masked.

;; f64: `0xf_ffff_ffff_ffff` (13 hex digits) is the maximum valid payload; one bit
;; more is out of range.
(module (func (result f64) f64.const nan:0xfffffffffffff))
(assert_malformed
  (module quote "(func (result f64) (f64.const nan:0xffffffffffffff))")
  "constant out of range")

;; f32: `0x7fffff` (23 bits) is the maximum valid payload; `0x800000` is out of
;; range.
(module (func (result f32) f32.const nan:0x7fffff))
(assert_malformed
  (module quote "(func (result f32) (f32.const nan:0x800000))")
  "constant out of range")

;; A zero payload is not a valid NaN (it would be an infinity).
(assert_malformed
  (module quote "(func (result f64) (f64.const nan:0x0))")
  "constant out of range")
