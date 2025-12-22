;; RUN: wast --assert default --snapshot tests/snapshots % -f=-compact-imports

(assert_malformed
  (module
    (import "test"
      (item "fi32" (func (result i32)))
      (item "fi64" (func (result i64)))
    )
  )
  "invalid leading byte 0x7F with compact imports proposal disabled")

(assert_malformed
  (module
    (import "test"
      (item "g1")
      (item "g20")
      (item "g300")
      (item "g4000")
      (global i32)
    )
  )
  "invalid leading byte 0x7E with compact imports proposal disabled")
