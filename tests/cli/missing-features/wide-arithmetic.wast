;; RUN: wast --assert default --snapshot tests/snapshots % -f mvp

(assert_invalid
  (module
    (func (param i64 i64 i64 i64)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i64.add128
      drop
      drop)
  )
  "wide arithmetic support is not enabled")

(assert_invalid
  (module
    (func (param i64 i64 i64 i64)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i64.sub128
      drop
      drop)
  )
  "wide arithmetic support is not enabled")

(assert_invalid
  (module
    (func (param i64 i64)
      local.get 0
      local.get 1
      i64.mul_wide_s
      drop
      drop)
  )
  "wide arithmetic support is not enabled")

(assert_invalid
  (module
    (func (param i64 i64)
      local.get 0
      local.get 1
      i64.mul_wide_u
      drop
      drop)
  )
  "wide arithmetic support is not enabled")
